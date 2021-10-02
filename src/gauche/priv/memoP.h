/*
 * priv/memoP.h - memoization table
 *
 *   Copyright (c) 2021  Shiro Kawai  <shiro@acm.org>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef GAUCHE_PRIV_MEMOP_H
#define GAUCHE_PRIV_MEMOP_H

#include "gauche/priv/atomicP.h"

/* Memotable is a mapping specialized for memoization.
   It has several characteristics we can take advantage of, and
   several specific requrements, than the generic hashtables.

   - Must be thread-safe.
   - No explicit deletion.
   - Multiple entries of the same key can exist, for it won't affect outcome.
   - Entries can dissapear at any time.  It simply causes recompuation.
   - No need to iterate over the table.
   - Its key is a list of arguments.  We don't want equal?-hash, for
     it would recurse into each arguments unnecessarily.  We use a
     tailored hash function and equality predicate.
 */

/* The storage contains flat array of entries.

   If num_keys argument is a positive number, that many keys are stored
   directly in the storage:

            +-------------+
    Entry > |   hashval   |
            +-------------+
            |    key 0    |
            +-------------+
            :             :
            +-------------+
            |   key n-1   |
            +-------------+
            |    value    |
            +-------------+

   If num_keys is 0, keys are in list and pointed from the storage:

            +-------------+
    Entry > |   hashval   |
            +-------------+
            |    keys --------> (key0 ... keyN-1)
            +-------------+
            |    value    |
            +-------------+

   The hashval slots may be:

     0..00   - Unused entry.
     0..10   - Invalid entry.  It won't stop probing, but the entry is
               unusable until table GC/expansion.
     x..x1   - If LSB is 1, the rest of the bits contains a shifted hash value.
               It is used to skip over the entry during probing.
     x..00   - Non zero entry with LSBs being 00.  This is a pointer to the
               <thread> that is building the entry.

   It is important that once an entry is created, the key-value association
   won't change.  (Except when the table is weak and either key or value is
   GC-ed.)


  Lookup

   - Compute a hash value modulo table capacity.
   - Start probing form entries[hashval%capacity]
       - h = LOAD(entry->hashval)
       - if h == 0
          probe failed.
       - if h & 0x01 == 1
            &&  h>>1 == hashval
            && all keys match
            && value != 0
          found.
       - otherwise, advance to the next entry and re-probe, until
         maximum re-probe count reaches.

  Insertion

   - Compute a hash value modulo table capacity.
   - Start probing form entries[hashval%capacity]
       - h = LOAD(entry->hashval)
       - if h == 0
          CAS(&entry->hashval, 0, Scm_VM())
          - if success
              fill the entry, then STORE(entry->hashval, ((hashval<<1) & 1))
          - else
              advance to the next entry and re-probe (*1)
       - if h & 0x01 == 1
            &&  h>>1 == hashval
            && all keys match
          STORE(value) (*2)
       - otherwise, advance to the next entry and re-probe
         - If maximum re-probe count reaches.

      (*1) This can cause duplicate entries if the other thread is inserting
           the same key&value there, but it's ok.
      (*2) This is idempotent.  However, if value reference is weak,
           the value slot can be 0.  We can safely reuse the entry.

  Dissapearing pointers

   If the table is weak, any key or value slot can become 0 after GC.

   - Value becomes 0 - We simply skip the entry during lookup, and set the
     value during insertion.  The same keys always yield the same value, so
     there's no race.

   - Key becomes 0 - The entry become invalid, for it will never match
     valid keys.  To accelerate probing, whoever finds such entry sets
     its hashval field to #b10, so that next probing can easily skip it.

  Table GC/expansion

   During insertion, if we can't find an unused entry until the certain
   number of re-probing, we have to make room.  We allocate a new
   stroage and rebuild the table, then swap the pointer to the storage.
   New entries added after copying and swapping may be lost, but that's ok
   for our purpose.

   During copying, these operations will make room:

   - If the entry is invalid, it is just ignored.
   - If the table is expandable, a bigger storage is allocated.
   - If the table is not expandable, drop one of the existing entry.
 */

/* Weakness.  The table can be made so that pointers to keys and values
   all weak, to avoid retaining too much data.  For memoization, dropping
   references merely triggers recalculation, and the outcome won't change.

   If we keep the list of keys (num_keys == 0), there's an increased
   chance that the key disappears even while value is alive, reducing
   the hit rate.  If the value retains the reference to the key list,
   we don't need to worry.
 */

enum {
    SCM_MEMO_TABLE_WEAK = (1L<<0),    /* use weak table */
    SCM_MEMO_TABLE_FIXED = (1L<<1)    /* don't allow expansion */
};

typedef struct ScmMemoTableStorageRec {
    u_long capacity;            /* read only */
    ScmAtomicWord *vec;         /* [capacity*entry_size] */
} ScmMemoTableStorage;

/* Fields other than storage are read-only */
typedef struct ScmMemoTableRec {
    SCM_HEADER;
    ScmMemoTableStorage *storage;
    u_long flags;
    int num_keys;
    u_int entry_size;
} ScmMemoTable;

SCM_CLASS_DECL(Scm_MemoTableClass);
#define SCM_CLASS_MEMO_TABLE   (&Scm_MemoTableClass)
#define SCM_MEMO_TABLE(obj)    ((ScmMemoTable*)(obj))
#define SCM_MEMO_TABLE_P(obj)  SCM_ISA(obj, SCM_CLASS_MEMO_TABLE)

SCM_EXTERN ScmObj Scm_MakeMemoTable(u_long capacity,
                                    int num_keys,
                                    u_long flags);

SCM_EXTERN ScmObj Scm_MemoTableGet(ScmMemoTable *tab, ScmObj keys);
SCM_EXTERN ScmObj Scm_MemoTableGetv(ScmMemoTable *tab, ScmObj *keys, int nkeys);

SCM_EXTERN ScmObj Scm_MemoTablePut(ScmMemoTable *tab, ScmObj keys, ScmObj val);
SCM_EXTERN ScmObj Scm_MemoTablePutv(ScmMemoTable *tab, ScmObj *keys, int nkeys,
                                    ScmObj val);

SCM_EXTERN void Scm__MemoTableDump(ScmMemoTable *tab, ScmPort *port);

#endif /*GAUCHE_PRIV_MEMOP_H*/
