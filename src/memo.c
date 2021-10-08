/*
 * memo.c - memoization table
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

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/priv/memoP.h"

/* See memoP.h for the design choices. */

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_MemoTableClass, NULL);

ScmObj Scm_MakeMemoTable(u_long capacity, int num_keys, u_long flags)
{
    ScmMemoTable *t = SCM_NEW(ScmMemoTable);
    SCM_SET_CLASS(t, SCM_CLASS_MEMO_TABLE);
    t->flags = flags;
    t->num_keys = num_keys;
    t->entry_size = (num_keys > 0? (num_keys+2) : (-num_keys+3));

    ScmMemoTableStorage *s = SCM_NEW(ScmMemoTableStorage);
    s->capacity = capacity;
    if (flags & SCM_MEMO_TABLE_WEAK) {
        s->vec = SCM_NEW_ATOMIC_ARRAY(ScmAtomicVar,
                                      capacity * t->entry_size);
    } else {
        s->vec = SCM_NEW_ARRAY(ScmAtomicVar,
                               capacity * t->entry_size);
    }
    t->storage = s;

    return SCM_OBJ(t);
}

/* common routine to extract # of fixed keys and # of rest keys */
static inline int get_numkeys(int nkeys, int *rest_keys)
{
    *rest_keys = nkeys <= 0;
    return (nkeys > 0 ? nkeys : -nkeys);
}


/*
 * hash and equality
 */

static inline _Bool equal_1(ScmObj a, ScmObj b)
{
    if (SCM_STRINGP(a)) {
        if (SCM_STRINGP(b)) {
            return Scm_StringEqual(SCM_STRING(a), SCM_STRING(b));
        } else {
            return FALSE;
        }
    } else {
        if (!SCM_STRINGP(b)) {
            return Scm_EqvP(a, b);
        } else {
            return FALSE;
        }
    }
}

static inline u_long hash_1(ScmObj key)
{
    if (SCM_STRINGP(key)) {
        return Scm_HashString(SCM_STRING(key), 0);
    } else {
        return Scm_EqvHash(key);
    }
}

static u_long memo_hashv(ScmObj *keys, int nkeys)
{
    u_long v = (u_long)Scm_HashSaltRef();
    int rest_keys;
    int fixed_keys = get_numkeys(nkeys, &rest_keys);
    for (int i = 0; i < fixed_keys; i++) {
        u_long v1 = hash_1(keys[i]);
        v = Scm_CombineHashValue(v, v1);
    }
    if (rest_keys) {
        ScmObj rest = keys[fixed_keys], rp;
        SCM_FOR_EACH(rp, rest) {
            u_long v1 = hash_1(SCM_CAR(rp));
            v = Scm_CombineHashValue(v, v1);
        }
    }
    return v;
}

static _Bool memo_equalv(ScmObj *keys, int nkeys, ScmAtomicVar *entry_keys)
{
    int rest_keys;
    int fixed_keys = get_numkeys(nkeys, &rest_keys);
    for (int i = 0; i < fixed_keys; i++) {
        ScmObj k = (ScmObj)AO_load(entry_keys+i);
        if (!equal_1(keys[i], k)) return FALSE;
    }
    if (rest_keys) {
        ScmObj rest = keys[fixed_keys], rp;
        ScmObj entry_rest = (ScmObj)AO_load(entry_keys+fixed_keys);
        SCM_FOR_EACH(rp, rest) {
            if (!SCM_PAIRP(entry_rest)) return FALSE;
            if (!equal_1(SCM_CAR(rp), SCM_CAR(entry_rest))) return FALSE;
            entry_rest = SCM_CDR(entry_rest);
        }
    }
    return TRUE;
}

/*
 * lookup
 */

/* Returns #<unbound> if entry not found.
   Be careful not to leak it to Scheme. */
ScmObj Scm_MemoTableGetv(ScmMemoTable *tab, ScmObj *keys, int nkeys)
{
    int rest_keys;
    int fixed_keys = get_numkeys(tab->num_keys, &rest_keys);
    if (fixed_keys+rest_keys != nkeys) return SCM_UNBOUND;
    u_long hashv = memo_hashv(keys, tab->num_keys);
    ScmAtomicWord hashv_hdr = (ScmAtomicWord)((hashv<<1)+1);

    /* storage pointer may be swapped by another thread, but we're going to
       operate on the current snapshot.*/
    ScmMemoTableStorage *st = tab->storage;

    for (u_long i = 0; i < st->capacity / 2; i++) {
        u_long k = (hashv+i) % st->capacity;
        u_long idx = k * tab->entry_size;
        ScmAtomicWord entry_hdr = AO_load(&st->vec[idx]);
        if (entry_hdr == 0) return SCM_UNBOUND; /* not found */
        if ((entry_hdr & 0x01) == 0
            || entry_hdr != hashv_hdr) {
            /* the entry is invalid, someone's working on it,
               or it is with other hash value. */
            continue;
        }
        if (memo_equalv(keys, nkeys, &st->vec[idx+1])) {
            /* found */
            return SCM_OBJ(st->vec[idx + nkeys + 1]);
        }
    }
    return SCM_UNBOUND;
}

ScmObj Scm_MemoTableGet(ScmMemoTable *tab, ScmObj keys)
{
    int rest_keys;
    int fixed_keys = get_numkeys(tab->num_keys, &rest_keys);
    ScmSmallInt given_keys = Scm_Length(keys);
    if ((!rest_keys && given_keys != fixed_keys)
        || (rest_keys && given_keys < fixed_keys)) return SCM_UNBOUND;

    ScmObj keyv[fixed_keys+rest_keys];
    for (int i=0; i<fixed_keys; i++) {
        keyv[i] = SCM_CAR(keys);
        keys = SCM_CDR(keys);
    }
    if (rest_keys) keyv[fixed_keys] = keys;

    return Scm_MemoTableGetv(tab, keyv, fixed_keys+rest_keys);
}

/*
 * insert
 */

ScmObj Scm_MemoTablePutv(ScmMemoTable *tab, ScmObj *keys, int nkeys, ScmObj val)
{
    int rest_keys;
    int fixed_keys = get_numkeys(tab->num_keys, &rest_keys);
    if (nkeys != fixed_keys+rest_keys) return SCM_FALSE;
    u_long hashv = memo_hashv(keys, tab->num_keys);
    ScmAtomicWord hashv_hdr = (ScmAtomicWord)((hashv<<1)+1);

    ScmMemoTableStorage *st = tab->storage;
    ScmVM *self = Scm_VM();

    for (u_long i = 0; i < st->capacity / 2; i++) {
        u_long k = (hashv+i) % st->capacity;
        u_long idx = k * tab->entry_size;
        ScmAtomicWord entry_hdr = AO_load(&st->vec[idx]);
        if (entry_hdr == 0) {
            /* The table doesn't have the key.  Try to claim this entry. */
            if (AO_compare_and_swap_full(&st->vec[idx], entry_hdr,
                                         (ScmAtomicWord)self)) {
                for (int ik = 0; ik < fixed_keys; ik++) {
                    AO_store(&st->vec[idx+1+ik], (ScmAtomicWord)keys[ik]);
                }
                if (rest_keys) {
                    SCM_ASSERT(SCM_LISTP(keys[fixed_keys]));
                    AO_store(&st->vec[idx+1+fixed_keys],
                             (ScmAtomicWord)keys[fixed_keys]);
                }

                AO_store(&st->vec[idx+1+fixed_keys+rest_keys],
                         (ScmAtomicWord)val);
                AO_store_full(&st->vec[idx], hashv_hdr);
                return SCM_TRUE;
            } else {
                continue;
            }
        }
        if ((entry_hdr & 0x01) == 0
            || entry_hdr != hashv_hdr) {
            /* the entry is invalid, someone's working on it,
               or it is with other hash value. */
            continue;
        }
        if (memo_equalv(keys, nkeys, &st->vec[idx+1])) {
            /* We already have the entry.*/
            return SCM_TRUE;
        }
    }
    /* Table is too crowded. */
    /* TODO: Extend the table if !SCM_MEMO_TABLE_FIXED */
    return SCM_FALSE;
}

ScmObj Scm_MemoTablePut(ScmMemoTable *tab, ScmObj keys, ScmObj val)
{
    int rest_keys;
    int fixed_keys = get_numkeys(tab->num_keys, &rest_keys);
    ScmSmallInt given_keys = Scm_Length(keys);
    if ((!rest_keys && given_keys != fixed_keys)
        || (rest_keys && given_keys < fixed_keys)) return SCM_UNBOUND;

    ScmObj keyv[fixed_keys+rest_keys];
    for (int i=0; i<fixed_keys; i++) {
        keyv[i] = SCM_CAR(keys);
        keys = SCM_CDR(keys);
    }
    if (rest_keys) keyv[fixed_keys] = keys;

    return Scm_MemoTablePutv(tab, keyv, fixed_keys+rest_keys, val);
}

/*
 * for debugging
 */

static inline void dump_1(ScmAtomicWord entry, int valid, ScmPort *port)
{
    if (valid && entry) {
        Scm_Printf(port, "     %S\n", SCM_OBJ(entry));
    } else if (valid) {
        Scm_Printf(port, "     #null\n");
    } else {
        Scm_Printf(port, "     #unused\n");
    }
}

/* This is not entirely thread-safe.  We assume this is called manually
   during interactive debugging. */
void Scm__MemoTableDump(ScmMemoTable *tab, ScmPort *port)
{
    Scm_Printf(port, "memo-tabpe %p (num_keys=%d entry_size=%d capacity=%d",
               tab, tab->num_keys, tab->entry_size, tab->storage->capacity);
    if (tab->flags & SCM_MEMO_TABLE_WEAK) {
        Scm_Printf(port, " weak");
    }
    if (tab->flags & SCM_MEMO_TABLE_FIXED) {
        Scm_Printf(port, " fixed");
    }
    Scm_Printf(port, ")\n");
    u_long num_entries = tab->storage->capacity * tab->entry_size;
    int rest_keys;
    int fixed_keys = get_numkeys(tab->num_keys, &rest_keys);
    for (u_long i = 0; i < num_entries; i += tab->entry_size) {
        Scm_Printf(port, "%4d %08x\n", i/tab->entry_size,
                   tab->storage->vec[i]);
        int valid = tab->storage->vec[i] & 0x01;
        for (int j = 0; j < fixed_keys; j++) {
            ScmAtomicWord key = AO_load(&tab->storage->vec[i+1+j]);
            dump_1(key, valid, port);
        }
        if (rest_keys) {
            ScmAtomicWord keys = AO_load(&tab->storage->vec[i+1+fixed_keys]);
            dump_1(keys, valid, port);
        }

        ScmAtomicWord value =
            AO_load(&tab->storage->vec[i+1+fixed_keys+rest_keys]);
        dump_1(value, valid, port);
    }
}

/*
 * initialization
 */

void Scm__InitMemoTable(void)
{
    ScmModule *mod = Scm_GaucheModule();
    Scm_InitStaticClass(&Scm_MemoTableClass, "<memo-table>", mod, NULL, 0);
}
