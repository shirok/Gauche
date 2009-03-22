/*
 * sptab.c - Sparse hash table
 *
 *   Copyright (c) 2009  Shiro Kawai  <shiro@acm.org>
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

#include "sptab.h"

/*===================================================================
 * Leaf node manipulation
 */

typedef struct SPTLeafRec {
    Leaf hdr;                   /* if key0 > 65536, the entry is chained. */
    union {
        struct {
            ScmObj key;
            ScmObj value;
        } entry;
        struct {
            ScmObj next;        /* alist of ((key . value) ...) */
            ScmObj pair;        /* first (key . value) */
        } chain;
    } d;
} SPTLeaf;

#define LEAF_CHAIN_BIT 0x10000

static inline int leaf_is_chained(SPTLeaf *leaf)
{
    return leaf->hdr.key0 & LEAF_CHAIN_BIT;
}

static inline void leaf_mark_chained(SPTLeaf *leaf)
{
    leaf->hdr.key0 |= LEAF_CHAIN_BIT;
}

static inline void leaf_mark_unchained(SPTLeaf *leaf)
{
    leaf->hdr.key0 &= ~LEAF_CHAIN_BIT;
}

static Leaf *leaf_allocate(void *data)
{
    SPTLeaf *z = SCM_NEW(SPTLeaf);
    z->d.entry.key = z->d.entry.value = SCM_UNBOUND;
    return (Leaf*)z;
}

/*===================================================================
 * Constructor
 */

static u_long string_hash(ScmObj key)
{
    if (!SCM_STRINGP(key)) {
        Scm_Error("sparse string hashtable got non-string key:", key);
    }
    return Scm_HashString(SCM_STRING(key), 0);
}

static int string_cmp(ScmObj a, ScmObj b)
{
    if (!SCM_STRINGP(a)) {
        Scm_Error("sparse string hashtable got non-string key:", a);
    }
    if (!SCM_STRINGP(b)) {
        Scm_Error("sparse string hashtable got non-string key:", b);
    }
    return Scm_StringEqual(SCM_STRING(a), SCM_STRING(b));
}

ScmObj MakeSparseTable(ScmHashType type, u_long flags)
{
    SparseTable *v = SCM_NEW(SparseTable);
    SCM_SET_CLASS(v, SCM_CLASS_SPARSE_TABLE);
    CompactTrieInit(&v->trie);
    v->numEntries = 0;

    switch (type) {
    case SCM_HASH_EQ:
        v->hashfn = Scm_EqHash;
        v->cmpfn = Scm_EqP;
        break;
    case SCM_HASH_EQV:
        v->hashfn = Scm_EqvHash;
        v->cmpfn = Scm_EqvP;
        break;
    case SCM_HASH_EQUAL:
        v->hashfn = Scm_Hash;
        v->cmpfn = Scm_EqualP;
        break;
    case SCM_HASH_STRING:
        v->hashfn = string_hash;
        v->cmpfn = string_cmp;
        break;
    default:
        Scm_Error("invalid hash type (%d) for a sparse hash table", type);
    }
    return SCM_OBJ(v);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_SparseTableClass,
                         NULL, NULL, NULL, NULL, NULL);

/*===================================================================
 * Lookup
 */

ScmObj SparseTableRef(SparseTable *sh, ScmObj key, ScmObj fallback)
{
    u_long hv = sh->hashfn(key);
    SPTLeaf *z = (SPTLeaf*)CompactTrieGet(&sh->trie, hv);
    ScmObj cp;
    
    if (z != NULL) {
        if (!leaf_is_chained(z)) {
            if (sh->cmpfn(key, z->d.entry.key)) return z->d.entry.value;
            else return fallback;
        } else if (sh->cmpfn(key, SCM_CAR(z->d.chain.pair))) {
            return SCM_CDR(z->d.chain.pair);
        } else {
            SCM_FOR_EACH(cp, z->d.chain.next) {
                ScmObj p = SCM_CAR(cp);
                if (sh->cmpfn(key, SCM_CAR(p))) return SCM_CDR(p);
            }
        }
    }
    return fallback;
}

/*===================================================================
 * Insertion
 */

ScmObj SparseTableSet(SparseTable *sh, ScmObj key,
                          ScmObj value, int flags)
{
    int createp = !(flags&SCM_DICT_NO_CREATE);
    u_long hv = sh->hashfn(key);
    SPTLeaf *z;
    ScmObj cp;

    if (!createp) {
        z = (SPTLeaf*)CompactTrieGet(&sh->trie, hv);
        if (z == NULL) return SCM_UNBOUND;
    } else {
        z = (SPTLeaf*)CompactTrieAdd(&sh->trie, hv, leaf_allocate, NULL);
    }

    if (!leaf_is_chained(z)) {
        if (SCM_UNBOUNDP(z->d.entry.key)) {
            /* new entry */
            z->d.entry.key = key;
            z->d.entry.value = value;
            sh->numEntries++;
            return value;
        } else if (sh->cmpfn(z->d.entry.key, key)) {
            z->d.entry.value = value;
            return value;
        } else {
            ScmObj p = Scm_Cons(z->d.entry.key, z->d.entry.value);
            leaf_mark_chained(z);
            z->d.chain.next = SCM_NIL;
            z->d.chain.pair = p;
            /*FALLTHROUGH*/
        }
    }
    /* we got a chained entry. */
    if (sh->cmpfn(SCM_CAR(z->d.chain.pair), key)) {
        SCM_SET_CDR(z->d.chain.pair, value);
        return value;
    }
    SCM_FOR_EACH(cp, z->d.chain.next) {
        ScmObj p = SCM_CAR(cp);
        SCM_ASSERT(SCM_PAIRP(p));
        if (sh->cmpfn(SCM_CAR(p), key)) {
            SCM_SET_CDR(p, value);
            return value;
        }
    }
    z->d.chain.next = Scm_Cons(z->d.chain.pair, z->d.chain.next);
    z->d.chain.pair = Scm_Cons(key, value);
    sh->numEntries++;
    return value;
}

/*===================================================================
 * Miscellaneous
 */

static void clear_leaf(Leaf *f, void *data)
{
    SPTLeaf *z = (SPTLeaf*)f;
    z->d.entry.key = z->d.entry.value = NULL;
}

void SparseTableClear(SparseTable *st)
{
    st->numEntries = 0;
    CompactTrieClear(&st->trie, clear_leaf, NULL);
}

#if SCM_DEBUG_HELPER
static void leaf_dump(ScmPort *out, Leaf *leaf, int indent, void *data)
{
    SPTLeaf *z = (SPTLeaf*)leaf;
    ScmObj cp;

    if (leaf_is_chained(z)) {
        Scm_Printf(out, "(chained)");
        Scm_Printf(out, "\n  %*s%S => %25.1S", indent, "",
                   SCM_CAR(z->d.chain.pair), SCM_CDR(z->d.chain.pair));
        SCM_FOR_EACH(cp, z->d.chain.next) {
            ScmObj p = SCM_CAR(cp);
            SCM_ASSERT(SCM_PAIRP(p));
            Scm_Printf(out, "\n  %*s%S => %25.1S", indent,
                       SCM_CAR(p), SCM_CDR(p));
        }
    } else {
        Scm_Printf(out, "\n  %*s%S => %25.1S", indent, "",
                   z->d.entry.key, z->d.entry.value);
    }
}

void SparseTableDump(SparseTable *st)
{
    CompactTrieDump(SCM_CUROUT, &st->trie, leaf_dump, NULL);
}
#endif /*SCM_DEBUG_HELPER*/

/*===================================================================
 * Initialization
 */

void Scm_Init_sptab(ScmModule *mod)
{
    Scm_InitStaticClass(&Scm_SparseTableClass, "<sptable>",
                        mod, NULL, 0);
}


