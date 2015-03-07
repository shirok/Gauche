/*
 * sptab.c - Sparse hash table
 *
 *   Copyright (c) 2009-2015  Shiro Kawai  <shiro@acm.org>
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

typedef struct TLeafRec {
    Leaf hdr;                   /* data bit 0 indicates if key is chained */
    union {
        struct {
            ScmObj key;
            ScmObj value;
        } entry;
        struct {
            ScmObj next;        /* alist of ((key . value) ...) */
            ScmObj pair;        /* first (key . value) */
        } chain;
    };
} TLeaf;

static inline int leaf_is_chained(TLeaf *leaf)
{
    return leaf_data(LEAF(leaf))&1;
}

static inline void leaf_mark_chained(TLeaf *leaf)
{
    leaf_data_bit_set(LEAF(leaf), 0);
}

static inline void leaf_mark_unchained(TLeaf *leaf)
{
    leaf_data_bit_reset(LEAF(leaf), 0);
}

static Leaf *leaf_allocate(void *data)
{
    TLeaf *z = SCM_NEW(TLeaf);
    z->entry.key = z->entry.value = SCM_UNBOUND;
    return (Leaf*)z;
}

/*===================================================================
 * Constructor
 */

static u_long string_hash(ScmObj key)
{
    if (!SCM_STRINGP(key)) {
        Scm_Error("sparse string hashtable got non-string key: %S", key);
    }
    return Scm_HashString(SCM_STRING(key), 0);
}

static int string_cmp(ScmObj a, ScmObj b)
{
    if (!SCM_STRINGP(a)) {
        Scm_Error("sparse string hashtable got non-string key: %S", a);
    }
    if (!SCM_STRINGP(b)) {
        Scm_Error("sparse string hashtable got non-string key: %S", b);
    }
    return Scm_StringEqual(SCM_STRING(a), SCM_STRING(b));
}

ScmObj MakeSparseTable(ScmHashType type, ScmComparator *comparator,
                       u_long flags)
{
    SparseTable *v = SCM_NEW(SparseTable);
    SCM_SET_CLASS(v, SCM_CLASS_SPARSE_TABLE);
    CompactTrieInit(&v->trie);
    v->numEntries = 0;
    v->comparator = comparator;

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
    case SCM_HASH_GENERAL:
        SCM_ASSERT(comparator != NULL);
        v->hashfn = NULL;
        v->cmpfn = NULL;
        break;
    default:
        Scm_Error("invalid hash type (%d) for a sparse hash table", type);
    }
    return SCM_OBJ(v);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_SparseTableClass,
                         NULL, NULL, NULL, NULL,
                         SCM_CLASS_DICTIONARY_CPL);

static u_long sparse_table_hash(SparseTable *st, ScmObj key)
{
    if (st->hashfn) return st->hashfn(key);
    ScmObj h = st->comparator->hashFn;
    ScmObj r = Scm_ApplyRec1(h, key);
    if (!SCM_INTEGERP(r)) {
        Scm_Error("hash function %S returns non-integer: %S", h, r);
    }
    return Scm_GetIntegerU(r);
}

static int sparse_table_eq(SparseTable *st, ScmObj a, ScmObj b)
{
    if (st->cmpfn) return st->cmpfn(a, b);
    ScmObj e = st->comparator->eqFn;
    ScmObj r = Scm_ApplyRec2(e, a, b);
    return !SCM_FALSEP(r);
}

/*===================================================================
 * Lookup
 */

ScmObj SparseTableRef(SparseTable *st, ScmObj key, ScmObj fallback)
{
    u_long hv = sparse_table_hash(st, key);
    TLeaf *z = (TLeaf*)CompactTrieGet(&st->trie, hv);

    if (z != NULL) {
        if (!leaf_is_chained(z)) {
            if (sparse_table_eq(st, key, z->entry.key)) return z->entry.value;
            else return fallback;
        } else if (sparse_table_eq(st, key, SCM_CAR(z->chain.pair))) {
            return SCM_CDR(z->chain.pair);
        } else {
            ScmObj cp;
            SCM_FOR_EACH(cp, z->chain.next) {
                ScmObj p = SCM_CAR(cp);
                if (sparse_table_eq(st, key, SCM_CAR(p))) return SCM_CDR(p);
            }
        }
    }
    return fallback;
}

/*===================================================================
 * Insertion
 */

ScmObj SparseTableSet(SparseTable *st, ScmObj key, ScmObj value, int flags)
{
    int createp = !(flags&SCM_DICT_NO_CREATE);
    u_long hv = sparse_table_hash(st, key);
    TLeaf *z;

    if (!createp) {
        z = (TLeaf*)CompactTrieGet(&st->trie, hv);
        if (z == NULL) return SCM_UNBOUND;
    } else {
        z = (TLeaf*)CompactTrieAdd(&st->trie, hv, leaf_allocate, NULL);
    }

    if (!leaf_is_chained(z)) {
        if (SCM_UNBOUNDP(z->entry.key)) {
            /* new entry */
            z->entry.key = key;
            z->entry.value = value;
            st->numEntries++;
            return value;
        } else if (sparse_table_eq(st, z->entry.key, key)) {
            z->entry.value = value;
            return value;
        } else {
            ScmObj p = Scm_Cons(z->entry.key, z->entry.value);
            leaf_mark_chained(z);
            z->chain.next = SCM_NIL;
            z->chain.pair = p;
            /*FALLTHROUGH*/
        }
    }
    /* we got a chained entry. */
    if (sparse_table_eq(st, SCM_CAR(z->chain.pair), key)) {
        SCM_SET_CDR(z->chain.pair, value);
        return value;
    }
    ScmObj cp;
    SCM_FOR_EACH(cp, z->chain.next) {
        ScmObj p = SCM_CAR(cp);
        SCM_ASSERT(SCM_PAIRP(p));
        if (sparse_table_eq(st, SCM_CAR(p), key)) {
            SCM_SET_CDR(p, value);
            return value;
        }
    }
    z->chain.next = Scm_Cons(z->chain.pair, z->chain.next);
    z->chain.pair = Scm_Cons(key, value);
    st->numEntries++;
    return value;
}

/*===================================================================
 * Deletion
 */

/* returns value of the deleted entry, or SCM_UNBOUND if there's no entry */
ScmObj SparseTableDelete(SparseTable *st, ScmObj key)
{
    u_long hv = sparse_table_hash(st, key);
    TLeaf *z = (TLeaf*)CompactTrieGet(&st->trie, hv);
    ScmObj retval = SCM_UNBOUND;

    if (z != NULL) {
        if (!leaf_is_chained(z)) {
            if (sparse_table_eq(st, key, z->entry.key)) {
                retval = z->entry.value;
                CompactTrieDelete(&st->trie, hv);
                st->numEntries--;
            }
        } else {
            if (sparse_table_eq(st, key, SCM_CAR(z->chain.pair))) {
                ScmObj p = z->chain.next;
                SCM_ASSERT(SCM_PAIRP(p));
                retval = SCM_CDR(z->chain.pair);
                z->chain.pair = SCM_CAR(p);
                z->chain.next = SCM_CDR(p);
                st->numEntries--;
            } else {
                ScmObj cp, prev = SCM_FALSE;
                SCM_FOR_EACH(cp, z->chain.next) {
                    ScmObj pp = SCM_CAR(cp);
                    if (sparse_table_eq(st, key, SCM_CAR(pp))) {
                        retval = SCM_CDR(pp);
                        if (SCM_FALSEP(prev)) z->chain.next = SCM_CDR(cp);
                        else SCM_SET_CDR(prev, SCM_CDR(cp));
                        st->numEntries--;
                        break;
                    }
                    prev = cp;
                }
            }
            /* make sure we have more than one entry in a chained leaf */
            if (SCM_NULLP(z->chain.next)) {
                ScmObj p = z->chain.pair;
                leaf_mark_unchained(z);
                z->entry.key = SCM_CAR(p);
                z->entry.value = SCM_CDR(p);
            }
        }
    }
    return retval;
}

static void clear_leaf(Leaf *f, void *data)
{
    TLeaf *z = (TLeaf*)f;
    z->entry.key = z->entry.value = NULL;
}

void SparseTableClear(SparseTable *st)
{
    st->numEntries = 0;
    CompactTrieClear(&st->trie, clear_leaf, NULL);
}

/*===================================================================
 * Copy
 */

static Leaf *copy_leaf(Leaf *leaf, void *data)
{
    TLeaf *s = (TLeaf*)leaf;
    TLeaf *d = SCM_NEW(TLeaf);
    d->hdr = s->hdr;
    if (leaf_is_chained(s)) {
        ScmObj h = SCM_NIL, t = SCM_NIL, cp;
        d->chain.pair = Scm_Cons(SCM_CAR(s->chain.pair),
                                 SCM_CDR(s->chain.pair));
        SCM_FOR_EACH(cp, s->chain.next) {
            SCM_APPEND1(h, t, Scm_Cons(SCM_CAAR(cp), SCM_CDAR(cp)));
        }
        d->chain.next = h;
    } else {
        d->entry.key   = s->entry.key;
        d->entry.value = s->entry.value;
    }
    return (Leaf*)d;
}

ScmObj SparseTableCopy(const SparseTable *s)
{
    SparseTable *d = SCM_NEW(SparseTable);
    memcpy(d, s, sizeof(SparseTable));
    CompactTrieCopy(&d->trie, &s->trie, copy_leaf, NULL);
    return SCM_OBJ(d);
}

/*===================================================================
 * Iterators
 */

void SparseTableIterInit(SparseTableIter *it, SparseTable *st)
{
    it->st = st;
    CompactTrieIterInit(&it->ctit, &st->trie);
    it->chain = SCM_NIL;
    it->end = FALSE;
}

/* returns (key . value) or #f */
ScmObj SparseTableIterNext(SparseTableIter *it)
{
    if (it->end) return SCM_FALSE;
    if (SCM_PAIRP(it->chain)) {
        ScmObj p = SCM_CAR(it->chain);
        it->chain = SCM_CDR(it->chain);
        return p;
    } else {
        TLeaf *z = (TLeaf*)CompactTrieIterNext(&it->ctit);
        if (z == NULL) { it->end = TRUE; return SCM_FALSE; }
        if (!leaf_is_chained(z)) {
            return Scm_Cons(z->entry.key, z->entry.value);
        }
        it->chain = z->chain.next;
        return z->chain.pair;
    }
}


/*===================================================================
 * Miscellaneous
 */

static void leaf_dump(ScmPort *out, Leaf *leaf, int indent, void *data)
{
    TLeaf *z = (TLeaf*)leaf;

    if (leaf_is_chained(z)) {
        Scm_Printf(out, "(chained)");
        Scm_Printf(out, "\n  %*s%S => %25.1S", indent, "",
                   SCM_CAR(z->chain.pair), SCM_CDR(z->chain.pair));
        ScmObj cp;
        SCM_FOR_EACH(cp, z->chain.next) {
            ScmObj p = SCM_CAR(cp);
            SCM_ASSERT(SCM_PAIRP(p));
            Scm_Printf(out, "\n  %*s%S => %25.1S", indent, "",
                       SCM_CAR(p), SCM_CDR(p));
        }
    } else {
        Scm_Printf(out, "\n  %*s%S => %25.1S", indent, "",
                   z->entry.key, z->entry.value);
    }
}

void SparseTableDump(SparseTable *st)
{
    CompactTrieDump(SCM_CUROUT, &st->trie, leaf_dump, NULL);
}

void SparseTableCheck(SparseTable *st)
{
    CompactTrieCheck(&st->trie, SCM_OBJ(st), NULL);
}

/*===================================================================
 * Initialization
 */

void Scm_Init_sptab(ScmModule *mod)
{
    Scm_InitStaticClass(&Scm_SparseTableClass, "<sparse-table>",
                        mod, NULL, 0);
}
