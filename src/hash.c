/*
 * hash.c - hash table implementation
 *
 *   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
 *
 *  $Id: hash.c,v 1.33 2005-07-28 22:46:41 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"

/* Usually, "shift+add" scheme for string hasing works well.  But
 * I found that it works well if you take the lower bits.
 * Unfortunately, we need to take higher bits for multiplicative
 * hashing of integers and addresses.  So, in HASH2INDEX function,
 * I take both lower bits and higher bits.
 */

#define STRING_HASH(hv, chars, size)                                    \
    do {                                                                \
        int i_ = (size);                                                \
        (hv) = 0;                                                       \
        while (i_-- > 0) {                                              \
            (hv) = ((hv)<<5) - (hv) + ((unsigned char)*chars++);        \
        }                                                               \
    } while (0)

/* Integer and address hash is a variation of "multiplicative hashing"
   scheme described in Knuth, TAOCP, section 6.4.  The final shifting
   is done by HASH2INDEX macro  */

#define SMALL_INT_HASH(result, val) \
    ((result) = (val)*2654435761UL)

#define ADDRESS_HASH(result, val) \
    ((result) = (SCM_WORD(val) >> 3) * 2654435761UL)

#define DEFAULT_NUM_BUCKETS    4
#define MAX_AVG_CHAIN_LIMITS   3
#define EXTEND_BITS            2

/* NB: we fix the word length to 32bits, since the multiplication
   constant above is fixed. */
#define HASH2INDEX(tabsiz, bits, hashval) \
    (((hashval)+((hashval)>>(32-(bits)))) & ((tabsiz) - 1))

/* Combining two hash values.  We need better one. */
#define COMBINE(hv1, hv2)   ((hv1)*5+(hv2))

static unsigned int round2up(unsigned int val)
{
    unsigned int n = 1;
    while (n < val) {
        n <<= 1;
        SCM_ASSERT(n > 1);      /* check overflow */
    }
    return n;
}

/*------------------------------------------------------------
 *
 * Accessor functions
 *
 *   To accelerate lookup, we use specialized functions for string,
 *   small integer and address hash table.   They are called whenever
 *   hash table needs to be searched.   They take those four arguments:
 * 
 *     ScmHashTable *table : hash table
 *     ScmObj key          : key
 *     ScmObj value        : value, if the request involves modification.
 *     int mode            : mode of operation; one of those three:
 *                              HASH_FIND - just try to find the entry
 *                                          with key.  If no entry is found,
 *                                          returns NULL.
 *                              HASH_ADD  - if the entry is found, return
 *                                          it without modification.
 *                                          otherwise, add an entry with
 *                                          the given value.
 *                              HASH_UPDATE - if the entry is found, update
 *                                          the entry.  Otherwise, add a
 *                                          new entry with the given value.
 *                              HASH_DELETE - delete the found entry.
 */

/* CAVEAT: eq?, eqv?, and string=? hash tables are guaranteed not to
 * throw an error during hash table access (except the case that string=?
 * hash table gets non-string key).  So the caller doesn't need to
 * set unwind handler in case it needs cleanup (like unlocking mutex).
 * However, equal? hash may call back to Scheme method, so it can
 * throw Scheme error.  Be aware of that.
 */

enum {
    HASH_FIND,           /* returns NULL if not found */
    HASH_ADD,            /* add entry iff the key is not in the table */
    HASH_UPDATE,         /* modify entry if key exists; add otherwise */
    HASH_DELETE          /* remove matched entry */
};

/*
 * Common function called when the accessor function needs to add an entry.
 */
static ScmHashEntry *insert_entry(ScmHashTable *table,
                                  ScmObj key,
                                  ScmObj value,
                                  int index)
{
    ScmHashEntry *e = SCM_NEW(ScmHashEntry);
    e->key = key;
    e->value = value;
    e->next = table->buckets[index];
    table->buckets[index] = e;
    table->numEntries++;

    if (table->numEntries > table->numBuckets*MAX_AVG_CHAIN_LIMITS) {
        /* Extend the table */
        ScmHashEntry **newb, *f;
        ScmHashIter iter;
        int i, newsize = (table->numBuckets << EXTEND_BITS);
        int newbits = table->numBucketsLog2 + EXTEND_BITS;

        newb = SCM_NEW_ARRAY(ScmHashEntry*, newsize);
        for (i=0; i<newsize; i++) newb[i] = NULL;
        
        Scm_HashIterInit(table, &iter);
        while ((f = Scm_HashIterNext(&iter)) != NULL) {
            unsigned long hashval = table->hashfn(f->key);
            index = HASH2INDEX(newsize, newbits, hashval);
            f->next = newb[index];
            newb[index] = f;
        }
        table->numBuckets = newsize;
        table->numBucketsLog2 = newbits;
        table->buckets = newb;
    }
    return e;
}

static ScmHashEntry *delete_entry(ScmHashTable *table,
                                  ScmHashEntry *entry, ScmHashEntry *prev,
                                  int index)
{
    if (prev) prev->next = entry->next;
    else table->buckets[index] = entry->next;
    table->numEntries--;
    SCM_ASSERT(table->numEntries >= 0);
    return entry;
}

/*
 * Accessor function for address.   Used for EQ-type hash.
 */
static ScmHashEntry *address_access(ScmHashTable *table,
                                    ScmObj key, int mode, ScmObj value)
{
    unsigned long hashval, index;
    ScmHashEntry *e, *p;

    ADDRESS_HASH(hashval, key);
    index = HASH2INDEX(table->numBuckets, table->numBucketsLog2, hashval);
    
    for (e = table->buckets[index], p = NULL; e; p = e, e = e->next) {
        if (e->key == key) {
            if (mode == HASH_FIND || mode == HASH_ADD) return e;
            if (mode == HASH_DELETE) return delete_entry(table, e, p, index);
            else {
                e->value = value;
                return e;
            }
        }
    }

    if (mode == HASH_FIND || mode == HASH_DELETE) return NULL;
    else return insert_entry(table, key, value, index);
}

static unsigned long address_hash(ScmObj obj)
{
    unsigned long hashval;
    ADDRESS_HASH(hashval, obj);
    return hashval;
}

static int address_cmp(ScmObj key, ScmHashEntry *e)
{
    return (key == e->key ? 0 : -1);
}

/*
 * Accessor function for eqv-hash
 */
static unsigned long eqv_hash(ScmObj obj)
{
    unsigned long hashval;
    if (SCM_NUMBERP(obj)) {
        if (SCM_INTP(obj)) {
            SMALL_INT_HASH(hashval, SCM_INT_VALUE(obj));
        } else if (SCM_BIGNUMP(obj)) {
            int i;
            unsigned long u = 0;
            for (i=0; i<SCM_BIGNUM_SIZE(obj); i++) {
                u += SCM_BIGNUM(obj)->values[i];
            }
            SMALL_INT_HASH(hashval, u);
        } else if (SCM_FLONUMP(obj)) {
            /* TODO: I'm not sure this is a good hash. */
            hashval = (unsigned long)(SCM_FLONUM_VALUE(obj)*2654435761UL);
        } else {
            /* TODO: I'm not sure this is a good hash. */
            hashval = (unsigned long)((SCM_COMPLEX_REAL(obj)+SCM_COMPLEX_IMAG(obj))*2654435761UL);
        }
    } else {
        ADDRESS_HASH(hashval, obj);
    }
    return hashval;
}

static int eqv_cmp(ScmObj key, ScmHashEntry *e)
{
    return (Scm_EqvP(key, e->key) ? 0 : -1);
}

/*
 * Accessor function for string type.
 */
static ScmHashEntry *string_access(ScmHashTable *table, ScmObj key,
                                   int mode, ScmObj value)
{
    unsigned long hashval, index;
    int size;
    const char *s;
    ScmHashEntry *e, *p;
    
    if (!SCM_STRINGP(key)) {
        Scm_Error("Got non-string key %S to the string hashtable %S",
                  key, table);
    }
    s = SCM_STRING_START(key);
    size = SCM_STRING_SIZE(key);
    STRING_HASH(hashval, s, size);
    index = HASH2INDEX(table->numBuckets, table->numBucketsLog2, hashval);

    for (e = table->buckets[index], p = NULL; e; p = e, e = e->next) {
        ScmObj ee = e->key;
        int eesize = SCM_STRING_SIZE(ee);
        if (size == eesize
            && memcmp(SCM_STRING_START(key), SCM_STRING_START(ee), eesize) == 0){
            if (mode == HASH_FIND || mode == HASH_ADD) return e;
            if (mode == HASH_DELETE) return delete_entry(table, e, p, index);
            else {
                e->value = value;
                return e;
            }
        }
    }

    if (mode == HASH_FIND || mode == HASH_DELETE) return NULL;
    else return insert_entry(table, key, value, index);
}

static unsigned long string_hash(ScmObj obj)
{
    unsigned long hashval;
    const char *p;
    
    if (!SCM_STRINGP(obj)) return 0;
    p = SCM_STRING_START(obj);
    STRING_HASH(hashval, p, SCM_STRING_SIZE(obj));
    return hashval;
}

static int string_cmp(ScmObj key, ScmHashEntry *e)
{
    ScmObj kk = e->key;
    int siz = SCM_STRING_SIZE(kk);
    
    if (!SCM_STRINGP(key)) return -1;
    if (SCM_STRING_SIZE(key) != siz) return -1;
    return memcmp(SCM_STRING_START(key), SCM_STRING_START(kk), siz);
}

/*
 * Accessor function for general case
 *    (hashfn and cmpfn are given by user)
 */
static ScmHashEntry *general_access(ScmHashTable *table, ScmObj key,
                                    int mode, ScmObj value)
{
    unsigned long hashval, index;
    ScmHashEntry *e, *p;

    hashval = table->hashfn(key);
    index = HASH2INDEX(table->numBuckets, table->numBucketsLog2, hashval);
    
    for (e = table->buckets[index], p = NULL; e; p = e, e = e->next) {
        if (table->cmpfn(key, e) == 0) {
            if (mode == HASH_FIND || mode == HASH_ADD) return e;
            if (mode == HASH_DELETE) return delete_entry(table, e, p, index);
            else {
                e->value = value;
                return e;
            }
        }
    }

    if (mode == HASH_FIND || mode == HASH_DELETE) return NULL;
    else return insert_entry(table, key, value, index);
}

/* generic scheme object hash.
   note that this doesn't stop if the structure is circular */
static unsigned long general_hash(ScmObj obj)
{
    if (!SCM_PTRP(obj)) {
        unsigned long hv;
        SMALL_INT_HASH(hv, (unsigned long)obj);
        return hv;
    } else if (SCM_NUMBERP(obj)) {
        return eqv_hash(obj);
    } else if (SCM_STRINGP(obj)) {
        return string_hash(obj);
    } else if (SCM_PAIRP(obj)) {
        unsigned long h = 0, h2;
        ScmObj cp;
        SCM_FOR_EACH(cp, obj) {
            h2 = general_hash(SCM_CAR(cp));
            h = COMBINE(h, h2);
        }
        h2 = general_hash(cp);
        h = COMBINE(h, h2);
        return h;
    } else if (SCM_VECTORP(obj)) {
        int i;
        unsigned long h = 0, h2;
        ScmObj elt;
        SCM_VECTOR_FOR_EACH(i, elt, obj) {
            h2 = general_hash(elt);
            h = COMBINE(h, h2);
        }
        return h;
    } else if (SCM_SYMBOLP(obj)) {
        return string_hash(SCM_OBJ(SCM_SYMBOL_NAME(obj)));
    } else if (SCM_KEYWORDP(obj)) {
        return string_hash(SCM_OBJ(SCM_KEYWORD_NAME(obj)));
    } else {
        /* Call specialized object-hash method */
        ScmObj r = Scm_Apply(SCM_OBJ(&Scm_GenericObjectHash),
                             SCM_LIST1(obj));
        if (SCM_INTP(r)) {
            return (unsigned long)SCM_INT_VALUE(r);
        }
        if (SCM_BIGNUMP(r)) {
            /* NB: Scm_GetUInteger clamps the result to [0, ULONG_MAX],
               but taking the LSW would give better distribution. */
            return SCM_BIGNUM(r)->values[0];
        }
        Scm_Error("object-hash returned non-integer: %S", r);
        return 0;               /* dummy */
    }
}

static int general_cmp(ScmObj key, ScmHashEntry *e)
{
    return (Scm_EqualP(key, e->key)? 0 : -1);
}

/*---------------------------------------------------------
 * Constructor
 */

static void hash_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
SCM_DEFINE_BUILTIN_CLASS(Scm_HashTableClass, hash_print, NULL, NULL, NULL,
                         SCM_CLASS_COLLECTION_CPL);


ScmObj Scm_MakeHashTable(ScmHashProc hashfn,
                         ScmHashCmpProc cmpfn,
                         unsigned int initSize)
{
    ScmHashTable *z;
    ScmHashEntry **b;
    int i;
    
    if (initSize != 0) initSize = round2up(initSize);
    else initSize = DEFAULT_NUM_BUCKETS;

    b = SCM_NEW_ARRAY(ScmHashEntry*, initSize);
    z = SCM_NEW(ScmHashTable);
    SCM_SET_CLASS(z, SCM_CLASS_HASH_TABLE);
    z->buckets = b;
    z->numBuckets = initSize;
    z->numEntries = 0;
    for (i=initSize, z->numBucketsLog2=0; i > 1; i /= 2) {
        z->numBucketsLog2++;
    }

    for (i=0; i<initSize; i++) z->buckets[i] = NULL;
    
    if (hashfn == (ScmHashProc)SCM_HASH_ADDRESS) {
        z->type = SCM_HASH_ADDRESS;
        z->accessfn = address_access;
        z->hashfn = address_hash;
        z->cmpfn = address_cmp;
    } else if (hashfn == (ScmHashProc)SCM_HASH_EQV) {
        z->type = SCM_HASH_EQV;
        z->accessfn = general_access;
        z->hashfn = eqv_hash;
        z->cmpfn =  eqv_cmp;
    } else if (hashfn == (ScmHashProc)SCM_HASH_EQUAL) {
        z->type = SCM_HASH_EQUAL;
        z->accessfn = general_access;
        z->hashfn = general_hash;
        z->cmpfn =  general_cmp;
    } else if (hashfn == (ScmHashProc)SCM_HASH_STRING) {
        z->type = SCM_HASH_STRING;
        z->accessfn = string_access;
        z->hashfn = string_hash;
        z->cmpfn =  string_cmp;
    } else {
        z->type = SCM_HASH_GENERAL;
        z->accessfn = general_access;
        z->hashfn = hashfn;
        z->cmpfn = (cmpfn ? cmpfn : general_cmp);
    }
    return SCM_OBJ(z);
}

/*
 * iteration
 */

void Scm_HashIterInit(ScmHashTable *hash, ScmHashIter *iter)
{
    int i;
    iter->table = hash;
    for (i=0; i<hash->numBuckets; i++) {
        if (hash->buckets[i]) {
            iter->currentBucket = i;
            iter->currentEntry = hash->buckets[i];
            return;
        }
    }
    iter->currentEntry = NULL;
}

ScmHashEntry *Scm_HashIterNext(ScmHashIter *iter)
{
    ScmHashEntry *e = iter->currentEntry;
    if (e != NULL) {
        if (e->next) iter->currentEntry = e->next;
        else {
            int i = iter->currentBucket + 1;
            for (; i < iter->table->numBuckets; i++) {
                if (iter->table->buckets[i]) {
                    iter->currentBucket = i;
                    iter->currentEntry = iter->table->buckets[i];
                    return e;
                }
            }
            iter->currentEntry = NULL;
        }
    }
    return e;
}

/*
 * Search
 */

ScmHashEntry *Scm_HashTableGet(ScmHashTable *table, ScmObj key)
{
    return table->accessfn(table, key, HASH_FIND, SCM_FALSE);
}

ScmHashEntry *Scm_HashTableAdd(ScmHashTable *table,
                               ScmObj key, ScmObj value)
{
    return table->accessfn(table, key, HASH_ADD, value);
}

ScmHashEntry *Scm_HashTablePut(ScmHashTable *table,
                               ScmObj key, ScmObj value)
{
    return table->accessfn(table, key, HASH_UPDATE, value);
}

ScmHashEntry *Scm_HashTableDelete(ScmHashTable *table, ScmObj key)
{
    return table->accessfn(table, key, HASH_DELETE, SCM_FALSE);
}

/*
 * Utilities
 */

ScmObj Scm_HashTableKeys(ScmHashTable *table)
{
    ScmHashIter iter;
    ScmHashEntry *e;
    ScmObj h = SCM_NIL, t = SCM_NIL;
    Scm_HashIterInit(table, &iter);
    while ((e = Scm_HashIterNext(&iter)) != NULL) {
        SCM_APPEND1(h, t, e->key);
    }
    return h;
}

ScmObj Scm_HashTableValues(ScmHashTable *table)
{
    ScmHashIter iter;
    ScmHashEntry *e;
    ScmObj h = SCM_NIL, t = SCM_NIL;
    Scm_HashIterInit(table, &iter);
    while ((e = Scm_HashIterNext(&iter)) != NULL) {
        SCM_APPEND1(h, t, e->value);
    }
    return h;
}

ScmObj Scm_HashTableStat(ScmHashTable *table)
{
    ScmObj h = SCM_NIL, t;
    ScmVector *v = SCM_VECTOR(Scm_MakeVector(table->numBuckets, SCM_NIL));
    ScmObj *vp;
    int i;
    
    SCM_APPEND1(h, t, SCM_MAKE_KEYWORD("num-entries"));
    SCM_APPEND1(h, t, Scm_MakeInteger(table->numEntries));
    SCM_APPEND1(h, t, SCM_MAKE_KEYWORD("num-buckets"));
    SCM_APPEND1(h, t, Scm_MakeInteger(table->numBuckets));
    SCM_APPEND1(h, t, SCM_MAKE_KEYWORD("num-buckets-log2"));
    SCM_APPEND1(h, t, Scm_MakeInteger(table->numBucketsLog2));
    for (vp = SCM_VECTOR_ELEMENTS(v), i = 0; i<table->numBuckets; i++, vp++) {
        ScmHashEntry *e = table->buckets[i];
        for (; e; e = e->next) {
            *vp = Scm_Acons(e->key, e->value, *vp);
        }
    }
    SCM_APPEND1(h, t, SCM_MAKE_KEYWORD("contents"));
    SCM_APPEND1(h, t, SCM_OBJ(v));
    return h;
}

/*
 * Printer
 */

static void hash_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmHashTable *ht = (ScmHashTable*)obj;
    char *str;

    switch (ht->type) {
    case (int)SCM_HASH_ADDRESS: str = "eq?"; break;
    case (int)SCM_HASH_EQV:     str = "eqv?"; break;
    case (int)SCM_HASH_EQUAL:   str = "equal?"; break;
    case (int)SCM_HASH_STRING:  str = "string=?"; break;
    default: str = "general"; break;
    }

#if 0
    /* Use read-time constructor so that table can be read back
       --- is it necessary?  I'm not sure yet. */
    Scm_Printf(port, "#,(<hash-table> %s", str);
    if (ht->numEntries > 0) {
        Scm_HashIterInit(ht, &iter);
        while ((e = Scm_HashIterNext(&iter)) != NULL) {
            Scm_Printf(port, " %S %S", e->key, e->value);
        }
    }
    SCM_PUTZ(")", -1, port);
#else
    Scm_Printf(port, "#<hash-table %s %p>", str, ht);
#endif
}

/*
 * Exposed hash functions
 */

unsigned long Scm_Hash(ScmObj obj)
{
    return general_hash(obj);
}

unsigned long Scm_HashString(ScmString *str, unsigned long modulo)
{
    u_long hashval;
    hashval = string_hash(SCM_OBJ(str));
    return (hashval % modulo);
}

unsigned long Scm_EqHash(ScmObj obj)
{
    return address_hash(obj);
}

unsigned long Scm_EqvHash(ScmObj obj)
{
    return eqv_hash(obj);
}
