/*
 * hash.c - hash table implementation
 *
 *   Copyright (c) 2000-2005 Shiro Kawai, All rights reserved.
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
 *  $Id: hash.c,v 1.46 2006-12-08 23:06:09 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"

/*-------------------------------------------------------------
 * Some macros & utilities
 */

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

/* In C-level, hash table can be used to contain arbitrary C data.
   There are some pre-wired hashtables that can restrict the data
   it holds to ScmObj.  We call such type "ScmObj hashtables", while
   the other ones "raw hashtables".

   Naturally, raw hashtables are only accessible from C-world; even
   if it leak out to the Scheme world, you can't access it.

   For the convenience, hash-table accessor API comes in pairs; those
   who has 'Raw' in the name can access any hashtables, while another
   one checks whether the given hashtable is an ScmObj hashtable,
   and rejects if not. */

/* internal utility to reject non-ScmObj hashtables. */
static void check_scm_hashtable(ScmHashTable *table)
{
    if (SCM_HASH_TABLE_RAW_P(table)) {
        Scm_Error("you can't access the raw hash table %S from Scheme",
                  table);        
    }
}

/*------------------------------------------------------------
 * Hash functions
 */

unsigned long Scm_EqHash(ScmObj obj)
{
    unsigned long hashval;
    ADDRESS_HASH(hashval, obj);
    return hashval;
}

unsigned long Scm_EqvHash(ScmObj obj)
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
        } else if (SCM_RATNUMP(obj)) {
            /* Ratnum must be normalized, so we can simply combine
               hashvals of numerator and denominator. */
            unsigned long h1 = Scm_EqvHash(SCM_RATNUM_NUMER(obj));
            unsigned long h2 = Scm_EqvHash(SCM_RATNUM_DENOM(obj));
            hashval = COMBINE(h1, h2);
        } else {
            /* TODO: I'm not sure this is a good hash. */
            hashval = (unsigned long)((SCM_COMPNUM_REAL(obj)+SCM_COMPNUM_IMAG(obj))*2654435761UL);
        }
    } else {
        ADDRESS_HASH(hashval, obj);
    }
    return hashval;
}

/* General hash function */
unsigned long Scm_Hash(ScmObj obj)
{
    unsigned long hashval;
    if (!SCM_PTRP(obj)) {
        SMALL_INT_HASH(hashval, (unsigned long)obj);
        return hashval;
    } else if (SCM_NUMBERP(obj)) {
        return Scm_EqvHash(obj);
    } else if (SCM_STRINGP(obj)) {
        goto string_hash;
    } else if (SCM_PAIRP(obj)) {
        unsigned long h = 0, h2;
        ScmObj cp;
        SCM_FOR_EACH(cp, obj) {
            h2 = Scm_Hash(SCM_CAR(cp));
            h = COMBINE(h, h2);
        }
        h2 = Scm_Hash(cp);
        h = COMBINE(h, h2);
        return h;
    } else if (SCM_VECTORP(obj)) {
        int i, siz = SCM_VECTOR_SIZE(obj);
        unsigned long h = 0, h2;
        for (i=0; i<siz; i++) {
            h2 = Scm_Hash(SCM_VECTOR_ELEMENT(obj, i));
            h = COMBINE(h, h2);
        }
        return h;
    } else if (SCM_SYMBOLP(obj)) {
        obj = SCM_OBJ(SCM_SYMBOL_NAME(obj));
        goto string_hash;
    } else if (SCM_KEYWORDP(obj)) {
        obj = SCM_OBJ(SCM_KEYWORD_NAME(obj));
        goto string_hash;
    } else {
        /* Call specialized object-hash method */
        ScmObj r = Scm_ApplyRec(SCM_OBJ(&Scm_GenericObjectHash),
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
  string_hash:
    {
        const char *p;
        const ScmStringBody *b = SCM_STRING_BODY(obj);
        p = SCM_STRING_BODY_START(b);
        STRING_HASH(hashval, p, SCM_STRING_BODY_SIZE(b));
        return hashval;
    }
}

unsigned long Scm_HashString(ScmString *str, unsigned long modulo)
{
    unsigned long hashval;
    const char *p;
    const ScmStringBody *b = SCM_STRING_BODY(str);
    p = SCM_STRING_BODY_START(b);
    STRING_HASH(hashval, p, SCM_STRING_BODY_SIZE(b));
    return (hashval % modulo);
}

/*------------------------------------------------------------
 * Parameterization
 *
 * Conceptually hash tables are parameterized by hash function and
 * compare function.  However, if they are trivial functions, calling
 * them via function pointers incur overhead.  So we layered the
 * parameterization.
 *
 * For the pre-defined simple hash tables, the calls to the hash and
 * compare functions are inlined in a single "access" function.
 * (In this case hashfn is only used for rehashing, and cmpfn is
 * never used).
 * For the generic hash tables, the general_access function uses
 * the info in hashfn and cmpfn fields.
 *
 * The accessor function takes four arguments.
 *
 *     ScmHashTable *table : hash table
 *     void *key           : key
 *     void *value         : value, if the request involves modification.
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

/* NOTE: eq?, eqv?, and string=? hash tables are guaranteed not to
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
        
        Scm_HashIterInitRaw(table, &iter);
        while ((f = Scm_HashIterNext(&iter)) != NULL) {
            unsigned long hashval = table->hashfn(table, f->key);
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
                                    void *key, int mode, void *value)
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

static unsigned long address_hash(ScmHashTable *ht, void *obj)
{
    unsigned long hashval;
    ADDRESS_HASH(hashval, obj);
    return hashval;
}

/*
 * Accessor function for equal and eqv-hash
 */
static unsigned long eqv_hash(ScmHashTable *table, void *key)
{
    return Scm_EqvHash(SCM_OBJ(key));
}

static int eqv_cmp(ScmHashTable *table, void *key, ScmHashEntry *e)
{
    return Scm_EqvP(SCM_OBJ(key), e->key);
}

static unsigned long equal_hash(ScmHashTable *table, void *key)
{
    return Scm_Hash(SCM_OBJ(key));
}

static int equal_cmp(ScmHashTable *table, void *key, ScmHashEntry *e)
{
    return Scm_EqualP(SCM_OBJ(key), SCM_OBJ(e->key));
}


/*
 * Accessor function for string type.
 */
static ScmHashEntry *string_access(ScmHashTable *table, void *k,
                                   int mode, void *v)
{
    unsigned long hashval, index;
    int size;
    const char *s;
    ScmObj key = SCM_OBJ(k), value = SCM_OBJ(v);
    ScmHashEntry *e, *p;
    const ScmStringBody *keyb;
    
    if (!SCM_STRINGP(key)) {
        Scm_Error("Got non-string key %S to the string hashtable %S",
                  key, table);
    }
    keyb = SCM_STRING_BODY(key);
    s = SCM_STRING_BODY_START(keyb);
    size = SCM_STRING_BODY_SIZE(keyb);
    STRING_HASH(hashval, s, size);
    index = HASH2INDEX(table->numBuckets, table->numBucketsLog2, hashval);

    for (e = table->buckets[index], p = NULL; e; p = e, e = e->next) {
        ScmObj ee = SCM_OBJ(e->key);
        const ScmStringBody *eeb = SCM_STRING_BODY(ee);
        int eesize = SCM_STRING_BODY_SIZE(eeb);
        if (size == eesize
            && memcmp(SCM_STRING_BODY_START(keyb),
                      SCM_STRING_BODY_START(eeb), eesize) == 0){
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

static unsigned long string_hash(ScmHashTable *table, void *key)
{
    unsigned long hashval;
    const char *p;
    const ScmStringBody *b = SCM_STRING_BODY(key);
    p = SCM_STRING_BODY_START(b);
    STRING_HASH(hashval, p, SCM_STRING_BODY_SIZE(b));
    return hashval;
}

/*
 * Accessor function for multiword raw hashtable.
 * Key points to an array of N words.
 */
static unsigned long multiword_hash(ScmHashTable *table, void *key)
{
    ScmWord keysize = (ScmWord)table->data;
    ScmWord *keyarray = (ScmWord*)key;
    unsigned long h = 0, h1;
    int i;
    for (i=0; i<keysize; i++) {
        ADDRESS_HASH(h1, keyarray[i]);
        h = COMBINE(h, h1);
    }
    return h;
}

static ScmHashEntry *multiword_access(ScmHashTable *table, void *k,
                                      int mode, void *v)
{
    unsigned long hashval, index;
    ScmWord keysize = (ScmWord)table->data;
    ScmHashEntry *e, *p;
    
    hashval = multiword_hash(table, k);
    index = HASH2INDEX(table->numBuckets, table->numBucketsLog2, hashval);

    for (e = table->buckets[index], p = NULL; e; p = e, e = e->next) {
        if (memcmp(k, e->key, keysize*sizeof(ScmWord)) == 0) {
            if (mode == HASH_FIND || mode == HASH_ADD) return e;
            if (mode == HASH_DELETE) return delete_entry(table, e, p, index);
            else {
                e->value = v;
                return e;
            }
        }
    }

    if (mode == HASH_FIND || mode == HASH_DELETE) return NULL;
    else return insert_entry(table, k, v, index);
}


/*
 * Accessor function for general case
 *    (hashfn and cmpfn are given by user)
 */
static ScmHashEntry *general_access(ScmHashTable *table, void *key,
                                    int mode, void *value)
{
    unsigned long hashval, index;
    ScmHashEntry *e, *p;

    hashval = table->hashfn(table, key);
    index = HASH2INDEX(table->numBuckets, table->numBucketsLog2, hashval);
    
    for (e = table->buckets[index], p = NULL; e; p = e, e = e->next) {
        if (table->cmpfn(table, key, e)) {
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

/*---------------------------------------------------------
 * Constructor
 */

static void hash_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);

SCM_DEFINE_BUILTIN_CLASS(Scm_HashTableClass, hash_print, NULL, NULL, NULL,
                         SCM_CLASS_COLLECTION_CPL);

static ScmObj make_hash_table(ScmClass *klass,
                              int type,
                              ScmHashAccessProc accessfn,
                              ScmHashProc hashfn,
                              ScmHashCmpProc cmpfn,
                              unsigned int initSize,
                              void *data)
{
    ScmHashTable *z;
    ScmHashEntry **b;
    int i;

    if (initSize != 0) initSize = round2up(initSize);
    else initSize = DEFAULT_NUM_BUCKETS;

    b = SCM_NEW_ARRAY(ScmHashEntry*, initSize);
    z = SCM_NEW(ScmHashTable);
    SCM_SET_CLASS(z, klass);
    z->buckets = b;
    z->numBuckets = initSize;
    z->numEntries = 0;
    z->type = type;
    z->accessfn = accessfn;
    z->hashfn = hashfn;
    z->cmpfn = cmpfn;
    z->data = data;
    for (i=initSize, z->numBucketsLog2=0; i > 1; i /= 2) {
        z->numBucketsLog2++;
    }
    for (i=0; i<initSize; i++) z->buckets[i] = NULL;
    return SCM_OBJ(z);
}

ScmObj Scm_MakeHashTableSimple(int type, int initSize)
{
    switch (type) {
    case SCM_HASH_EQ:
        return make_hash_table(SCM_CLASS_HASH_TABLE, SCM_HASH_EQ,
                               address_access, address_hash,
                               NULL, initSize, NULL);
    case SCM_HASH_EQV:
        return make_hash_table(SCM_CLASS_HASH_TABLE, SCM_HASH_EQV,
                               general_access, eqv_hash,
                               eqv_cmp, initSize, NULL);
    case SCM_HASH_EQUAL:
        return make_hash_table(SCM_CLASS_HASH_TABLE, SCM_HASH_EQUAL,
                               general_access, equal_hash,
                               equal_cmp, initSize, NULL);
    case SCM_HASH_STRING:
        return make_hash_table(SCM_CLASS_HASH_TABLE, SCM_HASH_STRING,
                               string_access, string_hash,
                               NULL, initSize, NULL);
    case SCM_HASH_WORD:
        return make_hash_table(SCM_CLASS_HASH_TABLE, SCM_HASH_WORD,
                               address_access, address_hash,
                               NULL, initSize, NULL);
    default:    
        Scm_Error("[internal error]: wrong TYPE argument passed to Scm_MakeHashTableSimple: %d", type);
        return SCM_UNDEFINED;   /* dummy */
    }
}

ScmObj Scm_MakeHashTableMultiWord(int keysize, int initsize)
{
    return make_hash_table(SCM_CLASS_HASH_TABLE, SCM_HASH_MULTIWORD,
                           multiword_access, multiword_hash,
                           NULL, initsize, (void*)SCM_WORD(keysize));
}

ScmObj Scm_MakeHashTableFull(ScmClass *klass, int type, ScmHashProc hashfn,
                             ScmHashCmpProc cmpfn, int initSize, void *data)
{
    if (!SCM_EQ(klass, SCM_CLASS_HASH_TABLE)) {
        if (!Scm_SubtypeP(klass, SCM_CLASS_HASH_TABLE)) {
            Scm_Error("[internal error]: non-hash-table class is given to Scm_MakeHashTableFull: %S", klass);
        }
    }

    switch (type) {
    case SCM_HASH_GENERAL:;
    case SCM_HASH_RAW:
        return make_hash_table(klass, type, general_access, hashfn,
                               cmpfn, initSize, data);
    default:    
        Scm_Error("[internal error]: wrong TYPE argument passed to Scm_MakeHashTableFull: %d", type);
        return SCM_UNDEFINED;   /* dummy */
    }
}

/* Legacy constructor.  DEPRECATED.  Will go away soon. */
ScmObj Scm_MakeHashTable(ScmHashProc hashfn,
                         ScmHashCmpProc cmpfn,
                         unsigned int initSize)
{
    if (hashfn == (ScmHashProc)SCM_HASH_EQ) {
        return Scm_MakeHashTableSimple(SCM_HASH_EQ, initSize);
    } else if (hashfn == (ScmHashProc)SCM_HASH_EQV) {
        return Scm_MakeHashTableSimple(SCM_HASH_EQV, initSize);
    } else if (hashfn == (ScmHashProc)SCM_HASH_EQUAL) {
        return Scm_MakeHashTableSimple(SCM_HASH_EQUAL, initSize);
    } else if (hashfn == (ScmHashProc)SCM_HASH_STRING) {
        return Scm_MakeHashTableSimple(SCM_HASH_STRING, initSize);
    } else {
        return Scm_MakeHashTableFull(SCM_CLASS_HASH_TABLE, SCM_HASH_GENERAL,
                                     hashfn, cmpfn, initSize, NULL);
    }
}

/*
 * iteration
 */

void Scm_HashIterInitRaw(ScmHashTable *table, ScmHashIter *iter)
{
    int i;
    iter->table = table;
    for (i=0; i<table->numBuckets; i++) {
        if (table->buckets[i]) {
            iter->currentBucket = i;
            iter->currentEntry = table->buckets[i];
            return;
        }
    }
    iter->currentEntry = NULL;
}

void Scm_HashIterInit(ScmHashTable *table, ScmHashIter *iter)
{
    check_scm_hashtable(table);
    Scm_HashIterInitRaw(table, iter);
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

ScmHashEntry *Scm_HashTableGetRaw(ScmHashTable *table, void *key)
{
    return table->accessfn(table, key, HASH_FIND, SCM_FALSE);
}

ScmHashEntry *Scm_HashTableAddRaw(ScmHashTable *table, void *key, void *value)
{
    return table->accessfn(table, key, HASH_ADD, value);
}

ScmHashEntry *Scm_HashTablePutRaw(ScmHashTable *table, void *key, void *value)
{
    return table->accessfn(table, key, HASH_UPDATE, value);
}

ScmHashEntry *Scm_HashTableDeleteRaw(ScmHashTable *table, void *key)
{
    return table->accessfn(table, key, HASH_DELETE, SCM_FALSE);
}

ScmHashEntry *Scm_HashTableGet(ScmHashTable *table, ScmObj key)
{
    check_scm_hashtable(table);
    return table->accessfn(table, key, HASH_FIND, SCM_FALSE);
}

ScmHashEntry *Scm_HashTableAdd(ScmHashTable *table, ScmObj key, ScmObj value)
{
    check_scm_hashtable(table);
    return table->accessfn(table, key, HASH_ADD, value);
}

ScmHashEntry *Scm_HashTablePut(ScmHashTable *table, ScmObj key, ScmObj value)
{
    check_scm_hashtable(table);
    return table->accessfn(table, key, HASH_UPDATE, value);
}

ScmHashEntry *Scm_HashTableDelete(ScmHashTable *table, ScmObj key)
{
    check_scm_hashtable(table);
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
    check_scm_hashtable(table);
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
    check_scm_hashtable(table);
    Scm_HashIterInit(table, &iter);
    while ((e = Scm_HashIterNext(&iter)) != NULL) {
        SCM_APPEND1(h, t, e->value);
    }
    return h;
}

ScmObj Scm_HashTableStat(ScmHashTable *table)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
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
    char *str = "";

    switch (ht->type) {
    case SCM_HASH_EQ:      str = "eq?"; break;
    case SCM_HASH_EQV:     str = "eqv?"; break;
    case SCM_HASH_EQUAL:   str = "equal?"; break;
    case SCM_HASH_STRING:  str = "string=?"; break;
    case SCM_HASH_GENERAL: str = "general"; break;

    case SCM_HASH_WORD:      str = "raw word"; break;
    case SCM_HASH_MULTIWORD: str = "raw multi-word"; break;
    case SCM_HASH_RAW:       str = "raw general"; break;

    default: Scm_Panic("something wrong with a hash table");
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

