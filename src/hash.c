/*
 * hash.c - hash table implementation
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: hash.c,v 1.21 2002-02-04 09:28:40 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"

/* 
 * Finding good hash functions is an interesting topic.  I looked around
 * the net and find some valuable informations such as those:
 *
 *  http://burtleburtle.net/bob/hash/doobs.html
 *  http://www.concentric.net/~Ttwang/tech/inthash.htm
 *
 * In our case, the most frequent use of hash tables are to hash symbol
 * names (for intern), and integers when we want to combine hash results
 * of the components (e.g. hasing a pair).   Symbol names tend to have
 * relatively small length, and the table size tends to be small,
 * Bob's function seems a bit overkill.
 *
 * I also looked at Tcl's hash function (by Ousterhout) and Perl's
 * (by Larry Wall), both uses multiplicate-and-add method.   A few test
 * over the Scheme code I have, Bob's one showed the best hash result,
 * but the lead was small.   Ousterhout's one was slightly better tha
 * Larry's.   So I choose Ousterhout's here.
 *
 * If you want to deal with large amount of data, the default hash function
 * may not be suitable.
 */

#define STRING_HASH(result, chars, size)                                \
    do {                                                                \
        int i_ = (size);                                                \
        (result) = 0;                                                   \
        while (i_-- > 0) {                                              \
            (result) += ((result) << 3) + (unsigned char)*chars++;      \
        }                                                               \
    } while (0)

#define SMALL_INT_HASH(result, val) \
    ((result) = (val)*2654435761UL)

#define ADDRESS_HASH(result, val) \
    ((result) = (SCM_WORD(val) >> 3) * 2654435761UL)

#define GENERAL_INT_HASH(result, val)           \
    do {                                        \
         (result) = (val);                      \
         (result) += (val) << 12;               \
         (result) ^= (val) >> 22;               \
         (result) += (val) << 4;                \
         (result) ^= (val) >> 9;                \
         (result) += (val) << 10;               \
         (result) ^= (val) >> 2;                \
         (result) += (val) << 7;                \
         (result) ^= (val) >> 12;               \
    } while (0)

#define DEFAULT_NUM_BUCKETS    4
#define MAX_AVG_CHAIN_LIMITS   3
#define EXTEND_FACTOR          4

#define HASH2INDEX(table, hashval)   (hashval & (table->numBuckets - 1))

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
        int i, newsize = table->numBuckets * EXTEND_FACTOR;
        
        newb = SCM_NEW2(ScmHashEntry **, sizeof(ScmHashEntry*) * newsize);
        for (i=0; i<newsize; i++) newb[i] = NULL;
        
        Scm_HashIterInit(table, &iter);
        while ((f = Scm_HashIterNext(&iter)) != NULL) {
            unsigned long hashval = table->hashfn(f->key);
            index = hashval & (newsize - 1);
            f->next = newb[index];
            newb[index] = f;
        }
        table->numBuckets = newsize;
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
    index = HASH2INDEX(table, hashval);
    
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
        Scm_Abort("Got non-string key to the string hashtable");
    }
    s = SCM_STRING_START(key);
    size = SCM_STRING_SIZE(key);
    STRING_HASH(hashval, s, size);
    index = HASH2INDEX(table, hashval);

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
    index = HASH2INDEX(table, hashval);
    
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

/* generic scheme object hash */
static unsigned long general_hash(ScmObj obj)
{
    if (!SCM_PTRP(obj)) {
        return (unsigned long)obj;
    } else if (SCM_NUMBERP(obj)) {
        return eqv_hash(obj);
    } else if (SCM_STRINGP(obj)) {
        return string_hash(obj);
    } else if (SCM_PAIRP(obj)) {
        /* Note: this diverges when we have circular structure */
        return (general_hash(SCM_CAR(obj)) + general_hash(SCM_CDR(obj)));
    } else if (SCM_VECTORP(obj)) {
        int i;
        unsigned long h = 0;
        ScmObj elt;
        SCM_VECTOR_FOR_EACH(i, elt, obj) {
            h += general_hash(elt);
        }
        return h;
    } else if (SCM_SYMBOLP(obj)) {
        return string_hash(SCM_OBJ(SCM_SYMBOL_NAME(obj)));
    } else if (SCM_KEYWORDP(obj)) {
        return string_hash(SCM_OBJ(SCM_KEYWORD_NAME(obj)));
    } else {
        /* TODO: allow to define per-object hash function */
        Scm_Error("object not hashable: %S", obj);
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

    b = SCM_NEW2(ScmHashEntry**, sizeof(ScmHashEntry)*initSize);
    z = SCM_NEW(ScmHashTable);
    SCM_SET_CLASS(z, SCM_CLASS_HASHTABLE);
    z->buckets = b;
    z->numBuckets = initSize;
    z->numEntries = 0;

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
    SCM_APPEND1(h, t, SCM_MAKE_KEYWORD("max-chain-length"));
    SCM_APPEND1(h, t, Scm_MakeInteger(table->maxChainLength));
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
    ScmHashIter iter;
    ScmHashEntry *e;
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

unsigned long Scm_HashString(ScmString *str, unsigned long modulo)
{
    u_long hashval;
    hashval = string_hash(SCM_OBJ(str));
    return (hashval % modulo);
}
