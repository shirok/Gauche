/*
 * hash.c - hash table implementation
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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
#include "gauche/class.h"

/*============================================================
 * Internal structures
 */


/* The beginning of this structure must match ScmDictEntry. */
typedef struct EntryRec {
    intptr_t key;
    intptr_t value;
    struct EntryRec *next;
    u_long   hashval;
} Entry;

#define BUCKETS(hc)   ((Entry**)hc->buckets)

#define DEFAULT_NUM_BUCKETS    4
#define MAX_AVG_CHAIN_LIMITS   3
#define EXTEND_BITS            2

/* We limit hash value to 32bits, for it must be portable across platforms.
   (Especially EQUAL-hash value */
#define HASHMASK  0xffffffffUL

typedef Entry *SearchProc(ScmHashCore *core, intptr_t key, ScmDictOp op);

static unsigned int round2up(unsigned int val);

/*============================================================
 * Hash functions
 */

/* Hash function calculates 32bit hash value from the given object.
   HASH2INDEX macro maps the hash value to the bucket number.
   (On 64 bit architecture, it's OK to calculate 64bit, but the
   upper bits are discarded by HASH2INDEX to maintain compatibility. */

/* For String
 *
 * Usually, "shift+add" scheme for string hasing works well.  But
 * I found that it works well only if you take the lower bits.
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

/* Integer and address. */
/* Integer and address hash is a variation of "multiplicative hashing"
   scheme described in Knuth, TAOCP, section 6.4.  The final shifting
   is done by HASH2INDEX macro  */

#define SMALL_INT_HASH(result, val) \
    (result) = ((val)*2654435761UL)

#define ADDRESS_HASH(result, val) \
    (result) = (u_long)((SCM_WORD(val) >> 3)*2654435761UL)

/* HASH2INDEX
   Map a hash value to bucket number.
   We fix the word length to 32bits, since the multiplication
   constant above is fixed. */
#define HASH2INDEX(tabsiz, bits, hashval) \
    (((hashval)+((hashval)>>(32-(bits)))) & ((tabsiz) - 1))

/* Combining two hash values. */
#define COMBINE(hv1, hv2)   ((hv1)*5+(hv2))

u_long Scm_EqHash(ScmObj obj)
{
    u_long hashval;
    ADDRESS_HASH(hashval, obj);
    return hashval&HASHMASK;
}

u_long Scm_EqvHash(ScmObj obj)
{
    u_long hashval;
    if (SCM_NUMBERP(obj)) {
        if (SCM_INTP(obj)) {
            SMALL_INT_HASH(hashval, SCM_INT_VALUE(obj));
        } else if (SCM_BIGNUMP(obj)) {
            u_int i;
            u_long u = 0;
            for (i=0; i<SCM_BIGNUM_SIZE(obj); i++) {
                u += SCM_BIGNUM(obj)->values[i];
            }
            SMALL_INT_HASH(hashval, u);
        } else if (SCM_FLONUMP(obj)) {
            /* TODO: I'm not sure this is a good hash. */
            hashval = (u_long)(SCM_FLONUM_VALUE(obj)*2654435761UL);
        } else if (SCM_RATNUMP(obj)) {
            /* Ratnum must be normalized, so we can simply combine
               hashvals of numerator and denominator. */
            u_long h1 = Scm_EqvHash(SCM_RATNUM_NUMER(obj));
            u_long h2 = Scm_EqvHash(SCM_RATNUM_DENOM(obj));
            hashval = COMBINE(h1, h2);
        } else {
            /* TODO: I'm not sure this is a good hash. */
            hashval = (u_long)((SCM_COMPNUM_REAL(obj)+SCM_COMPNUM_IMAG(obj))*2654435761UL);
        }
    } else {
        ADDRESS_HASH(hashval, obj);
    }
    return hashval&HASHMASK;
}

/* General hash function */
u_long Scm_Hash(ScmObj obj)
{
    u_long hashval;
    if (!SCM_PTRP(obj)) {
        SMALL_INT_HASH(hashval, (u_long)SCM_WORD(obj));
        return hashval;
    } else if (SCM_NUMBERP(obj)) {
        return Scm_EqvHash(obj);
    } else if (SCM_STRINGP(obj)) {
        goto string_hash;
    } else if (SCM_PAIRP(obj)) {
        u_long h = 0, h2;
        ScmObj cp;
        SCM_FOR_EACH(cp, obj) {
            h2 = Scm_Hash(SCM_CAR(cp));
            h = COMBINE(h, h2);
        }
        h2 = Scm_Hash(cp);
        h = COMBINE(h, h2);
        return h;
    } else if (SCM_VECTORP(obj)) {
        int siz = SCM_VECTOR_SIZE(obj);
        u_long h = 0, h2;
        for (int i=0; i<siz; i++) {
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
            return (u_long)SCM_INT_VALUE(r);
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
        const ScmStringBody *b = SCM_STRING_BODY(obj);
        const char *p = SCM_STRING_BODY_START(b);
        STRING_HASH(hashval, p, SCM_STRING_BODY_SIZE(b));
        return hashval;
    }
}

u_long Scm_HashString(ScmString *str, u_long modulo)
{
    u_long hashval;
    const ScmStringBody *b = SCM_STRING_BODY(str);
    const char *p = SCM_STRING_BODY_START(b);
    STRING_HASH(hashval, p, SCM_STRING_BODY_SIZE(b));
    if (modulo == 0) return hashval;
    else return (hashval % modulo);
}

/* Expose COMBINE. */
u_long Scm_CombineHashValue(u_long a, u_long b)
{
    u_long c = COMBINE(a, b);
#if SIZEOF_LONG == 8
    /* we limit portable hash value to 32bit. */
    c &= 0xffffffff;
#endif /**/
    return c;
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
 * (In this case hashfn and cmpfn are never used.)
 * For the generic hash tables, the general_access function uses
 * the info in hashfn and cmpfn fields.
 *
 * The accessor function takes three arguments.
 *
 *     ScmHashCore *core   : hash table core
 *     intptr_t key        : key
 *     ScmDictOp op        : operation
 */

/* NOTE: eq?, eqv?, and string=? hash tables are guaranteed not to
 * throw an error during hash table access (except the case that string=?
 * hash table gets non-string key).  So the caller doesn't need to
 * set unwind handler in case it needs cleanup (like unlocking mutex).
 * However, equal? hash may call back to Scheme method, so it can
 * throw Scheme error.  Be aware of that.
 */

/*
 * Common function called when the accessor function needs to add an entry.
 */
static Entry *insert_entry(ScmHashCore *table,
                           intptr_t key,
                           u_long   hashval,
                           int index)
{
    Entry *e = SCM_NEW(Entry);
    Entry **buckets = BUCKETS(table);
    e->key = key;
    e->value = 0;
    e->next = buckets[index];
    e->hashval = hashval;
    buckets[index] = e;
    table->numEntries++;

    if (table->numEntries > table->numBuckets*MAX_AVG_CHAIN_LIMITS) {
        /* Extend the table */
        int newsize = (table->numBuckets << EXTEND_BITS);
        int newbits = table->numBucketsLog2 + EXTEND_BITS;

        Entry **newb = SCM_NEW_ARRAY(Entry*, newsize);
        for (int i=0; i<newsize; i++) newb[i] = NULL;

        ScmHashIter iter;
        Entry *f;
        Scm_HashIterInit(&iter, table);
        while ((f = (Entry*)Scm_HashIterNext(&iter)) != NULL) {
            index = HASH2INDEX(newsize, newbits, f->hashval);
            f->next = newb[index];
            newb[index] = f;
        }
        /* gc friendliness */
        for (int i=0; i<table->numBuckets; i++) table->buckets[i] = NULL;

        table->numBuckets = newsize;
        table->numBucketsLog2 = newbits;
        table->buckets = (void**)newb;
    }
    return e;
}

/* NB: Deleting entry E doesn't modify E's key and value, but cut
   the "next" link for the sake of weak-gc robustness.  The hash core
   iterator prefetches a pointer to the next entry, so deleting the
   "current" entry of iteration is safe as far as other iterators
   are running on the same hash table. */
static Entry *delete_entry(ScmHashCore *table,
                           Entry *entry, Entry *prev,
                           int index)
{
    if (prev) prev->next = entry->next;
    else table->buckets[index] = (void*)entry->next;
    table->numEntries--;
    SCM_ASSERT(table->numEntries >= 0);
    entry->next = NULL;         /* GC friendliness */
    return entry;
}

#define FOUND(table, op, e, p, index)                   \
    do {                                                \
        switch (op) {                                   \
        case SCM_DICT_GET:;                             \
        case SCM_DICT_CREATE:;                          \
            return e;                                   \
        case SCM_DICT_DELETE:;                          \
            return delete_entry(table, e, p, index);    \
        }                                               \
    } while (0)

#define NOTFOUND(table, op, key, hashval, index)                \
    do {                                                        \
        if (op == SCM_DICT_CREATE) {                            \
           return insert_entry(table, key, hashval, index);     \
        } else {                                                \
           return NULL;                                         \
        }                                                       \
    } while (0)

/*
 * Accessor function for address.   Used for EQ-type hash.
 */
static Entry *address_access(ScmHashCore *table,
                             intptr_t key,
                             ScmDictOp op)
{
    u_long hashval, index;
    Entry **buckets = (Entry**)table->buckets;

    ADDRESS_HASH(hashval, key);
    index = HASH2INDEX(table->numBuckets, table->numBucketsLog2, hashval);

    for (Entry *e = buckets[index], *p = NULL; e; p = e, e = e->next) {
        if (e->key == key) FOUND(table, op, e, p, index);
    }
    NOTFOUND(table, op, key, hashval, index);
}

static u_long address_hash(const ScmHashCore *ht, intptr_t obj)
{
    u_long hashval;
    ADDRESS_HASH(hashval, obj);
    return hashval;
}

static int address_cmp(const ScmHashCore *ht, intptr_t key, intptr_t k2)
{
    return (key == k2);
}

/*
 * Accessor function for equal and eqv-hash.
 * We assume KEY is ScmObj.
 */
static u_long eqv_hash(const ScmHashCore *table, intptr_t key)
{
    return Scm_EqvHash(SCM_OBJ(key));
}

static int eqv_cmp(const ScmHashCore *table, intptr_t key, intptr_t k2)
{
    return Scm_EqvP(SCM_OBJ(key), SCM_OBJ(k2));
}

static u_long equal_hash(const ScmHashCore *table, intptr_t key)
{
    return Scm_Hash(SCM_OBJ(key));
}

static int equal_cmp(const ScmHashCore *table, intptr_t key, intptr_t k2)
{
    return Scm_EqualP(SCM_OBJ(key), SCM_OBJ(k2));
}


/*
 * Accessor function for string type.
 */
static Entry *string_access(ScmHashCore *table, intptr_t k, ScmDictOp op)
{
    ScmObj key = SCM_OBJ(k);

    if (!SCM_STRINGP(key)) {
        Scm_Error("Got non-string key %S to the string hashtable.", key);
    }
    const ScmStringBody *keyb = SCM_STRING_BODY(key);
    const char *s = SCM_STRING_BODY_START(keyb);
    int size = SCM_STRING_BODY_SIZE(keyb);
    u_long hashval;
    STRING_HASH(hashval, s, size);
    u_long index = HASH2INDEX(table->numBuckets, table->numBucketsLog2, hashval);
    Entry **buckets = (Entry**)table->buckets;

    for (Entry *e = buckets[index], *p = NULL; e; p = e, e = e->next) {
        ScmObj ee = SCM_OBJ(e->key);
        const ScmStringBody *eeb = SCM_STRING_BODY(ee);
        int eesize = SCM_STRING_BODY_SIZE(eeb);
        if (size == eesize
            && memcmp(SCM_STRING_BODY_START(keyb),
                      SCM_STRING_BODY_START(eeb), eesize) == 0){
            FOUND(table, op, e, p, index);
        }
    }
    NOTFOUND(table, op, k, hashval, index);
}

static u_long string_hash(const ScmHashCore *table, intptr_t key)
{
    u_long hashval;
    const ScmStringBody *b = SCM_STRING_BODY(key);
    const char *p = SCM_STRING_BODY_START(b);
    STRING_HASH(hashval, p, SCM_STRING_BODY_SIZE(b));
    return hashval;
}

static int string_cmp(const ScmHashCore *table, intptr_t k1, intptr_t k2)
{
    const ScmStringBody *b1 = SCM_STRING_BODY(k1);
    const ScmStringBody *b2 = SCM_STRING_BODY(k2);
    return ((SCM_STRING_BODY_SIZE(b1) == SCM_STRING_BODY_SIZE(b2))
            && (memcmp(SCM_STRING_BODY_START(b1),
                       SCM_STRING_BODY_START(b2),
                       SCM_STRING_BODY_SIZE(b1)) == 0));
}

/*
 * Accessor function for multiword raw hashtable.
 * Key points to an array of N words.
 */
#if 0                           /* not used yet */
static u_long multiword_hash(const ScmHashCore *table, intptr_t key)
{
    ScmWord keysize = (ScmWord)table->data;
    ScmWord *keyarray = (ScmWord*)key;
    u_long h = 0, h1;
    for (int i=0; i<keysize; i++) {
        ADDRESS_HASH(h1, keyarray[i]);
        h = COMBINE(h, h1);
    }
    return h;
}
#endif

#if 0
static Entry *multiword_access(ScmHashCore *table, intptr_t k, ScmDictOp op)
{
    u_long hashval, index;
    ScmWord keysize = (ScmWord)table->data;

    hashval = multiword_hash(table, k);
    index = HASH2INDEX(table->numBuckets, table->numBucketsLog2, hashval);
    Entry **buckets = (Entry**)table->buckets;

    for (Entry *e = buckets[index], *p = NULL; e; p = e, e = e->next) {
        if (memcmp((void*)k, (void*)e->key, keysize*sizeof(ScmWord)) == 0)
            FOUND(table, op, e, p, index);
    }
    NOTFOUND(table, op, k, hashval, index);
}
#endif


/*
 * Accessor function for general case
 *    (hashfn and cmpfn are given by user)
 */
static Entry *general_access(ScmHashCore *table, intptr_t key, ScmDictOp op)
{
    u_long hashval, index;

    hashval = table->hashfn(table, key);
    index = HASH2INDEX(table->numBuckets, table->numBucketsLog2, hashval);
    Entry **buckets = (Entry**)table->buckets;

    for (Entry *e = buckets[index], *p = NULL; e; p = e, e = e->next) {
        if (table->cmpfn(table, key, e->key)) FOUND(table, op, e, p, index);
    }
    NOTFOUND(table, op, key, hashval, index);
}

/*============================================================
 * Hash Core functions
 */

static void hash_core_init(ScmHashCore *table,
                           SearchProc  *accessfn,
                           ScmHashProc *hashfn,
                           ScmHashCompareProc *cmpfn,
                           unsigned int initSize,
                           void *data)
{
    if (initSize != 0) initSize = round2up(initSize);
    else initSize = DEFAULT_NUM_BUCKETS;

    Entry **b = SCM_NEW_ARRAY(Entry*, initSize);
    table->buckets = (void**)b;
    table->numBuckets = initSize;
    table->numEntries = 0;
    table->accessfn = (void*)accessfn;
    table->hashfn = hashfn;
    table->cmpfn = cmpfn;
    table->data = data;
    table->numBucketsLog2 = 0;
    for (u_int i=initSize; i > 1; i /= 2) {
        table->numBucketsLog2++;
    }
    for (u_int i=0; i<initSize; i++) table->buckets[i] = NULL;
}

/* choose appropriate procedures for predefined hash types. */
int  hash_core_predef_procs(ScmHashType type,
                            SearchProc  **accessfn,
                            ScmHashProc **hashfn,
                            ScmHashCompareProc **cmpfn)
{
    switch (type) {
    case SCM_HASH_EQ:
    case SCM_HASH_WORD:
        *accessfn = address_access;
        *hashfn = address_hash;
        *cmpfn  = address_cmp;
        return TRUE;
    case SCM_HASH_EQV:
        *accessfn = general_access;
        *hashfn = eqv_hash;
        *cmpfn  = eqv_cmp;
        return TRUE;
    case SCM_HASH_EQUAL:
        *accessfn = general_access;
        *hashfn = equal_hash;
        *cmpfn  = equal_cmp;
        return TRUE;
    case SCM_HASH_STRING:
        *accessfn = string_access;
        *hashfn = string_hash;
        *cmpfn  = string_cmp;
        return TRUE;
    default:
        return FALSE;
    }
}

void Scm_HashCoreInitSimple(ScmHashCore *core,
                            ScmHashType type,
                            unsigned int initSize,
                            void *data)
{
    SearchProc  *accessfn;
    ScmHashProc *hashfn;
    ScmHashCompareProc *cmpfn;

    if (hash_core_predef_procs(type, &accessfn, &hashfn, &cmpfn) == FALSE) {
        Scm_Error("[internal error]: wrong TYPE argument passed to Scm_HashCoreInitSimple: %d", type);
    }
    hash_core_init(core, accessfn, hashfn, cmpfn, initSize, data);
}

void Scm_HashCoreInitGeneral(ScmHashCore *core,
                             ScmHashProc *hashfn,
                             ScmHashCompareProc *cmpfn,
                             unsigned int initSize,
                             void *data)
{
    hash_core_init(core, general_access, hashfn,
                   cmpfn, initSize, data);
}

int Scm_HashCoreTypeToProcs(ScmHashType type,
                            ScmHashProc **hashfn,
                            ScmHashCompareProc **cmpfn)
{
    SearchProc *accessfn;       /* dummy */
    return hash_core_predef_procs(type, &accessfn, hashfn, cmpfn);
}

void Scm_HashCoreCopy(ScmHashCore *dst, const ScmHashCore *src)
{
    Entry **b = SCM_NEW_ARRAY(Entry*, src->numBuckets);

    for (int i=0; i<src->numBuckets; i++) {
        Entry *p = NULL;
        Entry *s = (Entry*)src->buckets[i];
        b[i] = NULL;
        while (s) {
            Entry *e = SCM_NEW(Entry);
            e->key = s->key;
            e->value = s->value;
            e->next = NULL;
            if (p) p->next = e;
            else   b[i] = e;
            p = e;
            s = s->next;
        }
    }

    /* A little trick to avoid hazard in careless race condition */
    dst->numBuckets = dst->numEntries = 0;

    dst->buckets = (void**)b;
    dst->hashfn   = src->hashfn;
    dst->cmpfn    = src->cmpfn;
    dst->accessfn = src->accessfn;
    dst->data     = src->data;
    dst->numEntries = src->numEntries;
    dst->numBucketsLog2 = src->numBucketsLog2;
    dst->numBuckets = src->numBuckets;
}

void Scm_HashCoreClear(ScmHashCore *table)
{
    for (int i=0; i<table->numBuckets; i++) {
        table->buckets[i] = NULL;
    }
    table->numEntries = 0;
}

ScmDictEntry *Scm_HashCoreSearch(ScmHashCore *table, intptr_t key,
                                 ScmDictOp op)
{
    SearchProc *p = (SearchProc*)table->accessfn;
    return (ScmDictEntry*)p(table, key, op);
}

int Scm_HashCoreNumEntries(ScmHashCore *table)
{
    return table->numEntries;
}

/*
 * NB: It is important to keep the pointer to the "next" entry,
 * not the "current", since the current entry may be deleted,
 * erasing its next pointer.
 */
void Scm_HashIterInit(ScmHashIter *iter, ScmHashCore *table)
{
    iter->core = table;
    for (int i=0; i<table->numBuckets; i++) {
        if (table->buckets[i]) {
            iter->bucket = i;
            iter->next = table->buckets[i];
            return;
        }
    }
    iter->next = NULL;
}

ScmDictEntry *Scm_HashIterNext(ScmHashIter *iter)
{
    Entry *e = (Entry*)iter->next;
    if (e != NULL) {
        if (e->next) iter->next = e->next;
        else {
            int i = iter->bucket + 1;
            for (; i < iter->core->numBuckets; i++) {
                if (iter->core->buckets[i]) {
                    iter->bucket = i;
                    iter->next = iter->core->buckets[i];
                    return (ScmDictEntry*)e;
                }
            }
            iter->next = NULL;
        }
    }
    return (ScmDictEntry*)e;
}

/*============================================================
 * Scheme <hash-table> object
 */

static void hash_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);

SCM_DEFINE_BUILTIN_CLASS(Scm_HashTableClass, hash_print, NULL, NULL, NULL,
                         SCM_CLASS_DICTIONARY_CPL);

ScmObj Scm_MakeHashTableSimple(ScmHashType type, unsigned int initSize)
{
    /* We only allow ScmObj in <hash-table> */
    if (type > SCM_HASH_GENERAL) {
        Scm_Error("Scm_MakeHashTableSimple: wrong type arg: %d", type);
    }
    ScmHashTable *z = SCM_NEW(ScmHashTable);
    SCM_SET_CLASS(z, SCM_CLASS_HASH_TABLE);
    Scm_HashCoreInitSimple(&z->core, type, initSize, NULL);
    z->type = type;
    return SCM_OBJ(z);
}

ScmObj Scm_MakeHashTableFull(ScmHashProc hashfn,
                             ScmHashCompareProc cmpfn,
                             unsigned int initSize, void *data)
{
    ScmHashTable *z = SCM_NEW(ScmHashTable);
    SCM_SET_CLASS(z, SCM_CLASS_HASH_TABLE);
    z->type = SCM_HASH_GENERAL;
    Scm_HashCoreInitGeneral(&z->core, hashfn, cmpfn, initSize, data);
    return SCM_OBJ(z);
}

ScmObj Scm_HashTableCopy(ScmHashTable *src)
{
    ScmHashTable *dst = SCM_NEW(ScmHashTable);
    SCM_SET_CLASS(dst, SCM_CLASS_HASH_TABLE);
    Scm_HashCoreCopy(SCM_HASH_TABLE_CORE(dst), SCM_HASH_TABLE_CORE(src));
    dst->type = src->type;
    return SCM_OBJ(dst);
}

ScmHashType Scm_HashTableType(ScmHashTable *ht)
{
    return ht->type;
}

ScmObj Scm_HashTableRef(ScmHashTable *ht, ScmObj key, ScmObj fallback)
{
    ScmDictEntry *e = Scm_HashCoreSearch(SCM_HASH_TABLE_CORE(ht),
                                         (intptr_t)key, SCM_DICT_GET);
    if (!e) return fallback;
    else    return SCM_DICT_VALUE(e);
}

ScmObj Scm_HashTableSet(ScmHashTable *ht, ScmObj key, ScmObj value, int flags)
{
    ScmDictEntry *e;

    e = Scm_HashCoreSearch(SCM_HASH_TABLE_CORE(ht),
                           (intptr_t)key,
                           (flags&SCM_DICT_NO_CREATE)?SCM_DICT_GET: SCM_DICT_CREATE);
    if (!e) return SCM_UNBOUND;
    if (e->value) {
        if (flags&SCM_DICT_NO_OVERWRITE) return SCM_DICT_VALUE(e);
        else return SCM_DICT_SET_VALUE(e, value);
    } else {
        return SCM_DICT_SET_VALUE(e, value);
    }
}

ScmObj Scm_HashTableDelete(ScmHashTable *ht, ScmObj key)
{
    ScmDictEntry *e = Scm_HashCoreSearch(SCM_HASH_TABLE_CORE(ht),
                                         (intptr_t)key, SCM_DICT_DELETE);
    if (e && e->value) return SCM_DICT_VALUE(e);
    else               return SCM_UNBOUND;
}

ScmObj Scm_HashTableKeys(ScmHashTable *table)
{
    ScmHashIter iter;
    ScmDictEntry *e;
    ScmObj h = SCM_NIL, t = SCM_NIL;
    Scm_HashIterInit(&iter, SCM_HASH_TABLE_CORE(table));
    while ((e = Scm_HashIterNext(&iter)) != NULL) {
        SCM_APPEND1(h, t, SCM_DICT_KEY(e));
    }
    return h;
}

ScmObj Scm_HashTableValues(ScmHashTable *table)
{
    ScmHashIter iter;
    ScmDictEntry *e;
    ScmObj h = SCM_NIL, t = SCM_NIL;
    Scm_HashIterInit(&iter, SCM_HASH_TABLE_CORE(table));
    while ((e = Scm_HashIterNext(&iter)) != NULL) {
        SCM_APPEND1(h, t, SCM_DICT_VALUE(e));
    }
    return h;
}

ScmObj Scm_HashTableStat(ScmHashTable *table)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    ScmHashCore *c = SCM_HASH_TABLE_CORE(table);
    SCM_APPEND1(h, t, SCM_MAKE_KEYWORD("num-entries"));
    SCM_APPEND1(h, t, Scm_MakeInteger(c->numEntries));
    SCM_APPEND1(h, t, SCM_MAKE_KEYWORD("num-buckets"));
    SCM_APPEND1(h, t, Scm_MakeInteger(c->numBuckets));
    SCM_APPEND1(h, t, SCM_MAKE_KEYWORD("num-buckets-log2"));
    SCM_APPEND1(h, t, Scm_MakeInteger(c->numBucketsLog2));

    Entry** b = BUCKETS(c);
    ScmVector *v = SCM_VECTOR(Scm_MakeVector(c->numBuckets, SCM_NIL));
    ScmObj *vp = SCM_VECTOR_ELEMENTS(v);
    for (int i = 0; i<c->numBuckets; i++, vp++) {
        Entry *e = b[i];
        for (; e; e = e->next) {
            *vp = Scm_Acons(SCM_DICT_KEY(e), SCM_DICT_VALUE(e), *vp);
        }
    }
    SCM_APPEND1(h, t, SCM_MAKE_KEYWORD("contents"));
    SCM_APPEND1(h, t, SCM_OBJ(v));
    return h;
}


/*
 * Utilities
 */

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
    default: Scm_Panic("something wrong with a hash table");
    }

#if 0
    /* Use read-time constructor so that table can be read back
       --- is it necessary?  I'm not sure yet. */
    Scm_Printf(port, "#,(<hash-table> %s", str);
    if (ht->numEntries > 0) {
        Scm_HashIterInit(&iter, ht);
        while ((e = Scm_HashIterNext(&iter)) != NULL) {
            Scm_Printf(port, " %S %S", e->key, e->value);
        }
    }
    SCM_PUTZ(")", -1, port);
#else
    Scm_Printf(port, "#<hash-table %s %p>", str, ht);
#endif
}

static unsigned int round2up(unsigned int val)
{
    unsigned int n = 1;
    while (n < val) {
        n <<= 1;
        SCM_ASSERT(n > 1);      /* check overflow */
    }
    return n;
}

/*====================================================================
 * For backward compatibility
 */

/* Backward compatibility.
   NB: Casting ScmDictEntry* to ScmHashEntry* would be invalid if
   sizeof(intptr_t) and sizeof(void*) differ.  I know only one
   such platform (PlayStation2).  If it is a problem, moving to
   the new API is recommended. */
ScmHashEntry *Scm_HashTableGet(ScmHashTable *ht, ScmObj key)
{
    if (sizeof(intptr_t) != sizeof(void*)) {
        Scm_Error("[internal] Scm_HashTableGet is obsoleted on this platform.  You should use the new hashtable API.");
    }
    return (ScmHashEntry*)Scm_HashCoreSearch(SCM_HASH_TABLE_CORE(ht),
                                             (intptr_t)key,
                                             SCM_DICT_GET);
}

ScmHashEntry *Scm_HashTableAdd(ScmHashTable *ht, ScmObj key, ScmObj value)
{
    ScmDictEntry *e = Scm_HashCoreSearch(SCM_HASH_TABLE_CORE(ht),
                                         (intptr_t)key, SCM_DICT_CREATE);
    if (sizeof(intptr_t) != sizeof(void*)) {
        Scm_Error("[internal] Scm_HashTableAdd is obsoleted on this platform.  You should use the new hashtable API.");
    }
    if (!e->value) (void)SCM_DICT_SET_VALUE(e, value);
    return (ScmHashEntry*)e;
}

ScmHashEntry *Scm_HashTablePut(ScmHashTable *ht, ScmObj key, ScmObj value)
{
    ScmDictEntry *e = Scm_HashCoreSearch(SCM_HASH_TABLE_CORE(ht),
                                         (intptr_t)key, SCM_DICT_CREATE);
    if (sizeof(intptr_t) != sizeof(void*)) {
        Scm_Error("[internal] Scm_HashTablePut is obsoleted on this platform.  You should use the new hashtable API.");
    }
    (void)SCM_DICT_SET_VALUE(e, value);
    return (ScmHashEntry*)e;
}

/* TRANSIENT: Pre-0.9 Compatibility routine.  Kept for the binary compatibility.
   Will be removed on 1.0 */
void Scm__HashIterInitCompat(ScmHashTable *table, ScmHashIter *iter)
{
    Scm_HashIterInit(iter, SCM_HASH_TABLE_CORE(table));
}

/* TRANSIENT: Pre-0.9 Compatibility routine.  Kept for the binary compatibility.
   Will be removed on 1.0 */
ScmHashEntry *Scm__HashIterNextCompat(ScmHashIter *iter)
{
    ScmDictEntry *e = Scm_HashIterNext(iter);
    return (ScmHashEntry*)e;
}

#if 0
ScmObj Scm_MakeHashTableMultiWord(int keysize, int initsize)
{
    return make_hash_table(SCM_CLASS_HASH_TABLE, SCM_HASH_MULTIWORD,
                           multiword_access, multiword_hash,
                           NULL, initsize, (void*)SCM_WORD(keysize));
}
#endif

/* Legacy constructor.  DEPRECATED.  Will go away soon. */
ScmObj Scm_MakeHashTable(ScmHashProc *hashfn,
                         ScmHashCompareProc *cmpfn,
                         unsigned int initSize)
{
    if (hashfn == (ScmHashProc*)SCM_HASH_EQ) {
        return Scm_MakeHashTableSimple(SCM_HASH_EQ, initSize);
    } else if (hashfn == (ScmHashProc*)SCM_HASH_EQV) {
        return Scm_MakeHashTableSimple(SCM_HASH_EQV, initSize);
    } else if (hashfn == (ScmHashProc*)SCM_HASH_EQUAL) {
        return Scm_MakeHashTableSimple(SCM_HASH_EQUAL, initSize);
    } else if (hashfn == (ScmHashProc*)SCM_HASH_STRING) {
        return Scm_MakeHashTableSimple(SCM_HASH_STRING, initSize);
    }
#if 0
    else {
        return Scm_MakeHashTableFull(SCM_CLASS_HASH_TABLE, SCM_HASH_GENERAL,
                                     hashfn, cmpfn, initSize, NULL);
    }
#else
    return SCM_UNDEFINED;
#endif
}
