/*
 * hash.c - hash table implementation
 *
 *   Copyright (c) 2000-2018  Shiro Kawai  <shiro@acm.org>
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
#include <math.h>
#include "atomic_ops.h"
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

/* We limit portable hash value to 32bits */
#define PORTABLE_HASHMASK  0xffffffffUL

/* For other hash values, we limit it in the fixnum range. */
#define HASHMASK SCM_SMALL_INT_MAX

typedef Entry *SearchProc(ScmHashCore *core, intptr_t key, ScmDictOp op);

static u_int round2up(unsigned int val);

/*============================================================
 * Hash salt
 */

/* The salt value is nonnegative fixnum.  For the time being, we initialize
   the default salt value for each run of the process; we might do
   per-hashtable salt in future.

   Internally we use parameter slot to keep the hash salt value, but
   we provide dedicated C API to access it to avoid overhead of parameter
   mechanism.
*/

static ScmParameterLoc hash_salt; /* initialized by Scm__InitHash() */

ScmSmallInt Scm_HashSaltRef()
{
    return SCM_INT_VALUE(Scm_ParameterRef(Scm_VM(), &hash_salt));
}

ScmSmallInt Scm_HashSaltSet(ScmSmallInt newval) /* returns old value */
{
    return SCM_INT_VALUE(Scm_ParameterSet(Scm_VM(), &hash_salt,
                                          SCM_MAKE_INT(newval)));
}

/*============================================================
 * Hash functions
 */

/* Hash function calculates 32bit hash value from the given object.
   HASH2INDEX macro maps the hash value to the bucket number.
   (On 64 bit architecture, it's OK to calculate 64bit, but the
   upper bits are discarded by HASH2INDEX to maintain compatibility. */

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

/* For strings, we employ siphash.
   We use public domain implementation by Sam Trenholme
   http://samiam.org/blog/20131006.html.  It has a version suitable
   for 32bit architecture, too.
   See dws_adapter.h for the details.
 */
#define SCM_DWSIPHASH_INTERFACE
#include "gauche/priv/dws_adapter.h"

u_long Scm_EqHash(ScmObj obj)
{
    u_long hashval;
    ADDRESS_HASH(hashval, obj);
    return hashval&HASHMASK;
}

static u_long number_hash(ScmObj obj, u_long salt, int portable);

static u_long flonum_hash(double d, u_long salt, int portable)
{
    int exp, sign;
    ScmObj mantissa = Scm_DecodeFlonum(d, &exp, &sign);
    u_long xh;
    SMALL_INT_HASH(xh, exp*sign);
    if (SCM_NUMBERP(mantissa)) {
        return COMBINE(number_hash(mantissa, salt, portable), xh);
    } else {
        /* d is not finite.  we just map +inf.0, -inf.0 and nan.0 to 0. */
        return 0;
    }
}

static u_long number_hash(ScmObj obj, u_long salt, int portable)
{
    u_long hashval;
    if (SCM_INTP(obj)) {
        /* On 64bit platform, if we have fixnum that is beyond the range
           of 32bit fixnum, we have to calculate the hash value the same
           way as 32bit bignum would do. */
        long u = SCM_INT_VALUE(obj);
        if (portable) {
            if (u < 0) u = -u;  /* safe, for u is in fixnum range */
#if SIZEOF_LONG == 8
            u = ((u & ((1UL<<32) - 1)) + (u >> 32)) & ((1UL<<32)-1);
#endif
        }
        SMALL_INT_HASH(hashval, u);
    } else if (SCM_BIGNUMP(obj)) {
        if (portable) {
            u_int i;
            u_long u = 0;
            for (i=0; i<SCM_BIGNUM_SIZE(obj); i++) {
#if SIZEOF_LONG == 4
                u += SCM_BIGNUM(obj)->values[i];
#elif SIZEOF_LONG == 8
                u += (SCM_BIGNUM(obj)->values[i] & ((1UL<<32) - 1))
                    + (SCM_BIGNUM(obj)->values[i] >> 32);
#else
#error "sizeof(long) > 8 platform unsupported"
#endif
            }
            SMALL_INT_HASH(hashval, u);
        } else {
            u_int i;
            u_long u = 0;
            for (i=0; i<SCM_BIGNUM_SIZE(obj); i++) {
                u += SCM_BIGNUM(obj)->values[i];
            }
            SMALL_INT_HASH(hashval, u);
        }
    } else if (SCM_FLONUMP(obj)) {
        hashval = flonum_hash(SCM_FLONUM_VALUE(obj), salt, portable);
    } else if (SCM_RATNUMP(obj)) {
        /* Ratnum must already be normalized, so we can simply combine
           hashvals of numerator and denominator. */
        u_long h1 = number_hash(SCM_RATNUM_NUMER(obj), salt, portable);
        u_long h2 = number_hash(SCM_RATNUM_DENOM(obj), salt, portable);
        hashval = COMBINE(h1, h2);
    } else {
        SCM_ASSERT(SCM_COMPNUMP(obj));
        hashval = COMBINE(flonum_hash(SCM_COMPNUM_REAL(obj), salt, portable),
                          flonum_hash(SCM_COMPNUM_IMAG(obj), salt, portable));
    }
    return hashval & (portable ? PORTABLE_HASHMASK : HASHMASK);
}

u_long Scm_EqvHash(ScmObj obj)
{
    u_long hashval;
    if (SCM_NUMBERP(obj)) {
        hashval = number_hash(obj, 0, FALSE);
    } else {
        ADDRESS_HASH(hashval, obj);
    }
    return hashval&HASHMASK;
}

static u_long internal_string_hash(ScmString *str, u_long salt, int portable)
{
    const ScmStringBody *b = SCM_STRING_BODY(str);
    if (portable) {
        return (u_long)Scm__DwSipPortableHash((uint8_t*)b->start, b->size,
                                              salt, salt);
    } else {
        return Scm__DwSipDefaultHash((uint8_t*)b->start, b->size,
                                     salt, salt);
    }
}

/* equal-hash, which satisfies
     forall x, y: equal(x,y) => hash(x) = hash(y)
  
   Both default-hash and portable-hash have this property but their
   requirements are slightly different, so here's the common part.
*/
static u_long equal_hash_common(ScmObj obj, u_long salt, int portable)
{
    if (SCM_NUMBERP(obj)) {
        return number_hash(obj, salt, portable);
    } else if (!SCM_PTRP(obj)) {
        u_long hashval;
        SMALL_INT_HASH(hashval, (u_long)SCM_WORD(obj));
        return hashval&PORTABLE_HASHMASK;
    } else if (SCM_STRINGP(obj)) {
        return internal_string_hash(SCM_STRING(obj), salt, portable);
    } else if (SCM_PAIRP(obj)) {
        u_long h = 0, h2;
        ScmObj cp;
        SCM_FOR_EACH(cp, obj) {
            h2 = equal_hash_common(SCM_CAR(cp), salt, portable);
            h = COMBINE(h, h2);
        }
        h2 = equal_hash_common(cp, salt, portable);
        return COMBINE(h, h2);
    } else if (SCM_VECTORP(obj)) {
        int siz = SCM_VECTOR_SIZE(obj);
        u_long h = 0, h2;
        for (int i=0; i<siz; i++) {
            h2 = equal_hash_common(SCM_VECTOR_ELEMENT(obj, i), salt, portable);
            h = COMBINE(h, h2);
        }
        return h;
#if GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION
    } else if (SCM_KEYWORDP(obj)) {
        if (portable) {
            if (SCM_SYMBOLP(obj)) {
                /* GAUCHE_KEYWORD_IS_SYMBOL mode */
                return internal_string_hash(SCM_KEYWORD_NAME(obj), salt, TRUE);
            } else {
                /* GAUCHE_KEYWORD_IS_DISJOINT mode.  SCM_KEYWORD_NAME does
                   not include prefix ':'.  We should append it so that
                   the hash value stays the same.  Appending string incurs
                   allocation, but we expect this branch isn't taken often
                   and eventually fade away. */
                static ScmString *prefix = NULL;
                if (prefix == NULL) {
                    /* idempotent.  no MT hazard. */
                    prefix = SCM_STRING(Scm_MakeString(":", 1, 1, 0));
                }
                ScmObj name = Scm_StringAppend2(prefix, SCM_KEYWORD_NAME(obj));
                return internal_string_hash(SCM_STRING(name), salt, TRUE);
            }
        } else {
            u_long hashval;
            ADDRESS_HASH(hashval, obj);
            return hashval;
        }
#endif /*GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION*/
    } else if (SCM_SYMBOLP(obj)) {
        if (portable) {
            return internal_string_hash(SCM_SYMBOL_NAME(obj), salt, TRUE);
        } else {
            u_long hashval;
            ADDRESS_HASH(hashval, obj);
            return hashval;
        }
    } else {
        /* Call specialized object-hash method
           We need some trick; See libomega.scm for the details. */
        static ScmObj call_object_hash_proc = SCM_UNDEFINED;
        static ScmObj portable_hash_proc = SCM_UNDEFINED;
        static ScmObj default_hash_proc = SCM_UNDEFINED;
        SCM_BIND_PROC(call_object_hash_proc, "%call-object-hash",
                      Scm_GaucheInternalModule());
        SCM_BIND_PROC(portable_hash_proc, "portable-hash", Scm_GaucheModule());
        SCM_BIND_PROC(default_hash_proc, "default-hash", Scm_GaucheModule());
        ScmObj r = Scm_ApplyRec3(call_object_hash_proc, obj,
                                 (portable
                                  ? portable_hash_proc
                                  : default_hash_proc),
                                 (portable
                                  ? Scm_MakeIntegerU(salt)
                                  : SCM_FALSE));
        if (SCM_INTP(r)) {
            return (u_long)SCM_INT_VALUE(r);
        }
        if (SCM_BIGNUMP(r)) {
            /* NB: Scm_GetUInteger clamps the result to [0, ULONG_MAX],
               so taking the LSW would give better distribution. */
            return SCM_BIGNUM(r)->values[0];
        }
        Scm_Error("object-hash returned non-integer: %S", r);
        return 0;               /* dummy */
    }
}

/* For recursive call to the current hash function - see call-object-hash
   and object-hash definitions in libomega.scm. */
static ScmParameterLoc current_recursive_hash;

ScmObj Scm_CurrentRecursiveHash(ScmObj newval)
{
    ScmObj val = Scm_ParameterRef(Scm_VM(), &current_recursive_hash);
    if (newval != SCM_UNBOUND) {
        Scm_ParameterSet(Scm_VM(), &current_recursive_hash, newval);
    }
    return val;
}

/* 'Portable' general hash function.
   
   It is guaranteed that the hash value won't change for the same objects
   (roughly, indistinguishable in their external representation)
   accross the runs of the program, and among different platforms.
   That is, the value can be used in persistent stores.
 */
u_long Scm_PortableHash(ScmObj obj, u_long salt)
{
    return equal_hash_common(obj, salt, TRUE) & PORTABLE_HASHMASK;
}

/* 'Default' general hash function. */
ScmSmallInt Scm_DefaultHash(ScmObj obj)
{
    return equal_hash_common(obj, Scm_HashSaltRef(), FALSE) & HASHMASK;
}

/* This is to expose string hash function.  Modulo is for the compatibility
   of srfi-13; just give 0 as modulo if you don't need it.  */
u_long Scm_HashString(ScmString *str, u_long modulo)
{
    u_long hashval = internal_string_hash(str, Scm_HashSaltRef(), FALSE);
    if (modulo == 0) return hashval&HASHMASK;
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
    return Scm_DefaultHash(SCM_OBJ(key));
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
    u_long hashval = Scm_HashString(SCM_STRING(key), 0);
    u_long index = HASH2INDEX(table->numBuckets, table->numBucketsLog2, hashval);
    Entry **buckets = (Entry**)table->buckets;

    const ScmStringBody *keyb = SCM_STRING_BODY(key);
    long size = SCM_STRING_BODY_SIZE(keyb);
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
    return Scm_HashString(SCM_STRING(key), 0);
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

SCM_DEFINE_BUILTIN_CLASS(Scm_HashTableClass, hash_print, Scm_ObjectCompare,
                         NULL, NULL,
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

/* Returns previous value; can return SCM_UNBOUND when the association hasn't
   been there.  Be careful not to let SCM_UNBOUND leak out to Scheme! */
ScmObj Scm_HashTableSet(ScmHashTable *ht, ScmObj key, ScmObj value, int flags)
{
    ScmDictEntry *e;

    e = Scm_HashCoreSearch(SCM_HASH_TABLE_CORE(ht),
                           (intptr_t)key,
                           (flags&SCM_DICT_NO_CREATE)?SCM_DICT_GET: SCM_DICT_CREATE);
    if (!e) return SCM_UNBOUND;
    ScmObj oldval = e->value? SCM_DICT_VALUE(e) : SCM_UNBOUND;
    if (!(flags&SCM_DICT_NO_OVERWRITE) || SCM_UNBOUNDP(oldval)) {
        return SCM_DICT_SET_VALUE(e, value);
    }
    return oldval;
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
 * Initialization
 */

void Scm__InitHash()
{
    struct timeval t;
    if (gettimeofday(&t, NULL) < 0) {
        Scm_Panic("gettimeofday failed");
    }
    u_long salt = ((u_long)getpid() * ((u_long)t.tv_sec^(u_long)t.tv_usec));
    ADDRESS_HASH(salt, salt);
    salt &= SCM_SMALL_INT_MAX;
    Scm_InitParameterLoc(Scm_VM(), &hash_salt, Scm_MakeIntegerU(salt));
    Scm_InitParameterLoc(Scm_VM(), &current_recursive_hash, SCM_FALSE);
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

/* Legacy hash function.
 *
 * This used to be used for equal?-hashtable hash.  It also guaranteed
 * that the hash result won't change between runs and among different
 * platforms, so it can be used for persistent data.
 *
 * There are several drawbacks, though.  The guaranteed hash value means
 * we can't change hash function.   The quality of the original hash
 * functon wasn't good (it behaves terrible on flonums and compnums);
 * it's vulnerable to collision attacks; and it had a few bugs in the
 * number hash that broke the 'portable' guarantee between platforms.
 *
 * Since there have already been stored data relying on the original hash
 * values, we keep the old function (with bugs fixed) here.
 * Scm_Hash() and Scheme's 'hash' function uses this for the backward
 * comaptibility, but it is not recommended for the new code.
 */

static u_long legacy_number_hash(ScmObj obj);
static u_long legacy_string_hash(ScmString *str);

u_long Scm_Hash(ScmObj obj)
{
    if (!SCM_PTRP(obj)) {
        u_long hashval;
        SMALL_INT_HASH(hashval, (u_long)SCM_WORD(obj));
        return hashval&PORTABLE_HASHMASK;
    } else if (SCM_NUMBERP(obj)) {
        return legacy_number_hash(obj);
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
        return h&PORTABLE_HASHMASK;
    } else if (SCM_VECTORP(obj)) {
        int siz = SCM_VECTOR_SIZE(obj);
        u_long h = 0, h2;
        for (int i=0; i<siz; i++) {
            h2 = Scm_Hash(SCM_VECTOR_ELEMENT(obj, i));
            h = COMBINE(h, h2);
        }
        return h&PORTABLE_HASHMASK;
    } else if (SCM_KEYWORDP(obj)) {
        /* If we have keyword and symbol unified, KEYWORD_NAME includes
           ':'.  Legacy hash didn't consider it. */
#if GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION
        if (SCM_SYMBOLP(obj)) {
            obj = Scm_Substring(SCM_KEYWORD_NAME(obj), 1, -1, FALSE);
        } else {
            obj = SCM_OBJ(SCM_KEYWORD_NAME(obj));
        }
#else  /*!GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION*/
        obj = Scm_Substring(SCM_KEYWORD_NAME(obj), 1, -1, FALSE);
#endif /*!GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION*/
        goto string_hash;
    } else if (SCM_SYMBOLP(obj)) {
        obj = SCM_OBJ(SCM_SYMBOL_NAME(obj));
        goto string_hash;
    } else {
        /* Call specialized object-hash method */
        ScmObj r = Scm_ApplyRec(SCM_OBJ(&Scm_GenericObjectHash),
                                SCM_LIST1(obj));
        if (SCM_INTP(r)) {
            return ((u_long)SCM_INT_VALUE(r))&PORTABLE_HASHMASK;
        }
        if (SCM_BIGNUMP(r)) {
            /* NB: Scm_GetUInteger clamps the result to [0, ULONG_MAX],
               but taking the LSW would give better distribution. */
            return (SCM_BIGNUM(r)->values[0])&PORTABLE_HASHMASK;
        }
        Scm_Error("object-hash returned non-integer: %S", r);
        return 0;               /* dummy */
    }
  string_hash:
    return legacy_string_hash(SCM_STRING(obj));
}

static u_long legacy_flonum_hash(double f)
{
    /* Originally the code was just (u_long)(f * 2654435761UL), but that's
       UB when the multiplication yields out of range of u_long.  I don't
       even remember why I adopted that for the hash function in the
       first place, but we have to stick to existing hash values recorded
       elsewhere.

       On x86 with 8087-compatible FPU, (u_long)(d) behaves as follows.
       If -2^63 < d < 2^63, the modulo of 2^32 is taken.  Otherwise
       it yields 0.

       On x86 with SSE, the out-of-range value yields #x8000_0000.

       On x86_64, if -2^63 < d < 2^64, the modulo of 2^32 is taken.
       Otherwise it yields #x8000_0000_0000_0000.

       To achieve maximum compatibility with historical data, we
       take the range of 8087, and the calculation is adjusted for
       x86_64 behavior.  There can be a slight discrepancy from the
       result of 8087 because of its internal 80bit calculation---for
       example, 3.767278962604362e-10 * 2654435761 is just tiny little
       bit less than 1.0 but with 64bit calculation it is rounded up
       to 1.0.  With 80 bit and integer truncation the result is 0 but
       with 64bit we get 1.
     */
    volatile double d = f * 2654435761UL;
    static double two_pow_63 = 0.0;
    static double minus_two_pow_63 = 0.0;
    static double two_pow_32 = 0.0;
    static int initialized = FALSE;

    if (!initialized) {
        /* This is idempotent - no need to lock */
        two_pow_63 = ldexp(1.0, 63);
        minus_two_pow_63 = -ldexp(1.0, 63);
        two_pow_32 = ldexp(1.0, 32);
        AO_nop_full();
        initialized = TRUE;
    }
    /* This condition eliminates NaN as well. */
    if (!(minus_two_pow_63 < d && d < two_pow_63)) return 0;
    if (-0.5 < d && d < 0.5) return 0;
                                 
    double dm = trunc(fmod(d, two_pow_32));
    if (dm < 0) dm += two_pow_32;
    return (u_long)trunc(dm);
}

/* Old hash function for numeric objects.  This is terrible for flonums,
   and we only keep it in order to maintain portable hash value generated
   by legacy hash function. */
static u_long legacy_number_hash(ScmObj obj)
{
    u_long hashval;
    SCM_ASSERT(SCM_NUMBERP(obj));
    if (SCM_INTP(obj)) {
        SMALL_INT_HASH(hashval, SCM_INT_VALUE(obj));
    } else if (SCM_BIGNUMP(obj)) {
        u_int i;
        u_long u = 0;
        for (i=0; i<SCM_BIGNUM_SIZE(obj); i++) {
#if SIZEOF_LONG == 4
            u += SCM_BIGNUM(obj)->values[i];
#elif SIZEOF_LONG == 8
            u += (SCM_BIGNUM(obj)->values[i] & ((1UL<<32) - 1))
                + (SCM_BIGNUM(obj)->values[i] >> 32);
#else
#error "sizeof(long) > 8 platform unsupported"
#endif
        }
        SMALL_INT_HASH(hashval, u);
    } else if (SCM_FLONUMP(obj)) {
        hashval = legacy_flonum_hash(SCM_FLONUM_VALUE(obj));
    } else if (SCM_RATNUMP(obj)) {
        /* Ratnum must already be normalized, so we can simply combine
           hashvals of numerator and denominator. */
        u_long h1 = legacy_number_hash(SCM_RATNUM_NUMER(obj));
        u_long h2 = legacy_number_hash(SCM_RATNUM_DENOM(obj));
        hashval = COMBINE(h1, h2);
    } else {
        hashval =
            legacy_flonum_hash(SCM_COMPNUM_REAL(obj)+SCM_COMPNUM_IMAG(obj));
    }
    return hashval&PORTABLE_HASHMASK;
}

/* Legacy hash function for strings.  This isn't very good hash function
   either, and it's difficult to adopt salting. */
static u_long legacy_string_hash(ScmString *str)
{
    const ScmStringBody *b = SCM_STRING_BODY(str);
    const char *p = SCM_STRING_BODY_START(b);
    long k = SCM_STRING_BODY_SIZE(b);
    u_long hv = 0;
    while (k-- > 0) {
        hv = (hv<<5) - (hv) + ((unsigned char)*p++);
    }
    return hv&PORTABLE_HASHMASK;
}
