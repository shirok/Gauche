/*
 * spvec.c - Sparse vector
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

#include "spvec.h"

#define INDEX_CHECK(n)  /*empty*/

/*===================================================================
 * Generic stuff
 * NB: The generic constructor is defined near the bottom of the source,
 * since it should come after static descriptor definitions.
 */

/* Common CPL */
ScmClass *spvec_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_SparseVectorBaseClass),
    SCM_CLASS_STATIC_PTR(Scm_DictionaryClass),
    SCM_CLASS_STATIC_PTR(Scm_CollectionClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

ScmClass *spmat_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_SparseMatrixBaseClass),
    SCM_CLASS_STATIC_PTR(Scm_CollectionClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

SCM_DEFINE_BUILTIN_CLASS(Scm_SparseVectorBaseClass,
                         NULL, NULL, NULL, NULL,
                         spvec_cpl+1);

SCM_DEFINE_BUILTIN_CLASS(Scm_SparseMatrixBaseClass,
                         NULL, NULL, NULL, NULL,
                         spmat_cpl+1);

ScmObj SparseVectorRef(SparseVector *sv, u_long index, ScmObj fallback)
{
    INDEX_CHECK(index);
    Leaf *leaf = CompactTrieGet(&sv->trie, index >> sv->desc->shift);
    if (leaf == NULL) return fallback;
    ScmObj v = sv->desc->ref(leaf, index);
    if (SCM_UNBOUNDP(v)) return fallback;
    else return v;
}

void SparseVectorSet(SparseVector *sv, u_long index, ScmObj value)
{
    INDEX_CHECK(index);
    Leaf *leaf = CompactTrieAdd(&sv->trie, index >> sv->desc->shift,
                                sv->desc->allocate, sv);
    /* set returns TRUE if this is a new entry */
    if (sv->desc->set(leaf, index, value)) sv->numEntries++;
}

/* returns value of the deleted entry, or SCM_UNBOUND if there's no entry */
ScmObj SparseVectorDelete(SparseVector *sv, u_long index)
{
    INDEX_CHECK(index);
    Leaf *leaf = CompactTrieGet(&sv->trie, index >> sv->desc->shift);
    if (leaf == NULL) return SCM_UNBOUND;
    ScmObj r = sv->desc->delete(leaf, index);
    if (!SCM_UNBOUNDP(r)) sv->numEntries--;
    return r;
}

void SparseVectorClear(SparseVector *sv)
{
    sv->numEntries = 0;
    CompactTrieClear(&sv->trie, sv->desc->clear, sv->desc);
}

ScmObj SparseVectorCopy(const SparseVector *src)
{
    SparseVector *dst =
        (SparseVector*)MakeSparseVector(Scm_ClassOf(SCM_OBJ(src)),
                                        src->defaultValue,
                                        src->flags);
    CompactTrieCopy(&dst->trie, &src->trie, src->desc->copy, src->desc);
    dst->numEntries = src->numEntries;
    return SCM_OBJ(dst);
}

void SparseVectorIterInit(SparseVectorIter *iter, SparseVector *sv)
{
    iter->sv = sv;
    iter->leaf = NULL;
    CompactTrieIterInit(&iter->citer, &sv->trie);
    iter->leafIndex = -1;
}

ScmObj SparseVectorIterNext(SparseVectorIter *iter)
{
    ScmObj (*iterproc)(Leaf*,int*) = iter->sv->desc->iter;
    for (;;) {
        if (iter->leaf) {
            ScmObj r = iterproc(iter->leaf, &iter->leafIndex);
            if (!SCM_UNBOUNDP(r)) {
                u_long ind = ((leaf_key(iter->leaf) << iter->sv->desc->shift)
                              + iter->leafIndex);
                return Scm_Cons(Scm_MakeIntegerU(ind), r);
            }
        }
        iter->leaf = CompactTrieIterNext(&iter->citer);
        if (iter->leaf == NULL) return SCM_FALSE; /* we're at the end */
        iter->leafIndex = -1;
    }
}

/* special routine for uniform numeric sparse vectors */
/* TODO: Allow clamp arg */
ScmObj SparseVectorInc(SparseVector *sv, u_long index,
                       ScmObj delta,    /* number */
                       ScmObj fallback) /* number or unbound */
{
    if (!SCM_NUMBERP(fallback)) {
        if (SCM_NUMBERP(sv->defaultValue)) {
            fallback = sv->defaultValue;
        } else {
            fallback = SCM_MAKE_INT(0);
        }
    }
    INDEX_CHECK(index);
    Leaf *leaf = CompactTrieGet(&sv->trie, index >> sv->desc->shift);
    if (leaf == NULL) {
        ScmObj v = Scm_Add(fallback, delta);
        SparseVectorSet(sv, index, v);
        return v;
    } else {
        ScmObj v = sv->desc->ref(leaf, index);
        if (SCM_UNBOUNDP(v)) v = fallback;
        v = Scm_Add(v, delta);
        sv->desc->set(leaf, index, v);
        return v;
    }
}

void SparseVectorDump(SparseVector *sv)
{
    CompactTrieDump(SCM_CUROUT, &sv->trie, sv->desc->dump, sv->desc);
}

/*===================================================================
 * Individual vector types
 */

/*-------------------------------------------------------------------
 * General vector
 */

typedef struct GLeafRec {
    Leaf hdr;
    ScmObj val[2];
} GLeaf;

static ScmObj g_ref(Leaf *leaf, u_long index)
{
    return ((GLeaf*)leaf)->val[index&1];
}

static Leaf *g_allocate(void *data)
{
    GLeaf *z = SCM_NEW(GLeaf);
    z->val[0] = z->val[1] = SCM_UNBOUND;
    return (Leaf*)z;
}

static int g_set(Leaf *leaf, u_long index, ScmObj value)
{
    ScmObj v = ((GLeaf*)leaf)->val[index&1];
    ((GLeaf*)leaf)->val[index&1] = value;
    return SCM_UNBOUNDP(v);
}

static ScmObj g_delete(Leaf *leaf, u_long index)
{
    ScmObj v = ((GLeaf*)leaf)->val[index&1];
    ((GLeaf*)leaf)->val[index&1] = SCM_UNBOUND;
    return v;
}

static void g_clear(Leaf *leaf, void *data)
{
    GLeaf *z = (GLeaf*)leaf;
    z->val[0] = z->val[1] = NULL;
}

static Leaf *g_copy(Leaf *leaf, void *data)
{
    GLeaf *dst = SCM_NEW(GLeaf);
    *dst = *(GLeaf*)leaf;
    return (Leaf*)dst;
}

static ScmObj g_iter(Leaf *leaf, int *index)
{
    GLeaf *z = (GLeaf*)leaf;
    if (++(*index) == 0) {
        if (!SCM_UNBOUNDP(z->val[0])) return z->val[0];
        (*index)++;
        /*FALLTHROUGH*/
    }
    if (*index == 1) {
        if (!SCM_UNBOUNDP(z->val[1])) return z->val[1];
        (*index)++;
    }
    return SCM_UNBOUND;
}

static void g_dump(ScmPort *out, Leaf *leaf, int indent, void *data)
{
    GLeaf *z = (GLeaf*)leaf;
    for (int i=0; i<2; i++) {
        if (!SCM_UNBOUNDP(z->val[i])) {
            Scm_Printf(out, "\n  %*s%2d: %25.1S", indent, "", i, z->val[i]);
        }
    }
}

static SparseVectorDescriptor g_desc = {
    g_ref, g_set, g_allocate, g_delete, g_clear, g_copy, g_iter, g_dump, 1
};

SCM_DEFINE_BUILTIN_CLASS(Scm_SparseVectorClass, NULL, NULL, NULL, NULL,
                         spvec_cpl);

SCM_DEFINE_BUILTIN_CLASS(Scm_SparseMatrixClass, NULL, NULL, NULL, NULL,
                         spmat_cpl);

/*-------------------------------------------------------------------
 * Uniform sparse vector common stuff
 */

typedef struct ULeafRec {
    Leaf hdr;                   /* LEAF_DATA has bitmap of used entries */
    union {
        ScmWord dummy[2];       /* placeholder */
        signed char    s8[2*SIZEOF_LONG];
        unsigned char  u8[2*SIZEOF_LONG];
        signed short   s16[SIZEOF_LONG];
        unsigned short u16[SIZEOF_LONG];
        ScmInt32       s32[SIZEOF_LONG/2];
        ScmUInt32      u32[SIZEOF_LONG/2];
        ScmInt64       s64[SIZEOF_LONG/4];
        ScmUInt64      u64[SIZEOF_LONG/4];
        ScmHalfFloat   f16[SIZEOF_LONG];
        float          f32[SIZEOF_LONG/2];
        double         f64[SIZEOF_LONG/4];
    };
} ULeaf;

#define ULEAF(leaf)  ((ULeaf*)leaf)

#if SIZEOF_LONG == 4
#define SHIFT8   3
#define SHIFT16  2
#define SHIFT32  1
#define SHIFT64  0
#else  /* SIZEOF_LONG != 4 */
#define SHIFT8   4
#define SHIFT16  3
#define SHIFT32  2
#define SHIFT64  1
#endif /* SIZEOF_LONG != 4 */

#define MASK8  ((1UL<<SHIFT8)-1)
#define MASK16 ((1UL<<SHIFT16)-1)
#define MASK32 ((1UL<<SHIFT32)-1)
#define MASK64 ((1UL<<SHIFT64)-1)

#define U_HAS_ENTRY(leaf, ind, mask) leaf_data_bit_test(leaf, (ind)&(mask))
#define U_SET_ENTRY(leaf, ind, mask) leaf_data_bit_set(leaf, (ind)&(mask))
#define U_RESET_ENTRY(leaf, ind, mask) leaf_data_bit_reset(leaf, (ind)&(mask))

static Leaf *u_allocate(void *data)
{
    ULeaf *z = SCM_NEW_ATOMIC(ULeaf);
    z->dummy[0] = z->dummy[1] = 0;
    return (Leaf*)z;
}

static void u_clear(Leaf *leaf, void *data)
{
    /* nothing to do */
}

static Leaf *u_copy(Leaf *leaf, void *data)
{
    ULeaf *z = SCM_NEW_ATOMIC(ULeaf);
    memcpy(z, leaf, sizeof(ULeaf));
    return (Leaf*)z;
}

/*-------------------------------------------------------------------
 * Uniform Sparse Vector Ref
 */

#define REF_CHECK(leaf, index, mask) \
    do { if (!U_HAS_ENTRY(leaf, index, mask)) return SCM_UNBOUND; } while(0)

#define U_REF(tag, mask, box)                                           \
    static ScmObj SCM_CPP_CAT(tag,_ref)(Leaf *leaf, u_long index)       \
    {                                                                   \
        REF_CHECK(leaf, index, mask);                                   \
        return box(ULEAF(leaf)->tag[index&mask]);                       \
    }

#define F16BOX(v) Scm_VMReturnFlonum(Scm_HalfToDouble(v))
#define F32BOX(v) Scm_VMReturnFlonum((double)(v))

U_REF(s8, MASK8, SCM_MAKE_INT)
U_REF(u8, MASK8, SCM_MAKE_INT)
U_REF(s16, MASK16, SCM_MAKE_INT)
U_REF(u16, MASK16, SCM_MAKE_INT)
U_REF(s32, MASK32, Scm_MakeInteger)
U_REF(u32, MASK32, Scm_MakeIntegerU)
U_REF(s64, MASK64, Scm_MakeInteger64)
U_REF(u64, MASK64, Scm_MakeIntegerU64)
U_REF(f16, MASK16, F16BOX)
U_REF(f32, MASK32, F32BOX)
U_REF(f64, MASK64, Scm_VMReturnFlonum)

/*-------------------------------------------------------------------
 * Uniform Sparse Vector Set
 */

#define U_SET_INT(elt, mask, getter)                                    \
    do {                                                                \
        ULEAF(leaf)->elt[index&mask] = getter(val, SCM_CLAMP_ERROR, NULL); \
        U_SET_CHECK(mask);                                              \
    } while(0)

#define U_SET_CHECK(mask)                       \
    do {                                        \
        int z = U_HAS_ENTRY(leaf, index, mask); \
        U_SET_ENTRY(leaf, index, mask);         \
        return !z;                              \
    } while (0)

static int s8_set(Leaf *leaf, u_long index, ScmObj val)
{
    U_SET_INT(s8, MASK8, Scm_GetInteger8Clamp);
}

static int u8_set(Leaf *leaf, u_long index, ScmObj val)
{
    U_SET_INT(u8, MASK8, Scm_GetIntegerU8Clamp);
}

static int s16_set(Leaf *leaf, u_long index, ScmObj val)
{
    U_SET_INT(s16, MASK16, Scm_GetInteger16Clamp);
}

static int u16_set(Leaf *leaf, u_long index, ScmObj val)
{
    U_SET_INT(u16, MASK16, Scm_GetIntegerU16Clamp);
}

static int s32_set(Leaf *leaf, u_long index, ScmObj val)
{
    U_SET_INT(s32, MASK32, Scm_GetInteger32Clamp);
}

static int u32_set(Leaf *leaf, u_long index, ScmObj val)
{
    U_SET_INT(u32, MASK32, Scm_GetIntegerU32Clamp);
}

static int s64_set(Leaf *leaf, u_long index, ScmObj val)
{
    U_SET_INT(s64, MASK64, Scm_GetInteger64Clamp);
}

static int u64_set(Leaf *leaf, u_long index, ScmObj val)
{
    U_SET_INT(u64, MASK64, Scm_GetIntegerU64Clamp);
}

static int f16_set(Leaf *leaf, u_long index, ScmObj val)
{
    ULEAF(leaf)->f16[index&MASK16] = Scm_DoubleToHalf(Scm_GetDouble(val));
    U_SET_CHECK(MASK16);
}

static int f32_set(Leaf *leaf, u_long index, ScmObj val)
{
    ULEAF(leaf)->f32[index&MASK32] = (float)Scm_GetDouble(val);
    U_SET_CHECK(MASK32);
}

static int f64_set(Leaf *leaf, u_long index, ScmObj val)
{
    ULEAF(leaf)->f64[index&MASK64] = Scm_GetDouble(val);
    U_SET_CHECK(MASK64);
}

/*-------------------------------------------------------------------
 * Uniform Sparse Vector Delete
 */

#define U_DEL(ref, mask)                        \
    do {                                        \
        ScmObj r = ref(leaf, index);            \
        if (SCM_UNBOUNDP(r)) return r;          \
        U_RESET_ENTRY(leaf, index, mask);       \
        return r;                               \
    } while (0);

static ScmObj s8_delete(Leaf *leaf, u_long index)
{
    U_DEL(s8_ref, MASK8);
}

static ScmObj u8_delete(Leaf *leaf, u_long index)
{
    U_DEL(u8_ref, MASK8);
}

static ScmObj s16_delete(Leaf *leaf, u_long index)
{
    U_DEL(s16_ref, MASK16);
}

static ScmObj u16_delete(Leaf *leaf, u_long index)
{
    U_DEL(u16_ref, MASK16);
}

static ScmObj s32_delete(Leaf *leaf, u_long index)
{
    U_DEL(s32_ref, MASK32);
}

static ScmObj u32_delete(Leaf *leaf, u_long index)
{
    U_DEL(u32_ref, MASK32);
}

static ScmObj s64_delete(Leaf *leaf, u_long index)
{
    U_DEL(s64_ref, MASK64);
}

static ScmObj u64_delete(Leaf *leaf, u_long index)
{
    U_DEL(u64_ref, MASK64);
}

static ScmObj f16_delete(Leaf *leaf, u_long index)
{
    U_DEL(f16_ref, MASK16);
}

static ScmObj f32_delete(Leaf *leaf, u_long index)
{
    U_DEL(f32_ref, MASK32);
}

static ScmObj f64_delete(Leaf *leaf, u_long index)
{
    U_DEL(f64_ref, MASK64);
}

/*-------------------------------------------------------------------
 * Uniform Sparse Vector Iter
 */

static ScmObj u_iter_sub(Leaf *leaf, ScmObj (*ref)(Leaf*, u_long),
                         int *index, int mask)
{
    int i = *index;
    for (i++; i<=mask; i++) {
        if (U_HAS_ENTRY(leaf, i, mask)) {
            *index = i;
            return ref(leaf, i);
        }
    }
    *index = i;
    return SCM_UNBOUND;
}

#define U_ITER(tag, mask) \
    static ScmObj SCM_CPP_CAT(tag,_iter)(Leaf *leaf, int *index) { \
        return u_iter_sub(leaf, SCM_CPP_CAT(tag,_ref), index, mask); \
    }

U_ITER(s8, MASK8)
U_ITER(u8, MASK8)
U_ITER(s16, MASK16)
U_ITER(u16, MASK16)
U_ITER(s32, MASK32)
U_ITER(u32, MASK32)
U_ITER(s64, MASK64)
U_ITER(u64, MASK64)
U_ITER(f16, MASK16)
U_ITER(f32, MASK32)
U_ITER(f64, MASK64)

/*-------------------------------------------------------------------
 * Uniform Sparse Vector Descriptors and constructors
 */

#define U_DECL(tag, TAG, shift)                                         \
    static SparseVectorDescriptor SCM_CPP_CAT(tag, _desc) = {           \
        SCM_CPP_CAT(tag,_ref),                                          \
        SCM_CPP_CAT(tag,_set),                                          \
        u_allocate,                                                     \
        SCM_CPP_CAT(tag,_delete),                                       \
        u_clear,                                                        \
        u_copy,                                                         \
        SCM_CPP_CAT(tag,_iter),                                         \
        NULL, shift,                                                    \
    };                                                                  \
    SCM_DEFINE_BUILTIN_CLASS(SCM_CPP_CAT3(Scm_Sparse,TAG,VectorClass),  \
                             NULL, NULL, NULL, NULL, spvec_cpl);        \
    SCM_DEFINE_BUILTIN_CLASS(SCM_CPP_CAT3(Scm_Sparse,TAG,MatrixClass),  \
                             NULL, NULL, NULL, NULL, spmat_cpl)

U_DECL(s8, S8, SHIFT8);
U_DECL(u8, U8, SHIFT8);
U_DECL(s16, S16, SHIFT16);
U_DECL(u16, U16, SHIFT16);
U_DECL(s32, S32, SHIFT32);
U_DECL(u32, U32, SHIFT32);
U_DECL(s64, S64, SHIFT64);
U_DECL(u64, U64, SHIFT64);
U_DECL(f16, F16, SHIFT16);
U_DECL(f32, F32, SHIFT32);
U_DECL(f64, F64, SHIFT64);

/*===================================================================
 * Generic constructor
 */

ScmObj MakeSparseVector(ScmClass *klass, ScmObj defaultValue, u_long flags)
{
    SparseVectorDescriptor *desc = NULL;

    if (SCM_EQ(SCM_CLASS_SPARSE_VECTOR, klass))         desc = &g_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_S8VECTOR, klass))  desc = &s8_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_U8VECTOR, klass))  desc = &u8_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_S16VECTOR, klass)) desc = &s16_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_U16VECTOR, klass)) desc = &u16_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_S32VECTOR, klass)) desc = &s32_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_U32VECTOR, klass)) desc = &u32_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_S64VECTOR, klass)) desc = &s64_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_U64VECTOR, klass)) desc = &u64_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_F16VECTOR, klass)) desc = &f16_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_F32VECTOR, klass)) desc = &f32_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_F64VECTOR, klass)) desc = &f64_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_MATRIX, klass))    desc = &g_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_S8MATRIX, klass))  desc = &s8_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_U8MATRIX, klass))  desc = &u8_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_S16MATRIX, klass)) desc = &s16_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_U16MATRIX, klass)) desc = &u16_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_S32MATRIX, klass)) desc = &s32_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_U32MATRIX, klass)) desc = &u32_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_S64MATRIX, klass)) desc = &s64_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_U64MATRIX, klass)) desc = &u64_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_F16MATRIX, klass)) desc = &f16_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_F32MATRIX, klass)) desc = &f32_desc;
    else if (SCM_EQ(SCM_CLASS_SPARSE_F64MATRIX, klass)) desc = &f64_desc;
    else {
        Scm_TypeError("class", "subclass of <sparse-vector-base>",
                      SCM_OBJ(klass));
    }

    SparseVector *v = SCM_NEW(SparseVector);
    SCM_SET_CLASS(v, klass);
    CompactTrieInit(&v->trie);
    v->numEntries = 0;
    v->desc = desc;
    v->flags = flags;
    v->defaultValue = defaultValue;
    return SCM_OBJ(v);
}

/*===================================================================
 * Initialization
 */

void Scm_Init_spvec(ScmModule *mod)
{
#define INITC(klass, name) \
    Scm_InitStaticClass(&klass, name, mod, NULL, 0)

    INITC(Scm_SparseVectorBaseClass, "<sparse-vector-base>");
    INITC(Scm_SparseVectorClass, "<sparse-vector>");
    INITC(Scm_SparseS8VectorClass, "<sparse-s8vector>");
    INITC(Scm_SparseU8VectorClass, "<sparse-u8vector>");
    INITC(Scm_SparseS16VectorClass, "<sparse-s16vector>");
    INITC(Scm_SparseU16VectorClass, "<sparse-u16vector>");
    INITC(Scm_SparseS32VectorClass, "<sparse-s32vector>");
    INITC(Scm_SparseU32VectorClass, "<sparse-u32vector>");
    INITC(Scm_SparseS64VectorClass, "<sparse-s64vector>");
    INITC(Scm_SparseU64VectorClass, "<sparse-u64vector>");
    INITC(Scm_SparseF16VectorClass, "<sparse-f16vector>");
    INITC(Scm_SparseF32VectorClass, "<sparse-f32vector>");
    INITC(Scm_SparseF64VectorClass, "<sparse-f64vector>");

    INITC(Scm_SparseMatrixBaseClass, "<sparse-matrix-base>");
    INITC(Scm_SparseMatrixClass, "<sparse-matrix>");
    INITC(Scm_SparseS8MatrixClass, "<sparse-s8matrix>");
    INITC(Scm_SparseU8MatrixClass, "<sparse-u8matrix>");
    INITC(Scm_SparseS16MatrixClass, "<sparse-s16matrix>");
    INITC(Scm_SparseU16MatrixClass, "<sparse-u16matrix>");
    INITC(Scm_SparseS32MatrixClass, "<sparse-s32matrix>");
    INITC(Scm_SparseU32MatrixClass, "<sparse-u32matrix>");
    INITC(Scm_SparseS64MatrixClass, "<sparse-s64matrix>");
    INITC(Scm_SparseU64MatrixClass, "<sparse-u64matrix>");
    INITC(Scm_SparseF16MatrixClass, "<sparse-f16matrix>");
    INITC(Scm_SparseF32MatrixClass, "<sparse-f32matrix>");
    INITC(Scm_SparseF64MatrixClass, "<sparse-f64matrix>");
}
