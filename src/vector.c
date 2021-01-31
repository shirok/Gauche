/*
 * vector.c - vector implementation
 *
 *   Copyright (c) 2000-2020  Shiro Kawai  <shiro@acm.org>
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
#include "gauche/priv/writerP.h"
#include "gauche/priv/mmapP.h"

#if defined(HAVE_SYS_MMAN_H)
#include <sys/mman.h>
#endif

/* Catch integer overflow.
   NB: If total size is too big, GC_malloc aborts.  But we have to prevent
   total size from overflowing before passed to GC_malloc.

   We'll try to allocate size*eltsize (+ up to two words of header).
   The GC's malloc routine first round it up to GC allocation unit boundary
   (8 or 16 bytes).  If there's not enough heap, then it tries to expand
   the heap by the size rounded up to the pagesize.  We don't want the final
   value overflows signed long.
   (In reality, expanding heap with close to LONG_MAX surely fails, so it  
   should suffice to avoid overflow before calling GC_MALLOC. But it won't
   harm to have a bit of margin here...)
 */
static void check_size(ScmSmallInt size, int eltsize)
{
    if (size >= (ScmSmallInt)((LONG_MAX - 0x400000)/eltsize)) {
        Scm_Error("Size too big: %ld", size);
    }
}

/*=====================================================================
 * Generic vectors
 */

/*
 * Constructor
 */

static void vector_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    SCM_PUTZ("#(", -1, port);
    for (int i=0; i<SCM_VECTOR_SIZE(obj); i++) {
        if (i != 0) SCM_PUTC(' ', port);
        Scm_Write(SCM_VECTOR_ELEMENT(obj, i), SCM_OBJ(port),
                  Scm_WriteContextMode(ctx));
    }
    SCM_PUTZ(")", -1, port);
}

static int vector_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        /* Vector equality is handled in Scm_Eq* and will never come
           here, but just in case. */
        return Scm_EqualP(x, y)? 0 : 1;
    }
    /* Follow srfi-114 */
    ScmWord xlen = SCM_VECTOR_SIZE(x);
    ScmWord ylen = SCM_VECTOR_SIZE(y);
    if (xlen < ylen) return -1;
    if (xlen > ylen) return 1;
    for (int i=0; i<xlen; i++) {
        int r = Scm_Compare(SCM_VECTOR_ELEMENT(x, i),
                            SCM_VECTOR_ELEMENT(y, i));
        if (r != 0) return r;
    }
    return 0;
}


SCM_DEFINE_BUILTIN_CLASS_FLAGS(Scm_VectorClass, vector_print, vector_compare,
                               NULL, NULL, SCM_CLASS_SEQUENCE_CPL,
                               SCM_CLASS_AGGREGATE);

static ScmVector *make_vector(ScmSmallInt size)
{
    check_size(size, sizeof(ScmObj));
    ScmVector *v = SCM_NEW2(ScmVector *,
                            sizeof(ScmVector) + sizeof(ScmObj)*(size-1));
    SCM_SET_CLASS(v, SCM_CLASS_VECTOR);
#if GAUCHE_API_VERSION >= 1000
    v->size_flags = (size << 1);
#else    
    v->size = size;
#endif
    return v;
}

ScmObj Scm_MakeVector(ScmSmallInt size, ScmObj fill)
{
    if (size < 0) {
        Scm_Error("vector size must be a positive integer, but got %d", size);
    }
    ScmVector *v = make_vector(size);
    if (SCM_UNBOUNDP(fill)) fill = SCM_UNDEFINED;
    for (ScmSmallInt i=0; i<size; i++) v->elements[i] = fill;
    return SCM_OBJ(v);
}

ScmObj Scm_ListToVector(ScmObj l, ScmSmallInt start, ScmSmallInt end)
{
    ScmVector *v;

    if (end < 0) {
        ScmSmallInt size = Scm_Length(l);
        if (size < 0) Scm_Error("bad list: %S", l);
        SCM_CHECK_START_END(start, end, size);
        v = make_vector(size - start);
    } else {
        SCM_CHECK_START_END(start, end, end);
        v = make_vector(end - start);
    }
    ScmObj e = Scm_ListTail(l, start, SCM_UNBOUND);
    for (ScmSmallInt i=0; i<end-start; i++, e=SCM_CDR(e)) {
        if (!SCM_PAIRP(e)) {
            Scm_Error("list too short: %S", l);
        }
        v->elements[i] = SCM_CAR(e);
    }
    return SCM_OBJ(v);
}

ScmObj Scm_VectorToList(ScmVector *v, ScmSmallInt start, ScmSmallInt end)
{
    ScmWord len = SCM_VECTOR_SIZE(v);
    SCM_CHECK_START_END(start, end, len);
    return Scm_ArrayToList(SCM_VECTOR_ELEMENTS(v)+start,
                           end-start);
}

/*
 * Accessors
 */

/* NB: we're permissive about the out-of-range index here; the strict
   check (for Scheme routines) should be done in the stub file, since
   Scheme version may receive bignum, which can't be passed to C API. */

ScmObj Scm_VectorRef(ScmVector *vec, ScmSmallInt i, ScmObj fallback)
{
    if (i < 0 || i >= SCM_VECTOR_SIZE(vec)) return fallback;
    return vec->elements[i];
}

ScmObj Scm_VectorSet(ScmVector *vec, ScmSmallInt i, ScmObj obj)
{
    SCM_VECTOR_CHECK_MUTABLE(vec);
    if (i >= 0 && i < SCM_VECTOR_SIZE(vec)) vec->elements[i] = obj;
    return obj;
}

ScmObj Scm_VectorFill(ScmVector *vec, ScmObj fill,
                      ScmSmallInt start, ScmSmallInt end)
{
    SCM_VECTOR_CHECK_MUTABLE(vec);
    ScmSmallInt len = SCM_VECTOR_SIZE(vec);
    SCM_CHECK_START_END(start, end, len);
    for (ScmSmallInt i=start; i < end; i++) {
        SCM_VECTOR_ELEMENT(vec, i) = fill;
    }
    return SCM_OBJ(vec);
}

ScmObj Scm_VectorCopy(ScmVector *vec,
                      ScmSmallInt start, ScmSmallInt end, ScmObj fill)
{
    ScmSmallInt len = SCM_VECTOR_SIZE(vec);
    ScmVector *v = NULL;
    if (end < 0) end = len;
    if (end < start) {
        Scm_Error("vector-copy: start (%ld) is greater than end (%ld)",
                  start, end);
    } else if (end == start) {
        v = make_vector(0);
    } else {
        if (SCM_UNBOUNDP(fill)) fill = SCM_UNDEFINED;
        v = make_vector(end - start);
        for (ScmSmallInt i=0; i<end-start; i++) {
            if (i+start < 0 || i+start >= len) {
                SCM_VECTOR_ELEMENT(v, i) = fill;
            } else {
                SCM_VECTOR_ELEMENT(v, i) = SCM_VECTOR_ELEMENT(vec, i+start);
            }
        }
    }
    return SCM_OBJ(v);
}

/*=====================================================================
 * Uniform vectors
 */

/*
 * Class stuff
 */
static ScmClass *uvector_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_UVectorClass),
    SCM_CLASS_STATIC_PTR(Scm_SequenceClass),
    SCM_CLASS_STATIC_PTR(Scm_CollectionClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

SCM_DEFINE_BUILTIN_CLASS(Scm_UVectorClass, NULL, NULL, NULL, NULL,
                         uvector_cpl+1);

#define DEF_UVCLASS(TAG, tag)                                           \
static void SCM_CPP_CAT3(print_,tag,vector)(ScmObj obj, ScmPort *out,   \
                                            ScmWriteContext *ctx);      \
static int SCM_CPP_CAT3(compare_,tag,vector)(ScmObj x, ScmObj y, int equalp); \
SCM_DEFINE_BUILTIN_CLASS_FLAGS(SCM_CPP_CAT3(Scm_,TAG,VectorClass),      \
                               SCM_CPP_CAT3(print_,tag,vector),         \
                               SCM_CPP_CAT3(compare_,tag,vector),       \
                               NULL, NULL, uvector_cpl,                 \
                               SCM_CLASS_AGGREGATE);

DEF_UVCLASS(S8, s8)
DEF_UVCLASS(U8, u8)
DEF_UVCLASS(S16, s16)
DEF_UVCLASS(U16, u16)
DEF_UVCLASS(S32, s32)
DEF_UVCLASS(U32, u32)
DEF_UVCLASS(S64, s64)
DEF_UVCLASS(U64, u64)
DEF_UVCLASS(F16, f16)
DEF_UVCLASS(F32, f32)
DEF_UVCLASS(F64, f64)
DEF_UVCLASS(C32, c32)
DEF_UVCLASS(C64, c64)
DEF_UVCLASS(C128, c128)

/*
 * Some generic APIs
 */
ScmUVectorType Scm_UVectorType(ScmClass *klass)
{
    if (SCM_EQ(klass, SCM_CLASS_S8VECTOR))   return SCM_UVECTOR_S8;
    if (SCM_EQ(klass, SCM_CLASS_U8VECTOR))   return SCM_UVECTOR_U8;
    if (SCM_EQ(klass, SCM_CLASS_S16VECTOR))  return SCM_UVECTOR_S16;
    if (SCM_EQ(klass, SCM_CLASS_U16VECTOR))  return SCM_UVECTOR_U16;
    if (SCM_EQ(klass, SCM_CLASS_S32VECTOR))  return SCM_UVECTOR_S32;
    if (SCM_EQ(klass, SCM_CLASS_U32VECTOR))  return SCM_UVECTOR_U32;
    if (SCM_EQ(klass, SCM_CLASS_S64VECTOR))  return SCM_UVECTOR_S64;
    if (SCM_EQ(klass, SCM_CLASS_U64VECTOR))  return SCM_UVECTOR_U64;
    if (SCM_EQ(klass, SCM_CLASS_F16VECTOR))  return SCM_UVECTOR_F16;
    if (SCM_EQ(klass, SCM_CLASS_F32VECTOR))  return SCM_UVECTOR_F32;
    if (SCM_EQ(klass, SCM_CLASS_F64VECTOR))  return SCM_UVECTOR_F64;
    if (SCM_EQ(klass, SCM_CLASS_C32VECTOR))  return SCM_UVECTOR_C32;
    if (SCM_EQ(klass, SCM_CLASS_C64VECTOR))  return SCM_UVECTOR_C64;
    if (SCM_EQ(klass, SCM_CLASS_C128VECTOR)) return SCM_UVECTOR_C128;
    else return SCM_UVECTOR_INVALID;
}

const char *Scm_UVectorTypeName(int type) /* for error msgs etc. */
{
    switch (type) {
    case SCM_UVECTOR_S8:   return "s8vector";
    case SCM_UVECTOR_U8:   return "u8vector";
    case SCM_UVECTOR_S16:  return "s16vector";
    case SCM_UVECTOR_U16:  return "u16vector";
    case SCM_UVECTOR_S32:  return "s32vector";
    case SCM_UVECTOR_U32:  return "u32vector";
    case SCM_UVECTOR_S64:  return "s64vector";
    case SCM_UVECTOR_U64:  return "u64vector";
    case SCM_UVECTOR_F16:  return "f16vector";
    case SCM_UVECTOR_F32:  return "f32vector";
    case SCM_UVECTOR_F64:  return "f64vector";
    case SCM_UVECTOR_C32:  return "c32vector";
    case SCM_UVECTOR_C64:  return "c64vector";
    case SCM_UVECTOR_C128: return "c128vector";
    default: return "invalid type of uvector (possibly implementation error)";
    }
}

/* Returns the size of element of the uvector of given class */
int Scm_UVectorElementSize(ScmClass *klass)
{
    static const int sizes[] = { 1, 1, 2, 2, 4, 4, 8, 8,
                                 2, sizeof(float), sizeof(double), -1,
                                 sizeof(ScmHalfComplex),
                                 sizeof(ScmFloatComplex),
                                 sizeof(ScmDoubleComplex),
                                 -1 };
    int ind = (int)Scm_UVectorType(klass);
    if (ind >= 0) return sizes[ind];
    return -1;
}

/* Returns the size of the vector body in bytes */
int Scm_UVectorSizeInBytes(ScmUVector *uv)
{
    return SCM_UVECTOR_SIZE(uv) * Scm_UVectorElementSize(Scm_ClassOf(SCM_OBJ(uv)));
}

/* Generic constructor */
ScmObj Scm_MakeUVectorFull(ScmClass *klass, ScmSmallInt size, void *init,
                           int immutable, void *owner)
{
    int eltsize = Scm_UVectorElementSize(klass);
    SCM_ASSERT(eltsize >= 1);
    ScmUVector *vec = SCM_NEW(ScmUVector);
    SCM_SET_CLASS(vec, klass);
    if (init) {
        vec->elements = init;   /* trust the caller */
    } else {
        check_size(size, eltsize);
        vec->elements = SCM_NEW_ATOMIC2(void*, size*eltsize);
    }
    vec->size_flags = (size << 1)|(immutable?1:0);
    vec->owner = owner;
    return SCM_OBJ(vec);
}

ScmObj Scm_MakeUVector(ScmClass *klass, ScmSmallInt size, void *init)
{
    return Scm_MakeUVectorFull(klass, size, init, FALSE, NULL);
}

ScmObj Scm_ListToUVector(ScmClass *klass, ScmObj list, int clamp)
{
    ScmUVectorType type = Scm_UVectorType(klass);
    if (type < 0) Scm_Error("uvector class required, but got: %S", klass);
    ScmSize length = Scm_Length(list);
    if (length < 0) Scm_Error("improper list not allowed: %S", list);
    if (length > SCM_SMALL_INT_MAX) Scm_Error("list is too long: %,,,,100S", list);

    ScmUVector *v = (ScmUVector*)Scm_MakeUVector(klass,
                                                 (ScmSmallInt)length,
                                                 NULL);
    ScmObj cp = list;
    for (ScmSize i=0; i<length; i++, cp = SCM_CDR(cp)) {
        switch (type) {
        case SCM_UVECTOR_S8:
            SCM_S8VECTOR_ELEMENTS(v)[i] =
                (int8_t)Scm_GetInteger8Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_U8:
            SCM_U8VECTOR_ELEMENTS(v)[i] =
                (uint8_t)Scm_GetIntegerU8Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_S16:
            SCM_S16VECTOR_ELEMENTS(v)[i] =
                (int16_t)Scm_GetInteger16Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_U16:
            SCM_U16VECTOR_ELEMENTS(v)[i] =
                (uint16_t)Scm_GetIntegerU16Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_S32:
            SCM_S32VECTOR_ELEMENTS(v)[i] =
                (int32_t)Scm_GetInteger32Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_U32:
            SCM_U32VECTOR_ELEMENTS(v)[i] =
                (uint32_t)Scm_GetIntegerU32Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_S64:
            SCM_S64VECTOR_ELEMENTS(v)[i] =
                (int64_t)Scm_GetInteger64Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_U64:
            SCM_U64VECTOR_ELEMENTS(v)[i] =
                (uint64_t)Scm_GetIntegerU64Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_F16:
            SCM_F16VECTOR_ELEMENTS(v)[i] =
                (ScmHalfFloat)Scm_DoubleToHalf(Scm_GetDouble(SCM_CAR(cp)));
            break;
        case SCM_UVECTOR_F32:
            SCM_F32VECTOR_ELEMENTS(v)[i] =
                (float)Scm_GetDouble(SCM_CAR(cp));
            break;
        case SCM_UVECTOR_F64:
            SCM_F64VECTOR_ELEMENTS(v)[i] = Scm_GetDouble(SCM_CAR(cp));
            break;
        case SCM_UVECTOR_C32:
            SCM_C32VECTOR_ELEMENTS(v)[i] = Scm_GetHalfComplex(SCM_CAR(cp));
            break;
        case SCM_UVECTOR_C64:
            SCM_C64VECTOR_ELEMENTS(v)[i] = Scm_GetFloatComplex(SCM_CAR(cp));
            break;
        case SCM_UVECTOR_C128:
            SCM_C128VECTOR_ELEMENTS(v)[i] = Scm_GetDoubleComplex(SCM_CAR(cp));
            break;
        default:
            Scm_Error("[internal error] unknown uvector type given to Scm_ListToUVector");
        }
    }
    return SCM_OBJ(v);
}

/* Generic accessor, intended to be called from VM loop.
   (As the 'VM' in the name suggests, the return value of this API
   should immediately be passed to VM.  See comments on FFX in gauche/number.h)
 */
ScmObj Scm_VMUVectorRef(ScmUVector *v, int t, ScmSmallInt k, ScmObj fallback)
{
    SCM_ASSERT(Scm_UVectorType(SCM_CLASS_OF(v)) == t);
    if (k < 0 || k >= SCM_UVECTOR_SIZE(v)) {
        if (SCM_UNBOUNDP(fallback)) {
            Scm_Error("%s-ref index out of range: %ld",
                      Scm_UVectorTypeName(t), k);
        }
        return fallback;
    }
    switch (t) {
    case SCM_UVECTOR_S8:  return SCM_MAKE_INT(SCM_S8VECTOR_ELEMENT(v, k));
    case SCM_UVECTOR_U8:  return SCM_MAKE_INT(SCM_U8VECTOR_ELEMENT(v, k));
    case SCM_UVECTOR_S16: return SCM_MAKE_INT(SCM_S16VECTOR_ELEMENT(v, k));
    case SCM_UVECTOR_U16: return SCM_MAKE_INT(SCM_U16VECTOR_ELEMENT(v, k));
    case SCM_UVECTOR_S32: return Scm_MakeInteger(SCM_S32VECTOR_ELEMENT(v, k));
    case SCM_UVECTOR_U32: return Scm_MakeIntegerU(SCM_U32VECTOR_ELEMENT(v, k));
    case SCM_UVECTOR_S64: return Scm_MakeInteger64(SCM_S64VECTOR_ELEMENT(v, k));
    case SCM_UVECTOR_U64: return Scm_MakeIntegerU64(SCM_U64VECTOR_ELEMENT(v, k));
    case SCM_UVECTOR_F16:
        return Scm_VMReturnFlonum(Scm_HalfToDouble(SCM_F16VECTOR_ELEMENT(v, k)));
    case SCM_UVECTOR_F32:
        return Scm_VMReturnFlonum((double)(SCM_F32VECTOR_ELEMENT(v, k)));
    case SCM_UVECTOR_F64:
        return Scm_VMReturnFlonum(SCM_F64VECTOR_ELEMENT(v, k));
    case SCM_UVECTOR_C32:
        return Scm_HalfComplexToComplex(SCM_C32VECTOR_ELEMENT(v, k));
    case SCM_UVECTOR_C64:
        return Scm_FloatComplexToComplex(SCM_C64VECTOR_ELEMENT(v, k));
    case SCM_UVECTOR_C128:
        return Scm_DoubleComplexToComplex(SCM_C128VECTOR_ELEMENT(v, k));
    default:
        Scm_Error("[internal error] unknown uvector type given to Scm_VMUVectorRef");
        return SCM_UNDEFINED;   /* dummy */
    }
}

/* Generic modifier */
ScmObj Scm_UVectorSet(ScmUVector *v, int t, ScmSmallInt k, ScmObj val, int clamp)
{
    SCM_ASSERT(Scm_UVectorType(SCM_CLASS_OF(v)) == t);
    SCM_UVECTOR_CHECK_MUTABLE(SCM_OBJ(v));
    if (k < 0 || k >= SCM_UVECTOR_SIZE(v)) {
        Scm_Error("%s-set! index out of range: %ld", Scm_UVectorTypeName(t), k);
    }
    switch (t) {
    case SCM_UVECTOR_S8:
        SCM_S8VECTOR_ELEMENTS(v)[k] = Scm_GetInteger8Clamp(val, clamp, NULL);
        break;
    case SCM_UVECTOR_U8:
        SCM_U8VECTOR_ELEMENTS(v)[k] = Scm_GetIntegerU8Clamp(val, clamp, NULL);
        break;
    case SCM_UVECTOR_S16:
        SCM_S16VECTOR_ELEMENTS(v)[k] = Scm_GetInteger16Clamp(val, clamp, NULL);
        break;
    case SCM_UVECTOR_U16:
        SCM_U16VECTOR_ELEMENTS(v)[k] = Scm_GetIntegerU16Clamp(val, clamp, NULL);
        break;
    case SCM_UVECTOR_S32:
        SCM_S32VECTOR_ELEMENTS(v)[k] = Scm_GetInteger32Clamp(val, clamp, NULL);
        break;
    case SCM_UVECTOR_U32:
        SCM_U32VECTOR_ELEMENTS(v)[k] = Scm_GetIntegerU32Clamp(val, clamp, NULL);
        break;
    case SCM_UVECTOR_S64:
        SCM_S64VECTOR_ELEMENTS(v)[k] = Scm_GetInteger64Clamp(val, clamp, NULL);
        break;
    case SCM_UVECTOR_U64:
        SCM_U64VECTOR_ELEMENTS(v)[k] = Scm_GetIntegerU64Clamp(val, clamp, NULL);
        break;
    case SCM_UVECTOR_F16:
        SCM_F16VECTOR_ELEMENTS(v)[k] = Scm_DoubleToHalf(Scm_GetDouble(val));
        break;
    case SCM_UVECTOR_F32:
        SCM_F32VECTOR_ELEMENTS(v)[k] = (float)Scm_GetDouble(val);
        break;
    case SCM_UVECTOR_F64:
        SCM_F64VECTOR_ELEMENTS(v)[k] = Scm_GetDouble(val);
        break;
    case SCM_UVECTOR_C32:
        SCM_C32VECTOR_ELEMENTS(v)[k] = Scm_GetHalfComplex(val);
        break;
    case SCM_UVECTOR_C64:
        SCM_C64VECTOR_ELEMENTS(v)[k] = Scm_GetFloatComplex(val);
        break;
    case SCM_UVECTOR_C128:
        SCM_C128VECTOR_ELEMENTS(v)[k] = Scm_GetDoubleComplex(val);
        break;
    default:
        Scm_Error("[internal error] unknown uvector type given to Scm_VMUVectorRef");
    }
    return SCM_UNDEFINED;
}

/*
 * Inidividual constructors for convenience
 */
#define DEF_UVCTOR_FILL(tag, T) \
ScmObj SCM_CPP_CAT3(Scm_Make,tag,Vector)(ScmSmallInt size, T fill)      \
{                                                                       \
    ScmUVector *u =                                                     \
        (ScmUVector*)Scm_MakeUVector(SCM_CPP_CAT3(SCM_CLASS_,tag,VECTOR),\
                                     size, NULL);                       \
    T *elts = SCM_CPP_CAT3(SCM_,tag,VECTOR_ELEMENTS)(u);                \
    for (ScmSmallInt i=0; i<size; i++) *elts++ = fill;                  \
    return SCM_OBJ(u);                                                  \
}

#define DEF_UVCTOR_ARRAY(tag, T) \
ScmObj SCM_CPP_CAT3(Scm_Make,tag,VectorFromArray)(ScmSmallInt size,     \
                                                  const T array[])      \
{                                                                       \
    check_size(size, sizeof(T));                                        \
    T *z = SCM_NEW_ATOMIC_ARRAY(T, size);                               \
    memcpy(z, array, size*sizeof(T));                                   \
    return Scm_MakeUVector(SCM_CPP_CAT3(SCM_CLASS_,tag,VECTOR),         \
                           size, (void*)z);                             \
}                                                                       \
ScmObj SCM_CPP_CAT3(Scm_Make,tag,VectorFromArrayShared)(ScmSmallInt size,\
                                                        T array[])      \
{                                                                       \
    return Scm_MakeUVector(SCM_CPP_CAT3(SCM_CLASS_,tag,VECTOR),         \
                           size, (void*)array);                         \
}

/* NB: For u8vector and s8vector we can let memset() to fill the
   contents, expecting it's optimized. */
ScmObj Scm_MakeS8Vector(ScmSmallInt size, int8_t fill)
{
    ScmUVector *u =
        (ScmUVector*)Scm_MakeUVector(SCM_CLASS_S8VECTOR, size, NULL);
    (void)memset(SCM_S8VECTOR_ELEMENTS(u), fill, size);
    return SCM_OBJ(u);
}

ScmObj Scm_MakeU8Vector(ScmSmallInt size, uint8_t fill)
{
    ScmUVector *u =
        (ScmUVector*)Scm_MakeUVector(SCM_CLASS_U8VECTOR, size, NULL);
    (void)memset(SCM_U8VECTOR_ELEMENTS(u), fill, size);
    return SCM_OBJ(u);
}

DEF_UVCTOR_FILL(S16, int16_t)
DEF_UVCTOR_FILL(U16, uint16_t)
DEF_UVCTOR_FILL(S32, int32_t)
DEF_UVCTOR_FILL(U32, uint32_t)
DEF_UVCTOR_FILL(S64, int64_t)
DEF_UVCTOR_FILL(U64, uint64_t)
DEF_UVCTOR_FILL(F16, ScmHalfFloat)
DEF_UVCTOR_FILL(F32, float)
DEF_UVCTOR_FILL(F64, double)
DEF_UVCTOR_FILL(C32, ScmHalfComplex)
DEF_UVCTOR_FILL(C64, ScmFloatComplex)
DEF_UVCTOR_FILL(C128,ScmDoubleComplex)

DEF_UVCTOR_ARRAY(S8,  int8_t)
DEF_UVCTOR_ARRAY(U8,  uint8_t)
DEF_UVCTOR_ARRAY(S16, int16_t)
DEF_UVCTOR_ARRAY(U16, uint16_t)
DEF_UVCTOR_ARRAY(S32, int32_t)
DEF_UVCTOR_ARRAY(U32, uint32_t)
DEF_UVCTOR_ARRAY(S64, int64_t)
DEF_UVCTOR_ARRAY(U64, uint64_t)
DEF_UVCTOR_ARRAY(F16, ScmHalfFloat)
DEF_UVCTOR_ARRAY(F32, float)
DEF_UVCTOR_ARRAY(F64, double)
DEF_UVCTOR_ARRAY(C32, ScmHalfComplex)
DEF_UVCTOR_ARRAY(C64, ScmFloatComplex)
DEF_UVCTOR_ARRAY(C128,ScmDoubleComplex)

/*
 * Reader
 */
ScmObj Scm_ReadUVector(ScmPort *port, const char *tag, ScmReadContext *ctx)
{
    ScmChar c;
    SCM_GETC(c, port);
    if (c != '(') Scm_Error("bad uniform vector syntax for %s", tag);
    ScmObj list = Scm_ReadList(SCM_OBJ(port), ')');
    ScmClass *klass = NULL;
    if (strcmp(tag, "s8") == 0)        klass = SCM_CLASS_S8VECTOR;
    else if (strcmp(tag, "u8") == 0)   klass = SCM_CLASS_U8VECTOR;
    else if (strcmp(tag, "s16") == 0)  klass = SCM_CLASS_S16VECTOR;
    else if (strcmp(tag, "u16") == 0)  klass = SCM_CLASS_U16VECTOR;
    else if (strcmp(tag, "s32") == 0)  klass = SCM_CLASS_S32VECTOR;
    else if (strcmp(tag, "u32") == 0)  klass = SCM_CLASS_U32VECTOR;
    else if (strcmp(tag, "s64") == 0)  klass = SCM_CLASS_S64VECTOR;
    else if (strcmp(tag, "u64") == 0)  klass = SCM_CLASS_U64VECTOR;
    else if (strcmp(tag, "f16") == 0)  klass = SCM_CLASS_F16VECTOR;
    else if (strcmp(tag, "f32") == 0)  klass = SCM_CLASS_F32VECTOR;
    else if (strcmp(tag, "f64") == 0)  klass = SCM_CLASS_F64VECTOR;
    else if (strcmp(tag, "c32") == 0)  klass = SCM_CLASS_C32VECTOR;
    else if (strcmp(tag, "c64") == 0)  klass = SCM_CLASS_C64VECTOR;
    else if (strcmp(tag, "c128") == 0) klass = SCM_CLASS_C128VECTOR;
    else Scm_Error("invalid unform vector tag: %s", tag);

    ScmObj uv = Scm_ListToUVector(klass, list, 0);
    
    /* If we are reading source file, let literal uvectors be immutable. */
    if (Scm_ReadContextLiteralImmutable(ctx)) {
        SCM_UVECTOR_IMMUTABLE_SET(uv, TRUE);
    }
    return uv;
}

/*
 * Class-dependent functions
 */

/* printer */

#define DEF_PRINT(TAG, tag, T, pr)                                      \
static void SCM_CPP_CAT3(print_,tag,vector)(ScmObj obj,                 \
                                            ScmPort *out,               \
                                            ScmWriteContext *ctx)       \
{                                                                       \
    const ScmWriteControls *wp =                                        \
        Scm_GetWriteControls(ctx, Scm_PortWriteState(out));             \
    Scm_Printf(out, "#"#tag"(");                                        \
    for (int i=0; i<SCM_CPP_CAT3(SCM_,TAG,VECTOR_SIZE)(obj); i++) {     \
        T elt = SCM_CPP_CAT3(SCM_,TAG,VECTOR_ELEMENTS)(obj)[i];         \
        if (i != 0) Scm_Printf(out, " ");                               \
        if (wp->printLength >= 0 && i >= wp->printLength) {             \
            Scm_Printf(out, "...");                                     \
            break;                                                      \
        }                                                               \
        pr(out, elt);                                                   \
    }                                                                   \
    Scm_Printf(out, ")");                                               \
}

#define spr(out, elt) Scm_Printf(out, "%d", elt)
#define upr(out, elt) Scm_Printf(out, "%u", elt)
#define fpr(out, elt) Scm_PrintDouble(out, (double)elt, 0)
#define c32pr(out, elt)                                                 \
    do {                                                                \
        Scm_PrintDouble(out, Scm_HalfToDouble(SCM_HALF_COMPLEX_REAL(elt)), 0); \
        Scm_Putz("+", 1, out);                                          \
        Scm_PrintDouble(out, Scm_HalfToDouble(SCM_HALF_COMPLEX_IMAG(elt)), 0); \
        Scm_Putz("i", 1, out);                                          \
    } while (0)
#define c64pr(out, elt)                                 \
    do {                                                \
        Scm_PrintDouble(out, (double)crealf(elt), 0);   \
        Scm_Putz("+", 1, out);                          \
        Scm_PrintDouble(out, (double)cimagf(elt), 0);   \
        Scm_Putz("i", 1, out);                          \
    } while (0)
#define c128pr(out, elt)                        \
    do {                                        \
        Scm_PrintDouble(out, creal(elt), 0);    \
        Scm_Putz("+", 1, out);                  \
        Scm_PrintDouble(out, cimag(elt), 0);    \
        Scm_Putz("i", 1, out);                  \
    } while (0)
    

static inline void s64pr(ScmPort *out, int64_t elt)
{
#if SIZEOF_LONG == 4
    char buf[50];
    snprintf(buf, 50, "%lld", elt);
    Scm_Printf(out, "%s", buf);
#else
    Scm_Printf(out, "%ld", elt);
#endif
}

static inline void u64pr(ScmPort *out, uint64_t elt)
{
#if SIZEOF_LONG == 4
    char buf[50];
    snprintf(buf, 50, "%llu", elt);
    Scm_Printf(out, "%s", buf);
#else
    Scm_Printf(out, "%lu", elt);
#endif
}

static inline void f16pr(ScmPort *out, ScmHalfFloat elt)
{
    Scm_PrintDouble(out, Scm_HalfToDouble(elt), 0);
}

DEF_PRINT(S8, s8,   int8_t, spr)
DEF_PRINT(U8, u8,   uint8_t, upr)
DEF_PRINT(S16, s16, int16_t, spr)
DEF_PRINT(U16, u16, uint16_t, upr)
DEF_PRINT(S32, s32, int32_t, spr)
DEF_PRINT(U32, u32, uint32_t, upr)
DEF_PRINT(S64, s64, int64_t, s64pr)
DEF_PRINT(U64, u64, uint64_t, u64pr)
DEF_PRINT(F16, f16, ScmHalfFloat, f16pr)
DEF_PRINT(F32, f32, float, fpr)
DEF_PRINT(F64, f64, double, fpr)
DEF_PRINT(C32, c32, ScmHalfComplex, c32pr)
DEF_PRINT(C64, c64, ScmFloatComplex, c64pr)
DEF_PRINT(C128, c128, ScmDoubleComplex, c128pr)


/* comparer */

#define DEF_CMP(TAG, tag, T, eq, lt)                                    \
static int SCM_CPP_CAT3(compare_,tag,vector)(ScmObj x, ScmObj y, int equalp) \
{                                                                       \
    ScmSmallInt xlen = SCM_CPP_CAT3(SCM_,TAG,VECTOR_SIZE)(x);           \
    ScmSmallInt ylen = SCM_CPP_CAT3(SCM_,TAG,VECTOR_SIZE)(y);           \
    if (equalp) {                                                       \
        if (xlen != ylen) return -1;                                    \
        for (ScmSmallInt i=0; i<xlen; i++) {                            \
            T xx = SCM_CPP_CAT3(SCM_,TAG,VECTOR_ELEMENTS)(x)[i];        \
            T yy = SCM_CPP_CAT3(SCM_,TAG,VECTOR_ELEMENTS)(y)[i];        \
            if (!eq(xx,yy)) return -1;                                  \
        }                                                               \
        return 0;                                                       \
    } else {                                                            \
        if (xlen != ylen) return (xlen < ylen) ? -1 : 1;                \
        for (ScmSmallInt i=0; i<xlen; i++) {                            \
            T xx = SCM_CPP_CAT3(SCM_,TAG,VECTOR_ELEMENTS)(x)[i];        \
            T yy = SCM_CPP_CAT3(SCM_,TAG,VECTOR_ELEMENTS)(y)[i];        \
            if (lt(xx, yy)) return -1;                                  \
            if (!eq(xx,yy)) return 1;                                   \
        }                                                               \
        return 0;                                                       \
    }                                                                   \
}

#define common_eqv(x, y)  ((x)==(y))
#define common_lt(x, y)   ((x)<(y))

#define f16eqv(a, b) SCM_HALF_FLOAT_CMP(==, a, b)
#define f16lt(a, b)  SCM_HALF_FLOAT_CMP(<, a, b)

static inline int c32eqv(ScmHalfComplex x, ScmHalfComplex y)
{
    return (SCM_HALF_FLOAT_CMP(==, x.r, y.r)
            && SCM_HALF_FLOAT_CMP(==, x.i, y.i));
}

static inline int c32lt(ScmHalfComplex x, ScmHalfComplex y)
{
    return (SCM_HALF_FLOAT_CMP(<, x.r, y.r)
            || (SCM_HALF_FLOAT_CMP(==, x.r, y.r)
                && SCM_HALF_FLOAT_CMP(<, x.i, y.i)));
}

static inline int c64lt(ScmFloatComplex x, ScmFloatComplex y)
{
    return (crealf(x) < crealf(y)
            || (crealf(x) == crealf(y)
                && cimagf(x) < cimagf(y)));
}

static inline int c128lt(ScmDoubleComplex x, ScmDoubleComplex y)
{
    return (creal(x) < creal(y)
            || (creal(x) == creal(y)
                && cimag(x) < cimag(y)));
}


DEF_CMP(S8, s8,   int8_t, common_eqv, common_lt)
DEF_CMP(U8, u8,   uint8_t, common_eqv, common_lt)
DEF_CMP(S16, s16, int16_t, common_eqv, common_lt)
DEF_CMP(U16, u16, uint16_t, common_eqv, common_lt)
DEF_CMP(S32, s32, int32_t, common_eqv, common_lt)
DEF_CMP(U32, u32, uint32_t, common_eqv, common_lt)
DEF_CMP(S64, s64, int64_t, common_eqv, common_lt)
DEF_CMP(U64, u64, uint64_t, common_eqv, common_lt)
DEF_CMP(F16, f16, ScmHalfFloat, f16eqv, f16lt)
DEF_CMP(F32, f32, float, common_eqv, common_lt)
DEF_CMP(F64, f64, double, common_eqv, common_lt)
DEF_CMP(C32, c32, ScmHalfComplex, c32eqv, c32lt)
DEF_CMP(C64, c64, ScmFloatComplex, common_eqv, c64lt)
DEF_CMP(C128, c128, ScmDoubleComplex, common_eqv, c128lt)

/* 
 * Extract mmapped region
 */

/* Returns a uvector of KLASS with length LEN, that provides a view
   to the memory region MEM starting from OFFSET.
   LEN can be negative, in that case byte OFFSET to the end of the memory region
   is viewed.  If MEM is mapped read-only, the resulting uvector becomes
   immutable.  Otherwise, the resulting vector's immutablity is
   defined by IMMUTABLE flag.
 */
ScmObj Scm_MakeViewUVector(ScmMemoryRegion *mem, ScmClass *klass,
                           ScmSmallInt len, ScmSmallInt offset,
                           int immutable)
{
#if defined(HAVE_SYS_MMAN_H)
    if (offset < 0) Scm_Error("offset must be positive, but got %ld", offset);
    int esize = Scm_UVectorElementSize(klass);
    if (esize < 0) Scm_Error("uvector class required, but got: %S", klass);
    if ((offset % esize) != 0) {
        Scm_Error("offset %ld is not properly aligned for %S", offset, klass);
    }
    if (len < 0) len = (mem->size - offset)/esize;

    if (!(mem->prot & PROT_WRITE)) immutable = TRUE;
    return Scm_MakeUVectorFull(klass, len, mem->ptr + offset, immutable,
                               (void*)mem);
#else /*!defined(HAVE_SYS_MMAN_H)*/
    Scm_Error("make-view-uvector isn't supported on this platform.");
    return SCM_UNDEFINED;       /* dummy */
#endif
}

/*=====================================================================
 * Bitvectors
 */

static void bitvector_write_int(ScmBitvector *v, int prefix, ScmPort *port)
{
    if (prefix) Scm_Putz("#*", -1, port);
    for (int i=0; i<SCM_BITVECTOR_SIZE(v); i++) {
        if (SCM_BITS_TEST(v->bits, i)) SCM_PUTC('1', port);
        else SCM_PUTC('0', port);
    }
}

static void bitvector_print(ScmObj obj, 
                            ScmPort *port, 
                            ScmWriteContext *ctx SCM_UNUSED)
{
    bitvector_write_int(SCM_BITVECTOR(obj), TRUE, port);
}

static int bitvector_compare(ScmObj x, ScmObj y, int equalp SCM_UNUSED)
{
    SCM_ASSERT(SCM_BITVECTORP(x)&&SCM_BITVECTORP(y));
    ScmBits *bx = SCM_BITVECTOR_BITS(x);
    ScmBits *by = SCM_BITVECTOR_BITS(y);
    ScmWord xlen = SCM_BITVECTOR_SIZE(x);
    ScmWord ylen = SCM_BITVECTOR_SIZE(y);

    /* NB: The ordering is somewhat counterintuitive, for the bits are
       stored in little-endian.  That is, #*0100 comes after #*1000.
       This is a lot faster.  Srfi-178 doesn't define the ordering. */
    if (xlen < ylen) return -1;
    if (xlen > ylen) return 1;
    size_t nw = SCM_BITS_NUM_WORDS(xlen);
    for (size_t i = 0; i < nw; i++, bx++, by++) {
        if (*bx < *by) return -1;
        if (*bx > *by) return 1;
    }
    return 0;
}

SCM_DEFINE_BUILTIN_CLASS_FLAGS(Scm_BitvectorClass,
                               bitvector_print, bitvector_compare,
                               NULL, NULL, SCM_CLASS_SEQUENCE_CPL,
                               SCM_CLASS_AGGREGATE);


int Scm_Bit2Int(ScmObj bit)
{
    if (SCM_EQ(bit, SCM_TRUE) || SCM_EQ(bit, SCM_MAKE_INT(1))) return 1;
    if (SCM_FALSEP(bit) || SCM_EQ(bit, SCM_MAKE_INT(0))) return 0;
    Scm_Error("bit value must be 0, 1, #f or #t, but got: %S", bit);
    return 0;                   /* dummy */
}

ScmObj Scm_Bit2Bool(ScmObj bit)
{
    if (SCM_EQ(bit, SCM_TRUE) || SCM_EQ(bit, SCM_MAKE_INT(1))) return SCM_TRUE;
    if (SCM_FALSEP(bit) || SCM_EQ(bit, SCM_MAKE_INT(0))) return SCM_FALSE;
    Scm_Error("bit value must be 0, 1, #f or #t, but got: %S", bit);
    return SCM_UNDEFINED;       /* dummy */
}

/* init can be 0, 1, #f or #t. */
ScmObj Scm_MakeBitvector(ScmSmallInt size, ScmObj init)
{
    if (size < 0) {
        Scm_Error("bitvector size must be a positive integer, but got %d", size);
    }
    ScmBitvector *v = SCM_NEW(ScmBitvector);
    SCM_SET_CLASS(v, SCM_CLASS_BITVECTOR);
    v->size_flags = (size << 1);
    v->bits = Scm_MakeBits(size);

    int fill = Scm_Bit2Int(init);
    Scm_BitsFill(v->bits, 0, size, fill);
    return SCM_OBJ(v);
}

ScmObj Scm_ListToBitvector(ScmObj lis) 
{
    ScmSmallInt len = Scm_Length(lis);
    if (len < 0) {
        Scm_Error("proper list required, but got: %S", lis);
    }
    
    ScmBitvector *v = SCM_BITVECTOR(Scm_MakeBitvector(len, SCM_FALSE));
    ScmObj cp;
    ScmSmallInt i = 0;
    SCM_FOR_EACH(cp, lis) {
        if (Scm_Bit2Int(SCM_CAR(cp))) SCM_BITS_SET(v->bits, i);
        else                          SCM_BITS_RESET(v->bits, i);
        i++;
    }
    return SCM_OBJ(v);
}

/* Parse string of 0's and 1's to a bitvector.  If PREFIX is true,      
   "#*" prefix is assumed.  Returns #f if unparsable. */

ScmObj Scm_StringToBitvector(ScmString *s, int prefix)
{
    const ScmStringBody *b = SCM_STRING_BODY(s);
    const char *p = SCM_STRING_BODY_START(b);
    /* multibyte string can't be a bitvector literal. */
    if (SCM_STRING_BODY_SIZE(b) != SCM_STRING_BODY_LENGTH(b)) return SCM_FALSE;
    ScmSmallInt len = SCM_STRING_BODY_LENGTH(b);
    
    if (prefix) {
        if (len < 2) return SCM_FALSE;
        if (strncmp(p, "#*", 2) != 0) return SCM_FALSE;
        p += 2;
        len -= 2;
    }

    ScmBitvector *v = SCM_BITVECTOR(Scm_MakeBitvector(len, SCM_FALSE));
    int i = 0;
    for (; i < len; p++, i++) {
        if (*p == '0')      SCM_BITS_RESET(v->bits, i);
        else if (*p == '1') SCM_BITS_SET(v->bits, i);
        else return SCM_FALSE;
    }
    return SCM_OBJ(v);
}

ScmObj Scm_BitvectorToString(ScmBitvector *v, int prefix)
{
    ScmObj out = Scm_MakeOutputStringPort(TRUE);
    bitvector_write_int(v, prefix, SCM_PORT(out));
    return Scm_GetOutputString(SCM_PORT(out), 0);
}

ScmObj Scm_BitvectorCopy(ScmBitvector *v, ScmSmallInt start, ScmSmallInt end)
{
    ScmSmallInt size = SCM_BITVECTOR_SIZE(v);
    SCM_CHECK_START_END(start, end, size);
    ScmBitvector *vv = SCM_BITVECTOR(Scm_MakeBitvector(end-start, SCM_FALSE));
    Scm_BitsCopyX(vv->bits, 0, v->bits, start, end);
    return SCM_OBJ(vv);
}

ScmObj Scm_BitvectorCopyX(ScmBitvector *dest, ScmSmallInt dstart,
                          ScmBitvector *src,
                          ScmSmallInt sstart, ScmSmallInt send)
{
    SCM_BITVECTOR_CHECK_MUTABLE(dest);
    ScmSmallInt ssize = SCM_BITVECTOR_SIZE(src);
    SCM_CHECK_START_END(sstart, send, ssize);
    ScmSmallInt dsize = SCM_BITVECTOR_SIZE(dest);
    ScmSmallInt dend = dstart + send - sstart;
    if (dstart > dsize || dstart < 0 || dend > dsize) {
        Scm_Error("destination index out of range (size=%ld, start=%ld, end=%ld)",
                  dsize, dstart, dend);
    }

    Scm_BitsCopyX(dest->bits, dstart, src->bits, sstart, send);
    return SCM_OBJ(dest);
}


/*=====================================================================
 * Utility
 */

const uint8_t *Scm_GetBytes(ScmObj obj, ScmSize *size)
{
    if (SCM_UVECTORP(obj)) {
        *size = Scm_UVectorSizeInBytes(SCM_UVECTOR(obj));
        return (const uint8_t*)SCM_UVECTOR_ELEMENTS(obj);
    } else if (SCM_STRINGP(obj)) {
        ScmSmallInt s;
        const char *z = Scm_GetStringContent(SCM_STRING(obj), &s, 0, 0);
        *size = s;
        return (const uint8_t*)z;
    } else {
        *size = 0;
        return 0;
    }
}
