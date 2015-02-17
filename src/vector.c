/*
 * vector.c - vector implementation
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


SCM_DEFINE_BUILTIN_CLASS(Scm_VectorClass, vector_print, vector_compare,
                         NULL, NULL, SCM_CLASS_SEQUENCE_CPL);

static ScmVector *make_vector(ScmSmallInt size)
{
    ScmVector *v = SCM_NEW2(ScmVector *,
                            sizeof(ScmVector) + sizeof(ScmObj)*(size-1));
    SCM_SET_CLASS(v, SCM_CLASS_VECTOR);
    v->size = size;
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
    if (i < 0 || i >= vec->size) return fallback;
    return vec->elements[i];
}

ScmObj Scm_VectorSet(ScmVector *vec, ScmSmallInt i, ScmObj obj)
{
    if (i >= 0 && i < vec->size) vec->elements[i] = obj;
    return obj;
}

ScmObj Scm_VectorFill(ScmVector *vec, ScmObj fill,
                      ScmSmallInt start, ScmSmallInt end)
{
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
        Scm_Error("vector-copy: start (%d) is greater than end (%d)",
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
SCM_DEFINE_BUILTIN_CLASS(SCM_CPP_CAT3(Scm_,TAG,VectorClass),            \
                         SCM_CPP_CAT3(print_,tag,vector),               \
                         SCM_CPP_CAT3(compare_,tag,vector),             \
                         NULL, NULL, uvector_cpl);

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

/*
 * Some generic APIs
 */
ScmUVectorType Scm_UVectorType(ScmClass *klass)
{
    if (SCM_EQ(klass, SCM_CLASS_S8VECTOR))  return SCM_UVECTOR_S8;
    if (SCM_EQ(klass, SCM_CLASS_U8VECTOR))  return SCM_UVECTOR_U8;
    if (SCM_EQ(klass, SCM_CLASS_S16VECTOR)) return SCM_UVECTOR_S16;
    if (SCM_EQ(klass, SCM_CLASS_U16VECTOR)) return SCM_UVECTOR_U16;
    if (SCM_EQ(klass, SCM_CLASS_S32VECTOR)) return SCM_UVECTOR_S32;
    if (SCM_EQ(klass, SCM_CLASS_U32VECTOR)) return SCM_UVECTOR_U32;
    if (SCM_EQ(klass, SCM_CLASS_S64VECTOR)) return SCM_UVECTOR_S64;
    if (SCM_EQ(klass, SCM_CLASS_U64VECTOR)) return SCM_UVECTOR_U64;
    if (SCM_EQ(klass, SCM_CLASS_F16VECTOR)) return SCM_UVECTOR_F16;
    if (SCM_EQ(klass, SCM_CLASS_F32VECTOR)) return SCM_UVECTOR_F32;
    if (SCM_EQ(klass, SCM_CLASS_F64VECTOR)) return SCM_UVECTOR_F64;
    else return SCM_UVECTOR_INVALID;
}

const char *Scm_UVectorTypeName(int type) /* for error msgs etc. */
{
    switch (type) {
    case SCM_UVECTOR_S8:  return "s8vector";
    case SCM_UVECTOR_U8:  return "u8vector";
    case SCM_UVECTOR_S16: return "s16vector";
    case SCM_UVECTOR_U16: return "u16vector";
    case SCM_UVECTOR_S32: return "s32vector";
    case SCM_UVECTOR_U32: return "u32vector";
    case SCM_UVECTOR_S64: return "s64vector";
    case SCM_UVECTOR_U64: return "u64vector";
    case SCM_UVECTOR_F16: return "f16vector";
    case SCM_UVECTOR_F32: return "f32vector";
    case SCM_UVECTOR_F64: return "f64vector";
    default: return "invalid type of uvector (possibly implementation error)";
    }
}

/* Returns the size of element of the uvector of given class */
int Scm_UVectorElementSize(ScmClass *klass)
{
    static const int sizes[] = { 1, 1, 2, 2, 4, 4, 8, 8,
                                 2, sizeof(float), sizeof(double) };
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
        vec->elements = SCM_NEW_ATOMIC2(void*, size*eltsize);
    }
#if GAUCHE_API_0_95
    vec->size_flags = (size << 1)|(immutable?1:0);
#else  /*!GAUCHE_API_0_95*/
    vec->size = size;
    vec->immutable = immutable;
#endif /*!GAUCHE_API_0_95*/
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
    ScmSmallInt length = Scm_Length(list);
    if (length < 0) Scm_Error("improper list not allowed: %S", list);

    ScmUVector *v = (ScmUVector*)Scm_MakeUVector(klass, length, NULL);
    ScmObj cp = list;
    for (int i=0; i<length; i++, cp = SCM_CDR(cp)) {
        switch (type) {
        case SCM_UVECTOR_S8:
            SCM_S8VECTOR_ELEMENTS(v)[i] =
                (signed char)Scm_GetInteger8Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_U8:
            SCM_U8VECTOR_ELEMENTS(v)[i] =
                (unsigned char)Scm_GetIntegerU8Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_S16:
            SCM_S16VECTOR_ELEMENTS(v)[i] =
                (short)Scm_GetInteger16Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_U16:
            SCM_U16VECTOR_ELEMENTS(v)[i] =
                (u_short)Scm_GetIntegerU16Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_S32:
            SCM_S32VECTOR_ELEMENTS(v)[i] =
                (ScmInt32)Scm_GetInteger32Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_U32:
            SCM_U32VECTOR_ELEMENTS(v)[i] =
                (ScmUInt32)Scm_GetIntegerU32Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_S64:
            SCM_S64VECTOR_ELEMENTS(v)[i] =
                (ScmInt64)Scm_GetInteger64Clamp(SCM_CAR(cp), clamp, NULL);
            break;
        case SCM_UVECTOR_U64:
            SCM_U64VECTOR_ELEMENTS(v)[i] =
                (ScmUInt64)Scm_GetIntegerU64Clamp(SCM_CAR(cp), clamp, NULL);
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
            SCM_F64VECTOR_ELEMENTS(v)[i] =
                Scm_GetDouble(SCM_CAR(cp));
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
    default:
        Scm_Error("[internal error] unknown uvector type given to Scm_VMUVectorRef");
        return SCM_UNDEFINED;   /* dummy */
    }
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
ScmObj Scm_MakeS8Vector(ScmSmallInt size, signed char fill)
{
    ScmUVector *u =
        (ScmUVector*)Scm_MakeUVector(SCM_CLASS_S8VECTOR, size, NULL);
    (void)memset(SCM_S8VECTOR_ELEMENTS(u), fill, size);
    return SCM_OBJ(u);
}

ScmObj Scm_MakeU8Vector(ScmSmallInt size, unsigned char fill)
{
    ScmUVector *u =
        (ScmUVector*)Scm_MakeUVector(SCM_CLASS_U8VECTOR, size, NULL);
    (void)memset(SCM_U8VECTOR_ELEMENTS(u), fill, size);
    return SCM_OBJ(u);
}

DEF_UVCTOR_FILL(S16, short)
DEF_UVCTOR_FILL(U16, u_short)
DEF_UVCTOR_FILL(S32, ScmInt32)
DEF_UVCTOR_FILL(U32, ScmUInt32)
DEF_UVCTOR_FILL(S64, ScmInt64)
DEF_UVCTOR_FILL(U64, ScmUInt64)
DEF_UVCTOR_FILL(F16, ScmHalfFloat)
DEF_UVCTOR_FILL(F32, float)
DEF_UVCTOR_FILL(F64, double)

DEF_UVCTOR_ARRAY(S8, signed char)
DEF_UVCTOR_ARRAY(U8, unsigned char)
DEF_UVCTOR_ARRAY(S16, short)
DEF_UVCTOR_ARRAY(U16, u_short)
DEF_UVCTOR_ARRAY(S32, ScmInt32)
DEF_UVCTOR_ARRAY(U32, ScmUInt32)
DEF_UVCTOR_ARRAY(S64, ScmInt64)
DEF_UVCTOR_ARRAY(U64, ScmUInt64)
DEF_UVCTOR_ARRAY(F16, ScmHalfFloat)
DEF_UVCTOR_ARRAY(F32, float)
DEF_UVCTOR_ARRAY(F64, double)

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
    if (strcmp(tag, "s8") == 0)       klass = SCM_CLASS_S8VECTOR;
    else if (strcmp(tag, "u8") == 0)  klass = SCM_CLASS_U8VECTOR;
    else if (strcmp(tag, "s16") == 0) klass = SCM_CLASS_S16VECTOR;
    else if (strcmp(tag, "u16") == 0) klass = SCM_CLASS_U16VECTOR;
    else if (strcmp(tag, "s32") == 0) klass = SCM_CLASS_S32VECTOR;
    else if (strcmp(tag, "u32") == 0) klass = SCM_CLASS_U32VECTOR;
    else if (strcmp(tag, "s64") == 0) klass = SCM_CLASS_S64VECTOR;
    else if (strcmp(tag, "u64") == 0) klass = SCM_CLASS_U64VECTOR;
    else if (strcmp(tag, "f16") == 0) klass = SCM_CLASS_F16VECTOR;
    else if (strcmp(tag, "f32") == 0) klass = SCM_CLASS_F32VECTOR;
    else if (strcmp(tag, "f64") == 0) klass = SCM_CLASS_F64VECTOR;
    else Scm_Error("invalid unform vector tag: %s", tag);

    ScmObj uv = Scm_ListToUVector(klass, list, 0);
    
    /* If we are reading source file, let literal uvectors be immutable. */
    if (Scm_ReadContextLiteralImmutable(ctx)) {
        SCM_UVECTOR_IMMUTABLE_P(uv) = TRUE;
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
    Scm_Printf(out, "#"#tag"(");                                        \
    for (int i=0; i<SCM_CPP_CAT3(SCM_,TAG,VECTOR_SIZE)(obj); i++) {     \
        T elt = SCM_CPP_CAT3(SCM_,TAG,VECTOR_ELEMENTS)(obj)[i];         \
        if (i != 0) Scm_Printf(out, " ");                               \
        pr(out, elt);                                                   \
    }                                                                   \
    Scm_Printf(out, ")");                                               \
}

#define spr(out, elt) Scm_Printf(out, "%d", elt)
#define upr(out, elt) Scm_Printf(out, "%u", elt)
#define fpr(out, elt) Scm_PrintDouble(out, (double)elt, 0)

static inline void s64pr(ScmPort *out, ScmInt64 elt)
{
#if SCM_EMULATE_INT64
    Scm_Printf(out, "%S", Scm_MakeInteger64(elt));
#elif SIZEOF_LONG == 4
    char buf[50];
    snprintf(buf, 50, "%lld", elt);
    Scm_Printf(out, "%s", buf);
#else
    Scm_Printf(out, "%ld", elt);
#endif
}

static inline void u64pr(ScmPort *out, ScmUInt64 elt)
{
#if SCM_EMULATE_INT64
    Scm_Printf(out, "%S", Scm_MakeIntegerU64(elt));
#elif SIZEOF_LONG == 4
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

DEF_PRINT(S8, s8, signed char, spr)
DEF_PRINT(U8, u8, unsigned char, upr)
DEF_PRINT(S16, s16, short, spr)
DEF_PRINT(U16, u16, u_short, upr)
DEF_PRINT(S32, s32, ScmInt32, spr)
DEF_PRINT(U32, u32, ScmUInt32, upr)
DEF_PRINT(S64, s64, ScmInt64, s64pr)
DEF_PRINT(U64, u64, ScmUInt64, u64pr)
DEF_PRINT(F16, f16, ScmHalfFloat, f16pr)
DEF_PRINT(F32, f32, float, fpr)
DEF_PRINT(F64, f64, double, fpr)


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

static inline int int64eqv(ScmInt64 x, ScmInt64 y)
{
#if SCM_EMULATE_INT64
    return (x.hi == y.hi && x.lo == y.lo);
#else
    return x == y;
#endif
}

static inline int uint64eqv(ScmUInt64 x, ScmUInt64 y)
{
#if SCM_EMULATE_INT64
    return (x.hi == y.hi && x.lo == y.lo);
#else
    return x == y;
#endif
}

static inline int int64lt(ScmInt64 x, ScmInt64 y)
{
#if SCM_EMULATE_INT64
    return (x.hi < y.hi || (x.hi == y.hi && x.lo < y.lo));
#else
    return x < y;
#endif
}

static inline int uint64lt(ScmUInt64 x, ScmUInt64 y)
{
#if SCM_EMULATE_INT64
    return (x.hi < y.hi || (x.hi == y.hi && x.lo < y.lo));
#else
    return x < y;
#endif
}

#define f16eqv(a, b) SCM_HALF_FLOAT_CMP(==, a, b)
#define f16lt(a, b)  SCM_HALF_FLOAT_CMP(<, a, b)

DEF_CMP(S8, s8, signed char, common_eqv, common_lt)
DEF_CMP(U8, u8, unsigned char, common_eqv, common_lt)
DEF_CMP(S16, s16, short, common_eqv, common_lt)
DEF_CMP(U16, u16, u_short, common_eqv, common_lt)
DEF_CMP(S32, s32, ScmInt32, common_eqv, common_lt)
DEF_CMP(U32, u32, ScmUInt32, common_eqv, common_lt)
DEF_CMP(S64, s64, ScmInt64, int64eqv, int64lt)
DEF_CMP(U64, u64, ScmUInt64, uint64eqv, uint64lt)
DEF_CMP(F16, f16, ScmHalfFloat, f16eqv, f16lt)
DEF_CMP(F32, f32, float, common_eqv, common_lt)
DEF_CMP(F64, f64, double, common_eqv, common_lt)
