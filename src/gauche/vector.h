/*
 * gauche/vector.h - Vector API
 *
 *   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_VECTOR_H
#define GAUCHE_VECTOR_H

/*
 * General vector
 */

struct ScmVectorRec {
    SCM_HEADER;
#if GAUCHE_API_VERSION >= 98
    ScmWord size_flags;
#else  /* GAUCHE_API_VERSION < 98 */
    ScmWord size;
#endif /* GAUCHE_API_VERSION < 98 */
    ScmObj elements[1];
};

SCM_CLASS_DECL(Scm_VectorClass);
#define SCM_CLASS_VECTOR     (&Scm_VectorClass)
#define SCM_VECTOR(obj)          ((ScmVector*)(obj))
#define SCM_VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_VECTOR)
#define SCM_VECTOR_ELEMENTS(obj) (SCM_VECTOR(obj)->elements)
#define SCM_VECTOR_ELEMENT(obj, i)   (SCM_VECTOR(obj)->elements[i])

/* SCM_VECTOR_SIZE_SLOT_INITIALIZER is used in cgen-generated code  */

#if GAUCHE_API_VERSION >= 98
#define SCM_VECTOR_SIZE(obj)        (SCM_VECTOR(obj)->size_flags >> 1)
#define SCM_VECTOR_IMMUTABLE_P(obj) (SCM_VECTOR(obj)->size_flags & 1)
#define SCM_VECTOR_IMMUTABLE_SET(obj, flag)     \
    ((flag)                                     \
     ? (SCM_UVECTOR(obj)->size_flags |= 1)      \
     : (SCM_UVECTOR(obj)->size_flags &= ~1))
#define SCM_VECTOR_SIZE_SLOT_INITIALIZER(len, imm) \
    SCM_OBJ(SCM_WORD(((len)<<1)|((imm)?1:0)))
#else  /* GAUCHE_API_VERSION < 98 */
#define SCM_VECTOR_SIZE(obj)        (SCM_VECTOR(obj)->size)
#define SCM_VECTOR_IMMUTABLE_P(obj) (!SCM_VECTORP(obj)) /* always FALSE, but need to use obj to avoid unused variable warning */
#define SCM_VECTOR_IMMUTABLE_SET(obj, flag)  /*empty*/
#define SCM_VECTOR_SIZE_SLOT_INITIALIZER(len, imm)  SCM_OBJ(len)
#endif /* GAUCHE_API_VERSION < 98 */

#define SCM_VECTOR_CHECK_MUTABLE(obj)           \
  do { if (SCM_VECTOR_IMMUTABLE_P(obj)) {       \
    Scm_Error("vector is immutable: %S", obj);  \
  }} while (0)

SCM_EXTERN ScmObj Scm_MakeVector(ScmSmallInt size, ScmObj fill);
SCM_EXTERN ScmObj Scm_VectorRef(ScmVector *vec, ScmSmallInt i, ScmObj fallback);
SCM_EXTERN ScmObj Scm_VectorSet(ScmVector *vec, ScmSmallInt i, ScmObj obj);
SCM_EXTERN ScmObj Scm_VectorFill(ScmVector *vec, ScmObj fill,
                                 ScmSmallInt start, ScmSmallInt end);

SCM_EXTERN ScmObj Scm_ListToVector(ScmObj l,
                                   ScmSmallInt start, ScmSmallInt end);
SCM_EXTERN ScmObj Scm_VectorToList(ScmVector *v,
                                   ScmSmallInt start, ScmSmallInt end);
SCM_EXTERN ScmObj Scm_VectorCopy(ScmVector *vec,
                                 ScmSmallInt start, ScmSmallInt end,
                                 ScmObj fill);

/*
 * Uniform vectors
 * NB: The Gauche core only includes basic uniform vector APIs, e.g.
 * constructors, accessors, and srfi-4 literal reader/writer syntax.
 * All other useful APIs and Scheme bindings are provided in ext/uvector.
 */

/* Common uniform vector structure */

typedef struct ScmUVectorRec {
    SCM_HEADER;
    ScmWord size_flags;         /* (len<<1)|immutable */
    void *owner;
    void *elements;
} ScmUVector;

SCM_CLASS_DECL(Scm_UVectorClass);
#define SCM_CLASS_UVECTOR         (&Scm_UVectorClass)
#define SCM_UVECTOR(obj)          ((ScmUVector*)(obj))
#define SCM_UVECTORP(obj)         Scm_TypeP(obj, SCM_CLASS_UVECTOR)
#define SCM_UVECTOR_OWNER(obj)    (SCM_UVECTOR(obj)->owner)
#define SCM_UVECTOR_ELEMENTS(obj) (SCM_UVECTOR(obj)->elements)

#define SCM_UVECTOR_SIZE(obj)     (SCM_UVECTOR(obj)->size_flags >> 1)
#define SCM_UVECTOR_IMMUTABLE_P(obj) (SCM_UVECTOR(obj)->size_flags & 1)
#define SCM_UVECTOR_IMMUTABLE_SET(obj, flag)    \
    ((flag)                                     \
     ? (SCM_UVECTOR(obj)->size_flags |= 1)      \
     : (SCM_UVECTOR(obj)->size_flags &= ~1))
#define SCM_UVECTOR_INITIALIZER(klass, size, elements, immutable, owner) \
    { { SCM_CLASS_STATIC_TAG(klass) }, (((size)<<1)|(immutable?1:0)),    \
      (owner), (elements) }

#define SCM_UVECTOR_CHECK_MUTABLE(obj)                 \
  do { if (SCM_UVECTOR_IMMUTABLE_P(obj)) {             \
    Scm_Error("uniform vector is immutable: %S", obj); \
  }} while (0)

/* A convenient enum to dispatch by specific uvector subclasses
   within a generic uvector API.
   NB: The value of those enums can be embedded in precompiled files,
   and also used in Scm_Compare to order between different uvectors.
   So the order shouldn't be changed.
*/
typedef enum {
    SCM_UVECTOR_S8,
    SCM_UVECTOR_U8,
    SCM_UVECTOR_S16,
    SCM_UVECTOR_U16,
    SCM_UVECTOR_S32,
    SCM_UVECTOR_U32,
    SCM_UVECTOR_S64,
    SCM_UVECTOR_U64,
    SCM_UVECTOR_F16,
    SCM_UVECTOR_F32,
    SCM_UVECTOR_F64,
    SCM_UVECTOR_RESERVED1,      /* reserved for f128*/
    SCM_UVECTOR_C32,
    SCM_UVECTOR_C64,
    SCM_UVECTOR_C128,
    SCM_UVECTOR_RESERVED2,      /* reserved for c256 */
    SCM_UVECTOR_INVALID = -1
} ScmUVectorType;

#define SCM_UVECTOR_SUBTYPE_P(obj, type) \
    (SCM_UVECTORP(obj)&&Scm_UVectorType(SCM_CLASS_OF(obj))==(type))

SCM_EXTERN ScmUVectorType Scm_UVectorType(ScmClass *klass);
SCM_EXTERN const char *Scm_UVectorTypeName(int type);
SCM_EXTERN int    Scm_UVectorElementSize(ScmClass *klass);
SCM_EXTERN int    Scm_UVectorSizeInBytes(ScmUVector *v);
SCM_EXTERN ScmObj Scm_MakeUVector(ScmClass *klass,
                                  ScmSmallInt size, void *init);
SCM_EXTERN ScmObj Scm_MakeUVectorFull(ScmClass *klass,
                                      ScmSmallInt size, void *init,
                                      int immutablep, void *owner);
SCM_EXTERN ScmObj Scm_ListToUVector(ScmClass *klass, ScmObj list, int clamp);
SCM_EXTERN ScmObj Scm_VMUVectorRef(ScmUVector *v, int t,
                                   ScmSmallInt k, ScmObj fallback);
SCM_EXTERN ScmObj Scm_UVectorSet(ScmUVector *v, int t,
                                 ScmSmallInt k, ScmObj val, int clamp);
SCM_EXTERN ScmObj Scm_ReadUVector(ScmPort *port, const char *tag,
                                  ScmReadContext *ctx);

/* Individual class definitions.
   Some of SCM_tagVECTOR* macros are redundant, for SCM_UVECTOR* macros
   is just enough.  We have them for the backward compatibility.
 */

SCM_CLASS_DECL(Scm_S8VectorClass);
#define SCM_CLASS_S8VECTOR         (&Scm_S8VectorClass)
#define SCM_S8VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_S8VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_S8VECTOR)
#define SCM_S8VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_S8VECTOR_ELEMENTS(obj) ((int8_t*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_S8VECTOR_ELEMENT(obj,k) SCM_S8VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeS8Vector(ScmSmallInt size, int8_t fill);
SCM_EXTERN ScmObj Scm_MakeS8VectorFromArray(ScmSmallInt size,
                                            const int8_t array[]);
SCM_EXTERN ScmObj Scm_MakeS8VectorFromArrayShared(ScmSmallInt size,
                                                  int8_t array[]);

SCM_CLASS_DECL(Scm_U8VectorClass);
#define SCM_CLASS_U8VECTOR         (&Scm_U8VectorClass)
#define SCM_U8VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_U8VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_U8VECTOR)
#define SCM_U8VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_U8VECTOR_ELEMENTS(obj) ((uint8_t*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_U8VECTOR_ELEMENT(obj,k) SCM_U8VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeU8Vector(ScmSmallInt size, uint8_t fill);
SCM_EXTERN ScmObj Scm_MakeU8VectorFromArray(ScmSmallInt size,
                                            const uint8_t array[]);
SCM_EXTERN ScmObj Scm_MakeU8VectorFromArrayShared(ScmSmallInt size,
                                                  uint8_t array[]);

SCM_CLASS_DECL(Scm_S16VectorClass);
#define SCM_CLASS_S16VECTOR         (&Scm_S16VectorClass)
#define SCM_S16VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_S16VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_S16VECTOR)
#define SCM_S16VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_S16VECTOR_ELEMENTS(obj) ((int16_t*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_S16VECTOR_ELEMENT(obj,k) SCM_S16VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeS16Vector(ScmSmallInt size, int16_t fill);
SCM_EXTERN ScmObj Scm_MakeS16VectorFromArray(ScmSmallInt size,
                                             const int16_t array[]);
SCM_EXTERN ScmObj Scm_MakeS16VectorFromArrayShared(ScmSmallInt size,
                                                   int16_t array[]);

SCM_CLASS_DECL(Scm_U16VectorClass);
#define SCM_CLASS_U16VECTOR         (&Scm_U16VectorClass)
#define SCM_U16VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_U16VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_U16VECTOR)
#define SCM_U16VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_U16VECTOR_ELEMENTS(obj) ((uint16_t*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_U16VECTOR_ELEMENT(obj,k) SCM_U16VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeU16Vector(ScmSmallInt size, uint16_t fill);
SCM_EXTERN ScmObj Scm_MakeU16VectorFromArray(ScmSmallInt size,
                                             const uint16_t array[]);
SCM_EXTERN ScmObj Scm_MakeU16VectorFromArrayShared(ScmSmallInt size,
                                                   uint16_t array[]);

SCM_CLASS_DECL(Scm_S32VectorClass);
#define SCM_CLASS_S32VECTOR         (&Scm_S32VectorClass)
#define SCM_S32VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_S32VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_S32VECTOR)
#define SCM_S32VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_S32VECTOR_ELEMENTS(obj) ((int32_t*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_S32VECTOR_ELEMENT(obj,k) SCM_S32VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeS32Vector(ScmSmallInt size, int32_t fill);
SCM_EXTERN ScmObj Scm_MakeS32VectorFromArray(ScmSmallInt size, const int32_t array[]);
SCM_EXTERN ScmObj Scm_MakeS32VectorFromArrayShared(ScmSmallInt size, int32_t array[]);

SCM_CLASS_DECL(Scm_U32VectorClass);
#define SCM_CLASS_U32VECTOR         (&Scm_U32VectorClass)
#define SCM_U32VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_U32VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_U32VECTOR)
#define SCM_U32VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_U32VECTOR_ELEMENTS(obj) ((uint32_t*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_U32VECTOR_ELEMENT(obj,k) SCM_U32VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeU32Vector(ScmSmallInt size, uint32_t fill);
SCM_EXTERN ScmObj Scm_MakeU32VectorFromArray(ScmSmallInt size,
                                             const uint32_t array[]);
SCM_EXTERN ScmObj Scm_MakeU32VectorFromArrayShared(ScmSmallInt size,
                                                   uint32_t array[]);

SCM_CLASS_DECL(Scm_S64VectorClass);
#define SCM_CLASS_S64VECTOR         (&Scm_S64VectorClass)
#define SCM_S64VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_S64VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_S64VECTOR)
#define SCM_S64VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_S64VECTOR_ELEMENTS(obj) ((int64_t*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_S64VECTOR_ELEMENT(obj,k) SCM_S64VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeS64Vector(ScmSmallInt size, int64_t fill);
SCM_EXTERN ScmObj Scm_MakeS64VectorFromArray(ScmSmallInt size,
                                             const int64_t array[]);
SCM_EXTERN ScmObj Scm_MakeS64VectorFromArrayShared(ScmSmallInt size,
                                                   int64_t array[]);

SCM_CLASS_DECL(Scm_U64VectorClass);
#define SCM_CLASS_U64VECTOR         (&Scm_U64VectorClass)
#define SCM_U64VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_U64VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_U64VECTOR)
#define SCM_U64VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_U64VECTOR_ELEMENTS(obj) ((uint64_t*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_U64VECTOR_ELEMENT(obj,k) SCM_U64VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeU64Vector(ScmSmallInt size, uint64_t fill);
SCM_EXTERN ScmObj Scm_MakeU64VectorFromArray(ScmSmallInt size,
                                             const uint64_t array[]);
SCM_EXTERN ScmObj Scm_MakeU64VectorFromArrayShared(ScmSmallInt size,
                                                   uint64_t array[]);

SCM_CLASS_DECL(Scm_F16VectorClass);
#define SCM_CLASS_F16VECTOR         (&Scm_F16VectorClass)
#define SCM_F16VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_F16VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_F16VECTOR)
#define SCM_F16VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_F16VECTOR_ELEMENTS(obj) ((ScmHalfFloat*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_F16VECTOR_ELEMENT(obj,k) SCM_F16VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeF16Vector(ScmSmallInt size, ScmHalfFloat fill);
SCM_EXTERN ScmObj Scm_MakeF16VectorFromArray(ScmSmallInt size,
                                             const ScmHalfFloat array[]);
SCM_EXTERN ScmObj Scm_MakeF16VectorFromArrayShared(ScmSmallInt size,
                                                   ScmHalfFloat array[]);

SCM_CLASS_DECL(Scm_F32VectorClass);
#define SCM_CLASS_F32VECTOR         (&Scm_F32VectorClass)
#define SCM_F32VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_F32VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_F32VECTOR)
#define SCM_F32VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_F32VECTOR_ELEMENTS(obj) ((float*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_F32VECTOR_ELEMENT(obj,k) SCM_F32VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeF32Vector(ScmSmallInt size, float fill);
SCM_EXTERN ScmObj Scm_MakeF32VectorFromArray(ScmSmallInt size,
                                             const float array[]);
SCM_EXTERN ScmObj Scm_MakeF32VectorFromArrayShared(ScmSmallInt size,
                                                   float array[]);

SCM_CLASS_DECL(Scm_F64VectorClass);
#define SCM_CLASS_F64VECTOR         (&Scm_F64VectorClass)
#define SCM_F64VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_F64VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_F64VECTOR)
#define SCM_F64VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_F64VECTOR_ELEMENTS(obj) ((double*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_F64VECTOR_ELEMENT(obj,k) SCM_F64VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeF64Vector(ScmSmallInt size, double fill);
SCM_EXTERN ScmObj Scm_MakeF64VectorFromArray(ScmSmallInt size,
                                             const double array[]);
SCM_EXTERN ScmObj Scm_MakeF64VectorFromArrayShared(ScmSmallInt size,
                                                   double array[]);

SCM_CLASS_DECL(Scm_C32VectorClass);
#define SCM_CLASS_C32VECTOR         (&Scm_C32VectorClass)
#define SCM_C32VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_C32VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_C32VECTOR)
#define SCM_C32VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_C32VECTOR_ELEMENTS(obj) ((ScmHalfComplex*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_C32VECTOR_ELEMENT(obj,k) SCM_C32VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeC32Vector(ScmSmallInt size, ScmHalfComplex fill);
SCM_EXTERN ScmObj Scm_MakeC32VectorFromArray(ScmSmallInt size,
                                             const ScmHalfComplex array[]);
SCM_EXTERN ScmObj Scm_MakeC32VectorFromArrayShared(ScmSmallInt size,
                                                   ScmHalfComplex array[]);

SCM_CLASS_DECL(Scm_C64VectorClass);
#define SCM_CLASS_C64VECTOR         (&Scm_C64VectorClass)
#define SCM_C64VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_C64VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_C64VECTOR)
#define SCM_C64VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_C64VECTOR_ELEMENTS(obj) ((ScmFloatComplex*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_C64VECTOR_ELEMENT(obj,k) SCM_C64VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeC64Vector(ScmSmallInt size, ScmFloatComplex fill);
SCM_EXTERN ScmObj Scm_MakeC64VectorFromArray(ScmSmallInt size,
                                             const ScmFloatComplex array[]);
SCM_EXTERN ScmObj Scm_MakeC64VectorFromArrayShared(ScmSmallInt size,
                                                   ScmFloatComplex array[]);

SCM_CLASS_DECL(Scm_C128VectorClass);
#define SCM_CLASS_C128VECTOR         (&Scm_C128VectorClass)
#define SCM_C128VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_C128VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_C128VECTOR)
#define SCM_C128VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_C128VECTOR_ELEMENTS(obj) ((ScmDoubleComplex*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_C128VECTOR_ELEMENT(obj,k) SCM_C128VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeC128Vector(ScmSmallInt size, ScmDoubleComplex fill);
SCM_EXTERN ScmObj Scm_MakeC128VectorFromArray(ScmSmallInt size,
                                             const ScmDoubleComplex array[]);
SCM_EXTERN ScmObj Scm_MakeC128VectorFromArrayShared(ScmSmallInt size,
                                                   ScmDoubleComplex array[]);


/* For the backward compatibility.  Use ScmUVector for the new code
   (that's why we don't define ScmC**Vector, for they are new additions. */
typedef ScmUVector ScmS8Vector;
typedef ScmUVector ScmU8Vector;
typedef ScmUVector ScmS16Vector;
typedef ScmUVector ScmU16Vector;
typedef ScmUVector ScmS32Vector;
typedef ScmUVector ScmU32Vector;
typedef ScmUVector ScmS64Vector;
typedef ScmUVector ScmU64Vector;
typedef ScmUVector ScmF16Vector;
typedef ScmUVector ScmF32Vector;
typedef ScmUVector ScmF64Vector;

/*
 * View memory as an uvector
 */
ScmObj Scm_MakeViewUVector(ScmMemoryRegion *mem, ScmClass *klass,
                           ScmSmallInt len, ScmSmallInt offset,
                           int immutable);

/*
 * String/uvector common utility
 */

/* Retrieves underlying byte array */
const uint8_t *Scm_GetBytes(ScmObj str_or_uvec, ScmSize *size);

/*
 * Bitvectors (srfi-178)
 *
 *   We could view a bitvector as u1vector, but the operations on bitvectors
 *   are sufficiently different from uniform vectors, so we made them
 *   disjoint.
 */

typedef struct ScmBitvectorRec {
    SCM_HEADER;
    ScmWord size_flags;         /* (len<<1)|immutable */
    ScmBits *bits;
} ScmBitvector;

SCM_CLASS_DECL(Scm_BitvectorClass);
#define SCM_CLASS_BITVECTOR         (&Scm_BitvectorClass)
#define SCM_BITVECTOR(obj)          ((ScmBitvector*)(obj))
#define SCM_BITVECTORP(obj)         Scm_TypeP(obj, SCM_CLASS_BITVECTOR)
#define SCM_BITVECTOR_BITS(obj)     (SCM_BITVECTOR(obj)->bits)

#define SCM_BITVECTOR_SIZE(obj)     (SCM_BITVECTOR(obj)->size_flags >> 1)
#define SCM_BITVECTOR_IMMUTABLE_P(obj) (SCM_BITVECTOR(obj)->size_flags & 1)
#define SCM_BITVECTOR_IMMUTABLE_SET(obj, flag)    \
    ((flag)                                     \
     ? (SCM_BITVECTOR(obj)->size_flags |= 1)      \
     : (SCM_BITVECTOR(obj)->size_flags &= ~1))

#define SCM_BITVECTOR_CHECK_MUTABLE(obj)                 \
  do { if (SCM_BITVECTOR_IMMUTABLE_P(obj)) {             \
    Scm_Error("bitvector is immutable: %S", obj); \
  }} while (0)

SCM_EXTERN int    Scm_Bit2Int(ScmObj bit);
SCM_EXTERN ScmObj Scm_Bit2Bool(ScmObj bit);
SCM_EXTERN ScmObj Scm_MakeBitvector(ScmSmallInt size, ScmObj init);
SCM_EXTERN ScmObj Scm_ListToBitvector(ScmObj lis);
SCM_EXTERN ScmObj Scm_BitvectorCopy(ScmBitvector *v,
                                    ScmSmallInt start,
                                    ScmSmallInt end);
SCM_EXTERN ScmObj Scm_BitvectorCopyX(ScmBitvector *dest, ScmSmallInt dstart,
                                     ScmBitvector *src,
                                     ScmSmallInt sstart, ScmSmallInt send);

SCM_EXTERN ScmObj Scm_StringToBitvector(ScmString *s, int prefix);
SCM_EXTERN ScmObj Scm_BitvectorToString(ScmBitvector *v, int prefix);

#endif /*GAUCHE_VECTOR_H*/
