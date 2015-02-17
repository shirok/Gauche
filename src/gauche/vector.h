/*
 * gauche/vector.h - Vector API
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

#ifndef GAUCHE_VECTOR_H
#define GAUCHE_VECTOR_H

/*
 * General vector
 */

struct ScmVectorRec {
    SCM_HEADER;
    ScmWord size;
    ScmObj elements[1];
};

#define SCM_VECTOR(obj)          ((ScmVector*)(obj))
#define SCM_VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_VECTOR)
#define SCM_VECTOR_SIZE(obj)     (SCM_VECTOR(obj)->size)
#define SCM_VECTOR_ELEMENTS(obj) (SCM_VECTOR(obj)->elements)
#define SCM_VECTOR_ELEMENT(obj, i)   (SCM_VECTOR(obj)->elements[i])

SCM_CLASS_DECL(Scm_VectorClass);
#define SCM_CLASS_VECTOR     (&Scm_VectorClass)

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
#if !GAUCHE_API_0_95
    unsigned int immutable : 1;
    int size : (SIZEOF_INT*CHAR_BIT-1);
#else  /* GAUCHE_API_0_95 */
    ScmWord size_flags;         /* (len<<1)|immutable */
#endif /* GAUCHE_API_0_95 */
    void *owner;
    void *elements;
} ScmUVector;

SCM_CLASS_DECL(Scm_UVectorClass);
#define SCM_CLASS_UVECTOR         (&Scm_UVectorClass)
#define SCM_UVECTOR(obj)          ((ScmUVector*)(obj))
#define SCM_UVECTORP(obj)         Scm_TypeP(obj, SCM_CLASS_UVECTOR)
#define SCM_UVECTOR_OWNER(obj)    (SCM_UVECTOR(obj)->owner)
#define SCM_UVECTOR_ELEMENTS(obj) (SCM_UVECTOR(obj)->elements)

#if !GAUCHE_API_0_95
#define SCM_UVECTOR_SIZE(obj)     (SCM_UVECTOR(obj)->size)
#define SCM_UVECTOR_IMMUTABLE_P(obj) (SCM_UVECTOR(obj)->immutable)
#define SCM_UVECTOR_INITIALIZER(klass, size, elements, immutable, owner) \
    { { SCM_CLASS_STATIC_TAG(klass) }, (immutable), (size), \
      (owner), (elements) }
#else  /* GAUCHE_API_0_95 */
#define SCM_UVECTOR_SIZE(obj)     (SCM_UVECTOR(obj)->size_flags >> 1)
#define SCM_UVECTOR_IMMUTABLE_P(obj) (SCM_UVECTOR(obj)->size_flags & 1)
#define SCM_UVECTOR_INITIALIZER(klass, size, elements, immutable, owner) \
    { { SCM_CLASS_STATIC_TAG(klass) }, (((size)<<1)|(immutable?1:0)),    \
      (owner), (elements) }
#endif /* GAUCHE_API_0_95 */


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
#define SCM_S8VECTOR_ELEMENTS(obj) ((signed char*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_S8VECTOR_ELEMENT(obj,k) SCM_S8VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeS8Vector(ScmSmallInt size, signed char fill);
SCM_EXTERN ScmObj Scm_MakeS8VectorFromArray(ScmSmallInt size,
                                            const signed char array[]);
SCM_EXTERN ScmObj Scm_MakeS8VectorFromArrayShared(ScmSmallInt size,
                                                  signed char array[]);

SCM_CLASS_DECL(Scm_U8VectorClass);
#define SCM_CLASS_U8VECTOR         (&Scm_U8VectorClass)
#define SCM_U8VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_U8VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_U8VECTOR)
#define SCM_U8VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_U8VECTOR_ELEMENTS(obj) ((u_char*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_U8VECTOR_ELEMENT(obj,k) SCM_U8VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeU8Vector(ScmSmallInt size, unsigned char fill);
SCM_EXTERN ScmObj Scm_MakeU8VectorFromArray(ScmSmallInt size,
                                            const unsigned char array[]);
SCM_EXTERN ScmObj Scm_MakeU8VectorFromArrayShared(ScmSmallInt size,
                                                  unsigned char array[]);

SCM_CLASS_DECL(Scm_S16VectorClass);
#define SCM_CLASS_S16VECTOR         (&Scm_S16VectorClass)
#define SCM_S16VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_S16VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_S16VECTOR)
#define SCM_S16VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_S16VECTOR_ELEMENTS(obj) ((short*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_S16VECTOR_ELEMENT(obj,k) SCM_S16VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeS16Vector(ScmSmallInt size, short fill);
SCM_EXTERN ScmObj Scm_MakeS16VectorFromArray(ScmSmallInt size,
                                             const short array[]);
SCM_EXTERN ScmObj Scm_MakeS16VectorFromArrayShared(ScmSmallInt size,
                                                   short array[]);

SCM_CLASS_DECL(Scm_U16VectorClass);
#define SCM_CLASS_U16VECTOR         (&Scm_U16VectorClass)
#define SCM_U16VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_U16VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_U16VECTOR)
#define SCM_U16VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_U16VECTOR_ELEMENTS(obj) ((u_short*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_U16VECTOR_ELEMENT(obj,k) SCM_U16VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeU16Vector(ScmSmallInt size, unsigned short fill);
SCM_EXTERN ScmObj Scm_MakeU16VectorFromArray(ScmSmallInt size,
                                             const unsigned short array[]);
SCM_EXTERN ScmObj Scm_MakeU16VectorFromArrayShared(ScmSmallInt size,
                                                   unsigned short array[]);

SCM_CLASS_DECL(Scm_S32VectorClass);
#define SCM_CLASS_S32VECTOR         (&Scm_S32VectorClass)
#define SCM_S32VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_S32VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_S32VECTOR)
#define SCM_S32VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_S32VECTOR_ELEMENTS(obj) ((ScmInt32*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_S32VECTOR_ELEMENT(obj,k) SCM_S32VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeS32Vector(ScmSmallInt size, ScmInt32 fill);
SCM_EXTERN ScmObj Scm_MakeS32VectorFromArray(ScmSmallInt size, const ScmInt32 array[]);
SCM_EXTERN ScmObj Scm_MakeS32VectorFromArrayShared(ScmSmallInt size, ScmInt32 array[]);

SCM_CLASS_DECL(Scm_U32VectorClass);
#define SCM_CLASS_U32VECTOR         (&Scm_U32VectorClass)
#define SCM_U32VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_U32VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_U32VECTOR)
#define SCM_U32VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_U32VECTOR_ELEMENTS(obj) ((ScmUInt32*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_U32VECTOR_ELEMENT(obj,k) SCM_U32VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeU32Vector(ScmSmallInt size, ScmUInt32 fill);
SCM_EXTERN ScmObj Scm_MakeU32VectorFromArray(ScmSmallInt size,
                                             const ScmUInt32 array[]);
SCM_EXTERN ScmObj Scm_MakeU32VectorFromArrayShared(ScmSmallInt size,
                                                   ScmUInt32 array[]);

SCM_CLASS_DECL(Scm_S64VectorClass);
#define SCM_CLASS_S64VECTOR         (&Scm_S64VectorClass)
#define SCM_S64VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_S64VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_S64VECTOR)
#define SCM_S64VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_S64VECTOR_ELEMENTS(obj) ((ScmInt64*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_S64VECTOR_ELEMENT(obj,k) SCM_S64VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeS64Vector(ScmSmallInt size, ScmInt64 fill);
SCM_EXTERN ScmObj Scm_MakeS64VectorFromArray(ScmSmallInt size,
                                             const ScmInt64 array[]);
SCM_EXTERN ScmObj Scm_MakeS64VectorFromArrayShared(ScmSmallInt size,
                                                   ScmInt64 array[]);

SCM_CLASS_DECL(Scm_U64VectorClass);
#define SCM_CLASS_U64VECTOR         (&Scm_U64VectorClass)
#define SCM_U64VECTOR(obj)          SCM_UVECTOR(obj)
#define SCM_U64VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_U64VECTOR)
#define SCM_U64VECTOR_SIZE(obj)     SCM_UVECTOR_SIZE(obj)
#define SCM_U64VECTOR_ELEMENTS(obj) ((ScmUInt64*)SCM_UVECTOR_ELEMENTS(obj))
#define SCM_U64VECTOR_ELEMENT(obj,k) SCM_U64VECTOR_ELEMENTS(obj)[k]
SCM_EXTERN ScmObj Scm_MakeU64Vector(ScmSmallInt size, ScmUInt64 fill);
SCM_EXTERN ScmObj Scm_MakeU64VectorFromArray(ScmSmallInt size,
                                             const ScmUInt64 array[]);
SCM_EXTERN ScmObj Scm_MakeU64VectorFromArrayShared(ScmSmallInt size,
                                                   ScmUInt64 array[]);

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


/* For the backward compatibility */
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

#endif /*GAUCHE_VECTOR_H*/
