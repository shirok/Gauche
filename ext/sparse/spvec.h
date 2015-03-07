/*
 * spvec.h - Sparse vector
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

#ifndef GAUCHE_SPVEC_H
#define GAUCHE_SPVEC_H

#include <gauche.h>
#include <gauche/extend.h>

#if defined(EXTSPARSE_EXPORTS)
#define LIBGAUCHE_EXT_BODY
#endif
#include <gauche/extern.h>      /* redefine SCM_EXTERN */

#include "ctrie.h"

typedef struct SparseVectorDescriptorRec SparseVectorDescriptor;

/* NB: All Scheme-level sparse vector classes use the single C-level
   object, ScmSparseVector.

   ScmSparseVector uses CompactTrie as a backing storage.  The 'leaf'
   of the CompactTrie may contain 1 to 16 elements of the vector,
   depending on the type of the sparse vector.
*/
typedef struct SparseVectorRec {
    SCM_HEADER;
    SparseVectorDescriptor *desc;
    CompactTrie trie;
    u_long      numEntries;
    u_long      flags;          /* reserved */
    ScmObj      defaultValue;
} SparseVector;

/* Iterator. */
typedef struct SparseVectorIterRec {
    SparseVector   *sv;
    Leaf           *leaf;
    int             leafIndex;
    CompactTrieIter citer;
} SparseVectorIter;

/* SparseVectorDescriptor has common information per class (it should be
   a part of each class, but we just hack for the time being.)
   The constructor of each class sets appropriate descriptor to the instance.

   ref(L,I)   - Returns an element at index I in leaf L.
                SCM_UNBOUND if the element doesn't exist.
   set(L,I,X) - Sets X as a value at index I in leaf L.  Returns TRUE
                if that caused new element creation.
   delete(L, I) - Deletes I-th entry.  Returns the original value,
                 or SCM_UNBOUND if the entry wasn't exist.
   clear(L,_)  - Clears subtype-dependent part of the leaf structure.
   iter(L,&I)  - Returns the next object after I's index in the leaf L,
                 and updates I.  SCM_UNBOUND if the next object is not in L.
                 I is intra-leaf index, not the vector-wide index.
   dump(P,L,I,_) - Dumps leaf data.

   The numEntries field is taken care of by generic routine.
 */
struct SparseVectorDescriptorRec {
    ScmObj   (*ref)(Leaf*, u_long index);
    int      (*set)(Leaf*, u_long index, ScmObj value);
    Leaf    *(*allocate)(void *data);
    ScmObj   (*delete)(Leaf*, u_long index);
    void     (*clear)(Leaf*, void*);
    Leaf    *(*copy)(Leaf*, void*);
    ScmObj   (*iter)(Leaf*, int*);
    void     (*dump)(ScmPort *out, Leaf *leaf, int indent, void *data);

    int shift;                  /* # of shift bits to access Leaf */
};

/* Max # of bits for index.  Theoretrically we can extend this
   as much as we like; current limitation is only for simplicity. */
#define SPARSE_VECTOR_MAX_INDEX_BITS  (SIZEOF_LONG*8)

/* Flags for constructors (currently unused) */
enum {
    SPARSE_VECTOR_ORDERED = (1L<<0)
};

/* Generic API. */
extern ScmObj MakeSparseVector(ScmClass *klass, ScmObj defaultValue, u_long flags);
extern ScmObj SparseVectorRef(SparseVector *sv, u_long index, ScmObj fallback);
extern void   SparseVectorSet(SparseVector *sv, u_long index, ScmObj value);
extern ScmObj SparseVectorDelete(SparseVector *sv, u_long index);
extern void   SparseVectorClear(SparseVector *sv);
extern ScmObj SparseVectorCopy(const SparseVector *src);
extern ScmObj SparseVectorInc(SparseVector *sv, u_long index, ScmObj delta,
                              ScmObj fallback);
extern void   SparseVectorDump(SparseVector *sv);

extern void   SparseVectorIterInit(SparseVectorIter *iter, SparseVector *sv);
extern ScmObj SparseVectorIterNext(SparseVectorIter *iter);

extern void   Scm_Init_spvec(ScmModule *mod);

/*
 * Class stuff
 */
SCM_CLASS_DECL(Scm_SparseVectorBaseClass);
#define SCM_CLASS_SPARSE_VECTOR_BASE  (&Scm_SparseVectorBaseClass)
#define SPARSE_VECTOR_BASE_P(obj) SCM_ISA(obj, SCM_CLASS_SPARSE_VECTOR_BASE)

SCM_CLASS_DECL(Scm_SparseVectorClass);
#define SCM_CLASS_SPARSE_VECTOR   (&Scm_SparseVectorClass)
#define SPARSE_VECTOR(obj)        ((SparseVector*)(obj))
#define SPARSE_VECTOR_P(obj)      SCM_XTYPEP(obj, SCM_CLASS_SPARSE_VECTOR)

SCM_CLASS_DECL(Scm_SparseS8VectorClass);
#define SCM_CLASS_SPARSE_S8VECTOR   (&Scm_SparseS8VectorClass)
#define SPARSE_S8VECTOR(obj)        ((SparseVector*)(obj))
#define SPARSE_S8VECTOR_P(obj)      SCM_XTYPEP(obj, SCM_CLASS_SPARSE_S8VECTOR)

SCM_CLASS_DECL(Scm_SparseU8VectorClass);
#define SCM_CLASS_SPARSE_U8VECTOR   (&Scm_SparseU8VectorClass)
#define SPARSE_U8VECTOR(obj)        ((SparseVector*)(obj))
#define SPARSE_U8VECTOR_P(obj)      SCM_XTYPEP(obj, SCM_CLASS_SPARSE_U8VECTOR)

SCM_CLASS_DECL(Scm_SparseS16VectorClass);
#define SCM_CLASS_SPARSE_S16VECTOR  (&Scm_SparseS16VectorClass)
#define SPARSE_S16VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_S16VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_S16VECTOR)

SCM_CLASS_DECL(Scm_SparseU16VectorClass);
#define SCM_CLASS_SPARSE_U16VECTOR  (&Scm_SparseU16VectorClass)
#define SPARSE_U16VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_U16VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_U16VECTOR)

SCM_CLASS_DECL(Scm_SparseS32VectorClass);
#define SCM_CLASS_SPARSE_S32VECTOR  (&Scm_SparseS32VectorClass)
#define SPARSE_S32VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_S32VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_S32VECTOR)

SCM_CLASS_DECL(Scm_SparseU32VectorClass);
#define SCM_CLASS_SPARSE_U32VECTOR  (&Scm_SparseU32VectorClass)
#define SPARSE_U32VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_U32VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_U32VECTOR)

SCM_CLASS_DECL(Scm_SparseS64VectorClass);
#define SCM_CLASS_SPARSE_S64VECTOR  (&Scm_SparseS64VectorClass)
#define SPARSE_S64VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_S64VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_S64VECTOR)

SCM_CLASS_DECL(Scm_SparseU64VectorClass);
#define SCM_CLASS_SPARSE_U64VECTOR  (&Scm_SparseU64VectorClass)
#define SPARSE_U64VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_U64VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_U64VECTOR)

SCM_CLASS_DECL(Scm_SparseF16VectorClass);
#define SCM_CLASS_SPARSE_F16VECTOR  (&Scm_SparseF16VectorClass)
#define SPARSE_F16VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_F16VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_F16VECTOR)

SCM_CLASS_DECL(Scm_SparseF32VectorClass);
#define SCM_CLASS_SPARSE_F32VECTOR  (&Scm_SparseF32VectorClass)
#define SPARSE_F32VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_F32VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_F32VECTOR)

SCM_CLASS_DECL(Scm_SparseF64VectorClass);
#define SCM_CLASS_SPARSE_F64VECTOR  (&Scm_SparseF64VectorClass)
#define SPARSE_F64VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_F64VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_F64VECTOR)

SCM_CLASS_DECL(Scm_SparseMatrixBaseClass);
#define SCM_CLASS_SPARSE_MATRIX_BASE  (&Scm_SparseMatrixBaseClass)
#define SPARSE_MATRIX_BASE_P(obj) SCM_ISA(obj, SCM_CLASS_SPARSE_MATRIX_BASE)

SCM_CLASS_DECL(Scm_SparseMatrixClass);
#define SCM_CLASS_SPARSE_MATRIX   (&Scm_SparseMatrixClass)
#define SPARSE_MATRIX(obj)        ((SparseVector*)(obj))
#define SPARSE_MATRIX_P(obj)      SCM_XTYPEP(obj, SCM_CLASS_SPARSE_MATRIX)

SCM_CLASS_DECL(Scm_SparseS8MatrixClass);
#define SCM_CLASS_SPARSE_S8MATRIX   (&Scm_SparseS8MatrixClass)
#define SPARSE_S8MATRIX(obj)        ((SparseVector*)(obj))
#define SPARSE_S8MATRIX_P(obj)      SCM_XTYPEP(obj, SCM_CLASS_SPARSE_S8MATRIX)

SCM_CLASS_DECL(Scm_SparseU8MatrixClass);
#define SCM_CLASS_SPARSE_U8MATRIX   (&Scm_SparseU8MatrixClass)
#define SPARSE_U8MATRIX(obj)        ((SparseVector*)(obj))
#define SPARSE_U8MATRIX_P(obj)      SCM_XTYPEP(obj, SCM_CLASS_SPARSE_U8MATRIX)

SCM_CLASS_DECL(Scm_SparseS16MatrixClass);
#define SCM_CLASS_SPARSE_S16MATRIX  (&Scm_SparseS16MatrixClass)
#define SPARSE_S16MATRIX(obj)       ((SparseVector*)(obj))
#define SPARSE_S16MATRIX_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_S16MATRIX)

SCM_CLASS_DECL(Scm_SparseU16MatrixClass);
#define SCM_CLASS_SPARSE_U16MATRIX  (&Scm_SparseU16MatrixClass)
#define SPARSE_U16MATRIX(obj)       ((SparseVector*)(obj))
#define SPARSE_U16MATRIX_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_U16MATRIX)

SCM_CLASS_DECL(Scm_SparseS32MatrixClass);
#define SCM_CLASS_SPARSE_S32MATRIX  (&Scm_SparseS32MatrixClass)
#define SPARSE_S32MATRIX(obj)       ((SparseVector*)(obj))
#define SPARSE_S32MATRIX_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_S32MATRIX)

SCM_CLASS_DECL(Scm_SparseU32MatrixClass);
#define SCM_CLASS_SPARSE_U32MATRIX  (&Scm_SparseU32MatrixClass)
#define SPARSE_U32MATRIX(obj)       ((SparseVector*)(obj))
#define SPARSE_U32MATRIX_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_U32MATRIX)

SCM_CLASS_DECL(Scm_SparseS64MatrixClass);
#define SCM_CLASS_SPARSE_S64MATRIX  (&Scm_SparseS64MatrixClass)
#define SPARSE_S64MATRIX(obj)       ((SparseVector*)(obj))
#define SPARSE_S64MATRIX_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_S64MATRIX)

SCM_CLASS_DECL(Scm_SparseU64MatrixClass);
#define SCM_CLASS_SPARSE_U64MATRIX  (&Scm_SparseU64MatrixClass)
#define SPARSE_U64MATRIX(obj)       ((SparseVector*)(obj))
#define SPARSE_U64MATRIX_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_U64MATRIX)

SCM_CLASS_DECL(Scm_SparseF16MatrixClass);
#define SCM_CLASS_SPARSE_F16MATRIX  (&Scm_SparseF16MatrixClass)
#define SPARSE_F16MATRIX(obj)       ((SparseVector*)(obj))
#define SPARSE_F16MATRIX_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_F16MATRIX)

SCM_CLASS_DECL(Scm_SparseF32MatrixClass);
#define SCM_CLASS_SPARSE_F32MATRIX  (&Scm_SparseF32MatrixClass)
#define SPARSE_F32MATRIX(obj)       ((SparseVector*)(obj))
#define SPARSE_F32MATRIX_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_F32MATRIX)

SCM_CLASS_DECL(Scm_SparseF64MatrixClass);
#define SCM_CLASS_SPARSE_F64MATRIX  (&Scm_SparseF64MatrixClass)
#define SPARSE_F64MATRIX(obj)       ((SparseVector*)(obj))
#define SPARSE_F64MATRIX_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_F64MATRIX)

#endif /*GAUCHE_SPVEC_H*/
