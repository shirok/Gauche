/*
 * spvec.h - Sparse vector
 *
 *   Copyright (c) 2009  Shiro Kawai  <shiro@acm.org>
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

#include "ctrie.h"

typedef struct SparseVectorDescriptorRec SparseVectorDescriptor;

/* NB: All Scheme-level sparse vector classes use the single C-level
   object, ScmSparseVector.

   ScmSparseVector uses CompactTrie as a backing storage.  The 'leaf'
   of the CompactTrie is a compacted array that can contain up to
   2^chunkBits.   Currently we support up to 5 chunkBits---that is,
   the leaf array can contain up to 32 elements.

   The current CompactTrie can use up to 32bit key, so the maximum
   bits of ScmSparseVector is 32+5=37bits.  This may be extended later.
*/
typedef struct SparseVectorRec {
    SCM_HEADER;
    SparseVectorDescriptor *desc;
    CompactTrie trie;
    u_long      numEntries;
    u_char      chunkBits;
    u_char      trieBits;
} SparseVector;

#define MAX_CHUNK_BITS  5
#define MAX_TRIE_BITS  32

/* SparseVectorDescriptor has common information per class (it should be
   a part of each class, but we just hack for the time being.  The constructor
   of each class sets appropriate descriptor to the instance.
 */
struct SparseVectorDescriptorRec {
    int      (*check)(ScmObj value);
    ScmObj   (*retrieve)(void *elements, u_long offset);
    void     (*store)(void *elements, u_long offset, ScmObj value);
    void    *(*extend)(void *elements, int origsize, int insertion);
    void     (*dump)(ScmPort *out, Leaf *leaf, int indent, void *data);

    const char *name;           /* name used in error messages */
    u_char      elementAtomic;  /* TRUE if we can use ATOMIC vector for elts */
    u_char      elementSize;    /* element size in bytes */
};

SCM_CLASS_DECL(Scm_SparseVectorBaseClass);
#define SCM_CLASS_SPARSE_VECTOR_BASE  (&Scm_SparseVectorBaseClass)
#define SCM_SPARSE_VECTOR_BASE_P(obj) SCM_ISA(obj, SCM_CLASS_SPARSE_VECTOR_BASE)

SCM_CLASS_DECL(Scm_SparseVectorClass);
#define SCM_CLASS_SPARSE_VECTOR   (&Scm_SparseVectorClass)
#define SPARSE_VECTOR(obj)        ((SparseVector*)(obj))
#define SPARSE_VECTOR_P(obj)      SCM_XTYPEP(obj, SCM_CLASS_SPARSE_VECTOR)

extern ScmObj MakeSparseVector(u_long flags);

#if 0

SCM_CLASS_DECL(Scm_SparseU8VectorClass);
#define SCM_CLASS_SPARSE_U8VECTOR   (&Scm_SparseU8VectorClass)
#define SPARSE_U8VECTOR(obj)        ((SparseVector*)(obj))
#define SPARSE_U8VECTOR_P(obj)      SCM_XTYPEP(obj, SCM_CLASS_SPARSE_U8VECTOR)

extern ScmObj MakeSparseU8Vector(int size,
                                 ScmObj defaultValue,
                                 u_short chunkSize);

SCM_CLASS_DECL(Scm_SparseS8VectorClass);
#define SCM_CLASS_SPARSE_S8VECTOR   (&Scm_SparseS8VectorClass)
#define SPARSE_S8VECTOR(obj)        ((SparseVector*)(obj))
#define SPARSE_S8VECTOR_P(obj)      SCM_XTYPEP(obj, SCM_CLASS_SPARSE_S8VECTOR)

extern ScmObj Scm_MakeSparseS8Vector(int size,
                                     ScmObj defaultValue,
                                     u_short chunkSize);

SCM_CLASS_DECL(Scm_SparseU16VectorClass);
#define SCM_CLASS_SPARSE_U16VECTOR  (&Scm_SparseU16VectorClass)
#define SPARSE_U16VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_U16VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_U16VECTOR)

extern ScmObj Scm_MakeSparseU16Vector(int size,
                                      ScmObj defaultValue,
                                      u_short chunkSize);

SCM_CLASS_DECL(Scm_SparseS16VectorClass);
#define SCM_CLASS_SPARSE_S16VECTOR  (&Scm_SparseS16VectorClass)
#define SPARSE_S16VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_S16VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_S16VECTOR)

SCM_CLASS_DECL(Scm_SparseU32VectorClass);
#define SCM_CLASS_SPARSE_U32VECTOR  (&Scm_SparseU32VectorClass)
#define SPARSE_U32VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_U32VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_U32VECTOR)

SCM_CLASS_DECL(Scm_SparseS32VectorClass);
#define SCM_CLASS_SPARSE_S32VECTOR  (&Scm_SparseS32VectorClass)
#define SPARSE_S32VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_S32VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_S32VECTOR)

SCM_CLASS_DECL(Scm_SparseU64VectorClass);
#define SCM_CLASS_SPARSE_U64VECTOR  (&Scm_SparseU64VectorClass)
#define SPARSE_U64VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_U64VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_U64VECTOR)

SCM_CLASS_DECL(Scm_SparseS64VectorClass);
#define SCM_CLASS_SPARSE_S64VECTOR  (&Scm_SparseS64VectorClass)
#define SPARSE_S64VECTOR(obj)       ((SparseVector*)(obj))
#define SPARSE_S64VECTOR_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_S64VECTOR)

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

#endif /*0*/

extern ScmObj MakeSparseVectorGeneric(ScmClass *klass,
                                      int chunkBits, int trieBits,
                                      SparseVectorDescriptor *desc);

extern ScmObj SparseVectorRef(SparseVector *sv, u_long index, ScmObj fallback);
extern void   SparseVectorSet(SparseVector *sv, u_long index, ScmObj value);
extern void   SparseVectorClear(SparseVector *sv);

extern void   SparseVectorDump(SparseVector *sv);

extern void   Scm_Init_spvec(ScmModule *mod);

#endif /*GAUCHE_SPVEC_H*/
