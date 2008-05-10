/*
 * gauche/vector.h - Vector API
 *
 *   Copyright (c) 2000-2008  Shiro Kawai  <shiro@acm.org>
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
 *
 *  $Id: vector.h,v 1.4 2008-05-10 13:36:25 shirok Exp $
 */

#ifndef GAUCHE_VECTOR_H
#define GAUCHE_VECTOR_H

struct ScmVectorRec {
    SCM_HEADER;
    int size;
    ScmObj elements[1];
};

#define SCM_VECTOR(obj)          ((ScmVector*)(obj))
#define SCM_VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_VECTOR)
#define SCM_VECTOR_SIZE(obj)     (SCM_VECTOR(obj)->size)
#define SCM_VECTOR_ELEMENTS(obj) (SCM_VECTOR(obj)->elements)
#define SCM_VECTOR_ELEMENT(obj, i)   (SCM_VECTOR(obj)->elements[i])

SCM_CLASS_DECL(Scm_VectorClass);
#define SCM_CLASS_VECTOR     (&Scm_VectorClass)

SCM_EXTERN ScmObj Scm_MakeVector(int size, ScmObj fill);
SCM_EXTERN ScmObj Scm_VectorRef(ScmVector *vec, int i, ScmObj fallback);
SCM_EXTERN ScmObj Scm_VectorSet(ScmVector *vec, int i, ScmObj obj);
SCM_EXTERN ScmObj Scm_VectorFill(ScmVector *vec, ScmObj fill, int start, int end);

SCM_EXTERN ScmObj Scm_ListToVector(ScmObj l, int start, int end);
SCM_EXTERN ScmObj Scm_VectorToList(ScmVector *v, int start, int end);
SCM_EXTERN ScmObj Scm_VectorCopy(ScmVector *vec, int start, int end,
                                 ScmObj fill);

#endif /*GAUCHE_VECTOR_H*/
