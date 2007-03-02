/*
 * vector.c - vector implementation
 *
 *   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
 *  $Id: vector.c,v 1.26 2007-03-02 07:39:14 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"

/*
 * Constructor
 */

static void vector_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    int i;
    SCM_PUTZ("#(", -1, port);
    for (i=0; i<SCM_VECTOR_SIZE(obj); i++) {
        if (i != 0) SCM_PUTC(' ', port);
        Scm_Write(SCM_VECTOR_ELEMENT(obj, i), SCM_OBJ(port), ctx->mode);
    }
    SCM_PUTZ(")", -1, port);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_VectorClass, vector_print, NULL, NULL, NULL,
                         SCM_CLASS_SEQUENCE_CPL);

static ScmVector *make_vector(int size)
{
    ScmVector *v = SCM_NEW2(ScmVector *,
                            sizeof(ScmVector) + sizeof(ScmObj)*(size-1));
    SCM_SET_CLASS(v, SCM_CLASS_VECTOR);
    v->size = size;
    return v;
}

ScmObj Scm_MakeVector(int size, ScmObj fill)
{
    int i;
    ScmVector *v;
    if (size < 0) {
        Scm_Error("vector size must be a positive integer, but got %d", size);
    }
    v = make_vector(size);
    if (SCM_UNBOUNDP(fill)) fill = SCM_UNDEFINED;
    for (i=0; i<size; i++) v->elements[i] = fill;
    return SCM_OBJ(v);
}

ScmObj Scm_ListToVector(ScmObj l, int start, int end)
{
    ScmVector *v;
    ScmObj e;
    int i;

    if (end < 0) {
        int size = Scm_Length(l);
        if (size < 0) Scm_Error("bad list: %S", l);
        SCM_CHECK_START_END(start, end, size);
        v = make_vector(size - start);
    } else {
        SCM_CHECK_START_END(start, end, end);
        v = make_vector(end - start);
    }
    e = Scm_ListTail(l, start, SCM_UNBOUND);
    for (i=0; i<end-start; i++, e=SCM_CDR(e)) {
        if (!SCM_PAIRP(e)) {
            Scm_Error("list too short: %S", l);
        }
        v->elements[i] = SCM_CAR(e);
    }
    return SCM_OBJ(v);
}

ScmObj Scm_VectorToList(ScmVector *v, int start, int end)
{
    int len = SCM_VECTOR_SIZE(v);
    SCM_CHECK_START_END(start, end, len);
    return Scm_ArrayToList(SCM_VECTOR_ELEMENTS(v)+start,
                           end-start);
}

/*
 * Accessors
 */

ScmObj Scm_VectorRef(ScmVector *vec, int i, ScmObj fallback)
{
    if (i < 0 || i >= vec->size) {
        if (SCM_UNBOUNDP(fallback)) {
            Scm_Error("argument out of range: %d", i);
        } else {
            return fallback;
        }
    }
    return vec->elements[i];
}

ScmObj Scm_VectorSet(ScmVector *vec, int i, ScmObj obj)
{
    if (i < 0 || i >= vec->size)
        Scm_Error("argument out of range: %d", i);
    return (vec->elements[i] = obj);
}

ScmObj Scm_VectorFill(ScmVector *vec, ScmObj fill, int start, int end)
{
    int i, len = SCM_VECTOR_SIZE(vec);
    SCM_CHECK_START_END(start, end, len);
    for (i=start; i < end; i++) {
        SCM_VECTOR_ELEMENT(vec, i) = fill;
    }
    return SCM_OBJ(vec);
}

ScmObj Scm_VectorCopy(ScmVector *vec, int start, int end, ScmObj fill)
{
    int i, len = SCM_VECTOR_SIZE(vec);
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
        for (i=0; i<end-start; i++) {
            if (i+start < 0 || i+start >= len) {
                SCM_VECTOR_ELEMENT(v, i) = fill;
            } else {
                SCM_VECTOR_ELEMENT(v, i) = SCM_VECTOR_ELEMENT(vec, i+start);
            }
        }
    }
    return SCM_OBJ(v);
}

