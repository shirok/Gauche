/*
 * vector.c - vector implementation
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: vector.c,v 1.6 2001-03-17 09:17:51 shiro Exp $
 */

#include "gauche.h"

/*
 * Constructor
 */

static int vector_print(ScmObj obj, ScmPort *port, int mode)
{
    int i, nc = 0;
    SCM_PUTCSTR("#(", port); nc += 2;
    for (i=0; i<SCM_VECTOR_SIZE(obj); i++) {
        if (i != 0) { SCM_PUTC(' ', port); nc++; }
        nc += Scm_Write(SCM_VECTOR_ELEMENT(obj, i), SCM_OBJ(port), mode);
    }
    SCM_PUTCSTR(")", port); nc++;
    return nc;
}

SCM_DEFINE_BUILTIN_CLASS(Scm_VectorClass,
                         vector_print, NULL, NULL, NULL,
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
    ScmVector *v = make_vector(size);
    for (i=0; i<size; i++) v->elements[i] = fill;
    return SCM_OBJ(v);
}

ScmObj Scm_ListToVector(ScmObj l)
{
    ScmVector *v;
    ScmObj e;
    int size = Scm_Length(l), i = 0;
    SCM_ASSERT(size >= 0);
    v = make_vector(size);
    SCM_FOR_EACH(e, l) {
        v->elements[i++] = SCM_CAR(e);
    }
    return SCM_OBJ(v);
}

ScmObj Scm_VectorToList(ScmVector *v)
{
    ScmObj start = SCM_NIL, last, e;
    int i;
    
    SCM_VECTOR_FOR_EACH(i, e, v) {
        SCM_APPEND1(start, last, e);
    }
    return start;
}

/*
 * Accessors
 */

ScmObj Scm_VectorRef(ScmVector *vec, int i)
{
    if (i < 0 || i >= vec->size)
        Scm_Error("argument out of range: %d", i);
    return vec->elements[i];
}

ScmObj Scm_VectorSet(ScmVector *vec, int i, ScmObj obj)
{
    if (i < 0 || i >= vec->size)
        Scm_Error("argument out of range: %d", i);
    return (vec->elements[i] = obj);
}

ScmObj Scm_VectorFill(ScmVector *vec, ScmObj fill)
{
    int i;
    for (i=0; i < SCM_VECTOR_SIZE(vec); i++) {
        SCM_VECTOR_ELEMENT(vec, i) = fill;
    }
    return SCM_OBJ(vec);
}
