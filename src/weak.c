/*
 * weak.c - weak vectors and tables
 *
 *  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: weak.c,v 1.1 2002-04-24 23:18:15 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"

/*=============================================================
 * Weak vector
 *
 *  A weak vector is like a vector of Scheme objects, except
 *  it doesn't prevent the referenced objects to be garbage-collected.
 *  Internally, it is implemented using "disappearing link" feature
 *  of Boehm GC; when the referenced object is collected, the pointer
 *  in the vector is set to NULL.
 *  It is important to keep track of whether the entry of the vector
 *  is registered as a disappearing link or not, for you can't register
 *  the same location more than once.
 */

static void weakvector_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    int i;
    ScmWeakVector *v = SCM_WEAKVECTOR(obj);
    ScmObj *ptrs = (ScmObj*)v->pointers;
    Scm_Printf(port, "#,(<weak-vector> %d", v->size);
    for (i=0; i<v->size; i++) {
        SCM_PUTC(' ', port);
        if (ptrs[i]) Scm_Write(ptrs[i], SCM_OBJ(port), ctx->mode);
        else         Scm_Write(SCM_FALSE, SCM_OBJ(port), ctx->mode);
    }
    SCM_PUTC(')', port);
}

static void weakvector_finalize(GC_PTR obj, GC_PTR data)
{
    int i;
    ScmWeakVector *v = SCM_WEAKVECTOR(obj);
    ScmObj *p = (ScmObj*)v->pointers;
    for (i=0; i<v->size; i++) {
        if (p[i]==NULL || SCM_PTRP(p[i])) {
            GC_unregister_disappearing_link((GC_PTR*)&p[i]);
        }
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_WeakVectorClass, weakvector_print,
                         NULL, NULL, NULL,
                         SCM_CLASS_SEQUENCE_CPL);

ScmObj Scm_MakeWeakVector(int size)
{
    int i;
    ScmObj *p;
    ScmWeakVector *v = SCM_NEW(ScmWeakVector);
    GC_finalization_proc ofn; GC_PTR ocd;
    
    SCM_SET_CLASS(v, SCM_CLASS_WEAKVECTOR);
    v->size = size;
    /* Allocate pointer array by ATOMIC, so that GC won't trace the
       pointers in it.  */
    p = SCM_NEW_ATOMIC2(ScmObj*, size * sizeof(ScmObj));
    for (i=0; i<size; i++) p[i] = SCM_FALSE;
    v->pointers = (void*)p;
    GC_register_finalizer((GC_PTR)v, weakvector_finalize, NULL, &ofn, &ocd);
    return SCM_OBJ(v);
}

ScmObj Scm_WeakVectorRef(ScmWeakVector *v, int index, ScmObj fallback)
{
    ScmObj *p;
    if (index < 0 || index >= v->size) {
        if (SCM_UNBOUNDP(fallback)) {
            Scm_Error("argument out of range: %d", index);
        } else {
            return fallback;
        }
    }
    p = (ScmObj*)v->pointers;
    if (p[index] == NULL) {
        if (SCM_UNBOUNDP(fallback)) return SCM_FALSE;
        else return fallback;
    } else {
        return p[index];
    }
}

ScmObj Scm_WeakVectorSet(ScmWeakVector *v, int index, ScmObj value)
{
    ScmObj *p;
    if (index < 0 || index >= v->size) {
        Scm_Error("argument out of range: %d", index);
    }
    p = (ScmObj*)v->pointers;

    /* unregister the location if it was registered before */
    if (p[index] == NULL || SCM_PTRP(p[index])) {
        GC_unregister_disappearing_link((GC_PTR*)&p[index]);
    }

    p[index] = value;
    /* register the location if the value is a heap object */
    if (SCM_PTRP(value)) {
        GC_general_register_disappearing_link((GC_PTR*)&p[index], (GC_PTR)value);
    }
    return SCM_UNDEFINED;
}

