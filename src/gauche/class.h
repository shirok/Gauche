/*
 * class.h - Gauche object system private header
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
 *  $Id: class.h,v 1.8 2001-03-22 08:21:34 shiro Exp $
 */

#ifndef GAUCHE_CLASS_H
#define GAUCHE_CLASS_H

#ifdef __cplusplus
extern "C" {
#endif

/* internal object to couple C-written getter & setter */
typedef struct ScmClassAccessorRec {
    SCM_HEADER;
    ScmClass *klass;
    ScmObj (*getter)(ScmObj instance);
    void (*setter)(ScmObj instance, ScmObj value);
} ScmClassAccessor;

typedef ScmObj (*ScmNativeGetterProc)(ScmObj);
typedef void   (*ScmNativeSetterProc)(ScmObj, ScmObj);

extern ScmClass Scm_ClassAccessorClass;
#define SCM_CLASS_CLASS_ACCESSOR    (&Scm_ClassAccessorClass)
#define SCM_CLASS_ACCESSOR(obj)     ((ScmClassAccessor*)obj)

/* for static declaration of fields */
typedef struct ScmClassStaticSlotSpecRec {
    const char *name;
    ScmClassAccessor accessor;
} ScmClassStaticSlotSpec;

#define SCM_CLASS_SLOT_SPEC(name, klass, getter, setter)        \
    { name, { SCM_CLASS_CLASS_ACCESSOR,                         \
              klass,                                            \
              (ScmNativeGetterProc)getter,                      \
              (ScmNativeSetterProc)setter }}
              
/* object system base classes */


/* for subclassing */
typedef struct ScmSubClassRec {
    ScmClass klassrec;
    ScmObj slots[1];
} ScmSubClass;


extern ScmObj Scm_ClassAllocate(ScmClass *klass, int nslots);

extern ScmObj Scm_GetSlotAllocation(ScmObj slot);
extern ScmObj Scm_ComputeCPL(ScmClass *klass, ScmObj directSupers);
extern ScmObj Scm_ComputeApplicableMethods(ScmGeneric *gf,
                                           ScmObj *args,
                                           int nargs);
extern ScmObj Scm_SortMethods(ScmObj methods, ScmObj *args, int nargs);
extern ScmObj Scm_MakeNextMethod(ScmGeneric *gf, ScmObj methods,
                                 ScmObj *args, int nargs);
extern ScmObj Scm_AddMethod(ScmGeneric *gf, ScmMethod *method);

#ifdef __cplusplus
}
#endif

#endif /* GAUCHE_CLASS_H */
