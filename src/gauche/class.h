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
 *  $Id: class.h,v 1.14 2001-03-31 08:42:31 shiro Exp $
 */

#ifndef GAUCHE_CLASS_H
#define GAUCHE_CLASS_H

#ifdef __cplusplus
extern "C" {
#endif

/* keep class initialization & access info */
typedef struct ScmSlotAccessorRec {
    SCM_HEADER;
    ScmObj (*getter)(ScmObj instance);
    void (*setter)(ScmObj instance, ScmObj value);
    ScmObj initValue;
    ScmObj initKeyword;
    ScmObj initThunk;
    int slotNumber;             /* for :instance slot access */
    ScmObj schemeAccessor;      /* for :virtual slot (getter . setter) */
} ScmSlotAccessor;

typedef ScmObj (*ScmNativeGetterProc)(ScmObj);
typedef void   (*ScmNativeSetterProc)(ScmObj, ScmObj);

extern ScmClass Scm_SlotAccessorClass;
#define SCM_CLASS_SLOT_ACCESSOR    (&Scm_SlotAccessorClass)
#define SCM_SLOT_ACCESSOR(obj)     ((ScmSlotAccessor*)obj)

/* for static declaration of fields */
typedef struct ScmClassStaticSlotSpecRec {
    const char *name;
    ScmSlotAccessor accessor;
} ScmClassStaticSlotSpec;

#define SCM_CLASS_SLOT_SPEC(name, getter, setter, initkey)      \
    { name, { { SCM_CLASS_SLOT_ACCESSOR },                      \
              (ScmNativeGetterProc)getter,                      \
              (ScmNativeSetterProc)setter,                      \
              SCM_UNBOUND,                                      \
              initkey,                                          \
              SCM_FALSE,                                        \
              0,                                                \
              SCM_FALSE } }

#define SCM_CLASS_SLOT_SPEC_END()   { NULL }

/* some internal methods */
    
extern ScmObj Scm_ClassAllocate(ScmClass *klass, int nslots);
extern ScmObj Scm_ComputeCPL(ScmClass *klass);
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
