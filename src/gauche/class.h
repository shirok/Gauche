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
 *  $Id: class.h,v 1.22 2002-01-02 21:16:14 shirok Exp $
 */

#ifndef GAUCHE_CLASS_H
#define GAUCHE_CLASS_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * SlotAccessor
 *  - Packages slot initialization and accessing methods.
 */
typedef struct ScmSlotAccessorRec {
    SCM_HEADER;
    ScmClass *klass;            /* the class this accessor belongs to.
                                   need to be checked before used, for the
                                   class may be changed. */
    ScmObj name;                /* slot name (symbol) */
    ScmObj (*getter)(ScmObj instance); /* getter for C accessor */
    void (*setter)(ScmObj instance, ScmObj value); /* setter for C accessor */
    ScmObj initValue;           /* :init-value */
    ScmObj initKeyword;         /* :init-keyword */
    ScmObj initThunk;           /* :initform or :init-thunk */
    int initializable;          /* is this slot initializable? */
    int slotNumber;             /* for :instance slot access */
    ScmObj schemeAccessor;      /* for :virtual slot (getter . setter) */
} ScmSlotAccessor;

typedef ScmObj (*ScmNativeGetterProc)(ScmObj);
typedef void   (*ScmNativeSetterProc)(ScmObj, ScmObj);

extern ScmClass Scm_SlotAccessorClass;
#define SCM_CLASS_SLOT_ACCESSOR    (&Scm_SlotAccessorClass)
#define SCM_SLOT_ACCESSOR(obj)     ((ScmSlotAccessor*)obj)
#define SCM_SLOT_ACCESSOR_P(obj)   SCM_XTYPEP(obj, SCM_CLASS_SLOT_ACCESSOR)

/* for static declaration of fields */
struct ScmClassStaticSlotSpecRec {
    const char *name;
    ScmSlotAccessor accessor;
};

#define SCM_CLASS_SLOT_SPEC(name, getter, setter)       \
    { name, { { SCM_CLASS_SLOT_ACCESSOR },              \
              NULL, NULL,                               \
              (ScmNativeGetterProc)getter,              \
              (ScmNativeSetterProc)setter,              \
              SCM_UNBOUND,                              \
              SCM_FALSE,                                \
              SCM_FALSE,                                \
              TRUE, 0,                                  \
              SCM_FALSE,                                \
             } }

#define SCM_CLASS_SLOT_SPEC_END()   { NULL }

/* cliche in allocate method */
#define SCM_ALLOCATE(klassname, klass) \
    SCM_NEW2(klassname*, sizeof(klassname) + sizeof(ScmObj)*((klass)->numInstanceSlots))

/* some internal methods */
    
extern ScmObj Scm_ClassAllocate(ScmClass *klass, int nslots);
extern ScmObj Scm_ComputeCPL(ScmClass *klass);
extern ScmObj Scm_ComputeApplicableMethods(ScmGeneric *gf,
                                           ScmObj *args,
                                           int nargs);
extern ScmObj Scm_SortMethods(ScmObj methods, ScmObj *args, int nargs);
extern ScmObj Scm_MakeNextMethod(ScmGeneric *gf, ScmObj methods,
                                 ScmObj *args, int nargs, int copyArgs);
extern ScmObj Scm_AddMethod(ScmGeneric *gf, ScmMethod *method);

extern ScmObj Scm_VMSlotRefUsingAccessor(ScmObj obj,
                                         ScmSlotAccessor *acc,
                                         int boundp);
extern ScmObj Scm_VMSlotSetUsingAccessor(ScmObj obj,
                                         ScmSlotAccessor *acc,
                                         ScmObj val);

extern ScmObj Scm_InstanceSlotRef(ScmObj obj, int number);
extern void Scm_InstanceSlotSet(ScmObj obj, int number, ScmObj val);

extern ScmGeneric Scm_GenericApplyGeneric;

#ifdef __cplusplus
}
#endif

#endif /* GAUCHE_CLASS_H */
