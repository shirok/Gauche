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
 *  $Id: class.h,v 1.28 2002-12-22 12:29:58 shirok Exp $
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

SCM_CLASS_DECL(Scm_SlotAccessorClass);
#define SCM_CLASS_SLOT_ACCESSOR    (&Scm_SlotAccessorClass)
#define SCM_SLOT_ACCESSOR(obj)     ((ScmSlotAccessor*)obj)
#define SCM_SLOT_ACCESSOR_P(obj)   SCM_XTYPEP(obj, SCM_CLASS_SLOT_ACCESSOR)

/* for static declaration of fields */
struct ScmClassStaticSlotSpecRec {
    const char *name;
    ScmSlotAccessor accessor;
};

#define SCM_CLASS_SLOT_SPEC(name, getter, setter)		\
    { name, { {SCM_CLASS_STATIC_PTR(Scm_SlotAccessorClass)},    \
              NULL, NULL,					\
              (ScmNativeGetterProc)getter,			\
              (ScmNativeSetterProc)setter,			\
              SCM_UNBOUND,					\
              SCM_FALSE,					\
              SCM_FALSE,					\
              TRUE, 0,						\
              SCM_FALSE,					\
             } }

#define SCM_CLASS_SLOT_SPEC_END()   { NULL }

/* cliche in allocate method */
#define SCM_ALLOCATE(klassname, klass) \
    ((klassname*)Scm_AllocateInstance(klass, sizeof(klassname)))

/* some internal methods */
    
SCM_EXTERN ScmObj Scm_AllocateInstance(ScmClass *klass, int coresize);
SCM_EXTERN ScmObj Scm_ComputeCPL(ScmClass *klass);
SCM_EXTERN ScmObj Scm_ComputeApplicableMethods(ScmGeneric *gf,
					       ScmObj *args,
					       int nargs);
SCM_EXTERN ScmObj Scm_SortMethods(ScmObj methods, ScmObj *args, int nargs);
SCM_EXTERN ScmObj Scm_MakeNextMethod(ScmGeneric *gf, ScmObj methods,
				     ScmObj *args, int nargs, int copyArgs);
SCM_EXTERN ScmObj Scm_AddMethod(ScmGeneric *gf, ScmMethod *method);

SCM_EXTERN ScmObj Scm_VMSlotRefUsingAccessor(ScmObj obj,
					     ScmSlotAccessor *acc,
					     int boundp);
SCM_EXTERN ScmObj Scm_VMSlotSetUsingAccessor(ScmObj obj,
					     ScmSlotAccessor *acc,
					     ScmObj val);

SCM_EXTERN ScmObj Scm_InstanceSlotRef(ScmObj obj, int number);
SCM_EXTERN void Scm_InstanceSlotSet(ScmObj obj, int number, ScmObj val);

SCM_EXTERN ScmObj Scm__InternalClassName(ScmClass *klass);

SCM_EXTERN ScmGeneric Scm_GenericApplyGeneric;
SCM_EXTERN ScmGeneric Scm_GenericObjectApply;
SCM_EXTERN ScmGeneric Scm_GenericObjectSetter;

#ifdef __cplusplus
}
#endif

#endif /* GAUCHE_CLASS_H */
