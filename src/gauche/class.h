/*
 * class.h - Gauche object system private header
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_CLASS_H
#define GAUCHE_CLASS_H

SCM_DECL_BEGIN

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
    ScmObj schemeGetter;        /* for :virtual slot getter; #f if N/A */
    ScmObj schemeSetter;        /* for :virtual slot setter; #f if N/A */
    ScmObj schemeBoundp;        /* for :virtual slot bound?; #f if N/A */
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

#define SCM_CLASS_SLOT_SPEC(name, getter, setter)               \
    { name, { {SCM_CLASS_STATIC_TAG(Scm_SlotAccessorClass)},    \
              NULL, NULL,                                       \
              (ScmNativeGetterProc)getter,                      \
              (ScmNativeSetterProc)setter,                      \
              SCM_UNBOUND,                                      \
              SCM_FALSE,                                        \
              SCM_FALSE,                                        \
              TRUE, -1,                                         \
              SCM_FALSE, SCM_FALSE, SCM_FALSE,                  \
             } }

#define SCM_CLASS_SLOT_SPEC_END()   { NULL }

/*
 * AccessorMethod
 *  - A special method to be used as a slot accessor
 *    It keeps ScmSlotAccessor in data field, and uses specialized
 *    routine to access the slot.
 */
typedef ScmMethod ScmAccessorMethod;

SCM_CLASS_DECL(Scm_AccessorMethodClass);
#define SCM_CLASS_ACCESSOR_METHOD    (&Scm_AccessorMethodClass)
#define SCM_ACCESSOR_METHOD(obj)     ((ScmAccessorMethod*)obj)
#define SCM_ACCESSOR_METHOD_P(obj)   SCM_ISA(obj, SCM_CLASS_SLOT_ACCESSOR)

/* Instance allocation API
 *   Because of some historical context, the API names are somewhat
 *   confusing.  We have several layer of 'allocate' API.
 *
 *   (1) Scheme 'allocate-instance' generic function and Scm_Allocate().
 *     This is a higher layer.  It takes a class and a plist of initargs.
 *     It uses internal dispatch mechanism to call proper concrete
 *     allocate function, then sets up slots to the sane values.
 *     NB: C Scm_Allocate() only calls "base" method, i.e. the one
 *     through ScmClass->allocate(), and may throw an error if no allocator
 *     is set.  Scheme version may be dispatched to Scheme-defined method.
 *
 *   (2) static *_allocate functions
 *     These are the 'methods' stored in ScmClass->allocate, and does
 *     specific allocation and setup for the class.
 *     Usually calls SCM_NEW_INSTANCE in it.
 *
 *   (3) SCM_NEW_INSTANCE, Scm_NewInstance
 *     The bottom layer.  Allocates memory, and if it's for Scheme
 *     instances, allocates slot vector as well.
 *     The code must always use the macro version SCM_NEW_INSTANCE.
 */


/* cliche in allocate method */
#define SCM_NEW_INSTANCE(klassname, klass) \
    ((klassname*)Scm_NewInstance(klass, sizeof(klassname)))

/* some internal methods */

SCM_EXTERN ScmObj Scm_Allocate(ScmClass *klass, ScmObj initargs);
SCM_EXTERN ScmObj Scm_NewInstance(ScmClass *klass, int coresize);
SCM_EXTERN ScmObj Scm__AllocateAndInitializeInstance(ScmClass *klass,
                                                     ScmObj *inits,
                                                     int numInits,
                                                     u_long flags);
SCM_EXTERN ScmObj Scm_ComputeCPL(ScmClass *klass);
SCM_EXTERN int    Scm_MethodApplicableForClasses(ScmMethod *m,
                                                 ScmClass *types[],
                                                 int nargs);
SCM_EXTERN ScmObj Scm_ComputeApplicableMethods(ScmGeneric *gf,
                                               ScmObj *argv,
                                               int argc,
                                               int applyargs);
SCM_EXTERN ScmObj Scm_SortMethods(ScmObj methods, ScmObj *argv, int argc);
SCM_EXTERN ScmObj Scm_MakeNextMethod(ScmGeneric *gf, ScmObj methods,
                                     ScmObj *argv, int argc,
                                     int copyargs, int applyargs);
SCM_EXTERN ScmObj Scm_AddMethod(ScmGeneric *gf, ScmMethod *method);
SCM_EXTERN ScmObj Scm_DeleteMethod(ScmGeneric *gf, ScmMethod *method);

SCM_EXTERN ScmObj Scm_VMSlotInitializeUsingAccessor(ScmObj obj,
                                                    ScmSlotAccessor *ca,
                                                    ScmObj initargs);
SCM_EXTERN ScmObj Scm_VMSlotRefUsingAccessor(ScmObj obj,
                                             ScmSlotAccessor *acc,
                                             int boundp);
SCM_EXTERN ScmObj Scm_VMSlotSetUsingAccessor(ScmObj obj,
                                             ScmSlotAccessor *acc,
                                             ScmObj val);

SCM_EXTERN ScmObj Scm_VMClassOf(ScmObj obj);
SCM_EXTERN ScmObj Scm_VMIsA(ScmObj obj, ScmClass *klass);

SCM_EXTERN ScmObj Scm_InstanceSlotRef(ScmObj obj, ScmSmallInt k);
SCM_EXTERN void   Scm_InstanceSlotSet(ScmObj obj, ScmSmallInt k, ScmObj val);

SCM_EXTERN void   Scm_StartClassRedefinition(ScmClass *klass);
SCM_EXTERN void   Scm_CommitClassRedefinition(ScmClass *klass, ScmObj newk);
SCM_EXTERN ScmObj Scm_CheckClassBinding(ScmObj name, ScmModule *module);
SCM_EXTERN void   Scm_ReplaceClassBinding(ScmClass *klass, ScmClass *newk);
SCM_EXTERN void   Scm_AddDirectSubclass(ScmClass *super, ScmClass *sub);
SCM_EXTERN void   Scm_RemoveDirectSubclass(ScmClass *super, ScmClass *sub);
SCM_EXTERN void   Scm_AddDirectMethod(ScmClass *super, ScmMethod *m);
SCM_EXTERN void   Scm_RemoveDirectMethod(ScmClass *super, ScmMethod *m);
SCM_EXTERN void   Scm_TransplantInstance(ScmObj src, ScmObj dst);
SCM_EXTERN ScmObj Scm_VMTouchInstance(ScmObj obj);

SCM_EXTERN void   Scm_DeleteDirectSubclass(ScmClass *super, ScmClass *sub);
SCM_EXTERN void   Scm_DeleteDirectMethod(ScmClass *super, ScmMethod *m);

SCM_EXTERN ScmObj Scm__InternalClassName(ScmClass *klass);

SCM_EXTERN ScmGeneric Scm_GenericApplyGeneric;
SCM_EXTERN ScmGeneric Scm_GenericObjectHash;
SCM_EXTERN ScmGeneric Scm_GenericObjectApply;
SCM_EXTERN ScmGeneric Scm_GenericObjectEqualP;
SCM_EXTERN ScmGeneric Scm_GenericObjectSetter;
SCM_EXTERN ScmGeneric Scm_GenericChangeClass;

SCM_EXTERN ScmObj Scm_UpdateDirectMethod(ScmMethod *m,
                                         ScmClass *oldk,
                                         ScmClass *newk);

/* TRANSIENT: Obsoleted. */
SCM_EXTERN ScmObj Scm_ObjectAllocate(ScmClass *klass, ScmObj initargs);
/* TRANSIENT: Obsoleted.  Use SCM_NEW_INSTANCE */
#define SCM_ALLOCATE(klassname, klass)  SCM_NEW_INSTANCE(klassname, klass)
/* TRANSIENT: Obsoleted.  Use Scm_NewInstance*/
SCM_EXTERN ScmObj Scm_AllocateInstance(ScmClass *klass, int coresize);

SCM_DECL_END

#endif /* GAUCHE_CLASS_H */
