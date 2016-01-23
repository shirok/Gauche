/*
 * class.c - class metaobject implementation
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

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"
#include "gauche/code.h"
#include "gauche/priv/builtin-syms.h"
#include "gauche/priv/macroP.h"
#include "gauche/priv/writerP.h"

/* Some routines uses small array on stack to keep data about
   arguments to dispatch.  If the # of args used for dispach is bigger
   than this, the routine allocates an array in heap. */
#define PREALLOC_SIZE  32

/*===================================================================
 * Built-in classes
 */

static void class_print(ScmObj, ScmPort *, ScmWriteContext*);
static void generic_print(ScmObj, ScmPort *, ScmWriteContext*);
static void method_print(ScmObj, ScmPort *, ScmWriteContext*);
static void next_method_print(ScmObj, ScmPort *, ScmWriteContext*);
static void slot_accessor_print(ScmObj, ScmPort *, ScmWriteContext*);
static void accessor_method_print(ScmObj, ScmPort *, ScmWriteContext*);

static ScmObj class_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj generic_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj method_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj slot_accessor_allocate(ScmClass *klass, ScmObj initargs);
static void   initialize_builtin_cpl(ScmClass *klass, ScmObj supers);

static ScmObj instance_class_redefinition(ScmObj obj, ScmClass *old);
static ScmObj slot_set_using_accessor(ScmObj obj, ScmSlotAccessor *sa,
                                      ScmObj val);
static ScmObj instance_allocate(ScmClass *klass, ScmObj initargs);

static int object_compare(ScmObj x, ScmObj y, int equalp);

static ScmObj builtin_initialize(ScmObj *, int, ScmGeneric *);

ScmClass *Scm_DefaultCPL[] = {
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

ScmClass *Scm_ObjectCPL[] = {
    SCM_CLASS_STATIC_PTR(Scm_ObjectClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

static ScmClass *Scm_MethodCPL[] = {
    SCM_CLASS_STATIC_PTR(Scm_MethodClass),
    SCM_CLASS_STATIC_PTR(Scm_ObjectClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

/* Class <top> is the superclass of all classes.  The class initialization
   routine ensures that the class precedence list always terminates by <top>.
   Class <bottom> is the subclass of all classes.  It won't appear in the
   class precedence list, but Scm_SubTypeP treats it specially and answers
   yes to Scm_SubTypeP(<bottom>, <any-class>). */
SCM_DEFINE_ABSTRACT_CLASS(Scm_TopClass, NULL);
SCM_DEFINE_ABSTRACT_CLASS(Scm_BottomClass, NULL);

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_BoolClass, NULL);
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_CharClass, NULL);
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_UnknownClass, NULL);
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_EOFObjectClass, NULL);
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_UndefinedObjectClass, NULL);
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_ForeignPointerClass, NULL);

SCM_DEFINE_BASE_CLASS(Scm_ObjectClass, ScmInstance,
                      NULL, NULL, NULL, instance_allocate,
                      SCM_CLASS_DEFAULT_CPL);

/* Basic metaobjects */
SCM_DEFINE_BASE_CLASS(Scm_ClassClass, ScmClass,
                      class_print, NULL, NULL, class_allocate,
                      SCM_CLASS_OBJECT_CPL);
SCM_DEFINE_BASE_CLASS(Scm_GenericClass, ScmGeneric,
                      generic_print, NULL, NULL, generic_allocate,
                      SCM_CLASS_OBJECT_CPL);
SCM_DEFINE_BASE_CLASS(Scm_MethodClass, ScmMethod,
                      method_print, NULL, NULL, method_allocate,
                      SCM_CLASS_OBJECT_CPL);

/* Internally used classes */
SCM_DEFINE_BUILTIN_CLASS(Scm_SlotAccessorClass,
                         slot_accessor_print, NULL, NULL,
                         slot_accessor_allocate,
                         SCM_CLASS_DEFAULT_CPL);
SCM_DEFINE_BUILTIN_CLASS(Scm_AccessorMethodClass,
                         accessor_method_print, NULL, NULL,
                         method_allocate,
                         Scm_MethodCPL);
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_NextMethodClass, next_method_print);

/* Builtin generic functions */
SCM_DEFINE_GENERIC(Scm_GenericMake, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericAllocate, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericInitialize, builtin_initialize, NULL);
SCM_DEFINE_GENERIC(Scm_GenericAddMethod, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericDeleteMethod, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericComputeCPL, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericComputeSlots, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericComputeGetNSet, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericComputeApplicableMethods, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericUpdateDirectMethod, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericApplyGeneric, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericMethodMoreSpecificP, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericSlotMissing, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericSlotUnbound, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericSlotRefUsingClass, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericSlotSetUsingClass, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericSlotBoundUsingClassP, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericObjectEqualP, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericObjectCompare, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericObjectHash, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericObjectApply, Scm_InvalidApply, NULL);
SCM_DEFINE_GENERIC(Scm_GenericObjectSetter, Scm_InvalidApply, NULL);
SCM_DEFINE_GENERIC(Scm_GenericChangeClass, Scm_NoNextMethod, NULL);

/* Some frequently-used pointers */
static ScmObj key_allocation     = SCM_FALSE;
static ScmObj key_slot_accessor  = SCM_FALSE;
static ScmObj key_builtin        = SCM_FALSE;
static ScmObj key_name           = SCM_FALSE;
static ScmObj key_lambda_list    = SCM_FALSE;
static ScmObj key_generic        = SCM_FALSE;
static ScmObj key_specializers   = SCM_FALSE;
static ScmObj key_body           = SCM_FALSE;

/* A global lock to serialize class redefinition.  We need it since
   class redefinition is not a local effect---it propagates through
   its subclasses.  So it is pretty difficult to guarantee consistency
   if two threads enter the class redefinition, even if they redefine
   different classes.
   This lock works as a recursive lock.  Scm_StartClassRedefinition
   increments the lock count, and Scm_CommitClassRedefinition decrements it.
*/
static struct {
    ScmVM             *owner;   /* thread that grabs the lock, or NULL */
    int               count;
    ScmInternalMutex  mutex;
    ScmInternalCond   cv;
} class_redefinition_lock = { NULL, -1 }; /* we initialize other than zero,
                                             to ensure this sturcture is
                                             placed in the data area */

/* Imporant slots in <class> metaboject can be modified only when the
   class is in 'malleable' state.   Here's the check. */
#define CHECK_MALLEABLE(k, who)                         \
    if (!SCM_CLASS_MALLEABLE_P(k)) {                    \
        Scm_Error("%s: class is not malleable: %S",     \
                  who, SCM_OBJ(k));                     \
    }

/*=====================================================================
 * Auxiliary utilities
 */

static ScmClass **class_list_to_array(ScmObj classes, int len)
{
    ScmObj cp;
    ScmClass **v, **vp;
    v = vp = SCM_NEW_ARRAY(ScmClass*, len+1);
    SCM_FOR_EACH(cp, classes) {
        if (!Scm_TypeP(SCM_CAR(cp), SCM_CLASS_CLASS))
            Scm_Error("list of classes required, but found non-class object"
                      " %S in %S", SCM_CAR(cp), classes);
        *vp++ = SCM_CLASS(SCM_CAR(cp));
    }
    *vp = NULL;
    return v;
}

static ScmObj class_array_to_list(ScmClass **array, int len)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    if (array) while (len-- > 0) SCM_APPEND1(h, t, SCM_OBJ(*array++));
    return h;
}

static ScmObj class_array_to_names(ScmClass **array, int len)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    for (int i=0; i<len; i++, array++) SCM_APPEND1(h, t, (*array)->name);
    return h;
}

/* If the class name has brackets '<' and '>', as in Gauche's convention,
   returns a string without those brackets.  Otherwise returns the class
   name in a string.  This is used by some print method.  Always returns
   a string. */
ScmObj Scm__InternalClassName(ScmClass *klass)
{
    ScmObj name = klass->name;

    if (SCM_SYMBOLP(name)) {
        const ScmStringBody *b = SCM_STRING_BODY(SCM_SYMBOL_NAME(name));
        int size;
        if (((size = SCM_STRING_BODY_SIZE(b)) > 2)
            && SCM_STRING_BODY_START(b)[0] == '<'
            && SCM_STRING_BODY_START(b)[size-1] == '>') {
            return Scm_Substring(SCM_SYMBOL_NAME(name), 1,
                                 SCM_STRING_BODY_LENGTH(b)-1, FALSE);
        } else {
            return SCM_OBJ(SCM_SYMBOL_NAME(name));
        }
    }
    /* Fallback.  At this moment we don't have unnamed classes,
       so this is an ad hoc code.  We may need better handling
       (like write-to-string) later. */
    return SCM_MAKE_STR("(unnamed class)");
}

/*=====================================================================
 * Class metaobject
 */

/* One of the design goals of Gauche object system is to make Scheme-defined
 * class easily accessible from C code, and vice versa.
 *
 * Class is implemented in two layers; Scheme layer and C layer.  The two
 * layers work together to realize efficient MOP.   In the following
 * description, (FOOBAR baz) indicates Scheme call where FooBar(baz) indicates
 * C call.
 *
 * Class instantiation is handled as follows.
 *
 *  (MAKE class . initargs)
 *    If class is a descendant of <class> eventually this calls
 *    a method (MAKE <class> . initargs).
 *
 *  (MAKE <class> . initargs)
 *    Defined in lib/gauche/object.scm.  This calls
 *    (ALLOCATE-INSTANCE <class> initargs), then
 *    (INITIALIZE obj initargs).
 *
 *  (ALLOCATE-INSTANCE <class> <list>)
 *    This is a C-defined method, and calls allocate() below.
 *
 *  static ScmObj allocate(ScmNextMethod *, ScmObj *, int, void*)
 *    The default allocation dispatcher.   This calls class->allocate().
 *    Some builtin function doesn't allow instantiation from Scheme and
 *    sets class->allocate() to NULL; an error is raised in such case.
 *
 *    The class->allocate() function usually allocates the instance
 *    (Scm*** structure) and initializes its slots with reasonable values.
 *    For example, if class is <class>, class->allocate allocates
 *    ScmClass structure.  If the class allows subclassing, class->allocate
 *    must allocate extra storage for as many slots as class->numInstanceSlots.
 *
 *    The allocated and set up structure is returned as ScmObj, which
 *    eventually retured by (ALLOCATE-INSTANCE ...) method, and passed to
 *    (INITIALIZE obj initargs) structure.
 *
 *  (INITIALIZE obj initargs)
 *    In most cases this method is defined in Scheme, if ever defined.
 *    The Scheme method does whatever it want, but it must call
 *    (NEXT-METHOD) in it, and it eventually calls the C-defined fallback
 *    method buildin_initialize().
 *
 *  ScmObj builtin_initialize(ScmObj *, int, ScmGeneric*)
 *    This function traverses the slot accessors, and if the slot has
 *    not been initialized, initialize it as specified in initargs or
 *    slot options.
 */

/* Defining builtin class in C.
 *
 *    Defining classes in C is devided in two steps.  First, you have to
 *    define the static part of the class; it is done by one of the
 *    SCM_DEFINE_***_CLASS macros provided in gauche.h, and it defines
 *    static instance of ScmClass structure.  Then, in the initialization
 *    phase, you have to call Scm_InitStaticClass to initialize the dynamic
 *    part of the structure.
 *
 *      void Scm_InitStaticClass(ScmClass *klass, const char *name,
 *                               ScmModule *mod,
 *                               ScmClassStaticSlotSpec *slots,
 *                               int flags)
 *
 *         This function fills the ScmClass structure that can't be
 *         defined statically, and inserts the binding from the named
 *         symbol to the class object in the specified module.
 *         The 'flags' arg is reserved for future use, and must be 0
 *         for the time being.
 *
 *    See comments in gauche.h (around "Class categories") about
 *    the categories of C-defined classes.
 */

/*
 * Built-in protocols
 *
 *  ScmObj klass->allocate(ScmClass *klass, ScmObj initargs)
 *     Called at the bottom of the chain of allocate-instance method.
 *     Besides allocating the required space, it must initialize
 *     members of the C-specific part of the instance, including SCM_HEADER.
 *     This protocol can be NULL for core base classes; if so, attempt
 *     to "make" such class reports an error.
 *
 *  void klass->print(ScmObj obj, ScmPort *sink, ScmWriteContext *ctx)
 *     OBJ is an instance of klass (you can safely assume it).  This
 *     function should print OBJ into SINK.  See write.c about the
 *     details of the context.
 *     If this function pointer is not set, a default print method
 *     is used.
 *
 *  int klass->compare(ScmObj x, ScmObj y, int equalp)
 *     X and Y are instances of klass.  If equalp is FALSE,
 *     return -1, 0, or 1, when X < Y, X == Y or X > Y, respectively.
 *     In case if klass is not orderable, it can signal an error.
 *     If equalp is TRUE, just test the equality: return -1 if X != Y
 *     and 0 if X == Y.
 *
 *  int klass->serialize(ScmObj obj, ScmPort *sink, ScmObj table)
 *     OBJ is an instance of klass.  This method is only called when OBJ
 *     has not been output in the current serializing session.
 */

/* A note on the 'data' member of ScmClass
 *
 *   It can be used to hang an opaque data to a specific class.  So far,
 *   we use it only for <simple> class mechanism.  Its use is highly
 *   controversial; I mean, The Right Thing is to define a metaclass
 *   which defines an extra member, and allocate <simple> class as an
 *   instance of it.  However, creating metaclass from C is messy now,
 *   so I chose to hack.  In future we may have a nice C API to create
 *   a metaclass, and then we may remove this 'data' member.  So DO NOT
 *   RELY ON ITS EXISTENCE.
 */

/*
 * Class metaobject protocol implementation
 */

/* Allocate class structure.  klass is a metaclass. */
static ScmObj class_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmClass *instance = SCM_NEW_INSTANCE(ScmClass, klass);
    instance->allocate = NULL;  /* will be set when CPL is set */
    instance->print = NULL;
    instance->compare = object_compare;
    instance->serialize = NULL; /* class_serialize? */
    instance->cpa = NULL;
    instance->numInstanceSlots = 0; /* will be adjusted in class init */
    instance->coreSize = 0;     /* will be set when CPL is set */
    instance->flags = SCM_CLASS_SCHEME|SCM_CLASS_MALLEABLE; /* default */
    instance->name = SCM_FALSE;
    instance->directSupers = SCM_NIL;
    instance->accessors = SCM_NIL;
    instance->cpl = SCM_NIL;
    instance->directSlots = SCM_NIL;
    instance->slots = SCM_NIL;
    instance->directSubclasses = SCM_NIL;
    instance->directMethods = SCM_NIL;
    instance->initargs = SCM_NIL;
    instance->modules = SCM_NIL;
    instance->redefined = SCM_FALSE;
    (void)SCM_INTERNAL_MUTEX_INIT(instance->mutex);
    (void)SCM_INTERNAL_COND_INIT(instance->cv);
    instance->data = NULL;      /* see the above note on the 'data' member */
    return SCM_OBJ(instance);
}

static void class_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<class %A%s>",
               SCM_CLASS(obj)->name,
               (SCM_FALSEP(SCM_CLASS(obj)->redefined)? "" : " (redefined)"));
}

/*
 * (make <class> ...)   - default method to make a class instance.
 */

/* defined in Scheme */

/*
 * (allocate-instance <class> initargs)
 */
ScmObj Scm_Allocate(ScmClass *c, ScmObj initargs)
{
    if (c->allocate == NULL) {
        Scm_Error("built-in class can't be allocated via allocate-instance: %S",
                  SCM_OBJ(c));
    }
    return c->allocate(c, initargs);
}

static ScmObj allocate(ScmNextMethod *nm, ScmObj *argv, int argc, void *d)
{
    return Scm_Allocate(SCM_CLASS(argv[0]), argv[1]);
}

static ScmClass *class_allocate_SPEC[] = {
    SCM_CLASS_STATIC_PTR(Scm_ClassClass), SCM_CLASS_STATIC_PTR(Scm_ListClass)
};
static SCM_DEFINE_METHOD(class_allocate_rec, &Scm_GenericAllocate,
                         2, 0, class_allocate_SPEC, allocate, NULL);

/*
 * (compute-cpl <class>)
 */
static ScmObj class_compute_cpl(ScmNextMethod *nm, ScmObj *argv, int argc,
                                void *d)
{
    ScmClass *c = SCM_CLASS(argv[0]);
    return Scm_ComputeCPL(c);
}

static ScmClass *class_compute_cpl_SPEC[] = {
    SCM_CLASS_STATIC_PTR(Scm_ClassClass)
};
static SCM_DEFINE_METHOD(class_compute_cpl_rec, &Scm_GenericComputeCPL,
                         1, 0, class_compute_cpl_SPEC,
                         class_compute_cpl, NULL);

/*
 * (class-of obj)
 */

ScmClass *Scm_ClassOf(ScmObj obj)
{
    if (!SCM_PTRP(obj)) {
        if (SCM_TRUEP(obj) || SCM_FALSEP(obj)) return SCM_CLASS_BOOL;
        if (SCM_NULLP(obj)) return SCM_CLASS_NULL;
        if (SCM_CHARP(obj)) return SCM_CLASS_CHAR;
        if (SCM_INTP(obj))  return SCM_CLASS_INTEGER;
        if (SCM_EOFP(obj))  return SCM_CLASS_EOF_OBJECT;
        if (SCM_UNDEFINEDP(obj)) return SCM_CLASS_UNDEFINED_OBJECT;
        else return SCM_CLASS_UNKNOWN;
    }
    if (SCM_FLONUMP(obj)) return SCM_CLASS_REAL;
    /* check lazy pair first, so that we won't trigger forcing. */
    if (SCM_LAZY_PAIR_P(obj)||SCM_PAIRP(obj)) return SCM_CLASS_PAIR;
    return SCM_CLASS_OF(obj);
}

/* Returns the pointer of the first base class found in the given
   class's CPA.  If the class is pure abstract or builtin, NULL is
   returned. */
ScmClass *Scm_BaseClassOf(ScmClass *klass)
{
    ScmClass **cp = klass->cpa;
    ScmClass *k;
    while ((k = *cp++) != NULL) {
        if (SCM_CLASS_CATEGORY(k) == SCM_CLASS_BASE) return k;
    }
    return NULL;
}

/*
 * (class-of obj class)
 *   - if obj's class is redefined, first updates obj.
 */
ScmObj class_of_cc(ScmObj result, void **data)
{
    return Scm_VMClassOf(result);
}

ScmObj Scm_VMClassOf(ScmObj obj)
{
    ScmClass *k = Scm_ClassOf(obj);
    if (!SCM_FALSEP(k->redefined)) {
        Scm_VMPushCC(class_of_cc, NULL, 0);
        return instance_class_redefinition(obj, k);
    }
    return SCM_OBJ(k);
}

/*
 * (is-a? obj class)
 *   - if obj's class is redefined, first updates obj.
 */
ScmObj is_a_cc(ScmObj result, void **data)
{
    return Scm_VMIsA(SCM_OBJ(data[0]), SCM_CLASS(data[1]));
}

ScmObj Scm_VMIsA(ScmObj obj, ScmClass *klass)
{
    ScmClass *k = Scm_ClassOf(obj);
    if (!SCM_FALSEP(k->redefined)) {
        void *data[2];
        data[0] = obj;
        data[1] = klass;
        Scm_VMPushCC(is_a_cc, data, 2);
        return instance_class_redefinition(obj, k);
    }
    return SCM_MAKE_BOOL(Scm_TypeP(obj, klass));
}

/*
 * Setting/resetting SCM_CLASS_MALLEABLE flag
 */
void Scm_ClassMalleableSet(ScmClass *klass, int flag)
{
    if (SCM_CLASS_CATEGORY(klass) != SCM_CLASS_SCHEME) {
        Scm_Error("You cannot modify malleable flag of a class not defined in Scheme: %S", SCM_OBJ(klass));
    }
    if (flag) {
        klass->flags |= SCM_CLASS_MALLEABLE;
    } else {
        klass->flags &= ~SCM_CLASS_MALLEABLE;
    }
}

/*--------------------------------------------------------------
 * Metainformation accessors
 */

/* TODO: disable modification of system-builtin classes */

static ScmObj class_name(ScmClass *klass)
{
    return klass->name;
}

static void class_name_set(ScmClass *klass, ScmObj val)
{
    CHECK_MALLEABLE(klass, "(setter name)");
    klass->name = val;
}

static ScmObj class_cpl(ScmClass *klass)
{
    return klass->cpl;
}

/* Subroutine for class_cpl_set.  Scans KLASS's CPL and find out the
   suitable allocator function, C-struct core size, and some flags.
   If KLASS inherits more than one C-defined classes (BASEs), they must
   form a single inheritance chain. */
static void find_core_allocator(ScmClass *klass)
{
    ScmClass *b = NULL; /* the base class klass gets the allocate func */
    int object_inherited = FALSE;

    klass->allocate = NULL;
    for (ScmClass **p = klass->cpa; *p; p++) {
        if (SCM_CLASS_CATEGORY(*p) == SCM_CLASS_BUILTIN) {
            Scm_Error("class '%S' attempted to inherit from a builtin class "
                      "%S; you cannot subclass a builtin class.",
                      klass->name, *p);
        }

        if ((*p)->allocate == instance_allocate) {
            /* Check if we certainly inherited <object> */
            object_inherited = TRUE;
            continue;
        }

        if ((*p)->flags & SCM_CLASS_APPLICABLE) {
            klass->flags |= SCM_CLASS_APPLICABLE;
        }

        if (b
            && SCM_CLASS_CATEGORY(*p) == SCM_CLASS_BASE
            && b->allocate != (*p)->allocate) {
            /* found different C-defined class.  check to see if (*p) is
               superclass of b.  If not, the single-inheritance rule is
               violated. */
            ScmClass **bp = b->cpa;
            for (; *bp; bp++) {
                if (*bp == *p) break;
            }
            if (!*bp) {
                Scm_Error("class '%S' attempted to inherit multiple C-defined "
                          "base class (%S and %S) which are not in a "
                          "superclass-subclass relathionship.",
                          klass->name, b, *p);
            }
            continue;
        }
        if (!b) {
            /* Here we found the closest C-base class.  Get the allocator
               from it. */
            b = *p;
            klass->allocate = b->allocate;
            klass->coreSize = b->coreSize;
        }
    }

    if (!object_inherited) {
        Scm_Error("class %S's precedence list doesn't have a base class: %S",
                  klass->name, klass->cpl);
    }
    if (!klass->allocate) {
        klass->allocate = instance_allocate;
        klass->coreSize = sizeof(ScmInstance);
    }
}

static void class_cpl_set(ScmClass *klass, ScmObj val)
{
    CHECK_MALLEABLE(klass, "(setter cpl)");

    /* We make sure things are consistent. */
    if (!SCM_PAIRP(val)) goto err;
    /* check if the CPL begins with the class itself. */
    if (SCM_CAR(val) != SCM_OBJ(klass)) goto err;

    /* set up the cpa */
    ScmObj cp = SCM_CDR(val);
    int len = Scm_Length(cp);
    if (len < 0) goto err;
    klass->cpa = class_list_to_array(cp, len);
    for (int i=0; i<len; i++) {
        /* sanity check */
        if (klass->cpa[i] == SCM_CLASS_BOTTOM) goto err;
    }
    if (klass->cpa[len-1] != SCM_CLASS_TOP) goto err;
    klass->cpl = Scm_CopyList(val);
    /* find correct allocation method */
    find_core_allocator(klass);
    return;
  err:
    Scm_Error("class precedence list must be a proper list of class "
              "metaobject, beginning from the class itself owing the list, "
              "and ending by the class <top>, and must not include <bottom>: "
              "%S", val);
}

static ScmObj class_direct_supers(ScmClass *klass)
{
    return klass->directSupers;
}

static void class_direct_supers_set(ScmClass *klass, ScmObj val)
{
    CHECK_MALLEABLE(klass, "(setter direct-supers)")
    ScmObj vp;
    SCM_FOR_EACH(vp, val) {
        if (!Scm_TypeP(SCM_CAR(vp), SCM_CLASS_CLASS))
            Scm_Error("non-class object found in direct superclass list: %S",
                      SCM_CAR(vp));
    }
    klass->directSupers = val;
}

static ScmObj class_direct_slots(ScmClass *klass)
{
    return klass->directSlots;
}

static void class_direct_slots_set(ScmClass *klass, ScmObj val)
{
    CHECK_MALLEABLE(klass, "(setter direct-slots)");
    ScmObj vp;
    SCM_FOR_EACH(vp, val) {
        if (!SCM_PAIRP(SCM_CAR(vp)))
            Scm_Error("bad slot spec found in direct slot list: %S",
                      SCM_CAR(vp));
    }
    klass->directSlots = val;
}

static ScmObj class_slots_ref(ScmClass *klass)
{
    return klass->slots;
}

static void class_slots_set(ScmClass *klass, ScmObj val)
{
    CHECK_MALLEABLE(klass, "(setter slots)");
    ScmObj vp;
    SCM_FOR_EACH(vp, val) {
        if (!SCM_PAIRP(SCM_CAR(vp)))
            Scm_Error("bad slot spec found in slot list: %S",
                      SCM_CAR(vp));
    }
    klass->slots = val;
}

static ScmObj class_accessors(ScmClass *klass)
{
    return klass->accessors;
}

static void class_accessors_set(ScmClass *klass, ScmObj val)
{
    CHECK_MALLEABLE(klass, "(setter accessors)");
    ScmObj vp;
    SCM_FOR_EACH(vp, val) {
        if (!SCM_PAIRP(SCM_CAR(vp))
            || !SCM_SLOT_ACCESSOR_P(SCM_CDAR(vp)))
            Scm_Error("slot accessor list must be an assoc-list of slot name and slot accessor object, but found: %S",
                      SCM_CAR(vp));
    }
    klass->accessors = val;
}

static ScmObj class_numislots(ScmClass *klass)
{
    return Scm_MakeInteger(klass->numInstanceSlots);
}

static void class_numislots_set(ScmClass *klass, ScmObj snf)
{
    CHECK_MALLEABLE(klass, "(setter num-instance-slots)");
    int nf = 0;
    if (!SCM_INTP(snf) || (nf = SCM_INT_VALUE(snf)) < 0) {
        Scm_Error("invalid argument: %S", snf);
        /*NOTREACHED*/
    }
    klass->numInstanceSlots = nf;
}

static ScmObj class_category(ScmClass *klass)
{
    switch (SCM_CLASS_CATEGORY(klass)) {
    case SCM_CLASS_BUILTIN:  return SCM_SYM_BUILTIN;
    case SCM_CLASS_ABSTRACT: return SCM_SYM_ABSTRACT;
    case SCM_CLASS_BASE:     return SCM_SYM_BASE;
    default:                 return SCM_SYM_SCHEME;
    }
}

static ScmObj class_initargs(ScmClass *klass)
{
    return klass->initargs;
}

static void class_initargs_set(ScmClass *klass, ScmObj val)
{
    CHECK_MALLEABLE(klass, "(setter initargs)");
    int len = Scm_Length(val);
    if (len < 0 || len%2 != 0) {
        Scm_Error("class-initargs must be a list of even number of elements, but got %S", val);
    }
    klass->initargs = val;
}

static ScmObj class_defined_modules(ScmClass *klass)
{
    return klass->modules;
}

static void class_defined_modules_set(ScmClass *klass, ScmObj val)
{
    CHECK_MALLEABLE(klass, "(setter defined-modules)");
    ScmObj cp;
    SCM_FOR_EACH(cp, val) {
        if (!SCM_MODULEP(SCM_CAR(cp))) goto err;
    }
    if (!SCM_NULLP(cp)) goto err;
    klass->modules = val;
    return;
  err:
    Scm_Error("list of modules required, bot got %S", val);
}

/*
 * The following slots should only be modified by a special MT-safe procedures.
 */
static ScmObj class_direct_subclasses(ScmClass *klass)
{
    return klass->directSubclasses;
}

static ScmObj class_direct_methods(ScmClass *klass)
{
    return klass->directMethods;
}

static ScmObj class_redefined(ScmClass *klass)
{
    int abandoned = FALSE;

    /* If this class is being redefined by other thread, you should wait */
    (void)SCM_INTERNAL_MUTEX_LOCK(klass->mutex);
    while (SCM_VMP(klass->redefined)) {
        if (SCM_VM(klass->redefined)->state == SCM_VM_TERMINATED) {
            /* TODO: this means redefinition of klass has been abandoned,
               so the state of klass may be inconsistent.  Should we do
               something to it? */
            abandoned = TRUE;
            klass->redefined = SCM_FALSE;
        } else {
            (void)SCM_INTERNAL_COND_WAIT(klass->cv, klass->mutex);
        }
    }
    ScmObj r = klass->redefined;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(klass->mutex);
    if (abandoned) {
        Scm_Warn("redefinition of class %S has been abandoned", klass);
    }
    return r;
}

/*--------------------------------------------------------------
 * Implicit metaclass
 */
/* This function does the equivalent to
 *  (make <class> :name NAME :supers (list <class>))
 */

static ScmClass *make_implicit_meta(const char *name,
                                    ScmClass **cpa,
                                    ScmModule *mod)
{
    ScmClass *meta = (ScmClass*)class_allocate(SCM_CLASS_CLASS, SCM_NIL);
    ScmObj s = SCM_INTERN(name);
    static ScmClass *metacpa[] = { SCM_CLASS_CLASS, SCM_CLASS_OBJECT, SCM_CLASS_TOP, NULL };
    ScmClass **metas = metacpa;

    /* check to see if parent class has also metaclass, and if so,
       adds it to the CPA.  We know all the builtin classes use
       single inheritance, so the CPA calculation should be straightforward.
       Note that this assumes the parent classes are already initialized.
    */
    {
        ScmClass **parent;
        int numExtraMetas = 0, i;
        for (parent = cpa; *parent; parent++) {
            if (SCM_CLASS_OF(*parent) != SCM_CLASS_CLASS) {
                numExtraMetas++;
            }
        }
        if (numExtraMetas) {
            metas = SCM_NEW_ARRAY(ScmClass*, numExtraMetas+4);
            for (i = 0, parent = cpa; *parent; parent++) {
                if (SCM_CLASS_OF(*parent) != SCM_CLASS_CLASS) {
                    metas[i++] = SCM_CLASS_OF(*parent);
                }
            }
            metas[i++] = SCM_CLASS_CLASS;
            metas[i++] = SCM_CLASS_OBJECT;
            metas[i++] = SCM_CLASS_TOP;
            metas[i] = NULL;
        }
    }

    meta->name = s;
    meta->allocate = class_allocate;
    meta->print = class_print;
    meta->cpa = metas;
    meta->flags = SCM_CLASS_ABSTRACT;
    initialize_builtin_cpl(meta, SCM_FALSE);
    Scm_Define(mod, SCM_SYMBOL(s), SCM_OBJ(meta));
    meta->slots = Scm_ClassClass.slots;
    meta->accessors = Scm_ClassClass.accessors;
    return meta;
}

/*--------------------------------------------------------------
 * External interface
 */

int Scm_SubtypeP(ScmClass *sub, ScmClass *type)
{
    if (sub == type) return TRUE;
    if (sub == SCM_CLASS_BOTTOM) return TRUE;

    ScmClass **p = sub->cpa;
    while (*p) {
        if (*p++ == type) return TRUE;
    }
    return FALSE;
}

int Scm_TypeP(ScmObj obj, ScmClass *type)
{
    return Scm_SubtypeP(Scm_ClassOf(obj), type);
}

/*
 * compute-cpl
 */
ScmObj Scm_ComputeCPL(ScmClass *klass)
{
    ScmObj seqh = SCM_NIL, seqt = SCM_NIL;

    /* a trick to ensure we have <object> <top> at the end of CPL. */
    ScmObj ds = Scm_Delete(SCM_OBJ(SCM_CLASS_OBJECT), klass->directSupers,
                           SCM_CMP_EQ);
    ds = Scm_Delete(SCM_OBJ(SCM_CLASS_TOP), ds, SCM_CMP_EQ);
    ds = Scm_Append2(ds, SCM_LIST1(SCM_OBJ(SCM_CLASS_OBJECT)));

    ScmObj dp;
    SCM_FOR_EACH(dp, klass->directSupers) {
        if (!Scm_TypeP(SCM_CAR(dp), SCM_CLASS_CLASS))
            Scm_Error("non-class found in direct superclass list: %S",
                      klass->directSupers);
        if (SCM_CAR(dp) == SCM_OBJ(SCM_CLASS_OBJECT)
            || SCM_CAR(dp) == SCM_OBJ(SCM_CLASS_TOP))
            continue;
        SCM_APPEND1(seqh, seqt, SCM_CLASS(SCM_CAR(dp))->cpl);
    }
    SCM_APPEND1(seqh, seqt, Scm_ObjectClass.cpl);

    SCM_APPEND1(seqh, seqt, ds);

    ScmObj result = Scm_MonotonicMerge1(seqh);
    if (SCM_FALSEP(result))
        Scm_Error("discrepancy found in class precedence lists of the superclasses: %S",
                  klass->directSupers);
    return Scm_Cons(SCM_OBJ(klass), result);
}

/*
 * Internal procedures for class redefinition
 */

/* global lock manipulation */
static void lock_class_redefinition(ScmVM *vm)
{
    ScmVM *stolefrom = NULL;
    if (class_redefinition_lock.owner == vm) {
        class_redefinition_lock.count++;
    } else {
        (void)SCM_INTERNAL_MUTEX_LOCK(class_redefinition_lock.mutex);
        while (class_redefinition_lock.owner != vm) {
            if (class_redefinition_lock.owner == NULL) {
                class_redefinition_lock.owner = vm;
            } else if (class_redefinition_lock.owner->state
                       == SCM_VM_TERMINATED) {
                stolefrom = class_redefinition_lock.owner;
                class_redefinition_lock.owner = vm;
            } else {
                (void)SCM_INTERNAL_COND_WAIT(class_redefinition_lock.cv,
                                             class_redefinition_lock.mutex);
            }
        }
        (void)SCM_INTERNAL_MUTEX_UNLOCK(class_redefinition_lock.mutex);
        if (stolefrom) {
            Scm_Warn("a thread holding class redefinition lock has been terminated: %S", stolefrom);
        }
        class_redefinition_lock.count = 1;
    }
}

static void unlock_class_redefinition(ScmVM *vm)
{
    if (class_redefinition_lock.owner != vm) return;
    if (--class_redefinition_lock.count <= 0) {
        (void)SCM_INTERNAL_MUTEX_LOCK(class_redefinition_lock.mutex);
        (void)SCM_INTERNAL_COND_BROADCAST(class_redefinition_lock.cv);
        class_redefinition_lock.owner = NULL;
        (void)SCM_INTERNAL_MUTEX_UNLOCK(class_redefinition_lock.mutex);
    }
}

/* %start-class-redefinition klass */
void Scm_StartClassRedefinition(ScmClass *klass)
{
    if (SCM_CLASS_CATEGORY(klass) != SCM_CLASS_SCHEME) {
        Scm_Error("cannot redefine class %S, which is not a Scheme-defined class", klass);
    }
    ScmVM *vm = Scm_VM();

    /* First, acquire the global lock. */
    lock_class_redefinition(vm);

    /* Mark this class to be redefined. */
    int success = FALSE;
    (void)SCM_INTERNAL_MUTEX_LOCK(klass->mutex);
    if (SCM_FALSEP(klass->redefined)) {
        klass->redefined = SCM_OBJ(vm);
        success = TRUE;
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(klass->mutex);

    if (!success) {
        unlock_class_redefinition(vm);
        Scm_Error("class %S seems abandoned during class redefinition", klass);
    }

    /* Allow modification of important slots */
    Scm_ClassMalleableSet(klass, TRUE);
}

/* %commit-class-redefinition klass newklass */
void Scm_CommitClassRedefinition(ScmClass *klass, ScmObj newklass)
{
    if (SCM_CLASS_CATEGORY(klass) != SCM_CLASS_SCHEME) return;
    if (!SCM_FALSEP(newklass)&&!SCM_CLASSP(newklass)) {
        Scm_Error("class or #f required, but got %S", newklass);
    }

    ScmVM *vm = Scm_VM();

    /* Release the lock of the class.
       We execute this regardless of class_redefinition_lock.owner.
       Theoretically, this procedure shouldn't be called unless the thread
       owns global class_redefinition_lock.  However, we don't require it,
       so that this procedure can be used for a program to exit from
       obscure state. */
    (void)SCM_INTERNAL_MUTEX_LOCK(klass->mutex);
    if (SCM_EQ(klass->redefined, SCM_OBJ(vm))) {
        Scm_ClassMalleableSet(klass, FALSE); /* disable modification */
        klass->redefined = newklass;
        (void)SCM_INTERNAL_COND_BROADCAST(klass->cv);
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(klass->mutex);

    /* Decrement the recursive global lock. */
    unlock_class_redefinition(vm);
}

/* %check-class-binding name module
   See the bindings of name in module, and iff it is bound to a class,
   returns the class; otherwise returns #f. */
ScmObj Scm_CheckClassBinding(ScmObj name, ScmModule *module)
{
    if (!SCM_SYMBOLP(name)) return SCM_FALSE;
    ScmObj v = Scm_GlobalVariableRef(module, SCM_SYMBOL(name), 0);
    return SCM_CLASSP(v) ? v : SCM_FALSE;
}

/* %replace-class-binding! klass newklass
   Called when a descendant of klass is redefined.  If klass has a global
   binding, replace it to newklass. */
void Scm_ReplaceClassBinding(ScmClass *klass, ScmClass *newklass)
{
    if (!SCM_SYMBOLP(klass->name)) return;
    ScmObj cp;
    SCM_FOR_EACH(cp, klass->modules) {
        if (!SCM_MODULEP(SCM_CAR(cp))) continue;
        Scm_Define(SCM_MODULE(SCM_CAR(cp)),
                   SCM_SYMBOL(klass->name),
                   SCM_OBJ(newklass));
    }
}

/* %add-direct-subclass! super sub */
void Scm_AddDirectSubclass(ScmClass *super, ScmClass *sub)
{
    if (SCM_CLASS_CATEGORY(super) == SCM_CLASS_SCHEME) {
        ScmObj p = Scm_Cons(SCM_OBJ(sub), SCM_NIL);
        (void)SCM_INTERNAL_MUTEX_LOCK(super->mutex);
        /* avoid duplication */
        if (SCM_FALSEP(Scm_Memq(super->directSubclasses, SCM_OBJ(sub)))) {
            SCM_SET_CDR(p, super->directSubclasses);
            super->directSubclasses = p;
        }
        (void)SCM_INTERNAL_MUTEX_UNLOCK(super->mutex);
    }
}

/* %delete-direct-subclass! super sub */
void Scm_DeleteDirectSubclass(ScmClass *super, ScmClass *sub)
{
    if (SCM_CLASS_CATEGORY(super) == SCM_CLASS_SCHEME) {
        (void)SCM_INTERNAL_MUTEX_LOCK(super->mutex);
        super->directSubclasses =
            Scm_DeleteX(SCM_OBJ(sub), super->directSubclasses, SCM_CMP_EQ);
        (void)SCM_INTERNAL_MUTEX_UNLOCK(super->mutex);
    }
}

/* %add-direct-method! super method
   method can be added or deleted freely, so we don't check malleablility. */
void Scm_AddDirectMethod(ScmClass *super, ScmMethod *m)
{
    if (SCM_CLASS_CATEGORY(super) == SCM_CLASS_SCHEME) {
        ScmObj p = Scm_Cons(SCM_OBJ(m), SCM_NIL);
        (void)SCM_INTERNAL_MUTEX_LOCK(super->mutex);
        /* avoid duplication */
        if (SCM_FALSEP(Scm_Memq(super->directMethods, SCM_OBJ(m)))) {
            SCM_SET_CDR(p, super->directMethods);
            super->directMethods = p;
        }
        (void)SCM_INTERNAL_MUTEX_UNLOCK(super->mutex);
    }
}

/* %delete-direct-method! super method
   method can be added or deleted freely, so we don't check malleablility. */
void Scm_DeleteDirectMethod(ScmClass *super, ScmMethod *m)
{
    if (SCM_CLASS_CATEGORY(super) == SCM_CLASS_SCHEME) {
        (void)SCM_INTERNAL_MUTEX_LOCK(super->mutex);
        super->directMethods =
            Scm_DeleteX(SCM_OBJ(m), super->directMethods, SCM_CMP_EQ);
        (void)SCM_INTERNAL_MUTEX_UNLOCK(super->mutex);
    }
}

/* %transplant-instance! src dst */
/* Copies the contents of the core structure pointed by src over
   the contents of dst.  The contents of dst is destroyed.  This
   astonishingly dangerous operation has to be done at the last stage
   of change-class, in order to keep the identity of the instance
   being updated.

   Note that this procedure doesn't overwrite the Scheme slot
   vectors. */
void Scm_TransplantInstance(ScmObj src, ScmObj dst)
{
    ScmClass *srcklass = Scm_ClassOf(src);
    ScmClass *dstklass = Scm_ClassOf(dst);
    ScmClass *base;

    /* Extra check.  We can't transplant the contents to different
       an instance that has different base class. */
    if ((base = Scm_BaseClassOf(srcklass)) == NULL
        || !SCM_EQ(base, Scm_BaseClassOf(dstklass))) {
        Scm_Error("%%transplant-instance: classes are incompatible between %S and %S",
                  src, dst);
    }
    if (base->coreSize < (int)sizeof(ScmInstance)) {
        Scm_Error("%%transplant-instance: baseclass is too small (implementation error?)");
    }
    memcpy(dst, src, base->coreSize);
}

/* touch-instance! obj
 * If obj's class is redefined, update obj.  Otherwise it does nothing.
 * Handy to ensure obj is in the newest state.  Returns obj.
 */
ScmObj Scm_VMTouchInstance(ScmObj obj)
{
    ScmClass *klass = Scm_ClassOf(obj);
    if (!SCM_FALSEP(klass->redefined)) {
        return instance_class_redefinition(obj, klass);
    }
    return obj;
}

/*=====================================================================
 * Scheme slot access
 */

/* Scheme slots are stored in ScmObj array pointed by slots field
 * of ScmInstance.  This one-level indirection allows an instance
 * to be redefined.
 */

/* Unbound slot: if the slot value yields either SCM_UNBOUND or
 * SCM_UNDEFINED, a generic function slot-unbound is called.
 * We count SCM_UNDEFINED as unbound so that a Scheme program can
 * make slot unbound, especially needed for procedural slots.
 */

/* A common routine to be used to allocate object.
   Coresize should be a size of base C structure in bytes.
   Klass may be a subclass.   If klass is inheritable by Scheme
   (i.e. it's category is either SCM_CLASS_BASE or SCM_CLASS_SCHEME),
   This routine also allocates a slot vector, and initializes the
   slot vector with SCM_UNBOUND.
   We don't care class redefinition at this point.  If the class is
   redefined simultaneously, it will be handled by the subsequent initialize
   method.
*/
ScmObj Scm_NewInstance(ScmClass *klass, int coresize)
{
    ScmObj obj = SCM_NEW2(ScmObj, coresize);

    if (SCM_CLASS_CATEGORY(klass) == SCM_CLASS_BASE
        || SCM_CLASS_CATEGORY(klass) == SCM_CLASS_SCHEME) {
        ScmObj *slots = SCM_NEW_ARRAY(ScmObj, klass->numInstanceSlots);

        /* NB: actually, for Scheme instances, 'coresize' argument is
           redundant since klass->coreSize has it.  There's a historical
           confusion in the class protocol.  We should clear it out someday.
        */
        if (coresize != klass->coreSize) {
            Scm_Printf(SCM_CURERR, "WARNING: allocating instance of class %S: coresize argument %d doesn't match the class definition's (%d)\n", klass, coresize, klass->coreSize);
        }

        for (int i=0; i<klass->numInstanceSlots; i++) {
            slots[i] = SCM_UNBOUND;
        }
        SCM_INSTANCE(obj)->slots = slots;
    }
    SCM_SET_CLASS(obj, klass);
    return obj;
}

/* TRANSIENT: For the binary compatibility.  Will go on 1.0. */
ScmObj Scm_AllocateInstance(ScmClass *klass, int coresize)
{
    return Scm_NewInstance(klass, coresize);
}


/* A special procedure that shortcuts allocate-instance and initialize
 * slots directly.
 * This is mainly used for fast construction of records.  This bypasses
 * normal MOP initialization steps, so this shouldn't be used casually.
 */
ScmObj Scm__AllocateAndInitializeInstance(ScmClass *klass,
                                          ScmObj *inits, int numInits,
                                          u_long flags /*reserved*/)
{
    if (SCM_CLASS_CATEGORY(klass) != SCM_CLASS_BASE
        && SCM_CLASS_CATEGORY(klass) != SCM_CLASS_SCHEME) {
        Scm_Error("Scm_AllocateAndInitializeInstance can't be called on "
                  "this class: %S", SCM_OBJ(klass));
    }

    /* We allocate an instance and a slot vector at once for speed.
       It is reasonable optimization, since record classes won't be redefined,
       and the instances' slot vector will never be replaced.
       We may provide an another mode to allocate the slot vector separately,
       using FLAGS argument. */
    int corewords = (klass->coreSize + sizeof(ScmObj)-1)/sizeof(ScmObj);
    ScmObj obj = SCM_NEW2(ScmObj, (corewords+klass->numInstanceSlots)*sizeof(ScmObj));
    SCM_SET_CLASS(obj, klass);
    ScmObj *slots = ((ScmObj*)obj) + corewords;
    for (int i=0; i<klass->numInstanceSlots; i++) {
        if (i < numInits) slots[i] = inits[i];
        else slots[i] = SCM_UNBOUND;
    }
    SCM_INSTANCE(obj)->slots = slots;
    return obj;
}

/* Invoke class redefinition method */
static ScmObj instance_class_redefinition(ScmObj obj, ScmClass *old)
{
    (void)SCM_INTERNAL_MUTEX_LOCK(old->mutex);
    while (!SCM_ISA(old->redefined, SCM_CLASS_CLASS)) {
        (void)SCM_INTERNAL_COND_WAIT(old->cv, old->mutex);
    }
    ScmObj newc = old->redefined;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(old->mutex);
    if (SCM_CLASSP(newc)) {
        return Scm_VMApply2(SCM_OBJ(&Scm_GenericChangeClass), obj, newc);
    } else {
        return SCM_OBJ(old);
    }
}

/* most primitive internal accessor */
static inline ScmObj scheme_slot_ref(ScmObj obj, ScmSmallInt number)
{
    ScmClass *k = Scm_ClassOf(obj);
    if (number < 0 || number >= k->numInstanceSlots)
        Scm_Error("instance slot index %ld out of bounds for %S", number, obj);
    return SCM_INSTANCE_SLOTS(obj)[number];
}

static inline void scheme_slot_set(ScmObj obj, ScmSmallInt number, ScmObj val)
{
    ScmClass *k = Scm_ClassOf(obj);
    if (number < 0 || number >= k->numInstanceSlots)
        Scm_Error("instance slot index %ld out of bounds for %S", number, obj);
    SCM_INSTANCE_SLOTS(obj)[number] = val;
}

/* These two are exposed to Scheme to do some nasty things.
   We shoudn't do class redefinition check here, since the slot number
   is calculated based on the old class, if the class is ever redefined.
*/
ScmObj Scm_InstanceSlotRef(ScmObj obj, ScmSmallInt number)
{
    return scheme_slot_ref(obj, number);
}

void Scm_InstanceSlotSet(ScmObj obj, ScmSmallInt number, ScmObj val)
{
    scheme_slot_set(obj, number, val);
}

/* Initialize a slot according to its accessor spec
   TODO: class redefintion check
*/
static ScmObj slot_initialize_cc(ScmObj result, void **data)
{
    ScmObj obj = data[0];
    ScmSlotAccessor *sa = SCM_SLOT_ACCESSOR(data[1]);
    return slot_set_using_accessor(obj, sa, result);
}

ScmObj Scm_VMSlotInitializeUsingAccessor(ScmObj obj,
                                         ScmSlotAccessor *sa,
                                         ScmObj initargs)
{
    /* (1) see if we have init-keyword */
    if (SCM_KEYWORDP(sa->initKeyword)) {
        ScmObj v = Scm_GetKeyword(sa->initKeyword, initargs, SCM_UNDEFINED);
        if (!SCM_UNDEFINEDP(v)) {
            return slot_set_using_accessor(obj, sa, v);
        }
    }
    /* (2) use init-value or init-thunk, if this slot is initializable. */
    if (sa->initializable) {
        if (!SCM_UNBOUNDP(sa->initValue)) {
            return slot_set_using_accessor(obj, sa, sa->initValue);
        }
        if (SCM_PROCEDUREP(sa->initThunk)) {
            void *data[2];
            data[0] = (void*)obj;
            data[1] = (void*)sa;
            Scm_VMPushCC(slot_initialize_cc, data, 2);
            return Scm_VMApply(sa->initThunk, SCM_NIL);
        }
    }
    return SCM_UNDEFINED;
}

/*-------------------------------------------------------------------
 * slot-ref, slot-set! and families
 */

/* helper macros */
#define SLOT_UNBOUND(klass, obj, slot)                  \
    Scm_VMApply(SCM_OBJ(&Scm_GenericSlotUnbound),       \
                SCM_LIST3(SCM_OBJ(klass), obj, slot))

#define SLOT_MISSING3(klass, obj, slot)                 \
    Scm_VMApply(SCM_OBJ(&Scm_GenericSlotMissing),       \
                SCM_LIST3(SCM_OBJ(klass), obj, slot))

#define SLOT_MISSING4(klass, obj, slot, val)            \
    Scm_VMApply(SCM_OBJ(&Scm_GenericSlotMissing),       \
                SCM_LIST4(SCM_OBJ(klass), obj, slot, val))

/* GET-SLOT-ACCESSOR
 *
 * (define (get-slot-accessor class slot)
 *   (cond ((assq slot (ref class 'accessors)) => cdr)
 *         (else (error !!!))))
 */
ScmSlotAccessor *Scm_GetSlotAccessor(ScmClass *klass, ScmObj slot)
{
    ScmObj p = Scm_Assq(slot, klass->accessors);
    if (!SCM_PAIRP(p)) return NULL;
    if (!SCM_XTYPEP(SCM_CDR(p), SCM_CLASS_SLOT_ACCESSOR))
        Scm_Error("slot accessor information of class %S, slot %S is screwed up.",
                  SCM_OBJ(klass), slot);
    return SCM_SLOT_ACCESSOR(SCM_CDR(p));
}

/* (internal) slot-ref-using-accessor
 *
 * - assumes accessor belongs to the proper class.
 * - no class redefinition check is done
 */
static ScmObj slot_ref_using_accessor_cc(ScmObj result, void **data)
{
    ScmObj obj = data[0];
    ScmObj slot = data[1];
    int boundp = (data[2] != NULL);

    if (SCM_UNBOUNDP(result) || SCM_UNDEFINEDP(result)) {
        if (boundp) {
            return SCM_FALSE;
        } else {
            return SLOT_UNBOUND(Scm_ClassOf(obj), obj, slot);
        }
    } else {
        if (boundp) return SCM_TRUE;
        else        return result;
    }
}

static ScmObj slot_boundp_using_accessor_cc(ScmObj result,
                                            void **data)
{
    return SCM_FALSEP(result)? SCM_FALSE:SCM_TRUE;
}

static ScmObj slot_ref_using_accessor(ScmObj obj,
                                      ScmSlotAccessor *sa,
                                      int boundp)
{
    ScmObj val = SCM_UNBOUND;
    if (sa->getter) {
        val = sa->getter(obj);
    } else if (sa->slotNumber >= 0) {
        val = scheme_slot_ref(obj, sa->slotNumber);
    } else if (boundp && SCM_PROCEDUREP(sa->schemeBoundp)) {
        void *data[3];
        data[0] = obj;
        data[1] = sa->name;
        data[2] = (void*)(intptr_t)boundp;
        Scm_VMPushCC(slot_boundp_using_accessor_cc, data, 3);
        return Scm_VMApply(sa->schemeBoundp, SCM_LIST1(obj));
    } else if (SCM_PROCEDUREP(sa->schemeGetter)) {
        void *data[3];
        data[0] = obj;
        data[1] = sa->name;
        data[2] = (void*)(intptr_t)boundp;
        Scm_VMPushCC(slot_ref_using_accessor_cc, data, 3);
        return Scm_VMApply(sa->schemeGetter, SCM_LIST1(obj));
    } else {
        Scm_Error("don't know how to retrieve value of slot %S of object %S (MOP error?)",
                  sa->name, obj);
    }
    if (boundp) {
        return SCM_MAKE_BOOL(!(SCM_UNBOUNDP(val) || SCM_UNDEFINEDP(val)));
    } else {
        if (SCM_UNBOUNDP(val) || SCM_UNDEFINEDP(val)) {
            return SLOT_UNBOUND(Scm_ClassOf(obj), obj, sa->name);
        } else {
            return val;
        }
    }
}

/* SLOT-REF
 *
 *(define (slot-ref obj slot bound-check?)
 *   (%check-class-redefined (class-of obj))
 *   (let ((sa (get-slot-accessor (class-of obj) slot)))
 *     (if sa
 *         (%internal-slot-ref-using-accessor obj sa bound-check?)
 *         (slot-missing (class-of obj) obj slot))))
 */
static ScmObj slot_ref_cc(ScmObj result, void **data)
{
    return Scm_VMSlotRef(SCM_OBJ(data[0]), SCM_OBJ(data[1]), (int)(intptr_t)data[2]);
}

ScmObj Scm_VMSlotRef(ScmObj obj, ScmObj slot, int boundp)
{
    ScmClass *klass = Scm_ClassOf(obj);

    if (!SCM_FALSEP(klass->redefined)) {
        void *data[3];
        data[0] = obj;
        data[1] = slot;
        data[2] = (void*)(intptr_t)boundp;
        Scm_VMPushCC(slot_ref_cc, data, 3);
        return instance_class_redefinition(obj, klass);
    }
    ScmSlotAccessor *sa = Scm_GetSlotAccessor(klass, slot);
    if (sa == NULL) return SLOT_MISSING3(klass, obj, slot);
    else            return slot_ref_using_accessor(obj, sa, boundp);
}

/* SLOT-REF-USING-ACCESSOR
 *
 * (define (slot-ref-using-accessor obj sa bound-check?)
 *   (%check-if-sa-is-valid-for-object obj sa)
 *   (%internal-slot-ref-using-accessor obj sa bound-check?))
 *
 * - no class redefinition check is performed.  if obj isn't updated
 *   for the new class, sa must come from the old class.
 */
ScmObj Scm_VMSlotRefUsingAccessor(ScmObj obj, ScmSlotAccessor *sa, int boundp)
{
    ScmClass *klass = Scm_ClassOf(obj);
    if (klass != sa->klass) {
        Scm_Error("attempt to use a slot accessor %S on the object of different class: %S",
                  SCM_OBJ(sa), obj);
    }
    return slot_ref_using_accessor(obj, sa, boundp);
}

/* SLOT-REF-USING-CLASS
 *
 * (define-method slot-ref-using-class
 *      ((class <class>) (obj <object>) slot)
 *   (unless (eq? (class-of obj) class) (error !!!))
 *   (let ((sa (get-slot-accessor class slot)))
 *     (if sa
 *         (%internal-slot-ref-using-accessor obj sa #f)
 *         (slot-missing class obj slot))))
 *
 * - no class redefinition check is performed.  if obj isn't updated,
 *   and class is an old class, then it can access to the old instance's
 *   slot value.
 */
static ScmObj slot_ref_using_class(ScmNextMethod *nm, ScmObj *argv,
                                   int argc, void *d)
{
    ScmClass *klass = SCM_CLASS(argv[0]);
    ScmObj obj = argv[1];
    ScmObj slot = argv[2];

    if (!SCM_EQ(SCM_OBJ(klass), SCM_OBJ(Scm_ClassOf(obj)))) {
        Scm_Error("slot-ref-using-class: class %S is not the class of object %S", klass, obj);
    }
    ScmSlotAccessor *sa = Scm_GetSlotAccessor(klass, slot);
    if (sa == NULL) return SLOT_MISSING3(klass, obj, slot);
    else            return slot_ref_using_accessor(obj, sa, FALSE);
}

static ScmClass *slot_ref_using_class_SPEC[] = {
    SCM_CLASS_STATIC_PTR(Scm_ClassClass),
    SCM_CLASS_STATIC_PTR(Scm_ObjectClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass)
};
static SCM_DEFINE_METHOD(slot_ref_using_class_rec,
                         &Scm_GenericSlotRefUsingClass,
                         3, 0, slot_ref_using_class_SPEC,
                         slot_ref_using_class, NULL);

/* (internal) SLOT-SET-USING-ACCESSOR
 *
 * - assumes accessor belongs to the proper class.
 * - no class redefinition check is done
 */
ScmObj slot_set_using_accessor(ScmObj obj,
                               ScmSlotAccessor *sa,
                               ScmObj val)
{
    if (sa->setter) {
        sa->setter(obj, val);
    } else if (sa->slotNumber >= 0) {
        scheme_slot_set(obj, sa->slotNumber, val);
    } else if (SCM_PROCEDUREP(sa->schemeSetter)) {
        return Scm_VMApply(sa->schemeSetter, SCM_LIST2(obj, val));
    } else {
        Scm_Error("slot %S of class %S is read-only", sa->name,
                  SCM_OBJ(Scm_ClassOf(obj)));
    }
    return SCM_UNDEFINED;
}

/* (internal) SLOT-ACCESSOR-SETTABLE
 * must be in sync with slot_set_using_accessor.
 * this won't detect the case when slot mutation is rejected by
 * the setter procedure.
 */
static int slot_accessor_settable_p(ScmSlotAccessor *sa)
{
    if (sa->setter
        || sa->slotNumber >= 0
        || SCM_PROCEDUREP(sa->schemeSetter))
        return TRUE;
    else
        return FALSE;
}

/* SLOT-SET!
 *
 * (define (slot-set! obj slot val)
 *   (%check-class-redefined (class-of obj))
 *   (let ((sa (get-slot-accessor (class-of obj) slot)))
 *     (if sa
 *         (%internal-slot-set-using-accessor obj sa val)
 *         (slot-missing (class-of obj) obj slot val))))
 */
static ScmObj slot_set_cc(ScmObj result, void **data)
{
    return Scm_VMSlotSet(SCM_OBJ(data[0]), SCM_OBJ(data[1]), SCM_OBJ(data[2]));
}

ScmObj Scm_VMSlotSet(ScmObj obj, ScmObj slot, ScmObj val)
{
    ScmClass *klass = Scm_ClassOf(obj);
    if (!SCM_FALSEP(klass->redefined)) {
        void *data[3];
        data[0] = obj;
        data[1] = slot;
        data[2] = val;
        Scm_VMPushCC(slot_set_cc, data, 3);
        return instance_class_redefinition(obj, klass);
    }
    ScmSlotAccessor *sa = Scm_GetSlotAccessor(klass, slot);
    if (sa == NULL) return SLOT_MISSING4(klass, obj, slot, val);
    else            return slot_set_using_accessor(obj, sa, val);
}

/* SLOT-SET-USING-ACCESSOR
 *
 * (define (slot-set-using-accessor obj sa val)
 *   (%check-if-sa-is-valid-for-object obj sa)
 *   (%internal-slot-set-using-accessor obj sa val))
 *
 * - no class redefinition check is performed.  if obj isn't updated
 *   for the new class, sa must come from the old class.
 */
ScmObj Scm_VMSlotSetUsingAccessor(ScmObj obj, ScmSlotAccessor *sa, ScmObj val)
{
    ScmClass *klass = Scm_ClassOf(obj);
    if (klass != sa->klass) {
        Scm_Error("attempt to use a slot accessor %S on the object of different class: %S",
                  SCM_OBJ(sa), obj);
    }
    return slot_set_using_accessor(obj, sa, val);
}

/* SLOT-SET-USING-CLASS
 *
 * (define-method slot-set-using-class
 *      ((class <class>) (obj <object>) slot val)
 *   (unless (eq? (class-of obj) class) (error !!!))
 *   (let ((sa (get-slot-accessor class slot)))
 *     (if sa
 *         (%internal-slot-set-using-accessor obj sa val)
 *         (slot-missing class obj slot val))))
 *
 * - no class redefinition check is performed.  if obj isn't updated,
 *   and class is an old class, then it can access to the old instance's
 *   slot value.
 */
static ScmObj slot_set_using_class(ScmNextMethod *nm, ScmObj *argv,
                                   int argc, void *d)
{
    ScmClass *klass = SCM_CLASS(argv[0]);
    ScmObj obj = argv[1];
    ScmObj slot = argv[2];
    ScmObj val = argv[3];

    if (!SCM_EQ(SCM_OBJ(klass), SCM_OBJ(Scm_ClassOf(obj)))) {
        Scm_Error("slot-ref-using-class: class %S is not the class of object %S", klass, obj);
    }
    ScmSlotAccessor *sa = Scm_GetSlotAccessor(klass, slot);
    if (sa == NULL) return SLOT_MISSING4(klass, obj, slot, val);
    else            return slot_set_using_accessor(obj, sa, val);
}

static ScmClass *slot_set_using_class_SPEC[] = {
    SCM_CLASS_STATIC_PTR(Scm_ClassClass),
    SCM_CLASS_STATIC_PTR(Scm_ObjectClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass)
};
static SCM_DEFINE_METHOD(slot_set_using_class_rec,
                         &Scm_GenericSlotSetUsingClass,
                         4, 0, slot_set_using_class_SPEC,
                         slot_set_using_class, NULL);

/* SLOT-BOUND?
 *
 * (define (slot-bound? obj slot)
 *   (%check-class-redefined (class-of obj))
 *   (slot-bound-using-class (class-of obj) obj slot))
 */
static ScmObj slot_boundp_cc(ScmObj result, void **data)
{
    ScmObj obj = SCM_OBJ(data[0]);
    ScmObj slot = SCM_OBJ(data[1]);
    return Scm_VMSlotBoundP(obj, slot);
}

ScmObj Scm_VMSlotBoundP(ScmObj obj, ScmObj slot)
{
    ScmClass *klass = Scm_ClassOf(obj);

    if (!SCM_FALSEP(klass->redefined)) {
        void *data[2];
        data[0] = obj;
        data[1] = slot;
        Scm_VMPushCC(slot_boundp_cc, data, 2);
        return instance_class_redefinition(obj, Scm_ClassOf(obj));
    }
    return Scm_VMApply(SCM_OBJ(&Scm_GenericSlotBoundUsingClassP),
                       SCM_LIST3(SCM_OBJ(klass), obj, slot));
}

/* SLOT-BOUND-USING-CLASS?
 *
 * (define-method slot-bound-using-class? ((class <class>)
 *                                         (obj <obj>)
 *                                         slot)
 *   (unless (eq? class (class-of obj)) (error !!!))
 *   (let ((sa (get-slot-accessor class slot)))
 *     (if sa
 *         (%internal-slot-ref-using-accessor obj sa #t)
 *         (slot-missing class obj slot)))
 *
 * - no redefinition check!
 */
static ScmObj slot_bound_using_class_p(ScmNextMethod *nm, ScmObj *argv,
                                       int argc, void *data)
{
    ScmClass *klass = SCM_CLASS(argv[0]);
    ScmObj obj = argv[1];
    ScmObj slot = argv[2];

    if (!SCM_EQ(SCM_OBJ(klass), SCM_OBJ(Scm_ClassOf(obj)))) {
        Scm_Error("slot-bound-using-class?: class %S is not the class of object %S", klass, obj);
    }
    ScmSlotAccessor *sa =Scm_GetSlotAccessor(klass, slot);
    if (sa == NULL) return SLOT_MISSING3(klass, obj, slot);
    else            return slot_ref_using_accessor(obj, sa, TRUE);
}

static ScmClass *slot_bound_using_class_p_SPEC[] = {
    SCM_CLASS_STATIC_PTR(Scm_ClassClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass)
};
static SCM_DEFINE_METHOD(slot_bound_using_class_p_rec,
                         &Scm_GenericSlotBoundUsingClassP,
                         3, 0,
                         slot_bound_using_class_p_SPEC,
                         slot_bound_using_class_p, NULL);

/*
 * Builtin object initializer
 * This is the fallback method of generic initialize.  Since all the
 * Scheme-defined objects will be initialized by object_initialize,
 * this method is called only for built-in classes.
 */
static ScmObj builtin_initialize(ScmObj *argv, int argc, ScmGeneric *gf)
{
    SCM_ASSERT(argc == 2);
    ScmObj instance = argv[0];
    ScmObj initargs = argv[1];
    if (Scm_Length(initargs) % 2) {
        Scm_Error("initializer list is not even: %S", initargs);
    }
    ScmClass *klass = Scm_ClassOf(instance);
    ScmObj ap;
    SCM_FOR_EACH(ap, klass->accessors) {
        ScmSlotAccessor *acc = SCM_SLOT_ACCESSOR(SCM_CDAR(ap));
        if (acc->setter && SCM_KEYWORDP(acc->initKeyword)) {
            ScmObj val = Scm_GetKeyword(acc->initKeyword, initargs, SCM_UNDEFINED);
            if (!SCM_UNDEFINEDP(val)) {
                acc->setter(instance, val);
            }
        }
    }
    return instance;
}

/*--------------------------------------------------------------
 * Slot accessor object
 */

/* we initialize fields appropriately here. */
static ScmObj slot_accessor_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmSlotAccessor *sa = SCM_NEW(ScmSlotAccessor);

    SCM_SET_CLASS(sa, klass);
    sa->name = SCM_FALSE;
    sa->getter = NULL;
    sa->setter = NULL;
    sa->initValue = SCM_UNBOUND;
    sa->initKeyword = SCM_FALSE;
    sa->initThunk = SCM_FALSE;
    sa->initializable = FALSE;
    sa->slotNumber = -1;
    sa->schemeGetter = SCM_FALSE;
    sa->schemeSetter = SCM_FALSE;
    sa->schemeBoundp = SCM_FALSE;
    return SCM_OBJ(sa);
}

static void slot_accessor_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    ScmSlotAccessor *sa = SCM_SLOT_ACCESSOR(obj);

    Scm_Printf(out, "#<slot-accessor %S.%S ",
               (sa->klass? sa->klass->name : SCM_FALSE),
               sa->name);
    if (sa->getter) Scm_Printf(out, "native");
    else if (!SCM_FALSEP(sa->schemeGetter)) Scm_Printf(out, "proc");
    else if (sa->slotNumber >= 0) Scm_Printf(out, "%d", sa->slotNumber);
    else Scm_Printf(out, "unknown");
    if (!SCM_FALSEP(sa->initKeyword))
        Scm_Printf(out, " %S", sa->initKeyword);
    Scm_Printf(out, ">");
}

/* some information is visible from Scheme world */
static ScmObj slot_accessor_class(ScmSlotAccessor *sa)
{
    return SCM_OBJ(sa->klass);
}

static void slot_accessor_class_set(ScmSlotAccessor *sa, ScmObj v)
{
    if (!Scm_TypeP(v, SCM_CLASS_CLASS)) {
        Scm_Error(":class argument must be a class metaobject, but got %S", v);
    }
    sa->klass = SCM_CLASS(v);
}

static ScmObj slot_accessor_name(ScmSlotAccessor *sa)
{
    return sa->name;
}

static void slot_accessor_name_set(ScmSlotAccessor *sa, ScmObj v)
{
    sa->name = v;
}

static ScmObj slot_accessor_init_value(ScmSlotAccessor *sa)
{
    return sa->initValue;
}

static void slot_accessor_init_value_set(ScmSlotAccessor *sa, ScmObj v)
{
    sa->initValue = v;
}

static ScmObj slot_accessor_init_keyword(ScmSlotAccessor *sa)
{
    return sa->initKeyword;
}

static void slot_accessor_init_keyword_set(ScmSlotAccessor *sa, ScmObj v)
{
    sa->initKeyword = v;
}

static ScmObj slot_accessor_init_thunk(ScmSlotAccessor *sa)
{
    return sa->initThunk;
}

static void slot_accessor_init_thunk_set(ScmSlotAccessor *sa, ScmObj v)
{
    sa->initThunk = v;
}

static ScmObj slot_accessor_slot_number(ScmSlotAccessor *sa)
{
    return SCM_MAKE_INT(sa->slotNumber);
}

static void slot_accessor_slot_number_set(ScmSlotAccessor *sa, ScmObj val)
{
    int n = 0;
    if (!SCM_INTP(val) || ((n = SCM_INT_VALUE(val)) < 0))
        Scm_Error("small positive integer required, but got %S", val);
    sa->slotNumber = n;
}

static ScmObj slot_accessor_initializable(ScmSlotAccessor *sa)
{
    return SCM_MAKE_BOOL(sa->initializable);
}

static void slot_accessor_initializable_set(ScmSlotAccessor *sa, ScmObj v)
{
    sa->initializable = SCM_FALSEP(v)? FALSE : TRUE;
}

static ScmObj slot_accessor_settable(ScmSlotAccessor *sa)
{
    return SCM_MAKE_BOOL(slot_accessor_settable_p(sa));
}

static ScmObj slot_accessor_scheme_getter(ScmSlotAccessor *sa)
{
    return sa->schemeGetter;
}

static void slot_accessor_scheme_getter_set(ScmSlotAccessor *sa, ScmObj p)
{
    /* TODO: check */
    sa->schemeGetter = p;
}

static ScmObj slot_accessor_scheme_setter(ScmSlotAccessor *sa)
{
    return sa->schemeSetter;
}

static void slot_accessor_scheme_setter_set(ScmSlotAccessor *sa, ScmObj p)
{
    /* TODO: check */
    sa->schemeSetter = p;
}

static ScmObj slot_accessor_scheme_boundp(ScmSlotAccessor *sa)
{
    return sa->schemeBoundp;
}

static void slot_accessor_scheme_boundp_set(ScmSlotAccessor *sa, ScmObj p)
{
    /* TODO: check */
    sa->schemeBoundp = p;
}

/*=====================================================================
 * <object> class initialization
 */

static ScmObj instance_allocate(ScmClass *klass, ScmObj initargs)
{
    return SCM_OBJ(SCM_NEW_INSTANCE(ScmInstance, klass));
}

/* TRANSIENT: For the binary compatibility during 0.9 series.  Remove
   this on 1.0 */
ScmObj Scm_ObjectAllocate(ScmClass *klass, ScmObj initargs)
{
    return instance_allocate(klass, initargs);
}

/* (initialize <object> initargs) */
static ScmObj object_initialize_cc(ScmObj result, void **data);

static ScmObj object_initialize1(ScmObj obj, ScmObj accs, ScmObj initargs)
{
    if (SCM_NULLP(accs)) return obj;
    SCM_ASSERT(SCM_PAIRP(SCM_CAR(accs))
               && SCM_SLOT_ACCESSOR_P(SCM_CDAR(accs)));
    void *next[3];
    next[0] = obj;
    next[1] = SCM_CDR(accs);
    next[2] = initargs;
    Scm_VMPushCC(object_initialize_cc, next, 3);
    return Scm_VMSlotInitializeUsingAccessor(obj,
                                             SCM_SLOT_ACCESSOR(SCM_CDAR(accs)),
                                             initargs);
}

static ScmObj object_initialize_cc(ScmObj result, void **data)
{
    ScmObj obj = SCM_OBJ(data[0]);
    ScmObj accs = SCM_OBJ(data[1]);
    ScmObj initargs = SCM_OBJ(data[2]);
    return object_initialize1(obj, accs, initargs);
}

static ScmObj object_initialize(ScmNextMethod *nm, ScmObj *argv, int argc,
                                void *data)
{
    ScmObj obj = argv[0];
    ScmObj initargs = argv[1];
    ScmObj accs = Scm_ClassOf(obj)->accessors;
    if (SCM_NULLP(accs)) return obj;
    return object_initialize1(obj, accs, initargs);
}

static ScmClass *object_initialize_SPEC[] = {
    SCM_CLASS_STATIC_PTR(Scm_ObjectClass), SCM_CLASS_STATIC_PTR(Scm_ListClass)
};
static SCM_DEFINE_METHOD(object_initialize_rec,
                         &Scm_GenericInitialize,
                         2, 0,
                         object_initialize_SPEC,
                         object_initialize, NULL);

/* Default equal? delegates compare action to generic function object-equal?.
   We can't use VMApply here */
static int object_compare(ScmObj x, ScmObj y, int equalp)
{
    ScmObj r;
    if (equalp) {
        r = Scm_ApplyRec2(SCM_OBJ(&Scm_GenericObjectEqualP), x, y);
        return (SCM_FALSEP(r)? -1 : 0);
    } else {
        r = Scm_ApplyRec2(SCM_OBJ(&Scm_GenericObjectCompare), x, y);
        if (SCM_INTP(r)) {
            int ri = SCM_INT_VALUE(r);
            if (ri < 0) return -1;
            if (ri > 0) return 1;
            else return 0;
        }
        Scm_Error("object %S and %S can't be ordered", x, y);
        return 0;               /* dummy */
    }
}

/* Fallback methods */
static ScmObj object_compare_default(ScmNextMethod *nm, ScmObj *argv,
                                     int argc, void *data)
{
    return SCM_FALSE;
}

static ScmClass *object_compare_SPEC[] = {
    SCM_CLASS_STATIC_PTR(Scm_TopClass), SCM_CLASS_STATIC_PTR(Scm_TopClass)
};
static SCM_DEFINE_METHOD(object_compare_rec,
                         &Scm_GenericObjectCompare,
                         2, 0,
                         object_compare_SPEC,
                         object_compare_default, NULL);
static SCM_DEFINE_METHOD(object_equalp_rec,
                         &Scm_GenericObjectEqualP,
                         2, 0,
                         object_compare_SPEC,
                         object_compare_default, NULL);

/*=====================================================================
 * Generic function
 */

static ScmObj generic_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmGeneric *gf = SCM_NEW_INSTANCE(ScmGeneric, klass);
    SCM_PROCEDURE_INIT(gf, 0, 0, SCM_PROC_GENERIC, SCM_FALSE);
    gf->methods = SCM_NIL;
    gf->fallback = Scm_NoNextMethod;
    gf->data = NULL;
    gf->maxReqargs = 0;
    (void)SCM_INTERNAL_MUTEX_INIT(gf->lock);
    return SCM_OBJ(gf);
}

static void generic_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
#if 0  /* enable this to show maxReqargs */
    Scm_Printf(port, "#<generic %S (%d:%d)>",
               SCM_GENERIC(obj)->common.info,
               Scm_Length(SCM_GENERIC(obj)->methods),
               SCM_GENERIC(obj)->maxReqargs);
#else
    Scm_Printf(port, "#<generic %S (%d)>",
               SCM_GENERIC(obj)->common.info,
               Scm_Length(SCM_GENERIC(obj)->methods));
#endif
}

/*
 * Accessors
 */
static ScmObj generic_name(ScmGeneric *gf)
{
    return gf->common.info;
}

static void generic_name_set(ScmGeneric *gf, ScmObj val)
{
    gf->common.info = val;
}

static ScmObj generic_methods(ScmGeneric *gf)
{
    return gf->methods;
}

static void generic_methods_set(ScmGeneric *gf, ScmObj val)
{
    int reqs = 0;
    ScmObj cp;
    SCM_FOR_EACH(cp, val) {
        if (!SCM_METHODP(SCM_CAR(cp))) {
            Scm_Error("The methods slot of <generic> must be a list of method, but given: %S", val);
        }
        if (SCM_PROCEDURE_REQUIRED(SCM_CAR(cp)) > reqs) {
            reqs = SCM_PROCEDURE_REQUIRED(SCM_CAR(cp));
        }
    }
    if (!SCM_NULLP(cp)) {
        Scm_Error("The methods slot of <generic> cannot contain an improper list: %S", val);
    }
    (void)SCM_INTERNAL_MUTEX_LOCK(gf->lock);
    gf->methods = val;
    gf->maxReqargs = reqs;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(gf->lock);
}

/* Make base generic function from C */
ScmObj Scm_MakeBaseGeneric(ScmObj name,
                           ScmObj (*fallback)(ScmObj *, int, ScmGeneric*),
                           void *data)
{
    ScmGeneric *gf = SCM_GENERIC(generic_allocate(SCM_CLASS_GENERIC, SCM_NIL));
    gf->common.info = name;
    if (fallback) {
        gf->fallback = fallback;
        gf->data = data;
    }
    return SCM_OBJ(gf);
}

/* default "default method" */
ScmObj Scm_NoNextMethod(ScmObj *argv, int argc, ScmGeneric *gf)
{
    Scm_Error("no applicable method for %S with arguments %S",
              SCM_OBJ(gf), Scm_ArrayToList(argv, argc));
    return SCM_UNDEFINED;       /* dummy */
}

/* another handy "default method", which does nothing. */
ScmObj Scm_NoOperation(ScmObj *argv, int argc, ScmGeneric *gf)
{
    return SCM_UNDEFINED;
}

/* fallback of object-apply */
ScmObj Scm_InvalidApply(ScmObj *argv, int argc, ScmGeneric *gf)
{
    Scm_Error("invalid application: %S", Scm_ArrayToList(argv, argc));
    return SCM_UNDEFINED;
}

/* method-appliable-for-classes?
   NB: This may need to be redesigned once we support eqv specializer.
 */
int Scm_MethodApplicableForClasses(ScmMethod *m, ScmClass *types[], int nargs)
{
    if (nargs < m->common.required
        || (!m->common.optional && nargs != m->common.required)) {
        return FALSE;
    } else {
        ScmClass **sp = m->specializers;
        int n = 0;
        for (; n < m->common.required; n++) {
            if (!Scm_SubtypeP(types[n], sp[n])) return FALSE;
        }
    }
    return TRUE;
}

/* compute-applicable-methods */
ScmObj Scm_ComputeApplicableMethods(ScmGeneric *gf, ScmObj *argv, int argc,
                                    int applyargs)
{
    ScmObj methods = gf->methods, mp, ap;
    ScmObj h = SCM_NIL, t = SCM_NIL;
    ScmClass *typev_s[PREALLOC_SIZE], **typev = typev_s;
    int i, nsel;

    if (SCM_NULLP(methods)) return SCM_NIL;

    if (gf->maxReqargs > PREALLOC_SIZE) {
        typev = SCM_NEW_ATOMIC_ARRAY(ScmClass*, gf->maxReqargs);
    }
    nsel = gf->maxReqargs;
    if (applyargs) argc--;
    for (i = 0; i < argc && nsel >= 0; i++, nsel--) {
        typev[i] = Scm_ClassOf(argv[i]);
    }
    if (applyargs && nsel) {
        SCM_FOR_EACH(ap, argv[argc]) {
            if (--nsel >= 0) typev[i++] = Scm_ClassOf(SCM_CAR(ap));
            argc++;
        }
    }

    SCM_FOR_EACH(mp, methods) {
        ScmObj m = SCM_CAR(mp);
        SCM_ASSERT(SCM_METHODP(m));
        if (Scm_MethodApplicableForClasses(SCM_METHOD(m), typev, argc)) {
            SCM_APPEND1(h, t, SCM_OBJ(m));
        }
    }
    return h;
}

static ScmObj compute_applicable_methods(ScmNextMethod *nm,
                                         ScmObj *argv,
                                         int argc,
                                         void *data)
{
    ScmGeneric *gf = SCM_GENERIC(argv[0]);
    ScmObj arglist = argv[1];
    int n = Scm_Length(arglist);
    if (n < 0) Scm_Error("bad argument list: %S", arglist);

    return Scm_ComputeApplicableMethods(gf, &arglist, 1, TRUE);
}

static ScmClass *compute_applicable_methods_SPEC[] = {
    SCM_CLASS_STATIC_PTR(Scm_GenericClass), SCM_CLASS_STATIC_PTR(Scm_ListClass)
};
static SCM_DEFINE_METHOD(compute_applicable_methods_rec,
                         &Scm_GenericComputeApplicableMethods,
                         2, 0,
                         compute_applicable_methods_SPEC,
                         compute_applicable_methods, NULL);

/* method-more-specific? */
static inline int method_more_specific(ScmMethod *x, ScmMethod *y,
                                       ScmClass **targv, int argc)
{
    ScmClass **xs = x->specializers;
    ScmClass **ys = y->specializers;
    int xreq = SCM_PROCEDURE_REQUIRED(x), yreq = SCM_PROCEDURE_REQUIRED(y);

    for (int i=0; i<xreq && i<yreq; i++) {
        if (xs[i] != ys[i]) {
            ScmClass *ac = targv[i];
            if (xs[i] == ac) return TRUE;
            if (ys[i] == ac) return FALSE;
            for (ScmClass **acpl = ac->cpa; *acpl; acpl++) {
                if (xs[i] == *acpl) return TRUE;
                if (ys[i] == *acpl) return FALSE;
            }
            /* If we're here, two methods are not orderable. */
            Scm_Error("Couldn't determine which method is more specific:"
                      " %S and %S: Check if compute-applicable-methods is "
                      "working properly.", SCM_OBJ(x), SCM_OBJ(y));
        }
    }
    if (xreq > yreq) return TRUE;
    if (xreq < yreq) return FALSE;

    /* all specializers match.  the one without optional arg is more special.*/
    if (SCM_PROCEDURE_OPTIONAL(y)) return TRUE;
    else return FALSE;
}

static ScmObj method_more_specific_p(ScmNextMethod *nm, ScmObj *argv,
                                     int argc, void *data)
{
    ScmMethod *x = SCM_METHOD(argv[0]);
    ScmMethod *y = SCM_METHOD(argv[1]);
    ScmObj targlist = argv[2];
    ScmClass *targv_s[PREALLOC_SIZE], **targv = targv_s;
    int targc = Scm_Length(targlist);
    if (targc < 0) Scm_Error("bad targ list: %S", targlist);
    if (targc > PREALLOC_SIZE) {
        targv = SCM_NEW_ARRAY(ScmClass*, targc);
    }
    int i = 0;
    ScmObj tp;
    SCM_FOR_EACH(tp, targlist) {
        if (!Scm_TypeP(SCM_CAR(tp), SCM_CLASS_CLASS))
            Scm_Error("bad class object in type list: %S", SCM_CAR(tp));
        targv[i++] = SCM_CLASS(SCM_CAR(tp));
    }
    return SCM_MAKE_BOOL(method_more_specific(x, y, targv, targc));
}
static ScmClass *method_more_specific_p_SPEC[] = {
    SCM_CLASS_STATIC_PTR(Scm_MethodClass),
    SCM_CLASS_STATIC_PTR(Scm_MethodClass),
    SCM_CLASS_STATIC_PTR(Scm_ListClass)
};
static SCM_DEFINE_METHOD(method_more_specific_p_rec,
                         &Scm_GenericMethodMoreSpecificP,
                         3, 0,
                         method_more_specific_p_SPEC,
                         method_more_specific_p, NULL);

/* sort-methods
 *  This is a naive implementation just to make things work.
 *
 *  Argv/argc is used to create an array of classes used to order methods.
 *  We never need the arguments more than the maximum number of required
 *  arguments among the given methods; when VM calls Scm_SortMethods, it
 *  only puts as many args as gf->maxReqargs.
 *
 *  TODO: can't we carry around the method list in array
 *  instead of list, at least internally?
 */
ScmObj Scm_SortMethods(ScmObj methods, ScmObj *argv, int argc)
{
    ScmObj array_s[PREALLOC_SIZE], *array = array_s;
    ScmClass *targv_s[PREALLOC_SIZE], **targv = targv_s;
    int cnt = 0, len = Scm_Length(methods);

    if (len >= PREALLOC_SIZE)  array = SCM_NEW_ARRAY(ScmObj, len);
    if (argc >= PREALLOC_SIZE) targv = SCM_NEW_ARRAY(ScmClass*, argc);

    ScmObj mp;
    SCM_FOR_EACH(mp, methods) {
        if (!Scm_TypeP(SCM_CAR(mp), SCM_CLASS_METHOD))
            Scm_Error("bad method in applicable method list: %S", SCM_CAR(mp));
        array[cnt] = SCM_CAR(mp);
        cnt++;
    }
    for (int i=0; i<argc; i++) targv[i] = Scm_ClassOf(argv[i]);

    for (int step = len/2; step > 0; step /= 2) {
        for (int i=step; i<len; i++) {
            for (int j=i-step; j >= 0; j -= step) {
                if (method_more_specific(SCM_METHOD(array[j]),
                                         SCM_METHOD(array[j+step]),
                                         targv, argc)) {
                    break;
                } else {
                    ScmObj tmp = array[j+step];
                    array[j+step] = array[j];
                    array[j] = tmp;
                }
            }
        }
    }
    return Scm_ArrayToList(array, len);
}

/*=====================================================================
 * Method
 */

static ScmObj method_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmMethod *instance = SCM_NEW_INSTANCE(ScmMethod, klass);
    SCM_PROCEDURE_INIT(instance, 0, 0, SCM_PROC_METHOD, SCM_FALSE);
    instance->generic = NULL;
    instance->specializers = NULL;
    instance->func = NULL;
    return SCM_OBJ(instance);
}

static void method_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<method %S>", SCM_METHOD(obj)->common.info);
}

/*
 * (initialize <method> (&key lamdba-list generic specializers body))
 *    Method initialization.   This needs to be hardcoded, since
 *    we can't call Scheme verison of initialize to initialize the
 *    "initialize" method (chicken-and-egg circularity).
 */
static ScmObj method_initialize(ScmNextMethod *nm, ScmObj *argv, int argc,
                                void *data)
{
    ScmMethod *m = SCM_METHOD(argv[0]);
    ScmObj initargs = argv[1];
    ScmObj llist = Scm_GetKeyword(key_lambda_list, initargs, SCM_FALSE);
    ScmObj generic = Scm_GetKeyword(key_generic, initargs, SCM_FALSE);
    ScmObj specs = Scm_GetKeyword(key_specializers, initargs, SCM_FALSE);
    ScmObj body = Scm_GetKeyword(key_body, initargs, SCM_FALSE);
    ScmObj lp, h, t;
    int speclen = 0, req = 0, opt = 0;

    if (!Scm_TypeP(generic, SCM_CLASS_GENERIC))
        Scm_Error("generic function required for :generic argument: %S",
                  generic);
    ScmGeneric *g = SCM_GENERIC(generic);
    if (!SCM_CLOSUREP(body))
        Scm_Error("closure required for :body argument: %S", body);
    if ((speclen = Scm_Length(specs)) < 0)
        Scm_Error("invalid specializers list: %S", specs);
    ScmClass **specarray = class_list_to_array(specs, speclen);

    /* find out # of args from lambda list */
    SCM_FOR_EACH(lp, llist) req++;
    if (!SCM_NULLP(lp)) opt++;

    if (SCM_PROCEDURE_REQUIRED(body) != req + opt + 1)
        Scm_Error("body doesn't match with lambda list: %S", body);
    if (speclen != req)
        Scm_Error("specializer list doesn't match with lambda list: %S",specs);

    m->common.required = req;
    m->common.optional = opt;
    m->common.info = Scm_Cons(g->common.info,
                              class_array_to_names(specarray, speclen));
    m->generic = g;
    m->specializers = specarray;
    m->func = NULL;
    m->data = SCM_CLOSURE(body)->code;
    m->env = SCM_CLOSURE(body)->env;

    /* NB: for comprehensive debugging & profiling information, we modify
       the 'name' field of the compiled code to contain
       (generic-name specializer-class-names ...).  It may be a hazard if
       some existing named closure is given as BODY; as far as the standard
       macro is used, though, altering it should be OK. */
    h = t = SCM_NIL;
    for (int i=0; i<speclen; i++) {
        SCM_APPEND1(h, t, specarray[i]->name);
    }
    SCM_COMPILED_CODE(m->data)->name = Scm_Cons(SCM_PROCEDURE_INFO(g), h);

    /* Register this method to all classes in the specializers.
       This has to come after the part that may throw an error. */
    for (int i=0; i<speclen; i++) {
        Scm_AddDirectMethod(specarray[i], m);
    }
    return SCM_OBJ(m);
}

static ScmClass *method_initialize_SPEC[] = {
    SCM_CLASS_STATIC_PTR(Scm_MethodClass),
    SCM_CLASS_STATIC_PTR(Scm_ListClass)
};
static SCM_DEFINE_METHOD(method_initialize_rec,
                         &Scm_GenericInitialize,
                         2, 0,
                         method_initialize_SPEC,
                         method_initialize, NULL);

/*
 * Accessors
 */
static ScmObj method_required(ScmMethod *m)
{
    return SCM_MAKE_INT(m->common.required);
}

static ScmObj method_optional(ScmMethod *m)
{
    return SCM_MAKE_BOOL(m->common.optional);
}

static ScmObj method_generic(ScmMethod *m)
{
    return m->generic ? SCM_OBJ(m->generic) : SCM_FALSE;
}

static void method_generic_set(ScmMethod *m, ScmObj val)
{
    if (SCM_GENERICP(val))
        m->generic = SCM_GENERIC(val);
    else
        Scm_Error("generic function required, but got %S", val);
}

static ScmObj method_specializers(ScmMethod *m)
{
    if (m->specializers) {
        return class_array_to_list(m->specializers, m->common.required);
    } else {
        return SCM_NIL;
    }
}

static void method_specializers_set(ScmMethod *m, ScmObj val)
{
    int len = Scm_Length(val);
    if (len != m->common.required)
        Scm_Error("specializer list doesn't match body's lambda list: %S", val);
    if (len == 0)
        m->specializers = NULL;
    else
        m->specializers = class_list_to_array(val, len);
}

/* update-direct-method! method old-class new-class
 *   To be called during class redefinition, and swaps reference of
 *   old-class for new-class.
 *
 *   This procedure swaps the pointer "in-place", so as far as the pointer
 *   arithmetic is atomic, we won't have a race condition.  Class
 *   redefinition is serialized inside class-redefinition, so we won't
 *   have the case that more than one thread call this procedure with
 *   the same OLD pointer.  It is possible that more than one thread call
 *   this procedure on the same method simultaneously, but the OLD pointer
 *   should differ, and it won't do any harm for them to run concurrently.
 *
 *   Note that if we implement this in Scheme, we need a mutex to lock the
 *   specializer array.
 */
ScmObj Scm_UpdateDirectMethod(ScmMethod *m, ScmClass *old, ScmClass *newc)
{
    int rec = SCM_PROCEDURE_REQUIRED(m);
    ScmClass **sp = m->specializers;
    for (int i=0; i<rec; i++) {
        if (sp[i] == old) sp[i] = newc;
    }
    if (SCM_FALSEP(Scm_Memq(SCM_OBJ(m), newc->directMethods))) {
        newc->directMethods = Scm_Cons(SCM_OBJ(m), newc->directMethods);
    }
    return SCM_OBJ(m);
}

static ScmObj generic_updatedirectmethod(ScmNextMethod *nm, ScmObj *argv,
                                         int argc, void *data)
{
    return Scm_UpdateDirectMethod(SCM_METHOD(argv[0]),
                                  SCM_CLASS(argv[1]),
                                  SCM_CLASS(argv[2]));
}

static ScmClass *generic_updatedirectmethod_SPEC[] = {
    SCM_CLASS_STATIC_PTR(Scm_MethodClass),
    SCM_CLASS_STATIC_PTR(Scm_ClassClass),
    SCM_CLASS_STATIC_PTR(Scm_ClassClass)
};
static SCM_DEFINE_METHOD(generic_updatedirectmethod_rec,
                         &Scm_GenericUpdateDirectMethod, 3, 0,
                         generic_updatedirectmethod_SPEC,
                         generic_updatedirectmethod, NULL);

/*
 * ADD-METHOD, and it's default method version.
 */
ScmObj Scm_AddMethod(ScmGeneric *gf, ScmMethod *method)
{
    if (method->generic && method->generic != gf)
        Scm_Error("method %S already added to a generic function %S",
                  method, method->generic);
    if (!SCM_FALSEP(Scm_Memq(SCM_OBJ(method), gf->methods)))
        Scm_Error("method %S already appears in a method list of generic %S"
                  " something wrong in MOP implementation?",
                  method, gf);

    int replaced = FALSE, reqs = gf->maxReqargs;
    method->generic = gf;
    /* pre-allocate cons pair to avoid triggering GC in the critical region */
    ScmObj pair = Scm_Cons(SCM_OBJ(method), gf->methods);
    if (SCM_PROCEDURE_REQUIRED(method) > reqs) {
        reqs = SCM_PROCEDURE_REQUIRED(method);
    }

    /* Check if a method with the same signature exists */
    (void)SCM_INTERNAL_MUTEX_LOCK(gf->lock);
    ScmObj mp;
    SCM_FOR_EACH(mp, gf->methods) {
        ScmMethod *mm = SCM_METHOD(SCM_CAR(mp));
        if (SCM_PROCEDURE_REQUIRED(method) == SCM_PROCEDURE_REQUIRED(mm)
            && SCM_PROCEDURE_OPTIONAL(method) == SCM_PROCEDURE_OPTIONAL(mm)) {
            ScmClass **sp1 = method->specializers;
            ScmClass **sp2 = mm->specializers;
            int i;
            for (i=0; i<SCM_PROCEDURE_REQUIRED(method); i++) {
                if (sp1[i] != sp2[i]) break;
            }
            if (i == SCM_PROCEDURE_REQUIRED(method)) {
                SCM_SET_CAR(mp, SCM_OBJ(method));
                replaced = TRUE;
                break;
            }
        }
    }
    if (!replaced) {
        gf->methods = pair;
        gf->maxReqargs = reqs;
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(gf->lock);
    return SCM_UNDEFINED;
}

static ScmObj generic_addmethod(ScmNextMethod *nm, ScmObj *argv, int argc,
                                void *data)
{
    return Scm_AddMethod(SCM_GENERIC(argv[0]), SCM_METHOD(argv[1]));
}

static ScmClass *generic_addmethod_SPEC[] = {
    SCM_CLASS_STATIC_PTR(Scm_GenericClass),
    SCM_CLASS_STATIC_PTR(Scm_MethodClass)
};
static SCM_DEFINE_METHOD(generic_addmethod_rec,
                         &Scm_GenericAddMethod, 2, 0,
                         generic_addmethod_SPEC,
                         generic_addmethod, NULL);

/*
 * DELETE-METHOD, and it's default method version.
 */
ScmObj Scm_DeleteMethod(ScmGeneric *gf, ScmMethod *method)
{
    if (!method->generic || method->generic != gf) return SCM_UNDEFINED;

    (void)SCM_INTERNAL_MUTEX_LOCK(gf->lock);
    ScmObj mp = gf->methods;
    if (SCM_PAIRP(mp)) {
        if (SCM_EQ(SCM_CAR(mp), SCM_OBJ(method))) {
            gf->methods = SCM_CDR(mp);
            method->generic = NULL;
        } else {
            while (SCM_PAIRP(SCM_CDR(mp))) {
                if (SCM_EQ(SCM_CADR(mp), SCM_OBJ(method))) {
                    SCM_CDR(mp) = SCM_CDDR(mp);
                    method->generic = NULL;
                    break;
                }
                mp = SCM_CDR(mp);
            }
        }
    }
    SCM_FOR_EACH(mp, gf->methods) {
        /* sync # of required selector */
        if (SCM_PROCEDURE_REQUIRED(SCM_CAR(mp)) > gf->maxReqargs) {
            gf->maxReqargs = SCM_PROCEDURE_REQUIRED(SCM_CAR(mp));
        }
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(gf->lock);
    return SCM_UNDEFINED;
}

static ScmObj generic_deletemethod(ScmNextMethod *nm, ScmObj *argv, int argc,
                                   void *data)
{
    return Scm_DeleteMethod(SCM_GENERIC(argv[0]), SCM_METHOD(argv[1]));
}

static ScmClass *generic_deletemethod_SPEC[] = {
    SCM_CLASS_STATIC_PTR(Scm_GenericClass),
    SCM_CLASS_STATIC_PTR(Scm_MethodClass)
};
static SCM_DEFINE_METHOD(generic_deletemethod_rec,
                         &Scm_GenericDeleteMethod, 2, 0,
                         generic_deletemethod_SPEC,
                         generic_deletemethod, NULL);

/*=====================================================================
 * Next Method
 */

ScmObj Scm_MakeNextMethod(ScmGeneric *gf, ScmObj methods,
                          ScmObj *argv, int argc, int copyargs, int applyargs)
{
    ScmNextMethod *nm = SCM_NEW(ScmNextMethod);
    SCM_SET_CLASS(nm, SCM_CLASS_NEXT_METHOD);
    SCM_PROCEDURE_INIT(nm, 0, 0, SCM_PROC_NEXT_METHOD, SCM_FALSE);
    nm->generic = gf;
    nm->methods = methods;
    if (copyargs) {
        nm->argv = SCM_NEW_ARRAY(ScmObj, argc);
        memcpy(nm->argv, argv, sizeof(ScmObj)*argc);
    } else {
        nm->argv = argv;
    }
    nm->argc = argc;
    nm->applyargs = applyargs;
    return SCM_OBJ(nm);
}

static void next_method_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    ScmNextMethod *nm = SCM_NEXT_METHOD(obj);
    ScmObj args = Scm_ArrayToList(nm->argv, nm->argc);
    Scm_Printf(out, "#<next-method %S%d %S>", nm->methods, nm->applyargs, args);
}

/*=====================================================================
 * Accessor Method
 */

static void accessor_method_print(ScmObj obj, ScmPort *port,
                                  ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<accessor-method %S>", SCM_METHOD(obj)->common.info);
}

static ScmObj accessor_get_proc(ScmNextMethod *nm, ScmObj *argv, int argc,
                                void *data)
{
    ScmObj obj = argv[0];
    ScmSlotAccessor *ca = (ScmSlotAccessor*)data;
    /* NB: we need this extra check, in case if the getter method of parent
       class and the one of subclass don't share the generic function, and
       the getter method of parent class is called on the subclass's instance.
       See test/object.scm "module and accessor" section for a concrete
       example. */
    if (!SCM_EQ(Scm_ClassOf(obj), ca->klass)) {
        /* fallback to a normal protocol */
        return Scm_VMSlotRef(obj, ca->name, FALSE);
    }
    /* Standard path.  We can skip searching the slot, so it is faster. */
    return slot_ref_using_accessor(obj, ca, FALSE);
}

static ScmObj accessor_set_proc(ScmNextMethod *nm, ScmObj *argv, int argc,
                                void *data)
{
    ScmObj obj = argv[0];
    ScmObj val = argv[1];
    ScmSlotAccessor *ca = (ScmSlotAccessor*)data;
    /* See the comment in accessor_get_proc above about this check. */
    if (!SCM_EQ(Scm_ClassOf(obj), ca->klass)) {
        return Scm_VMSlotSet(obj, ca->name, val);
    }
    return slot_set_using_accessor(obj, ca, val);
}

/* Accessor method can be just created by usual allocate/initialize
   sequence.  But it requires :slot-accessor initarg.  The method body
   is overridden by C function, and the closure given to :body doesn't
   have an effect.  */
static ScmObj accessor_method_initialize(ScmNextMethod *nm, ScmObj *argv,
                                         int argc, void *data)
{
    ScmMethod *m = SCM_METHOD(method_initialize(nm, argv, argc, data));
    ScmObj initargs = argv[1];
    ScmObj sa = Scm_GetKeyword(key_slot_accessor, initargs, SCM_FALSE);

    if (!SCM_SLOT_ACCESSOR_P(sa)) {
        Scm_Error("slot accessor required for :slot-accessor argument: %S",
                  sa);
    }

    m->data = sa;
    switch (SCM_PROCEDURE_REQUIRED(m)) {
    case 1: /* accessor <obj> - this is a getter */
        m->func = accessor_get_proc;
        break;
    case 2: /* accessor <obj> <val> - this is a setter */
        m->func = accessor_set_proc;
        break;
    default:
        Scm_Error("bad initialization parameter for accessor method %S", m);
    }
    return SCM_OBJ(m);
}

static ScmClass *accessor_method_initialize_SPEC[] = {
    SCM_CLASS_STATIC_PTR(Scm_AccessorMethodClass),
    SCM_CLASS_STATIC_PTR(Scm_ListClass)
};
static SCM_DEFINE_METHOD(accessor_method_initialize_rec,
                         &Scm_GenericInitialize,
                         2, 0,
                         accessor_method_initialize_SPEC,
                         accessor_method_initialize, NULL);

static ScmObj accessor_method_slot_accessor(ScmAccessorMethod *m)
{
    SCM_ASSERT(SCM_SLOT_ACCESSOR_P(m->data));
    return SCM_OBJ(m->data);
}

static void accessor_method_slot_accessor_set(ScmAccessorMethod *m, ScmObj v)
{
    if (!SCM_SLOT_ACCESSOR_P(v)) {
        Scm_Error("slot accessor required, but got %S", v);
    }
    m->data = v;
}

/*=====================================================================
 * Foreign pointer mechanism
 */

/* foreign pointer instance flags */
enum {
    SCM_FOREIGN_POINTER_INVALID = (1L<<0) /* The pointer is no longer valid. */
};

struct foreign_data_rec {
    int flags;
    ScmForeignCleanupProc cleanup;
    ScmInternalMutex attr_mutex;     /* lock for updating foreign pointer's
                                        "attribute" slot.  we use one-per-class
                                        mutex, assuming the mutation of attrs
                                        is rare and saving space per foreign
                                        pointer is more important. */
    ScmHashCore *identity_map;       /* for KEEP_IDENTITY */
    ScmInternalMutex identity_mutex; /* lock for identity_map */
};

ScmClass *Scm_MakeForeignPointerClass(ScmModule *mod,
                                      const char *name,
                                      ScmClassPrintProc print_proc,
                                      ScmForeignCleanupProc cleanup_proc,
                                      int flags)
{
    ScmClass *fp = (ScmClass*)class_allocate(SCM_CLASS_CLASS, SCM_NIL);
    ScmObj s = SCM_INTERN(name);
    struct foreign_data_rec *data = SCM_NEW(struct foreign_data_rec);
    /* NB: here we don't need to use SCM_CLASS_STATIC_PTR, since we only
       refer intra-dll classes, and we don't go through init_class.
       If we ever find the need to go through init_class, do not forget
       to change fpcpa initializers as well, to make it work on windows. */
    static ScmClass *fpcpa[] = { SCM_CLASS_FOREIGN_POINTER,
                                 SCM_CLASS_TOP,
                                 NULL };
    fp->name = s;
    fp->allocate = NULL;
    fp->print = print_proc;
    fp->cpa = fpcpa;
    fp->flags = SCM_CLASS_BUILTIN;
    initialize_builtin_cpl(fp, SCM_FALSE);
    Scm_Define(mod, SCM_SYMBOL(s), SCM_OBJ(fp));
    fp->slots = SCM_NIL;
    fp->accessors = SCM_NIL;
    data->flags = flags;
    data->cleanup = cleanup_proc;
    (void)SCM_INTERNAL_MUTEX_INIT(data->attr_mutex);
    if (flags & SCM_FOREIGN_POINTER_KEEP_IDENTITY) {
        (void)SCM_INTERNAL_MUTEX_INIT(data->identity_mutex);
        data->identity_map = SCM_NEW(ScmHashCore);
        Scm_HashCoreInitSimple(data->identity_map, SCM_HASH_WORD, 256, NULL);
    } else {
        data->identity_map = NULL;
    }
    fp->data = (void*)data; /* see the note above class_allocate() */
    return fp;
}

static void fp_finalize(ScmObj obj, void *data)
{
    void (*cleanup)(ScmObj) = (void (*)(ScmObj))data;
    cleanup(obj);
}

/* This shouldn't raise an error. */
static ScmForeignPointer *make_foreign_int(ScmClass *klass, void *ptr,
                                           ScmObj attr,
                                           struct foreign_data_rec *data)
{
    ScmForeignPointer *obj = SCM_NEW(ScmForeignPointer);
    SCM_SET_CLASS(obj, klass);
    obj->ptr = ptr;
    obj->attributes = attr;
    obj->flags = 0;
    if (data->cleanup) {
        Scm_RegisterFinalizer(SCM_OBJ(obj), fp_finalize, data->cleanup);
    }
    return obj;
}

/* Note for future API: Scm_MakeForeignPointer should take attr argument.
   We add *WithAttr only to keep ABI compatibility. */
ScmObj Scm_MakeForeignPointer(ScmClass *klass, void *ptr)
{
    return Scm_MakeForeignPointerWithAttr(klass, ptr, SCM_NIL);
}

ScmObj Scm_MakeForeignPointerWithAttr(ScmClass *klass, void *ptr, ScmObj attr)
{
    ScmForeignPointer *obj;
    struct foreign_data_rec *data = (struct foreign_data_rec *)klass->data;

    if (!klass) {               /* for extra safety */
        Scm_Error("NULL pointer passed to Scm_MakeForeignPointer");
    }
    if (!Scm_SubtypeP(klass, SCM_CLASS_FOREIGN_POINTER)) {
        Scm_Error("attempt to instantiate non-foreign-pointer class %S via Scm_MakeForeignPointer", klass);
    }

    if (ptr == NULL && (data->flags & SCM_FOREIGN_POINTER_MAP_NULL)) {
        return SCM_FALSE;
    }

    if (data->identity_map) {
        (void)SCM_INTERNAL_MUTEX_LOCK(data->identity_mutex);
        ScmDictEntry *e = Scm_HashCoreSearch(data->identity_map,
                                             (intptr_t)ptr, SCM_DICT_CREATE);
        if (e->value) {
            if (Scm_WeakBoxEmptyP((ScmWeakBox*)e->value)) {
                obj = make_foreign_int(klass, ptr, attr, data);
                Scm_WeakBoxSet((ScmWeakBox*)e->value, obj);
            } else {
                obj = (ScmForeignPointer*)Scm_WeakBoxRef((ScmWeakBox*)e->value);
            }
        } else {
            obj = make_foreign_int(klass, ptr, attr, data);
            e->value = (intptr_t)Scm_MakeWeakBox(obj);
        }
        (void)SCM_INTERNAL_MUTEX_UNLOCK(data->identity_mutex);
    } else {
        obj = make_foreign_int(klass, ptr, attr, data);
    }
    return SCM_OBJ(obj);
}

void *Scm_ForeignPointerRef(ScmForeignPointer *fp)
{
    if (Scm_ForeignPointerInvalidP(fp)) {
        Scm_Error("attempt to dereference a foreign pointer "
                  "that is no longer valid: %S", SCM_OBJ(fp));
        
    }
    return fp->ptr;
}

int Scm_ForeignPointerInvalidP(ScmForeignPointer *fp)
{
    return (fp->flags & SCM_FOREIGN_POINTER_INVALID);
}

void Scm_ForeignPointerInvalidate(ScmForeignPointer *fp)
{
    fp->flags |= SCM_FOREIGN_POINTER_INVALID;
}

ScmObj Scm_ForeignPointerAttr(ScmForeignPointer *fp)
{
    return fp->attributes;
}

ScmObj Scm_ForeignPointerAttrGet(ScmForeignPointer *fp,
                                 ScmObj key, ScmObj fallback)
{
    /* no need to lock, for AttrSet won't make fp->attributes inconsisnent
       at any moment. */
    ScmObj p = Scm_Assq(key, fp->attributes);
    if (SCM_PAIRP(p)) return SCM_CDR(p);
    if (SCM_UNBOUNDP(fallback)) {
        Scm_Error("No value associated with key %S in a foreign pointer %S",
                  key, SCM_OBJ(fp));
    }
    return fallback;
}

ScmObj Scm_ForeignPointerAttrSet(ScmForeignPointer *fp,
                                 ScmObj key, ScmObj value)
{
    struct foreign_data_rec *data
        = (struct foreign_data_rec*)(SCM_CLASS_OF(fp)->data);

    /* NB: We presume mutating foreign pointer attributes is rare operation,
       so we don't try hard to make it efficient.   Particularly, we use
       one mutex shared among all instances of the same class, in order to
       keep the size of each foreign pointer instance small.  We'll reconsider
       the design if the performance ever becomes a problem.  */
    (void)SCM_INTERNAL_MUTEX_LOCK(data->attr_mutex);
    ScmObj r = SCM_UNDEFINED;
    ScmObj p = Scm_Assq(key, fp->attributes);
    if (SCM_PAIRP(p)) {
        SCM_SET_CDR(p, value);
        r = value;
    } else {
        fp->attributes = Scm_Acons(key, value, fp->attributes);
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(data->attr_mutex);
    return r;
}

/*=====================================================================
 * Class initialization
 */

/* TODO: need a cleaner way! */
/* static declaration of some structures */

static ScmClassStaticSlotSpec class_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", class_name, class_name_set),
    SCM_CLASS_SLOT_SPEC("cpl",  class_cpl, class_cpl_set),
    SCM_CLASS_SLOT_SPEC("direct-supers",  class_direct_supers, class_direct_supers_set),
    SCM_CLASS_SLOT_SPEC("accessors", class_accessors, class_accessors_set),
    SCM_CLASS_SLOT_SPEC("slots", class_slots_ref, class_slots_set),
    SCM_CLASS_SLOT_SPEC("direct-slots", class_direct_slots, class_direct_slots_set),
    SCM_CLASS_SLOT_SPEC("num-instance-slots", class_numislots, class_numislots_set),
    SCM_CLASS_SLOT_SPEC("direct-subclasses", class_direct_subclasses, NULL),
    SCM_CLASS_SLOT_SPEC("direct-methods", class_direct_methods, NULL),
    SCM_CLASS_SLOT_SPEC("initargs", class_initargs, class_initargs_set),
    SCM_CLASS_SLOT_SPEC("defined-modules", class_defined_modules, class_defined_modules_set),
    SCM_CLASS_SLOT_SPEC("redefined", class_redefined, NULL),
    SCM_CLASS_SLOT_SPEC("category", class_category, NULL),
    SCM_CLASS_SLOT_SPEC_END()
};

static ScmClassStaticSlotSpec generic_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", generic_name, generic_name_set),
    SCM_CLASS_SLOT_SPEC("methods", generic_methods, generic_methods_set),
    SCM_CLASS_SLOT_SPEC_END()
};

static ScmClassStaticSlotSpec method_slots[] = {
    SCM_CLASS_SLOT_SPEC("required", method_required, NULL),
    SCM_CLASS_SLOT_SPEC("optional", method_optional, NULL),
    SCM_CLASS_SLOT_SPEC("generic", method_generic, method_generic_set),
    SCM_CLASS_SLOT_SPEC("specializers", method_specializers, method_specializers_set),
    SCM_CLASS_SLOT_SPEC_END()
};

static ScmClassStaticSlotSpec accessor_method_slots[] = {
    SCM_CLASS_SLOT_SPEC("required", method_required, NULL),
    SCM_CLASS_SLOT_SPEC("optional", method_optional, NULL),
    SCM_CLASS_SLOT_SPEC("generic", method_generic, method_generic_set),
    SCM_CLASS_SLOT_SPEC("specializers", method_specializers, method_specializers_set),
    SCM_CLASS_SLOT_SPEC("slot-accessor", accessor_method_slot_accessor, accessor_method_slot_accessor_set),
    SCM_CLASS_SLOT_SPEC_END()
};

static ScmClassStaticSlotSpec slot_accessor_slots[] = {
    SCM_CLASS_SLOT_SPEC("class", slot_accessor_class,
                        slot_accessor_class_set),
    SCM_CLASS_SLOT_SPEC("name", slot_accessor_name,
                        slot_accessor_name_set),
    SCM_CLASS_SLOT_SPEC("init-value", slot_accessor_init_value,
                        slot_accessor_init_value_set),
    SCM_CLASS_SLOT_SPEC("init-keyword", slot_accessor_init_keyword,
                        slot_accessor_init_keyword_set),
    SCM_CLASS_SLOT_SPEC("init-thunk", slot_accessor_init_thunk,
                        slot_accessor_init_thunk_set),
    SCM_CLASS_SLOT_SPEC("initializable", slot_accessor_initializable,
                        slot_accessor_initializable_set),
    SCM_CLASS_SLOT_SPEC("settable", slot_accessor_settable,
                        NULL),
    SCM_CLASS_SLOT_SPEC("slot-number", slot_accessor_slot_number,
                        slot_accessor_slot_number_set),
    SCM_CLASS_SLOT_SPEC("getter", slot_accessor_scheme_getter,
                        slot_accessor_scheme_getter_set),
    SCM_CLASS_SLOT_SPEC("setter", slot_accessor_scheme_setter,
                        slot_accessor_scheme_setter_set),
    SCM_CLASS_SLOT_SPEC("bound?", slot_accessor_scheme_boundp,
                        slot_accessor_scheme_boundp_set),
    SCM_CLASS_SLOT_SPEC_END()
};

/*
 * Sets up CPL from CPA
 */
static void initialize_builtin_cpl(ScmClass *klass, ScmObj supers)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;

    SCM_APPEND1(h, t, SCM_OBJ(klass));
    for (ScmClass **p = klass->cpa; *p; p++) SCM_APPEND1(h, t, SCM_OBJ(*p));
    klass->cpl = h;
    if (SCM_PAIRP(supers)) {
        /* Check validity of the given supers. */
        ScmObj cp, sp = supers;
        SCM_FOR_EACH(cp, klass->cpl) {
            if (SCM_EQ(SCM_CAR(cp), SCM_CAR(sp))) {
                sp = SCM_CDR(sp);
                if (SCM_NULLP(sp)) break;
            }
        }
        if (!SCM_NULLP(sp)) {
            /* NB: At this point we may not have initialized error handing
               mechanism, so we have no option but quit. */
            const char *cname = "(unnamed class)";
            if (SCM_STRINGP(klass->name)) {
                cname = Scm_GetStringConst(SCM_STRING(klass->name));
            }
            Scm_Panic("Class %s is being initialized with inconsistent super class list.  Must be an implementation error.  Report to the author.", cname);
        }
        klass->directSupers = supers;
    } else if (SCM_PAIRP(SCM_CDR(h))) {
        /* Default: take the next class of CPL as the only direct super */
        klass->directSupers = SCM_LIST1(SCM_CADR(h));
    } else {
        /* Should this happen? */
        klass->directSupers = SCM_NIL;
    }
}

/*
 * A common part for builtin class initialization
 */
static void init_class(ScmClass *klass,
                       const char *name,
                       ScmModule *mod,
                       ScmObj supers,  /* SCM_FALSE if using default */
                       ScmClassStaticSlotSpec *specs,
                       int flags)  /* reserved */
{
    ScmObj slots = SCM_NIL, t = SCM_NIL, acc = SCM_NIL;

    /* initialize CPL and directSupers */
    if (klass->cpa == NULL) {
        klass->cpa = SCM_CLASS_DEFAULT_CPL;
    }

    klass->name = SCM_INTERN(name);
    initialize_builtin_cpl(klass, supers);

    /* On Windows, mutex and cv must be initialized at runtime. */
    SCM_INTERNAL_MUTEX_INIT(klass->mutex);
    SCM_INTERNAL_COND_INIT(klass->cv);

    /* insert binding */
    Scm_Define(mod, SCM_SYMBOL(klass->name), SCM_OBJ(klass));

    /* initialize direct slots */
    if (specs) {
        for (;specs->name; specs++) {
            ScmObj snam = SCM_INTERN(specs->name);
            specs->accessor.klass = klass;
            specs->accessor.name = snam;
            acc = Scm_Acons(snam, SCM_OBJ(&specs->accessor), acc);
            specs->accessor.initKeyword = SCM_MAKE_KEYWORD(specs->name);
            SCM_APPEND1(slots, t,
                        Scm_List(snam,
                                 key_allocation, key_builtin,
                                 key_slot_accessor, SCM_OBJ(&specs->accessor),
                                 NULL));
        }
    }
    klass->directSlots = slots;

    /* compute other slots inherited from supers */
    for (ScmClass **super = klass->cpa; *super; super++) {
        ScmObj sp;
        SCM_FOR_EACH(sp, (*super)->directSlots) {
            ScmObj slot = SCM_CAR(sp), snam, p, a;
            SCM_ASSERT(SCM_PAIRP(slot));
            snam = SCM_CAR(slot);
            p = Scm_Assq(snam, slots);
            if (SCM_FALSEP(p)) {
                slots = Scm_Cons(Scm_CopyList(slot), slots);
                a = Scm_GetKeyword(key_slot_accessor, SCM_CDR(slot), SCM_FALSE);
                SCM_ASSERT(SCM_HOBJP(a));
                SCM_ASSERT(SCM_SLOT_ACCESSOR_P(a));
                acc = Scm_Acons(snam, a, acc);
            }
        }
    }
    klass->slots = slots;
    klass->accessors = acc;
}

/*
 * Inter-module API
 */

/* The most standard way to initialize a class. */
void Scm_InitStaticClass(ScmClass *klass,
                         const char *name,
                         ScmModule *mod,
                         ScmClassStaticSlotSpec *specs,
                         int flags) /* reserved */
{
    init_class(klass, name, mod, SCM_FALSE, specs, flags);
}

/* If the builtin class needs multiple inheritance... */
void Scm_InitStaticClassWithSupers(ScmClass *klass,
                                   const char *name,
                                   ScmModule *mod,
                                   ScmObj supers,
                                   ScmClassStaticSlotSpec *specs,
                                   int flags) /* reserved */
{
    init_class(klass, name, mod, supers, specs, flags);
}

/* A special initialization for some of builtin classes.
   Sets klass's metaclass to META.  If META is NULL, a new metaclass
   (whose name has "-meta" after the original class name except brackets)
   is created automatically.  This procedure should be used only if
   metaclass is absolutely required (e.g. all condition classes should
   be an instance of <condition-meta>).   The older version of Gauche
   has metaclasses for many builtin classes, which is a compensation of
   lack of eqv-method specializer; such use of metaclass is deprecated
   and will be removed in future. */
void Scm_InitStaticClassWithMeta(ScmClass *klass,
                                 const char *name,
                                 ScmModule *mod,
                                 ScmClass *meta,
                                 ScmObj supers,
                                 ScmClassStaticSlotSpec *specs,
                                 int flags)
{
    init_class(klass, name, mod, supers, specs, flags);

    if (meta) {
        SCM_SET_CLASS(klass, meta);
    } else {
        int nlen = (int)strlen(name);
        char *metaname = SCM_NEW_ATOMIC2(char *, nlen + 6);

        if (name[nlen - 1] == '>') {
            strncpy(metaname, name, nlen-1);
            strcpy(metaname+nlen-1, "-meta>");
        } else {
            strcpy(metaname, name);
            strcat(metaname, "-meta");
        }
        SCM_SET_CLASS(klass, make_implicit_meta(metaname, klass->cpa, mod));
    }
}

/* The old API - deprecated.  We keep this around for a while
   for backward compatibility. */
void Scm_InitBuiltinClass(ScmClass *klass, const char *name,
                          ScmClassStaticSlotSpec *specs,
                          int withMeta, ScmModule *mod)
{
    if (withMeta) {
        Scm_InitStaticClassWithMeta(klass, name, mod, NULL, SCM_FALSE, specs, 0);
    } else {
        Scm_InitStaticClass(klass, name, mod, specs, 0);
    }
}

void Scm_InitBuiltinGeneric(ScmGeneric *gf, const char *name, ScmModule *mod)
{
    ScmObj s = SCM_INTERN(name);
    gf->common.info = s;
    if (gf->fallback == NULL) {
        gf->fallback = Scm_NoNextMethod;
    }
    (void)SCM_INTERNAL_MUTEX_INIT(gf->lock);
    Scm_Define(mod, SCM_SYMBOL(s), SCM_OBJ(gf));
}

void Scm_InitBuiltinMethod(ScmMethod *m)
{
    m->common.info = Scm_Cons(m->generic->common.info,
                              class_array_to_names(m->specializers,
                                                   m->common.required));
    Scm_AddMethod(m->generic, m);
}

void Scm__InitClass(void)
{
    ScmModule *mod = Scm_GaucheModule();
    static ScmClass *nullcpa[1] = {NULL}; /* for <top> */

    key_allocation = SCM_MAKE_KEYWORD("allocation");
    key_builtin = SCM_MAKE_KEYWORD("builtin");
    key_slot_accessor = SCM_MAKE_KEYWORD("slot-accessor");
    key_name = SCM_MAKE_KEYWORD("name");
    key_lambda_list = SCM_MAKE_KEYWORD("lambda-list");
    key_generic = SCM_MAKE_KEYWORD("generic");
    key_specializers = SCM_MAKE_KEYWORD("specializers");
    key_body = SCM_MAKE_KEYWORD("body");

    (void)SCM_INTERNAL_MUTEX_INIT(class_redefinition_lock.mutex);
    (void)SCM_INTERNAL_COND_INIT(class_redefinition_lock.cv);

    /* booting class metaobject */
    Scm_TopClass.cpa = nullcpa;

#define BINIT(cl, nam, slots) \
    Scm_InitStaticClass(cl, nam, mod, slots, 0)

#define CINIT(cl, nam) \
    Scm_InitStaticClassWithMeta(cl, nam, mod, NULL, SCM_FALSE, NULL, 0)

    /* box.c */
    CINIT(SCM_CLASS_BOX,    "<%box>");

    /* class.c */
    BINIT(SCM_CLASS_CLASS,  "<class>", class_slots);
    BINIT(SCM_CLASS_TOP,    "<top>",     NULL);
    BINIT(SCM_CLASS_BOTTOM, "<bottom>",  NULL);
    CINIT(SCM_CLASS_BOOL,   "<boolean>");
    CINIT(SCM_CLASS_CHAR,   "<char>");
    BINIT(SCM_CLASS_EOF_OBJECT,"<eof-object>", NULL);
    BINIT(SCM_CLASS_UNDEFINED_OBJECT,"<undefined-object>", NULL);
    BINIT(SCM_CLASS_UNKNOWN,"<unknown>", NULL);
    BINIT(SCM_CLASS_OBJECT, "<object>",  NULL);
    BINIT(SCM_CLASS_GENERIC,"<generic>", generic_slots);
    Scm_GenericClass.flags |= SCM_CLASS_APPLICABLE;
    BINIT(SCM_CLASS_METHOD, "<method>",  method_slots);
    Scm_MethodClass.flags |= SCM_CLASS_APPLICABLE;
    BINIT(SCM_CLASS_NEXT_METHOD, "<next-method>", NULL);
    Scm_NextMethodClass.flags |= SCM_CLASS_APPLICABLE;
    BINIT(SCM_CLASS_ACCESSOR_METHOD, "<accessor-method>", accessor_method_slots);
    Scm_AccessorMethodClass.flags |= SCM_CLASS_APPLICABLE;
    BINIT(SCM_CLASS_SLOT_ACCESSOR,"<slot-accessor>", slot_accessor_slots);
    BINIT(SCM_CLASS_FOREIGN_POINTER, "<foreign-pointer>", NULL);

    /* char.c */
    CINIT(SCM_CLASS_CHAR_SET,         "<char-set>");

    /* comparator.c */
    /* initialized in Scm__InitComparator */

    /* compile.c */
    /* initialized in Scm__InitCompiler */

    /* error.c */
    /* initialized in Scm__InitExceptions */

    /* hash.c */
    CINIT(SCM_CLASS_HASH_TABLE,       "<hash-table>");

    /* list.c */
    CINIT(SCM_CLASS_LIST,             "<list>");
    CINIT(SCM_CLASS_PAIR,             "<pair>");
    CINIT(SCM_CLASS_NULL,             "<null>");

    /* load.c */
    CINIT(SCM_CLASS_AUTOLOAD,         "<autoload>");

    /* macro.c */
    CINIT(SCM_CLASS_SYNTAX,           "<syntax>");
    CINIT(SCM_CLASS_MACRO,            "<macro>");
    CINIT(SCM_CLASS_SYNTAX_RULES,     "<syntax-rules>");

    /* module.c */
    /* class initialized in libmod.scm */

    /* number.c */
    CINIT(SCM_CLASS_NUMBER,           "<number>");
    CINIT(SCM_CLASS_COMPLEX,          "<complex>");
    CINIT(SCM_CLASS_REAL,             "<real>");
    CINIT(SCM_CLASS_RATIONAL,         "<rational>");
    CINIT(SCM_CLASS_INTEGER,          "<integer>");

    /* port.c */
    /* initialized in Scm__InitPort */

    /* proc.c */
    /* initialized in Scm__InitProc */

    /* promise.c */
    CINIT(SCM_CLASS_PROMISE,          "<promise>");
    CINIT(SCM_CLASS_LAZY_PAIR,        "<lazy-pair>");

    /* read.c */
    BINIT(SCM_CLASS_READ_CONTEXT,     "<read-context>", NULL);
    BINIT(SCM_CLASS_READ_REFERENCE,   "<read-reference>", NULL);

    /* regexp.c */
    CINIT(SCM_CLASS_REGEXP,           "<regexp>");
    CINIT(SCM_CLASS_REGMATCH,         "<regmatch>");

    /* string.c */
    CINIT(SCM_CLASS_STRING,           "<string>");
    CINIT(SCM_CLASS_STRING_POINTER,   "<string-pointer>");

    /* symbol.c */
    CINIT(SCM_CLASS_SYMBOL,           "<symbol>");
    CINIT(SCM_CLASS_GLOC,             "<gloc>");
    CINIT(SCM_CLASS_KEYWORD,          "<keyword>");

    /* system.c */
    /* initialized in Scm__InitSystem */

    /* treemap.c */
    CINIT(SCM_CLASS_TREE_MAP,         "<tree-map>");

    /* vector.c */
    CINIT(SCM_CLASS_VECTOR,           "<vector>");
    CINIT(SCM_CLASS_UVECTOR,          "<uvector>");
    CINIT(SCM_CLASS_S8VECTOR,         "<s8vector>");
    CINIT(SCM_CLASS_U8VECTOR,         "<u8vector>");
    CINIT(SCM_CLASS_S16VECTOR,        "<s16vector>");
    CINIT(SCM_CLASS_U16VECTOR,        "<u16vector>");
    CINIT(SCM_CLASS_S32VECTOR,        "<s32vector>");
    CINIT(SCM_CLASS_U32VECTOR,        "<u32vector>");
    CINIT(SCM_CLASS_S64VECTOR,        "<s64vector>");
    CINIT(SCM_CLASS_U64VECTOR,        "<u64vector>");
    CINIT(SCM_CLASS_F16VECTOR,        "<f16vector>");
    CINIT(SCM_CLASS_F32VECTOR,        "<f32vector>");
    CINIT(SCM_CLASS_F64VECTOR,        "<f64vector>");

    /* weak.c */
    CINIT(SCM_CLASS_WEAK_VECTOR,      "<weak-vector>");
    CINIT(SCM_CLASS_WEAK_HASH_TABLE,  "<weak-hash-table>");

    /* write.c */
    BINIT(SCM_CLASS_WRITE_STATE,      "<write-state>", NULL);

#define GINIT(gf, nam) \
    Scm_InitBuiltinGeneric(gf, nam, mod);

    GINIT(&Scm_GenericMake, "make");
    GINIT(&Scm_GenericAllocate, "allocate-instance");
    GINIT(&Scm_GenericInitialize, "initialize");
    GINIT(&Scm_GenericAddMethod, "add-method!");
    GINIT(&Scm_GenericDeleteMethod, "delete-method!");
    GINIT(&Scm_GenericComputeCPL, "compute-cpl");
    GINIT(&Scm_GenericComputeSlots, "compute-slots");
    GINIT(&Scm_GenericComputeGetNSet, "compute-get-n-set");
    GINIT(&Scm_GenericComputeApplicableMethods, "compute-applicable-methods");
    GINIT(&Scm_GenericUpdateDirectMethod, "update-direct-method!");
    GINIT(&Scm_GenericMethodMoreSpecificP, "method-more-specific?");
    GINIT(&Scm_GenericApplyGeneric, "apply-generic");
    GINIT(&Scm_GenericSlotMissing, "slot-missing");
    GINIT(&Scm_GenericSlotUnbound, "slot-unbound");
    GINIT(&Scm_GenericSlotRefUsingClass, "slot-ref-using-class");
    GINIT(&Scm_GenericSlotSetUsingClass, "slot-set-using-class!");
    GINIT(&Scm_GenericSlotBoundUsingClassP, "slot-bound-using-class?");
    GINIT(&Scm_GenericObjectEqualP, "object-equal?");
    GINIT(&Scm_GenericObjectCompare, "object-compare");
    GINIT(&Scm_GenericObjectHash, "object-hash");
    GINIT(&Scm_GenericObjectApply, "object-apply");
    GINIT(&Scm_GenericObjectSetter, "setter of object-apply");
    GINIT(&Scm_GenericChangeClass, "change-class");

    Scm_SetterSet(SCM_PROCEDURE(&Scm_GenericObjectApply),
                  SCM_PROCEDURE(&Scm_GenericObjectSetter),
                  TRUE);

    Scm_InitBuiltinMethod(&class_allocate_rec);
    Scm_InitBuiltinMethod(&class_compute_cpl_rec);
    Scm_InitBuiltinMethod(&slot_ref_using_class_rec);
    Scm_InitBuiltinMethod(&slot_set_using_class_rec);
    Scm_InitBuiltinMethod(&slot_bound_using_class_p_rec);
    Scm_InitBuiltinMethod(&object_initialize_rec);
    Scm_InitBuiltinMethod(&generic_addmethod_rec);
    Scm_InitBuiltinMethod(&generic_deletemethod_rec);
    Scm_InitBuiltinMethod(&method_initialize_rec);
    Scm_InitBuiltinMethod(&accessor_method_initialize_rec);
    Scm_InitBuiltinMethod(&compute_applicable_methods_rec);
    Scm_InitBuiltinMethod(&generic_updatedirectmethod_rec);
    Scm_InitBuiltinMethod(&method_more_specific_p_rec);
    Scm_InitBuiltinMethod(&object_equalp_rec);
    Scm_InitBuiltinMethod(&object_compare_rec);
}
