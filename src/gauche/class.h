/*
 * class.h - Gauche object system header
 *
 *   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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
 * Class stuff
 */

/* Print procedure is used to print an instance of the class. */
typedef void (*ScmClassPrintProc)(ScmObj obj,
                                  ScmPort *sink,
                                  ScmWriteContext *mode);

/* Compare procedure is used for equality testing (equal?, Scm_EqualP)
   and ordering (compare, Scm_Compare).
   When called from equal?, EQUALP is TRUE.  It must return 0 if x and y
   is equal to each other, and non-zero if not.
   Wehn called from compare, EQUALP is FALSE.  it must return -1 if x is
   less than y, 0 if x is equal to y, and 1 if x is greater than y.
 */
typedef int  (*ScmClassCompareProc)(ScmObj x, ScmObj y, int equalp);

/* Compute a hash value of obj, using SALT.  See below for
   the possible value(s) of FLAGS.
 */
typedef ScmSmallInt (*ScmClassHashProc)(ScmObj obj, ScmSmallInt salt,
                      u_long flags);

/* Returns a new instance of KLASS.  KLASS is the class itself, or one
   of its subclasses.  The fallback method of allocate-instance examines
   this field in the classes in class precedence list.  You only need
   to set this when you want your class to be used in 'make' method.
 */
typedef ScmObj (*ScmClassAllocateProc)(ScmClass *klass, ScmObj initargs);

/* Flags value for ScmClassHashProc */
enum {
    SCM_HASH_PORTABLE = 1L<<0  /* must calculate a portable hash value,
                                  can be used for portable-hash. */
};


/* See class.c for the description of function pointer members.
   There's a lot of voodoo magic in class structure, so don't touch
   those fields casually.  Also, the order of these fields must be
   reflected to the class definition macros below. */
struct ScmClassRec {
    /* A trick to align statically allocated class structure on 8-byte
       boundary.  This doesn't guarantee, though, so we use __alignment__
       attribute as well, whenever possible (see SCM_ALIGN8 macro). */
    union {
        SCM_INSTANCE_HEADER;
        double align_dummy;
    } classHdr;
#if defined(GAUCHE_BROKEN_LINKER_WORKAROUND)
    ScmClass **classPtr;
#endif
    /* Some type-specific primitive methods.  Note that these take precedence
       than the generic function verison (write-object, object-compare etc.)
       See the typedefs above for the description.
    */
    ScmClassPrintProc     print;
    ScmClassCompareProc   compare;
    ScmClassHashProc      hash;
    ScmClassAllocateProc  allocate;

    ScmClass **cpa;             /* class precedence array, NULL terminated */
    int numInstanceSlots;       /* # of instance slots */
    int coreSize;               /* size of core structure; 0 == unknown */
    unsigned int flags;
    ScmObj name;                /* scheme name */
    ScmObj directSupers;        /* list of classes */
    ScmObj cpl;                 /* list of classes */
    ScmObj accessors;           /* alist of slot-name & slot-accessor */
    ScmObj directSlots;         /* alist of slot-name & slot-definition */
    ScmObj slots;               /* alist of slot-name & slot-definition */
    ScmObj directSubclasses;    /* list of direct subclasses */
    ScmObj directMethods;       /* list of methods that has this class in
                                   its specializer */
    ScmObj initargs;            /* saved key-value list for redefinition */
    ScmObj modules;             /* modules where this class is defined */
    ScmObj redefined;           /* if this class is obsoleted by class
                                   redefinition, points to the new class.
                                   if this class is being redefined, points
                                   to a thread that is handling the
                                   redefinition.  (it won't be seen by
                                   Scheme; see class.c)
                                   otherwise #f */
    ScmInternalMutex mutex;     /* to protect from MT hazard */
    ScmInternalCond cv;         /* wait on this while a class being updated */
    void   *data;               /* extra data to do nasty trick.  See the note
                                   in class.c */
} SCM_ALIGN8;

typedef struct ScmClassStaticSlotSpecRec ScmClassStaticSlotSpec;

#define SCM_CLASS(obj)        ((ScmClass*)(obj))
#define SCM_CLASSP(obj)       SCM_ISA(obj, SCM_CLASS_CLASS)

#define SCM_CLASS_NUM_INSTANCE_SLOTS(obj)  SCM_CLASS(obj)->numInstanceSlots

/* Class categories

   In C level, there are four categories of classes.  The category of
   class can be obtained by masking the lower two bits of flags field.

   SCM_CLASS_BUILTIN
       An instance of this class doesn't have "slots" member (thus
       cannot be cast to ScmInstance*).   From Scheme level, this
       class cannot be redefined.   It cannot be inherited in Scheme
       code with the standard inheritance mechanism; though it can have
       subclasses, provided a special allocator and initializer.

   SCM_CLASS_ABSTRACT
       This class is defined in C, but doesn't allowed to create an
       instance by its own.  It is intended to be used as a mixin from
       both C and Scheme-defined class.   An instance of this class
       shouldn't have C members other than SCM_HEADER.
       This class cannot be redefined.

   SCM_CLASS_BASE
       This class is defined in C, and can be subclassed in Scheme.
       An instance of this class must have "slots" member and be
       able to be cast to ScmInstance.  The instance may have other
       C members.  This class cannot be redefined.

   SCM_CLASS_SCHEME
       A Scheme-defined class.  This class will have one or more
       SCM_CLASS_BASE classes in its CPL.  Specifically, <object>
       class is always included in its CPL.  This class can be
       redefined.

   This classification and its rules are to integrate C structures
   and Scheme classes.   C structure level inheritance has to be
   single-inheritance, with the subclass structure including its
   parent structure.  Scheme level inheritance is more flexible,
   but for that flexibility it has to have "slots" member in its
   instance (i.e. it has to be castable to ScmInstance*).

   Here's the basic inheritance rules:

   - First, ABSTRACT class can be inserted at any place in the
     inheritance chain.  It doesn't affect C-level operation.  It is
     only to add the type information in Scheme-level.
     In the following rules we ignore ABSTRACT classes.

   - BASE class can be inherited from BASE classes, and its
     inheritance chain must form a single inheritance.

   - BUILTIN class can be inherited from BUILTIN classes, and
     its inheritance chain must form a single inheritance

   - SCHEME class can be inherited from SCHEME or BASE classes.
     It can inherite from multiple SCHEME and/or BASE classes.
*/

enum {
    SCM_CLASS_BUILTIN  = 0,
    SCM_CLASS_ABSTRACT = 1,
    SCM_CLASS_BASE     = 2,
    SCM_CLASS_SCHEME   = 3,

    /* A special flag that only be used for "natively applicable"
       objects, which basically inherits ScmProcedure. */
    SCM_CLASS_APPLICABLE = 0x04,

    /* If this flag is set, important slots such as class-precedence-list
       or class-slots becomes settable.
       We reset this flag at the end of class initialization, so that
       we can avoid the behavior of a class from being accidentally
       changed.  The flag may be set during updating a class metaobject
       triggered by metaclass change (see lib/gauche/redefutil.scm).
     */
    SCM_CLASS_MALLEABLE = 0x08,

    /* This flag indicates the class is for the aggregate data type.
       Currently the writer uses this info to determine when to stop
       recursing (see print-level).  We may use this later for generic
       data structure walker. */
    SCM_CLASS_AGGREGATE = 0x10
};

#define SCM_CLASS_FLAGS(obj)        (SCM_CLASS(obj)->flags)
#define SCM_CLASS_APPLICABLE_P(obj) (SCM_CLASS_FLAGS(obj)&SCM_CLASS_APPLICABLE)

#define SCM_CLASS_CATEGORY(obj)     (SCM_CLASS_FLAGS(obj)&3)
#define SCM_CLASS_MALLEABLE_P(obj)  (SCM_CLASS_FLAGS(obj)&SCM_CLASS_MALLEABLE)

SCM_EXTERN void Scm_InitStaticClass(ScmClass *klass, const char *name,
                                    ScmModule *mod,
                                    ScmClassStaticSlotSpec *slots,
                                    int flags);
SCM_EXTERN void Scm_InitStaticClassWithSupers(ScmClass *klass,
                                              const char *name,
                                              ScmModule *mod,
                                              ScmObj supers,
                                              ScmClassStaticSlotSpec *slots,
                                              int flags);
SCM_EXTERN void Scm_InitStaticClassWithMeta(ScmClass *klass,
                                            const char *name,
                                            ScmModule *mod,
                                            ScmClass *meta,
                                            ScmObj supers,
                                            ScmClassStaticSlotSpec *slots,
                                            int flags);
SCM_EXTERN ScmObj Scm_ShortClassName(ScmClass *klass); /* strip '<' and '>' */

/* Use this in 'compare' slot to allow Scheme method to define
   compare/equal? behavior thru object-compare/object-equal? */
SCM_EXTERN int Scm_ObjectCompare(ScmObj x, ScmObj y, int equalp);

/* OBSOLETE */
SCM_EXTERN void Scm_InitBuiltinClass(ScmClass *c, const char *name,
                                     ScmClassStaticSlotSpec *slots,
                                     int withMeta,
                                     ScmModule *m);

SCM_EXTERN ScmClass *Scm_ClassOf(ScmObj obj);
SCM_EXTERN ScmClass *Scm_BaseClassOf(ScmClass *klass);

#if GAUCHE_API_VERSION >= 98
SCM_EXTERN int Scm_SubclassP(ScmClass *sub, ScmClass *type);
#define Scm_SubtypeP(sub, type)   Scm_SubclassP(sub, type)
#else  /*GAUCHE_API_VERSION < 98*/
#define Scm_SubclassP(sub, type)  Scm_SubtypeP(sub, type)
SCM_EXTERN int Scm_SubtypeP(ScmClass *sub, ScmClass *type);
#endif /*GAUCHE_API_VERSION < 98*/

SCM_EXTERN int Scm_TypeP(ScmObj obj, ScmClass *type);

SCM_EXTERN int    Scm_TypeConstructorP(ScmObj obj);
/* This is called from init routie of precompiled code */
SCM_EXTERN ScmObj Scm_ConstructType(ScmObj ctor, ScmObj args);

SCM_EXTERN void   Scm_ClassMalleableSet(ScmClass *klass, int flag);

SCM_EXTERN ScmObj Scm_VMSlotRef(ScmObj obj, ScmObj slot, int boundp);
SCM_EXTERN ScmObj Scm_VMSlotSet(ScmObj obj, ScmObj slot, ScmObj value);
SCM_EXTERN ScmObj Scm_VMSlotBoundP(ScmObj obj, ScmObj slot);


/* built-in classes */
SCM_CLASS_DECL(Scm_TopClass);
SCM_CLASS_DECL(Scm_BottomClass);
SCM_CLASS_DECL(Scm_BoolClass);
SCM_CLASS_DECL(Scm_CharClass);
SCM_CLASS_DECL(Scm_TypeClass);
SCM_CLASS_DECL(Scm_ClassClass);
SCM_CLASS_DECL(Scm_EOFObjectClass);
SCM_CLASS_DECL(Scm_UndefinedObjectClass);
SCM_CLASS_DECL(Scm_UnknownClass);
SCM_CLASS_DECL(Scm_ObjectClass); /* base of Scheme-defined objects */
SCM_CLASS_DECL(Scm_ForeignPointerClass);


#define SCM_CLASS_TOP              (&Scm_TopClass)
#define SCM_CLASS_BOTTOM           (&Scm_BottomClass)
#define SCM_CLASS_BOOL             (&Scm_BoolClass)
#define SCM_CLASS_CHAR             (&Scm_CharClass)
#define SCM_CLASS_TYPE             (&Scm_TypeClass)
#define SCM_CLASS_CLASS            (&Scm_ClassClass)
#define SCM_CLASS_EOF_OBJECT       (&Scm_EOFObjectClass)
#define SCM_CLASS_UNDEFINED_OBJECT (&Scm_UndefinedObjectClass)
#define SCM_CLASS_UNKNOWN          (&Scm_UnknownClass)
#define SCM_CLASS_OBJECT           (&Scm_ObjectClass)
#define SCM_CLASS_FOREIGN_POINTER  (&Scm_ForeignPointerClass)

/* NB: we can't use SCM_EXTERN because Windows DLL can't use the address of
   dllimport-ed variables as constants. */
extern ScmClass *Scm_DefaultCPL[];
extern ScmClass *Scm_ObjectCPL[];
extern ScmClass *Scm_MetaclassCPL[];
extern ScmClass *Scm_DescriptiveTypeCPL[];

#define SCM_CLASS_DEFAULT_CPL       (Scm_DefaultCPL)
#define SCM_CLASS_OBJECT_CPL        (Scm_ObjectCPL)
#define SCM_CLASS_METACLASS_CPL     (Scm_MetaclassCPL)
#define SCM_CLASS_TYPE_CPL          (Scm_MetaclassCPL+2)

/* Static definition of classes
 *   SCM_DEFINE_BUILTIN_CLASS
 *   SCM_DEFINE_BUILTIN_CLASS_FLAGS
 *   SCM_DEFINE_BUILTIN_CLASS_SIMPLE
 *   SCM_DEFINE_ABSTRACT_CLASS
 *   SCM_DEFINE_BASE_CLASS
 */

/* internal macro.  do not use directly */
#if defined(GAUCHE_BROKEN_LINKER_WORKAROUND)
#define SCM__CLASS_PTR_SLOT(cname)  (&SCM_CPP_CAT(_imp__, cname)),
#define SCM__CLASS_PTR_BODY(cname) \
    ; ScmClass *SCM_CPP_CAT(_imp__, cname) = &cname
#else  /*!GAUCHE_BROKEN_LINKER_WORKAROUND*/
#define SCM__CLASS_PTR_SLOT(cname)  /* none */
#define SCM__CLASS_PTR_BODY(cname)  /* none */
#endif /*!GAUCHE_BROKEN_LINKER_WORKAROUND*/

#define SCM__DEFINE_CLASS_COMMON(cname, coreSize, flag, printer, compare, hash, allocate, cpa) \
    ScmClass cname = {                           \
        {{ SCM_CLASS_STATIC_TAG(Scm_ClassClass), NULL }},       \
        SCM__CLASS_PTR_SLOT(cname)               \
        printer,                                 \
        compare,                                 \
        hash,                                    \
        allocate,                                \
        cpa,                                     \
        0,        /*numInstanceSlots*/           \
        coreSize, /*coreSize*/                   \
        flag,     /*flags*/                      \
        SCM_FALSE,/*name*/                       \
        SCM_NIL,  /*directSupers*/               \
        SCM_NIL,  /*cpl*/                        \
        SCM_NIL,  /*accessors*/                  \
        SCM_NIL,  /*directSlots*/                \
        SCM_NIL,  /*slots*/                      \
        SCM_NIL,  /*directSubclasses*/           \
        SCM_NIL,  /*directMethods*/              \
        SCM_NIL,  /*initargs*/                   \
        SCM_NIL,  /*modules*/                    \
        SCM_FALSE, /*redefined*/                 \
        SCM_INTERNAL_MUTEX_INITIALIZER,          \
        SCM_INTERNAL_COND_INITIALIZER,           \
        NULL       /* data */                    \
    } SCM__CLASS_PTR_BODY(cname)

/* Define built-in class statically -- full-featured version */
#define SCM_DEFINE_BUILTIN_CLASS(cname, printer, compare, hash, allocate, cpa) \
    SCM__DEFINE_CLASS_COMMON(cname, 0,                    \
                             SCM_CLASS_BUILTIN,           \
                             printer, compare, hash, allocate, cpa)

#define SCM_DEFINE_BUILTIN_CLASS_FLAGS(cname, printer, compare, hash, allocate, cpa, flags) \
    SCM__DEFINE_CLASS_COMMON(cname, 0,                                  \
                             SCM_CLASS_BUILTIN|(flags),                 \
                             printer, compare, hash, allocate, cpa)

/* Define built-in class statically -- simpler version */
#define SCM_DEFINE_BUILTIN_CLASS_SIMPLE(cname, printer)         \
    SCM_DEFINE_BUILTIN_CLASS(cname, printer, NULL, NULL, NULL, NULL)

/* define an abstract class */
#define SCM_DEFINE_ABSTRACT_CLASS(cname, cpa)             \
    SCM__DEFINE_CLASS_COMMON(cname, 0,                    \
                             SCM_CLASS_ABSTRACT,          \
                             NULL, NULL, NULL, NULL, cpa)

/* define a class that can be subclassed by Scheme */
#define SCM_DEFINE_BASE_CLASS(cname, ctype, printer, compare, hash, allocate, cpa) \
    SCM__DEFINE_CLASS_COMMON(cname, sizeof(ctype),        \
                             SCM_CLASS_BASE,              \
                             printer, compare, hash, allocate, cpa)

/*
 * Foreign pointer class
 */


/*
 * A simple class and instance API to wrap C pointer.
 * This is for C programs that want to define a visible class from Scheme
 * but don't want to go through full-fledged class mechanism.
 */
typedef struct ScmForeignPointerRec {
    SCM_HEADER;
    void *ptr;                  /* foreign object.  this pointer shouldn't
                                   be modified once <foreign-pointer> is
                                   constructed by Scm_MakeForeignPointer. */
    ScmObj attributes;          /* alist.  useful to store e.g. callbacks.
                                   use accessor procedures. */
    ScmWord flags;              /* used internally.  We use ScmWord to keep
                                   ScmForeignPointer fit in 4 words. */
} ScmForeignPointer;

#define SCM_FOREIGN_POINTER_P(obj)   SCM_ISA(obj, SCM_CLASS_FOREIGN_POINTER)
#define SCM_FOREIGN_POINTER(obj)     ((ScmForeignPointer*)(obj))
#define SCM_FOREIGN_POINTER_REF(type, obj) \
    ((type)(Scm_ForeignPointerRef(SCM_FOREIGN_POINTER(obj))))

typedef void (*ScmForeignCleanupProc)(ScmObj);

SCM_EXTERN ScmClass *Scm_MakeForeignPointerClass(ScmModule *module,
                                                 const char *name,
                                                 ScmClassPrintProc print,
                                                 ScmForeignCleanupProc cleanup,
                                                 int flags);
SCM_EXTERN ScmObj Scm_MakeForeignPointer(ScmClass *klass, void *ptr);
SCM_EXTERN ScmObj Scm_MakeForeignPointerWithAttr(ScmClass *klass, void *ptr,
                                                 ScmObj attr);
SCM_EXTERN void  *Scm_ForeignPointerRef(ScmForeignPointer *fp);
SCM_EXTERN int    Scm_ForeignPointerInvalidP(ScmForeignPointer *fp);
SCM_EXTERN void   Scm_ForeignPointerInvalidate(ScmForeignPointer *fp);

/* foreign pointer class flags */
enum {
    SCM_FOREIGN_POINTER_KEEP_IDENTITY = (1L<<0),
         /* If set, a foreign pointer class keeps a weak hash table that maps
            PTR to the wrapping ScmObj, so Scm_MakeForeignPointer returns
            eq? object if the same PTR is given.  This incurs some overhead,
            but cleanup procedure can safely free the foreign object without
            worring if there's other ScmObj that's pointing to PTR.
            Do not use this flag if PTR is also allocated by GC_malloc.  The
            used hash table is only weak for its value, so PTR wouldn't be
            GCed. */
    SCM_FOREIGN_POINTER_MAP_NULL = (1L<<1)
         /* If set, Scm_MakeForeignPointer returns SCM_FALSE whenever the
            given PTR is NULL.   It is the only case that
            Scm_MakeForeignPointer returns non-ForeignPointer object. */
};

/* foreign pointer attributes.  you can attach info to each foreign pointer.
   possible applications:
   - Keep Scheme objects that are set in the foreign object, preventing
     them from begin GCed.
   - Keep mutex to use the foreign object from multiple threads */

SCM_EXTERN ScmObj Scm_ForeignPointerAttr(ScmForeignPointer *fp);
SCM_EXTERN ScmObj Scm_ForeignPointerAttrGet(ScmForeignPointer *fp,
                                            ScmObj key, ScmObj fallback);
SCM_EXTERN ScmObj Scm_ForeignPointerAttrSet(ScmForeignPointer *fp,
                                            ScmObj key, ScmObj value);

/*
 * Proxy types
 */

/* A proxy type is to hold a reference to a class.
   Used in other descriptive types to handle redefinition of the original class.
   The actual definition and construtor is in gauche/priv/classP.h */
typedef struct ScmProxyTypeRec ScmProxyType;

SCM_CLASS_DECL(Scm_ProxyTypeClass);
#define SCM_CLASS_PROXY_TYPE      (&Scm_ProxyTypeClass)
#define SCM_PROXY_TYPE(obj)       ((ScmProxyType*)(obj))
#define SCM_PROXY_TYPE_P(obj)     (SCM_XTYPEP(obj, SCM_CLASS_PROXY_TYPE))

/* Since ProxyType is instantiated at compile-time, the constructor
   is called only from the compiler or initializer (in the case proxytype
   is serialized into compiled code).   We put Scm_MakeProxyType decl here
   so that the initializer of precompiled code can see it.

   The REF argument is technically redundant, since it is a binding of ID.
   But the compile has looked up ID already, so we can just use it.
   We trust the caller giving the proer REF.
   REF can be NULL when invoked from the initializer; in that case
   we derive it from ID, but lazily (in Scm_ProxyTypeRef).
 */
SCM_EXTERN ScmObj    Scm_MakeProxyType(ScmIdentifier *id, ScmGloc *ref);
SCM_EXTERN ScmClass *Scm_ProxyTypeRef(ScmProxyType *p);
SCM_EXTERN ScmObj    Scm_ProxyTypeId(ScmProxyType *p);

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

#define SCM_CLASS_SLOT_SPEC_END()   SCM_CLASS_SLOT_SPEC(NULL, NULL, NULL)

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

#if GAUCHE_API_VERSION < 98
SCM_EXTERN ScmObj Scm_InstanceSlotRef(ScmObj obj, ScmSmallInt k);
SCM_EXTERN ScmObj Scm_InstanceSlotRef3(ScmObj obj, ScmSmallInt k,
                                       ScmObj fallback);
#else  /*GAUCHE_API_VERSION >= 98*/
SCM_EXTERN ScmObj Scm_InstanceSlotRef(ScmObj obj, ScmSmallInt k,
                                      ScmObj fallback);
#endif /*GAUCHE_API_VERSION >= 98*/

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

SCM_EXTERN ScmGeneric Scm_GenericApplyGeneric;
SCM_EXTERN ScmGeneric Scm_GenericObjectHash;
SCM_EXTERN ScmGeneric Scm_GenericObjectApply;
SCM_EXTERN ScmGeneric Scm_GenericObjectEqualP;
SCM_EXTERN ScmGeneric Scm_GenericObjectSetter;
SCM_EXTERN ScmGeneric Scm_GenericChangeClass;

SCM_EXTERN ScmObj Scm_UpdateDirectMethod(ScmMethod *m,
                                         ScmClass *oldk,
                                         ScmClass *newk);

#if GAUCHE_API_VERSION < 1000
/* TRANSIENT: Obsoleted.  Use SCM_NEW_INSTANCE */
#define SCM_ALLOCATE(klassname, klass)  SCM_NEW_INSTANCE(klassname, klass)
#endif /*GAUCHE_API_VERSION < 1000*/

#if GAUCHE_API_VERSION < 98
/* TRANSIENT: Obsoleted. */
SCM_EXTERN ScmObj Scm_ObjectAllocate(ScmClass *klass, ScmObj initargs);
/* TRANSIENT: Obsoleted.  Use Scm_NewInstance*/
SCM_EXTERN ScmObj Scm_AllocateInstance(ScmClass *klass, int coresize);
#endif /*GAUCHE_API_VERSION < 98*/

SCM_DECL_END

#endif /* GAUCHE_CLASS_H */
