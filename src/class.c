/*
 * class.c - class metaobject implementation
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
 *  $Id: class.c,v 1.12 2001-03-16 10:54:17 shiro Exp $
 */

#include "gauche.h"
#include "gauche/class.h"

/*
 * Built-in classes
 */

static int class_print(ScmObj, ScmPort *, int);

ScmClass *Scm_DefaultCPL[] = { SCM_CLASS_TOP, NULL };
ScmClass *Scm_CollectionCPL[] = {
    SCM_CLASS_COLLECTION, SCM_CLASS_TOP, NULL
};
ScmClass *Scm_SequenceCPL[] = {
    SCM_CLASS_SEQUENCE, SCM_CLASS_COLLECTION, SCM_CLASS_TOP, NULL
};
ScmClass *Scm_ObjectCPL[] = {
    SCM_CLASS_OBJECT, SCM_CLASS_TOP, NULL
};

SCM_DEFCLASS(Scm_TopClass,     "<top>",     NULL, NULL); /* will be patched */
SCM_DEFCLASS(Scm_BoolClass,    "<bool>",    NULL, SCM_CLASS_DEFAULT_CPL);
SCM_DEFCLASS(Scm_CharClass,    "<char>",    NULL, SCM_CLASS_DEFAULT_CPL);
SCM_DEFCLASS(Scm_UnknownClass, "<unknown>", NULL, SCM_CLASS_DEFAULT_CPL);

/* Intercessory classes */
SCM_DEFCLASS(Scm_CollectionClass, "<collection>", NULL, SCM_CLASS_DEFAULT_CPL);
SCM_DEFCLASS(Scm_SequenceClass, "<sequence>", NULL, SCM_CLASS_COLLECTION_CPL);
SCM_DEFCLASS(Scm_ObjectClass, "<object>", NULL, SCM_CLASS_OBJECT_CPL);

/* Class metaobject needs customized initialization, which is done
   in Scm__InitClass(). */
SCM_DEFCLASS(Scm_ClassClass, "<class>", class_print, SCM_CLASS_OBJECT_CPL);

/* Internally used classes */
SCM_DEFCLASS(Scm_ClassAccessorClass, "<class-accessor>", NULL,
             SCM_CLASS_DEFAULT_CPL);

/* Some frequently-used pointers */
static ScmObj key_allocation;
static ScmObj key_instance;

/*=====================================================================
 * Auxiliary classes
 */

/*=====================================================================
 * Class metaobject
 */

/* One of the design goals of Gauche object system is to make Scheme-defined
 * class easily accessible from C code, and vice versa.
 *
 * Classes in Gauche fall into four categories: core final class, core
 * base class, core abstract class and Scheme class.  A C-defined class
 * may belong to one of the first three, while a Scheme-defined class is
 * always a Scheme class.
 *
 * Core final classes are the ones that represents basic objects of the
 * language, such as <integer> or <port>.   Those classes are just
 * the way to reify the basic object, and don't follow object protorol;
 * for example, you can't overload "initialize" method specialized to
 * <integer> to customize initialization of integers, nor subclass <integer>,
 * although you can use them to specialize methods you write.  "Make" methods
 * for these objects are dispatched to the appropriate C functions.
 *
 * Core base classes are the ones from which you can derive Scheme classes.
 * <class>, <generic-method> and <method> are in this category.  The instance
 * of those classes have extra slots that contains C-specific data, such
 * as function pointers.  You can subclass them, but there is one restriction:
 * There can't be more than one core base class in the class' superclasses.
 * Because of this fact, C routines can take the pointer to the instance
 * of subclasses and safely cast it to oen of the core base classes.
 *
 * Core abstract classes are just for method specialization.  They can't
 * create instances directly, and they shouldn't have any direct slots.
 * <top>, <object> and <sequence> are in this category, among others.
 *
 * Since a class must be <class> itself or its descendants, C code can
 * treat them as ScmClass*, and can determine the category of the class.
 *
 * Depending on its category, a class must or may provide those function
 * pointers:
 *
 *   Category:  core final     core base     core abstract 
 *   -----------------------------------------------------
 *    allocate   required       optional        NULL
 *    apply      (*1)           NULL            NULL
 *    print      optional       optional        ignored
 *    equal      optional       optional        ignored
 *    compare    optional       optional        ignored
 *    serialize  optional       optional        ignored
 *
 *  (*1: required for applicable classes, must be NULL otherwise)
 *
 * If the function is optional, you can set NULL there and the system
 * uses default function.  For Scheme class the system sets appropriate
 * functions.   See the following section for details of these function
 * potiners.
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
 *  ScmObj klass->apply(ScmObj obj, ScmObj args)
 *     Called from the VM engine to initiate application.
 *
 *  int klass->print(ScmObj obj, ScmPort *sink, int mode)
 *     OBJ is an instance of klass (you can safely assume it).  This
 *     function should print OBJ into SINK, and returns number of characters
 *     output to SINK.   MODE can be SCM_PRINT_DISPLAY for display(),
 *     SCM_PRINT_WRITE for write(), or SCM_PRINT_DEBUG for more precise
 *     debug information.
 *     If this function pointer is not set, a default print method
 *     is used.
 *
 *  int klass->equal(ScmObj x, ScmObj y)
 *     X and Y are instances of klass.  This function should return TRUE iff
 *     X equals Y, FALSE otherwise.   If this function pointer is not set,
 *     Gauche uses pointer comparison to see their equality.
 *
 *  int klass->compare(ScmObj x, ScmObj y)
 *     X and Y are instances of klass or its descendants.  If the objects
 *     are fully orderable, this function returns either -1, 0 or 1, depending
 *     X preceding Y, X being equal to Y, or X following Y, respectively.
 *     If the objects are not fully orderable, just returns 0.
 *     If this function pointer is not set, Gauche assumes objects are
 *     not orderable.
 *
 *  int klass->serialize(ScmObj obj, ScmPort *sink, ScmObj table)
 *     OBJ is an instance of klass.  This method is only called when OBJ
 *     has not been output in the current serializing session.
 */

/*
 * Class metaobject protocol implementation
 */

/* Allocate class structure.  klass is a metaclass. */
ScmObj class_allocate(ScmClass *klass, int nslots)
{
    ScmClass *instance;
    int i;

    SCM_ASSERT(nslots >= 0);
    instance = SCM_NEW2(ScmClass*,
                        sizeof(ScmClass) + sizeof(ScmObj)*nslots);
    SCM_SET_CLASS(instance, klass);
    instance->name = "(uninitialized)";
    instance->print = class_print;
    instance->equal = NULL;
    instance->compare = NULL;
    instance->serialize = NULL; /* class_serialize? */
    instance->allocate = class_allocate;
    instance->apply = NULL;
    instance->cpa = NULL;
    instance->numInstanceSlots = nslots;
    instance->flags = 0;        /* ?? */
    instance->sname = SCM_FALSE;
    instance->directSupers = SCM_NIL;
    instance->cpl = SCM_NIL;
    instance->directSlots = SCM_NIL;
    instance->slots = SCM_NIL;
    instance->directSubclasses = SCM_NIL;
    instance->directMethods = SCM_NIL;

    if (nslots > 0) {
        ScmSubClass *s = (ScmSubClass*)instance;
        for (i=0; i<nslots; i++) s->slots[i] = SCM_UNBOUND;
    }
    return SCM_OBJ(instance);
}

/* fallback print method */
static int class_print(ScmObj obj, ScmPort *port, int mode) 
{
    ScmClass *c = (ScmClass*)obj;
    return Scm_Printf(port, "#<class %A>", Scm_ClassName(c));
}

/*
 * Get class
 */

ScmClass *Scm_ClassOf(ScmObj obj)
{
    if (!SCM_PTRP(obj)) {
        if (SCM_TRUEP(obj) || SCM_FALSEP(obj)) return SCM_CLASS_BOOL;
        if (SCM_NULLP(obj)) return SCM_CLASS_NULL;
        if (SCM_CHARP(obj)) return SCM_CLASS_CHAR;
        if (SCM_INTP(obj))  return SCM_CLASS_INTEGER;
        else return SCM_CLASS_UNKNOWN;
    } else {
        return obj->klass;
    }
}

/*====================================================================
 * Metainformation accessors
 */
/* TODO: disable modification of system-builtin classes */

ScmObj Scm_ClassName(ScmClass *klass)
{
    if (SCM_FALSEP(klass->sname) && klass->name) {
        klass->sname = SCM_INTERN(klass->name);
    }
    return klass->sname;
}

static void class_name_set(ScmClass *klass, ScmObj val)
{
    klass->sname = val;
}

ScmObj Scm_ClassCPL(ScmClass *klass)
{
    /* TODO: MT safeness */
    if (!SCM_PAIRP(klass->cpl)) {
        /* This is the case of builtin class.  The second head of cpl is
           always the only direct superclass. */
        ScmClass **p = klass->cpa;
        ScmObj h = SCM_NIL, t;
        SCM_APPEND1(h, t, SCM_OBJ(klass));
        while (*p) {
            SCM_APPEND1(h, t, SCM_OBJ(*p));
            p++;
        }
        klass->cpl = h;
        if (SCM_PAIRP(SCM_CDR(h))) {
            klass->directSupers = SCM_LIST1(SCM_CADR(h));
        } else {
            klass->directSupers = SCM_NIL;
        }
    }
    return klass->cpl;
}

static void class_cpl_set(ScmClass *klass, ScmObj val)
{
    ScmObj cp;
    if (!SCM_PAIRP(val)) goto err;
    if (SCM_CAR(val) != SCM_OBJ(klass)) goto err;
    SCM_FOR_EACH(cp, val) {
        if (!Scm_TypeP(SCM_CAR(cp), SCM_CLASS_CLASS)) goto err;
        if (SCM_NULLP(SCM_CDR(cp)) && SCM_CAR(cp) != SCM_OBJ(SCM_CLASS_TOP))
            goto err;
    }
    if (!SCM_NULLP(cp)) goto err;
    klass->cpl = Scm_CopyList(val);
    return;
  err:
    Scm_Error("class precedence list must be a proper list of class "
              "metaobject, begenning from the class itself owing the list, "
              "and ending by the class <top>: %S", val);
}

ScmObj Scm_ClassDirectSupers(ScmClass *klass)
{
    if (!SCM_PAIRP(klass->directSupers)) {
        Scm_ClassCPL(klass);    /* set directSupers */
    }
    return klass->directSupers;
}

static void class_direct_supers_set(ScmClass *klass, ScmObj val)
{
    /* TODO: check argument vailidity */
    klass->directSupers = val;
}

ScmObj Scm_ClassDirectSlots(ScmClass *klass)
{
    return klass->directSlots;
}

static void class_direct_slots_set(ScmClass *klass, ScmObj val)
{
    /* TODO: check argument vailidity */
    klass->directSlots = val;
}

ScmObj Scm_ClassSlots(ScmClass *klass)
{
    return klass->slots;
}

static void class_slots_set(ScmClass *klass, ScmObj val)
{
    /* TODO: check argument vailidity */
    klass->slots = val;
}

ScmObj Scm_ClassAccessorTable(ScmClass *klass)
{
    return klass->accessorTable;
}

static void class_accessor_table_set(ScmClass *klass, ScmObj val)
{
    if (!SCM_FALSEP(val) && !SCM_HASHTABLEP(val))
        Scm_Error("only hashtable or #f allowed: %S", val);
    klass->accessorTable = val;
}

ScmObj Scm_ClassDirectSubclasses(ScmClass *klass)
{
    return klass->directSubclasses;
}

static void class_direct_subclasses_set(ScmClass *klass, ScmObj val)
{
    /* TODO: check argument vailidity */
    klass->directSubclasses = val;
}

static ScmObj class_numfields(ScmClass *klass)
{
    klass->numInstanceSlots;
}

static void class_numfields_set(ScmClass *klass, ScmObj snf)
{
    int nf;
    if (!SCM_INTP(nf) || (nf = SCM_INT_VALUE(nf) < 0)) {
        Scm_Error("invalid argument: %S", snf);
    }
    klass->numInstanceSlots = nf;
}

/*================================================================
 * External interface
 */

int Scm_SubtypeP(ScmClass *sub, ScmClass *type)
{
    ScmClass **p;
    if (sub == type) return TRUE;

    p = sub->cpa;
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
static ScmObj compute_cpl_cb(ScmObj k, void *dummy)
{
    return SCM_CLASS(k)->directSupers;
}

ScmObj Scm_ComputeCPL(ScmClass *klass, ScmObj directSupers)
{
    ScmObj seqh = SCM_NIL, seqt, dp, result, cp;
    int cpllen, i;
    
    SCM_FOR_EACH(dp, directSupers) {
        if (!SCM_CLASSP(SCM_CAR(dp)))
            Scm_Error("non-class found in direct superclass list: %S", directSupers);
        SCM_APPEND1(seqh, seqt, Scm_ClassCPL(SCM_CLASS(SCM_CAR(dp))));
    }
    result = Scm_MonotonicMerge(SCM_OBJ(klass), seqh, compute_cpl_cb, NULL);
    if (SCM_FALSEP(result))
        Scm_Error("discrepancy found in class precedence lists of the superclasses: %S",
                  directSupers);
    return result;
}

/* allocate-instance fallback */
ScmObj Scm_AllocateInstance(ScmObj klass, ScmObj initargs)
{
    ScmClass *c;
    if (!Scm_TypeP(klass, SCM_CLASS_CLASS))
        Scm_Error("can't allocate instance of non class: %S", klass);
    c = SCM_CLASS(klass);
    if (c->allocate == NULL) {
        Scm_Error("built-in class can't be allocated via allocate-instance: %S",
                  klass);
    }
    return c->allocate(c, c->numInstanceSlots);
}

/*=====================================================================
 * Slot accessors
 */

ScmObj Scm_SlotRef(ScmObj obj, ScmObj slot)
{
    ScmClass *klass = SCM_CLASS_OF(obj);
    ScmObj atab = klass->accessorTable;
    if (SCM_HASHTABLEP(atab)) {
        ScmHashEntry *e = Scm_HashTableGet(SCM_HASHTABLE(atab), slot);
        if (e) {
            ScmObj acc = e->value;
            if (SCM_XTYPEP(acc, SCM_CLASS_CLASS_ACCESSOR)) {
                ScmClassAccessor *ca = SCM_CLASS_ACCESSOR(acc);
                if (ca->getter) {
                    return ca->getter(obj);
                } else {
                    return SCM_FALSE; /* should signal error? */
                }
            }
            Scm_Error("slot-ref: not implemented yet.");
        }
    }
#ifdef NOT_IMPLEMENTED_YET
    return Scm_ApplyGeneric(SCM_GF_SLOT_MISSING,
                            SCM_LIST3(SCM_OBJ(klass), obj, slot));
#else
    Scm_Error("no slot %S in class %S", slot, SCM_OBJ(klass));
    return SCM_UNDEFINED;
#endif
}

void Scm_SlotSet(ScmObj obj, ScmObj slot, ScmObj val)
{
    ScmClass *klass = SCM_CLASS_OF(obj);
    ScmObj atab = klass->accessorTable;
    if (SCM_HASHTABLEP(atab)) {
        ScmHashEntry *e = Scm_HashTableGet(SCM_HASHTABLE(atab), slot);
        if (e) {
            ScmObj acc = e->value;
            if (SCM_XTYPEP(acc, SCM_CLASS_CLASS_ACCESSOR)) {
                ScmClassAccessor *ca = SCM_CLASS_ACCESSOR(acc);
                if (ca->setter) {
                    ca->setter(obj, val);
                } else {
                    Scm_Error("slot %S of class %S is read-only",
                              slot, SCM_OBJ(klass));
                }
            } else {
                Scm_Error("slot-set!: not implemented yet.");
            }
        }
        return;
    }
#ifdef NOT_IMPLEMENTED_YET
    Scm_ApplyGeneric(SCM_GF_SLOT_MISSING,
                     SCM_LIST4(SCM_OBJ(klass), obj, slot, val));
#else
    Scm_Error("no slot %S in class %S", slot, SCM_OBJ(klass));
#endif
}

ScmObj Scm_GetSlotAllocation(ScmObj slot)
{
    if (SCM_PAIRP(slot)) {
        return Scm_GetKeyword(key_allocation, SCM_CDR(slot), key_instance);
    } else {
        return key_instance;
    }
}

/*=====================================================================
 * Generic function
 */


/*=====================================================================
 * Method
 */


/*=====================================================================
 * Class initialization
 */

/* TODO: need a cleaner way! */
/* static declaration of some structures */

static ScmClassStaticSlotSpec class_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", &Scm_ClassClass,
                        Scm_ClassName, class_name_set),
    SCM_CLASS_SLOT_SPEC("cpl", &Scm_ClassClass,
                        Scm_ClassCPL, class_cpl_set),
    SCM_CLASS_SLOT_SPEC("direct-supers", &Scm_ClassClass,
                        Scm_ClassDirectSupers, class_direct_supers_set),
    SCM_CLASS_SLOT_SPEC("accessor-table", &Scm_ClassClass,
                        Scm_ClassAccessorTable, class_accessor_table_set),
    SCM_CLASS_SLOT_SPEC("slots", &Scm_ClassClass,
                        Scm_ClassSlots, class_slots_set),
    SCM_CLASS_SLOT_SPEC("direct-slots", &Scm_ClassClass,
                        Scm_ClassDirectSlots, class_direct_slots_set),
    SCM_CLASS_SLOT_SPEC("direct-subclasses", &Scm_ClassClass,
                        Scm_ClassDirectSubclasses,
                        class_direct_subclasses_set),
    SCM_CLASS_SLOT_SPEC("num-fields", &Scm_ClassClass,
                        class_numfields, class_numfields_set),
    SCM_CLASS_SLOT_SPEC(NULL, NULL, NULL, NULL)
};

/* booting class metaobject */
void bootstrap_class(ScmClass *k)
{
    ScmHashTable *atab;
    ScmObj slots = SCM_NIL, t;
    ScmClassStaticSlotSpec *specs = class_slots;

    k->allocate = class_allocate;
    k->accessorTable = Scm_MakeHashTable(SCM_HASH_ADDRESS, NULL, 8);
    atab = SCM_HASHTABLE(k->accessorTable);
    for (;specs->name; specs++) {
        ScmObj snam = SCM_INTERN(specs->name);
        Scm_HashTablePut(atab, snam, SCM_OBJ(&specs->accessor));
        SCM_APPEND1(slots, t, snam);
    }
    k->directSlots = k->slots = slots;
}

void Scm__InitClass(void)
{
    ScmModule *mod = Scm_SchemeModule();
    ScmClass *nullcpa[] = { NULL }; /* for <top> */

    key_allocation = SCM_MAKE_KEYWORD("allocation");
    key_instance = SCM_MAKE_KEYWORD("instance");

    /* booting class metaobject */
    Scm_TopClass.cpa = nullcpa;
    bootstrap_class(&Scm_ClassClass);
    
    
    SCM_DEFINE(mod, "<top>",      SCM_OBJ(SCM_CLASS_TOP));
    SCM_DEFINE(mod, "<boolean>",  SCM_OBJ(SCM_CLASS_BOOL));
    SCM_DEFINE(mod, "<char>",     SCM_OBJ(SCM_CLASS_CHAR));
    SCM_DEFINE(mod, "<unknown>",  SCM_OBJ(SCM_CLASS_UNKNOWN));
    SCM_DEFINE(mod, "<class>",    SCM_OBJ(SCM_CLASS_CLASS));
    SCM_DEFINE(mod, "<collection>", SCM_OBJ(SCM_CLASS_COLLECTION));
    SCM_DEFINE(mod, "<sequence>", SCM_OBJ(SCM_CLASS_SEQUENCE));
    SCM_DEFINE(mod, "<string>",   SCM_OBJ(SCM_CLASS_STRING));
    SCM_DEFINE(mod, "<symbol>",   SCM_OBJ(SCM_CLASS_SYMBOL));
    SCM_DEFINE(mod, "<gloc>",     SCM_OBJ(SCM_CLASS_GLOC));
    SCM_DEFINE(mod, "<syntax>",   SCM_OBJ(SCM_CLASS_SYNTAX));
    SCM_DEFINE(mod, "<port>",     SCM_OBJ(SCM_CLASS_PORT));
    SCM_DEFINE(mod, "<list>",     SCM_OBJ(SCM_CLASS_LIST));
    SCM_DEFINE(mod, "<pair>",     SCM_OBJ(SCM_CLASS_PAIR));
    SCM_DEFINE(mod, "<null>",     SCM_OBJ(SCM_CLASS_NULL));
    SCM_DEFINE(mod, "<vector>",   SCM_OBJ(SCM_CLASS_VECTOR));
    SCM_DEFINE(mod, "<hash-table>", SCM_OBJ(SCM_CLASS_HASHTABLE));
    SCM_DEFINE(mod, "<module>",   SCM_OBJ(SCM_CLASS_MODULE));
    SCM_DEFINE(mod, "<number>",   SCM_OBJ(SCM_CLASS_NUMBER));
    SCM_DEFINE(mod, "<complex>",  SCM_OBJ(SCM_CLASS_COMPLEX));
    SCM_DEFINE(mod, "<real>",     SCM_OBJ(SCM_CLASS_REAL));
    SCM_DEFINE(mod, "<integer>",  SCM_OBJ(SCM_CLASS_INTEGER));
    SCM_DEFINE(mod, "<procedure>", SCM_OBJ(SCM_CLASS_PROCEDURE));
    SCM_DEFINE(mod, "<closure>",  SCM_OBJ(SCM_CLASS_CLOSURE));
    SCM_DEFINE(mod, "<subr>",     SCM_OBJ(SCM_CLASS_SUBR));
    SCM_DEFINE(mod, "<vm>",       SCM_OBJ(SCM_CLASS_VM));
    SCM_DEFINE(mod, "<source-info>", SCM_OBJ(SCM_CLASS_SOURCE_INFO));
}
