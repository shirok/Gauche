/*
 * module.c - module implementation
 *
 *  Copyright(C) 2000-2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: module.c,v 1.39 2002-09-03 00:30:57 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"

/*
 * Modules
 *
 *  A module maps symbols to global locations.
 *  The mapping is resolved at the compile time.   Therefore,
 *  Scheme's current-module is therefore a syntax, instead of
 *  a procedure, to capture compile-time information.
 */

static void module_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<module %S>", SCM_MODULE(obj)->name);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_ModuleClass, module_print, NULL, NULL, NULL,
                         SCM_CLASS_COLLECTION_CPL);

/* Global module table */
static struct {
    ScmHashTable *table;
    ScmInternalMutex mutex;
} modules;

/* Predefined modules - slots will be initialized by Scm__InitModule */
#define DEFINE_STATIC_MODULE(cname) \
    static ScmModule cname = { { NULL } }

DEFINE_STATIC_MODULE(nullModule);
DEFINE_STATIC_MODULE(schemeModule);
DEFINE_STATIC_MODULE(gaucheModule);
DEFINE_STATIC_MODULE(gfModule);
DEFINE_STATIC_MODULE(userModule);

static ScmObj defaultParents = SCM_NIL; /* will be initialized */
static ScmObj defaultMpl = SCM_NIL;     /* will be initialized */

/*----------------------------------------------------------------------
 * Constructor
 */

static void init_module(ScmModule *m, ScmSymbol *name)
{
    m->name = name;
    m->imported = m->exported = SCM_NIL;
    m->parents = defaultParents;
    m->mpl = Scm_Cons(SCM_OBJ(m), defaultMpl);
    m->table = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_ADDRESS, NULL, 0));
    (void)SCM_INTERNAL_MUTEX_INIT(m->mutex);
    Scm_HashTablePut(modules.table, SCM_OBJ(name), SCM_OBJ(m));
}

/* Internal.  Caller is responsible to lock the global module table */
static ScmObj make_module(ScmSymbol *name)
{
    ScmModule *m;
    m = SCM_NEW(ScmModule);
    SCM_SET_CLASS(m, SCM_CLASS_MODULE);
    init_module(m, name);
    return SCM_OBJ(m);
}

ScmObj Scm_MakeModule(ScmSymbol *name)
{
    ScmObj r;
    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    r = make_module(name);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
    return r;
}

/*----------------------------------------------------------------------
 * Finding and modifying bindings
 */

ScmGloc *Scm_FindBinding(ScmModule *module, ScmSymbol *symbol,
                         int stay_in_module)
{
    ScmHashEntry *e;
    ScmModule *m = module;
    ScmObj p, mp, searched = SCM_NIL;

    /* fist, search from the specified module */
    (void)SCM_INTERNAL_MUTEX_LOCK(m->mutex);
    e = Scm_HashTableGet(m->table, SCM_OBJ(symbol));
    (void)SCM_INTERNAL_MUTEX_UNLOCK(m->mutex);
    if (e) return SCM_GLOC(e->value);
    
    if (!stay_in_module) {
        /* Next, search from imported modules */
        SCM_FOR_EACH(p, module->imported) {
            SCM_ASSERT(SCM_MODULEP(SCM_CAR(p)));
            SCM_FOR_EACH(mp, SCM_MODULE(SCM_CAR(p))->mpl) {
                SCM_ASSERT(SCM_MODULEP(SCM_CAR(mp)));
                if (!SCM_FALSEP(Scm_Memq(SCM_CAR(mp), searched))) break;
                
                m = SCM_MODULE(SCM_CAR(mp));
                (void)SCM_INTERNAL_MUTEX_LOCK(m->mutex);
                e = Scm_HashTableGet(m->table, SCM_OBJ(symbol));
                (void)SCM_INTERNAL_MUTEX_UNLOCK(m->mutex);
                if (e &&
                    (SCM_TRUEP(m->exported)
                     || !SCM_FALSEP(Scm_Memq(SCM_OBJ(symbol), m->exported)))) {
                    return SCM_GLOC(e->value);
                }
                searched = Scm_Cons(SCM_OBJ(m), searched);
            }
        }
        /* Then, search from parent modules */
        SCM_ASSERT(SCM_PAIRP(module->mpl));
        SCM_FOR_EACH(mp, SCM_CDR(module->mpl)) {
            SCM_ASSERT(SCM_MODULEP(SCM_CAR(mp)));
            m = SCM_MODULE(SCM_CAR(mp));
            (void)SCM_INTERNAL_MUTEX_LOCK(m->mutex);
            e = Scm_HashTableGet(m->table, SCM_OBJ(symbol));
            (void)SCM_INTERNAL_MUTEX_UNLOCK(m->mutex);
            if (e) return SCM_GLOC(e->value);
        }
    }
    return NULL;
}

ScmObj Scm_SymbolValue(ScmModule *module, ScmSymbol *symbol)
{
    ScmGloc *g = Scm_FindBinding(module, symbol, FALSE);
    if (g == NULL) return SCM_UNBOUND;
    else return SCM_GLOC_GET(g);
}

/*
 * Definition.
 */
ScmObj Scm_Define(ScmModule *module, ScmSymbol *symbol, ScmObj value)
{
    ScmGloc *g;
    ScmHashEntry *e;
    int redefining = FALSE;
    
    (void)SCM_INTERNAL_MUTEX_LOCK(module->mutex);
    e = Scm_HashTableGet(module->table, SCM_OBJ(symbol));
    if (e) {
        g = SCM_GLOC(e->value);
        if (SCM_GLOC_CONST_P(g)) {
            redefining = TRUE;
            g->setter = NULL;
        }
        SCM_GLOC_SET(g, value);
    } else {
        g = SCM_GLOC(Scm_MakeGloc(symbol, module));
        SCM_GLOC_SET(g, value);
        Scm_HashTablePut(module->table, SCM_OBJ(symbol), SCM_OBJ(g));
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(module->mutex);
    
    if (redefining) {
        Scm_Warn("redefining constant %S::%S", g->module, g->name);
    }
    return SCM_OBJ(g);
}

ScmObj Scm_DefineConst(ScmModule *module, ScmSymbol *symbol, ScmObj value)
{
    ScmGloc *g;
    ScmHashEntry *e;
    ScmObj oldval = SCM_UNDEFINED;
    int redefining = FALSE;

    (void)SCM_INTERNAL_MUTEX_LOCK(module->mutex);
    e = Scm_HashTableGet(module->table, SCM_OBJ(symbol));
    /* NB: this function bypasses check of gloc setter */
    if (e) {
        g = SCM_GLOC(e->value);
        if (SCM_GLOC_CONST_P(g)) {
            redefining = TRUE;
            oldval = g->value;
        }
        g->setter = Scm_GlocConstSetter;
        g->value  = value;
    } else {
        g = SCM_GLOC(Scm_MakeConstGloc(symbol, module));
        g->value = value;
        Scm_HashTablePut(module->table, SCM_OBJ(symbol), SCM_OBJ(g));
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(module->mutex);

    if (redefining && !Scm_EqualP(value, oldval)) {
        Scm_Warn("redefining constant %S::%S", g->module->name, g->name);
    }
    return SCM_OBJ(g);
}

ScmObj Scm_ImportModules(ScmModule *module, ScmObj list)
{
    ScmObj lp, mod;
    ScmSymbol *name = NULL;
    SCM_FOR_EACH(lp, list) {
        if (SCM_SYMBOLP(SCM_CAR(lp))) {
            name = SCM_SYMBOL(SCM_CAR(lp));
        } else if (SCM_IDENTIFIERP(SCM_CAR(lp))) {
            name = SCM_IDENTIFIER(SCM_CAR(lp))->name;
        } else {
            Scm_Error("module name required, but got %S", SCM_CAR(lp));
        }
        mod = Scm_FindModule(name, FALSE);
        if (!SCM_MODULEP(mod)) Scm_Error("no such module: %S", SCM_CAR(lp));
        (void)SCM_INTERNAL_MUTEX_LOCK(module->mutex);
        if (SCM_FALSEP(Scm_Memq(mod, module->imported))) {
            module->imported = Scm_Cons(mod, module->imported);
        }
        (void)SCM_INTERNAL_MUTEX_UNLOCK(module->mutex);
    }
    return module->imported;
}

ScmObj Scm_ExportSymbols(ScmModule *module, ScmObj list)
{
    ScmObj lp, syms, badsym = SCM_FALSE;
    int error = FALSE;

    (void)SCM_INTERNAL_MUTEX_LOCK(module->mutex);
    syms = module->exported;
    if (!SCM_TRUEP(syms)) {
        SCM_FOR_EACH(lp, list) {
            if (!SCM_SYMBOLP(SCM_CAR(lp))) {
                error = TRUE;
                badsym = SCM_CAR(lp);
                break;
            }
            if (SCM_FALSEP(Scm_Memq(SCM_CAR(lp), syms)))
                syms = Scm_Cons(SCM_CAR(lp), syms);
        }
        if (!error) module->exported = syms;
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(module->mutex);
    if (error) Scm_Error("symbol required, but got %S", badsym);
    return syms;
}

ScmObj Scm_ExportAll(ScmModule *module)
{
    (void)SCM_INTERNAL_MUTEX_LOCK(module->mutex);
    module->exported = SCM_TRUE;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(module->mutex);
    return SCM_OBJ(module);
}

/*----------------------------------------------------------------------
 * Extending (inheriting) modules
 */

/* Module inheritance obeys the same rule as class inheritance,
   hence we use monotonic merge. */
/* NB: ExtendModule alters module's precedence list, and may cause
   unwanted side effects when used carelessly.  */

static ScmObj mod_get_super(ScmObj module, void *data)
{
    return SCM_MODULE(module)->parents;
}

ScmObj Scm_ExtendModule(ScmModule *module, ScmObj supers)
{
    ScmObj mpl, seqh = SCM_NIL, seqt = SCM_NIL, sp;

    SCM_APPEND1(seqh, seqt, supers);
    SCM_FOR_EACH(sp, supers) {
        if (!SCM_MODULEP(SCM_CAR(sp))) {
            Scm_Error("non-module object found in the extend syntax: %S",
                      SCM_CAR(sp));
        }
        SCM_APPEND1(seqh, seqt, SCM_MODULE(SCM_CAR(sp))->mpl);
    }
    module->parents = supers;
    mpl = Scm_MonotonicMerge(SCM_OBJ(module), seqh, mod_get_super, NULL);
    if (SCM_FALSEP(mpl)) {
        Scm_Error("can't extend those modules simultaneously because of inconsistent precedence lists: %S", supers);
    }
    module->mpl = mpl;
    return mpl;
}

/*----------------------------------------------------------------------
 * Finding modules
 */

ScmObj Scm_FindModule(ScmSymbol *name, int createp)
{
    ScmHashEntry *e;
    ScmObj m;

    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    e = Scm_HashTableGet(modules.table, SCM_OBJ(name));
    if (e == NULL) {
        if (createp) m = make_module(name);
        else m = SCM_FALSE;
    }
    else m = e->value;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
    return m;
}

ScmObj Scm_AllModules(void)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    ScmHashIter iter;
    ScmHashEntry *e;

    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    Scm_HashIterInit(modules.table, &iter);
    while ((e = Scm_HashIterNext(&iter)) != NULL) {
        SCM_APPEND1(h, t, e->value);
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
    return h;
}

void Scm_SelectModule(ScmModule *mod)
{
    SCM_ASSERT(SCM_MODULEP(mod));
    Scm_VM()->module = mod;
}

/*----------------------------------------------------------------------
 * Predefined modules and initialization
 */

ScmModule *Scm_NullModule(void)
{
    return &nullModule;
}

ScmModule *Scm_SchemeModule(void)
{
    return &schemeModule;
}

ScmModule *Scm_GaucheModule(void)
{
    return &gaucheModule;
}

ScmModule *Scm_UserModule(void)
{
    return &userModule;
}

ScmModule *Scm_CurrentModule(void)
{
    return Scm_VM()->module;
}

#define INIT_MOD(mod, name, mpl)                                           \
    do {                                                                   \
        SCM_SET_CLASS(&mod, SCM_CLASS_MODULE);                             \
        init_module(&mod, SCM_SYMBOL(name));                               \
        mod.parents = (SCM_NULLP(mpl)? SCM_NIL : SCM_LIST1(SCM_CAR(mpl))); \
        mpl = mod.mpl = Scm_Cons(SCM_OBJ(&mod), mpl);                      \
    } while (0)

void Scm__InitModule(void)
{
    ScmObj mpl = SCM_NIL;

    (void)SCM_INTERNAL_MUTEX_INIT(modules.mutex);
    modules.table = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_ADDRESS, NULL, 64));

    INIT_MOD(nullModule, SCM_SYM_NULL, mpl);
    INIT_MOD(schemeModule, SCM_SYM_SCHEME, mpl);
    INIT_MOD(gaucheModule, SCM_SYM_GAUCHE, mpl);
    INIT_MOD(gfModule, SCM_SYM_GAUCHE_GF, mpl);
    INIT_MOD(userModule, SCM_SYM_USER, mpl);

    mpl = SCM_CDR(mpl);  /* default mpl doesn't include user module */
    defaultParents = SCM_LIST1(SCM_CAR(mpl));
    defaultMpl = mpl;
}
