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
 *  $Id: module.c,v 1.26 2002-05-12 06:35:20 shirok Exp $
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

/* Predefined modules */
static ScmModule *nullModule;
static ScmModule *schemeModule;
static ScmModule *gaucheModule;
static ScmModule *userModule;

/*----------------------------------------------------------------------
 * Constructor
 */

/* internal */
static ScmObj make_module(ScmSymbol *name, ScmModule *parent)
{
    ScmModule *z;
    z = SCM_NEW(ScmModule);
    SCM_SET_CLASS(z, SCM_CLASS_MODULE);
    z->name = name;
    z->parent = parent;
    z->imported = SCM_NIL;
    z->exported = SCM_NIL;
    z->table = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_ADDRESS, NULL, 0));

    Scm_HashTablePut(modules.table, SCM_OBJ(name), SCM_OBJ(z));
    return SCM_OBJ(z);
}

ScmObj Scm_MakeModule(ScmSymbol *name)
{
    ScmObj r;
    (void)SCM_INTERNAL_MUTEX_LOCK(&modules.mutex);
    r = make_module(name, gaucheModule);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(&modules.mutex);
    return r;
}

/*----------------------------------------------------------------------
 * Finding and modifying bindings
 */
/* TODO: MT safeness */

ScmGloc *Scm_FindBinding(ScmModule *module, ScmSymbol *symbol,
                         int stay_in_module)
{
    ScmHashEntry *e;
    ScmModule *m;
    ScmObj p;

    e = Scm_HashTableGet(module->table, SCM_OBJ(symbol));
    if (e) return SCM_GLOC(e->value);
    if (!stay_in_module) {
        /* First, search from imported modules */
        SCM_FOR_EACH(p, module->imported) {
            SCM_ASSERT(SCM_MODULEP(SCM_CAR(p)));
            m = SCM_MODULE(SCM_CAR(p));
            e = Scm_HashTableGet(m->table, SCM_OBJ(symbol));
            if (e &&
                (SCM_TRUEP(m->exported)
                 || !SCM_FALSEP(Scm_Memq(SCM_OBJ(symbol), m->exported))))
                return SCM_GLOC(e->value);
        }
        /* Then, search from parent module */
        for (m = module->parent; m; m = m->parent) {
            e = Scm_HashTableGet(m->table, SCM_OBJ(symbol));
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

ScmObj Scm_Define(ScmModule *module, ScmSymbol *symbol, ScmObj value)
{
    ScmGloc *g = Scm_FindBinding(module, symbol, TRUE);
    if (g) {
        SCM_GLOC_SET(g, value);
    } else {
        g = SCM_GLOC(Scm_MakeGloc(symbol, module));
        SCM_GLOC_SET(g, value);
        Scm_HashTablePut(module->table, SCM_OBJ(symbol), SCM_OBJ(g));
    }
    return SCM_OBJ(g);
}

ScmObj Scm_ImportModules(ScmModule *module, ScmObj list)
{
    ScmObj lp, head = SCM_NIL, tail = SCM_NIL, mod;
    SCM_FOR_EACH(lp, list) {
        if (!SCM_SYMBOLP(SCM_CAR(lp)))
            Scm_Error("module name required, but got %S", SCM_CAR(lp));
        mod = Scm_FindModule(SCM_SYMBOL(SCM_CAR(lp)), FALSE);
        if (!SCM_MODULEP(mod))
            Scm_Error("no such module: %S", SCM_CAR(lp));
        if (SCM_FALSEP(Scm_Memq(mod, head)))
            SCM_APPEND1(head, tail, mod);
    }
    SCM_APPEND(head, tail, module->imported);
    return (module->imported = head);
}

ScmObj Scm_ExportSymbols(ScmModule *module, ScmObj list)
{
    ScmObj lp, syms = module->exported;
    if (SCM_TRUEP(syms)) return syms; /* all symbols are exported */
    SCM_FOR_EACH(lp, list) {
        if (!SCM_SYMBOLP(SCM_CAR(lp)))
            Scm_Error("symbol required, but got %S", SCM_CAR(lp));
        if (SCM_FALSEP(Scm_Memq(SCM_CAR(lp), syms)))
            syms = Scm_Cons(SCM_CAR(lp), syms);
    }
    return (module->exported = syms);
}

ScmObj Scm_ExportAll(ScmModule *module)
{
    module->exported = SCM_TRUE;
    return SCM_OBJ(module);
}

/*----------------------------------------------------------------------
 * Finding modules
 */

ScmObj Scm_FindModule(ScmSymbol *name, int createp)
{
    ScmHashEntry *e;
    ScmObj m;

    (void)SCM_INTERNAL_MUTEX_LOCK(&modules.mutex);
    e = Scm_HashTableGet(modules.table, SCM_OBJ(name));
    if (e == NULL) {
        if (createp) m = make_module(name, gaucheModule);
        else m = SCM_FALSE;
    }
    else m = e->value;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(&modules.mutex);
    return m;
}

ScmObj Scm_AllModules(void)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    ScmHashIter iter;
    ScmHashEntry *e;

    (void)SCM_INTERNAL_MUTEX_LOCK(&modules.mutex);
    Scm_HashIterInit(modules.table, &iter);
    while ((e = Scm_HashIterNext(&iter)) != NULL) {
        SCM_APPEND1(h, t, e->value);
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(&modules.mutex);
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
    return nullModule;
}

ScmModule *Scm_SchemeModule(void)
{
    return schemeModule;
}

ScmModule *Scm_GaucheModule(void)
{
    return gaucheModule;
}

ScmModule *Scm_UserModule(void)
{
    return userModule;
}

ScmModule *Scm_CurrentModule(void)
{
    return Scm_VM()->module;
}

#define MAKEMOD(sym, parent) SCM_MODULE(make_module(SCM_SYMBOL(sym), parent))


void Scm__InitModule(void)
{
    (void)SCM_INTERNAL_MUTEX_INIT(&modules.mutex);
    modules.table = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_ADDRESS, NULL, 64));

    nullModule   = MAKEMOD(SCM_SYM_NULL, NULL);
    schemeModule = MAKEMOD(SCM_SYM_SCHEME, nullModule);
    gaucheModule = MAKEMOD(SCM_SYM_GAUCHE, schemeModule);
    userModule   = MAKEMOD(SCM_SYM_USER, gaucheModule);
}

