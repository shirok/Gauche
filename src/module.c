/*
 * module.c - module implementation
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
 *  $Id: module.c,v 1.7 2001-03-05 00:39:11 shiro Exp $
 */

#include "gauche.h"

/*
 * Modules
 *
 *   A module maps symbols to global locations.
 */

static int module_print(ScmObj obj, ScmPort *port, int mode)
{
    ScmModule *m = SCM_MODULE(obj);
    return Scm_Printf(port, "#<module %S>", m->name);
}

SCM_DEFCLASS(Scm_ModuleClass, "<module>", module_print,
             SCM_CLASS_COLLECTION_CPL);

static ScmHashTable *moduleTable; /* global, must be protected in MT env */

/*----------------------------------------------------------------------
 * Constructor
 */
ScmObj Scm_MakeModule(ScmSymbol *name, ScmObj parentList)
{
    ScmModule *z;
    ScmObj e;
    int pllen = Scm_Length(parentList), i = 0;

    /* Assertion */
    if (pllen < 0) Scm_Abort("improper list is given to Scm_MakeModule");
    SCM_FOR_EACH(e, parentList) {
        if (!SCM_MODULEP(SCM_CAR(e)))
            Scm_Abort("non-module is passed to Scm_MakeModule as a parent");
    }
    
    z = SCM_NEW(ScmModule);
    SCM_SET_CLASS(z, SCM_CLASS_MODULE);
    z->name = name;
    z->parents = Scm_CopyList(parentList);
    z->table = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_ADDRESS, NULL, 0));

    Scm_HashTablePut(moduleTable, SCM_OBJ(name), SCM_OBJ(z));

    return SCM_OBJ(z);
}

/*----------------------------------------------------------------------
 * Finding and modifying bindings
 */

ScmGloc *Scm_FindBinding(ScmModule *module, ScmSymbol *symbol,
                         int stay_in_module)
{
    ScmHashEntry *e = Scm_HashTableGet(module->table, SCM_OBJ(symbol));
    if (e) return SCM_GLOC(e->value);
    if (!stay_in_module) {
        ScmObj mod;
        SCM_FOR_EACH(mod, module->parents) {
            e = Scm_HashTableGet(SCM_MODULE(SCM_CAR(mod))->table,
                                 SCM_OBJ(symbol));
            if (e) return SCM_GLOC(e->value);
        }
    }
    return NULL;
}

ScmObj Scm_SymbolValue(ScmModule *module, ScmSymbol *symbol)
{
    ScmObj mod;
    ScmGloc *g = Scm_FindBinding(module, symbol, FALSE);
    return (g != NULL)? g->value : SCM_UNBOUND;
}

ScmObj Scm_Define(ScmModule *module, ScmSymbol *symbol, ScmObj value)
{
    ScmGloc *g = Scm_FindBinding(module, symbol, TRUE);
    if (g) {
        g->value = value;
    } else {
        g = SCM_GLOC(Scm_MakeGloc(symbol, module));
        g->value = value;
        Scm_HashTablePut(module->table, SCM_OBJ(symbol), SCM_OBJ(g));
    }
    return SCM_OBJ(g);
}

ScmObj Scm_GlobalSet(ScmModule *module, ScmSymbol *symbol, ScmObj value)
{
    ScmObj mod;
    ScmHashEntry *e = Scm_HashTableGet(module->table, SCM_OBJ(symbol));

    if (e) {
        SCM_GLOC(e->value)->value = value;
        return value;
    } else {
        SCM_FOR_EACH(mod, module->parents) {
            e = Scm_HashTableGet(SCM_MODULE(SCM_CAR(mod))->table,
                                 SCM_OBJ(symbol));
            if (e) {
                SCM_GLOC(e->value)->value = value;
                return value;
            }
        }
        {
            ScmGloc *g = SCM_GLOC(Scm_MakeGloc(symbol, module));
            g->value = value;
            Scm_HashTablePut(module->table, SCM_OBJ(symbol), SCM_OBJ(g));
            return value;
        }
    }
}

/*----------------------------------------------------------------------
 * Switching modules
 */

ScmObj Scm_FindModule(ScmSymbol *name)
{
    ScmHashEntry *e = Scm_HashTableGet(moduleTable, SCM_OBJ(name));
    if (e == NULL) return SCM_FALSE;
    else return e->value;
}

/*----------------------------------------------------------------------
 * Predefined modules and initialization
 */

static ScmModule *nullModule;
static ScmModule *schemeModule;
static ScmModule *gaucheModule;
static ScmModule *userModule;

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

#define MAKEMOD(sym, parent) \
    SCM_MODULE(Scm_MakeModule(SCM_SYMBOL(sym), parent))


void Scm__InitModule(void)
{
    moduleTable = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_ADDRESS, NULL, 64));

    nullModule   = MAKEMOD(SCM_SYM_NULL, SCM_NIL);
    schemeModule = MAKEMOD(SCM_SYM_SCHEME, SCM_LIST1(SCM_OBJ(nullModule)));
    gaucheModule = MAKEMOD(SCM_SYM_GAUCHE, SCM_LIST1(SCM_OBJ(schemeModule)));
    userModule   = MAKEMOD(SCM_SYM_USER, SCM_LIST1(SCM_OBJ(schemeModule)));
}

