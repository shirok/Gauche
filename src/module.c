/*
 * module.c - module implementation
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, ditribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: module.c,v 1.3 2001-02-05 08:23:49 shiro Exp $
 */

#include "gauche.h"

/*
 * Modules
 *
 *   A module maps symbols to global loations.
 */

static ScmClass *collection_cpl[] = {
    SCM_CLASS_COLLECTION, SCM_CLASS_TOP, NULL
};

static int module_print(ScmObj obj, ScmPort *port, int mode)
{
    ScmModule *m = SCM_MODULE(obj);
    return Scm_Printf(port, "#<module %S>", m->name);
}

ScmClass Scm_ModuleClass = {
    SCM_CLASS_CLASS,
    "<module>",
    module_print,
    collection_cpl
};

/*
 * Constructor
 */
ScmObj Scm_MakeModule(ScmString *name, ScmObj parentList)
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
    z->hdr.klass = SCM_CLASS_MODULE;
    z->name = SCM_STRING(Scm_CopyString(name));
    z->parents = Scm_CopyList(parentList);
    z->table = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_ADDRESS, NULL, 0));
    return SCM_OBJ(z);
}

/*
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
    return SCM_OBJ(symbol);
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

/*
 * Predefined modules and initialization
 */

static ScmModule *schemeModule;
static ScmModule *userModule;

ScmModule *Scm_SchemeModule(void)
{
    return schemeModule;
}

ScmModule *Scm_UserModule(void)
{
    return userModule;
}

void Scm__InitModule(void)
{
    ScmString *Sscheme = SCM_STRING(Scm_MakeStringConst("scheme", -1, -1));
    ScmString *Suser = SCM_STRING(Scm_MakeStringConst("user", -1, -1));
    schemeModule = SCM_MODULE(Scm_MakeModule(Sscheme, SCM_NIL));
    userModule = SCM_MODULE(Scm_MakeModule(Suser,
                                           Scm_Cons(SCM_OBJ(schemeModule),
                                                    SCM_NIL)));
}

