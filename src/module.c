/*
 * module.c - module implementation
 *
 *   Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
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
 *
 *  $Id: module.c,v 1.48 2004-04-24 09:58:14 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/builtin-syms.h"

/*
 * Modules
 *
 *  A module maps symbols to global locations.
 *  The mapping is resolved at the compile time.
 *  Scheme's current-module is therefore a syntax, instead of
 *  a procedure, to capture compile-time information.
 *
 *  Modules are registered to global hash table using their names
 *  as keys, so that the module is retrieved by its name.  The exception
 *  is "anonymous modules", which have '#' as the name field
 *  and not registered in the global table.   Anonymous modules are especially
 *  useful for certain applications that need temporary, segregated
 *  namespace---for example, a 'sandbox' environment to evaluate an
 *  expression sent over the network during a session.
 *  The anonymous namespace will be garbage-collected if nobody references
 *  it, recovering its resouces.
 */

static ScmObj anon_module_name = SCM_UNBOUND; /* Name used for anonymous
                                                 modules.  Symbol '#',
                                                 set by init */

static void module_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<module %A>", SCM_MODULE(obj)->name);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_ModuleClass,
                         module_print, NULL, NULL, NULL,
                         SCM_CLASS_COLLECTION_CPL);

/* Global module table */
static struct {
    ScmHashTable *table;    /* Maps name -> module. */
    ScmInternalMutex mutex; /* Lock for table.  Only register_module and
                               lookup_module may hold the lock. */
} modules = { NULL };

/* Predefined modules - slots will be initialized by Scm__InitModule */
#define DEFINE_STATIC_MODULE(cname) \
    static ScmModule cname = { { NULL } }

DEFINE_STATIC_MODULE(nullModule);
DEFINE_STATIC_MODULE(schemeModule);
DEFINE_STATIC_MODULE(gaucheModule);
DEFINE_STATIC_MODULE(gfModule);
DEFINE_STATIC_MODULE(userModule);

static ScmObj defaultParents = SCM_NIL; /* will be initialized */
static ScmObj defaultMpl =     SCM_NIL; /* will be initialized */

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
}

/* Internal */
static ScmObj make_module(ScmSymbol *name)
{
    ScmModule *m;
    m = SCM_NEW(ScmModule);
    SCM_SET_CLASS(m, SCM_CLASS_MODULE);
    init_module(m, name);
    return SCM_OBJ(m);
}

/* Internal.  Lookup module with name N from the table. */
static ScmModule *lookup_module(ScmSymbol *name)
{
    ScmHashEntry *e;
    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    e = Scm_HashTableGet(modules.table, SCM_OBJ(name));
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
    if (e) return SCM_MODULE(e->value);
    else return NULL;
}

/* Internal.  Lookup module, and if there's none, create one. */
static ScmModule *lookup_module_create(ScmSymbol *name, int *created)
{
    ScmHashEntry *e;
    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    e = Scm_HashTableAdd(modules.table, SCM_OBJ(name), SCM_FALSE);
    if (e->value == SCM_FALSE) {
        e->value = make_module(name);
        *created = TRUE;
    } else {
        *created = FALSE;
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
    return SCM_MODULE(e->value);
}

ScmObj Scm_MakeModule(ScmSymbol *name, int error_if_exists)
{
    ScmObj r;
    if (name == NULL) name = SCM_SYMBOL(anon_module_name);
    if (SCM_EQ(SCM_OBJ(name), anon_module_name)) {
        r = make_module(name);
    } else {
        int created;
        r = SCM_OBJ(lookup_module_create(name, &created));
        if (!created) {
            if (error_if_exists) {
                Scm_Error("couldn't create module '%S': named module already exists",
                          SCM_OBJ(name));
            } else {
                r = SCM_FALSE;
            }
        }
    }
    return r;
}

/*----------------------------------------------------------------------
 * Finding and modifying bindings
 */

#define SEARCHED_ARRAY_SIZE  64

ScmGloc *Scm_FindBinding(ScmModule *module, ScmSymbol *symbol,
                         int stay_in_module)
{
    ScmHashEntry *e;
    ScmModule *m = module;
    ScmObj p, mp;

    /* keep record of searched modules.  we use stack array for small # of
       modules, in order to avoid consing for common cases. */
    ScmObj searched[SEARCHED_ARRAY_SIZE];
    int num_searched = 0, i;
    ScmObj more_searched = SCM_NIL;

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
                
                for (i=0; i<num_searched; i++) {
                    if (SCM_EQ(SCM_CAR(mp), searched[i])) goto skip;
                }
                if (i == num_searched) {
                    if (!SCM_FALSEP(Scm_Memq(SCM_CAR(mp), more_searched))) {
                        goto skip;
                    }
                }
                
                m = SCM_MODULE(SCM_CAR(mp));
                (void)SCM_INTERNAL_MUTEX_LOCK(m->mutex);
                e = Scm_HashTableGet(m->table, SCM_OBJ(symbol));
                (void)SCM_INTERNAL_MUTEX_UNLOCK(m->mutex);
                if (e &&
                    (SCM_TRUEP(m->exported)
                     || !SCM_FALSEP(Scm_Memq(SCM_OBJ(symbol), m->exported)))) {
                    return SCM_GLOC(e->value);
                }

                if (num_searched < SEARCHED_ARRAY_SIZE) {
                    searched[num_searched++] = SCM_OBJ(m);
                } else {
                    more_searched = Scm_Cons(SCM_OBJ(m), more_searched);
                }
            }
          skip:;
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
        module->imported =
            Scm_Cons(mod, Scm_DeleteX(mod, module->imported, SCM_CMP_EQ));
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

ScmObj Scm_ExtendModule(ScmModule *module, ScmObj supers)
{
    ScmObj mpl, seqh = SCM_NIL, seqt = SCM_NIL, sp;

    SCM_FOR_EACH(sp, supers) {
        if (!SCM_MODULEP(SCM_CAR(sp))) {
            Scm_Error("non-module object found in the extend syntax: %S",
                      SCM_CAR(sp));
        }
        SCM_APPEND1(seqh, seqt, SCM_MODULE(SCM_CAR(sp))->mpl);
    }
    SCM_APPEND1(seqh, seqt, supers);
    module->parents = supers;
    mpl = Scm_MonotonicMerge(SCM_OBJ(module), seqh);
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
    ScmModule *m;
    int created;

    if (createp) {
        m = lookup_module_create(name, &created);
    } else {
        m = lookup_module(name);
    }
    if (m) return SCM_OBJ(m);
    else return SCM_FALSE;
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
 * Module and pathnames
 */

/* Convert module name and pathname (mod load-path) and vice versa.
   The default conversion is pretty straightforward, e.g.
   util.list <=> "util/list"  etc.  However, modules and files can
   have many-to-many mapping, and I'd like to reserve the room
   of future extensions.   Eventually there will be some special
   mapping table so the programmer can register exceptional mappings. */

ScmObj Scm_ModuleNameToPath(ScmSymbol *name)
{
    char *buf = SCM_NEW_ATOMIC2(char *, SCM_STRING_SIZE(name->name)+1);
    char *p = buf, *e = buf + SCM_STRING_SIZE(name->name);
    memcpy(buf, SCM_STRING_START(name->name), SCM_STRING_SIZE(name->name));
    while (p < e) {
        int n = SCM_CHAR_NFOLLOWS(*p);
        if (*p == '.') *p++ = '/';
        else p += n+1;
    }
    *e = '\0';
    return Scm_MakeString(buf, SCM_STRING_SIZE(name->name),
                          SCM_STRING_LENGTH(name->name), 0);
}

ScmObj Scm_PathToModuleName(ScmString *path)
{
    char *buf = SCM_NEW_ATOMIC2(char *, SCM_STRING_SIZE(path)+1);
    char *p = buf, *e = buf + SCM_STRING_SIZE(path);
    memcpy(buf, SCM_STRING_START(path), SCM_STRING_SIZE(path));
    while (p < e) {
        int n = SCM_CHAR_NFOLLOWS(*p);
        if (*p == '/') *p++ = '.';
        else if (*p == '.') Scm_Error("bad pathname for module path: %S", path);
        else p += n+1;
    }
    *e = '\0';
    return SCM_INTERN(buf);
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

/* NB: we don't need to lock the global module table in initialization */
#define INIT_MOD(mod, mname, mpl)                                           \
    do {                                                                    \
        SCM_SET_CLASS(&mod, SCM_CLASS_MODULE);                              \
        init_module(&mod, SCM_SYMBOL(mname));                               \
        Scm_HashTablePut(modules.table, SCM_OBJ((mod).name), SCM_OBJ(&mod));\
        mod.parents = (SCM_NULLP(mpl)? SCM_NIL : SCM_LIST1(SCM_CAR(mpl)));  \
        mpl = mod.mpl = Scm_Cons(SCM_OBJ(&mod), mpl);                       \
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

    anon_module_name = SCM_SYM_SHARP;
}
