/*
 * module.c - module implementation
 *
 *   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
 *  $Id: module.c,v 1.69 2007-08-10 01:19:36 shirok Exp $
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

/* Mutex of module operation
 *
 * [SK] Each module used to have a mutex for accesses to it.  I changed it
 * to use a single global lock (modules.mutex), based on the following
 * observations:
 *
 *  - Profiling showed mutex_lock was taking around 10% of program loading
 *    phase in the previous version.
 *
 *  - Module operations almost always occur during program loading and
 *    interactive session.  Having giant lock for module operations won't
 *    affect normal runtime performance.
 *
 * Benchmark showed the change made program loading 30% faster.
 */

static void module_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<module %A>", SCM_MODULE(obj)->name);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_ModuleClass,
                         module_print, NULL, NULL, NULL,
                         SCM_CLASS_COLLECTION_CPL);

/* Global module table */
static struct {
    ScmObj anon_name;       /* Name used for anonymous modules.
                               Symbol '#', set by init */
    ScmHashTable *table;    /* Maps name -> module. */
    ScmInternalMutex mutex; /* Lock for table.  Only register_module and
                               lookup_module may hold the lock. */
} modules = { SCM_UNBOUND, NULL };

/* Predefined modules - slots will be initialized by Scm__InitModule */
#define DEFINE_STATIC_MODULE(cname) \
    static ScmModule cname = { { NULL } }

DEFINE_STATIC_MODULE(nullModule);     /* #<module null> */
DEFINE_STATIC_MODULE(schemeModule);   /* #<module scheme> */
DEFINE_STATIC_MODULE(gaucheModule);   /* #<module gauche> */
DEFINE_STATIC_MODULE(internalModule); /* #<module gauche.internal> */
DEFINE_STATIC_MODULE(gfModule);       /* #<module gauche.gf> */
DEFINE_STATIC_MODULE(userModule);     /* #<module user> */

static ScmObj defaultParents = SCM_NIL; /* will be initialized */
static ScmObj defaultMpl =     SCM_NIL; /* will be initialized */

/*----------------------------------------------------------------------
 * Constructor
 */

static void init_module(ScmModule *m, ScmSymbol *name)
{
    m->name = name;
    m->imported = m->exported = m->depended = SCM_NIL;
    m->exportAll = FALSE;
    m->parents = defaultParents;
    m->mpl = Scm_Cons(SCM_OBJ(m), defaultMpl);
    m->table = SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_EQ, 0));
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
    ScmObj v;
    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    v = Scm_HashTableRef(modules.table, SCM_OBJ(name), SCM_UNBOUND);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
    if (SCM_UNBOUNDP(v)) return NULL;
    else return SCM_MODULE(v);
}

/* Internal.  Lookup module, and if there's none, create one. */
static ScmModule *lookup_module_create(ScmSymbol *name, int *created)
{
    ScmDictEntry *e;
    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    e = Scm_HashCoreSearch(SCM_HASH_TABLE_CORE(modules.table),
                           (intptr_t)name,
                           SCM_DICT_CREATE);
    if (e->value == 0) {
        (void)SCM_DICT_SET_VALUE(e, make_module(name));
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
    if (name == NULL) name = SCM_SYMBOL(modules.anon_name);
    if (SCM_EQ(SCM_OBJ(name), modules.anon_name)) {
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
                         int flags)
{
    ScmModule *m = module;
    ScmObj v, p, mp;
    ScmGloc *gloc = NULL;
    int stay_in_module = flags&SCM_BINDING_STAY_IN_MODULE;

    /* keep record of searched modules.  we use stack array for small # of
       modules, in order to avoid consing for typical cases. */
    ScmObj searched[SEARCHED_ARRAY_SIZE];
    int num_searched = 0, i;
    ScmObj more_searched = SCM_NIL;

    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);

    /* first, search from the specified module.
       NB: we directly check gloc->value instead of calling
       SCM_GLOC_GET, since this check is merely to eliminate
       the GLOC inserted by export. */
    v = Scm_HashTableRef(m->table, SCM_OBJ(symbol), SCM_FALSE);
    if (SCM_GLOCP(v)) {
        gloc = SCM_GLOC(v);
        if (!SCM_UNBOUNDP(gloc->value)) goto found;
    }
    
    if (!stay_in_module) {
        /* Next, search from imported modules */
        SCM_FOR_EACH(p, module->imported) {
            SCM_ASSERT(SCM_MODULEP(SCM_CAR(p)));
            SCM_FOR_EACH(mp, SCM_MODULE(SCM_CAR(p))->mpl) {
                ScmGloc *g;
                
                SCM_ASSERT(SCM_MODULEP(SCM_CAR(mp)));
                
                for (i=0; i<num_searched; i++) {
                    if (SCM_EQ(SCM_CAR(mp), searched[i])) goto skip;
                }
                if (!SCM_NULLP(more_searched)) {
                    if (!SCM_FALSEP(Scm_Memq(SCM_CAR(mp), more_searched))) {
                        goto skip;
                    }
                }
                
                m = SCM_MODULE(SCM_CAR(mp));
                v = Scm_HashTableRef(m->table, SCM_OBJ(symbol), SCM_FALSE);
                /* see above comment about the check of gloc->value */
                if (SCM_GLOCP(v) && (g = SCM_GLOC(v))->exported
                    && !SCM_UNBOUNDP(g->value)) {
                    gloc = g;
                    goto found;
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
            v = Scm_HashTableRef(m->table, SCM_OBJ(symbol), SCM_FALSE);
            if (SCM_GLOCP(v)) { gloc = SCM_GLOC(v); goto found; }
        }
    }
  found:
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
    return gloc;
}

ScmObj Scm_GlobalVariableRef(ScmModule *module,
                             ScmSymbol *symbol,
                             int flags)
{
    ScmGloc *g = Scm_FindBinding(module, symbol, flags);
    ScmObj val;
    
    if (g == NULL) return SCM_UNBOUND;
    val = SCM_GLOC_GET(g);
    if (!(flags & SCM_BINDING_KEEP_AUTOLOAD)
        && SCM_AUTOLOADP(val)) {
        val = Scm_LoadAutoload(SCM_AUTOLOAD(val));
    }
    return val;
}

/*
 * Definition.
 *  TODO: consolidate the common code between Scm_Define and Scm_DefineConst.
 */
ScmObj Scm_Define(ScmModule *module, ScmSymbol *symbol, ScmObj value)
{
    ScmGloc *g;
    ScmObj v;
    int redefining = FALSE;
    
    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    v = Scm_HashTableRef(module->table, SCM_OBJ(symbol), SCM_FALSE);
    if (SCM_GLOCP(v)) {
        g = SCM_GLOC(v);
        if (SCM_GLOC_CONST_P(g)) {
            redefining = TRUE;
            g->setter = NULL;
        }
        SCM_GLOC_SET(g, value);
    } else {
        g = SCM_GLOC(Scm_MakeGloc(symbol, module));
        SCM_GLOC_SET(g, value);
        Scm_HashTableSet(module->table, SCM_OBJ(symbol), SCM_OBJ(g), 0);
        /* If module is marked 'export-all', export this binding by default */
        if (module->exportAll) {
            g->exported = TRUE;
            module->exported = Scm_Cons(SCM_OBJ(g->name), module->exported);
        }
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
    
    if (redefining) {
        Scm_Warn("redefining constant %S::%S", g->module, g->name);
    }
    return SCM_OBJ(g);
}

ScmObj Scm_DefineConst(ScmModule *module, ScmSymbol *symbol, ScmObj value)
{
    ScmGloc *g;
    ScmObj v;
    ScmObj oldval = SCM_UNDEFINED;
    int redefining = FALSE;

    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    v = Scm_HashTableRef(module->table, SCM_OBJ(symbol), SCM_FALSE);
    /* NB: this function bypasses check of gloc setter */
    if (SCM_GLOCP(v)) {
        g = SCM_GLOC(v);
        if (SCM_GLOC_CONST_P(g)) {
            redefining = TRUE;
            oldval = g->value;
        }
        g->setter = Scm_GlocConstSetter;
        g->value  = value;
    } else {
        g = SCM_GLOC(Scm_MakeConstGloc(symbol, module));
        g->value = value;
        Scm_HashTableSet(module->table, SCM_OBJ(symbol), SCM_OBJ(g), 0);
        /* If module is marked 'export-all', export this binding by default */
        if (module->exportAll) {
            g->exported = TRUE;
            module->exported = Scm_Cons(SCM_OBJ(g->name), module->exported);
        }
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);

    if (redefining && !Scm_EqualP(value, oldval)) {
        Scm_Warn("redefining constant %S::%S", g->module->name, g->name);
    }
    return SCM_OBJ(g);
}

ScmObj Scm_ImportModules(ScmModule *module, ScmObj list)
{
    ScmObj lp;
    ScmModule *mod;
    ScmSymbol *name = NULL;
    SCM_FOR_EACH(lp, list) {
        if (SCM_SYMBOLP(SCM_CAR(lp))) {
            name = SCM_SYMBOL(SCM_CAR(lp));
        } else if (SCM_IDENTIFIERP(SCM_CAR(lp))) {
            name = SCM_IDENTIFIER(SCM_CAR(lp))->name;
        } else {
            Scm_Error("module name required, but got %S", SCM_CAR(lp));
        }
        mod = Scm_FindModule(name, 0);
        (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
        module->imported =
            Scm_Cons(SCM_OBJ(mod),
                     Scm_DeleteX(SCM_OBJ(mod), module->imported, SCM_CMP_EQ));
        (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
    }
    return module->imported;
}

ScmObj Scm_ExportSymbols(ScmModule *module, ScmObj list)
{
    ScmObj lp, syms, badsym = SCM_FALSE;
    int error = FALSE;
    ScmSymbol *s;
    ScmDictEntry *e;
    ScmGloc *g;

    /* We used to do something like
     *  (set! (module-exports module)
     *        (delete-duplicates (union (module-exports module) list)))
     * This is slow when we export lots of symbols.  As of 0.8.6,
     * each GLOC has exported flag, so we can check whether a binding
     * is exported or not in O(1).   Module-exports list is kept
     * for backward compatibility.
     */
    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    syms = module->exported;
    SCM_FOR_EACH(lp, list) {
        if (!SCM_SYMBOLP(SCM_CAR(lp))) {
            error = TRUE;
            badsym = SCM_CAR(lp);
            break;
        }
        s = SCM_SYMBOL(SCM_CAR(lp));
        e = Scm_HashCoreSearch(SCM_HASH_TABLE_CORE(module->table),
                               (intptr_t)s, SCM_DICT_CREATE);
        if (e->value) {         /* e->value must be GLOC. */
            g = SCM_GLOC(e->value);
            if (!g->exported) {
                syms = Scm_Cons(SCM_OBJ(s), syms);
                g->exported = TRUE;
            }
        } else {
            g = SCM_GLOC(Scm_MakeGloc(s, module));
            g->exported = TRUE;
            (void)SCM_DICT_SET_VALUE(e, SCM_OBJ(g));
            syms = Scm_Cons(SCM_OBJ(s), syms);
        }
    }
    if (!error) module->exported = syms;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
    if (error) Scm_Error("symbol required, but got %S", badsym);
    return syms;
}

ScmObj Scm_ExportAll(ScmModule *module)
{
    ScmHashIter iter;
    ScmDictEntry *e;
    
    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    if (!module->exportAll) {
        /* Mark the module 'export-all' so that the new bindings would get
           exported mark by default. */
        module->exportAll = TRUE;
        
        /* Scan the module and mark all existing bindings as exported. */
        Scm_HashIterInit(&iter, SCM_HASH_TABLE_CORE(module->table));
        while ((e = Scm_HashIterNext(&iter)) != NULL) {
            ScmGloc *g = SCM_GLOC(SCM_DICT_VALUE(e));
            if (!g->exported) {
                g->exported = TRUE;
                module->exported =
                    Scm_Cons(SCM_OBJ(g->name), module->exported);
            }
        }
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
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

ScmModule *Scm_FindModule(ScmSymbol *name, int flags)
{
    ScmModule *m;
    int created;

    if (flags & SCM_FIND_MODULE_CREATE) {
        m = lookup_module_create(name, &created);
        SCM_ASSERT(m != NULL);
        return m;
    } else {
        m = lookup_module(name);
        if (m == NULL) {
            if (!(flags & SCM_FIND_MODULE_QUIET)) {
                Scm_Error("no such module: %S", name);
            }
            return NULL;
        } else {
            return m;
        }
    }
}

ScmObj Scm_AllModules(void)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    ScmHashIter iter;
    ScmDictEntry *e;

    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    Scm_HashIterInit(&iter, SCM_HASH_TABLE_CORE(modules.table));
    while ((e = Scm_HashIterNext(&iter)) != NULL) {
        SCM_APPEND1(h, t, SCM_DICT_VALUE(e));
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
    const ScmStringBody *b = SCM_STRING_BODY(SCM_SYMBOL_NAME(name));
    char *buf = SCM_NEW_ATOMIC2(char *, SCM_STRING_BODY_SIZE(b)+1);
    char *p = buf, *e = buf + SCM_STRING_BODY_SIZE(b);
    memcpy(buf, SCM_STRING_BODY_START(b), SCM_STRING_BODY_SIZE(b));
    while (p < e) {
        int n = SCM_CHAR_NFOLLOWS(*p);
        if (*p == '.') *p++ = '/';
        else p += n+1;
    }
    *e = '\0';
    return Scm_MakeString(buf, SCM_STRING_BODY_SIZE(b),
                          SCM_STRING_BODY_LENGTH(b), 0);
}

ScmObj Scm_PathToModuleName(ScmString *path)
{
    const ScmStringBody *b = SCM_STRING_BODY(path);
    char *buf = SCM_NEW_ATOMIC2(char *, SCM_STRING_BODY_SIZE(b)+1);
    char *p = buf, *e = buf + SCM_STRING_BODY_SIZE(b);
    memcpy(buf, SCM_STRING_BODY_START(b), SCM_STRING_BODY_SIZE(b));
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

ScmModule *Scm_GaucheInternalModule(void)
{
    return &internalModule;
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
      SCM_SET_CLASS(&mod, SCM_CLASS_MODULE);                                \
      init_module(&mod, SCM_SYMBOL(mname));                                 \
      Scm_HashTableSet(modules.table, SCM_OBJ((mod).name), SCM_OBJ(&mod), 0);\
      mod.parents = (SCM_NULLP(mpl)? SCM_NIL : SCM_LIST1(SCM_CAR(mpl)));    \
      mpl = mod.mpl = Scm_Cons(SCM_OBJ(&mod), mpl);                         \
    } while (0)

void Scm__InitModule(void)
{
    ScmObj mpl = SCM_NIL;

    (void)SCM_INTERNAL_MUTEX_INIT(modules.mutex);
    modules.table = SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_EQ, 64));

    /* standard module chain */
    INIT_MOD(nullModule, SCM_SYM_NULL, mpl);
    INIT_MOD(schemeModule, SCM_SYM_SCHEME, mpl);
    INIT_MOD(gaucheModule, SCM_SYM_GAUCHE, mpl);
    INIT_MOD(gfModule, SCM_SYM_GAUCHE_GF, mpl);
    INIT_MOD(userModule, SCM_SYM_USER, mpl);

    mpl = SCM_CDR(mpl);  /* default mpl doesn't include user module */
    defaultParents = SCM_LIST1(SCM_CAR(mpl));
    defaultMpl = mpl;
    modules.anon_name = SCM_SYM_SHARP;

    /* other modules */
    mpl = defaultMpl;
    INIT_MOD(internalModule, SCM_SYM_GAUCHE_INTERNAL, mpl);
}
