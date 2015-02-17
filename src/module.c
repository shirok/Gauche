/*
 * module.c - module implementation
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
#include "gauche/priv/builtin-syms.h"
#include "gauche/priv/moduleP.h"

/*
 * Modules
 *
 *  A module maps symbols to global locations (GLOCs).
 *  The mapping is resolved at the compile time.
 *  Scheme's current-module is therefore a syntax, instead of
 *  a procedure, to capture compile-time information.
 *
 *  Each module has two hashtables; the 'internal' table keeps all the
 *  bindings in the module, while the 'external' table keeps only the
 *  bindings that are exported.  In most cases, the latter is a subset
 *  of the former.  If a binding is renamed on export, however,
 *  two tables map different symbols on the same GLOC.
 *
 *  Modules are registered to a global hash table using their names
 *  as keys, so that the module is retrieved by its name.  The exception
 *  is "anonymous modules", which have #f as the name field
 *  and not registered in the global table.   Anonymous modules are especially
 *  useful for certain applications that need temporary, segregated
 *  namespace---for example, a 'sandbox' environment to evaluate an
 *  expression sent over the network during a session.
 *  The anonymous namespace will be garbage-collected if nobody references
 *  it, recovering its resouces.
 */

/* Note on mutex of module operation
 *
 * Each module used to have a mutex for accesses to it.  I changed it
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

/* Special treatment of keyword modules.
 * We need to achieve two goals:
 *
 * (1) For ordinary Gauche programs (modules that uses implicit inheritance
 *   of #<module gauche>), we want to see all keywords being bound to
 *   itself by default.  It can be achieved by inheriting a module that
 *   has such keyword bindings.
 * (2) For R7RS programs we want to see the default keyword bindings only
 *   when the programmer explicitly asks so - notably, by importing some
 *   special module.  It can be achieved by a module that has all keywords
 *   each bound to itself, *and* exports all of such bindings.
 *
 * It turned out we can't use one 'keyword' module for both purpose; if
 * we have a keyword module that exports all bindings, they are automatically   
 * exported from modules that inherits them.  This means if a R7RS program
 * imports any of Gauche modules, it carries all of keyword bindings.  It's
 * not only nasty, but also dangerous for it can shadow bindings to the
 * symbols starting with a colon inadvertently.
 *
 * So we have two modules, #<module gauche.keyword> and
 * #<module keyword>.  The former have export-all flag, and to be imported
 * from R7RS programs as needed.  The latter doesn't export anything, and
 * to be inherited to Gauche modules by default.  Whenever a keyword
 * is created, its default binding is inserted to both - actually,
 * to prevent two modules from being out of sync, we specially wire them
 * to share a single hashtable for their internal bindings.
 */

static void module_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    if (SCM_MODULEP(SCM_MODULE(obj)->origin)) {
        Scm_Printf(port, "#<module %A$%A @%p>",
                   SCM_MODULE(obj)->name,
                   SCM_MODULE(SCM_MODULE(obj)->origin)->name,
                   obj);
    } else {
        Scm_Printf(port, "#<module %A>", SCM_MODULE(obj)->name);
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_ModuleClass,
                         module_print, NULL, NULL, NULL,
                         SCM_CLASS_COLLECTION_CPL);

/* Global module table */
static struct {
    ScmHashTable *table;    /* Maps name -> module. */
    ScmInternalMutex mutex; /* Lock for table.  Only register_module and
                               lookup_module may hold the lock. */
} modules;

/* Predefined modules - slots will be initialized by Scm__InitModule */
#define DEFINE_STATIC_MODULE(cname) \
    static ScmModule cname = { { NULL } }

DEFINE_STATIC_MODULE(nullModule);     /* #<module null> */
DEFINE_STATIC_MODULE(schemeModule);   /* #<module scheme> */
DEFINE_STATIC_MODULE(gaucheModule);   /* #<module gauche> */
DEFINE_STATIC_MODULE(internalModule); /* #<module gauche.internal> */
DEFINE_STATIC_MODULE(gfModule);       /* #<module gauche.gf> */
DEFINE_STATIC_MODULE(userModule);     /* #<module user> */
DEFINE_STATIC_MODULE(keywordModule);  /* #<module keyword> */
DEFINE_STATIC_MODULE(gkeywordModule); /* #<module gauche.keyword> */

static ScmObj defaultParents = SCM_NIL; /* will be initialized */
static ScmObj defaultMpl =     SCM_NIL; /* will be initialized */

/*----------------------------------------------------------------------
 * Constructor
 */

static void init_module(ScmModule *m, ScmObj name, ScmHashTable *internal)
{
    m->name = name;
    m->imported = m->depended = SCM_NIL;
    m->exportAll = FALSE;
    m->parents = defaultParents;
    m->mpl = Scm_Cons(SCM_OBJ(m), defaultMpl);
    if (internal) {
        m->internal = internal;
    } else {
        m->internal = SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_EQ, 0));
    }
    m->external = SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_EQ, 0));
    m->origin = m->prefix = SCM_FALSE;
}

/* Internal */
static ScmObj make_module(ScmObj name, ScmHashTable *internal)
{
    ScmModule *m = SCM_NEW(ScmModule);
    SCM_SET_CLASS(m, SCM_CLASS_MODULE);
    init_module(m, name, internal);
    return SCM_OBJ(m);
}

/* Internal.  Lookup module with name N from the table. */
static ScmModule *lookup_module(ScmSymbol *name)
{
    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    ScmObj v = Scm_HashTableRef(modules.table, SCM_OBJ(name), SCM_UNBOUND);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
    if (SCM_UNBOUNDP(v)) return NULL;
    else return SCM_MODULE(v);
}

/* Internal.  Lookup module, and if there's none, create one. */
static ScmModule *lookup_module_create(ScmSymbol *name, int *created)
{
    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    ScmDictEntry *e = Scm_HashCoreSearch(SCM_HASH_TABLE_CORE(modules.table),
                                         (intptr_t)name,
                                         SCM_DICT_CREATE);
    if (e->value == 0) {
        (void)SCM_DICT_SET_VALUE(e, make_module(SCM_OBJ(name), NULL));
        *created = TRUE;
    } else {
        *created = FALSE;
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
    return SCM_MODULE(e->value);
}

ScmObj Scm_MakeModule(ScmSymbol *name, int error_if_exists)
{
    if (name == NULL) {
        return make_module(SCM_FALSE, NULL);
    }
    int created;
    ScmObj r = SCM_OBJ(lookup_module_create(name, &created));
    if (!created) {
        if (error_if_exists) {
            Scm_Error("couldn't create module '%S': named module already exists",
                      SCM_OBJ(name));
        }
        return SCM_FALSE;
    }
    return r;
}

/* internal API to create an anonymous wrapper module */
ScmObj Scm__MakeWrapperModule(ScmModule *origin, ScmObj prefix)
{
    ScmModule *m = SCM_MODULE(make_module(SCM_FALSE, NULL));
    m->parents = SCM_LIST1(SCM_OBJ(origin));
    m->mpl = Scm_Cons(SCM_OBJ(m), origin->mpl);
    m->prefix = prefix;
    while (SCM_MODULEP(origin->origin)) {
        origin = SCM_MODULE(origin->origin);
    }
    m->origin = SCM_OBJ(origin);
    return SCM_OBJ(m);
}

/*----------------------------------------------------------------------
 * Finding and modifying bindings
 */

#define SEARCHED_ARRAY_SIZE  64

/* Keep record of searched modules.  we use stack array for small # of
   modules, in order to avoid consing for typical cases. */
typedef struct {
    int num_searched;
    ScmObj searched[SEARCHED_ARRAY_SIZE];
    ScmObj more_searched;
} module_cache;


static inline void init_module_cache(module_cache *c)
{
    c->num_searched = 0;
    c->more_searched = SCM_NIL;
}

static inline int module_visited_p(module_cache *c, ScmObj m)
{
    for (int i=0; i<c->num_searched; i++) {
        if (SCM_EQ(m, c->searched[i])) return TRUE;
    }
    if (!SCM_NULLP(c->more_searched)) {
        if (!SCM_FALSEP(Scm_Memq(m, c->more_searched))) return TRUE;
    }
    return FALSE;
}

static inline void module_add_visited(module_cache *c, ScmObj m)
{
    if (c->num_searched < SEARCHED_ARRAY_SIZE) {
        c->searched[c->num_searched++] = m;
    } else {
        c->more_searched = Scm_Cons(m, c->more_searched);
    }
}

/* The main logic of global binding search.  We factored this out since
   we need recursive searching in case of phantom binding (see gloc.h
   about phantom bindings).  The flags stay_in_module and external_only
   corresponds to the flags passed to Scm_FindBinding.  The exclude_self
   flag is only used in recursive search. */
static ScmGloc *search_binding(ScmModule *module, ScmSymbol *symbol,
                               int stay_in_module, int external_only,
                               int exclude_self)
{
    module_cache searched;
    init_module_cache(&searched);

    /* First, search from the specified module.  In this phase, we just ignore
       phantom bindings, for we'll search imported bindings later anyway. */
    if (!exclude_self) {
        ScmObj v = Scm_HashTableRef(
            external_only? module->external : module->internal,
            SCM_OBJ(symbol), SCM_FALSE);
        if (SCM_GLOCP(v)) {
            if (SCM_GLOC_PHANTOM_BINDING_P(SCM_GLOC(v))) {
                /* If we're here, the symbol is external to MODULE but
                   the real GLOC is somewhere in imported or inherited
                   modules.  We turn off external_only switch so that
                   when we search inherited modules we look into it's
                   internal bindings. */
                external_only = FALSE;
                symbol = SCM_GLOC(v)->name; /* in case it is renamed on export */
            } else {
                return SCM_GLOC(v);
            }
        }
        if (stay_in_module) return NULL;
        module_add_visited(&searched, SCM_OBJ(module));
    }

    ScmObj p, mp;
    /* Next, search from imported modules */
    SCM_FOR_EACH(p, module->imported) {
        ScmObj elt = SCM_CAR(p);
        ScmObj sym = SCM_OBJ(symbol);

        SCM_ASSERT(SCM_MODULEP(elt));
        SCM_FOR_EACH(mp, SCM_MODULE(elt)->mpl) {
            ScmGloc *g;

            SCM_ASSERT(SCM_MODULEP(SCM_CAR(mp)));

            if (module_visited_p(&searched, SCM_CAR(mp))) continue;
            ScmModule *m = SCM_MODULE(SCM_CAR(mp));
            if (SCM_SYMBOLP(m->prefix)) {
                sym = Scm_SymbolSansPrefix(SCM_SYMBOL(sym),
                                           SCM_SYMBOL(m->prefix));
                if (!SCM_SYMBOLP(sym)) break;
            }

            ScmObj v = Scm_HashTableRef(m->external, SCM_OBJ(sym), SCM_FALSE);
            if (SCM_GLOCP(v)) {
                g = SCM_GLOC(v);
                if (g->hidden) break;
                if (SCM_GLOC_PHANTOM_BINDING_P(g)) {
                    g = search_binding(m, g->name, FALSE, FALSE, TRUE);
                    if (g) return g;
                } else {
                    return g;
                }
            }
            module_add_visited(&searched, SCM_OBJ(m));
        }
    }

    /* Then, search from parent modules */
    SCM_ASSERT(SCM_PAIRP(module->mpl));
    SCM_FOR_EACH(mp, SCM_CDR(module->mpl)) {
        SCM_ASSERT(SCM_MODULEP(SCM_CAR(mp)));
        ScmModule *m = SCM_MODULE(SCM_CAR(mp));

        if (SCM_SYMBOLP(m->prefix)) {
            ScmObj sym = Scm_SymbolSansPrefix(symbol, SCM_SYMBOL(m->prefix));
            if (!SCM_SYMBOLP(sym)) return NULL;
            symbol = SCM_SYMBOL(sym);
        }
        ScmObj v = Scm_HashTableRef(external_only?m->external:m->internal,
                                    SCM_OBJ(symbol), SCM_FALSE);
        if (SCM_GLOCP(v)) {
            if (SCM_GLOC_PHANTOM_BINDING_P(SCM_GLOC(v))) {
                external_only = FALSE; /* See above comment */
            } else {
                return SCM_GLOC(v);
            }
        }
    }
    return NULL;
}

ScmGloc *Scm_FindBinding(ScmModule *module, ScmSymbol *symbol, int flags)
{
    int stay_in_module = flags&SCM_BINDING_STAY_IN_MODULE;
    int external_only = flags&SCM_BINDING_EXTERNAL;
    ScmGloc *gloc = NULL;

    SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(modules.mutex);
    gloc = search_binding(module, symbol, stay_in_module, external_only, FALSE);
    SCM_INTERNAL_MUTEX_SAFE_LOCK_END();
    return gloc;
}

ScmObj Scm_GlobalVariableRef(ScmModule *module,
                             ScmSymbol *symbol,
                             int flags)
{
    ScmGloc *g = Scm_FindBinding(module, symbol, flags);

    if (g == NULL) return SCM_UNBOUND;
    ScmObj val = SCM_GLOC_GET(g);
    if (SCM_AUTOLOADP(val)) {
        /* NB: Scm_ResolveAutoload may return SCM_UNBOUND */
        val = Scm_ResolveAutoload(SCM_AUTOLOAD(val), 0);
    }
    return val;
}

/*
 * Definition.
 */
ScmGloc *Scm_MakeBinding(ScmModule *module, ScmSymbol *symbol,
                         ScmObj value, int flags)
{
    ScmGloc *g;
    ScmObj oldval = SCM_UNDEFINED;
    int prev_kind = 0;
    int kind = ((flags&SCM_BINDING_CONST)
                ? SCM_BINDING_CONST
                : ((flags&SCM_BINDING_INLINABLE)
                   ? SCM_BINDING_INLINABLE
                   : 0));

    SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(modules.mutex);
    ScmObj v = Scm_HashTableRef(module->internal, SCM_OBJ(symbol), SCM_FALSE);
    /* NB: this function bypasses check of gloc setter */
    if (SCM_GLOCP(v)) {
        g = SCM_GLOC(v);
        if (Scm_GlocConstP(g))          prev_kind = SCM_BINDING_CONST;
        else if (Scm_GlocInlinableP(g)) prev_kind = SCM_BINDING_INLINABLE;
        oldval = g->value;
    } else {
        g = SCM_GLOC(Scm_MakeGloc(symbol, module));
        Scm_HashTableSet(module->internal, SCM_OBJ(symbol), SCM_OBJ(g), 0);
        /* If module is marked 'export-all', export this binding by default */
        if (module->exportAll) {
            Scm_HashTableSet(module->external, SCM_OBJ(symbol), SCM_OBJ(g), 0);
        }
    }
    SCM_INTERNAL_MUTEX_SAFE_LOCK_END();

    g->value = value;
    Scm_GlocMark(g, kind);

    if (prev_kind != 0) {
        /* NB: Scm_EqualP may throw an error.  It won't leave the state
           inconsistent, but be aware. */
        if (prev_kind != kind || !Scm_EqualP(value, oldval)) {
            Scm_Warn("redefining %s %S::%S",
                     (prev_kind == SCM_BINDING_CONST)? "constant" : "inlinable",
                     g->module->name, g->name);
        }
    }
    return g;
}

/* Convenience wrapper (return value is ScmObj for the backward compatibility)*/
ScmObj Scm_Define(ScmModule *module, ScmSymbol *symbol, ScmObj value)
{
    return SCM_OBJ(Scm_MakeBinding(module, symbol, value, 0));
}

ScmObj Scm_DefineConst(ScmModule *module, ScmSymbol *symbol, ScmObj value)
{
    return SCM_OBJ(Scm_MakeBinding(module, symbol, value, SCM_BINDING_CONST));
}

/*
 * Injecting hidden binding
 *   This inserts a dummy binding with hidden==true so that
 *   the module effectively removes the binding of the given symbol
 *   inherited from parent.
 *   This is not for genreral use.  It is intended to be used for
 *   intermediate anonymous modules, created by import handling
 *   routine to implement :except and :rename qualifiers.
 *   Since we assume MODULE is for intermediate modules, we only
 *   insert bindings to the external table, for those modules are
 *   only searched in the 'import' path.
 */
void Scm_HideBinding(ScmModule *module, ScmSymbol *symbol)
{
    int err_exists = FALSE;

    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    ScmObj v = Scm_HashTableRef(module->external, SCM_OBJ(symbol), SCM_FALSE);
    if (!SCM_FALSEP(v)) {
        err_exists = TRUE;
    } else {
        ScmGloc *g = SCM_GLOC(Scm_MakeGloc(symbol, module));
        g->hidden = TRUE;
        Scm_HashTableSet(module->external, SCM_OBJ(symbol), SCM_OBJ(g), 0);
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);

    if (err_exists) {
        Scm_Error("hide-binding: binding already exists: %S (exports=%S)", SCM_OBJ(symbol), Scm_ModuleExports(module));
    }
}

/*
 * Binding aliasing
 *   This is a special operation to realize :only and :rename import option.
 *   The name ORIGINNAME is looked up in the module ORIGIN to get a gloc.
 *   Then the gloc is directly inserted into the module TARGET under the name
 *   TARGETNAME.
 *   Since gloc is shared, subsequent changes in the binding are also shared.
 *
 *   If the original binding doesn't exist, or isn't exported, noop and
 *   FALSE is returned.  Otherwise TRUE is returned.
 *
 *   CAVEATS:
 *
 *   - gloc's module remains the same.
 *   - autoload won't be resolved.
 *   - TARGETNAME shouldn't be bound in TARGET beforehand.  We don't check
 *     it and just insert the gloc.  If there is an existing binding,
 *     it would become orphaned, possibly causing problems.
 *
 *   NB: This is the only operation that causes a gloc to be shared between
 *   more than one modules.  I'm not yet clear on the implication of such
 *   sharing in general, so this should be used with care.  At least it
 *   won't cause much trouble if the target module is an implicit anonymous
 *   module created by :only and :rename import options.
 */
int Scm_AliasBinding(ScmModule *target, ScmSymbol *targetName,
                     ScmModule *origin, ScmSymbol *originName)
{
    ScmGloc *g = Scm_FindBinding(origin, originName, SCM_BINDING_EXTERNAL);
    if (g == NULL) return FALSE;
    SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(modules.mutex);
    Scm_HashTableSet(target->external, SCM_OBJ(targetName), SCM_OBJ(g), 0);
    Scm_HashTableSet(target->internal, SCM_OBJ(targetName), SCM_OBJ(g), 0);
    SCM_INTERNAL_MUTEX_SAFE_LOCK_END();
    return TRUE;
}

/*
 * Import
 */
ScmObj Scm_ImportModule(ScmModule *module,
                        ScmObj imported,
                        ScmObj prefix,
                        u_long flags) /* reserved for future use */
{
    ScmModule *imp = NULL;
    if (SCM_MODULEP(imported)) {
        imp = SCM_MODULE(imported);
    } else if (SCM_SYMBOLP(imported)) {
        imp = Scm_FindModule(SCM_SYMBOL(imported), 0);
    } else if (SCM_IDENTIFIERP(imported)) {
        imp = Scm_FindModule(Scm_UnwrapIdentifier(SCM_IDENTIFIER(imported)), 0);
    } else {
        Scm_Error("module name or module required, but got %S", imported);
    }

    if (SCM_SYMBOLP(prefix)) {
        imp = SCM_MODULE(Scm__MakeWrapperModule(imp, prefix));
    }

    /* Preallocate a pair, so that we won't call malloc during locking */
    ScmObj p = Scm_Cons(SCM_OBJ(imp), SCM_NIL);

    /* Prepend imported module to module->imported list. */
    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    {
        ScmObj ms, prev = p;
        SCM_SET_CDR(p, module->imported);
        /* Remove duplicate module, if any.
           NB: We allow to import the same module multiple times if they are
           qualified by :only, :prefix, etc.  Theoretically we should check
           exactly same qualifications, but we hope that kind of duplication
           is rare.
        */
        SCM_FOR_EACH(ms, SCM_CDR(p)) {
            ScmModule *m = SCM_MODULE(SCM_CAR(ms));
            if (!SCM_EQ(SCM_OBJ(m), SCM_OBJ(imp))) {
                prev = ms;
                continue;
            }
            SCM_SET_CDR(prev, SCM_CDR(ms));
            break;
        }
        module->imported = p;
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);

    return module->imported;
}

/* Deprecated */
ScmObj Scm_ImportModules(ScmModule *module, ScmObj list)
{
    ScmObj lp;
    SCM_FOR_EACH(lp, list) {
        Scm_ImportModule(module, SCM_CAR(lp), SCM_FALSE, 0);
    }
    return module->imported;
}

/*
 * Export
 */
/* <spec>  :: <name> | (rename <name> <exported-name>) */
ScmObj Scm_ExportSymbols(ScmModule *module, ScmObj specs)
{
    ScmObj lp;
    ScmObj overwritten = SCM_NIL; /* list of (exported-name orig-internal-name
                                     new-internal-name). */
    /* Check input first */
    SCM_FOR_EACH(lp, specs) {
        ScmObj spec = SCM_CAR(lp);
        if (!(SCM_SYMBOLP(spec)
              || (SCM_PAIRP(spec) && SCM_PAIRP(SCM_CDR(spec))
                  && SCM_PAIRP(SCM_CDDR(spec))
                  && SCM_NULLP(SCM_CDR(SCM_CDDR(spec)))
                  && SCM_EQ(SCM_CAR(spec), SCM_SYM_RENAME)
                  && SCM_SYMBOLP(SCM_CADR(spec))
                  && SCM_SYMBOLP(SCM_CAR(SCM_CDDR(spec)))))) {
            Scm_Error("Invalid export-spec; a symbol, or (rename <symbol> <symbol>) is expected, but got %S", spec);
        }
    }

    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    SCM_FOR_EACH(lp, specs) {
        ScmObj spec = SCM_CAR(lp);
        ScmSymbol *name, *exported_name;
        if (SCM_SYMBOLP(spec)) {
            name = exported_name = SCM_SYMBOL(spec);
        } else {
            /* we already knew those are symbols */
            name = SCM_SYMBOL(SCM_CADR(spec));
            exported_name = SCM_SYMBOL(SCM_CAR(SCM_CDDR(spec)));
        }
        ScmDictEntry *e = Scm_HashCoreSearch(SCM_HASH_TABLE_CORE(module->external),
                                             (intptr_t)exported_name, SCM_DICT_GET);
        if (e) {
            /* If we have e, it's already exported.  Check if
               the previous export is for the same binding. */
            SCM_ASSERT(SCM_DICT_VALUE(e) && SCM_GLOCP(SCM_DICT_VALUE(e)));
            ScmGloc *g = SCM_GLOC(SCM_DICT_VALUE(e));
            if (!SCM_EQ(name, g->name)) {
                /* exported_name got a different meaning. we record it to warn
                   later, then 'unexport' the old one. */
                overwritten = Scm_Cons(SCM_LIST3(SCM_OBJ(exported_name),
                                                 SCM_OBJ(g->name),
                                                 SCM_OBJ(name)),
                                       overwritten);
                Scm_HashCoreSearch(SCM_HASH_TABLE_CORE(module->external),
                                   (intptr_t)exported_name, SCM_DICT_DELETE);
                e = NULL;
            }
        }
        /* we check again, for the symbol may be unexported above. */
        if (e == NULL) {
            /* This symbol hasn't been exported.  Either it only has an
               internal binding, or there's no binding at all.  In the latter
               case, we create a new binding (without value). */
            e = Scm_HashCoreSearch(SCM_HASH_TABLE_CORE(module->internal),
                                   (intptr_t)name, SCM_DICT_CREATE);
            if (!e->value) {
                ScmGloc *g = SCM_GLOC(Scm_MakeGloc(name, module));
                (void)SCM_DICT_SET_VALUE(e, SCM_OBJ(g));
            }
            Scm_HashTableSet(module->external, SCM_OBJ(exported_name),
                             SCM_DICT_VALUE(e), 0);
        }
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);

    /* Now, if this export changes the meaning of exported symbols, we
       warn it.  We expect this only happens at the development time, when
       one is fiddling exports incrementally, so we just use Scm_Warn -
       a library ready to be used shouldn't cause this warning. */
    if (!SCM_NULLP(overwritten)) {
        ScmObj lp;
        SCM_FOR_EACH(lp, overwritten) {
            ScmObj p = SCM_CAR(lp);
            Scm_Warn("Exporting %S from %S as %S overrides the previous export of %S",
                     SCM_CAR(SCM_CDDR(p)), SCM_OBJ(module), SCM_CAR(p),
                     SCM_CADR(p));
        }
    }

    return SCM_UNDEFINED;  /* we might want to return something more useful...*/
}

ScmObj Scm_ExportAll(ScmModule *module)
{
    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    if (!module->exportAll) {
        /* Mark the module 'export-all' so that the new bindings would get
           exported mark by default. */
        module->exportAll = TRUE;

        /* Scan the module and mark all existing bindings as exported. */
        ScmHashIter iter;
        Scm_HashIterInit(&iter, SCM_HASH_TABLE_CORE(module->internal));
        ScmDictEntry *e;
        while ((e = Scm_HashIterNext(&iter)) != NULL) {
            ScmDictEntry *ee;
            ee = Scm_HashCoreSearch(SCM_HASH_TABLE_CORE(module->external),
                                    e->key, SCM_DICT_CREATE);
            if (!ee->value) {
                (void)SCM_DICT_SET_VALUE(ee, SCM_DICT_VALUE(e));
            }
        }
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
    return SCM_OBJ(module);
}

/* Returns list of exported symbols.   We assume this is infrequent
   operation, so we build the list every call.  If it becomes a problem,
   we can cache the result. */
ScmObj Scm_ModuleExports(ScmModule *module)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;

    (void)SCM_INTERNAL_MUTEX_LOCK(modules.mutex);
    ScmHashIter iter;
    Scm_HashIterInit(&iter, SCM_HASH_TABLE_CORE(module->external));
    ScmDictEntry *e;
    while ((e = Scm_HashIterNext(&iter)) != NULL) {
        SCM_APPEND1(h, t, SCM_DICT_KEY(e));
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(modules.mutex);
    return h;
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
    ScmObj seqh = SCM_NIL, seqt = SCM_NIL;

    ScmObj sp;
    SCM_FOR_EACH(sp, supers) {
        if (!SCM_MODULEP(SCM_CAR(sp))) {
            Scm_Error("non-module object found in the extend syntax: %S",
                      SCM_CAR(sp));
        }
        SCM_APPEND1(seqh, seqt, SCM_MODULE(SCM_CAR(sp))->mpl);
    }
    SCM_APPEND1(seqh, seqt, supers);
    module->parents = supers;
    ScmObj mpl = Scm_MonotonicMerge1(seqh);
    if (SCM_FALSEP(mpl)) {
        Scm_Error("can't extend those modules simultaneously because of inconsistent precedence lists: %S", supers);
    }
    module->mpl = Scm_Cons(SCM_OBJ(module), mpl);
    return module->mpl;
}

/*----------------------------------------------------------------------
 * Finding modules
 */

ScmModule *Scm_FindModule(ScmSymbol *name, int flags)
{
    if (flags & SCM_FIND_MODULE_CREATE) {
        int created;
        ScmModule *m = lookup_module_create(name, &created);
        SCM_ASSERT(m != NULL);
        return m;
    } else {
        ScmModule *m = lookup_module(name);
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
   We moved the definition in Scheme.  These are just stubs to call them. */
ScmObj Scm_ModuleNameToPath(ScmSymbol *name)
{
    static ScmObj module_name_to_path_proc = SCM_UNDEFINED;
    SCM_BIND_PROC(module_name_to_path_proc, "module-name->path", Scm_GaucheModule());
    return Scm_ApplyRec1(module_name_to_path_proc, SCM_OBJ(name));
}

ScmObj Scm_PathToModuleName(ScmString *path)
{
    static ScmObj path_to_module_name_proc = SCM_UNDEFINED;
    SCM_BIND_PROC(path_to_module_name_proc, "path->module-name", Scm_GaucheModule());
    return Scm_ApplyRec1(path_to_module_name_proc, SCM_OBJ(path));
}

/*----------------------------------------------------------------------
 * Module introspection
 */

static ScmObj module_name(ScmObj m)
{
    return SCM_MODULE(m)->name;
}

static ScmObj module_imported(ScmObj m)
{
    return SCM_MODULE(m)->imported;
}

static ScmObj module_exported(ScmObj m)
{
    return Scm_ModuleExports(SCM_MODULE(m));
}

static ScmObj module_exportAll(ScmObj m)
{
    return SCM_MAKE_BOOL(SCM_MODULE(m)->exportAll);
}

static ScmObj module_parents(ScmObj m)
{
    return SCM_MODULE(m)->parents;
}

static ScmObj module_mpl(ScmObj m)
{
    return SCM_MODULE(m)->mpl;
}

static ScmObj module_depended(ScmObj m)
{
    return SCM_MODULE(m)->depended;
}

static ScmObj module_table(ScmObj m)
{
    return SCM_OBJ(SCM_MODULE(m)->internal);
}

static ScmObj module_origin(ScmObj m)
{
    return SCM_MODULE(m)->origin;
}

static ScmObj module_prefix(ScmObj m)
{
    return SCM_MODULE(m)->prefix;
}

static ScmClassStaticSlotSpec module_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", module_name, NULL),
    SCM_CLASS_SLOT_SPEC("mpl", module_mpl, NULL),
    SCM_CLASS_SLOT_SPEC("parents", module_parents, NULL),
    SCM_CLASS_SLOT_SPEC("imports", module_imported, NULL),
    SCM_CLASS_SLOT_SPEC("exports", module_exported, NULL),
    SCM_CLASS_SLOT_SPEC("export-all", module_exportAll, NULL),
    SCM_CLASS_SLOT_SPEC("table", module_table, NULL),
    SCM_CLASS_SLOT_SPEC("depends", module_depended, NULL),
    SCM_CLASS_SLOT_SPEC("origin", module_origin, NULL),
    SCM_CLASS_SLOT_SPEC("prefix", module_prefix, NULL),
    SCM_CLASS_SLOT_SPEC_END()
};

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

ScmModule *Scm__KeywordModule(void) /* internal */
{
    return &keywordModule;
}

ScmModule *Scm__GaucheKeywordModule(void) /* internal */
{
    return &gkeywordModule;
}

ScmModule *Scm_CurrentModule(void)
{
    return Scm_VM()->module;
}

/* NB: we don't need to lock the global module table in initialization */
#define INIT_MOD(mod, mname, mpl, inttab)                                   \
    do {                                                                    \
      SCM_SET_CLASS(&mod, SCM_CLASS_MODULE);                                \
      init_module(&mod, mname,  inttab);                                    \
      Scm_HashTableSet(modules.table, (mod).name, SCM_OBJ(&mod), 0);        \
      mod.parents = (SCM_NULLP(mpl)? SCM_NIL : SCM_LIST1(SCM_CAR(mpl)));    \
      mpl = mod.mpl = Scm_Cons(SCM_OBJ(&mod), mpl);                         \
    } while (0)

void Scm__InitModule(void)
{
    /* List of builtin modules.  We create these so that 'use' or r7rs 'import'
       won't try to search the file.
       The modules listed here are marked "provided" at the startup, so it can
       no longer be loaded by 'use' or 'require'.  Don't list modules that
       needs to be loaded.
    */
    static const char *builtin_modules[] = {
        "srfi-2", "srfi-6", "srfi-8", "srfi-10", "srfi-16", "srfi-17",
        "srfi-22", "srfi-23", "srfi-28", "srfi-34",
        "srfi-35", "srfi-36", "srfi-38", "srfi-45", "srfi-61",
        "srfi-62", "srfi-87", "srfi-95", "srfi-111",
        NULL };
    const char **modname;

    (void)SCM_INTERNAL_MUTEX_INIT(modules.mutex);
    modules.table = SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_EQ, 64));

    /* standard module chain */
    ScmObj mpl = SCM_NIL;
    INIT_MOD(nullModule, SCM_SYM_NULL, mpl, NULL);
    INIT_MOD(schemeModule, SCM_SYM_SCHEME, mpl, NULL);
    INIT_MOD(keywordModule, SCM_SYM_KEYWORD, mpl, NULL);
    INIT_MOD(gaucheModule, SCM_SYM_GAUCHE, mpl, NULL);
    INIT_MOD(gfModule, SCM_SYM_GAUCHE_GF, mpl, NULL);
    INIT_MOD(userModule, SCM_SYM_USER, mpl, NULL);

    mpl = SCM_CDR(mpl);  /* default mpl doesn't include user module */
    defaultParents = SCM_LIST1(SCM_CAR(mpl));
    defaultMpl = mpl;

    /* other modules */
    mpl = defaultMpl;
    INIT_MOD(internalModule, SCM_SYM_GAUCHE_INTERNAL, mpl, NULL);

    mpl = keywordModule.mpl;
    INIT_MOD(gkeywordModule, SCM_INTERN("gauche.keyword"), mpl,
             keywordModule.internal);
    gkeywordModule.exportAll = TRUE;

    /* create predefined moudles */
    for (modname = builtin_modules; *modname; modname++) {
        (void)SCM_FIND_MODULE(*modname, SCM_FIND_MODULE_CREATE);
    }
}

void Scm__InitModulePost(void)
{
    Scm_InitStaticClassWithMeta(&Scm_ModuleClass, "<module>", &gaucheModule,
                                NULL, /* auto-generate meta */
                                SCM_FALSE, /* calculate supers from cpl */
                                module_slots,
                                0);
}
