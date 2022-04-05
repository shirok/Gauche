/*
 * compaux.c - C API bridge for the compiler
 *
 *   Copyright (c) 2000-2021  Shiro Kawai  <shiro@acm.org>
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

/* This file serves as a bridge to the compiler, which is implemented
   in Scheme (see compile.scm) */

#include <stdlib.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/vm.h"
#include "gauche/vminsn.h"
#include "gauche/code.h"
#include "gauche/priv/glocP.h"
#include "gauche/priv/identifierP.h"
#include "gauche/priv/builtin-syms.h"
#include "gauche/priv/readerP.h" /* for Scm_MakeReadReference */

/*
 * Syntax
 */


/*
 * Compiler Entry
 */

static ScmGloc *compile_gloc = NULL;
static ScmGloc *compile_partial_gloc = NULL;
static ScmGloc *compile_finish_gloc = NULL;
static ScmGloc *init_compiler_gloc = NULL;

static ScmInternalMutex compile_finish_mutex;

ScmObj Scm_Compile(ScmObj program, ScmObj env)
{
    return Scm_ApplyRec2(SCM_GLOC_GET(compile_gloc), program, env);
}

ScmObj Scm_CompilePartial(ScmObj program, ScmObj env)
{
    return Scm_ApplyRec2(SCM_GLOC_GET(compile_partial_gloc), program, env);
}

void Scm_CompileFinish(ScmCompiledCode *cc)
{
    if (cc->code == NULL) {
        SCM_INTERNAL_MUTEX_LOCK(compile_finish_mutex);
        SCM_UNWIND_PROTECT {
            if (cc->code == NULL) {
                Scm_ApplyRec1(SCM_GLOC_GET(compile_finish_gloc), SCM_OBJ(cc));
            }
        }
        SCM_WHEN_ERROR {
            SCM_INTERNAL_MUTEX_UNLOCK(compile_finish_mutex);
            SCM_NEXT_HANDLER;
        }
        SCM_END_PROTECT;
        SCM_INTERNAL_MUTEX_UNLOCK(compile_finish_mutex);
    }
}

/*-------------------------------------------------------------
 * Syntactic closure object
 */

static void synclo_print(ScmObj obj, ScmPort *port,
                         ScmWriteContext *ctx SCM_UNUSED)
{
    Scm_Printf(port, "#<syntactic-closure %S>",
               SCM_SYNTACTIC_CLOSURE(obj)->expr);
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SyntacticClosureClass, synclo_print);

ScmObj Scm_MakeSyntacticClosure(ScmObj env, ScmObj literals, ScmObj expr)
{
    ScmSyntacticClosure *s = SCM_NEW(ScmSyntacticClosure);
    SCM_SET_CLASS(s, SCM_CLASS_SYNTACTIC_CLOSURE);
    s->env = env;
    s->literals = literals;
    s->expr = expr;
    return SCM_OBJ(s);
}

static ScmObj synclo_env_get(ScmObj obj)
{
    return SCM_SYNTACTIC_CLOSURE(obj)->env;
}

static ScmObj synclo_literals_get(ScmObj obj)
{
    return SCM_SYNTACTIC_CLOSURE(obj)->literals;
}

static ScmObj synclo_expr_get(ScmObj obj)
{
    return SCM_SYNTACTIC_CLOSURE(obj)->expr;
}

static ScmClassStaticSlotSpec synclo_slots[] = {
    SCM_CLASS_SLOT_SPEC("env", synclo_env_get, NULL),
    SCM_CLASS_SLOT_SPEC("literals", synclo_literals_get, NULL),
    SCM_CLASS_SLOT_SPEC("expr", synclo_expr_get, NULL),
    SCM_CLASS_SLOT_SPEC_END()
};

/*-------------------------------------------------------------
 * Identifier object
 */

/*
 * About identifier's ENV slot.
 * We close identifier's binding environment (list of frames), so that later
 * we can look up its bindings hygienically.  We truncate the frames up to
 * where the binding occur, for the efficient lookup and comparison.
 * Notably, the identifiers that are unbound or refer to toplevel variable
 * has () in its env.
 * A caveat--we can't truncate frames at the time of construction, since
 * the entire frame structure may not be fixed while we're processing internal
 * defines.  The identifier may refer to another identifier that will be
 * inserted later.  Thus, we delay the truncation operation until it is
 * needed.
 * The ENV slot itself now contains (<flag> . <frames>), where <flag> is #f
 * if truncation hasn't be done, #t otherwise.  ENV should be treated as
 * an opaque data for the others; you should always get it with
 * Scm_IdentifierEnv(), and never directly access ENV slot itself.
 */

static void identifier_print(ScmObj obj, ScmPort *port,
                             ScmWriteContext *ctx SCM_UNUSED)
{
    ScmIdentifier *id = SCM_IDENTIFIER(obj);
    /* We may want to have an external identifier syntax, so that an
       identifier can be written out and then read back.  It will be
       convenient if we can embed a reference to other module's global
       binding directly in the program.  However, it can also breaches
       module-based sandbox implementation, so further consideration is
       required.
    */
    Scm_Printf(port, "#<identifier %S#%S.%x>",
               id->module->name, id->name, SCM_WORD(id));
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_IdentifierClass, identifier_print);

/* Truncate local frames */
static ScmObj get_binding_frame(ScmObj var, ScmObj env)
{
    ScmObj frame, fp;
    SCM_FOR_EACH(frame, env) {
        if (!SCM_PAIRP(SCM_CAR(frame))) continue;
        SCM_FOR_EACH(fp, SCM_CDAR(frame)) {
            if (SCM_CAAR(fp) == var) {
                SCM_ASSERT(SCM_LISTP(frame));
                return frame;
            }
        }
    }
    return SCM_NIL;
}

ScmObj Scm_MakeIdentifier(ScmObj name, ScmModule *mod, ScmObj env)
{
    ScmIdentifier *id = SCM_NEW(ScmIdentifier);
    SCM_SET_CLASS(id, SCM_CLASS_IDENTIFIER);
    id->name = name;
    id->module = mod? mod : SCM_CURRENT_MODULE();
    id->frames = Scm_Cons(SCM_FALSE, env); /* see the above comment */
    return SCM_OBJ(id);
}

ScmObj Scm_IdentifierEnv(ScmIdentifier *id)
{
    SCM_ASSERT(SCM_PAIRP(id->frames));
    if (SCM_FALSEP(SCM_CAR(id->frames))) {
        /* MT safety: This operation is idempotent, so it's ok if more than
           one thread execute here. */
        ScmObj f = get_binding_frame(id->name, SCM_CDR(id->frames));
        SCM_SET_CDR_UNCHECKED(id->frames, f);
        SCM_SET_CAR_UNCHECKED(id->frames, SCM_TRUE);
    }
    return SCM_CDR(id->frames);
}

ScmIdentifier *Scm_OutermostIdentifier(ScmIdentifier *id)
{
    while (SCM_IDENTIFIERP(id->name)) {
        id = SCM_IDENTIFIER(SCM_IDENTIFIER(id)->name);
    }
    return id;
}

ScmSymbol *Scm_UnwrapIdentifier(ScmIdentifier *id)
{
    ScmObj z = Scm_OutermostIdentifier(id)->name;
    SCM_ASSERT(SCM_SYMBOLP(z));
    return SCM_SYMBOL(z);
}

/* Returns global binding of the identifier */
ScmGloc *Scm_IdentifierGlobalBinding(ScmIdentifier *id)
{
    ScmIdentifier *z = Scm_OutermostIdentifier(id);
    return Scm_FindBinding(z->module, SCM_SYMBOL(z->name), 0);
}

/* Runtime reference/mutation of global variables.  The are called
   from VM instructoins GREF and GSET, and will also be used from
   the AOT compiled code.

   We cache the result of global variable to GLOC lookup in VM instructions,
   so these procedures can also store the looked up GLOC in *pgloc.  You can
   pass NULL if you don't need it.

   If the referenced variable is unbound, or is immutable when attempted to
   set!, an error is thrown.
 */
ScmObj Scm_IdentifierGlobalRef(ScmIdentifier *id,
                               ScmGloc **pgloc /* out */)
{
    ScmGloc *gloc = Scm_IdentifierGlobalBinding(id);
    if (gloc == NULL) {
        Scm_Error("unbound variable: %S", SCM_OBJ(id->name));
    }
    if (pgloc != NULL) *pgloc = gloc;

    ScmObj v = Scm_GlocGetValue(gloc);
    if (SCM_AUTOLOADP(v)) {
        return Scm_ResolveAutoload(SCM_AUTOLOAD(v), 0);
    } else {
        return v;
    }
}

/* returns true if SYM has the same binding with ID in ENV. */
int Scm_IdentifierBindingEqv(ScmIdentifier *id, ScmSymbol *sym, ScmObj env)
{
    ScmObj env1 = Scm_IdentifierEnv(id);
    ScmObj env2 = get_binding_frame(SCM_OBJ(sym), env);
    return (env1 == env2);
}

ScmObj Scm_WrapIdentifier(ScmIdentifier *orig)
{
    ScmIdentifier *id = SCM_NEW(ScmIdentifier);
    SCM_SET_CLASS(id, SCM_CLASS_IDENTIFIER);
    id->name = SCM_OBJ(orig);
    id->module = orig->module;
    id->frames = orig->frames;
    return SCM_OBJ(id);
}

static ScmObj identifier_name_get(ScmObj obj)
{
    return SCM_OBJ(SCM_IDENTIFIER(obj)->name);
}

static void   identifier_name_set(ScmObj obj, ScmObj val)
{
    if (!SCM_SYMBOLP(val) && !SCM_IDENTIFIERP(val)) {
        Scm_Error("symbol or identifier required, but got %S", val);
    }
    SCM_IDENTIFIER(obj)->name = val;
}

static ScmObj identifier_module_get(ScmObj obj)
{
    return SCM_OBJ(SCM_IDENTIFIER(obj)->module);
}

static ScmObj identifier_env_get(ScmObj obj)
{
    return Scm_IdentifierEnv(SCM_IDENTIFIER(obj));
}

/* Identifier name can be mutated during macro expansion to avoid
   conflicts on macro-inserted toplevel identifiers.  See
   %rename-toplevel-identifier! in compiler pass1.
   Other than that, identifiers must be treated as immutable objects. */
static ScmClassStaticSlotSpec identifier_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", identifier_name_get, identifier_name_set),
    SCM_CLASS_SLOT_SPEC("module", identifier_module_get, NULL),
    SCM_CLASS_SLOT_SPEC("env", identifier_env_get, NULL),
    SCM_CLASS_SLOT_SPEC_END()
};

/*------------------------------------------------------------------
 * Unwrapping syntax
 *
 * Traverses a form and replaces identifiers for bare symbols.
 * It's complicated because the form may have cycles.
 * We use Scm_ReadReference as the temporary placeholder to handle
 * circular structure.  All the read references are replaced before
 * Scm_UnwrapSyntax returns.
 */

typedef struct unwrap_ctx_rec {
    ScmHashCore history;        /* object -> #f | R() | R(v)
                                   where
                                     SCM_UNBOUND - the object has been visited
                                         only once
                                     value - the object is realized
                                     R() - empty ReadReference.  The object
                                         has been visited more than once,
                                         but its final replacement hasn't
                                         been materialized yet.
                                     R(v) - ReadReference with value.
                                         The object should correspond to
                                         the value.
                                 */
    ScmHashCore refs;           /* location -> read reference*/
    int immutable;              /* flag */
} unwrap_ctx;

static void register_location(unwrap_ctx *ctx,
                              ScmObj *loc,
                              ScmObj ref)
{
    if (!SCM_READ_REFERENCE_P(ref)) return;
    if (SCM_READ_REFERENCE_REALIZED(ref)) {
        *loc = SCM_READ_REFERENCE(ref)->value;
    } else {
        ScmDictEntry *e =
            Scm_HashCoreSearch(&ctx->refs, (intptr_t)loc, SCM_DICT_CREATE);
        e->value = (intptr_t)ref;
    }
}

static void fill_history(ScmDictEntry *e, ScmObj value)
{
    if (e->value) {
        if (SCM_READ_REFERENCE_P(e->value)) {
            SCM_READ_REFERENCE(e->value)->value = value;
        }
    } else {
        e->value = (intptr_t)value;
    }
}

static void patch_locations(unwrap_ctx *ctx)
{
    ScmHashIter iter;
    Scm_HashIterInit(&iter, &ctx->refs);
    for (;;) {
        ScmDictEntry *e = Scm_HashIterNext(&iter);
        if (e == NULL) break;
        ScmObj *loc = (ScmObj*)SCM_DICT_KEY(e);
        ScmReadReference *ref = SCM_READ_REFERENCE(SCM_DICT_VALUE(e));
        if (SCM_READ_REFERENCE_P(ref)) {
            SCM_ASSERT(SCM_READ_REFERENCE_REALIZED(ref));
            *loc = SCM_READ_REFERENCE(ref)->value;
        }
    }
}

/* Returns either original form, or converted form, or a read reference. */
static ScmObj unwrap_rec(ScmObj form, unwrap_ctx *ctx)
{
    if (!SCM_PTRP(form)) return form;
    ScmDictEntry *e = Scm_HashCoreSearch(&ctx->history,
                                         (intptr_t)form,
                                         SCM_DICT_GET);
    if (e) {
        /* We've visited FORM before.  If this is the second time,
           we allocate ReadReference to hold the value.  It will be
           filled later by the caller. */
        if (!e->value) {
            e->value = (intptr_t)Scm_MakeReadReference();
        }
        return SCM_DICT_VALUE(e);
    }

    if (SCM_PAIRP(form)) {
        e = Scm_HashCoreSearch(&ctx->history, (intptr_t)form, SCM_DICT_CREATE);
        ScmObj ca = unwrap_rec(SCM_CAR(form), ctx);
        ScmObj cd = unwrap_rec(SCM_CDR(form), ctx);
        if (ca == SCM_CAR(form) && cd == SCM_CDR(form)
            && (!ctx->immutable || Scm_ImmutablePairP(form))) {
            fill_history(e, form);
            return form;
        }
        ScmObj p = (ctx->immutable
                    ? Scm_MakeImmutablePair(ca, cd)
                    : Scm_Cons(ca, cd));
        fill_history(e, p);
        register_location(ctx, &SCM_PAIR(p)->car, ca);
        register_location(ctx, &SCM_PAIR(p)->cdr, cd);
        return p;
    }
    if (SCM_IDENTIFIERP(form)) {
        return SCM_OBJ(Scm_UnwrapIdentifier(SCM_IDENTIFIER(form)));
    }
    if (SCM_VECTORP(form)) {
        int len = SCM_VECTOR_SIZE(form);
        ScmObj *pelt = SCM_VECTOR_ELEMENTS(form);
        e = Scm_HashCoreSearch(&ctx->history, (intptr_t)form, SCM_DICT_CREATE);
        for (int i=0; i<len; i++, pelt++) {
            ScmObj elt = unwrap_rec(*pelt, ctx);
            if (elt != *pelt
                || (ctx->immutable && !SCM_VECTOR_IMMUTABLE_P(form))) {
                ScmObj newvec = Scm_MakeVector(len, SCM_FALSE);
                pelt = SCM_VECTOR_ELEMENTS(form);
                int j;
                for (j=0; j<i; j++, pelt++) {
                    SCM_VECTOR_ELEMENT(newvec, j) = *pelt;
                }
                register_location(ctx, &SCM_VECTOR_ELEMENT(newvec, i), elt);
                SCM_VECTOR_ELEMENT(newvec, i) = elt;
                for (j=i+1, pelt++; j<len; j++, pelt++) {
                    elt = unwrap_rec(*pelt, ctx);
                    register_location(ctx, &SCM_VECTOR_ELEMENT(newvec, i), elt);
                    SCM_VECTOR_ELEMENT(newvec, j) = elt;
                }
                if (ctx->immutable) {
                    SCM_VECTOR_IMMUTABLE_SET(newvec, TRUE);
                }
                fill_history(e, newvec);
                return newvec;
            }
        }
        fill_history(e, form);
        return form;
    }
    return form;
}

#if GAUCHE_API_VERSION < 98
ScmObj Scm_UnwrapSyntax(ScmObj form)
{
    return Scm_UnwrapSyntax2(form, FALSE);
}
ScmObj Scm_UnwrapSyntax2(ScmObj form, int immutablep)
#else  /* GAUCHE_API_VERSION >= 98 */
ScmObj Scm_UnwrapSyntax(ScmObj form, int immutablep)
#endif /* GAUCHE_API_VERSION >= 98 */
{
    unwrap_ctx ctx;
    Scm_HashCoreInitSimple(&ctx.history, SCM_HASH_EQ, 0, NULL);
    Scm_HashCoreInitSimple(&ctx.refs, SCM_HASH_EQ, 0, NULL);
    ctx.immutable = immutablep;
    ScmObj r = unwrap_rec(form, &ctx);
    patch_locations(&ctx);
    return r;
}

/*===================================================================
 * Initializer
 */

#define INIT_GLOC(gloc, name, mod)                                      \
    do {                                                                \
        gloc = Scm_FindBinding(mod, SCM_SYMBOL(SCM_INTERN(name)),       \
                               SCM_BINDING_STAY_IN_MODULE);             \
        if (gloc == NULL) {                                             \
            Scm_Panic("no " name " procedure in gauche.internal");      \
        }                                                               \
    } while (0)

void Scm__InitCompaux(void)
{
    ScmModule *g = Scm_GaucheModule();
    ScmModule *gi = Scm_GaucheInternalModule();

    Scm_InitStaticClass(SCM_CLASS_SYNTACTIC_CLOSURE, "<syntactic-closure>", g,
                        synclo_slots, 0);
    Scm_InitStaticClass(SCM_CLASS_IDENTIFIER, "<identifier>", g,
                        identifier_slots, 0);

    SCM_INTERNAL_MUTEX_INIT(compile_finish_mutex);

    /* Grab the entry points of compile.scm */
    INIT_GLOC(init_compiler_gloc,   "init-compiler", gi);
    INIT_GLOC(compile_gloc,         "compile",       gi);
    INIT_GLOC(compile_partial_gloc, "compile-partial", gi);
    INIT_GLOC(compile_finish_gloc,  "compile-finish",  gi);

    Scm_ApplyRec0(SCM_GLOC_GET(init_compiler_gloc));
}
