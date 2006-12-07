/*
 * macro.c - macro implementation
 *
 *   Copyright (c) 2000-2005 Shiro Kawai, All rights reserved.
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
 *  $Id: macro.c,v 1.64 2006-12-07 01:27:15 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/macro.h"
#include "gauche/code.h"
#include "gauche/vminsn.h"
#include "gauche/builtin-syms.h"

/* avoid C++ reserved name conflict.
   (I hate languages that take away names from programmers!) */
#define template templat

/* define if you want to debug syntax-rule expander */
/*#define DEBUG_SYNRULE*/

/*===================================================================
 * Syntax object
 */

static void syntax_print(ScmObj obj, ScmPort *port, ScmWriteContext *mode)
{
    Scm_Printf(port, "#<syntax %A>", SCM_SYNTAX(obj)->name);
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SyntaxClass, syntax_print);

ScmObj Scm_MakeSyntax(ScmSymbol *name, ScmObj handler)
{
    ScmSyntax *s = SCM_NEW(ScmSyntax);
    SCM_SET_CLASS(s, SCM_CLASS_SYNTAX);
    s->name = name;
    s->handler = handler;
    return SCM_OBJ(s);
}

/*===================================================================
 * Macro object
 */

static void macro_print(ScmObj obj, ScmPort *port, ScmWriteContext *mode)
{
    Scm_Printf(port, "#<macro %A>", SCM_MACRO(obj)->name);
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_MacroClass, macro_print);

ScmObj Scm_MakeMacro(ScmSymbol *name, ScmTransformerProc transformer,
                     void *data)
{
    ScmMacro *s = SCM_NEW(ScmMacro);
    SCM_SET_CLASS(s, SCM_CLASS_MACRO);
    s->name = name;
    s->transformer = transformer;
    s->data = data;
    return SCM_OBJ(s);
}

/*===================================================================
 * SyntaxPattern object
 *   Internal object to construct pattern matcher
 */

static void pattern_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<pattern:%d%S %S%s>",
               SCM_SYNTAX_PATTERN(obj)->level,
               SCM_SYNTAX_PATTERN(obj)->vars,
               SCM_SYNTAX_PATTERN(obj)->pattern,
               SCM_SYNTAX_PATTERN(obj)->repeat? " ..." : "");
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SyntaxPatternClass, pattern_print);

ScmSyntaxPattern *make_syntax_pattern(int level, int repeat)
{
    ScmSyntaxPattern *p = SCM_NEW(ScmSyntaxPattern);
    SCM_SET_CLASS(p, SCM_CLASS_SYNTAX_PATTERN);
    p->pattern = SCM_NIL;
    p->vars = SCM_NIL;
    p->level = level;
    p->repeat = repeat;
    return p;
}

/*===================================================================
 * SyntaxRules object
 *   Internal object to construct pattern matcher
 */

static void synrule_print(ScmObj obj, ScmPort *port, ScmWriteContext *mode)
{
    int i;
    ScmSyntaxRules *r = SCM_SYNTAX_RULES(obj);

    Scm_Printf(port, "#<syntax-rules(%d)\n", r->numRules);
    for (i = 0; i < r->numRules; i++) {
        Scm_Printf(port, "%2d: (numPvars=%d, maxLevel=%d)\n",
                   i, r->rules[i].numPvars, r->rules[i].maxLevel);
        Scm_Printf(port, "   pattern  = %S\n", r->rules[i].pattern);
        Scm_Printf(port, "   template = %S\n", r->rules[i].template);
    }
    Scm_Printf(port, ">");
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SyntaxRulesClass, synrule_print);

ScmSyntaxRules *make_syntax_rules(int nr) 
{
    ScmSyntaxRules *r = SCM_NEW2(ScmSyntaxRules *,
                                 sizeof(ScmSyntaxRules)+(nr-1)*sizeof(ScmSyntaxRuleBranch));
    SCM_SET_CLASS(r, SCM_CLASS_SYNTAX_RULES);
    r->numRules = nr;
    return r;
}

/*===================================================================
 * Macro for the new compiler
 */

/* In the new compiler, macro transformers for hygienic and traditional
 * macros are integrated.
 * The lowest-level macro transformer can be introduced by define-syntax,
 * let-syntax and letrec-syntax (but not syntax-case or syntax-rules; they
 * are built on top of it).
 *
 *   (define-syntax foo <transformer>)
 *
 * Where <transformer> is a procedure that takes one argument, a syntactic
 * closure.  It must return a syntactic closure as the result of trans
 * formation.
 *
 * From the point of the compiler, define-syntax triggers the following
 * actions.
 *
 *  - evaluate <transformer> in the compiler environment.
 *  - encapsulate it into <macro> object, and insert it to the compiler
 *    environment.
 *  - insert the binding to foo in the runtime toplevel environment.
 *
 * Define-macro is also built on top of define-syntax.  Concepturally,
 * it is transformed as follows.
 *
 *  (define-macro foo procedure)
 *   => (define-syntax foo
 *        (lambda (x)
 *          (let ((env  (slot-ref x 'env))
 *                (form (slot-ref x 'expr)))
 *            (make-syntactic-closure
 *              env () (apply procedure form)))))
 */

static ScmObj macro_transform(ScmObj self, ScmObj form, ScmObj env,
                              void *data)
{
    ScmObj proc = SCM_OBJ(data);
    SCM_ASSERT(SCM_SYNTACTIC_CLOSURE_P(form));
    return Scm_ApplyRec(proc, SCM_LIST1(form));
}

ScmObj Scm_MakeMacroTransformer(ScmSymbol *name, ScmObj proc)
{
    return Scm_MakeMacro(name, macro_transform, (void*)proc);
}

/*===================================================================
 * Traditional Macro
 */

/* TODO: how to retain debug info? */
/* TODO: better error message on syntax error (macro invocation with
   bad number of arguments) */

static ScmObj macro_transform_old(ScmObj self, ScmObj form,
                                  ScmObj env, void *data)
{
    ScmObj proc = SCM_OBJ(data);
    SCM_ASSERT(SCM_PAIRP(form));
#ifdef GAUCHE_VMAPI_VM
    return Scm_VMApply(Scm_VM(), proc, SCM_CDR(form));
#else
    return Scm_VMApply(proc, SCM_CDR(form));
#endif
}

ScmObj Scm_MakeMacroTransformerOld(ScmSymbol *name, ScmProcedure *proc)
{
    return Scm_MakeMacro(name, macro_transform_old, (void*)proc);
}

static ScmMacro *resolve_macro_autoload(ScmAutoload *adata)
{
    ScmObj mac = Scm_LoadAutoload(adata);
    if (!SCM_MACROP(mac)) {
        Scm_Error("tried to autoload macro %S, but it yields non-macro object: %S", adata->name, mac);
    }
    return SCM_MACRO(mac);
}

static ScmObj macro_autoload(ScmObj self, ScmObj form, ScmObj env, void *data)
{
    ScmMacro *mac = resolve_macro_autoload(SCM_AUTOLOAD(data));
    return mac->transformer(SCM_OBJ(mac), form, env, mac->data);
}

ScmObj Scm_MakeMacroAutoload(ScmSymbol *name, ScmAutoload *adata)
{
    return Scm_MakeMacro(name, macro_autoload, (void*)adata);
}

/*===================================================================
 * R5RS Macro
 */

/* Keeping hygienic reference
 *
 *  - symbols which a template inserts into the expanded form are
 *    converted to identifiers at the macro definition time, encapsulating
 *    the defining environment of the macro.   So it doesn't interfere
 *    with the macro call environment.
 *
 *  - literal symbols provided to the syntax-rules are also converted
 *    to identifiers encapsulating the defining environment, and the
 *    environment information is used when comparing with the symbols
 *    in the macro call.
 *
 *  - symbols in the macro call is treated as they are.  Since the result
 *    of macro expansion is immediately compiled in the macro call 
 *    environment, those symbols can refer proper bindings.
 */

/*-------------------------------------------------------------------
 * pattern language compiler
 *   - convert literals into identifiers
 *   - recognize repeatable subpatterns and replace it to SyntaxPattern node.
 *   - convert free symbols in the template into identifiers
 *   - convert pattern variables into LREF object.
 */
/* TODO: avoid unnecessary consing as much as possible */

/* context of pattern traversal */
typedef struct {                
    ScmObj name;                /* name of this macro (for error msg)*/
    ScmObj form;                /* form being compiled (for error msg) */
    ScmObj literals;            /* list of literal identifiers */
    ScmObj pvars;               /* list of (pvar . pvref) */
    int pvcnt;                  /* counter of pattern variables */
    int maxlev;                 /* maximum level */
    ScmObj tvars;               /* list of identifies inserted in template */
    ScmModule *mod;             /* module where this macro is defined */
    ScmObj env;                 /* compiler env of this macro definition */
} PatternContext;

#define PVREF_P(pvref)         SCM_PVREF_P(pvref)
#define PVREF_LEVEL(pvref)     SCM_PVREF_LEVEL(pvref)
#define PVREF_COUNT(pvref)     SCM_PVREF_COUNT(pvref)

/* add pattern variable pvar.  called when compiling a pattern */
static inline ScmObj add_pvar(PatternContext *ctx,
                              ScmSyntaxPattern *pat,
                              ScmObj pvar)
{
    ScmObj pvref = SCM_MAKE_PVREF(pat->level, ctx->pvcnt);
    if (!SCM_FALSEP(Scm_Assq(pvar, ctx->pvars))) {
        Scm_Error("pattern variable %S appears more than once in the macro definition of %S: %S", 
                  pvar, ctx->name, ctx->form);
    }
    ctx->pvcnt++;
    ctx->pvars = Scm_Acons(pvar, pvref, ctx->pvars);
    pat->vars = Scm_Cons(pvref, pat->vars);
    return pvref;
}

/* returns pvref corresponds to the given pvar in template compilation.
   if pvar is not a valid pvar, returns pvar itself. */
static inline ScmObj pvar_to_pvref(PatternContext *ctx,
                                   ScmSyntaxPattern *pat,
                                   ScmObj pvar)
{
    ScmObj q = Scm_Assq(pvar, ctx->pvars), pvref;
    if (!SCM_PAIRP(q)) return pvar;
    pvref = SCM_CDR(q);
    if (PVREF_LEVEL(pvref) > pat->level) {
        Scm_Error("%S: Pattern variable %S is used in wrong level: %S",
                  ctx->name, pvar, ctx->form);
    }
    return pvref;
}

static inline ScmObj pvref_to_pvar(PatternContext *ctx, ScmObj pvref)
{
    int count = PVREF_COUNT(pvref);
    ScmObj q = Scm_ListRef(ctx->pvars, count, SCM_UNBOUND);
    SCM_ASSERT(SCM_PAIRP(q));
    return SCM_CAR(q);
}

/* search an identifier with name NAME from a list of identifiers */
static ScmObj id_memq(ScmObj name, ScmObj list)
{
    ScmObj lp;
    ScmObj n;
    if (SCM_IDENTIFIERP(name)) {
        n = SCM_OBJ(SCM_IDENTIFIER(name)->name);
    } else {
        n = name;
    } 
    SCM_FOR_EACH(lp, list) {
        if (SCM_OBJ(SCM_IDENTIFIER(SCM_CAR(lp))->name) == name)
            return SCM_CAR(lp);
    }
    return SCM_FALSE;
}

#define ELLIPSIS_FOLLOWING(Pat) \
    (SCM_PAIRP(SCM_CDR(Pat)) && SCM_CADR(Pat)==SCM_SYM_ELLIPSIS)

#define BAD_ELLIPSIS(Ctx)                                               \
    Scm_Error("Bad ellipsis usage in macro definition of %S: %S",       \
               Ctx->name, Ctx->form)

/* convert literal symbols into identifiers */
static ScmObj preprocess_literals(ScmObj literals, ScmModule *mod, ScmObj env)
{
    ScmObj lp, h = SCM_NIL, t = SCM_NIL;
    SCM_FOR_EACH(lp, literals) {
        ScmObj lit = SCM_CAR(lp);
        if (SCM_IDENTIFIERP(lit))
            SCM_APPEND1(h, t, lit);
        else if (SCM_SYMBOLP(lit))
            SCM_APPEND1(h, t, Scm_MakeIdentifier(SCM_SYMBOL(lit), mod, env));
        else
            Scm_Error("literal list contains non-symbol: %S", literals);
    }
    if (!SCM_NULLP(lp))
        Scm_Error("bad literal list in syntax-rules: %S", literals);
    return h;
}

/* compile a pattern or a template.
   In a pattern, replace literal symbols for identifiers; leave
   non-literal symbols (i.e. pattern variables) as they are, but
   records it's presence in the context.   Also, when encounters
   a repeatable subpattern, replace it with SyntaxPattern node.
   In a template, replace symbols for identifiers except pattern variables.
*/

static ScmObj compile_rule1(ScmObj form,
                            ScmSyntaxPattern *spat,
                            PatternContext *ctx,
                            int patternp)
{
    if (SCM_PAIRP(form)) {
        ScmObj pp, h = SCM_NIL, t = SCM_NIL;
        SCM_FOR_EACH(pp, form) {
            if (ELLIPSIS_FOLLOWING(pp)) {
                ScmSyntaxPattern *nspat;
                if (patternp && !SCM_NULLP(SCM_CDDR(pp))) BAD_ELLIPSIS(ctx);
                nspat = make_syntax_pattern(spat->level+1, TRUE);
                if (ctx->maxlev <= spat->level) ctx->maxlev++;
                nspat->pattern = compile_rule1(SCM_CAR(pp), nspat, ctx,
                                               patternp);
                SCM_APPEND1(h, t, SCM_OBJ(nspat));
                if (!patternp) {
                    ScmObj vp;
                    if (SCM_NULLP(nspat->vars)) {
                        Scm_Error("in definition of macro %S: "
                                  "a template contains repetition "
                                  "of constant form: %S",
                                  ctx->name, form);
                    }
                    SCM_FOR_EACH(vp, nspat->vars) {
                        if (PVREF_LEVEL(SCM_CAR(vp)) >= nspat->level) break;
                    }
                    if (SCM_NULLP(vp)) {
                        Scm_Error("in definition of macro %S: "
                                  "template's ellipsis nesting"
                                  " is deeper than pattern's: %S",
                                  ctx->name, form);
                    }
                }
                spat->vars = Scm_Append2(spat->vars, nspat->vars);
                pp = SCM_CDR(pp);
            } else {
                SCM_APPEND1(h, t,
                            compile_rule1(SCM_CAR(pp), spat, ctx, patternp));
            }
        }
        if (!SCM_NULLP(pp))
            SCM_APPEND(h, t, compile_rule1(pp, spat, ctx, patternp));
        return h;
    }
    else if (SCM_VECTORP(form)) {
        /* TODO: this is a sloppy implementation.
           Eliminate intermediate list structure! */
        ScmObj l = Scm_VectorToList(SCM_VECTOR(form), 0, -1);
        return Scm_ListToVector(compile_rule1(l, spat, ctx, patternp), 0, -1);
    }
#if 0
    else if (patternp && SCM_IDENTIFIERP(form)) {
        /* this happens in a macro produced by another macro */
        form = SCM_OBJ(SCM_IDENTIFIER(form)->name);
    }
#endif
    if (SCM_SYMBOLP(form)||SCM_IDENTIFIERP(form)) {
        ScmObj q;
        if (form == SCM_SYM_ELLIPSIS) BAD_ELLIPSIS(ctx);
        if (!SCM_FALSEP(q = id_memq(form, ctx->literals))) return q;

        if (patternp) {
            return add_pvar(ctx, spat, form);
        } else {
            ScmObj id, pvref = pvar_to_pvref(ctx, spat, form);
            if (pvref == form) {
                /* form is not a pattern variable.  make it an identifier. */
                if (!SCM_FALSEP(q = id_memq(form, ctx->tvars))) return q;
                if (SCM_IDENTIFIERP(form)) {
                    id = form;
                } else {
                    id = Scm_MakeIdentifier(SCM_SYMBOL(form),
                                            ctx->mod, ctx->env);
                }
                ctx->tvars = Scm_Cons(id, ctx->tvars);
                return id;
            } else {
                spat->vars = Scm_Cons(pvref, spat->vars);
            }
            return pvref;
        }
    }
    return form;
}

/* compile rules into ScmSyntaxRules structure */
static ScmSyntaxRules *compile_rules(ScmObj name,
                                     ScmObj literals,
                                     ScmObj rules,
                                     ScmModule *mod,
                                     ScmObj env) /* compiler env */
{
    PatternContext ctx;
    ScmSyntaxPattern *pat, *tmpl;
    ScmSyntaxRules *sr;
    ScmObj rp;
    int numRules = Scm_Length(rules), i;

    if (numRules < 1) goto badform;
    if (Scm_Length(literals) < 0) goto badform;

    ctx.name = name;
    ctx.literals = preprocess_literals(literals, mod, env);
    ctx.mod = mod;
    ctx.env = env;

    sr = make_syntax_rules(numRules);
    sr->name = name;
    sr->numRules = numRules;
    sr->maxNumPvars = 0;
    for (i=0, rp = rules; i < numRules; i++, rp = SCM_CDR(rp)) {
        ScmObj rule = SCM_CAR(rp);
        if (Scm_Length(rule) != 2) goto badform;

        pat  = make_syntax_pattern(0, FALSE);
        tmpl = make_syntax_pattern(0, FALSE);
        ctx.pvars = SCM_NIL;
        ctx.tvars = SCM_NIL;
        ctx.pvcnt = 0;
        ctx.maxlev = 0;

        ctx.form = SCM_CAR(rule);
        if (!SCM_PAIRP(ctx.form)) goto badform;
        pat->pattern = compile_rule1(SCM_CDR(ctx.form), pat, &ctx, TRUE);

        ctx.form = SCM_CADR(rule);
        tmpl->pattern = compile_rule1(ctx.form, tmpl, &ctx, FALSE);

        sr->rules[i].pattern  = SCM_OBJ(pat->pattern);
        sr->rules[i].template = SCM_OBJ(tmpl->pattern);
        sr->rules[i].numPvars = ctx.pvcnt;
        sr->rules[i].maxLevel = ctx.maxlev;
        if (ctx.pvcnt > sr->maxNumPvars) sr->maxNumPvars = ctx.pvcnt;
    }
    return sr;

  badform:
    Scm_Error("malformed macro %S: %S", name,
              Scm_Cons(SCM_SYM_SYNTAX_RULES, Scm_Cons(literals, rules)));
    return NULL;       /* dummy */
}

/*-------------------------------------------------------------------
 * pattern language matcher
 */

/* Matchvec
 *   A sort of shallow binding technique is used to bind pattern
 *   variables with matched patterns.
 *
 *   Matchlist itself is an assoc list whose key is a pattern variable.
 *   It's value is a tree of the same depth of the pattern variable.
 *
 *   Suppose you have a pattern
 *      (?a (?b (?c ?d ...) ...) ...)
 *   In it, pattern variable ?a is level 0, ?b is 1, ?c is 2 and ?d is 3.
 *   When the pattern matches the following form:
 *      (1 (2 (3 4 5) (6)) (7 (8 9) (10 11 12)))
 *   trees bound to each pattern variables are like this:
 *
 *      ?a => 1
 *      ?b => (2 7)
 *      ?c => ((3 6) (8 10))
 *      ?d => (((4 5) ()) ((9) (11 12)))
 */

typedef struct {
    ScmObj branch;              /* current level match */
    ScmObj sprout;              /* current sprout */
    ScmObj root;                /* root of the tree */
} MatchVar;

static MatchVar *alloc_matchvec(int numPvars)
{
    return SCM_NEW_ARRAY(MatchVar, numPvars);
}

static void init_matchvec(MatchVar *mvec, int numPvars)
{
    int i;
    for (i=0; i<numPvars; i++) {
        mvec[i].branch = mvec[i].sprout = mvec[i].root = SCM_NIL;
    }
}

/* get value associated to the pvref.  if exhausted, return SCM_UNBOUND
   and set exhaust level in *exlev. */
static ScmObj get_pvref_value(ScmObj pvref, MatchVar *mvec,
                              int *indices, int *exlev)
{
    int level = PVREF_LEVEL(pvref), count = PVREF_COUNT(pvref);
    int i, j;
    ScmObj tree = mvec[count].root;
    for (i=1; i<=level; i++) {
        for (j=0; j<indices[i]; j++) {
            if (!SCM_PAIRP(tree)) {
                *exlev = i;
                return SCM_UNBOUND;
            }
            tree = SCM_CDR(tree);
        }
        if (!SCM_PAIRP(tree)) {
            *exlev = i;
            return SCM_UNBOUND;
        }
        tree = SCM_CAR(tree);
    }
    return tree;
}

/* for debug */
#ifdef DEBUG_SYNRULE
static void print_matchvec(MatchVar *mvec, int numPvars, ScmPort *port)
{
    int i;
    for (i=0; i<numPvars; i++) {
        Scm_Printf(port, "[%S %S %S]\n",
                   mvec[i].branch, mvec[i].sprout, mvec[i].root);
    }
}
#endif

static int match_synrule(ScmObj form, ScmObj pattern, ScmObj env,
                         MatchVar *mvec);

#define SPROUT  Scm_Cons(SCM_NIL, SCM_NIL)

/* add a new "sprout" to the given tree at the given level. */
static void grow_branch(MatchVar *rec, int level)
{
    ScmObj trunc;
    int i;
    if (level <= 1) return;
    if (rec->root == SCM_NIL) {
        rec->sprout = rec->root = SPROUT;
        if (level == 2) return;
    }
    
    trunc = rec->root;
    for (i=1; i<level-1; i++, trunc = SCM_CAR(trunc)) {
        SCM_FOR_EACH(trunc, trunc) {
            if (SCM_NULLP(SCM_CDR(trunc))) break;
        }
        if (SCM_NULLP(SCM_CAR(trunc))) {
            for (i++; i<level-1; i++, trunc = SCM_CAR(trunc)) {
                SCM_SET_CAR(trunc, SPROUT);
            }
            rec->sprout = SPROUT;
            SCM_SET_CAR(trunc, rec->sprout);
            return;
        }
    }
    SCM_FOR_EACH(trunc, trunc) {
        if (SCM_NULLP(SCM_CDR(trunc))) {
            rec->sprout = SPROUT;
            SCM_SET_CDR(trunc, rec->sprout);
            break;
        }
    }
}

static void enter_subpattern(ScmSyntaxPattern *subpat, MatchVar *mvec)
{
    ScmObj pp;
    SCM_FOR_EACH(pp, subpat->vars) {
        ScmObj pvref = SCM_CAR(pp);
        int count = PVREF_COUNT(pvref);
        grow_branch(mvec+count, subpat->level);
    }
}

static void exit_subpattern(ScmSyntaxPattern *subpat, MatchVar *mvec)
{
    ScmObj pp;
    SCM_FOR_EACH(pp, subpat->vars) {
        ScmObj pvref = SCM_CAR(pp);
        int count = PVREF_COUNT(pvref);
        if (PVREF_LEVEL(pvref) == subpat->level) {
            if (subpat->level == 1) {
                mvec[count].root = Scm_ReverseX(mvec[count].branch);
            } else {
                SCM_SET_CAR(mvec[count].sprout,
                            Scm_ReverseX(mvec[count].branch));
                mvec[count].branch = SCM_NIL;
            }
        }
    }
}

/* add pattern variable PVREF and its matched object MATCHED into MVEC */
static inline void match_insert(ScmObj pvref, ScmObj matched, MatchVar *mvec)
{
    int count = PVREF_COUNT(pvref);
    if (PVREF_LEVEL(pvref) == 0) {
        mvec[count].root = matched;
    } else {
        mvec[count].branch = Scm_Cons(matched, mvec[count].branch);
    }
}

/* see if literal identifier ID in the pattern matches the given object */
static inline int match_identifier(ScmIdentifier *id, ScmObj obj, ScmObj env)
{
    if (SCM_SYMBOLP(obj)) {
        return (id->name == SCM_SYMBOL(obj)
                && Scm_IdentifierBindingEqv(id, SCM_SYMBOL(obj), env));
    }
    if (SCM_IDENTIFIERP(obj)) {
        /*TODO: module?*/
        return (id->name == SCM_IDENTIFIER(obj)->name
                && id->env == SCM_IDENTIFIER(obj)->env);
    }
    return FALSE;
}

static inline int match_subpattern(ScmObj form, ScmSyntaxPattern *pat,
                                   ScmObj env, MatchVar *mvec)
{
    enter_subpattern(pat, mvec);
    while (SCM_PAIRP(form)) {
        if (!match_synrule(SCM_CAR(form), pat->pattern, env, mvec))
            return FALSE;
        form = SCM_CDR(form);
    }
    if (!SCM_NULLP(form)) return FALSE;
    exit_subpattern(pat, mvec);
    return TRUE;
}

/* See if form matches pattern.  If match, add matched syntax variable
   bindings to match vector and return TRUE; otherwise, return FALSE
*/
static int match_synrule(ScmObj form, ScmObj pattern, ScmObj env,
                         MatchVar *mvec)
{
    if (PVREF_P(pattern)) {
        match_insert(pattern, form, mvec);
        return TRUE;
    }
    if (SCM_IDENTIFIERP(pattern)) {
        return match_identifier(SCM_IDENTIFIER(pattern), form, env);
    }
    if (SCM_SYNTAX_PATTERN_P(pattern)) {
        return match_subpattern(form, SCM_SYNTAX_PATTERN(pattern), env, mvec);
    }
    if (SCM_PAIRP(pattern)) {
        while (SCM_PAIRP(pattern)) {
            ScmObj elt = SCM_CAR(pattern);
            if (SCM_SYNTAX_PATTERN_P(elt)) {
                return match_subpattern(form, SCM_SYNTAX_PATTERN(elt),
                                        env, mvec);
            } else if (!SCM_PAIRP(form)) {
                return FALSE;
            } else {
                if (!match_synrule(SCM_CAR(form), elt, env, mvec))
                    return FALSE;
                pattern = SCM_CDR(pattern);
                form = SCM_CDR(form);
            }
        }
        if (!SCM_NULLP(pattern))
            return match_synrule(form, pattern, env, mvec);
        else 
            return SCM_NULLP(form);
    }
    if (SCM_VECTORP(pattern)) {
        int i, plen, flen, elli;
        if (!SCM_VECTORP(form)) return FALSE;
        plen = SCM_VECTOR_SIZE(pattern);
        flen = SCM_VECTOR_SIZE(form);
        if (plen == 0) return (flen == 0);
        elli = SCM_SYNTAX_PATTERN_P(SCM_VECTOR_ELEMENT(pattern, plen-1));
        if ((!elli && plen!=flen) || (elli && plen-1>flen)) return FALSE;
        for (i=0; i < plen-elli; i++) {
            if (!match_synrule(SCM_VECTOR_ELEMENT(form, i),
                               SCM_VECTOR_ELEMENT(pattern, i),
                               env, mvec))
                return FALSE;
        }
        if (elli) {
            ScmObj h = SCM_NIL, t = SCM_NIL;
            ScmObj pat = SCM_VECTOR_ELEMENT(pattern, plen-1);
            for (i=plen-1; i<flen; i++) {
                SCM_APPEND1(h, t, SCM_VECTOR_ELEMENT(form, i));
            }
            return match_subpattern(h, SCM_SYNTAX_PATTERN(pat), env, mvec);
        }
        return TRUE;
    }

    /* literal */
    return Scm_EqualP(pattern, form);
}

/*-------------------------------------------------------------------
 * pattern language transformer
 */

/* If a pattern variable is exhausted, SCM_UNDEFINED is returned. */
static ScmObj realize_template_rec(ScmObj template,
                                   MatchVar *mvec,
                                   int level,
                                   int *indices,
                                   ScmObj *idlist,
                                   int *exlev)
{
    if (SCM_PAIRP(template)) {
        ScmObj h = SCM_NIL, t = SCM_NIL, r, e;
        while (SCM_PAIRP(template)) {
            e = SCM_CAR(template);
            if (SCM_SYNTAX_PATTERN_P(e)) {
                r = realize_template_rec(e, mvec, level, indices, idlist, exlev);
                if (SCM_UNBOUNDP(r)) return r;
                SCM_APPEND(h, t, r);
            } else {
                r = realize_template_rec(e, mvec, level, indices, idlist, exlev);
                if (SCM_UNBOUNDP(r)) return r;
                SCM_APPEND1(h, t, r);
            }
            template = SCM_CDR(template);
        }
        if (!SCM_NULLP(template)) {
            r = realize_template_rec(template, mvec, level, indices, idlist, exlev);
            if (SCM_UNBOUNDP(r)) return r;
            if (SCM_NULLP(h)) return r; /* (a ... . b) and a ... is empty */
            SCM_APPEND(h, t, r);
        }
        return h;
    }
    if (PVREF_P(template)) {
        return get_pvref_value(template, mvec, indices, exlev);
    }
    if (SCM_SYNTAX_PATTERN_P(template)) {
        ScmSyntaxPattern *pat = SCM_SYNTAX_PATTERN(template);
        ScmObj h = SCM_NIL, t = SCM_NIL, r;
        indices[level+1] = 0;
        for (;;) {
            r = realize_template_rec(pat->pattern, mvec, level+1, indices, idlist, exlev);
            if (SCM_UNBOUNDP(r)) return (*exlev < pat->level)? r : h;
            SCM_APPEND1(h, t, r);
            indices[level+1]++;
        }
    }
    if (SCM_VECTORP(template)) {
        ScmObj h = SCM_NIL, t = SCM_NIL, r, *pe;
        int len = SCM_VECTOR_SIZE(template), i;
        pe = SCM_VECTOR_ELEMENTS(template);
        
        for (i=0; i<len; i++, pe++) {
            if (SCM_SYNTAX_PATTERN_P(*pe)) {
                r = realize_template_rec(*pe, mvec, level, indices, idlist, exlev);
                if (SCM_UNBOUNDP(r)) return r;
                SCM_APPEND(h, t, r);
            } else {
                r = realize_template_rec(*pe, mvec, level, indices, idlist, exlev);
                if (SCM_UNBOUNDP(r)) return r;
                SCM_APPEND1(h, t, r);
            }
        }
        return Scm_ListToVector(h, 0, -1);
    }
    if (SCM_IDENTIFIERP(template)) {
        /* we copy the identifier, so that the symbol bindings introduced
           by recursive macro call won't interfere each other.
           (e.g. the macro definitions of "letrec" and "do" shown in R5RS
           use the fact that the symbol "newtemp" introduced in each
           iteration of macro expansion are distinct. */
        ScmObj p = Scm_Assq(template, *idlist);
        if (SCM_PAIRP(p)) return SCM_CDR(p);
        else {
            ScmObj id = Scm_CopyIdentifier(SCM_IDENTIFIER(template));
            *idlist = Scm_Acons(template, id, *idlist);
            return id;
        }
    }
    return template;
}

#define DEFAULT_MAX_LEVEL  10

static ScmObj realize_template(ScmSyntaxRuleBranch *branch,
                               MatchVar *mvec)
{
    int index[DEFAULT_MAX_LEVEL], *indices = index, i;
    int exlev = 0;
    ScmObj idlist = SCM_NIL;
    
    if (branch->maxLevel > DEFAULT_MAX_LEVEL)
        indices = SCM_NEW_ATOMIC2(int*, (branch->maxLevel+1) * sizeof(int));
    for (i=0; i<=branch->maxLevel; i++) indices[i] = 0;
    return realize_template_rec(branch->template, mvec, 0, indices, &idlist, &exlev);
}

static ScmObj synrule_expand(ScmObj form, ScmObj env, ScmSyntaxRules *sr)
{
    MatchVar *mvec = alloc_matchvec(sr->maxNumPvars);
    ScmObj expanded;
    int i;

#ifdef DEBUG_SYNRULE    
    Scm_Printf(SCM_CUROUT, "**** synrule_transform: %S\n", form);
#endif
    for (i=0; i<sr->numRules; i++) {
#ifdef DEBUG_SYNRULE    
        Scm_Printf(SCM_CUROUT, "pattern #%d: %S\n", i, sr->rules[i].pattern);
#endif
        init_matchvec(mvec, sr->rules[i].numPvars);
        if (match_synrule(SCM_CDR(form), sr->rules[i].pattern, env, mvec)) {
#ifdef DEBUG_SYNRULE    
            Scm_Printf(SCM_CUROUT, "success #%d:\n", i);
            print_matchvec(mvec, sr->rules[i].numPvars, SCM_CUROUT);
#endif
            expanded = realize_template(&sr->rules[i], mvec);
#ifdef DEBUG_SYNRULE    
            Scm_Printf(SCM_CUROUT, "result: %S\n", expanded);
#endif
            return expanded;
        }
    }
    Scm_Error("malformed %S: %S", SCM_CAR(form), form);
    return SCM_NIL;
}

static ScmObj synrule_transform(ScmObj self, ScmObj form, ScmObj env,
                                void *data)
{
    ScmSyntaxRules *sr = (ScmSyntaxRules *)data;
    return synrule_expand(form, env, sr);
}

/* NB: a stub for the new compiler (TEMPORARY) */
ScmObj Scm_CompileSyntaxRules(ScmObj name, ScmObj literals, ScmObj rules,
                              ScmObj mod, ScmObj env)
{
    ScmSyntaxRules *sr;

    if (SCM_IDENTIFIERP(name)) name = SCM_OBJ(SCM_IDENTIFIER(name)->name);
    else if (!SCM_SYMBOLP(name)) {
        Scm_Error("symbol required, but got %S", name);
    }
    if (!SCM_MODULEP(mod)) Scm_Error("module required, but got %S", mod);
    sr = compile_rules(name, literals, rules, SCM_MODULE(mod), env);
    return Scm_MakeMacro(SCM_SYMBOL(name), synrule_transform, (void*)sr);
}

/*===================================================================
 * macro-expand
 */

ScmObj macro_expand_cc(GAUCHE_CC_VM_ARG ScmObj result, void **data)
{
    ScmObj env = SCM_OBJ(data[0]);
    return Scm_VMMacroExpand(result, env, FALSE);
}

ScmObj Scm_VMMacroExpand(ScmObj expr, ScmObj env, int oncep)
{
    ScmObj sym, op;
    ScmMacro *mac;

    if (!SCM_PAIRP(expr)) return expr;
    op = SCM_CAR(expr);
    if (SCM_MACROP(op)) {
        mac = SCM_MACRO(op);
    } else if (!SCM_SYMBOLP(op) && !SCM_IDENTIFIERP(op)) {
        return expr;
    } else {
        mac = NULL;
        sym = op;
        if (SCM_MACROP(sym)) {
            /* local syntactic binding */
            mac = SCM_MACRO(sym);
        } else {
            if (SCM_IDENTIFIERP(sym)) {
                sym = SCM_OBJ(SCM_IDENTIFIER(sym)->name);
            }
            if (SCM_SYMBOLP(sym)) {
                ScmGloc *g = Scm_FindBinding(Scm_VM()->module,
                                             SCM_SYMBOL(sym), 0);
                if (g) {
                    ScmObj gv = SCM_GLOC_GET(g);
                    if (SCM_MACROP(gv)) mac = SCM_MACRO(gv);
                }
            }
        }
    }
    if (mac) {
        if (!oncep) {
            void *data[1];
            data[0] = env;
#ifdef GAUCHE_VMAPI_VM
            Scm_VMPushCC(Scm_VM(), macro_expand_cc, data, 1);
#else
            Scm_VMPushCC(macro_expand_cc, data, 1);
#endif
        }
        expr = Scm_CallMacroExpander(mac, expr, env);
    }
    return expr;
}

ScmObj Scm_CallMacroExpander(ScmMacro *mac, ScmObj expr, ScmObj env)
{
    return mac->transformer(SCM_OBJ(mac), expr, env, mac->data);
}

/*===================================================================
 * Initializer
 */

void Scm__InitMacro(void)
{
}
