/*
 * macro.c - macro implementation
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
 *  $Id: macro.c,v 1.5 2001-02-22 19:40:00 shiro Exp $
 */

#include "gauche.h"
#include "gauche/macro.h"

/*===================================================================
 * Syntax object
 */

static int syntax_print(ScmObj obj, ScmPort *port, int mode)
{
    return Scm_Printf(port, "#<syntax %A>", SCM_SYNTAX(obj)->name);
}

SCM_DEFCLASS(Scm_SyntaxClass, "<syntax>", syntax_print, SCM_CLASS_DEFAULT_CPL);

ScmObj Scm_MakeSyntax(ScmSymbol *name, ScmCompileProc compiler, void *data)
{
    ScmSyntax *s = SCM_NEW(ScmSyntax);
    SCM_SET_CLASS(s, SCM_CLASS_SYNTAX);
    s->name = name;
    s->compiler = compiler;
    s->data = data;
    return SCM_OBJ(s);
}

/*===================================================================
 * SyntaxPattern object
 *   Internal object to construct pattern matcher
 */

static int pattern_print(ScmObj obj, ScmPort *port, int mode)
{
    return Scm_Printf(port, "#<pattern/%d/%S %S%s >",
                      SCM_SYNTAX_PATTERN(obj)->level,
                      SCM_SYNTAX_PATTERN(obj)->vars,
                      SCM_SYNTAX_PATTERN(obj)->pattern,
                      SCM_SYNTAX_PATTERN(obj)->repeat? " ..." : "");
}

SCM_DEFCLASS(Scm_SyntaxPatternClass, "<syntax-pattern>",
             pattern_print, SCM_CLASS_DEFAULT_CPL);

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
 * Traditional Macro
 */

/* TODO: how to retain debug info? */
/* TODO: better error message on syntax error (macro invocation with
   bad number of arguments) */

static ScmObj macro_transform(ScmObj form, ScmObj env, int ctx, void *data)
{
    ScmObj proc = SCM_OBJ(data);
    ScmObj newform = Scm_Apply(proc, form);
    return Scm_Compile(newform, env, ctx);
}

ScmObj Scm_MakeMacroTransformer(ScmSymbol *name, ScmProcedure *proc)
{
    return Scm_MakeSyntax(name, macro_transform, (void*)proc);
}

/* Expand macro.
 * To capture locally-bound macros, macro-expand needs to be a syntax.
 * From scheme, this syntax is visible as %macro-expand.
 * The procedure version, which works only for globally defined macros,
 * can be defined as
 *  (define (macro-expand form) (%macro-expand form))
 */
static ScmObj compile_macro_expand(ScmObj form, ScmObj env,
                                   int ctx, void *data)
{
    ScmObj expr, sym;
    ScmGloc *gloc;
    
    if (!SCM_PAIRP(SCM_CDR(form)) || !SCM_NULLP(SCM_CDDR(form)))
        Scm_Error("syntax error: %S", form);
    expr = SCM_CADR(form);
    if (!SCM_PAIRP(expr)) return SCM_LIST1(expr);
    if (!SCM_SYMBOLP(SCM_CAR(expr))) return SCM_LIST1(expr);
    
    sym = Scm_CompileLookupEnv(SCM_CAR(expr), env, TRUE);
    /* TODO: adapt to the new compiler API (check identifier/syntax) */
    if (SCM_SYMBOLP(sym)) {
        ScmGloc *g = Scm_FindBinding(Scm_VM()->module, SCM_SYMBOL(sym), FALSE);
        if (g && SCM_SYNTAXP(g->value)) {
            ScmSyntax *syn = SCM_SYNTAX(g->value);
            if (syn->compiler == macro_transform) {
                ScmObj proc = SCM_OBJ(syn->data);
                ScmObj translated = Scm_Apply(proc, expr);
                return SCM_LIST1(translated);
            }
        }
    }
    return SCM_LIST1(expr);
}

static ScmSyntax syntax_macro_expand = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_MACRO_EXPAND),
    compile_macro_expand,
    NULL
};

/*===================================================================
 * R5RS Macro
 */

/* To be a handy scripting interpreter, expansion speed of complex macro
 * is important.  That's why I impelement it in C.
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
 * pattern language preprocessor
 *   - convert literals into identifiers
 *   - recognize repeatable subpatterns and replace it to SyntaxPattern node.
 *   - convert all the free symbols in the template into identifiers
 */
/* TODO: avoid unnecessary consing as much as possible */

/* context of pattern traversal */
typedef struct {                
    ScmObj name;                /* name of this macro (for error msg)*/
    ScmObj pattern;             /* entire pattern (for error msg) */
    ScmObj literals;            /* list of literal identifiers */
    ScmObj pvars;               /* list of (pattern-variable . level) */
    ScmObj tvars;               /* list of identifies inserted in template */
    ScmObj env;                 /* compiler env of this macro definition */
} pattern_ctx;

#define PUSH_PVAR(Ctx, Pat, Pvar)                                       \
    do {                                                                \
        ScmObj q = Scm_Assq(Pvar, Ctx->pvars);                          \
        if (!SCM_FALSEP(q))                                             \
            Scm_Error("pattern variable %S appears more than once "     \
                      "in the macro definition of %S: %S",              \
                      Pvar, Ctx->name, Ctx->pattern);                   \
        Ctx->pvars = Scm_Acons(Pvar, SCM_MAKE_INT(Pat->level),          \
                               Ctx->pvars);                             \
        Pat->vars = Scm_Cons(Pvar, Pat->vars);                          \
    } while (0)

#define ELLIPSIS_FOLLOWING(Pat) \
    (SCM_PAIRP(SCM_CDR(Pat)) && SCM_CADR(Pat)==SCM_SYM_ELLIPSIS)

#define BAD_ELLIPSIS(Ctx)                                               \
    Scm_Error("Bad ellipsis usage in macro definition of %S: %S",       \
               Ctx->name, Ctx->pattern)

#define BAD_PVAR(Ctx, Pvar)                                             \
    Scm_Error("%S: Pattern variable %S is used in wrong level: %S",     \
              Ctx->name, Pvar, Ctx->pattern)

/* convert literal symbols into identifiers */
static ScmObj preprocess_literals(ScmObj literals, ScmObj env)
{
    ScmObj lp, h = SCM_NIL, t;
    SCM_FOR_EACH(lp, literals) {
        ScmObj lit = SCM_CAR(lp);
        if (SCM_IDENTIFIERP(lit))
            SCM_APPEND1(h, t, lit);
        else if (SCM_SYMBOLP(lit))
            SCM_APPEND1(h, t, Scm_MakeIdentifier(SCM_SYMBOL(lit), env));
        else
            Scm_Error("literal list contains non-symbol: %S", literals);
    }
    if (!SCM_NULLP(lp))
        Scm_Error("bad literal list in syntax-rules: %S", literals);
    return h;
}

/* in a pattern, replace literal symbols into identifiers.   Leave
   non-literal symbols (i.e. pattern variables) as they are, but
   records it's presence in patvars structure.   Also, when encounters
   a repeatable subpattern, replace it with SyntaxPattern node. */

static ScmObj preprocess_pattern(ScmObj pattern,
                                 ScmSyntaxPattern *spat,
                                 pattern_ctx *ctx)
{
    if (SCM_PAIRP(pattern)) {
        ScmObj pp, h = SCM_NIL, t;
        SCM_FOR_EACH(pp, pattern) {
            if (ELLIPSIS_FOLLOWING(pp)) {
                ScmSyntaxPattern *nspat;
                if (!SCM_NULLP(SCM_CDDR(pp))) BAD_ELLIPSIS(ctx);
                nspat = make_syntax_pattern(spat->level+1, TRUE);
                nspat->pattern = preprocess_pattern(SCM_CAR(pp), nspat, ctx);
                SCM_APPEND1(h, t, SCM_OBJ(nspat));
                return h;
            }
            SCM_APPEND1(h, t, preprocess_pattern(SCM_CAR(pp), spat, ctx));
        }
        if (!SCM_NULLP(pp))
            SCM_APPEND1(h, t, preprocess_pattern(pp, spat, ctx));
        return h;
    }
    else if (SCM_VECTORP(pattern)) {
        int i, len = SCM_VECTOR_SIZE(pattern);
        ScmObj *pe = SCM_VECTOR_ELEMENTS(pattern);
        ScmObj nv;
        nv = Scm_MakeVector((pe[len-1] == SCM_SYM_ELLIPSIS ? len-1 : len),
                            SCM_FALSE);
        for (i=0; i<len-1; i++, pe++) {
            if (*(pe+1) == SCM_SYM_ELLIPSIS) {
                ScmSyntaxPattern *nspat;
                if (i != len-2) BAD_ELLIPSIS(ctx);
                nspat = make_syntax_pattern(spat->level+1, TRUE);
                nspat->pattern = preprocess_pattern(*pe, nspat, ctx);
                SCM_VECTOR_ELEMENT(nv, i) = SCM_OBJ(nspat);
                break;
            }
            SCM_VECTOR_ELEMENT(nv, i) = preprocess_pattern(*pe, spat, ctx);
        }
        return nv;
    }
    else if (SCM_IDENTIFIERP(pattern)) {
        /* this happens in a macro produced by another macro */
        pattern = SCM_OBJ(SCM_IDENTIFIER(pattern)->name);
    }
    if (SCM_SYMBOLP(pattern)) {
        ScmObj lp;
        if (pattern == SCM_SYM_ELLIPSIS) BAD_ELLIPSIS(ctx);
        SCM_FOR_EACH(lp, ctx->literals) {
            if (SCM_OBJ(SCM_IDENTIFIER(SCM_CAR(lp))->name) == pattern) {
                return SCM_CAR(lp);
            }
        }
        PUSH_PVAR(ctx, spat, pattern);
    }
    return pattern;
}

static ScmObj preprocess_template(ScmObj templ,
                                  ScmSyntaxPattern *spat,
                                  pattern_ctx *ctx)
{
    if (SCM_SYMBOLP(templ)) {
        ScmObj lp, id, q;
        if (templ == SCM_SYM_ELLIPSIS) BAD_ELLIPSIS(ctx);
        SCM_FOR_EACH(lp, ctx->literals) {
            if (SCM_OBJ(SCM_IDENTIFIER(SCM_CAR(lp))->name) == templ)
                return SCM_CAR(lp);
        }
        q = Scm_Assq(templ, ctx->pvars);
        if (!SCM_FALSEP(q)) {
            int level;
            SCM_ASSERT(SCM_INTP(SCM_CDAR(lp)));
            level = SCM_INT_VALUE(SCM_CDAR(lp));
            if (level != spat->level) BAD_PVAR(ctx, templ);
            spat->vars = Scm_Cons(templ, spat->vars);
            return templ;
        }
        SCM_FOR_EACH(lp, SCM_CDR(ctx->tvars)) {
            if (SCM_OBJ(SCM_IDENTIFIER(SCM_CAR(lp))->name) == templ) {
                return SCM_CAR(lp);
            }
        }
        id = Scm_MakeIdentifier(SCM_SYMBOL(templ), ctx->env);
        ctx->tvars = Scm_Cons(id, ctx->tvars);
        return id;
    }
    if (SCM_PAIRP(templ)) {
        ScmObj cp, h=SCM_NIL, t;
        SCM_FOR_EACH(cp, templ) {
            if (ELLIPSIS_FOLLOWING(cp)) {
                ScmSyntaxPattern *nspat;
                if (!SCM_NULLP(SCM_CDDR(cp))) BAD_ELLIPSIS(ctx);
                nspat = make_syntax_pattern(spat->level+1, TRUE);
                nspat->pattern = preprocess_template(SCM_CAR(cp), nspat, ctx);
                SCM_APPEND(h, t, SCM_OBJ(nspat));
            }
            SCM_APPEND1(h, t, preprocess_template(SCM_CAR(cp), spat, ctx));
        }
        if (!SCM_NULLP(cp)) {
            SCM_APPEND(h, t, preprocess_template(cp, spat, ctx));
        }
        return h;
    }
    if (SCM_VECTORP(templ)) {
        int i, len = SCM_VECTOR_SIZE(templ);
        ScmObj *pe = SCM_VECTOR_ELEMENTS(templ);
        ScmObj nv;
        nv = Scm_MakeVector((pe[len-1] == SCM_SYM_ELLIPSIS ? len-1 : len),
                            SCM_FALSE);
        for (i=0; i<len; i++, pe++) {
            if (*(pe+1) == SCM_SYM_ELLIPSIS) {
                ScmSyntaxPattern *nspat;
                if (i != len-2) BAD_ELLIPSIS(ctx);
                nspat = make_syntax_pattern(spat->level+1, TRUE);
                nspat->pattern = preprocess_template(*pe, nspat, ctx);
                SCM_VECTOR_ELEMENT(nv, i) = SCM_OBJ(nspat);
                break;
            }
            SCM_VECTOR_ELEMENT(nv, i) = preprocess_template(*pe, spat, ctx);
        }
        return nv;
    }
    return templ;
}

/*-------------------------------------------------------------------
 * pattern language matcher
 */

/* add pattern variable VAR and its matched object MATCHED into MATCHLIST */
static inline ScmObj match_insert(ScmObj var, ScmObj matched, ScmObj matchlist)
{
    ScmObj p = Scm_Assq(var, matchlist);
    if (SCM_PAIRP(p)) {
        SCM_SET_CDR(p, Scm_Cons(matched, SCM_CDR(p)));
        return matchlist;
    } else {
        return Scm_Cons(SCM_LIST2(var, matched), matchlist);
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

/* See if form matches pattern.  If match, add matched syntax variable
   bindings to matchlist and returns modified matchlist. */
static ScmObj match_synrule(ScmObj form, ScmObj pattern, ScmObj env,
                            ScmObj matchlist)
{
    ScmObj r;

/*    Scm_Printf(SCM_CUROUT, "--- %S %S %S\n", form, pattern, matchlist);*/
    if (SCM_SYMBOLP(pattern)) {
        return match_insert(pattern, form, matchlist);
    }
    if (SCM_IDENTIFIERP(pattern)) {
        if (match_identifier(SCM_IDENTIFIER(pattern), form, env))
            return matchlist;
        else
            return SCM_FALSE;
    }
    if (SCM_PAIRP(pattern)) {
        while (SCM_PAIRP(pattern)) {
            if (ELLIPSIS_FOLLOWING(pattern)) {
                while (SCM_PAIRP(form)) {
                    r = match_synrule(SCM_CAR(form), SCM_CAR(pattern), env, matchlist);
                    if (SCM_FALSEP(r)) return SCM_FALSE;
                    form = SCM_CDR(form);
                    matchlist = r;
                }
                if (!SCM_NULLP(form)) return SCM_FALSE;
                else return matchlist;
            } else if (!SCM_PAIRP(form)) {
                return SCM_FALSE;
            } else {
                r = match_synrule(SCM_CAR(form), SCM_CAR(pattern), env, matchlist);
                if (SCM_FALSEP(r)) return SCM_FALSE;
                matchlist = r;
                pattern = SCM_CDR(pattern);
                form = SCM_CDR(form);
            }
        }
        if (!SCM_NULLP(pattern))
            return match_insert(pattern, form, matchlist);
        else 
            return SCM_NULLP(form)? matchlist : SCM_FALSE;
    }
    if (SCM_VECTORP(pattern)) {
        int i, plen, flen, elli;
        if (!SCM_VECTORP(form)) return SCM_FALSE;
        plen = SCM_VECTOR_SIZE(pattern);
        flen = SCM_VECTOR_SIZE(form);
        if (plen == 0) return (flen == 0 ? matchlist: SCM_FALSE);
        elli = (SCM_VECTOR_ELEMENT(pattern, plen-1) == SCM_SYM_ELLIPSIS)? 2 : 0;
        if (plen < 2 && elli) Scm_Error("bad pattern: %S", form);
        if ((!elli && plen!=flen) || (elli && plen-2>flen)) return SCM_FALSE;
        for (i=0; i < plen-elli; i++) {
            r = match_synrule(SCM_VECTOR_ELEMENT(form, i),
                              SCM_VECTOR_ELEMENT(pattern, i),
                              env, matchlist);
            if (SCM_FALSEP(r)) return SCM_FALSE;
            matchlist = r;
        }
        if (!elli) return matchlist;
        for (i=plen-elli; i<flen; i++) {
            r = match_synrule(SCM_VECTOR_ELEMENT(form, i),
                              SCM_VECTOR_ELEMENT(pattern, plen-2),
                              env, matchlist);
            if (SCM_FALSEP(r)) return SCM_FALSE;
            matchlist = r;
        }
        return matchlist;
    }

    /* literal */
    if (Scm_EqualP(pattern, form)) return matchlist;
    else return SCM_FALSE;
}

/*-------------------------------------------------------------------
 * pattern language transformer
 */

static ScmObj synrule_transform(ScmObj form, ScmObj env,
                                int ctx, void *data)
{
    ScmObj cp;
    ScmObj name = SCM_CAR(data), rules = SCM_CDR(data);
    
    Scm_Printf(SCM_CUROUT, "**** synrule_transform: %S\n", form);
    SCM_FOR_EACH(cp, rules) {
        ScmObj r = match_synrule(form, SCM_CAAR(cp), env, SCM_NIL);
        if (!SCM_FALSEP(r)) {
            Scm_Printf(SCM_CUROUT, "match %S => %S\n",
                       SCM_CAAR(cp), r);
            return SCM_NIL;
        }
    }
    Scm_Error("malformed %S: %S", name, form);
    return SCM_NIL;
}

static ScmObj make_synrule_transformer(ScmSymbol *name, ScmObj rules)
{
    return Scm_MakeSyntax(name,
                          synrule_transform,
                          (void*)Scm_Cons(SCM_OBJ(name), rules));
}

/*-------------------------------------------------------------------
 * %syntax-rules
 *    Internal macro of syntax-rules.  Taking macro name as the first arg.
 */
static ScmObj compile_syntax_rules(ScmObj form, ScmObj env,
                                   int ctx, void *data)
{
    ScmObj name, literals, litids, rules, cp;
    ScmObj rhead = SCM_NIL, rtail, tmpids;
    ScmSyntaxPattern *spat;
    pattern_ctx *pctx;

    if (Scm_Length(form) < 4) {
        SCM_ASSERT(SCM_PAIRP(SCM_CDR(form)));
        goto badform;
    }
    name = SCM_CADR(form);
    if (SCM_IDENTIFIERP(name)) name = SCM_OBJ(SCM_IDENTIFIER(name)->name);
    SCM_ASSERT(SCM_SYMBOLP(name));
    
    literals = SCM_CAR(SCM_CDDR(form));
    rules = SCM_CDR(SCM_CDDR(form));

    litids = preprocess_literals(literals, env);
    tmpids = Scm_Cons(SCM_NIL, SCM_NIL);

    spat = make_syntax_pattern(0, FALSE);
    pctx = SCM_NEW(pattern_ctx);
    pctx->name = name;
    pctx->literals = litids;
    pctx->pvars = SCM_NIL;
    pctx->tvars = SCM_NIL;
    pctx->env = env;
    
    SCM_FOR_EACH(cp, rules) {
        ScmObj rule = SCM_CAR(cp), templ, patvars, pattern;
        if (Scm_Length(rule) != 2) goto badform;
        pctx->pattern = SCM_CAR(rule);
        if (!SCM_PAIRP(pctx->pattern)) goto badform;
        spat->pattern = preprocess_pattern(SCM_CDAR(rule), spat, pctx);
#if 0        
        templ = preprocess_template(SCM_CADR(rule), SCM_CDR(patvars), litids,
                                    tmpids, env);
#endif
        SCM_APPEND1(rhead, rtail, SCM_OBJ(spat));
    }
    if (!SCM_NULLP(cp)) goto badform;

    Scm_Printf(SCM_CUROUT, "lit=%S, rules=%S\n", litids, rhead);
    
    return SCM_LIST1(make_synrule_transformer(SCM_SYMBOL(name), rhead));
  badform:
    Scm_Error("malformed syntax-rules: ",
              Scm_Cons(SCM_INTERN("syntax-rules"), SCM_CDDR(form)));
    return SCM_NIL;
}

static ScmSyntax syntax_syntax_rules = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_SYNTAX_RULES),
    compile_syntax_rules,
    NULL
};

/*===================================================================
 * Initializer
 */

void Scm__InitMacro(void)
{
    ScmModule *m = SCM_MODULE(Scm_SchemeModule());

#define DEFSYN(symbol, syntax) \
    Scm_Define(m, SCM_SYMBOL(symbol), SCM_OBJ(&syntax))
    
    DEFSYN(SCM_SYM_MACRO_EXPAND, syntax_macro_expand);
    DEFSYN(SCM_SYM_SYNTAX_RULES, syntax_syntax_rules);
}
