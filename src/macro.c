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
 *  $Id: macro.c,v 1.4 2001-02-22 06:50:11 shiro Exp $
 */

#include "gauche.h"

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
 *   - convert all the free symbols in the template into identifiers
 */

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

/* look into a pattern, and convert symbols in a literal list into
   identifiers.  also collect pattern variables into patvars */
static ScmObj preprocess_pattern(ScmObj pattern, ScmObj litids, ScmObj patvars)
{
    if (SCM_PAIRP(pattern)) {
        ScmObj pp, elt, h = SCM_NIL, t;
        SCM_FOR_EACH(pp, pattern) {
            elt = SCM_CAR(pp);
            SCM_APPEND1(h, t, preprocess_pattern(elt, litids, patvars));
        }
        if (!SCM_NULLP(pp))
            SCM_APPEND1(h, t, preprocess_pattern(pp, litids, patvars));
        return h;
    }
    else if (SCM_VECTORP(pattern)) {
        int i, len = SCM_VECTOR_SIZE(pattern);
        ScmObj *pe = SCM_VECTOR_ELEMENTS(pattern);
        ScmObj nv = Scm_MakeVector(len, SCM_FALSE);
        for (i=0; i<len; i++, pe++) {
            ScmObj p = preprocess_pattern(*pe, litids, patvars);
            SCM_VECTOR_ELEMENT(nv, i) = p;
        }
        return nv;
    }
    else if (SCM_IDENTIFIERP(pattern)) {
        /* this happens in a macro produced by another macro */
        pattern = SCM_OBJ(SCM_IDENTIFIER(pattern)->name);
    }
    if (SCM_SYMBOLP(pattern)) {
        ScmObj lp;
        if (pattern == SCM_SYM_ELLIPSIS) return pattern;
        SCM_FOR_EACH(lp, litids) {
            if (SCM_OBJ(SCM_IDENTIFIER(SCM_CAR(lp))->name) == pattern) {
                return SCM_CAR(lp);
            }
        }
        SCM_SET_CDR(patvars, Scm_Cons(pattern, SCM_CDR(patvars)));
        return pattern;
    }
    return pattern;
}

static ScmObj preprocess_template(ScmObj templ,
                                  ScmObj patvars, /* pattern variables */
                                  ScmObj lits, /* list of literal identifiers */
                                  ScmObj tempids, /* identifiers inserted by
                                                     this template */
                                  ScmObj env)
{
    if (SCM_SYMBOLP(templ)) {
        ScmObj lp, id;
        if (templ == SCM_SYM_ELLIPSIS) return templ;
        SCM_FOR_EACH(lp, lits) {
            if (SCM_OBJ(SCM_IDENTIFIER(SCM_CAR(lp))->name) == templ)
                return SCM_CAR(lp);
        }
        SCM_FOR_EACH(lp, patvars) {
            if (templ == SCM_CAR(lp))
                return SCM_CAR(lp);
        }
        SCM_FOR_EACH(lp, SCM_CDR(tempids)) {
            if (SCM_OBJ(SCM_IDENTIFIER(SCM_CAR(lp))->name) == templ) {
                return SCM_CAR(lp);
            }
        }
        id = Scm_MakeIdentifier(SCM_SYMBOL(templ), env);
        SCM_SET_CDR(tempids, Scm_Cons(id, SCM_CDR(tempids)));
        return id;
    }
    if (SCM_PAIRP(templ)) {
        ScmObj cp, h=SCM_NIL, t;
        SCM_FOR_EACH(cp, templ) {
            SCM_APPEND1(h, t, preprocess_template(SCM_CAR(cp), patvars,
                                                  lits, tempids, env));
        }
        if (!SCM_NULLP(cp)) {
            SCM_APPEND1(h, t, preprocess_template(cp, patvars,
                                                  lits, tempids, env));
        }
        return h;
    }
    if (SCM_VECTORP(templ)) {
        int i, len = SCM_VECTOR_SIZE(templ);
        ScmObj *pe = SCM_VECTOR_ELEMENTS(templ);
        ScmObj nv = Scm_MakeVector(len, SCM_FALSE);
        for (i=0; i<len; i++, pe++) {
            SCM_VECTOR_ELEMENT(nv, i) = preprocess_template(*pe, patvars,
                                                            lits, tempids, env);
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

#define ELLIPSIS_FOLLOWING(pat) \
    (SCM_PAIRP(SCM_CDR(pat)) && SCM_CADR(pat)==SCM_SYM_ELLIPSIS)

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
    ScmObj literals = SCM_CAR(data);
    ScmObj rules = SCM_CADR(data);
    ScmObj cmpl_env = SCM_CAR(SCM_CDDR(data));
    
    Scm_Printf(SCM_CUROUT, "**** synrule_transform: %S\n", form);
    SCM_FOR_EACH(cp, rules) {
        ScmObj r = match_synrule(form, SCM_CAAR(cp), env, SCM_NIL);
        Scm_Printf(SCM_CUROUT, "  %S => %S\n", SCM_CAAR(cp), r);
    }
    return SCM_NIL;
}

static ScmObj make_synrule_transformer(ScmObj literals, ScmObj rules,
                                       ScmObj cmpl_env)
{
    return Scm_MakeSyntax(SCM_SYMBOL(SCM_INTERN("macro")), /* TODO: need better info */
                          synrule_transform,
                          (void*)SCM_LIST3(literals, rules, cmpl_env));
}

/*-------------------------------------------------------------------
 * syntax-rules
 */
static ScmObj compile_syntax_rules(ScmObj form, ScmObj env,
                                   int ctx, void *data)
{
    ScmObj literals, litids, rules, cp;
    ScmObj rhead = SCM_NIL, rtail, tmpids;
    
    if (Scm_Length(form) < 3)
        Scm_Error("malformed syntax-rules: %S", form);
    literals = SCM_CADR(form);
    rules = SCM_CDDR(form);

    litids = preprocess_literals(literals, env);
    tmpids = Scm_Cons(SCM_NIL, SCM_NIL);
    SCM_FOR_EACH(cp, rules) {
        ScmObj rule = SCM_CAR(cp), templ, patvars, pattern;
        if (Scm_Length(rule) != 2) {
            Scm_Error("malformed syntax-rules: %S", form);
        }
        patvars = Scm_Cons(SCM_NIL, SCM_NIL);
        pattern = preprocess_pattern(SCM_CAR(rule), litids, patvars);
        templ = preprocess_template(SCM_CADR(rule), SCM_CDR(patvars), litids,
                                    tmpids, env);
        SCM_APPEND1(rhead, rtail, SCM_LIST2(pattern, templ));
    }
    if (!SCM_NULLP(cp)) Scm_Error("malformed syntax-rules: %S", form);

    Scm_Printf(SCM_CUROUT, "lit=%S, rules=%S\n", litids, rhead);
    
    return SCM_LIST1(make_synrule_transformer(litids, rhead, env));
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
