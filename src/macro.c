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
 *  $Id: macro.c,v 1.3 2001-02-21 13:33:19 shiro Exp $
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
    
    sym = Scm_CompileLookupEnv(SCM_CAR(expr), env);
    /* TODO: case of locally bound macros */
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

/*-------------------------------------------------------------------
 * pattern language matcher
 */

#if 0
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

#define ELLIPSIS_FOLLOWING(pat) \
    (SCM_PAIRP(SCM_CDR(pat)) && SCM_CADR(pat)==SCM_SYM_ELLIPSIS)

/* See if form matches pattern.  If match, add matched syntax variable
   bindings to matchlist and returns modified matchlist. */
static ScmObj match_synrule(ScmObj form, ScmObj pattern,
                            ScmObj literals, ScmObj env,
                            ScmObj matchlist)
{
    ScmObj r;

    Scm_Printf(SCM_CUROUT, "--- %S %S %S\n", form, pattern, matchlist);
    if (SCM_SYMBOLP(pattern)) {
        if (SCM_FALSEP(Scm_Member(pattern, literals))) {
            return match_insert(pattern, form, matchlist);
        } else {
            
        }
    }
    if (SCM_PAIRP(pattern)) {
        while (SCM_PAIRP(pattern)) {
            if (ELLIPSIS_FOLLOWING(pattern)) {
                while (SCM_PAIRP(form)) {
                    r = match_synrule(SCM_CAR(form), SCM_CAR(pattern), matchlist);
                    if (SCM_FALSEP(r)) return SCM_FALSE;
                    form = SCM_CDR(form);
                    matchlist = r;
                }
                if (!SCM_NULLP(form)) return SCM_FALSE;
                else return matchlist;
            } else if (!SCM_PAIRP(form)) {
                return SCM_FALSE;
            } else {
                r = match_synrule(SCM_CAR(form), SCM_CAR(pattern), matchlist);
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
                              matchlist);
            if (SCM_FALSEP(r)) return SCM_FALSE;
            matchlist = r;
        }
        if (!elli) return matchlist;
        for (i=plen-elli; i<flen; i++) {
            r = match_synrule(SCM_VECTOR_ELEMENT(form, i),
                              SCM_VECTOR_ELEMENT(pattern, plen-2),
                              matchlist);
            if (SCM_FALSEP(r)) return SCM_FALSE;
            matchlist = r;
        }
        return matchlist;
    }

    /* literal */
    if (Scm_EqualP(pattern, form)) return matchlist;
    else return SCM_FALSE;
}
#endif
/*-------------------------------------------------------------------
 * pattern language transformer
 */
static ScmObj synrule_transform(ScmObj form, ScmObj env,
                                int ctx, void *data)
{
#if 0
    ScmObj cp;
    ScmObj literals = SCM_CAR(data);
    ScmObj rules = SCM_CADR(data);
    ScmObj cmpl_env = SCM_CAR(SCM_CDDR(data));
    
    Scm_Printf(SCM_CUROUT, "**** synrule_transform: %S\n", form);
    SCM_FOR_EACH(cp, rules) {
        ScmObj r = match_synrule(form, SCM_CAAR(cp), SCM_NIL);
        Scm_Printf(SCM_CUROUT, "  %S => %S\n", SCM_CAAR(cp), r);
    }
#endif
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
    ScmObj literals, rules, cp;
    
    if (Scm_Length(form) < 3)
        Scm_Error("malformed syntax-rules: %S", form);
    literals = SCM_CADR(form);
    rules = SCM_CDDR(form);

    SCM_FOR_EACH(cp, literals) {
        if (!SCM_SYMBOLP(SCM_CAR(cp))) break;
    }
    if (!SCM_NULLP(cp)) {
        Scm_Error("bad literal list in syntax-rules: %S", literals);
    }

    SCM_FOR_EACH(cp, rules) {
        if (Scm_Length(SCM_CAR(cp)) != 2) {
            Scm_Error("malformed syntax-rules: %S", form);
        }
    }

    return SCM_LIST1(make_synrule_transformer(literals, rules, env));
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
