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
 *  $Id: macro.c,v 1.2 2001-02-20 12:01:57 shiro Exp $
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

static ScmObj synrule_transform(ScmObj form, ScmObj env,
                                  int ctx, void *data)
{
    Scm_Error("synrule-tranformer: not implemented yet.  Sorry.");
    return SCM_NIL;
}

static ScmObj make_synrule_transformer(ScmObj literals, ScmObj rules)
{
    return Scm_MakeSyntax(SCM_SYMBOL(SCM_INTERN("macro")), /* TODO: need better info */
                          synrule_transform,
                          (void*)Scm_Cons(literals, rules));
}

static ScmObj compile_syntax_rules(ScmObj form, ScmObj env,
                                   int ctx, void *data)
{
    ScmObj literals, rules, cp;
    int badlit = 0;
    
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

    return SCM_LIST1(make_synrule_transformer(literals, rules));
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
