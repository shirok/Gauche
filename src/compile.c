/*
 * compile.c - compile the given form to an intermediate form
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
 *  $Id: compile.c,v 1.16 2001-02-03 10:42:11 shiro Exp $
 */

#include "gauche.h"

/*
 * Syntax and macro objects
 */

static int syntax_print(ScmObj obj, ScmPort *port, int mode)
{
    return Scm_Printf(port, "#<syntax %A>", SCM_SYNTAX(obj)->name);
}

static ScmClass *top_cpl[] = { SCM_CLASS_TOP, NULL };

ScmClass Scm_SyntaxClass = {
    SCM_CLASS_CLASS,
    "<syntax>",
    syntax_print,
    top_cpl
};

ScmObj Scm_MakeSyntax(ScmSymbol *name, ScmCompileProc compiler, void *data)
{
    ScmSyntax *s = SCM_NEW(ScmSyntax);
    s->hdr.klass = SCM_CLASS_SYNTAX;
    s->name = name;
    s->compiler = compiler;
    s->data = data;
    return SCM_OBJ(s);
}

/*
 * SourceInfo object
 */

static int source_info_print(ScmObj obj, ScmPort *port, int mode)
{
    return Scm_Printf(port, "#<source-info %S>",
                      SCM_SOURCE_INFO(obj)->info);
}

ScmClass Scm_SourceInfoClass = {
    SCM_CLASS_CLASS,
    "<source-info>",
    source_info_print,
    top_cpl
};

ScmObj Scm_MakeSourceInfo(ScmObj info, ScmSourceInfo *up)
{
    ScmSourceInfo *i = SCM_NEW(ScmSourceInfo);
    i->hdr.klass = SCM_CLASS_SOURCE_INFO;
    i->info = info;
    i->up = up;
    return SCM_OBJ(i);
}

/* Conventions of internal functions
 *
 *  - ctx parameter takes one of SCM_COMPILE_STMT, SCM_COMPILE_TAIL,
 *    SCM_COMPILE_NORMAL
 *
 *  - compile_* function always returns a list, which may be destructively
 *    concatenated later.
 */

static ScmObj lookup_env(ScmObj symbol, ScmObj env);
static ScmObj compile_varref(ScmObj form, ScmObj env);
static ScmObj compile_int(ScmObj form, ScmObj env, int ctx);
static ScmObj compile_lambda_family(ScmObj form, ScmObj args, ScmObj body,
                                    ScmObj env, int ctx);

#define FREE_VAR_P(sym, env)  SCM_SYMBOLP(lookup_env(sym, env))

#define ADDCODE1(c)   SCM_APPEND1(code, codetail, c)
#define ADDCODE(c)    SCM_APPEND(code, codetail, c)

/* type of let-family bindings */
enum {
    BIND_LET,
    BIND_LET_STAR,
    BIND_LETREC
};

/*================================================================
 *
 * Compile toplevel form
 *
 *   Statically analyzes given form recursively, converting it
 *   to the intermediate form.   Syntactic error is detected here.
 *
 *   TODO: macro expansion
 */

/* Semantics of global (free) reference:
 *
 *   We compile each toplevel form one-by-one.  Since the free
 *   reference binding can be inserted into this module after the
 *   current toplevel form, we can't resolve binding at the compilation time.
 *   (If we had only one module, we could insert bindings whenever
 *   we saw free variables; in our hierarchical module system, we can't
 *   do that.)
 *
 *   So we just put a [GREF symbol] or [SET symbol] in the compiled code.
 *   At runtime, VM looks for the binding in the given module and replaces
 *   the symbol to the GLOC object.  Afterwards, the global reference will
 *   be faster.
 *
 *   This "memorization" creates a small hazard in the interative
 *   environment, however.   Suppose module A defines a variable x
 *   and module B inherits it:
 *
 *      (with-module A  (export x) (define x 1))
 *      (with-module B  (import A) (define (y v) (set! x v)))
 *
 *   The binding of x in the procedure y is resolved when y is called
 *   for the first time, and then memorized.  Thus, these two code
 *   will behave differently:  (suppose they're evaluated in B)
 *
 *    Code 1:
 *      (y 3)         ; x in y is bound to A::x
 *      (define x #f) ; symbol x is inserted in B, i.e. B::x
 *      (y 2)         ; still A::x is modified.
 *      x  ==> yields #f
 *
 *    Code 2:
 *      (define x #f) ; symbol x is inserted in B, i.e. B::x
 *      (y 3)         ; x in y is bound to B::x now.
 *      (y 2)         ; B::x is modified.
 *      x  ==> yields 2
 *
 *   One way to address this problem is to prohibit the assignment to
 *   the variable from outside the module, described in Tung: "Interative
 *   Modular Programming in Scheme", Proc. of the conf. on Lisp and
 *   functional programming, 1992, pp.86-95.  Unfortunately, such
 *   restriction may conflict with the tradition of exporting some
 *   global variables for module users to customize the behavior of
 *   the module, although such tradition should be avoided.   In Gauche,
 *   I accept the hazard for now; maybe I'll introduce some run-time
 *   flag to switch the behavior.
 *
 *   For the macros and built-in syntaxes, the story is different.  We
 *   need them in the compilation time, so we just look up the bindings
 *   at the compile time.  If the macro is not defined then, it is not
 *   valid.   This semantics is compatible for other implementations,
 *   I think.
 */

ScmObj Scm_Compile(ScmObj form, ScmObj env, int context)
{
    return compile_int(form, env, context);
}

static ScmObj compile_int(ScmObj form, ScmObj env, int ctx)
{
    ScmObj code = SCM_NIL, codetail;
    ScmVM *vm = Scm_VM();

    if (SCM_PAIRP(form)) {
        /* we have a pair.  This is either a special form
           or a function call */
        ScmObj head = SCM_CAR(form);
        
        if (SCM_SYMBOLP(head)) {
            head = lookup_env(head, env);
            if (SCM_SYMBOLP(head)) {
                /* Let's see if the symbol is bound to a syntax, a macro,
                   or an inlinable procedure in the current module. */
                ScmGloc *g = Scm_FindBinding(vm->module, SCM_SYMBOL(head), 0);
                if (g != NULL) {
                    if (SCM_SYNTAXP(g->value)) {
                        ScmCompileProc cmpl = SCM_SYNTAX(g->value)->compiler;
                        void *data = SCM_SYNTAX(g->value)->data;
                        return cmpl(form, env, ctx, data);
                    }
                    if (vm->enableInline &&
                        SCM_SUBRP(g->value) && SCM_SUBR_INLINER(g->value)) {
                        ScmObj inlined =
                            SCM_SUBR_INLINER(g->value)(SCM_SUBR(g->value),
                                                       form, env, ctx);
                        if (!SCM_FALSEP(inlined)) return inlined;
                    }
                }
                
                /* Symbol doesn't have syntactic bindings.  It must be
                   a global procedure call. */
                head = SCM_LIST2(SCM_VM_INSN(SCM_VM_GREF), head);
            } else {
                head = SCM_LIST1(head);
            }
        } else {
            head = compile_int(head, env, SCM_COMPILE_NORMAL);
        }
        /* here, we have general application */
        {
            ScmObj ap;
            int nargs = 0;
            
            SCM_FOR_EACH(ap, SCM_CDR(form)) {
                ScmObj arg = compile_int(SCM_CAR(ap), env, SCM_COMPILE_NORMAL);
                ADDCODE(arg);
                ADDCODE1(SCM_VM_INSN(SCM_VM_PUSH));
                nargs++;
            }

            ADDCODE(head);
            ADDCODE1(((ctx == SCM_COMPILE_TAIL)?
                      SCM_VM_INSN1(SCM_VM_TAIL_CALL, nargs) :
                      SCM_VM_INSN1(SCM_VM_CALL, nargs)));
            ADDCODE1(Scm_MakeSourceInfo(form, NULL));

            if (ctx == SCM_COMPILE_TAIL) {
                code = Scm_Cons(SCM_VM_INSN(SCM_VM_PRE_TAIL), code);
            } else {
                code = SCM_LIST2(SCM_VM_INSN(SCM_VM_PRE_CALL), code);
            }
            return code;
        }
    }
    if (SCM_SYMBOLP(form)) {
        /* variable reference.  even in the statement context we evaluate
           the variable, for it may raise an error. */
        ADDCODE(compile_varref(form, env));
        return code;
    }
    else {
        /* literal object.  if it appears in the statement context,
           we don't bother to include it. */
        if (ctx == SCM_COMPILE_STMT) return SCM_NIL;
        else return SCM_LIST1(form);
    }
}

static ScmObj lookup_env(ScmObj symbol, ScmObj env)
{
    int depth = 0;
    int offset = 0;
    ScmObj frame, var;
    
    SCM_FOR_EACH(frame, env) {
        SCM_FOR_EACH(var, SCM_CAR(frame)) {
            if (SCM_CAR(var) == symbol)
                return SCM_VM_INSN2(SCM_VM_LREF, depth, offset);
            offset++;
        }
        depth++;
        offset = 0;
    }
    return symbol;              /* global ref */
}

static ScmObj compile_varref(ScmObj sym, ScmObj env)
{
    ScmObj v = lookup_env(sym, env);
    ScmObj code = SCM_NIL, codetail;
    if (SCM_SYMBOLP(v)) {
        ADDCODE1(SCM_VM_INSN(SCM_VM_GREF));
    }
    ADDCODE1(v);
    ADDCODE1(Scm_MakeSourceInfo(sym, NULL));
    return code;
}

static int check_valid_lambda_args(ScmObj args)
{
    /* TODO: check them! */
    return 1;
}

/*==================================================================
 * Built-in syntax
 */

/*------------------------------------------------------------------
 * DEFINE (toplevel define)
 *   This should never called for internal defines (they are handled by
 *   compile_body).
 */
static ScmObj compile_define(ScmObj form,
                             ScmObj env,
                             int ctx,
                             void *data)
{
    ScmObj var, val, tail = SCM_CDR(form), code = SCM_NIL, codetail;
    if (!SCM_PAIRP(tail)) Scm_Error("syntax error: %S", form);
    var = SCM_CAR(tail);

    if (SCM_PAIRP(var)) {
        /* (define (f args ...) body ...) */
        if (!SCM_SYMBOLP(SCM_CAR(var)))
            Scm_Error("syntax error: %S", form);
        val = compile_lambda_family(form, SCM_CDR(var), SCM_CDR(tail),
                                    env, SCM_COMPILE_NORMAL);
        var = SCM_CAR(var);
    } else {
        if (!SCM_PAIRP(SCM_CDR(tail)) || !SCM_NULLP(SCM_CDR(SCM_CDR(tail))))
            Scm_Error("syntax error: %S", form);
        val = compile_int(SCM_CADR(tail), env, SCM_COMPILE_NORMAL);
    }

    ADDCODE(val);
    ADDCODE1(SCM_VM_INSN(SCM_VM_DEFINE));
    ADDCODE1(var);
    return code;
}

static ScmSyntax syntax_define = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_DEFINE),
    compile_define,
    NULL
};

/*------------------------------------------------------------------
 * QUOTE-family (QUOTE, QUASIQUOTE, UNQUOTE and UNQUOTE-SPLICING)
 */
static ScmObj compile_quote(ScmObj form,
                            ScmObj env, 
                            int ctx,
                            void *data)
{
    ScmObj tail = SCM_CDR(form);
    if (!SCM_PAIRP(tail) || !SCM_NULLP(SCM_CDR(tail)))
        Scm_Error("syntax error: %S", form);
    if (ctx == SCM_COMPILE_STMT) return SCM_NIL;
    else return SCM_LIST1(SCM_CAR(tail));
}

static ScmSyntax syntax_quote = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_QUOTE),
    compile_quote,
    NULL
};


/*------------------------------------------------------------------
 * SET!
 */
static ScmObj compile_set(ScmObj form,
                          ScmObj env,
                          int ctx,
                          void *data)
{
    ScmObj tail = SCM_CDR(form);
    ScmObj location, expr;
    ScmObj code = SCM_NIL, codetail;

    if (!SCM_PAIRP(tail) || !SCM_PAIRP(SCM_CDR(tail))
        || !SCM_NULLP(SCM_CDDR(tail))) {
        Scm_Error("syntax error: %S", form);
    }
    location = SCM_CAR(tail);
    expr = SCM_CADR(tail);

    /* TODO: support generalized set! */
    if (SCM_PAIRP(location)) {
        Scm_Error("generalized set! not supported (yet): %S", form);
    }
    if (!SCM_SYMBOLP(location)) {
        Scm_Error("syntax error: %S", form);
    }

    code = compile_int(expr, env, SCM_COMPILE_NORMAL);
    codetail = Scm_LastPair(code);
    ADDCODE1(SCM_VM_INSN(SCM_VM_SET));
    ADDCODE1(lookup_env(location, env));
    return code;
}

static ScmSyntax syntax_set = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_SET),
    compile_set,
    NULL
};


/*------------------------------------------------------------------
 * LAMBDA
 */

/* Common routine for lambda, let-family and begin, to compile its body.
 * Assumes FORM is a proper list.
 */
static ScmObj compile_body(ScmObj form,
                           ScmObj env,
                           int ctx)
{
    ScmObj body = SCM_NIL, bodytail, formtail;
    ScmObj idef_vars = SCM_NIL, idef_vars_tail;
    ScmObj idef_vals = SCM_NIL, idef_vals_tail;
    int idefs = 0, body_started = 0;

    SCM_FOR_EACH(formtail, form) {
        ScmObj expr = SCM_CAR(formtail), x;

        /* Check for internal define. */
        if (SCM_PAIRP(expr) && SCM_CAR(expr) == SCM_SYM_DEFINE) {
            ScmObj var, val;
            int llen;

            if (FREE_VAR_P(SCM_SYM_DEFINE, env)) {
                if (body_started)
                    Scm_Error("internal define should appear at the head of teh body: %S",
                              expr);
                if ((llen = Scm_Length(expr)) < 3)
                    Scm_Error("badly formed internal define: %S", expr);
                var = SCM_CADR(expr);
                if (SCM_PAIRP(var)) {
                    ScmObj args = SCM_CDR(var);
                    if (!check_valid_lambda_args(args))
                        Scm_Error("badly formed internal define: %S", expr);
                    var = SCM_CAR(var);
                    /* TODO: this doens't work if `lambda' is locally bound.
                       Need to use compile_lambda_family, but we can't
                       use it until we have the new environment. */
                    val = Scm_Cons(SCM_SYM_LAMBDA,
                                   Scm_Cons(args, SCM_CDDR(expr)));
                } else {
                    if (llen != 3)
                        Scm_Error("badly formed internal define: %S", expr);
                    val = SCM_CAR(SCM_CDDR(expr));
                }

                SCM_APPEND1(idef_vars, idef_vars_tail, var);
                SCM_APPEND1(idef_vals, idef_vals_tail, val);
                idefs++;
                continue;
            }
        } else if (!body_started && idefs > 0) {
            /* starting the `real' body */
            int cnt;
            
            env = Scm_Cons(idef_vars, env);
            SCM_APPEND1(body, bodytail, SCM_VM_INSN1(SCM_VM_LET, idefs));
            SCM_APPEND1(body, bodytail, form);
            for (cnt=0; cnt<idefs; cnt++) {
                ScmObj loc = compile_varref(SCM_CAR(idef_vars), env);
                SCM_APPEND(body, bodytail,
                           compile_int(SCM_CAR(idef_vals), env, SCM_COMPILE_NORMAL));
                SCM_APPEND1(body, bodytail, SCM_VM_INSN(SCM_VM_SET));
                SCM_APPEND(body, bodytail, loc);
                idef_vars = SCM_CDR(idef_vars);
                idef_vals = SCM_CDR(idef_vals);
            }
        }
        body_started = 1;

        if (SCM_NULLP(SCM_CDR(formtail))) {
            /* tail call */
            x = compile_int(expr, env, ctx);
        } else {
            x = compile_int(expr, env, SCM_COMPILE_STMT);
        }
        SCM_APPEND(body, bodytail, x);
    }
    
    if (idefs > 0 && body_started) {
        SCM_APPEND1(body, bodytail, SCM_VM_INSN(SCM_VM_POPENV));
    }
    return body;
}

/* Common routine to compile lambda.
 */
static ScmObj compile_lambda_family(ScmObj form, ScmObj args, ScmObj body,
                                    ScmObj env, int ctx)
{
    ScmObj newenv, bodycode, code = SCM_NIL, codetail;
    int nargs, restarg;
    
    if (!check_valid_lambda_args(args))
        Scm_Error("syntax error: %S", form);
    if (Scm_Length(body) <= 0)
        Scm_Error("badly formed body: %S", form);

    /* extend environment */
    {
        ScmObj a, e = SCM_NIL, t;
        nargs = 0;
        restarg = 0;
        
        SCM_FOR_EACH(a, args) {
            SCM_APPEND1(e, t, SCM_CAR(a));
            nargs++;
        }
        if (!SCM_NULLP(a)) {
            SCM_APPEND1(e, t, a);
            restarg++;
        }
        newenv = Scm_Cons(e, env);
    }

    bodycode = compile_body(body, newenv, SCM_COMPILE_TAIL);
    SCM_APPEND(code, codetail, 
               SCM_LIST3(SCM_VM_INSN2(SCM_VM_LAMBDA, nargs,restarg),
                         form, bodycode));
    return code;
}

/* TODO: possible optimization: if arglist is null (i.e. form is thunk),
 * we don't need to push environment frame, sacrificing debugging capability.
 * Need to cooperate with VM.
 */
static ScmObj compile_lambda(ScmObj form,
                             ScmObj env,
                             int ctx,
                             void *data)
{
    ScmObj tail = SCM_CDR(form);
    ScmObj args, body;
    int nargs, restarg;
    
    if (!SCM_PAIRP(tail) || !SCM_PAIRP(SCM_CDR(tail)))
        Scm_Error("syntax error: %S", form);
    args = SCM_CAR(tail);
    body = SCM_CDR(tail);

    return compile_lambda_family(form, args, body, env, ctx);
}

static ScmSyntax syntax_lambda = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_LAMBDA),
    compile_lambda,
    NULL
};

/*------------------------------------------------------------------
 * BEGIN
 */
static ScmObj compile_begin(ScmObj form,
                            ScmObj env,
                            int ctx,
                            void *data)
{
    /* TODO: distinguish toplevel begin */
    return compile_body(SCM_CDR(form), env, ctx);
}

static ScmSyntax syntax_begin = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_BEGIN),
    compile_begin,
    NULL
};

/*------------------------------------------------------------------
 * IF family (IF, WHEN, UNLESS, AND, OR, COND)
 */

/* Common part for compiling if-family.  TEST_CODE is a form or a
   compiled code of the test part (if it is a compiled code, the
   first element must be a vm insn.).
   THEN_CODE and ELSE_CODE must be a compiled code for then clause
   and else clause, respectively.
   INSN is a VM instruction to be emitted, which is one of 
   SCM_VM_IF, SCM_VM_AND or SCM_VM_OR. */

static ScmObj compile_if_family(ScmObj test_code, ScmObj then_code,
                                ScmObj else_code, int insn,
                                int mergep, ScmObj env)
{
    ScmObj code = SCM_NIL, codetail;

    if (!SCM_PAIRP(test_code) || !SCM_VM_INSNP(SCM_CAR(test_code))) {
        test_code = compile_int(test_code, env, SCM_COMPILE_NORMAL);
    }
    if (mergep) {
        /* make two instruction stream merges */
        ScmObj next_cell = SCM_LIST1(SCM_VM_INSN(SCM_VM_NOP));
        then_code = Scm_Append2X(then_code, next_cell);
        else_code = Scm_Append2X(else_code, next_cell);
    }
    
    ADDCODE(test_code);
    ADDCODE1(SCM_VM_INSN(insn));
    ADDCODE1(then_code);
    ADDCODE(else_code);
    return code;
}

static ScmObj compile_if(ScmObj form, ScmObj env, int ctx, void *data)
{
    ScmObj tail = SCM_CDR(form), then_clause, else_clause;
    int nargs = Scm_Length(tail);
    
    if (nargs < 2 || nargs > 3) Scm_Error("syntax error: %S", form);
    then_clause = SCM_CADR(tail);
    if (nargs == 3) {
        else_clause = SCM_CAR(SCM_CDDR(tail));
    } else {
        else_clause = SCM_UNDEFINED;
    }
    return compile_if_family(SCM_CAR(tail),
                             compile_int(then_clause, env, ctx),
                             compile_int(else_clause, env, ctx),
                             SCM_VM_IF, 1, env);
}

static ScmSyntax syntax_if = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_IF),
    compile_if,
    NULL
};

static ScmObj compile_when(ScmObj form, ScmObj env, int ctx, void *data)
{
    ScmObj tail = SCM_CDR(form), body, then_code, else_code;
    int unlessp = (int)data;
    int nargs = Scm_Length(tail);
    if (nargs < 2) Scm_Error("syntax error: %S", form);
    body = SCM_CDR(tail);
    then_code = compile_body(body, env, ctx);
    else_code = (ctx == 0)? SCM_NIL : SCM_LIST1(SCM_UNDEFINED);

    if (unlessp) {
        /* for UNLESS, we just swap then and else clause. */
        ScmObj t = then_code; then_code = else_code; else_code = t;
    }

    return compile_if_family(SCM_CAR(tail),
                             then_code, else_code,
                             SCM_VM_IF, 1, env);
}

static ScmSyntax syntax_when = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_WHEN),
    compile_when,
    (void*)0
};

static ScmSyntax syntax_unless = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_UNLESS),
    compile_when,
    (void*)1
};

static ScmObj compile_and_rec(ScmObj conds, ScmObj merger, int orp,
                              ScmObj env, int ctx)
{
    if (!SCM_PAIRP(SCM_CDR(conds))) {
        ScmObj last_test = compile_int(SCM_CAR(conds), env, ctx);
        return Scm_Append2X(last_test, merger);
    } else {
        ScmObj more_test =
            compile_and_rec(SCM_CDR(conds), merger, orp, env, ctx);
        ScmObj no_more_test = merger;
        return compile_if_family(SCM_CAR(conds),
                                 orp? no_more_test : more_test,
                                 orp? more_test : no_more_test,
                                 SCM_VM_IF, 0, env);
    }
}

static ScmObj compile_and(ScmObj form, ScmObj env, int ctx, void *data)
{
    ScmObj tail = SCM_CDR(form);
    int orp = (int)data;
    
    if (!SCM_PAIRP(tail)) {
        /* (and) or (or) is compiled into a literal boolean, or
           even a null if at the statement context. */
        if (ctx == 0) return SCM_NIL;
        else return orp ? SCM_LIST1(SCM_FALSE) : SCM_LIST1(SCM_TRUE);
    } else {
        ScmObj merger = SCM_LIST1(SCM_VM_INSN(SCM_VM_NOP));
        return compile_and_rec(tail, merger, orp, env, ctx);
    }
}

static ScmSyntax syntax_and = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_AND),
    compile_and,
    (void*)0
};

static ScmSyntax syntax_or = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_OR),
    compile_and,
    (void*)1
};

/* Common part of compiling cond/case.
 *   CLAUSES - list of clauses
 *   MERGER - compiled code stream to where all the control emerge.
 *   CASEP - 1 if we're compiling case, 0 for cond.
 */
static ScmObj compile_cond_int(ScmObj form, ScmObj clauses, ScmObj merger,
                               ScmObj env, int ctx, int casep)
{
    ScmObj clause, test, body;
    ScmObj code = SCM_NIL, codetail;
    ScmObj altcode = SCM_NIL, altcodetail;
    int clen;

    if (SCM_NULLP(clauses)) {
        /* If caller expects a result, let it have undefined value. */
        if (ctx != 0) SCM_APPEND1(code, codetail, SCM_UNDEFINED);
        /* merge control */
        SCM_APPEND(code, codetail, merger);
        return code;
    }
    if (!SCM_PAIRP(clauses)) Scm_Error("syntax error: %S", form);
    
    clause = SCM_CAR(clauses);
    clen = Scm_Length(clause);
    if (clen <= 0+casep) Scm_Error("invalid clause in the form: %S", form);
    test = SCM_CAR(clause);
    body = SCM_CDR(clause);

    /* Check for `else' clause. */
    if (test == SCM_SYM_ELSE) {
        if (!SCM_NULLP(SCM_CDR(clauses))) {
            Scm_Error("extra clause appears after 'else' clause: %S",
                      form);
        }
        if (!SCM_PAIRP(body)) {
            Scm_Error("empty `else' clause is not allowed: %S", form);
        }
        ADDCODE(compile_body(body, env, ctx));
        ADDCODE(merger);
        return code;
    }

    /* Let's compile the clause. */
    if (!casep && clen >= 2 && SCM_CAR(body) == SCM_SYM_YIELDS) {
        /* `=>' */
        ScmObj xcode = SCM_NIL, xtail;
        
        if (clen != 3) {
            Scm_Error("badly formed '=>' clause in the form: %S", form);
        }
        
        SCM_APPEND1(xcode, xtail, SCM_VM_INSN(SCM_VM_PUSH));
        SCM_APPEND(xcode, xtail,
                   compile_int(SCM_CADR(body), env, SCM_COMPILE_NORMAL));
        if (ctx == SCM_COMPILE_TAIL) {
            SCM_APPEND1(xcode, xtail, SCM_VM_INSN1(SCM_VM_TAIL_CALL, 1));
            SCM_APPEND(xcode, xtail, merger);
            ADDCODE(Scm_Cons(SCM_VM_INSN(SCM_VM_PRE_TAIL), xcode));
        } else {
            SCM_APPEND1(xcode, xtail, SCM_VM_INSN1(SCM_VM_CALL, 1));
            ADDCODE1(SCM_VM_INSN(SCM_VM_PRE_CALL));
            ADDCODE1(xcode);
            ADDCODE(merger);
        }
    } else if (clen == 1) {
        /* This only applies for cond forms.
           We can leave the test on the stack, if this form needs
           the result.  If this is in a statement context, however,
           we need to pop the test result. */
        ADDCODE(merger);
    } else {
        /* Normal case */
        ADDCODE(compile_body(body, env, ctx));
        ADDCODE(merger);
    }

    /* Rest of clauses.   We have the result of test
       on the stack when the rest of clauses are called, so
       we need to pop it first. */
    SCM_APPEND(altcode, altcodetail,
               compile_cond_int(form, SCM_CDR(clauses),
                                merger, env, ctx, casep));

    /* Emit test code for `case' form.  The value of the key is already
       on top of the stack. */
    if (casep) {
        ScmObj testcode = SCM_NIL, testtail;
        int testlen = Scm_Length(test);
        if (testlen < 0)
            Scm_Error("badly formed clause in case form: %S", clause);
        SCM_APPEND1(testcode, testtail, SCM_VM_INSN(SCM_VM_PUSH));
        SCM_APPEND1(testcode, testtail, test);
        SCM_APPEND1(testcode, testtail, SCM_VM_INSN(SCM_VM_MEMV));
        test = testcode;
    }
    
    return compile_if_family(test, code, altcode, SCM_VM_IF, 0, env);
}


static ScmObj compile_cond(ScmObj form, ScmObj env, int ctx, void *data)
{
    ScmObj clauses = SCM_CDR(form), merger;
    if (SCM_NULLP(clauses)) {
        Scm_Error("at least one clause is required for cond: %S", form);
    }
    merger = SCM_LIST1(SCM_VM_INSN(SCM_VM_NOP));
    return compile_cond_int(form, clauses, merger, env, ctx, 0);
}

static ScmSyntax syntax_cond = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_COND),
    compile_cond,
    NULL
};

static ScmObj compile_case(ScmObj form, ScmObj env, int ctx, void *data)
{
    ScmObj tail = SCM_CDR(form), key, clauses, merger;
    ScmObj code = SCM_NIL, codetail;
    int nlen = Scm_Length(tail);
    if (nlen < 3) Scm_Error("bad case form: %S", form);
    key = SCM_CAR(tail);
    clauses = SCM_CDR(tail);

    /* First, push the value of the key on the stack */
    ADDCODE(compile_int(key, env, SCM_COMPILE_NORMAL));

    merger = SCM_LIST1(SCM_VM_INSN(SCM_VM_NOP));
    ADDCODE(compile_cond_int(form, clauses, merger, env, ctx, 1));
    return code;
}

static ScmSyntax syntax_case = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_CASE),
    compile_case,
    NULL
};

/*------------------------------------------------------------------
 * LET family (LET, LET*, LETREC)
 */
static const char *let_name(int type)
{
    const char *p = "";
    
    switch (type) {
    case BIND_LET: p = "let"; break;
    case BIND_LET_STAR: p = "let*"; break;
    case BIND_LETREC: p = "letrec"; break;
    }
    return p;
}

/* Common routine to compile binding construct.   The compilation of
   body part is delegated to BODY_COMPILER function */
static ScmObj compile_let_family(ScmObj form, ScmObj vars, ScmObj vals,
                                 int nvars, int type, ScmObj body,
                                 ScmObj (*body_compiler)(ScmObj body,
                                                         ScmObj env,
                                                         int ctx),
                                 ScmObj env, int ctx)
{
    ScmObj code = SCM_NIL, codetail;
    ScmObj cfr = SCM_NIL, cfrtail;  /* current frame */
    ScmObj newenv, varp, valp;
    int count = 0;
    ADDCODE1(SCM_VM_INSN1(SCM_VM_LET, nvars));
    ADDCODE1(form);             /* debug info */

    if (type == BIND_LETREC) cfr = vars;
    else                     cfr = SCM_NIL;
    newenv = Scm_Cons(cfr, env);
    
    for (count=0, varp=vars, valp=vals;
         count<nvars;
         count++, varp=SCM_CDR(varp), valp=SCM_CDR(valp)) {
        ScmObj val = compile_int(SCM_CAR(valp), newenv, SCM_COMPILE_NORMAL);
        ADDCODE(val);
        ADDCODE1(SCM_VM_INSN(SCM_VM_SET));
        ADDCODE1(SCM_VM_INSN2(SCM_VM_LREF, 0, count));
            
        if (type == BIND_LET_STAR) {
            SCM_APPEND1(cfr, cfrtail, SCM_CAR(varp));
            newenv = Scm_Cons(cfr, env);
        }
    }
    
    if (type == BIND_LET) newenv = Scm_Cons(vars, env);
/*    SCM_APPEND(code, codetail, body_compiler(body, newenv, ctx));*/
    ADDCODE(body_compiler(body, newenv, SCM_COMPILE_NORMAL));
    ADDCODE1(SCM_VM_INSN(SCM_VM_POPENV));
    return code;
}

static ScmObj compile_let(ScmObj form,
                          ScmObj env,
                          int ctx,
                          void *data)
{
    int type = (int)data;
    ScmObj tail = SCM_CDR(form);
    ScmObj bindings, body, vars, vals, name = SCM_FALSE;
    ScmObj newenv, code = SCM_NIL, codetail, bodycode;
    int nvars;

    if (!SCM_PAIRP(tail))
        Scm_Error("syntax error: %S", form);
    bindings = SCM_CAR(tail);
    body = SCM_CDR(tail);

    /* Check named let */
    if (SCM_SYMBOLP(bindings)) {
        if (type != BIND_LET) Scm_Error("syntax error: %S", form);
        if (!SCM_PAIRP(body)) Scm_Error("badly formed named let: %S", form);
        name = bindings;
        bindings = SCM_CAR(body);
        body = SCM_CDR(body);
    }

    /* Check binding syntax */
    {
        ScmObj vars_p, vals_p, bind_p;

        vars = SCM_NIL;
        vals = SCM_NIL;
        nvars = 0;
        SCM_FOR_EACH(bind_p, bindings) {
            ScmObj binding = SCM_CAR(bind_p);

            if (!SCM_PAIRP(binding)
                || !SCM_PAIRP(SCM_CDR(binding))
                || !SCM_NULLP(SCM_CDR(SCM_CDR(binding)))
                || !SCM_SYMBOLP(SCM_CAR(binding))) {
                Scm_Error("syntax error (invalid binding form): %S", form);
            }
            /* TODO: check duplicate binding */
            SCM_APPEND1(vars, vars_p, SCM_CAR(binding));
            SCM_APPEND1(vals, vals_p, SCM_CADR(binding));
            nvars++;
        }
        if (!SCM_NULLP(bind_p))
            Scm_Error("syntax error (invalid binding form): %S", form);
    }

    if (SCM_FALSEP(name)) {
        return compile_let_family(form, vars, vals, nvars, type,
                                  body, compile_body,
                                  env, ctx);
    } else {
        /* Named let. */
        static ScmObj compile_named_let_body(ScmObj, ScmObj, int);
        /* TODO: this is broken if lambda is locally bound! */
        ScmObj proc = Scm_Cons(SCM_SYM_LAMBDA, Scm_Cons(vars, body));
        return compile_let_family(form, SCM_LIST1(name), SCM_LIST1(proc),
                                  1, BIND_LETREC,
                                  Scm_Cons(env, Scm_Cons(name, vals)),
                                  compile_named_let_body, env, ctx);
    }
}

static ScmObj compile_named_let_body(ScmObj body, ScmObj env, int ctx)
{
    /* Trick: we need to compile initial values in the upper environment,
       while the "name" to be looked up in the new environment. */
    ScmObj oldenv = SCM_CAR(body);
    ScmObj name = SCM_CADR(body);
    ScmObj args = SCM_CDR(SCM_CDR(body));
    name = lookup_env(name, env);
    return compile_body(SCM_LIST1(Scm_Cons(name, args)),
                        Scm_Cons(SCM_NIL, oldenv),
                        ctx);
}

static ScmSyntax syntax_let = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_LET),
    compile_let,
    (void*)BIND_LET
};

static ScmSyntax syntax_let_star = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_LET_STAR),
    compile_let,
    (void*)BIND_LET_STAR
};

static ScmSyntax syntax_letrec = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_LETREC),
    compile_let,
    (void*)BIND_LETREC
};

/*------------------------------------------------------------------
 * Loop construct (DO)
 *   Beware!  These functions create a circular list.  
 */

static ScmObj compile_do_body(ScmObj body, ScmObj env, int ctx)
{
    ScmObj test = SCM_CAR(body);
    ScmObj vars = SCM_CADR(body);
    ScmObj updts = SCM_CAR(SCM_CDDR(body)), updtsp;
    ScmObj merger = SCM_LIST1(SCM_VM_INSN(SCM_VM_NOP));

    ScmObj code = SCM_NIL, codetail;
    ScmObj fincode = SCM_NIL, fintail;
    ScmObj bodycode = SCM_NIL, bodytail;
    int varcnt;

    /* Compile body */
    body = SCM_CDR(SCM_CDR(SCM_CDR(body)));
    SCM_APPEND(bodycode, bodytail, compile_body(body, env, SCM_COMPILE_STMT));
    varcnt = 0;
    /* Compile updates.  We need to calculate all the updates first,
       discard current env, allocates new env then put the updates. */
    SCM_APPEND1(bodycode, bodytail, SCM_VM_INSN(SCM_VM_PRE_TAIL));
    SCM_FOR_EACH(updtsp, updts) {
        SCM_APPEND(bodycode, bodytail,
                   compile_int(SCM_CAR(updtsp), env, SCM_COMPILE_NORMAL));
        SCM_APPEND1(bodycode, bodytail, SCM_VM_INSN(SCM_VM_PUSH));
        varcnt++;
    }
    SCM_APPEND1(bodycode, bodytail, SCM_VM_INSN1(SCM_VM_TAILBIND, varcnt));
    SCM_APPEND1(bodycode, bodytail, SCM_NIL); /* dbg info */

    /* Compile finalization code */
    if (SCM_PAIRP(SCM_CDR(test))) {
        SCM_APPEND(fincode, fintail, compile_body(SCM_CDR(test), env, ctx));
    } else {
        if (ctx != 0) SCM_APPEND1(fincode, fintail, SCM_UNDEFINED);
    }

    /* Compile test part and branch.
       We need to negate the test so that the loop will exits through
       'else' branch.   Otherwise, 'else' branch becomes circular
       list and the rest of compilers will be confused. */
    SCM_APPEND(code, codetail, merger);
    SCM_APPEND(code, codetail,
               compile_int(SCM_CAR(test), env, SCM_COMPILE_NORMAL));
    SCM_APPEND1(code, codetail, SCM_VM_INSN(SCM_VM_NOT));
    code = compile_if_family(code, bodycode, fincode, SCM_VM_IF, 0, env);

    /* Make the list circular.   */
    SCM_APPEND(bodycode, bodytail, merger);
    return code;
}

static ScmObj compile_do(ScmObj form, ScmObj env, int ctx, void *data)
{
    ScmObj binds, test, body, bp;
    ScmObj vars = SCM_NIL, vars_tail;
    ScmObj inits = SCM_NIL, inits_tail;
    ScmObj updts = SCM_NIL, updts_tail;
    int nvars = 0;
    int flen = Scm_Length(form);
    if (flen < 3) Scm_Error("badly formed `do': %S", form);
    binds = SCM_CADR(form);
    test = SCM_CAR(SCM_CDDR(form));
    body = SCM_CDR(SCM_CDDR(form));

    if (!SCM_PAIRP(binds)) Scm_Error("badly formed `do': %S", form);
    SCM_FOR_EACH(bp, binds) {
        ScmObj bind = SCM_CAR(bp);
        int blen = Scm_Length(bind);
        if ((blen != 2) && (blen != 3))
            Scm_Error("bad binding form in `do': %S", form);
        SCM_APPEND1(vars, vars_tail, SCM_CAR(bind));
        SCM_APPEND1(inits, inits_tail, SCM_CADR(bind));
        SCM_APPEND1(updts, updts_tail,
                    (blen == 3)? SCM_CAR(SCM_CDDR(bind)) : SCM_CAR(bind));
        nvars++;
    }
    
    if (Scm_Length(test) < 1) Scm_Error("bad test form in `do': %S", form);
    return compile_let_family(form, vars, inits, nvars, BIND_LET,
                              Scm_Cons(test, Scm_Cons(vars, Scm_Cons(updts, body))),
                              compile_do_body, env, ctx);
}

static ScmSyntax syntax_do = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_DO),
    compile_do,
    NULL
};

/*------------------------------------------------------------------
 * Quasiquoter (QUASIQUOTE, UNQUOTE, UNQUOTE-SPLICING)
 */

/* TODO: improve this very naive, terribly inefficient code.
 */

#define VALID_QUOTE_SYNTAX_P(form) \
    (SCM_PAIRP(SCM_CDR(form)) && SCM_NULLP(SCM_CDDR(form)))
#define UNQUOTEP(obj, env) \
    ((obj)==SCM_SYM_UNQUOTE && FREE_VAR_P(obj, env))
#define UNQUOTE_SPLICING_P(obj, env) \
    ((obj)==SCM_SYM_UNQUOTE_SPLICING && FREE_VAR_P(obj, env))
#define QUASIQUOTEP(obj, env) \
    ((obj)==SCM_SYM_QUASIQUOTE && FREE_VAR_P(obj, env))

static ScmObj compile_qq_list(ScmObj form, ScmObj env, int level);
static ScmObj compile_qq_vec(ScmObj form, ScmObj env, int level);

static ScmObj compile_qq(ScmObj form, ScmObj env, int level)
{
    if (!SCM_PTRP(form)) {
        return SCM_LIST1(form);
    } if (SCM_PAIRP(form)) {
        return compile_qq_list(form, env, level);
    } else if (SCM_VECTORP(form)) {
        return compile_qq_vec(form, env, level);
    } else {
        return SCM_LIST1(form);
    }
}

static ScmObj compile_qq_list(ScmObj form, ScmObj env, int level)
{
    int len = 0, splice = 0;
    ScmObj car = SCM_CAR(form), cp;
    ScmObj code = SCM_NIL, codetail;

    if (UNQUOTEP(car, env)) {
        if (!VALID_QUOTE_SYNTAX_P(form))
            Scm_Error("badly formed unquote: %S\n", form);
        if (level == 0) {
            return compile_int(SCM_CADR(form), env, SCM_COMPILE_NORMAL);
        } else {
            return compile_qq(SCM_CADR(form), env, level-1);
        }
    } else if (UNQUOTE_SPLICING_P(car, env)) {
        Scm_Error("unquote-splicing appeared in invalid context: %S",
                  form);
        return SCM_NIL;     /* dummy */
    } else if (QUASIQUOTEP(car, env)) {
        if (!VALID_QUOTE_SYNTAX_P(form))
            Scm_Error("badly formed quasiquote: %S\n", form);
        return compile_qq(SCM_CADR(form), env, level+1);
    }

    /* ordinary list */
    SCM_FOR_EACH(cp, form) {
        car = SCM_CAR(cp);
        if (UNQUOTEP(car, env)) {
            break;
        } else if (UNQUOTE_SPLICING_P(car, env)) {
            Scm_Error("unquote-splicing appeared in invalid context: %S",form);
        }
        if (SCM_PAIRP(car) && UNQUOTE_SPLICING_P(SCM_CAR(car), env)) {
            if (!VALID_QUOTE_SYNTAX_P(car))
                Scm_Error("badly formed quasiquote: %S\n", form);
            ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST, len));
            ADDCODE1(SCM_VM_INSN(SCM_VM_PUSH));
            len = 0;
            ADDCODE(compile_int(SCM_CADR(car), env, SCM_COMPILE_NORMAL));
            splice+=2;
        } else {
            if (cp != form) ADDCODE1(SCM_VM_INSN(SCM_VM_PUSH));
            ADDCODE(compile_qq(SCM_CAR(cp), env, level));
            len++;
        }
    }
    if (!SCM_NULLP(cp)) {
        ADDCODE1(SCM_VM_INSN(SCM_VM_PUSH));
        ADDCODE(compile_qq(cp, env, level));
        ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST_STAR, len+1));
    } else {
        if (len == 0 && !SCM_NULLP(form)) {
            ADDCODE1(SCM_VM_INSN(SCM_VM_PUSH));
        }
        ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST, len));
    }
    if (splice) {
        ADDCODE1(SCM_VM_INSN1(SCM_VM_APPEND, splice+1));
    }
    return code;
}

static ScmObj compile_qq_vec(ScmObj form, ScmObj env, int level)
{
    ScmObj code = SCM_NIL, codetail;
    int vlen = SCM_VECTOR_SIZE(form), i, alen = 0, spliced = 0;
    for (i=0; i<vlen; i++) {
        ScmObj p = SCM_VECTOR_ELEMENT(form, i), q;
        if (SCM_PAIRP(p)) {
            ScmObj car = SCM_CAR(p);
            if (UNQUOTEP(car, env)) {
                if (!VALID_QUOTE_SYNTAX_P(p))
                    Scm_Error("badly formed unquote: %S\n", p);
                if (level == 0) {
                    if (i > 0) ADDCODE1(SCM_VM_INSN(SCM_VM_PUSH));
                    ADDCODE(compile_int(SCM_CADR(p), env, SCM_COMPILE_NORMAL));
                } else {
                    ADDCODE(compile_qq(SCM_CADR(p), env, level-1));
                }
                alen++;
            } else if (UNQUOTE_SPLICING_P(car, env)) {
                if (!VALID_QUOTE_SYNTAX_P(p))
                    Scm_Error("badly formed quasiquote: %S\n", form);
                ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST, alen));
                ADDCODE1(SCM_VM_INSN(SCM_VM_PUSH));
                alen = 0;
                ADDCODE(compile_int(SCM_CADR(p), env, SCM_COMPILE_NORMAL));
                spliced+=2;
            } else {
                if (i > 0) ADDCODE1(SCM_VM_INSN(SCM_VM_PUSH));
                ADDCODE1(p);
                alen++;
            }
        } else {
            if (i > 0) ADDCODE1(SCM_VM_INSN(SCM_VM_PUSH));
            ADDCODE1(p);
            alen++;
        }
    }

    if (spliced == 0) {
        ADDCODE1(SCM_VM_INSN1(SCM_VM_VEC, vlen));
    } else {
        if (alen) {
            ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST, alen));
            spliced++;
        }
        ADDCODE1(SCM_VM_INSN1(SCM_VM_APP_VEC, spliced));
    }
    return code;
}

static ScmObj compile_quasiquote(ScmObj form, ScmObj env, int ctx,
                                 void *data)
{
    if (!VALID_QUOTE_SYNTAX_P(form))
        Scm_Error("badly formed quasiquote: %S\n", form);
    return compile_qq(SCM_CADR(form), env, 0);
}

static ScmObj compile_unquote(ScmObj form, ScmObj env, int ctx, void *data)
{
    const char *name = (const char *)data;
    Scm_Error("%s appeared outside corresponding quasiquote: %S", name, form);
    return SCM_NIL;             /* dummy */
}

static ScmSyntax syntax_quasiquote = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_QUASIQUOTE),
    compile_quasiquote,
    NULL
};

static ScmSyntax syntax_unquote = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_UNQUOTE),
    compile_unquote,
    "unquote"
};

static ScmSyntax syntax_unquote_splicing = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_UNQUOTE_SPLICING),
    compile_unquote,
    "unquote-splicing"
};

/*------------------------------------------------------------------
 * Delay
 */

static ScmObj compile_delay(ScmObj form, ScmObj env, int ctx, void *data)
{
    ScmObj code = SCM_NIL, codetail, info, p;
    
    if (!SCM_PAIRP(SCM_CDR(form)) || !SCM_NULLP(SCM_CDDR(form)))
        Scm_Error("bad delay form: %S", form);
    ADDCODE(compile_int(SCM_LIST3(SCM_SYM_LAMBDA,
                                  SCM_NIL,
                                  SCM_CADR(form)),
                        env, SCM_COMPILE_NORMAL));
    ADDCODE1(SCM_VM_INSN(SCM_VM_PROMISE));
    return code;
}

static ScmSyntax syntax_delay = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_DELAY),
    compile_delay,
    NULL
};

/*===================================================================
 * Initializer
 */

void Scm__InitCompiler(void)
{
    ScmModule *m = SCM_MODULE(Scm_SchemeModule());

#define DEFSYN(symbol, syntax) \
    Scm_Define(m, SCM_SYMBOL(symbol), SCM_OBJ(&syntax))
    
    DEFSYN(SCM_SYM_DEFINE,       syntax_define);
    DEFSYN(SCM_SYM_QUOTE,        syntax_quote);
    DEFSYN(SCM_SYM_QUASIQUOTE,   syntax_quasiquote);
    DEFSYN(SCM_SYM_UNQUOTE,      syntax_unquote);
    DEFSYN(SCM_SYM_UNQUOTE_SPLICING, syntax_unquote_splicing);
    DEFSYN(SCM_SYM_SET,          syntax_set);
    DEFSYN(SCM_SYM_LAMBDA,       syntax_lambda);
    DEFSYN(SCM_SYM_BEGIN,        syntax_begin);
    DEFSYN(SCM_SYM_IF,           syntax_if);
    DEFSYN(SCM_SYM_WHEN,         syntax_when);
    DEFSYN(SCM_SYM_UNLESS,       syntax_unless);
    DEFSYN(SCM_SYM_AND,          syntax_and);
    DEFSYN(SCM_SYM_OR,           syntax_or);
    DEFSYN(SCM_SYM_COND,         syntax_cond);
    DEFSYN(SCM_SYM_CASE,         syntax_case);
    DEFSYN(SCM_SYM_LET,          syntax_let);
    DEFSYN(SCM_SYM_LET_STAR,     syntax_let_star);
    DEFSYN(SCM_SYM_LETREC,       syntax_letrec);
    DEFSYN(SCM_SYM_DO,           syntax_do);
    DEFSYN(SCM_SYM_DELAY,        syntax_delay);
}
