/*
 * compile.c - compile the given form to an intermediate form
 *
 *  Copyright(C) 2000 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: compile.c,v 1.1.1.1 2001-01-11 19:26:03 shiro Exp $
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
 *  - Most internal functions takes ctx argument, which shows the context
 *    where the form is evaluated.
 *     context == 0 : statement context, i.e. the result will be discarded.
 *     context > 0  : the context indicates expected number of result values.
 *     context < 0  : this is a tail call.
 *
 *  - compile_* function always returns a list, which may be destructively
 *    concatenated later.
 */

static ScmObj lookup_env(ScmObj symbol, ScmObj env);
static ScmObj compile_varref(ScmObj form, ScmObj env);
static ScmObj compile_int(ScmObj form, ScmObj env, int ctx);

/* type of let-family bindings */
enum {
    BIND_LET,
    BIND_LET_STAR,
    BIND_LETREC
};

/*
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

ScmObj Scm_Compile(ScmObj form)
{
    return compile_int(form, SCM_NIL, -1);
}

static ScmObj compile_int(ScmObj form, ScmObj env, int ctx)
{
    ScmObj code = SCM_NIL, codetail;
    ScmVM *vm = Scm_VM();

    if (!SCM_PTRP(form)) {  /* immediate value */
        if (ctx == 0) return SCM_NIL;
        else return SCM_LIST1(form);
    }
    
    if (SCM_PAIRP(form)) {
        /* we have a pair.  This is either a special form
           or a function call */
        ScmObj head = SCM_CAR(form);
        
        if (SCM_SYMBOLP(head)) {
            head = lookup_env(head, env);
            if (SCM_SYMBOLP(head)) {
                /* Let's see if the symbol is bound to a syntax or a macro
                   in the current module. */
                ScmGloc *g = Scm_FindBinding(vm->module, SCM_SYMBOL(head), 0);
                if (g != NULL) {
                    if (SCM_SYNTAXP(g->value)) {
                        ScmCompileProc cmpl = SCM_SYNTAX(g->value)->compiler;
                        void *data = SCM_SYNTAX(g->value)->data;
                        return cmpl(form, env, ctx, data);
                    }
                }
                /* Symbol doesn't have syntactic bindings.  It must be
                   a global procedure call. */
                head = SCM_LIST2(SCM_VM_MAKE_INSN(SCM_VM_GREF), head);
            } else {
                head = SCM_LIST1(head);
            }
        } else {
            head = compile_int(head, env, 1);
        }
        /* here, we have general application */
        {
            ScmObj ap;
            int nargs = 0;

            SCM_FOR_EACH(ap, SCM_CDR(form)) {
                ScmObj arg = compile_int(SCM_CAR(ap), env, 1);
                SCM_GROW_LIST_SPLICING(code, codetail, arg);
                nargs++;
            }

            SCM_GROW_LIST_SPLICING(code, codetail, head);
            SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_CALL(nargs, ctx));
            SCM_GROW_LIST(code, codetail,
                          Scm_MakeSourceInfo(form, NULL));

            /* if this is in a statement context, discard the result. */
            if (ctx == 0) {
                SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_INSN(SCM_VM_POPARG));
            }
            return code;
        }
    }
    if (SCM_SYMBOLP(form)) {
        /* variable reference.  even in the statement context we evaluate
           the variable, for it may raise an error. */
        SCM_GROW_LIST_SPLICING(code, codetail, compile_varref(form, env));
        if (ctx == 0) {
            SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_INSN(SCM_VM_POPARG));
        }
        return code;
    }
    else {
        /* literal object.  if it appears in the statement context,
           we don't bother to include it. */
        if (ctx == 0) return SCM_NIL;
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
                return SCM_VM_MAKE_LREF(depth, offset);
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
    ScmObj start = SCM_NIL, end;
    if (SCM_SYMBOLP(v)) {
        SCM_GROW_LIST(start, end, SCM_VM_MAKE_INSN(SCM_VM_GREF));
    }
    SCM_GROW_LIST(start, end, v);
    SCM_GROW_LIST(start, end, Scm_MakeSourceInfo(sym, NULL));
    return start;
}

static int check_valid_lambda_args(ScmObj args)
{
    /* TODO: check them! */
    return 1;
}

/*
 * Built-in syntax
 */

/*
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
        if (!SCM_SYMBOLP(SCM_CAR(var)))
            Scm_Error("syntax error: %S", form);
        if (!check_valid_lambda_args(SCM_CDR(var)))
            Scm_Error("invalid argument list: %S", form);
        val = Scm_Cons(SCM_SYM_LAMBDA,
                       Scm_Cons(SCM_CDR(var), SCM_CDR(tail)));
        var = SCM_CAR(var);
    } else {
        if (!SCM_PAIRP(SCM_CDR(tail)) || !SCM_NULLP(SCM_CDR(SCM_CDR(tail))))
            Scm_Error("syntax error: %S", form);
        val = SCM_CAR(SCM_CDR(tail));
    }

    SCM_GROW_LIST_SPLICING(code, codetail, compile_int(val, env, 1));
    SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_INSN(SCM_VM_DEFINE));
    SCM_GROW_LIST(code, codetail, var);
    return code;
}

static ScmSyntax syntax_define = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_DEFINE),
    compile_define,
    NULL
};

/*
 * QUOTE
 */
static ScmObj compile_quote(ScmObj form,
                            ScmObj env, 
                            int ctx,
                            void *data)
{
    ScmObj tail = SCM_CDR(form);
    if (!SCM_PAIRP(tail) || !SCM_NULLP(SCM_CDR(tail)))
        Scm_Error("syntax error: %S", form);
    if (ctx == 0) return SCM_NIL;
    else          return SCM_LIST1(SCM_CAR(tail));
}

static ScmSyntax syntax_quote = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_QUOTE),
    compile_quote,
    NULL
};


/*
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
        || !SCM_NULLP(SCM_CDR(SCM_CDR(tail)))) {
        Scm_Error("syntax error: %S", form);
    }
    location = SCM_CAR(tail);
    expr = SCM_CAR(SCM_CDR(tail));

    /* TODO: support generalized set! */
    if (SCM_PAIRP(location)) {
        Scm_Error("generalized set! not supported (yet): %S", form);
    }
    if (!SCM_SYMBOLP(location)) {
        Scm_Error("syntax error: %S", form);
    }

    code = compile_int(expr, env, 1);
    codetail = Scm_LastPair(code);
    SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_INSN(SCM_VM_SET));
    SCM_GROW_LIST(code, codetail, lookup_env(location, env));

    /* set! doesn't return values.  however, if this is the tail call,
       the continuation may expect some value. */
    if (ctx != 0) SCM_GROW_LIST(code, codetail, SCM_UNDEFINED);
    return code;
}

static ScmSyntax syntax_set = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_SET),
    compile_set,
    NULL
};


/* Common routine for lambda, let-family and begin, to compile its body.
 * Assumes form is a proper list.
 * TODO: Internal defines need to be recognized.
 */
static ScmObj compile_body(ScmObj form,
                           ScmObj env,
                           int ctx)
{
    ScmObj body = SCM_NIL, body_p, form_p;

    SCM_FOR_EACH(form_p, form) {
        ScmObj x;
        
        if (SCM_NULLP(SCM_CDR(form_p))) {
            /* tail call */
            x = compile_int(SCM_CAR(form_p), env, ctx);
        } else {
            x = compile_int(SCM_CAR(form_p), env, 0);
        }
        SCM_GROW_LIST_SPLICING(body, body_p, x);
    }
    return body;
}

/*
 * LAMBDA
 */

/* TODO: possible optimization: if arglist is null (i.e. form is thunk),
 * we don't need to push environment frame, sacrificing debuging capability.
 * Need to cooperate with VM.
 */
static ScmObj compile_lambda(ScmObj form,
                             ScmObj env,
                             int ctx,
                             void *data)
{
    ScmObj tail = SCM_CDR(form);
    ScmObj args, body, newenv, bodycode, code = SCM_NIL, codetail;
    int nargs, restarg;
    
    if (!SCM_PAIRP(tail) || !SCM_PAIRP(SCM_CDR(tail)))
        Scm_Error("syntax error: %S", form);
    args = SCM_CAR(tail);
    body = SCM_CDR(tail);

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
            SCM_GROW_LIST(e, t, SCM_CAR(a));
            nargs++;
        }
        if (!SCM_NULLP(a)) {
            SCM_GROW_LIST(e, t, a);
            restarg++;
        }
        newenv = Scm_Cons(e, env);
    }

    bodycode = compile_body(body, newenv, -1);
    SCM_GROW_LIST_SPLICING(code, codetail, 
                           SCM_LIST3(SCM_VM_MAKE_LAMBDA(nargs,restarg),
                                     form, bodycode));
    if (ctx == 0)
        SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_INSN(SCM_VM_POPARG));
    return code;
}

static ScmSyntax syntax_lambda = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_LAMBDA),
    compile_lambda,
    NULL
};

/*
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

/*
 * IF family (IF, WHEN, UNLESS, AND, OR, COND)
 */

/* Common part for compiling if-family.  TEST_CLAUSE is a form of test
   part, while THEN_CODE and ELSE_CODE must be a compiled code.
   INSN is a VM instruction to be emitted, which is one of 
   SCM_VM_IF, SCM_VM_AND or SCM_VM_OR. */
static ScmObj compile_if_family(ScmObj test_clause, ScmObj then_code,
                                ScmObj else_code, int insn,
                                ScmObj env, int ctx)
{
    ScmObj code = SCM_NIL, codetail, test_code;

    test_code = compile_int(test_clause, env, 1);
    if (insn == SCM_VM_IF) {
        /* we need to make sure both instruction stream merges.
           (if insn is AND or OR, it is taken care of by the caller.) */
        ScmObj next_cell = SCM_LIST1(SCM_VM_MAKE_INSN(SCM_VM_NOP));
        then_code = Scm_Append2X(then_code, next_cell);
        else_code = Scm_Append2X(else_code, next_cell);
    }
    
    SCM_GROW_LIST_SPLICING(code, codetail, test_code);
    SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_INSN(insn));
    SCM_GROW_LIST(code, codetail, then_code);
    SCM_GROW_LIST_SPLICING(code, codetail, else_code);
    return code;
}

static ScmObj compile_if(ScmObj form, ScmObj env, int ctx, void *data)
{
    ScmObj tail = SCM_CDR(form), then_clause, else_clause;
    int nargs = Scm_Length(tail);
    
    if (nargs < 2 || nargs > 3) Scm_Error("syntax error: %S", form);
    then_clause = SCM_CAR(SCM_CDR(tail));
    if (nargs == 3) {
        else_clause = SCM_CAR(SCM_CDR(SCM_CDR(tail)));
    } else {
        else_clause = SCM_UNDEFINED;
    }
    return compile_if_family(SCM_CAR(tail),
                             compile_int(then_clause, env, ctx),
                             compile_int(else_clause, env, ctx),
                             SCM_VM_IF,
                             env, ctx);
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
                             SCM_VM_IF,
                             env, ctx);
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
            Scm_Cons(SCM_VM_MAKE_INSN(SCM_VM_POPARG),
                     compile_and_rec(SCM_CDR(conds), merger, orp, env, ctx));
        ScmObj no_more_test =
            (ctx == 0)? Scm_Cons(SCM_VM_MAKE_INSN(SCM_VM_POPARG), merger) : merger;
        return compile_if_family(SCM_CAR(conds),
                                 orp? no_more_test : more_test,
                                 orp? more_test : no_more_test,
                                 SCM_VM_IFNP,
                                 env, ctx);
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
        ScmObj merger = SCM_LIST1(SCM_VM_MAKE_INSN(SCM_VM_NOP));
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

static ScmObj compile_cond_int(ScmObj form, ScmObj clauses, ScmObj merger,
                               ScmObj env, int ctx)
{
    ScmObj clause, test, body;
    ScmObj code = SCM_NIL, codetail;
    ScmObj altcode = SCM_NIL, altcodetail;
    int clen;

    if (SCM_NULLP(clauses)) {
        if (ctx == 0) return merger;
        else return Scm_Cons(SCM_UNDEFINED, merger);
    }
    if (!SCM_PAIRP(clauses)) Scm_Error("syntax error: %S", form);
    
    clause = SCM_CAR(clauses);
    clen = Scm_Length(clause);
    if (clen <= 0) Scm_Error("invalid clause in cond form: %S", form);
    test = SCM_CAR(clause);
    body = SCM_CDR(clause);

    /* Check for `else' clause.  We don't need to check its binding. */
    if (test == SCM_SYM_ELSE) {
        if (!SCM_NULLP(SCM_CDR(clauses))) {
            Scm_Error("in `cond' form, extra clause appears after 'else' clause: %S",
                      form);
        }
        if (!SCM_PAIRP(body)) {
            Scm_Error("empty `else' clause is not allowed: %S", form);
        }
        SCM_GROW_LIST_SPLICING(code, codetail, compile_body(body, env, ctx));
        SCM_GROW_LIST_SPLICING(code, codetail, merger);
        return code;
    }

    /* Let's compile the clause. */
    if (clen >= 2 && SCM_CAR(body) == SCM_SYM_YIELDS) {
        /* `=>' */
        if (clen != 3) {
            Scm_Error("badly formed '=>' clause in cond form: %S", form);
        }
        SCM_GROW_LIST_SPLICING(code, codetail,
                               compile_int(SCM_CAR(SCM_CDR(body)), env, 1));
        SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_CALL(1, ctx));
        SCM_GROW_LIST_SPLICING(code, codetail, merger);
    } else if (clen == 1) {
        /* We can leave the test on the stack, if this form needs
           the result.  If this is in a statement context, however,
           we need to pop the test result. */
        if (ctx == 0) {
            SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_INSN(SCM_VM_POPARG));
        }
        SCM_GROW_LIST_SPLICING(code, codetail, merger);
    } else {
        /* Normal case */
        SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_INSN(SCM_VM_POPARG));
        SCM_GROW_LIST_SPLICING(code, codetail, compile_body(body, env, ctx));
        SCM_GROW_LIST_SPLICING(code, codetail, merger);
    }

    /* Rest of clauses. We have the result of test
       on the stack when the rest of clauses are called, so need to
       pop it first. */
    SCM_GROW_LIST(altcode, altcodetail, SCM_VM_MAKE_INSN(SCM_VM_POPARG));
    SCM_GROW_LIST_SPLICING(altcode, altcodetail,
                           compile_cond_int(form, SCM_CDR(clauses),
                                            merger, env, ctx));

    return compile_if_family(test, code, altcode,
                             SCM_VM_IFNP, env, ctx);
}


static ScmObj compile_cond(ScmObj form, ScmObj env, int ctx, void *data)
{
    ScmObj clauses = SCM_CDR(form);
    if (SCM_NULLP(clauses)) {
        if (ctx == 0) return SCM_NIL;
        else return SCM_LIST1(SCM_UNDEFINED);
    } else {
        ScmObj merger = SCM_LIST1(SCM_VM_MAKE_INSN(SCM_VM_NOP));
        return compile_cond_int(form, clauses, merger, env, ctx);
    }
}

static ScmSyntax syntax_cond = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_COND),
    compile_cond,
    NULL
};

/*
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

static ScmObj compile_let(ScmObj form,
                          ScmObj env,
                          int ctx,
                          void *data)
{
    int type = (int)data;
    ScmObj tail = SCM_CDR(form);
    ScmObj bindings, body, vars, vals;
    ScmObj newenv, code = SCM_NIL, codetail, bodycode;
    int nvars;

    /* TODO: check named let */

    if (!SCM_PAIRP(tail))
        Scm_Error("syntax error: %S", form);
    bindings = SCM_CAR(tail);
    body = SCM_CDR(tail);

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
            SCM_GROW_LIST(vars, vars_p, SCM_CAR(binding));
            SCM_GROW_LIST(vals, vals_p, SCM_CAR(SCM_CDR(binding)));
            nvars++;
        }
        if (!SCM_NULLP(bind_p))
            Scm_Error("syntax error (invalid binding form): %S", form);
    }

    /* Beginning of code */
    SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_LET(nvars));
    SCM_GROW_LIST(code, codetail, form); /* debug info */

    /* Compile binding part */
    {
        ScmObj vars_p, vals_p;
        ScmObj curframe, curframe_p; /* current frame */

        if (type == BIND_LETREC) curframe = vars;
        else                     curframe = SCM_NIL;
        newenv = Scm_Cons(curframe, env);

        vals_p = vals;
        nvars = 0;
        SCM_FOR_EACH(vars_p, vars) {
            ScmObj val = compile_int(SCM_CAR(vals_p), newenv, 1);
            SCM_GROW_LIST_SPLICING(code, codetail, val);
            SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_INSN(SCM_VM_SET));
            SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_LREF(0, nvars));
            vals_p = SCM_CDR(vals_p);
            
            if (type == BIND_LET_STAR) {
                SCM_GROW_LIST(curframe, curframe_p, SCM_CAR(vars_p));
                newenv = Scm_Cons(curframe, env);
            }
            nvars++;
        }
        if (type == BIND_LET) newenv = Scm_Cons(vars, env);
    }
    
    /* Compile body and append it to bindcode */
    bodycode = compile_body(body, newenv, ctx);
    SCM_GROW_LIST_SPLICING(code, codetail, bodycode);
    SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_INSN(SCM_VM_POPENV));
    if (ctx == 0)
        SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_INSN(SCM_VM_POPARG));
    return code;
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

/*
 * Initializer
 */

void Scm__InitCompiler(void)
{
    ScmModule *m = SCM_MODULE(Scm_SchemeModule());

#define DEFSYN(symbol, syntax) \
    Scm_Define(m, SCM_SYMBOL(symbol), SCM_OBJ(&syntax))
    
    DEFSYN(SCM_SYM_DEFINE,       syntax_define);
    DEFSYN(SCM_SYM_QUOTE,        syntax_quote);
    DEFSYN(SCM_SYM_SET,          syntax_set);
    DEFSYN(SCM_SYM_LAMBDA,       syntax_lambda);
    DEFSYN(SCM_SYM_BEGIN,        syntax_begin);
    DEFSYN(SCM_SYM_IF,           syntax_if);
    DEFSYN(SCM_SYM_WHEN,         syntax_when);
    DEFSYN(SCM_SYM_UNLESS,       syntax_unless);
    DEFSYN(SCM_SYM_AND,          syntax_and);
    DEFSYN(SCM_SYM_OR,           syntax_or);
    DEFSYN(SCM_SYM_COND,         syntax_cond);
    DEFSYN(SCM_SYM_LET,          syntax_let);
    DEFSYN(SCM_SYM_LET_STAR,     syntax_let_star);
    DEFSYN(SCM_SYM_LETREC,       syntax_letrec);
}
