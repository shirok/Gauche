/*
 * compile.c - compile the given form to an intermediate form
 *
 *   Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
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
 *  $Id: compile.c,v 1.110 2004-06-18 00:33:06 shirok Exp $
 */

#include <stdlib.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/builtin-syms.h"

/* constructor definition comes below */

/* global id to be inserted during transformation.
   initialized by Init routine. */
static ScmObj id_lambda = SCM_UNBOUND;
static ScmObj id_if     = SCM_UNBOUND;
static ScmObj id_begin  = SCM_UNBOUND;
static ScmObj id_letrec = SCM_UNBOUND;

/*#define GAUCHE_USE_NVM*/

/*#define GAUCHE_NVM_VECTORIZATION*/

/* Conventions of internal functions
 *
 *  - ctx parameter takes one of SCM_COMPILE_STMT, SCM_COMPILE_TAIL,
 *    SCM_COMPILE_NORMAL
 *
 *  - depth parameter is set by the maximum stack depth consumed by
 *    the execution of form.
 *
 *  - compile_* function always returns a list, which may be destructively
 *    concatenated later.
 */

static ScmObj compile_varref(ScmObj form, ScmObj env);
static ScmObj compile_int(ScmObj form, ScmObj env, int ctx, int *depth);
static ScmObj compile_lambda_family(ScmObj form, ScmObj args, ScmObj body,
                                    ScmObj env, int ctx, int *depth);
static ScmObj compile_body(ScmObj form, ScmObj env, int ctx, int *depth);

#define LIST1_P(obj) \
    (SCM_PAIRP(obj) && SCM_NULLP(SCM_CDR(obj)))
#define LIST2_P(obj) \
    (SCM_PAIRP(obj) && SCM_PAIRP(SCM_CDR(obj)) && SCM_NULLP(SCM_CDDR(obj)))

#define TAILP(ctx)  ((ctx) == SCM_COMPILE_TAIL)

#define NOINLINEP(vm) SCM_VM_COMPILER_FLAG_IS_SET((vm), SCM_COMPILE_NOINLINE)

#define ADDCODE1(c)   SCM_APPEND1(code, codetail, c)
#define ADDCODE(c)    SCM_APPEND(code, codetail, c)

#define ADDPUSH()     (combine_push(&code, &codetail))

/* common idiom to count maximum stack depth */
#define MAXDEPTH(maxdepth, depth) \
    do { if ((maxdepth) < (depth)) (maxdepth) = (depth); } while (0)

/* create local ref/set insn.  special instruction is used for local
   ref/set to the first frame with small number of offset (<5) for
   performance reason. */
static inline ScmObj make_lref(int depth, int offset)
{
#ifndef GAUCHE_NVM_VECTORIZATION
    if (depth == 0) {
        switch (offset) {
        case 0: return SCM_VM_INSN(SCM_VM_LREF0);
        case 1: return SCM_VM_INSN(SCM_VM_LREF1);
        case 2: return SCM_VM_INSN(SCM_VM_LREF2);
        case 3: return SCM_VM_INSN(SCM_VM_LREF3);
        case 4: return SCM_VM_INSN(SCM_VM_LREF4);
        }
    } else if (depth == 1) {
        switch (offset) {
        case 0: return SCM_VM_INSN(SCM_VM_LREF10);
        case 1: return SCM_VM_INSN(SCM_VM_LREF11);
        case 2: return SCM_VM_INSN(SCM_VM_LREF12);
        case 3: return SCM_VM_INSN(SCM_VM_LREF13);
        case 4: return SCM_VM_INSN(SCM_VM_LREF14);
        }
    }
    return SCM_VM_INSN2(SCM_VM_LREF, depth, offset);
#else  /* GAUCHE_NVM_VECTORIZATION */
    return SCM_VM_INSN2(SCM_VM_LREF, depth, offset);
#endif /* GAUCHE_NVM_VECTORIZATION */
}

static inline ScmObj make_lset(int depth, int offset)
{
    if (depth == 0) {
        switch (offset) {
        case 0: return SCM_VM_INSN(SCM_VM_LSET0);
        case 1: return SCM_VM_INSN(SCM_VM_LSET1);
        case 2: return SCM_VM_INSN(SCM_VM_LSET2);
        case 3: return SCM_VM_INSN(SCM_VM_LSET3);
        case 4: return SCM_VM_INSN(SCM_VM_LSET4);
        }
    }
    return SCM_VM_INSN2(SCM_VM_LSET, depth, offset);
}

static ScmObj add_srcinfo(ScmObj code, ScmObj source)
{
    if (SCM_PAIRP(code)) {
        SCM_PAIR_ATTR(code) = Scm_Acons(SCM_SYM_SOURCE_INFO, source,
                                        SCM_PAIR_ATTR(code));
    }
    return code;
}

static ScmObj add_bindinfo(ScmObj code, ScmObj info)
{
    if (SCM_PAIRP(code)) {
        SCM_PAIR_ATTR(code) = Scm_Acons(SCM_SYM_BIND_INFO, info,
                                        SCM_PAIR_ATTR(code));
    }
    return code;
}

/* add PUSH instruction to the stream.  if the last instruction is
   LREF, substitute it for combined instruction instead. */
static void combine_push(ScmObj *head, ScmObj *tail)
{
#ifndef GAUCHE_NVM_VECTORIZATION
    if (SCM_NULLP(*head)) {
        SCM_APPEND1(*head, *tail, SCM_VM_INSN(SCM_VM_PUSH));
    } else if (!SCM_VM_INSNP(SCM_CAR(*tail))) {
        ScmObj val = SCM_CAR(*tail);
        if (SCM_NULLP(val)) {
            SCM_SET_CAR(*tail, SCM_VM_INSN(SCM_VM_PUSHNIL));
        } else if (SCM_INTP(val) && SCM_VM_INSN_ARG_FITS(SCM_INT_VALUE(val))) {
            SCM_SET_CAR(*tail, SCM_VM_INSN1(SCM_VM_PUSHI, SCM_INT_VALUE(val)));
        } else {
            SCM_APPEND1(*head, *tail, SCM_VM_INSN(SCM_VM_PUSH));
        }
    } else {
        ScmObj insn = SCM_CAR(*tail);
        switch (SCM_VM_INSN_CODE(insn)) {
        case SCM_VM_LREF:
            SCM_SET_CAR(*tail, SCM_VM_INSN2(SCM_VM_LREF_PUSH,
                                            SCM_VM_INSN_ARG0(insn),
                                            SCM_VM_INSN_ARG1(insn)));
            break;
        case SCM_VM_LREF0:
            SCM_SET_CAR(*tail, SCM_VM_INSN(SCM_VM_LREF0_PUSH));
            break;
        case SCM_VM_LREF1:
            SCM_SET_CAR(*tail, SCM_VM_INSN(SCM_VM_LREF1_PUSH));
            break;
        case SCM_VM_LREF2:
            SCM_SET_CAR(*tail, SCM_VM_INSN(SCM_VM_LREF2_PUSH));
            break;
        case SCM_VM_LREF3:
            SCM_SET_CAR(*tail, SCM_VM_INSN(SCM_VM_LREF3_PUSH));
            break;
        case SCM_VM_LREF4:
            SCM_SET_CAR(*tail, SCM_VM_INSN(SCM_VM_LREF4_PUSH));
            break;
        case SCM_VM_LREF10:
            SCM_SET_CAR(*tail, SCM_VM_INSN(SCM_VM_LREF10_PUSH));
            break;
        case SCM_VM_LREF11:
            SCM_SET_CAR(*tail, SCM_VM_INSN(SCM_VM_LREF11_PUSH));
            break;
        case SCM_VM_LREF12:
            SCM_SET_CAR(*tail, SCM_VM_INSN(SCM_VM_LREF12_PUSH));
            break;
        case SCM_VM_LREF13:
            SCM_SET_CAR(*tail, SCM_VM_INSN(SCM_VM_LREF13_PUSH));
            break;
        case SCM_VM_LREF14:
            SCM_SET_CAR(*tail, SCM_VM_INSN(SCM_VM_LREF14_PUSH));
            break;
        case SCM_VM_CONS:
            SCM_SET_CAR(*tail, SCM_VM_INSN(SCM_VM_CONS_PUSH));
            break;
        case SCM_VM_CAR:
            SCM_SET_CAR(*tail, SCM_VM_INSN(SCM_VM_CAR_PUSH));
            break;
        case SCM_VM_CDR:
            SCM_SET_CAR(*tail, SCM_VM_INSN(SCM_VM_CDR_PUSH));
            break;
        default:
            SCM_APPEND1(*head, *tail, SCM_VM_INSN(SCM_VM_PUSH));
        }
    }
#else  /* GAUCHE_NVM_VECTORIZATION */
    SCM_APPEND1(*head, *tail, SCM_VM_INSN(SCM_VM_PUSH));
#endif /* GAUCHE_NVM_VECTORIZATION */
}

/* type of let-family bindings */
enum {
    BIND_LET,
    BIND_LET_STAR,
    BIND_LETREC
};

/*================================================================
 * Compiler
 *
 *   Statically analyzes given form recursively, converting it
 *   to the intermediate form.   Syntactic error is detected here.
 */

/* Semantics of global (free) reference:
 *
 *   We compile each toplevel form one-by-one.  Since the free
 *   reference binding can be inserted into the module after the
 *   current toplevel form, we can't resolve such bindings at the
 *   compilation time.  (If we had only one module, we could insert
 *   bindings whenever we saw free variables; in our hierarchical
 *   module system, we don't know if that binding will exist in the
 *   current module or the imported module).
 *
 *   So we just put a [GREF <id>] or [GSET <id>] in the compiled code,
 *   where <id> is an identifier.
 *   At runtime, VM looks for the binding in the given module and replaces
 *   <id> to the GLOC object.  Afterwards, the global reference will
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
 *   the variable from outside the module, described in Tung: "Interactive
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

/* entry point. */
ScmObj Scm_Compile(ScmObj form, ScmObj env, int context)
{
    int dummy;
    return compile_int(form, env, context, &dummy);
}

ScmObj Scm_CompileBody(ScmObj form, ScmObj env, int context)
{
    int dummy;
    return compile_body(form, env, context, &dummy);
}

/* Notes on compiler environment:
 *
 *   Compiler environment is a structure representing current lexical
 *   scope.  It is passed around during compilation.   It's not the
 *   same as runtime environment, which keeps actual values.
 *
 *   Compiler environment is simply a list of lists.  Each inner list
 *   represents a frame, and innermost frame comes first.  Each frame
 *   is either a variable binding frame or a syntactic binding frame.
 *   Syntactic binding frame is inserted by let-syntax and letrec-syntax.
 *
 *     (<var> ...)       ; variable binding frame
 *
 *     (#t (<var> . <syntax>) ...) ; syntactic binding frame
 *
 *   where <var> is either a symbol or an identifier.
 *
 *   There are a few interface functions to access this structure.
 *
 *     lookup_env(VAR, ENV, OP)  
 *        When OP is false, VAR is looked up in variable binding frames.
 *        if it is bound locally, an LREF object is returned.  Otherwise
 *        an identifier is returned.
 *        When OP is true, VAR is looked up both in variable binding
 *        frames and in syntactic binding frames.  If it is bound locally,
 *        an LREF object or a syntax object is returned.  Otherwise, VAR
 *        is returned.
 *
 *     global_eq(VAR, SYM, ENV)  returns true iff VAR is a free variable
 *        and it's symbol is eq? to SYM.
 *
 *   NB: empty variable frame is not counted by the compiler; at runtime,
 *   such frame is not generated.
 */

#define VAR_P(obj)         (SCM_SYMBOLP(obj)||SCM_IDENTIFIERP(obj))
#define ENSURE_SYMBOL(obj) \
    (SCM_IDENTIFIERP(obj)? SCM_OBJ(SCM_IDENTIFIER(obj)->name) : (obj))

#define TOPLEVEL_ENV_P(env)   SCM_NULLP(env)

static ScmObj lookup_env(ScmObj var, ScmObj env, int op)
{
    ScmObj ep, frame, fp;
    int depth = 0;
    SCM_FOR_EACH(ep, env) {
        int offset = 0, found = -1;
        if (SCM_IDENTIFIERP(var) && SCM_IDENTIFIER(var)->env == ep) {
            /* strip off the "wrapping" */
            var = SCM_OBJ(SCM_IDENTIFIER(var)->name);
        }
        frame = SCM_CAR(ep);
        if (SCM_PAIRP(frame)) {
            if (SCM_TRUEP(SCM_CAR(frame))) {
                /* macro binding */
                if (op) {
                    SCM_FOR_EACH(fp, SCM_CDR(frame)) {
                        if (SCM_EQ(var, SCM_CAAR(fp))) {
                            return SCM_CDAR(fp);
                        }
                    }
                }
                continue;
            }
            /* look for variable binding.  there may be a case that
               single frame contains more than one variable with the
               same name (in the case like '(let* ((x 1) (x 2)) ...)'),
               so we have to scan the frame until the end. */
            SCM_FOR_EACH(fp, frame) {
                if (SCM_EQ(var, SCM_CAR(fp))) {
                    found = offset;
                }
                offset++;
            }
            if (found >= 0) return make_lref(depth, offset - found - 1);
            depth++;
        }
    }
    if (SCM_SYMBOLP(var) && !op) {
        return Scm_MakeIdentifier(SCM_SYMBOL(var), SCM_NIL);
    } else {
        return var;
    }
}

static ScmObj get_binding_frame(ScmObj var, ScmObj env)
{
    ScmObj frame, fp;
    SCM_FOR_EACH(frame, env) {
        if (!SCM_PAIRP(SCM_CAR(frame))) continue;
        if (SCM_TRUEP(SCM_CAAR(frame))) {
            SCM_FOR_EACH(fp, SCM_CDAR(frame))
                if (SCM_CAAR(fp) == var) return frame;
        } else {
            SCM_FOR_EACH(fp, SCM_CAR(frame))
                if (SCM_CAR(fp) == var) return frame;
        }
    }
    return SCM_NIL;
}

/* Given symbol or identifier, try to find its global binding.
   Returns GLOC if found, or NULL otherwise. */
static ScmGloc *find_identifier_binding(ScmVM *vm, ScmObj sym_or_id)
{
    ScmModule *mod;
    ScmSymbol *sym;
    SCM_ASSERT(VAR_P(sym_or_id));
    if (SCM_IDENTIFIERP(sym_or_id)) {
        sym = SCM_IDENTIFIER(sym_or_id)->name;
        mod = SCM_IDENTIFIER(sym_or_id)->module;
    } else { /* sym_or_id is symbol */
        sym = SCM_SYMBOL(sym_or_id);
        mod = vm->module;
    }
    return Scm_FindBinding(mod, sym, FALSE);
}

static inline int global_eq(ScmObj var, ScmObj sym, ScmObj env)
{
    ScmObj v;
    if (!VAR_P(var)) return FALSE;
    v = lookup_env(var, env, TRUE);
    if (SCM_IDENTIFIERP(v)) {
        return SCM_OBJ(SCM_IDENTIFIER(v)->name) == sym;
    } else if (SCM_SYMBOLP(v)) {
        return v == sym;
    } else {
        return FALSE;
    }
}

ScmObj Scm_CompileLookupEnv(ScmObj sym, ScmObj env, int op)
{
    return lookup_env(sym, env, op);
}

/*-------------------------------------------------------------
 * Identifier object
 */

static void identifier_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<id %p %A::%A>",
               obj,
               SCM_IDENTIFIER(obj)->module->name,
               SCM_IDENTIFIER(obj)->name);
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_IdentifierClass, identifier_print);

ScmObj Scm_MakeIdentifier(ScmSymbol *name, ScmObj env)
{
    ScmIdentifier *id = SCM_NEW(ScmIdentifier);
    SCM_SET_CLASS(id, SCM_CLASS_IDENTIFIER);
    id->name = name;
    id->module = SCM_CURRENT_MODULE();
    id->env = (env == SCM_NIL)? SCM_NIL : get_binding_frame(SCM_OBJ(name), env);
    return SCM_OBJ(id);
}

/* returns true if SYM has the same binding with ID in ENV. */
int Scm_IdentifierBindingEqv(ScmIdentifier *id, ScmSymbol *sym, ScmObj env)
{
    ScmObj bf = get_binding_frame(SCM_OBJ(sym), env);
    return (bf == id->env);
}

/* returns true if variable VAR (symbol or identifier) is free and equal
   to symbol SYM */
int Scm_FreeVariableEqv(ScmObj var, ScmObj sym, ScmObj env)
{
    return global_eq(var, sym, env);
}

ScmObj Scm_CopyIdentifier(ScmIdentifier *orig)
{
    ScmIdentifier *id = SCM_NEW(ScmIdentifier);
    SCM_SET_CLASS(id, SCM_CLASS_IDENTIFIER);
    id->name = orig->name;
    id->module = orig->module;
    id->env = orig->env;
    return SCM_OBJ(id);
}

/* used in compile_define.  var may be a symbol or an identifier. */
static ScmObj ensure_identifier(ScmObj var, ScmObj env, ScmModule *mod)
{
    ScmObj ident;
    if (SCM_SYMBOLP(var)) {
        ident = Scm_MakeIdentifier(SCM_SYMBOL(var), env);
        if (mod) SCM_IDENTIFIER(ident)->module = mod;
        return ident;
    } else if (mod) {
        ident = Scm_CopyIdentifier(SCM_IDENTIFIER(var));
        SCM_IDENTIFIER(ident)->module = mod;
        return ident;
    } else {
        return var;
    }
}

/*------------------------------------------------------------------
 * Compiler main body
 */

static ScmObj compile_int(ScmObj form, ScmObj env, int ctx, int *depth)
{
    ScmObj code = SCM_NIL, codetail = SCM_NIL;
    ScmVM *vm = Scm_VM();
    *depth = 0;

    if (SCM_PAIRP(form)) {
        /* we have a pair.  This is either a special form
           or a function call */
        ScmObj head = SCM_CAR(form);
        ScmCompileProc cmpl;
        void *data;
        int headdepth = 0;

        if (Scm_Length(form) < 0) {
            Scm_Error("improper list can't be evaluated: %S", form);
        }

        if (VAR_P(head)) {
            ScmObj var = lookup_env(head, env, TRUE);

            if (SCM_VM_INSNP(var)) {
                /* variable is bound locally */
                head = add_srcinfo(SCM_LIST1(var), head);
            } else if (SCM_SYNTAXP(var)) {
                /* variable is bound syntactically. */
                cmpl = SCM_SYNTAX(var)->compiler;
                data = SCM_SYNTAX(var)->data;
                return cmpl(form, env, ctx, depth, data);
            } else {
                /* it's a global variable.   Let's see if the symbol is
                   bound to a global syntax, or an inlinable procedure
                   in the current module. */
                ScmGloc *g = find_identifier_binding(vm, var);
                if (g != NULL) {
                    ScmObj gv = SCM_GLOC_GET(g);
                    if (SCM_SYNTAXP(gv)) {
                        cmpl = SCM_SYNTAX(gv)->compiler;
                        data = SCM_SYNTAX(gv)->data;
                        return cmpl(form, env, ctx, depth, data);
                    }
                    if (!NOINLINEP(vm) && SCM_SUBRP(g->value)
                        && SCM_SUBR_INLINER(gv)) {
                        ScmObj inlined
                            = SCM_SUBR_INLINER(gv)(SCM_SUBR(g->value),
                                                   form, env, ctx, depth);
                        if (!SCM_FALSEP(inlined)) {
                            add_srcinfo(Scm_LastPair(inlined), form);
                            return inlined;
                        }
                    }
                }
                /* Symbol doesn't have syntactic bindings.  It must be
                   a global procedure call. */
                head = compile_varref(var, SCM_NIL);
            }
        } else {
            head = compile_int(head, env, SCM_COMPILE_NORMAL, &headdepth);
        }
        /* here, we have general application */
        {
            ScmObj ap, argcode;
            int nargs = 0;
            int maxdepth = 0, subdepth;
            
            SCM_FOR_EACH(ap, SCM_CDR(form)) {
                argcode = compile_int(SCM_CAR(ap), env, SCM_COMPILE_NORMAL,
                                      &subdepth);
                ADDCODE(argcode);
                ADDPUSH();
                MAXDEPTH(maxdepth, subdepth + nargs);
                nargs++;
            }

            ADDCODE(head);
            MAXDEPTH(maxdepth, headdepth + nargs);
            if (TAILP(ctx)) {
                ADDCODE1(SCM_VM_INSN1(SCM_VM_TAIL_CALL, nargs));
                code = Scm_Cons(SCM_VM_INSN1(SCM_VM_PRE_TAIL, nargs), code);
            } else {
                MAXDEPTH(maxdepth, ENV_SIZE(nargs)+CONT_FRAME_SIZE);
                ADDCODE1(SCM_VM_INSN1(SCM_VM_CALL, nargs));
                code = SCM_LIST2(SCM_VM_INSN1(SCM_VM_PRE_CALL, nargs), code);
            }
            *depth = maxdepth;
            return add_srcinfo(code, form);
        }
    }
    if (VAR_P(form)) {
        /* variable reference.  even in the statement context we evaluate
           the variable, for it may raise an error. */
        ADDCODE(compile_varref(form, env));
        return add_srcinfo(code, form);
    }
    else {
        /* literal object.  if it appears in the statement context,
           we don't bother to include it. */
        if (ctx == SCM_COMPILE_STMT) return SCM_NIL;
        else return SCM_LIST1(form);
    }
}

/* obj may be a symbol or an identifier */
static ScmObj compile_varref(ScmObj obj, ScmObj env)
{
    ScmObj loc, code;

    loc = lookup_env(obj, env, FALSE);
    if (VAR_P(loc)) {
        /* global variable.  see if it's constant or not */
        ScmGloc *g = find_identifier_binding(Scm_VM(), loc);
        if (g && SCM_GLOC_CONST_P(g)) {
            code = SCM_LIST1(SCM_GLOC_GET(g)); /* insert constant value */
        } else {
            code = SCM_LIST2(SCM_VM_INSN(SCM_VM_GREF), loc);
        }
    } else {
        code = SCM_LIST1(loc);
    }
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
 * DEFINE-CONSTANT
 * DEFINE-IN-MODULE
 *   This should never called for internal defines (they are handled by
 *   compile_body).
 */
enum {
    DEFINE_TYPE_DEFINE,
    DEFINE_TYPE_CONST,
    DEFINE_TYPE_IN_MODULE
};

static ScmObj compile_define(ScmObj form, ScmObj env, int ctx,
                             int *depth, void *data)
{
    ScmObj var, val, tail = SCM_CDR(form);
    ScmObj code = SCM_NIL, codetail = SCM_NIL;
    ScmModule *module = NULL;
    int type = (int)(long)data;

    /* See if we are called in non-toplevel env.
       NB: this isn't checked until Gauche 0.8, and there may be
       some code that depends on the fact that one can use define-in-module
       in non-toplevel env.  That should be forbidden in future, but for
       now we just warn it to the user. */
    if (!SCM_NULLP(env)) {
        if (type == DEFINE_TYPE_IN_MODULE) {
            Scm_Warn("%S is used at non-toplevel: %S",
                     SCM_CAR(form), form);
        } else {
            Scm_Error("%S is used at non-toplevel: %S",
                      SCM_CAR(form), form);
        }
    }
    
    if (type == DEFINE_TYPE_IN_MODULE) {
        ScmObj mod;
        if (!SCM_PAIRP(tail)) Scm_Error("syntax error: %S", form);
        mod = SCM_CAR(tail);
        tail = SCM_CDR(tail);
        if (SCM_IDENTIFIERP(mod))  mod = SCM_OBJ(SCM_IDENTIFIER(mod)->name);
        if (SCM_SYMBOLP(mod)){
            module = SCM_MODULE(Scm_FindModule(SCM_SYMBOL(mod), FALSE));
            if (!SCM_MODULEP(module)) {
                Scm_Error("define-in-module: no such module: %S", mod);
            }
        } else if (SCM_MODULEP(mod)) {
            module = SCM_MODULE(mod);
        } else {
            Scm_Error("malformed define-in-module: module or module name required, but got %S", mod);
        }
    }
    if (!SCM_PAIRP(tail)) Scm_Error("syntax error: %S", form);
    var = SCM_CAR(tail);

    if (SCM_PAIRP(var)) {
        /* (define (f args ...) body ...) */
        if (!VAR_P(SCM_CAR(var))) Scm_Error("syntax error: %S", form);
        val = compile_lambda_family(form, SCM_CDR(var), SCM_CDR(tail),
                                    env, SCM_COMPILE_NORMAL, depth);
        var = ensure_identifier(SCM_CAR(var), env, module);
    } else {
        if (!VAR_P(var) || !LIST1_P(SCM_CDR(tail)))
            Scm_Error("syntax error: %S", form);
        val = compile_int(SCM_CADR(tail), env, SCM_COMPILE_NORMAL, depth);
        var = ensure_identifier(var, env, module);
    }

    ADDCODE(val);
    if (type == DEFINE_TYPE_CONST) {
        ADDCODE1(SCM_VM_INSN(SCM_VM_DEFINE_CONST));
    } else {
        ADDCODE1(SCM_VM_INSN(SCM_VM_DEFINE));
    }
    ADDCODE1(var);
    return code;
}

static ScmSyntax syntax_define = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_DEFINE),
    compile_define,
    (void*)DEFINE_TYPE_DEFINE
};

static ScmSyntax syntax_define_constant = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_DEFINE_CONSTANT),
    compile_define,
    (void*)DEFINE_TYPE_CONST
};

static ScmSyntax syntax_define_in_module = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_DEFINE_IN_MODULE),
    compile_define,
    (void*)DEFINE_TYPE_IN_MODULE
};

/*------------------------------------------------------------------
 * QUOTE
 */

/* Convert all identifiers in form into a symbol.
   Avoid extra allocation as much as possible. */
static ScmObj unwrap_identifier(ScmObj form)
{
    if (SCM_PAIRP(form)) {
        ScmObj ca = unwrap_identifier(SCM_CAR(form));
        ScmObj cd = unwrap_identifier(SCM_CDR(form));
        if (ca == SCM_CAR(form) && cd == SCM_CDR(form)) {
            return form;
        } else {
            return Scm_Cons(ca, cd);
        }
    }
    if (SCM_IDENTIFIERP(form)) {
        return SCM_OBJ(SCM_IDENTIFIER(form)->name);
    }
    if (SCM_VECTORP(form)) {
        int i, j, len = SCM_VECTOR_SIZE(form);
        ScmObj elt, *pelt = SCM_VECTOR_ELEMENTS(form);
        for (i=0; i<len; i++, pelt++) {
            elt = unwrap_identifier(*pelt);
            if (elt != *pelt) {
                ScmObj newvec = Scm_MakeVector(len, SCM_FALSE);
                pelt = SCM_VECTOR_ELEMENTS(form);
                for (j=0; j<i; j++, pelt++) {
                    SCM_VECTOR_ELEMENT(newvec, j) = *pelt;
                }
                SCM_VECTOR_ELEMENT(newvec, i) = elt;
                for (; j<len; j++, pelt++) {
                    SCM_VECTOR_ELEMENT(newvec, j) = unwrap_identifier(*pelt);
                }
                return newvec;
            }
        }
        return form;
    }
    return form;
}

static ScmObj compile_quote(ScmObj form, ScmObj env, int ctx,
                            int *depth, void *data)
{
    ScmObj tail = SCM_CDR(form), info;
    *depth = 0;
    if (!LIST1_P(tail)) Scm_Error("syntax error: %S", form);
    if (ctx == SCM_COMPILE_STMT) return SCM_NIL;
    /* Kludge!  We don't want to call unwrap_identifier if the literal
       quote form contains circle, e.g. '#0=(1 . #0#).
       Unwrap_identifier is needed only if the form is created by
       macro; so, for the time being, we just check if the form is
       a literal form or not.   This still has a problem if the circular
       form is introduced within a macro definition, but we'll get into
       other troubles in such a case anyway. */
    /* NB: This relies on the fact that macro expander doesn't preserve
       the source-info.   If macro expander start handling source-info,
       we need another strategy. */
    info = Scm_PairAttrGet(SCM_PAIR(form), SCM_SYM_SOURCE_INFO, SCM_FALSE);
    if (SCM_FALSEP(info)) {
        return SCM_LIST1(unwrap_identifier(SCM_CAR(tail)));
    } else {
        return SCM_LIST1(SCM_CAR(tail));
    }
}

static ScmSyntax syntax_quote = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_QUOTE),
    compile_quote,
    NULL
};

/*------------------------------------------------------------------
 * SET!
 */
static ScmObj compile_set(ScmObj form, ScmObj env, int ctx,
                          int *depth, void *data)
{
    ScmObj tail = SCM_CDR(form);
    ScmObj location, expr;
    ScmObj code = SCM_NIL, codetail = SCM_NIL;
    *depth = 0;

    if (!LIST2_P(tail)) Scm_Error("syntax error: %S", form);
    location = SCM_CAR(tail);
    expr = SCM_CADR(tail);

    if (SCM_PAIRP(location)) {
        /* generalized set!
         * (set (proc args ...) value) => ((setter proc) args ... value)
         */
        /* TODO: inline known setters */
        ScmObj args, arg;
        int nargs = 0, maxdepth = 0, argdepth;
        SCM_FOR_EACH(args, SCM_CDR(location)) {
            arg = compile_int(SCM_CAR(args), env, SCM_COMPILE_NORMAL,
                              &argdepth);
            ADDCODE(arg);
            ADDPUSH();
            MAXDEPTH(maxdepth, argdepth + nargs);
            nargs++;
        }
        if (!SCM_NULLP(args)) {
            Scm_Error("syntax error for generalized set! location: %S", form);
        }
        ADDCODE(compile_int(expr, env, SCM_COMPILE_NORMAL, &argdepth));
        ADDPUSH();
        MAXDEPTH(maxdepth, argdepth + nargs);
        nargs++;
        ADDCODE(compile_int(SCM_CAR(location), env, SCM_COMPILE_NORMAL,
                            &argdepth));
        MAXDEPTH(maxdepth, argdepth + nargs);
        ADDCODE1(SCM_VM_INSN(SCM_VM_SETTER));

        if (TAILP(ctx)) {
            ADDCODE1(SCM_VM_INSN1(SCM_VM_TAIL_CALL, nargs));
            code = Scm_Cons(SCM_VM_INSN1(SCM_VM_PRE_TAIL, nargs), code);
        } else {
            MAXDEPTH(maxdepth, ENV_SIZE(nargs) + CONT_FRAME_SIZE);
            ADDCODE1(SCM_VM_INSN1(SCM_VM_CALL, nargs));
            code = SCM_LIST2(SCM_VM_INSN1(SCM_VM_PRE_CALL, nargs), code);
        }
        *depth += maxdepth;
        return code;
    }
    if (!VAR_P(location)) {
        Scm_Error("syntax error: %S", form);
    }

    location = lookup_env(location, env, FALSE);
    ADDCODE(compile_int(expr, env, SCM_COMPILE_NORMAL, depth));
    if (SCM_IDENTIFIERP(location)) {
        ADDCODE1(SCM_VM_INSN(SCM_VM_GSET));
        ADDCODE1(location);
    } else {
        switch (SCM_VM_INSN_CODE(location)) {
        case SCM_VM_LREF: {
            int dep = SCM_VM_INSN_ARG0(location);
            int off = SCM_VM_INSN_ARG1(location);
            ADDCODE1(make_lset(dep, off));
            break;
        }
        case SCM_VM_LREF0: ADDCODE1(make_lset(0, 0)); break;
        case SCM_VM_LREF1: ADDCODE1(make_lset(0, 1)); break;
        case SCM_VM_LREF2: ADDCODE1(make_lset(0, 2)); break;
        case SCM_VM_LREF3: ADDCODE1(make_lset(0, 3)); break;
        case SCM_VM_LREF4: ADDCODE1(make_lset(0, 4)); break;
        case SCM_VM_LREF10: ADDCODE1(make_lset(1, 0)); break;
        case SCM_VM_LREF11: ADDCODE1(make_lset(1, 1)); break;
        case SCM_VM_LREF12: ADDCODE1(make_lset(1, 2)); break;
        case SCM_VM_LREF13: ADDCODE1(make_lset(1, 3)); break;
        case SCM_VM_LREF14: ADDCODE1(make_lset(1, 4)); break;
        default:
            Scm_Panic("something definitely wrong with compiler");
        }
    }
    return code;
}

static ScmSyntax syntax_set = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_SET),
    compile_set,
    NULL
};


/*------------------------------------------------------------------
 * LAMBDA
 */

/* Common routine for lambda, let-family and begin, to compile its body.
 */
static ScmObj compile_body(ScmObj form, ScmObj env,
                           int ctx, int *depth)
{
    ScmObj body = SCM_NIL, bodytail = SCM_NIL, formtail = SCM_NIL;
    ScmObj idef_vars = SCM_NIL, idef_vars_tail = SCM_NIL, idef_save = SCM_NIL;
    ScmObj idef_vals = SCM_NIL, idef_vals_tail = SCM_NIL;
    int idefs = 0, body_started = 0, maxdepth = 0, subdepth;

    if (Scm_Length(form) < 0) {
        Scm_Error("body must be a proper list, but got %S", form);
    }

    *depth = 0;
    for (formtail = form; SCM_PAIRP(formtail); ) {
        ScmObj expr = SCM_CAR(formtail), x;
        /* Begin in the body should work as if it's body is spliced in
           the current form. */
        if (SCM_PAIRP(expr) && global_eq(SCM_CAR(expr), SCM_SYM_BEGIN, env)) {
            ScmObj beginbody = Scm_CopyList(SCM_CDR(expr));
            formtail = Scm_Append2X(beginbody, SCM_CDR(formtail));
            continue;
        }
        
        /* Check for internal define. */
        if (SCM_PAIRP(expr) && global_eq(SCM_CAR(expr), SCM_SYM_DEFINE, env)) {
            ScmObj var, val;
            int llen;

            if (body_started)
                Scm_Error("internal define should appear at the head of the body: %S",
                          expr);
            if ((llen = Scm_Length(expr)) < 3)
                Scm_Error("badly formed internal define: %S", expr);
            var = SCM_CADR(expr);
            if (SCM_PAIRP(var)) {
                ScmObj args = SCM_CDR(var);
                var = SCM_CAR(var);
                if (!VAR_P(var) || !check_valid_lambda_args(args))
                    Scm_Error("badly formed internal define: %S", expr);
                val = Scm_Cons(id_lambda, Scm_Cons(args, SCM_CDDR(expr)));
            } else {
                if (!VAR_P(var) || llen != 3)
                    Scm_Error("badly formed internal define: %S", expr);
                val = SCM_CAR(SCM_CDDR(expr));
            }

            SCM_APPEND1(idef_vars, idef_vars_tail, var);
            SCM_APPEND1(idef_vals, idef_vals_tail, val);
            idefs++;
            formtail = SCM_CDR(formtail);
            continue;
        } else if (!body_started && idefs > 0) {
            /* This is the beginning of the real body after interal defines.
               Creates a new env for them and bind them. */
            int cnt;

            idef_save = idef_vars;
            env = Scm_Cons(idef_vars, env);
            for (cnt=0; cnt<idefs; cnt++) {
                SCM_APPEND(body, bodytail,
                           compile_int(SCM_CAR(idef_vals), env,
                                       SCM_COMPILE_NORMAL, &subdepth));
                SCM_APPEND1(body, bodytail, make_lset(0, idefs-cnt-1));
                idef_vars = SCM_CDR(idef_vars);
                idef_vals = SCM_CDR(idef_vals);
                MAXDEPTH(maxdepth, subdepth);
            }
            *depth += maxdepth + ENV_SIZE(idefs);
            maxdepth = 0;
        }

        if (SCM_NULLP(SCM_CDR(formtail))) {
            /* tail call */
            x = compile_int(expr, env, ctx, &subdepth);
        } else {
            x = compile_int(expr, env, SCM_COMPILE_STMT, &subdepth);
        }
        SCM_APPEND(body, bodytail, x);
        body_started = !SCM_NULLP(body);
        MAXDEPTH(maxdepth, subdepth);
        formtail = SCM_CDR(formtail);
    }

    *depth += maxdepth;
    if (idefs > 0) {
        /* Internal defines introduced a new scope. */
        body = add_bindinfo(SCM_LIST2(SCM_VM_INSN1(SCM_VM_LET, idefs), body),
                            idef_save);
    }
    return body;
}

/* Common routine to compile lambda.
 */
static ScmObj compile_lambda_family(ScmObj form, ScmObj args, ScmObj body,
                                    ScmObj env, int ctx, int *depth)
{
    ScmObj newenv, bodycode, code = SCM_NIL, codetail = SCM_NIL;
    int nargs, restarg;
    
    if (!check_valid_lambda_args(args))
        Scm_Error("syntax error: %S", form);
    if (Scm_Length(body) <= 0)
        Scm_Error("badly formed body: %S", form);

    /* extend environment */
    {
        ScmObj a, e = SCM_NIL, t = SCM_NIL;
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

    bodycode = compile_body(body, newenv, SCM_COMPILE_TAIL, depth);
    SCM_APPEND(code, codetail, 
               add_bindinfo(SCM_LIST2(SCM_VM_INSN2(SCM_VM_LAMBDA, nargs, restarg),
                                      bodycode),
                            SCM_CAR(newenv)));
    *depth += ENV_SIZE(nargs+restarg);
    return code;
}

static ScmObj compile_lambda(ScmObj form,
                             ScmObj env,
                             int ctx,
                             int *depth,
                             void *data)
{
    ScmObj tail = SCM_CDR(form);
    ScmObj args, body;
    
    if (!SCM_PAIRP(tail) || !SCM_PAIRP(SCM_CDR(tail))) {
        Scm_Error("bad lambda form: %S", form);
    }
    args = SCM_CAR(tail);
    body = SCM_CDR(tail);

    return compile_lambda_family(form, args, body, env, ctx, depth);
}

static ScmSyntax syntax_lambda = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
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
                            int *depth,
                            void *data)
{
    if (TOPLEVEL_ENV_P(env)) {
        ScmObj code = SCM_NIL, codetail = SCM_NIL, cp;
        int maxdepth = 0, subdepth;
        
        SCM_FOR_EACH(cp, SCM_CDR(form)) {
            ADDCODE(compile_int(SCM_CAR(cp), env,
                                (SCM_NULLP(SCM_CDR(cp)))?
                                SCM_COMPILE_NORMAL : SCM_COMPILE_STMT,
                                &subdepth));
            MAXDEPTH(maxdepth, subdepth);
        }
        *depth = maxdepth;
        return code;
    } else {
        return compile_body(SCM_CDR(form), env, ctx, depth);
    }
}

static ScmSyntax syntax_begin = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_BEGIN),
    compile_begin,
    NULL
};

/*------------------------------------------------------------------
 * IF family (IF, WHEN, UNLESS, AND, OR, COND)
 */

/* Common part for compiling if-family.  TEST_CODE is a form or a
   compiled code of the test part.
   THEN_CODE and ELSE_CODE must be a compiled code for then clause
   and else clause, respectively. */

static ScmObj compile_if_family(ScmObj test_code, ScmObj then_code,
                                ScmObj else_code,
                                int test_compile_p, ScmObj env, int *depth)
{
    ScmObj code = SCM_NIL, codetail = SCM_NIL;
    if (test_compile_p) {
        test_code = compile_int(test_code, env, SCM_COMPILE_NORMAL, depth);
    }
    ADDCODE(test_code);
    ADDCODE1(SCM_VM_INSN(SCM_VM_IF));
    ADDCODE1(then_code);
    ADDCODE(else_code);
    return code;
}

static ScmObj compile_if(ScmObj form, ScmObj env, int ctx, int *depth,
                         void *data)
{
    ScmObj tail = SCM_CDR(form);
    ScmObj then_code = SCM_NIL, then_tail = SCM_NIL;
    ScmObj else_code = SCM_NIL, else_tail = SCM_NIL;
    ScmObj merger = TAILP(ctx)? SCM_NIL : SCM_LIST1(SCM_VM_INSN(SCM_VM_MNOP));
    int nargs = Scm_Length(tail);

    if (nargs < 2 || nargs > 3) Scm_Error("syntax error: %S", form);
    SCM_APPEND(then_code, then_tail, compile_int(SCM_CADR(tail), env, ctx, depth));
    SCM_APPEND(then_code, then_tail, merger);
    if (nargs == 3) {
        SCM_APPEND(else_code, else_tail,
                   compile_int(SCM_CAR(SCM_CDDR(tail)), env, ctx, depth));
    } else {
        SCM_APPEND1(else_code, else_tail, SCM_UNDEFINED);
    }
    SCM_APPEND(else_code, else_tail, merger);
    return compile_if_family(SCM_CAR(tail), then_code, else_code, TRUE, env, depth);
}

static ScmSyntax syntax_if = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_IF),
    compile_if,
    NULL
};

static ScmObj compile_when(ScmObj form, ScmObj env, int ctx, int *depth,
                           void *data)
{
    ScmObj tail = SCM_CDR(form);
    ScmObj then_code = SCM_NIL, then_tail = SCM_NIL;
    ScmObj else_code = SCM_NIL, else_tail = SCM_NIL;
    ScmObj merger = TAILP(ctx)? SCM_NIL : SCM_LIST1(SCM_VM_INSN(SCM_VM_MNOP));
    int unlessp = (data != NULL);
    int nargs = Scm_Length(tail);
    if (nargs < 2) Scm_Error("syntax error: %S", form);

    SCM_APPEND(then_code, then_tail, compile_body(SCM_CDR(tail), env, ctx, depth));
    SCM_APPEND(then_code, then_tail, merger);
    if (ctx != SCM_COMPILE_STMT)
        SCM_APPEND1(else_code, else_tail, SCM_UNDEFINED);
    SCM_APPEND(else_code, else_tail, merger);

    if (unlessp) {
        /* for UNLESS, we just swap then and else clause. */
        ScmObj t = then_code; then_code = else_code; else_code = t;
    }
    return compile_if_family(SCM_CAR(tail), then_code, else_code, TRUE, env, depth);
}

static ScmSyntax syntax_when = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_WHEN),
    compile_when,
    (void*)0
};

static ScmSyntax syntax_unless = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_UNLESS),
    compile_when,
    (void*)1
};

static ScmObj compile_and_rec(ScmObj conds, ScmObj merger, int orp,
                              ScmObj env, int ctx, int *depth)
{
    if (!SCM_PAIRP(SCM_CDR(conds))) {
        ScmObj last_test = compile_int(SCM_CAR(conds), env, ctx, depth);
        return Scm_Append2X(last_test, merger);
    } else {
        ScmObj more_test =
            compile_and_rec(SCM_CDR(conds), merger, orp, env, ctx, depth);
        ScmObj no_more_test = merger;
        return compile_if_family(SCM_CAR(conds),
                                 orp? no_more_test : more_test,
                                 orp? more_test : no_more_test,
                                 TRUE, env, depth);
    }
}

static ScmObj compile_and(ScmObj form, ScmObj env, int ctx, int *depth, void *data)
{
    ScmObj tail = SCM_CDR(form);
    int orp = (data != NULL);
    
    if (!SCM_PAIRP(tail)) {
        /* (and) or (or) is compiled into a literal boolean */
        if (ctx == SCM_COMPILE_STMT) return SCM_NIL;
        else return orp ? SCM_LIST1(SCM_FALSE) : SCM_LIST1(SCM_TRUE);
    } else {
        ScmObj merger = TAILP(ctx)? SCM_NIL:SCM_LIST1(SCM_VM_INSN(SCM_VM_MNOP));
        return compile_and_rec(tail, merger, orp, env, ctx, depth);
    }
}

static ScmSyntax syntax_and = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_AND),
    compile_and,
    (void*)0
};

static ScmSyntax syntax_or = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_OR),
    compile_and,
    (void*)1
};

/* Common part of compiling cond/case.
 *   CLAUSES - list of clauses
 *   MERGER - compiled code stream to where all the control emerge.
 *   CASEP - TRUE if we're compiling case, FALSE for cond.
 */
static ScmObj compile_cond_int(ScmObj form, ScmObj clauses, ScmObj merger,
                               ScmObj env, int ctx, int *depth, int casep)
{
    ScmObj clause, test, body;
    ScmObj code = SCM_NIL, codetail = SCM_NIL;
    ScmObj altcode = SCM_NIL, altcodetail = SCM_NIL;
    int clen;

    if (SCM_NULLP(clauses)) {
        if (casep) ADDCODE1(SCM_VM_INSN(SCM_VM_POP));
        /* If caller expects a result, let it have undefined value. */
        if (ctx != SCM_COMPILE_STMT) ADDCODE1(SCM_UNDEFINED);
        /* merge control */
        ADDCODE(merger);
        return code;
    }
    if (!SCM_PAIRP(clauses)) Scm_Error("syntax error: %S", form);
    
    clause = SCM_CAR(clauses);
    clen = Scm_Length(clause);
    if ((casep && clen < 2) || (!casep && clen < 1))
        Scm_Error("invalid clause in the form: %S", form);
    test = SCM_CAR(clause);
    body = SCM_CDR(clause);

    /* Check for `else' clause. */
    if (global_eq(test, SCM_SYM_ELSE, env)) {
        if (!SCM_NULLP(SCM_CDR(clauses))) {
            Scm_Error("extra clause appears after 'else' clause: %S", form);
        }
        if (!SCM_PAIRP(body)) {
            Scm_Error("empty `else' clause is not allowed: %S", form);
        }
        if (casep) ADDCODE1(SCM_VM_INSN(SCM_VM_POP));
        ADDCODE(compile_body(body, env, ctx, depth));
        ADDCODE(merger);
        return code;
    }

    /* Let's compile the clause. */
    if (!casep && clen >= 2 && global_eq(SCM_CAR(body), SCM_SYM_YIELDS, env)) {
        /* `=>' */
        ScmObj xcode = SCM_NIL, xtail = SCM_NIL;
        
        if (clen != 3) {
            Scm_Error("badly formed '=>' clause in the form: %S", form);
        }

        combine_push(&xcode, &xtail);
        SCM_APPEND(xcode, xtail,
                   compile_int(SCM_CADR(body), env, SCM_COMPILE_NORMAL, depth));
        if (TAILP(ctx)) {
            SCM_APPEND1(xcode, xtail, SCM_VM_INSN1(SCM_VM_TAIL_CALL, 1));
            SCM_APPEND(xcode, xtail, merger);
            ADDCODE(Scm_Cons(SCM_VM_INSN1(SCM_VM_PRE_TAIL, 1), xcode));
        } else {
            SCM_APPEND1(xcode, xtail, SCM_VM_INSN1(SCM_VM_CALL, 1));
            ADDCODE1(SCM_VM_INSN1(SCM_VM_PRE_CALL, 1));
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
        if (casep) ADDCODE1(SCM_VM_INSN(SCM_VM_POP));
        ADDCODE(compile_body(body, env, ctx, depth));
        ADDCODE(merger);
    }

    /* Rest of clauses. */
    SCM_APPEND(altcode, altcodetail,
               compile_cond_int(form, SCM_CDR(clauses),
                                merger, env, ctx, depth, casep));

    /* Emit test code. */
    if (casep) {
        ScmObj testcode = SCM_NIL, testtail;
        ScmObj h = SCM_NIL, t = SCM_NIL, tp;
        int testlen = Scm_Length(test);
        if (testlen < 0)
            Scm_Error("badly formed clause in case form: %S", clause);
        /* if this is a macro-expanded form, symbols in test may
           contain identifiers.  we replace it. */
        SCM_FOR_EACH(tp, test) {
            if (SCM_IDENTIFIERP(SCM_CAR(tp))) {
                SCM_APPEND1(h, t, SCM_OBJ(SCM_IDENTIFIER(SCM_CAR(tp))->name));
            } else {
                SCM_APPEND1(h, t, SCM_CAR(tp));
            }
        }
        test = h;
        
        /* the value of the key is on top of the stack.  */
        SCM_APPEND1(testcode, testtail, SCM_VM_INSN(SCM_VM_DUP));
        SCM_APPEND1(testcode, testtail, test);
        SCM_APPEND1(testcode, testtail, SCM_VM_INSN(SCM_VM_MEMV));
        test = testcode;
    } else {
        test = compile_int(test, env, SCM_COMPILE_NORMAL, depth);
    }
    
    return compile_if_family(test, code, altcode, FALSE, env, depth);
}


static ScmObj compile_cond(ScmObj form, ScmObj env, int ctx, int *depth, void *data)
{
    ScmObj clauses = SCM_CDR(form), merger;
    if (SCM_NULLP(clauses)) {
        Scm_Error("at least one clause is required for cond: %S", form);
    }
    merger = TAILP(ctx)? SCM_NIL:SCM_LIST1(SCM_VM_INSN(SCM_VM_MNOP));
    return compile_cond_int(form, clauses, merger, env, ctx, depth, FALSE);
}

static ScmSyntax syntax_cond = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_COND),
    compile_cond,
    NULL
};

static ScmObj compile_case(ScmObj form, ScmObj env, int ctx, int *depth, void *data)
{
    ScmObj tail = SCM_CDR(form), key, clauses, merger;
    ScmObj code = SCM_NIL, codetail = SCM_NIL;
    int nlen = Scm_Length(tail);
    if (nlen < 2) Scm_Error("bad case form: %S", form);
    key = SCM_CAR(tail);
    clauses = SCM_CDR(tail);

    ADDCODE(compile_int(key, env, SCM_COMPILE_NORMAL, depth));
    ADDPUSH();
    merger = TAILP(ctx)? SCM_NIL:SCM_LIST1(SCM_VM_INSN(SCM_VM_MNOP));
    ADDCODE(compile_cond_int(form, clauses, merger, env, ctx, depth, TRUE));
    return code;
}

static ScmSyntax syntax_case = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_CASE),
    compile_case,
    NULL
};

/*------------------------------------------------------------------
 * LET family (LET, LET*, LETREC)
 */

/* Common routine to compile a binding construct.   The compilation of
   body part is delegated to BODY_COMPILER function */
static ScmObj compile_let_family(ScmObj form, ScmObj vars, ScmObj vals,
                                 int nvars, int type, ScmObj body,
                                 ScmObj (*body_compiler)(ScmObj body,
                                                         ScmObj env,
                                                         int ctx,
                                                         int *depth),
                                 ScmObj env, int ctx, int *depth)
{
    ScmObj code = SCM_NIL, codetail = SCM_NIL;
    ScmObj cfr = SCM_NIL;  /* current frame */
    ScmObj newenv, varp, valp;
    int count = 0;

    if (type == BIND_LETREC) cfr = vars;
    else cfr = Scm_MakeList(nvars, SCM_UNDEFINED); /* dummy frame */
    newenv = Scm_Cons(cfr, env);

    for (count=0, varp=vars, valp=vals;
         count<nvars;
         count++, varp=SCM_CDR(varp), valp=SCM_CDR(valp)) {
        ScmObj val = compile_int(SCM_CAR(valp), newenv, SCM_COMPILE_NORMAL, depth);
        ADDCODE(val);
        ADDCODE1(make_lset(0, nvars-count-1));
            
        if (type == BIND_LET_STAR) {
            ScmObj p = Scm_ListTail(cfr, count);
            SCM_SET_CAR(p, SCM_CAR(varp));
        }
    }
    if (type == BIND_LET) newenv = Scm_Cons(vars, env);
    ADDCODE(body_compiler(body, newenv, ctx, depth));

    if (nvars > 0) {
        return add_bindinfo(add_srcinfo(SCM_LIST2(SCM_VM_INSN1(SCM_VM_LET, nvars), code), form), vars);
    } else {
        return add_srcinfo(code, form);
    }
}

static ScmObj compile_let(ScmObj form, ScmObj env, int ctx, int *depth, void *data)
{
    long type = (long)data;
    ScmObj tail = SCM_CDR(form);
    ScmObj bindings, body, vars, vals, name = SCM_FALSE;
    int nvars;

    if (!SCM_PAIRP(tail)) Scm_Error("syntax error: %S", form);
    bindings = SCM_CAR(tail);
    body = SCM_CDR(tail);

    /* Check named let */
    if (VAR_P(bindings)) {
        if (type != BIND_LET) Scm_Error("syntax error: %S", form);
        if (!SCM_PAIRP(body)) Scm_Error("badly formed named let: %S", form);
        name = bindings;
        bindings = SCM_CAR(body);
        body = SCM_CDR(body);
    }

    /* Check binding syntax */
    {
        ScmObj vars_p = SCM_NIL, vals_p = SCM_NIL, bind_p;

        vars = SCM_NIL;
        vals = SCM_NIL;
        nvars = 0;
        SCM_FOR_EACH(bind_p, bindings) {
            ScmObj binding = SCM_CAR(bind_p);

            if (!SCM_PAIRP(binding)
                || !LIST1_P(SCM_CDR(binding))
                || !VAR_P(SCM_CAR(binding))) {
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
                                  env, ctx, depth);
    } else {
        /* Named let. */
        static ScmObj compile_named_let_body(ScmObj, ScmObj, int, int *);
        /* TODO: this is broken if lambda is locally bound! */
        ScmObj proc = Scm_Cons(id_lambda, Scm_Cons(vars, body));
        return compile_let_family(form, SCM_LIST1(name), SCM_LIST1(proc),
                                  1, BIND_LETREC,
                                  Scm_Cons(env, Scm_Cons(name, vals)),
                                  compile_named_let_body, env, ctx, depth);
    }
}

static ScmObj compile_named_let_body(ScmObj body, ScmObj env, int ctx, int *depth)
{
    /* Trick: we need to compile initial values in the upper environment,
       while the "name" to be looked up in the new environment. */
    ScmObj oldenv = SCM_CAR(body);
    ScmObj name = SCM_CADR(body);
    ScmObj args = SCM_CDR(SCM_CDR(body));
    name = lookup_env(name, env, FALSE);
    return compile_body(SCM_LIST1(Scm_Cons(name, args)),
                        Scm_Cons(SCM_LIST1(SCM_UNDEFINED), oldenv),
                        ctx, depth);
}

static ScmSyntax syntax_let = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_LET),
    compile_let,
    (void*)BIND_LET
};

static ScmSyntax syntax_let_star = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_LET_STAR),
    compile_let,
    (void*)BIND_LET_STAR
};

static ScmSyntax syntax_letrec = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_LETREC),
    compile_let,
    (void*)BIND_LETREC
};

/*------------------------------------------------------------------
 * Loop construct (DO)
 */

static ScmObj compile_do(ScmObj form, ScmObj env, int ctx, int *depth, void *data)
{
    ScmObj binds, test, body, bp, testbody, newform;
    ScmObj vars = SCM_NIL, vars_tail = SCM_NIL;
    ScmObj inits = SCM_NIL, inits_tail = SCM_NIL;
    ScmObj updts = SCM_NIL, updts_tail = SCM_NIL;
    ScmObj do_id = Scm_MakeIdentifier(SCM_SYMBOL(SCM_SYM_DO), SCM_NIL);
    int nvars = 0;
    int flen = Scm_Length(form);
    if (flen < 3) Scm_Error("badly formed `do': %S", form);
    binds = SCM_CADR(form);
    test = SCM_CAR(SCM_CDDR(form));
    body = SCM_CDR(SCM_CDDR(form));

    SCM_FOR_EACH(bp, binds) {
        ScmObj bind = SCM_CAR(bp);
        int blen = Scm_Length(bind);
        if (!((blen >= 2) && (blen <= 3)) || !VAR_P(SCM_CAR(bind)))
            Scm_Error("bad binding form in `do': %S", form);
        SCM_APPEND1(vars, vars_tail, SCM_CAR(bind));
        SCM_APPEND1(inits, inits_tail, SCM_CADR(bind));
        SCM_APPEND1(updts, updts_tail,
                    (blen == 3)? SCM_CAR(SCM_CDDR(bind)) : SCM_CAR(bind));
        nvars++;
    }
    if (!SCM_NULLP(bp)) Scm_Error("badly formed `do': %S", form);

    if (Scm_Length(test) < 1) Scm_Error("bad test form in `do': %S", form);

    if (SCM_NULLP(SCM_CDR(test))) {
        testbody = SCM_UNDEFINED;
    } else {
        testbody = Scm_Cons(id_begin, SCM_CDR(test));
    }

    body = SCM_LIST4(id_if, SCM_CAR(test), testbody,
                     SCM_LIST3(id_begin, Scm_Cons(id_begin, body),
                               Scm_Cons(do_id, updts)));

    newform =
        SCM_LIST3(id_letrec,
                  SCM_LIST1(SCM_LIST2(do_id,
                                      SCM_LIST3(id_lambda, vars, body))),
                  Scm_Cons(do_id, inits));
    return compile_int(newform, env, ctx, depth);
}

static ScmSyntax syntax_do = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
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
    global_eq(obj, SCM_SYM_UNQUOTE, env)
#define UNQUOTE_SPLICING_P(obj, env) \
    global_eq(obj, SCM_SYM_UNQUOTE_SPLICING, env)
#define QUASIQUOTEP(obj, env) \
    global_eq(obj, SCM_SYM_QUASIQUOTE, env)

static ScmObj compile_qq_list(ScmObj form, ScmObj env, int level, int *depth);
static ScmObj compile_qq_vec(ScmObj form, ScmObj env, int level, int *depth);

static ScmObj compile_qq(ScmObj form, ScmObj env, int level, int *depth)
{
    if (!SCM_PTRP(form)) {
        return SCM_LIST1(form);
    } if (SCM_PAIRP(form)) {
        return compile_qq_list(form, env, level, depth);
    } else if (SCM_VECTORP(form)) {
        return compile_qq_vec(form, env, level, depth);
    } else {
        return SCM_LIST1(unwrap_identifier(form));
    }
}

static ScmObj compile_qq_list(ScmObj form, ScmObj env, int level, int *depth)
{
    int len = 0, splice = 0, last_spliced = FALSE, stacksize = 0;
    ScmObj car = SCM_CAR(form), cp;
    ScmObj code = SCM_NIL, codetail = SCM_NIL;

    if (UNQUOTEP(car, env)) {
        if (!VALID_QUOTE_SYNTAX_P(form))
            Scm_Error("badly formed unquote: %S\n", form);
        if (level == 0) {
            return compile_int(SCM_CADR(form), env, SCM_COMPILE_NORMAL, depth);
        } else {
            ADDCODE1(car);
            ADDPUSH();
            ADDCODE(compile_qq(SCM_CADR(form), env, level-1, depth));
            ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST, 2));
            return code;
        }
    } else if (UNQUOTE_SPLICING_P(car, env)) {
        Scm_Error("unquote-splicing appeared in invalid context: %S",
                  form);
        return SCM_NIL;     /* dummy */
    } else if (QUASIQUOTEP(car, env)) {
        if (!VALID_QUOTE_SYNTAX_P(form))
            Scm_Error("badly formed quasiquote: %S\n", form);
        ADDCODE1(car);
        ADDPUSH();
        ADDCODE(compile_qq(SCM_CADR(form), env, level+1, depth));
        ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST, 2));
        return code;
    }

    /* ordinary list */
    SCM_FOR_EACH(cp, form) {
        stacksize++;
        car = SCM_CAR(cp);
        if (UNQUOTEP(car, env)) {
            break;
        } else if (UNQUOTE_SPLICING_P(car, env)) {
            Scm_Error("unquote-splicing appeared in invalid context: %S",form);
        }
        if (SCM_PAIRP(car) && UNQUOTE_SPLICING_P(SCM_CAR(car), env)) {
            if (!VALID_QUOTE_SYNTAX_P(car))
                Scm_Error("badly formed quasiquote: %S\n", form);
            if (level == 0) {
                if (last_spliced) ADDPUSH();
                ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST, len));
                ADDPUSH();
                len = 0;
                ADDCODE(compile_int(SCM_CADR(car), env, SCM_COMPILE_NORMAL, depth));
                last_spliced = TRUE;
                splice+=2;
            } else {
                if (cp != form) ADDPUSH();
                ADDCODE1(SCM_CAR(car));
                ADDPUSH();
                ADDCODE(compile_qq(SCM_CADR(car), env, level-1, depth));
                ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST, 2));
                len++;
            }
        } else {
            if (cp != form) ADDPUSH();
            ADDCODE(compile_qq(SCM_CAR(cp), env, level, depth));
            last_spliced = FALSE;
            len++;
        }
    }
    if (!SCM_NULLP(cp)) {
        ADDPUSH();
        ADDCODE(compile_qq(cp, env, level, depth));
        ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST_STAR, len+1));
    } else {
        if (last_spliced) ADDPUSH();
        ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST, len));
    }
    if (splice) {
        ADDCODE1(SCM_VM_INSN1(SCM_VM_APPEND, splice+1));
    }
    return code;
}

static ScmObj compile_qq_vec(ScmObj form, ScmObj env, int level, int *depth)
{
    ScmObj code = SCM_NIL, codetail = SCM_NIL;
    int vlen = SCM_VECTOR_SIZE(form), i, alen = 0;
    int spliced = 0, last_spliced = FALSE;

    for (i=0; i<vlen; i++) {
        ScmObj p = SCM_VECTOR_ELEMENT(form, i);
        if (SCM_PAIRP(p)) {
            ScmObj car = SCM_CAR(p);
            if (UNQUOTEP(car, env)) {
                if (!VALID_QUOTE_SYNTAX_P(p))
                    Scm_Error("badly formed unquote: %S\n", p);
                if (i > 0) ADDPUSH();
                if (level == 0) {
                    ADDCODE(compile_int(SCM_CADR(p), env, SCM_COMPILE_NORMAL, depth));
                } else {
                    ADDCODE1(car);
                    ADDPUSH();
                    ADDCODE(compile_qq(SCM_CADR(p), env, level-1, depth));
                    ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST, 2));
                }
                last_spliced = FALSE;
                alen++;
            } else if (UNQUOTE_SPLICING_P(car, env)) {
                if (!VALID_QUOTE_SYNTAX_P(p))
                    Scm_Error("badly formed unquote-splicing: %S\n", form);
                if (level == 0) {
                    if (last_spliced) ADDPUSH();
                    ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST, alen));
                    ADDPUSH();
                    alen = 0;
                    ADDCODE(compile_int(SCM_CADR(p), env, SCM_COMPILE_NORMAL, depth));
                    last_spliced = TRUE;
                    spliced+=2;
                } else {
                    if (i > 0) ADDPUSH();
                    ADDCODE1(car);
                    ADDPUSH();
                    ADDCODE(compile_qq(SCM_CADR(p), env, level-1, depth));
                    ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST, 2));
                    alen++;
                }
            } else if (QUASIQUOTEP(car, env)) {
                if (!VALID_QUOTE_SYNTAX_P(p))
                    Scm_Error("badly formed quasiquote: %S\n", form);
                if (i > 0) ADDPUSH();
                ADDCODE1(car);
                ADDPUSH();
                ADDCODE(compile_qq(SCM_CADR(p), env, level+1, depth));
                ADDCODE1(SCM_VM_INSN1(SCM_VM_LIST, 2));
                last_spliced = FALSE;
                alen++;
            } else {
                if (i > 0) ADDPUSH();
                ADDCODE1(unwrap_identifier(p));
                last_spliced = FALSE;
                alen++;
            }
        } else {
            if (i > 0) ADDPUSH();
            ADDCODE1(unwrap_identifier(p));
            last_spliced = FALSE;
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

static ScmObj compile_quasiquote(ScmObj form, ScmObj env, int ctx, int *depth,
                                 void *data)
{
    if (!VALID_QUOTE_SYNTAX_P(form))
        Scm_Error("badly formed quasiquote: %S\n", form);
    return compile_qq(SCM_CADR(form), env, 0, depth);
}

static ScmObj compile_unquote(ScmObj form, ScmObj env, int ctx, int *depth,
                              void *data)
{
    const char *name = (const char *)data;
    Scm_Error("%s appeared outside corresponding quasiquote: %S", name, form);
    return SCM_NIL;             /* dummy */
}

static ScmSyntax syntax_quasiquote = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_QUASIQUOTE),
    compile_quasiquote,
    NULL
};

static ScmSyntax syntax_unquote = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_UNQUOTE),
    compile_unquote,
    "unquote"
};

static ScmSyntax syntax_unquote_splicing = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_UNQUOTE_SPLICING),
    compile_unquote,
    "unquote-splicing"
};

/*------------------------------------------------------------------
 * Delay
 */

static ScmObj compile_delay(ScmObj form, ScmObj env, int ctx, int *depth,
                            void *data)
{
    ScmObj code = SCM_NIL, codetail = SCM_NIL;
    
    if (!LIST1_P(SCM_CDR(form))) Scm_Error("bad delay form: %S", form);
    ADDCODE(compile_int(SCM_LIST3(id_lambda,
                                  SCM_NIL,
                                  SCM_CADR(form)),
                        env, SCM_COMPILE_NORMAL, depth));
    ADDCODE1(SCM_VM_INSN(SCM_VM_PROMISE));
    return code;
}

static ScmSyntax syntax_delay = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_DELAY),
    compile_delay,
    NULL
};

/*------------------------------------------------------------------
 * Receive
 */

static ScmObj compile_receive(ScmObj form, ScmObj env, int ctx, int *depth,
                              void *data)
{
    ScmObj code = SCM_NIL, codetail = SCM_NIL, vars, expr, body;
    ScmObj bind = SCM_NIL, bindtail = SCM_NIL, vp;
    int nvars, restvars;
    
    if (Scm_Length(form) < 4) Scm_Error("badly formed receive: %S", form);
    vars = SCM_CADR(form);
    expr = SCM_CAR(SCM_CDDR(form));
    body = SCM_CDR(SCM_CDDR(form));

    nvars = restvars = 0;
    SCM_FOR_EACH(vp, vars) {
        if (!VAR_P(SCM_CAR(vp))) Scm_Error("badly formed receive: %S", form);
        nvars++;
        SCM_APPEND1(bind, bindtail, SCM_CAR(vp));
    }
    if (!SCM_NULLP(vp)) { restvars=1; SCM_APPEND1(bind, bindtail, vp); }
    
    ADDCODE(compile_int(expr, env, SCM_COMPILE_NORMAL, depth));
    ADDCODE(add_bindinfo(SCM_LIST1(SCM_VM_INSN2(SCM_VM_VALUES_BIND, nvars, restvars)),
                         vars));
    ADDCODE1(compile_body(body, Scm_Cons(bind, env), ctx, depth));
    return code;
}

static ScmSyntax syntax_receive = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_RECEIVE),
    compile_receive,
    NULL
};

/*------------------------------------------------------------------
 * Module related routines
 */

static ScmObj compile_with_module(ScmObj form, ScmObj env, int ctx, int *depth,
                                  void *data)
{
    ScmObj modname, module;
    int createp = (data != NULL);
    volatile ScmObj body, code = SCM_NIL, codetail = SCM_NIL;
    volatile ScmModule *current;

    if (Scm_Length(form) < 2) Scm_Error("syntax error: %S", form);
    modname = SCM_CADR(form);
    body = SCM_CDDR(form);
    if (SCM_IDENTIFIERP(modname)) {
        modname = SCM_OBJ(SCM_IDENTIFIER(modname)->name);
    } else if (!SCM_SYMBOLP(modname)) {
        Scm_Error("with-module: bad module name: %S", modname);
    }
    module = Scm_FindModule(SCM_SYMBOL(modname), createp);
    if (!SCM_MODULEP(module)) {
        Scm_Error("with-module: no such module: %S", modname);
    }
    /* TODO: insert source-info */
    current = Scm_CurrentModule();
    SCM_UNWIND_PROTECT {
        Scm_SelectModule(SCM_MODULE(module));
        SCM_FOR_EACH(body, body) {
            ADDCODE(compile_int(SCM_CAR(body), env,
                                SCM_NULLP(SCM_CDR(body))?
                                ctx : SCM_COMPILE_STMT, depth));
        }
    }
    SCM_WHEN_ERROR {
        Scm_SelectModule(SCM_MODULE(current));
        SCM_NEXT_HANDLER;
    }
    SCM_END_PROTECT;
    Scm_SelectModule(SCM_MODULE(current));

    /* if the body is empty, just return the module itself. */
    if (SCM_NULLP(code)) ADDCODE1(module);
    return code;
}

static ScmSyntax syntax_with_module = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_WITH_MODULE),
    compile_with_module,
    (void*)0
};

static ScmSyntax syntax_define_module = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_DEFINE_MODULE),
    compile_with_module,
    (void*)1
};

static ScmObj compile_select_module(ScmObj form, ScmObj env, int ctx,
                                    int *depth, void *data)
{
    ScmObj modname, module;
    if (Scm_Length(form) != 2) Scm_Error("syntax error: %S", form);
    modname = SCM_CADR(form);
    if (!SCM_SYMBOLP(modname))
        Scm_Error("select-module: bad module name: %S", modname);
    module = Scm_FindModule(SCM_SYMBOL(modname), FALSE);
    if (!SCM_MODULEP(module))
        Scm_Error("select-module: no such module: %S", modname);
    Scm_SelectModule(SCM_MODULE(module));
    /* TODO: insert source-info */
    return SCM_LIST1(module);
}

static ScmSyntax syntax_select_module = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_SELECT_MODULE),
    compile_select_module,
    NULL
};

static ScmObj compile_current_module(ScmObj form, ScmObj env, int ctx,
                                     int *depth, void *data)
{
    if (Scm_Length(form) != 1) Scm_Error("syntax error: %S", form);
    return SCM_LIST1(SCM_OBJ(SCM_CURRENT_MODULE()));
}

static ScmSyntax syntax_current_module = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_CURRENT_MODULE),
    compile_current_module,
    NULL
};

static ScmObj compile_import(ScmObj form, ScmObj env, int ctx,
                             int *depth, void *data)
{
    ScmObj m = Scm_ImportModules(SCM_CURRENT_MODULE(), SCM_CDR(form));
    return SCM_LIST1(m);
}

static ScmSyntax syntax_import = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_IMPORT),
    compile_import,
    NULL
};

static ScmObj compile_export(ScmObj form, ScmObj env, int ctx,
                             int *depth, void *data)
{
    ScmObj m = Scm_ExportSymbols(SCM_CURRENT_MODULE(), SCM_CDR(form));
    return SCM_LIST1(m);
}

static ScmSyntax syntax_export = {
    { SCM_CLASS_STATIC_PTR(Scm_SyntaxClass) },
    SCM_SYMBOL(SCM_SYM_EXPORT),
    compile_export,
    NULL
};

/*------------------------------------------------------------------
 * Inlining routines
 *   These routine are called from genstub-generated C code at
 *   compile time to generate a code to inline it.
 */
ScmObj Scm_CompileInliner(ScmObj form, ScmObj env,
                          int reqargs, int optargs, int insn, char *proc,
                          int *depth)
{
    ScmObj cp = SCM_CDR(form);
    ScmObj code = SCM_NIL, codetail = SCM_NIL;
    int nargs = Scm_Length(cp);
    if (optargs) {
        if (0 < reqargs && nargs < reqargs) {
            Scm_Error("%s requires at least %d arg(s)", proc, reqargs);
        }
    } else {
        if (nargs != reqargs) {
            Scm_Error("%s requires exactly %d arg(s)", proc, reqargs);
        }
    }
    SCM_FOR_EACH(cp, cp) {
        ADDCODE(compile_int(SCM_CAR(cp), env, SCM_COMPILE_NORMAL, depth));
        if (SCM_PAIRP(SCM_CDR(cp))) ADDPUSH();
    }
    if (optargs) {
        ADDCODE1(SCM_VM_INSN1(insn, nargs));
    } else {
        ADDCODE1(SCM_VM_INSN(insn));
    }
    return code;
}

/*===================================================================
 * Code vectorization
 */

#ifdef GAUCHE_NVM_VECTORIZATION
/* This is the first step of moving to the New VM architecture, which
   uses a vector instead of a list for code.  In this transitional stage,
   we use the old compiler to generate a directed graph, then call this
   routine to make a code vector out of it.  Later, the whole compiler
   passes will be reorganized for more efficient compilation.

   Note that the constant values are embedded in the code vector; we
   still need the constant vector, but that is only to protect heap-
   allocated constants from GC; the code vector itself is allocated from
   the atomic heap to reduce the scanning overhead of GC.

   Vectorization pass takes DG from the first pass, and creates a code
   vector and a constant vector.

   The first pass of vectorization is a serialization pass, where DG
   is turned into a list, inserting JUMP instructions.  Several peephole
   optimizations are done in this stage, too.  During the serialization
   pass, instructions are counted and the jump target cells (MNOPs) are
   associated with its address.

   Once the serialized form is obtained, a code vector is allocated and
   the instructions are packed into it.  Since we already know the
   jump target address, we don't need to back up to fill them in.
*/

typedef struct v_context_rec {
    int numCodes;               /* # of instructions */
    ScmObj targets;             /* assoc list of jump targets and addresses */
    ScmObj constants;           /* list of heap-allocated constants */
} v_context;

static ScmObj vectorize_rec(ScmObj form, v_context *ctx)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    while (!SCM_NULLP(form)) {
        ScmObj ca = SCM_CAR(form);
        int code;
        
        if (!SCM_VM_INSNP(ca)) {
            /* Constant */
            SCM_APPEND1(h, t, ca);
            if (SCM_PTRP(ca) && SCM_FALSEP(Scm_Memq(ctx->constants, ca))) {
                ctx->constants = Scm_Cons(ca, ctx->constants);
            }
            continue;
        }

        code = SCM_VM_INSN_CODE(ca);
        switch (code) {
        case SCM_VM_NOP: break; /* We omit NOP */
        case SCM_VM_MNOP: /* we reached merging point */
            break;
        case SCM_VM_PUSH:;
        case SCM_VM_POP:;
        case SCM_VM_DUP:;
            
        case SCM_VM_PRE_CALL:;
        case SCM_VM_PRE_TAIL:;
        case SCM_VM_CHECK_STACK:;
        case SCM_VM_CALL:;
        case SCM_VM_TAIL_CALL:;
        case SCM_VM_JUMP:;
        case SCM_VM_DEFINE:;
        case SCM_VM_DEFINE_CONST:;
        case SCM_VM_LAMBDA:;
        case SCM_VM_LET:;
        case SCM_VM_IF:;
        case SCM_VM_VALUES_BIND:;
        case SCM_VM_LSET:;
        case SCM_VM_GSET:;
        case SCM_VM_LREF:;
        case SCM_VM_GREF:;
        case SCM_VM_PROMISE:;
        case SCM_VM_QUOTE_INSN:;
        case SCM_VM_CONS:;
        case SCM_VM_CAR:;
        case SCM_VM_CDR:;
        case SCM_VM_LIST:;
        case SCM_VM_LIST_STAR:;
        case SCM_VM_MEMQ:;
        case SCM_VM_MEMV:;
        case SCM_VM_ASSQ:;
        case SCM_VM_ASSV:;
        case SCM_VM_EQ:;
        case SCM_VM_EQV:;
        case SCM_VM_APPEND:;
        case SCM_VM_NOT:;
        case SCM_VM_REVERSE:;
        case SCM_VM_APPLY:;
        case SCM_VM_NULLP:;
        case SCM_VM_PAIRP:;
        case SCM_VM_CHARP:;
        case SCM_VM_EOFP:;
            
        case SCM_VM_STRINGP:;
        case SCM_VM_SYMBOLP:;
        case SCM_VM_SETTER:;
        case SCM_VM_VALUES:;
        case SCM_VM_VEC:;
        case SCM_VM_APP_VEC:;
        case SCM_VM_VEC_LEN:;
        case SCM_VM_VEC_REF:;
        case SCM_VM_VEC_SET:;
        case SCM_VM_NUMEQ2:;
        case SCM_VM_NUMLT2:;
        case SCM_VM_NUMLE2:;
        case SCM_VM_NUMGT2:;
        case SCM_VM_NUMGE2:;
        case SCM_VM_NUMADD2:;
        case SCM_VM_NUMSUB2:;
        case SCM_VM_NUMADDI:;
        case SCM_VM_NUMSUBI:;
        case SCM_VM_READ_CHAR:;
        case SCM_VM_WRITE_CHAR:;
        case SCM_VM_SLOT_REF:;
        case SCM_VM_SLOT_SET:;
        }
    }
}

#endif /*GAUCHE_NVM_VECTORIZATION*/

/*===================================================================
 * Initializer
 */

void Scm__InitCompiler(void)
{
    ScmModule *n = SCM_MODULE(Scm_NullModule());   /* for r5rs syntax */
    ScmModule *g = SCM_MODULE(Scm_GaucheModule()); /* for gauche syntax */

#define DEFSYN_N(symbol, syntax) \
    Scm_Define(n, SCM_SYMBOL(symbol), SCM_OBJ(&syntax))
#define DEFSYN_G(symbol, syntax) \
    Scm_Define(g, SCM_SYMBOL(symbol), SCM_OBJ(&syntax))

    DEFSYN_N(SCM_SYM_DEFINE,       syntax_define);
    DEFSYN_G(SCM_SYM_DEFINE_CONSTANT,  syntax_define_constant);
    DEFSYN_G(SCM_SYM_DEFINE_IN_MODULE, syntax_define_in_module);
    DEFSYN_N(SCM_SYM_QUOTE,        syntax_quote);
    DEFSYN_N(SCM_SYM_QUASIQUOTE,   syntax_quasiquote);
    DEFSYN_N(SCM_SYM_UNQUOTE,      syntax_unquote);
    DEFSYN_N(SCM_SYM_UNQUOTE_SPLICING, syntax_unquote_splicing);
    DEFSYN_N(SCM_SYM_SET,          syntax_set);
    DEFSYN_N(SCM_SYM_LAMBDA,       syntax_lambda);
    DEFSYN_N(SCM_SYM_BEGIN,        syntax_begin);
    DEFSYN_N(SCM_SYM_IF,           syntax_if);
    DEFSYN_G(SCM_SYM_WHEN,         syntax_when);
    DEFSYN_G(SCM_SYM_UNLESS,       syntax_unless);
    DEFSYN_N(SCM_SYM_AND,          syntax_and);
    DEFSYN_N(SCM_SYM_OR,           syntax_or);
    DEFSYN_N(SCM_SYM_COND,         syntax_cond);
    DEFSYN_N(SCM_SYM_CASE,         syntax_case);
    DEFSYN_N(SCM_SYM_LET,          syntax_let);
    DEFSYN_N(SCM_SYM_LET_STAR,     syntax_let_star);
    DEFSYN_N(SCM_SYM_LETREC,       syntax_letrec);
    DEFSYN_N(SCM_SYM_DO,           syntax_do);
    DEFSYN_N(SCM_SYM_DELAY,        syntax_delay);
    DEFSYN_G(SCM_SYM_RECEIVE,      syntax_receive);
    DEFSYN_G(SCM_SYM_DEFINE_MODULE, syntax_define_module);
    DEFSYN_G(SCM_SYM_WITH_MODULE,  syntax_with_module);
    DEFSYN_G(SCM_SYM_SELECT_MODULE, syntax_select_module);
    DEFSYN_G(SCM_SYM_CURRENT_MODULE, syntax_current_module);
    DEFSYN_G(SCM_SYM_IMPORT,       syntax_import);
    DEFSYN_G(SCM_SYM_EXPORT,       syntax_export);

    id_lambda = Scm_MakeIdentifier(SCM_SYMBOL(SCM_SYM_LAMBDA), SCM_NIL);
    id_if = Scm_MakeIdentifier(SCM_SYMBOL(SCM_SYM_IF), SCM_NIL);
    id_begin = Scm_MakeIdentifier(SCM_SYMBOL(SCM_SYM_BEGIN), SCM_NIL);
    id_letrec = Scm_MakeIdentifier(SCM_SYMBOL(SCM_SYM_LETREC), SCM_NIL);
}

