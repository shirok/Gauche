/*
 * vm.c - evaluator
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
 *  $Id: vm.c,v 1.2 2001-01-12 11:33:38 shiro Exp $
 */

#include "gauche.h"

/*
 * The VM.
 *
 *   VM encapsulates the dynamic status of the current exection.
 *   In Gauche, there's always one active virtual machine per thread,
 *   refered by Scm_VM().
 *
 */

static int vm_print(ScmObj obj, ScmPort *out, int mode)
{
    ScmVM *v = SCM_VM(obj);
    return Scm_Printf(out, "#<vm %p>", v);
}

static ScmClass *top_cpl[] = { SCM_CLASS_TOP, NULL };

ScmClass Scm_VMClass = {
    SCM_CLASS_CLASS,
    "<vm>",
    vm_print,
    top_cpl
};

static ScmVM *theVM;    /* this must be thread specific in MT version */

/* base can be NULL iff called from Scm_Init. */

static ScmEnvFrame *topenv(ScmModule *module);

ScmVM *Scm_NewVM(ScmVM *base,
                 ScmModule *module)
{
    ScmVM *v = SCM_NEW(ScmVM);
    v->hdr.klass = &Scm_VMClass;
    v->parent = base;
    v->module = module ? module : base->module;
    v->escape = base ? base->escape : NULL;
    v->errstr = SCM_FALSE;

    v->curin  = SCM_PORT(Scm_Stdin());
    v->curout = SCM_PORT(Scm_Stdout());
    v->curerr = SCM_PORT(Scm_Stderr());

    v->debugLevel = SCM_VM_DEBUG_DEFAULT;

    /* initial frame */
    v->env = topenv(v->module);
        
    v->cont = NULL;
    v->argp = SCM_NIL;
    v->pc = SCM_NIL;

    v->handlers = SCM_NIL;
    
    return v;
}

static void vm_reset()
{
    theVM->errstr = SCM_FALSE;
    theVM->env = topenv(theVM->module);
    theVM->cont = NULL;
    theVM->argp = SCM_NIL;
    theVM->pc = SCM_NIL;
}

ScmObj Scm_VMGetResult(ScmVM *vm)
{
    return Scm_Reverse(vm->argp);
}

ScmObj Scm_VMGetErrorString(ScmVM *vm)
{
    return vm->errstr;
}

/*
 * Current VM.
 */
ScmVM *Scm_VM(void)
{
    return theVM;
}

ScmVM *Scm_SetVM(ScmVM *vm)
{
    ScmVM *prev = theVM;
    theVM = vm;
    return prev;
}

static ScmEnvFrame *topenv(ScmModule *module)
{
    ScmEnvFrame *e = SCM_NEW(ScmEnvFrame);
    e->up = NULL;
    e->info = SCM_MAKE_STR("[toplevel]");
    e->size = 1;
    e->data[0] = SCM_OBJ(module);
    return e;
}

static ScmEnvFrame *newenv(ScmObj info, int nlocals, ScmEnvFrame *up)
{
    ScmEnvFrame *e =
        SCM_NEW2(ScmEnvFrame*,
                 sizeof(ScmEnvFrame) + (nlocals-1)*sizeof(ScmObj));
    e->up = up;
    e->info = info;
    e->size = nlocals;
    return e;
}

static ScmContFrame *get_continuation(ScmObj next)
{
    ScmContFrame *c = SCM_NEW(ScmContFrame);
    c->prev = theVM->cont;
    c->env = theVM->env;
    c->argp = theVM->argp;
    c->next = next;
    return c;
}

static void push_continuation(ScmObj next)
{
    ScmContFrame *c = get_continuation(next);
    theVM->cont = c;
}

/*====================================================================
 * Scm_Run - VM interpreter
 *
 *  Interprets intermediate code CODE on VM.
 */

#define PUSH_ARG(obj) (vm->argp = Scm_Cons(obj, vm->argp))
#define POP_ARG(var)  (var = SCM_CAR(vm->argp), vm->argp = SCM_CDR(vm->argp))
#define PEEK_ARG(var) (var = SCM_CAR(vm->argp))
#define DISCARD_ARG() (vm->argp = SCM_CDR(vm->argp))

#define CHECK_ARGCNT(cnt)  SCM_ASSERT(Scm_Length(vm->argp) >= (cnt))

static void run_loop(ScmObj);

void Scm_Run(ScmObj program)
{
    vm_reset();
    run_loop(program);
}
    
static void run_loop(ScmObj program)
{
    register ScmObj pc = program, code;
    register ScmVM *vm = theVM;
    
    for (;;) {
        if (!SCM_PAIRP(pc)) {
            /* We are at the end of procedure.  Activate the most recent
               continuation. */

            if (vm->cont == NULL) return; /* no more continuations */
            vm->env = vm->cont->env;
            pc = vm->cont->next;
            vm->cont = vm->cont->prev;
            continue;
        }
        
        code = SCM_CAR(pc);
        pc = SCM_CDR(pc);
        
        if (!SCM_VM_INSNP(code)) {
            if (SCM_CCONTP(code)) {
                /* C-continuation */
                ScmObj result;
                ScmCContinuation *cc = SCM_CCONT(code);
                if (!SCM_NULLP(vm->argp)) {
                    POP_ARG(result);
                } else {
                    result = SCM_UNDEFINED;
                }
                vm->pc = pc;
                cc->func(result, cc->data);
                pc = vm->pc;
            } else if (SCM_SOURCE_INFOP(code)) {
                /* source info.  ignore it. */
                continue;
            } else {
                /* Literal object */
                PUSH_ARG(code); 
            }
            continue;
        }
        switch(SCM_VM_INSN_CODE(code)) {
        case SCM_VM_DEFINE:
            {
                ScmObj var, val;

                SCM_ASSERT(SCM_PAIRP(pc));
                var = SCM_CAR(pc);
                SCM_ASSERT(SCM_SYMBOLP(var));
                pc = SCM_CDR(pc);
                CHECK_ARGCNT(1);
                POP_ARG(val);
                Scm_Define(vm->module, SCM_SYMBOL(var), val);
                PUSH_ARG(var);
                continue;
            }
        case SCM_VM_GREF:
            {
                ScmObj sym, val;

                SCM_ASSERT(SCM_PAIRP(pc));
                sym = SCM_CAR(pc);

                if (SCM_GLOCP(sym)) {
                    val = SCM_GLOC(sym)->value;
                    if (val == SCM_UNBOUND) {
                        vm->pc = SCM_CDR(pc);
                        Scm_Error("unbound variable: %S",
                                  SCM_OBJ(SCM_GLOC(sym)->name));
                    }
                } else if (SCM_SYMBOLP(sym)) {
                    ScmGloc *gloc =
                        Scm_FindBinding(vm->module, SCM_SYMBOL(sym), 0);
                    if (gloc == NULL) {
                        vm->pc = SCM_CDR(pc);
                        Scm_Error("unbound variable: %S", sym);
                    }
                    val = gloc->value;
                    if (val == SCM_UNBOUND) {
                        vm->pc = SCM_CDR(pc);
                        Scm_Error("unbound variable: %S", sym);
                    }
                    /* memorize gloc */
                    SCM_SET_CAR(pc, SCM_OBJ(gloc));
                }
                pc = SCM_CDR(pc);
                PUSH_ARG(val);
                continue;
            }
        case SCM_VM_LREF:
            {
                int off = SCM_VM_LREF_OFFSET(code);
                int dep = SCM_VM_LREF_DEPTH(code);
                ScmEnvFrame *e = vm->env;
                
                while (dep-- > 0) {
                    SCM_ASSERT(e != NULL);
                    e = e->up;
                }
                SCM_ASSERT(e != NULL);
                    /*DBG*/
                    if (e->size <= off) {
                        printf("e->size = %d, off = %d\n", e->size, off);
                        Scm_VMDump(vm);
                    }
                SCM_ASSERT(e->size > off);
                PUSH_ARG(e->data[off]);
                continue;
            }
        case SCM_VM_CALL:
            {
                int nargs = SCM_VM_CALL_NARGS(code);
                int nrets = SCM_VM_CALL_NRETS(code);
                int i, reqargs, restarg, argcnt;
                ScmObj proc, a;
                ScmEnvFrame *e;

                CHECK_ARGCNT(nargs+1);
                POP_ARG(proc);

                if (!SCM_PROCEDUREP(proc)) {
                    vm->pc = pc;
                    Scm_Error("bad procedure: %S", proc);
                }

                reqargs = SCM_PROCEDURE_REQUIRED(proc);
                restarg = SCM_PROCEDURE_OPTIONAL(proc);
                argcnt  = reqargs + (restarg? 1 : 0);
                e = newenv(SCM_PROCEDURE_INFO(proc), argcnt, NULL);

                if (restarg) {
                    ScmObj rest = SCM_NIL;

                    if (nargs < reqargs) {
                        vm->pc = pc;
                        Scm_Error("wrong number of arguments for %S (required %d, got %d)",
                                  proc, reqargs, nargs);
                    }
                    for (i=nargs-1; i>=reqargs; i--) {
                        POP_ARG(a);
                        rest = Scm_Cons(a, rest);
                    }
                    e->data[reqargs] = rest;
                    for (; i>=0; i--) {
                        POP_ARG(a);
                        e->data[i] = a;
                    }
                } else {
                    if (nargs != reqargs) {
                        vm->pc = pc;
                        Scm_Error("wrong number of arguments for %S (required %d, got %d)",
                                  proc, reqargs, nargs);
                    }
                    for (i=nargs-1; i>=0; i--) {
                        POP_ARG(a);
                        e->data[i] = a;
                    }
                }

                if (SCM_SUBRP(proc)) {
                    /* SUBR may push C-Continuation, so we need to save
                       current pc. */
                    vm->pc = pc;
                    SCM_SUBR(proc)->func(e->data, argcnt,
                                         SCM_SUBR(proc)->data);
                    pc = vm->pc;
                } else {
                    if (nrets == SCM_VM_NRETS_UNKNOWN) {
                        /* This is a tail call.  We don't need to save
                           current continuation, rather the callee to
                           use it. */
                        vm->pc = pc;
                    } else {
                        push_continuation(pc);
                    }
                    e->up = SCM_CLOSURE(proc)->env;
                    vm->env = e;
                    pc = SCM_CLOSURE(proc)->code;
                }
                continue;
            }
        case SCM_VM_LET:
            {
                int nlocals = SCM_VM_LET_NLOCALS(code);
                ScmEnvFrame *e;
                ScmObj info;

                SCM_ASSERT(SCM_PAIRP(pc));
                info = SCM_CAR(pc);
                pc = SCM_CDR(pc);
                
                e = newenv(info, nlocals, vm->env);
                vm->env = e;
                continue;
            }
        case SCM_VM_POPENV:
            {
                SCM_ASSERT(vm->env != NULL);
                vm->env = vm->env->up;
                continue;
            }
        case SCM_VM_POPARG:
            {
                SCM_ASSERT(SCM_PAIRP(vm->argp));
                DISCARD_ARG();
                continue;
            }
        case SCM_VM_DUPARG:
            {
                ScmObj arg;
                CHECK_ARGCNT(1);
                PEEK_ARG(arg);
                PUSH_ARG(arg);
                continue;
            }
        case SCM_VM_SET:
            {
                ScmObj location, val;
                SCM_ASSERT(SCM_PAIRP(pc));
                CHECK_ARGCNT(1);
                
                location = SCM_CAR(pc);
                POP_ARG(val);
                if (SCM_GLOCP(location)) {
                    SCM_GLOC(location)->value = val;
                } else if (SCM_VM_INSNP(location)
                           && SCM_VM_INSN_CODE(location) == SCM_VM_LREF) {
                    ScmEnvFrame *e = vm->env;
                    int off = SCM_VM_LREF_OFFSET(location);
                    int dep = SCM_VM_LREF_DEPTH(location);
                    
                    while (dep-- > 0) {
                        SCM_ASSERT(e != NULL);
                        e = e->up;
                    }
                    SCM_ASSERT(e != NULL);
                    SCM_ASSERT(e->size > off);
                    e->data[off] = val;
                } else if (SCM_SYMBOLP(location)) {
                    ScmGloc *gloc = Scm_FindBinding(vm->module,
                                                    SCM_SYMBOL(location), 1);
                    if (gloc == NULL) {
                        vm->pc = SCM_CDR(pc);
                        Scm_Error("symbol not defined: %S", location);
                    }
                    gloc->value = val;
                    /* memorize looked up gloc */
                    SCM_SET_CAR(pc, SCM_OBJ(gloc));
                } else {
                    Scm_Panic("SET instruction got invalid operand");
                }
                pc = SCM_CDR(pc);
                continue;
            }
        case SCM_VM_NOP:
            {
                continue;
            }
        case SCM_VM_IF:
            {
                ScmObj test;
                CHECK_ARGCNT(1);
                POP_ARG(test);
                if (SCM_FALSEP(test)) { pc = SCM_CDR(pc); }
                else                  { pc = SCM_CAR(pc); }
                continue;
            }
        case SCM_VM_IFNP:
            {
                ScmObj test;
                CHECK_ARGCNT(1);
                PEEK_ARG(test);
                if (SCM_FALSEP(test)) { pc = SCM_CDR(pc); }
                else                  { pc = SCM_CAR(pc); }
                continue;
            }
        case SCM_VM_LAMBDA:
            {
                ScmObj info, body, closure;
                
                SCM_ASSERT(SCM_PAIRP(pc));
                info = SCM_CAR(pc);
                pc = SCM_CDR(pc);
                SCM_ASSERT(SCM_PAIRP(pc));
                body = SCM_CAR(pc);
                pc = SCM_CDR(pc);

                closure = Scm_MakeClosure(SCM_VM_LAMBDA_NARGS(code),
                                          SCM_VM_LAMBDA_RESTARG(code),
                                          body, vm->env, info);
                PUSH_ARG(closure);
                continue;
            }
        case SCM_VM_CONS:
            {
                ScmObj car, cdr;
                CHECK_ARGCNT(2);
                POP_ARG(cdr);
                POP_ARG(car);
                PUSH_ARG(Scm_Cons(car, cdr));
                continue;
            }
        case SCM_VM_CAR:
            {
                ScmObj pair;
                CHECK_ARGCNT(1);
                POP_ARG(pair);
                PUSH_ARG(SCM_CAR(pair));
                continue;
            }
        case SCM_VM_CDR:
            {
                ScmObj pair;
                CHECK_ARGCNT(1);
                POP_ARG(pair);
                PUSH_ARG(SCM_CDR(pair));
                continue;
            }
        case SCM_VM_EQ:
            {
                ScmObj a, b;
                CHECK_ARGCNT(2);
                POP_ARG(b);
                POP_ARG(a);
                PUSH_ARG(SCM_MAKE_BOOL(a==b));
                continue;
            }
        case SCM_VM_EQV:
            {
                ScmObj a, b;
                CHECK_ARGCNT(2);
                POP_ARG(b);
                POP_ARG(a);
                PUSH_ARG(Scm_EqvP(a, b));
                continue;
            }
        case SCM_VM_MEMV:
            {
                ScmObj elt, list;
                CHECK_ARGCNT(2);
                POP_ARG(list);
                POP_ARG(elt);
                PUSH_ARG(Scm_Memv(elt, list));
                continue;
            }
        default:
            {
                Scm_Panic("Illegal instruction: %08x",
                          SCM_VM_INSN_CODE(code));
            }
        }
    }
}

/*
 * C-continuations.  Arrange c-function to be called.
 */

static int cc_print(ScmObj obj, ScmPort *port, int mode)
{
    return Scm_Printf(port, "#<c-continuation %p>", obj);
}

ScmClass Scm_CContClass = {
    SCM_CLASS_CLASS,
    "<c-continuation>",
    cc_print,
    top_cpl
};

void Scm_VMPushCC(void (*func)(ScmObj result, void **data),
                  void **data,
                  int datasize)
{
    int i = 0;
    ScmCContinuation *cc = SCM_NEW(ScmCContinuation);
    
    cc->hdr.klass = &Scm_CContClass;
    cc->func = func;

    for (i=0; i<datasize && i<SCM_CCONT_DATA_SIZE; i++) {
        cc->data[i] = data[i];
    }
    theVM->pc = Scm_Cons(SCM_OBJ(cc), theVM->pc);
}

/*==================================================================
 * Function application.
 */

void Scm_Apply0(ScmObj proc)
{
    ScmVM *vm = theVM;
    if (!SCM_PROCEDUREP(proc))
        Scm_Error("procedure required, but got %S", proc);
    PUSH_ARG(proc);
    vm->pc = Scm_Cons(SCM_VM_MAKE_CALL(0, 1), vm->pc);
}

void Scm_Apply1(ScmObj proc, ScmObj arg)
{
    ScmVM *vm = theVM;
    if (!SCM_PROCEDUREP(proc))
        Scm_Error("procedure required, but got %S", proc);
    PUSH_ARG(arg);
    PUSH_ARG(proc);
    vm->pc = Scm_Cons(SCM_VM_MAKE_CALL(1, 1), vm->pc);
}

void Scm_Apply2(ScmObj proc, ScmObj arg1, ScmObj arg2)
{
    ScmVM *vm = theVM;
    if (!SCM_PROCEDUREP(proc))
        Scm_Error("procedure required, but got %S", proc);
    PUSH_ARG(arg1);
    PUSH_ARG(arg2);
    PUSH_ARG(proc);
    vm->pc = Scm_Cons(SCM_VM_MAKE_CALL(2, 1), vm->pc);
}

void Scm_Apply(ScmObj proc, ScmObj args)
{
    ScmVM *vm = theVM;
    ScmObj cp;
    int numargs = 0;
    
    if (!SCM_PROCEDUREP(proc))
        Scm_Error("procedure required, but got %S", proc);
    SCM_FOR_EACH(cp, args) {
        PUSH_ARG(SCM_CAR(cp));
        numargs++;
    }
    if (!SCM_NULLP(cp)) Scm_Error("improper list not allowed: %S", args);
    PUSH_ARG(proc);
    vm->pc = Scm_Cons(SCM_VM_MAKE_CALL(numargs, 1), vm->pc);
}

/* TODO: write proper environment object!
   for now, we ignore env argument. */
void Scm_Eval(ScmObj expr, ScmObj env)
{
    ScmObj code = Scm_Compile(expr);
    push_continuation(theVM->pc);
    /* restore env here */
    theVM->env = NULL;
    theVM->pc = code;
}

/*
 * Dynamic handlers
 */

static void dynwind_before_cc(ScmObj result, void **data);
static void dynwind_body_cc(ScmObj result, void **data);
static void dynwind_after_cc(ScmObj result, void **data);

void Scm_DynamicWind(ScmObj before, ScmObj body, ScmObj after)
{
    void *data[3];

    if (!SCM_PROCEDUREP(before) || SCM_PROCEDURE_REQUIRED(before) != 0)
        Scm_Error("thunk required for BEFORE argument, but got %S", before);
    if (!SCM_PROCEDUREP(body) || SCM_PROCEDURE_REQUIRED(body) != 0)
        Scm_Error("thunk required for BODY argument, but got %S", body);
    if (!SCM_PROCEDUREP(after) || SCM_PROCEDURE_REQUIRED(after) != 0)
        Scm_Error("thunk required for AFTER argument, but got %S", after);

    data[0] = (void*)before;
    data[1] = (void*)body;
    data[2] = (void*)after;

    Scm_VMPushCC(dynwind_before_cc, data, 3);
    Scm_Apply0(before);
}

static void dynwind_before_cc(ScmObj result, void **data)
{
    ScmObj before  = SCM_OBJ(data[0]);
    ScmObj body = SCM_OBJ(data[1]);
    ScmObj after = SCM_OBJ(data[2]);
    ScmObj prev = theVM->handlers;

    void *d[2];
    d[0] = (void*)after;
    d[1] = (void*)prev;
    theVM->handlers = Scm_Cons(Scm_Cons(before, after), prev);

    Scm_VMPushCC(dynwind_body_cc, d, 2);
    Scm_Apply0(body);
}

static void dynwind_body_cc(ScmObj result, void **data)
{
    ScmObj after = SCM_OBJ(data[0]);
    ScmObj prev  = SCM_OBJ(data[1]);
    theVM->handlers = prev;
    Scm_VMPushCC(dynwind_after_cc, (void**)&result, 1);
    Scm_Apply0(after);
}

static void dynwind_after_cc(ScmObj result, void **data)
{
    ScmObj r = SCM_OBJ(data[0]);
    SCM_RETURN(r);
}

/*==============================================================
 * Continuation
 */

struct cont_data {
    ScmContFrame *cont;
    ScmObj handlers;
};

static void throw_cont_cc(ScmObj, void **);

static void throw_cont_body(ScmObj cur_handlers, /* dynamic handlers of
                                                    current continuation */
                            ScmObj dest_handlers, /* dynamic handlers of
                                                     target continuation */
                            ScmContFrame *cont, /* target continuation */
                            ScmObj args)        /* args to pass to the
                                                   target continuation */ 
{
    void *data[4];

    Scm_Printf(SCM_PORT(Scm_Stderr()),
               ">>> cur: %S\n>>> dst: %S\n>>>cont: %p\n>>>args: %S\n",
               cur_handlers, dest_handlers, cont, args);
    
    /*
     * first, check to see if we need to evaluate dynamic handlers.
     */
    if (cur_handlers != dest_handlers) {
        if (SCM_PAIRP(cur_handlers) 
            && SCM_FALSEP(Scm_Memq(SCM_CAR(cur_handlers), dest_handlers))) {
            /* evaluate "after" handlers of the current continuation */
            SCM_ASSERT(SCM_PAIRP(SCM_CAR(cur_handlers)));
            data[0] = (void*)SCM_CDR(cur_handlers);
            data[1] = (void*)dest_handlers;
            data[2] = (void*)cont;
            data[3] = (void*)args;
            Scm_VMPushCC(throw_cont_cc, data, 4);
            Scm_Apply0(SCM_CDR(SCM_CAR(cur_handlers)));
            return;
        } else if (SCM_PAIRP(dest_handlers)
                   && SCM_FALSEP(Scm_Memq(SCM_CAR(dest_handlers), cur_handlers))) {
            /* evaluate "before" handlers of the target continuation */
            SCM_ASSERT(SCM_PAIRP(SCM_CAR(cur_handlers)));
            data[0] = (void*)cur_handlers;
            data[1] = (void*)SCM_CDR(dest_handlers);
            data[2] = (void*)cont;
            data[3] = (void*)args;
            Scm_VMPushCC(throw_cont_cc, data, 4);
            Scm_Apply0(SCM_CAR(SCM_CAR(dest_handlers)));
            return;
        }
    }

    /*
     * now, install the target continuation
     */
    theVM->pc = cont->next;
    theVM->env = cont->env;
    theVM->argp = cont->argp;
    theVM->cont = cont->prev;
    theVM->handlers = dest_handlers;

    /* push args to the stack */
    SCM_FOR_EACH(args, args) {
        theVM->argp = Scm_Cons(SCM_CAR(args), theVM->argp);
    }

    Scm_VMDump(theVM);
}

static void throw_cont_cc(ScmObj result, void **data)
{
    ScmObj cur_handlers = SCM_OBJ(data[0]);
    ScmObj dest_handlers = SCM_OBJ(data[1]);
    ScmContFrame *cont = (ScmContFrame *)data[2];
    ScmObj args = SCM_OBJ(data[3]);

    throw_cont_body(cur_handlers, dest_handlers, cont, args);
}

static void throw_continuation(ScmObj *argframe, int nargs, void *data)
{

    struct cont_data *cd = (struct cont_data*)data;
    ScmObj handlers = cd->handlers;
    ScmObj current = theVM->handlers;
    ScmContFrame *cont = cd->cont;
    ScmObj args = argframe[0];

    SCM_ASSERT(cont != NULL);
    args = argframe[0];
    
    throw_cont_body(current, handlers, cont, args);
}

void Scm_CallCC(ScmObj proc)
{
    ScmObj contproc;
    struct cont_data *data;

    if (!SCM_PROCEDUREP(proc) || SCM_PROCEDURE_REQUIRED(proc) != 1)
        Scm_Error("Procedure taking one argument is required, but got: %S",
                  proc);

    data = SCM_NEW(struct cont_data);

    data->cont = get_continuation(theVM->pc);
    data->handlers = theVM->handlers;
    contproc = Scm_MakeSubr(throw_continuation,
                            (void*)data,
                            0, 1,
                            SCM_MAKE_STR("[continuation]"));
    Scm_Apply1(proc, contproc);
}

/*==============================================================
 * Debug features.
 */

/*
 * Stack trace.
 */

ScmObj Scm_VMGetStack(ScmVM *vm)
{
    ScmContFrame *c = vm->cont;
    ScmEnvFrame *e = vm->env;
    ScmObj pc = vm->pc;
    ScmObj stack = SCM_NIL, stacktail;

    for (;;) {
        SCM_FOR_EACH(pc, pc) {
            if (SCM_SOURCE_INFOP(SCM_CAR(pc))) {
                SCM_GROW_LIST(stack, stacktail,
                              SCM_SOURCE_INFO(SCM_CAR(pc))->info);
                break;
            }
            if (SCM_CCONTP(SCM_CAR(pc))) {
                /* We're middle in some "splitted" c-function.
                   Look for the source info from which the c-function
                   is called. */
                continue;
            }
            break;
        }
        if (e) {
            SCM_GROW_LIST(stack, stacktail, e->info);
        }

        if (!c) break;
        e = c->env;
        pc = c->next;
        c = c->prev;
    } while (c);
    return stack;
}

/*
 * Printer of VM instruction.
 */

static struct insn_info {
    const char *name;
    int nparams;
} insn_table[] = {
#define DEFINSN(sym, nam, np) { nam, np },
#include "gauche/vminsn.h"
#undef DEFINSN
};

int Scm__VMInsnWrite(ScmObj obj, ScmPort *out, int mode)
{
    struct insn_info *info;
    int nc, param0, param1;
    char buf[50];
    int insn = SCM_VM_INSN_CODE(obj);
    SCM_ASSERT(insn >= 0 && insn < SCM_VM_NUM_INSNS);

    info = &insn_table[insn];
    switch (info->nparams) {
    case 0:
        nc = snprintf(buf, 50, "#<%s>", info->name);
        break;
    case 1:
        param0 = SCM_VM_INSN_ARG(obj);
        nc = snprintf(buf, 50, "#<%s %d>", info->name, param0);
        break;
    case 2:
        param0 = SCM_VM_INSN_ARG0(obj);
        param1 = SCM_VM_INSN_ARG1(obj);
        nc = snprintf(buf, 50, "#<%s %d,%d>", info->name, param0, param1);
        break;
    default:
        Scm_Panic("something screwed up");
    }
    SCM_PUTCSTR(buf, out);
    return nc;
}

/* Returns list of insn name and parameters.  Useful if you want to 
   inspect compiled code from Scheme. */
ScmObj Scm_VMInsnInspect(ScmObj obj)
{
    struct insn_info *info;
    ScmObj r;
    int param0, param1;
    int insn;

    if (!SCM_VM_INSNP(obj))
        Scm_Error("VM instruction expected, but got %S", obj);
    insn = SCM_VM_INSN_CODE(obj);
    SCM_ASSERT(insn >= 0 && insn < SCM_VM_NUM_INSNS);

    info = &insn_table[insn];
    switch (info->nparams) {
    case 0:
        r = SCM_LIST1(SCM_MAKE_STR(info->name));
        break;
    case 1:
        param0 = SCM_VM_INSN_ARG(obj);
        r = SCM_LIST2(SCM_MAKE_STR(info->name), SCM_MAKE_INT(param0));
        break;
    case 2:
        param0 = SCM_VM_INSN_ARG0(obj);
        param1 = SCM_VM_INSN_ARG1(obj);
        r = SCM_LIST3(SCM_MAKE_STR(info->name),
                      SCM_MAKE_INT(param0), SCM_MAKE_INT(param1));
        break;
    default:
        Scm_Panic("something screwed up");
        r = SCM_UNDEFINED;      /* dummy */
    }
    return r;
}

/*
 * Dump VM internal state.
 */

static void dump_env(ScmEnvFrame *env, ScmPort *out)
{
    int i;
    Scm_Printf(out, "   %p %55.1S\n", env, env->info);
    Scm_Printf(out, "       [");
    for (i=0; i<env->size; i++)
        Scm_Printf(out, " %S", env->data[i]);
    Scm_Printf(out, " ]\n");
}

void Scm_VMDump(ScmVM *vm)
{
    ScmPort *out = vm->curerr;
    char buf[50];
    ScmEnvFrame *env = vm->env;
    ScmContFrame *cont = vm->cont;
    int j;

    Scm_Printf(out, "VM %p\n", vm);
    Scm_Printf(out, " argp: %65.1S\n", vm->argp);
    Scm_Printf(out, "   pc: %65.1S\n", vm->pc);

    Scm_Printf(out, " envs:\n");
    while (env) {
        dump_env(env, out);
        env = env->up;
    }
    
    Scm_Printf(out, "conts:\n");
    while (cont) {
        Scm_Printf(out, "   %p  argp = %S\n", cont, cont->argp);
        Scm_Printf(out, "              env = %p\n", cont->env);
        Scm_Printf(out, "             next = %50.1S\n", cont->next);
        cont = cont->prev;
    }

    Scm_Printf(out, "dynenv: %S\n", vm->handlers);
}

