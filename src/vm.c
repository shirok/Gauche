/*
 * vm.c - evaluator
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
 *  $Id: vm.c,v 1.20 2001-02-02 12:17:12 shiro Exp $
 */

#include "gauche.h"
#include "gauche/memory.h"

/*
 * The VM.
 *
 *   VM encapsulates the dynamic status of the current exection.
 *   In Gauche, there's always one active virtual machine per thread,
 *   refered by Scm_VM().
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
    v->errorHandler = SCM_FALSE;

    v->curin  = SCM_PORT(Scm_Stdin());
    v->curout = SCM_PORT(Scm_Stdout());
    v->curerr = SCM_PORT(Scm_Stderr());

    v->enableInline = 1;

    /* initial frame */
    v->env = NULL;
        
    v->cont = NULL;
    v->pc = SCM_NIL;
    v->val0 = SCM_UNDEFINED;
    
    v->handlers = SCM_NIL;

    v->stack = SCM_NEW2(ScmObj*, SCM_VM_STACK_SIZE * sizeof(ScmObj));
    v->sp = v->stack;
    v->stackSize = SCM_VM_STACK_SIZE;
    return v;
}

static void vm_reset()
{
    theVM->sp = theVM->stack;
    theVM->env = NULL; /*topenv(theVM->module);*/
    theVM->argp = (ScmEnvFrame*)theVM->stack;
    theVM->cont = NULL;
    theVM->pc = SCM_NIL;
    theVM->val0 = SCM_UNDEFINED;
}

ScmObj Scm_VMGetResult(ScmVM *vm)
{
    return Scm_Cons(vm->val0, SCM_NIL);
}

void Scm_VMSetResult(ScmObj obj)
{
    theVM->val0 = obj;
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

/*====================================================================
 * VM interpreter
 *
 *  Interprets intermediate code CODE on VM.
 */

/* Micro-operations
 *   These macros concerning a primitive operations involving updating
 *   one or more registers.
 */

/* push OBJ to the top of the stack */
#define PUSH_ARG(obj)      (*sp++ = (obj))

/* pop the top object of the stack and store it to VAR */
#define POP_ARG(var)       ((var) = *--sp)

/* fetch an instruction */
#define FETCH_INSN(var)    ((var) = SCM_CAR(pc), pc = SCM_CDR(pc))

/* declare local variables for registers, and copy the current VM regs
   to them. */
#define DECL_REGS                               \
    ScmVM *vm = theVM;                          \
    ScmObj pc = vm->pc;                         \
    ScmContFrame *cont = vm->cont;              \
    ScmEnvFrame *env = vm->env;                 \
    ScmEnvFrame *argp = vm->argp;               \
    ScmObj *sp = vm->sp;                        \
    ScmObj val0 = vm->val0

/* save VM regs into VM structure. */
#define SAVE_REGS()                             \
    do {                                        \
        vm->pc = pc;                            \
        vm->env = env;                          \
        vm->argp = argp;                        \
        vm->cont = cont;                        \
        vm->sp = sp;                            \
        vm->val0 = val0;                        \
    } while (0)

#define RESTORE_REGS()                          \
    do {                                        \
        pc = vm->pc;                            \
        env = vm->env;                          \
        argp = vm->argp;                        \
        cont = vm->cont;                        \
        sp = vm->sp;                            \
    } while (0)

/* Push a continuation frame.  next_pc is the PC from where execution
   will be resumed.  */
#define PUSH_CONT(next_pc)                              \
    do {                                                \
        ScmContFrame *newcont = (ScmContFrame*)sp;      \
        newcont->prev = cont;                           \
        newcont->env = env;                             \
        newcont->argp = argp;                           \
        newcont->size = sp - (ScmObj*)argp;             \
        newcont->pc = next_pc;                          \
        cont = newcont;                                 \
        sp += CONT_FRAME_SIZE;                          \
    } while (0)

/* pop a continuation frame, i.e. return from a procedure.
   env is assumed to point the argument frame.  */
#define POP_CONT()                                      \
    do {                                                \
        sp   = (ScmObj*)cont->argp + cont->size;        \
        env  = cont->env;                               \
        argp = cont->argp;                              \
        pc   = cont->pc;                                \
        cont = cont->prev;                              \
    } while (0)

/* push a header of an environment frame.   this is the second stage of
   preparing a procedure call.   info and size are set just before
   the call, by adjust_argument_frame(). */
#define PUSH_ENV_HDR()                          \
    do {                                        \
        argp = (ScmEnvFrame*)sp;                \
        argp->up   = NULL;                      \
        argp->info = SCM_FALSE;                 \
        argp->size = 0;     /*patched later*/   \
        sp += ENV_HDR_SIZE;                     \
    } while (0)

/* extend the current environment by SIZE words.   used for LET. */
#define PUSH_LOCAL_ENV(size_, info_)            \
    do {                                        \
        ScmEnvFrame *newenv = (ScmEnvFrame*)sp; \
        int i;                                  \
        sp += ENV_SIZE(size_);                  \
        newenv->size = size_;                   \
        newenv->up = env;                       \
        newenv->info = info_;                   \
        for (i=0; i<size_; i++) {               \
            newenv->data[i] = SCM_UNDEFINED;    \
        }                                       \
        env = newenv;                           \
    } while (0)

/* discard extended environment. */
#define POP_LOCAL_ENV()                         \
    do {                                        \
        sp -= ENV_SIZE(env->size);              \
        env = env->up;                          \
    } while (0)

#define VM_DUMP(delimiter)                      \
    SAVE_REGS();                                \
    fprintf(stderr, delimiter);                 \
    Scm_VMDump(theVM)

#define VM_ASSERT(expr)                                                 \
    do {                                                                \
        if (!(expr))  {                                                 \
            fprintf(stderr, "\"%s\", line %d: Assertion failed: %s\n",  \
                    __FILE__, __LINE__, #expr);                         \
            Scm_VMDump(theVM);                                          \
            Scm_Panic("exitting...\n");                                 \
        }                                                               \
    } while (0)

#define VM_ERR(errargs) do { SAVE_REGS(); Scm_Error errargs; } while (0)

/* return true if ptr points into the stack area */
#ifdef __GNUC__
inline
#endif
static int in_stack(ScmVM *vm, ScmObj *ptr)
{
    return (ptr >= vm->stack && ptr < vm->stack+vm->stackSize);
}

/* move the current chain of environments from the stack to the heap.
   if there're continuation frames which point to the moved env, those
   pointers are adjusted as well. */
#ifdef __GNUC__
inline
#endif
static ScmEnvFrame *save_env(ScmVM *vm)
{
    ScmEnvFrame *e = vm->env, *prev = NULL;
    ScmContFrame *c = vm->cont;
    for (; in_stack(vm, (ScmObj*)e); e = e->up) {
        int size = sizeof(ScmEnvFrame) + (e->size-1)*sizeof(ScmObj);
        ScmEnvFrame *s = SCM_NEW2(ScmEnvFrame*, size);
        memcpy(s, e, size);
        for (; c; c = c->prev) {
            if (c->env == e) {
                c->env = s;
                break;
            }
        }
        if (vm->env == e) vm->env = s;
        if (prev) prev->up = s;
        prev = s;
    }
    return vm->env;
}

/* check the argument count is OK for call to PROC.  if PROC takes &rest
 * args, fold those arguments to the list.  Returns adjusted size of
 * the argument frame.
 */
#ifdef __GNUC__
inline
#endif
static int adjust_argument_frame(ScmVM *vm,
                                 ScmObj proc, int caller_args)
{
    int i, reqargs, restarg, callee_args;
    
    if (!SCM_PROCEDUREP(proc)) Scm_Error("bad procedure: %S", proc);

    reqargs = SCM_PROCEDURE_REQUIRED(proc);
    restarg = SCM_PROCEDURE_OPTIONAL(proc);
    callee_args  = reqargs + (restarg? 1 : 0);
    
    if (restarg) {
        ScmObj *sp = vm->sp;
        ScmObj p = SCM_NIL, a;
        if (caller_args < reqargs) {
            Scm_Error("wrong number of arguments for %S (required %d, got %d)",
                      proc, reqargs, caller_args);
        }
        /* fold &rest args */
        for (i = reqargs; i < caller_args; i++) {
            POP_ARG(a);
            p = Scm_Cons(a, p);
        }
        vm->argp->data[reqargs] = p;
        vm->sp = (ScmObj*)vm->argp + ENV_SIZE(callee_args);
    } else {
        if (caller_args != reqargs) {
            Scm_Error("wrong number of arguments for %S (required %d, got %d)",
                      proc, reqargs, caller_args);
        }
    }
    vm->argp->info = SCM_PROCEDURE_INFO(proc);
    vm->argp->size = callee_args;
    return callee_args;
}



static ScmEnvFrame *topenv(ScmModule *module)
{
    ScmEnvFrame *e = (ScmEnvFrame *)theVM->sp;
    e->up = NULL;
    e->info = SCM_MAKE_STR("[toplevel]");
    e->size = 1;
    e->data[0] = SCM_OBJ(module);
    theVM->sp += 5;
    return e;
}

/*
 * main loop of VM
 */
static void run_loop()
{
    DECL_REGS;
    ScmObj code;
    ScmObj val1, val2, val3, valn; /* values registers */
    int nvals;                  /* # of values */
    
    for (;;) {
/*        VM_DUMP("");*/
        
        if (!SCM_PAIRP(pc)) {
            /* We are at the end of procedure.  Activate the most recent
               continuation. */
            if (cont == NULL) {
                SAVE_REGS();
                return; /* no more continuations */
            }
            if (cont->argp == NULL) {
                /* C continuation */
                void *data[SCM_CCONT_DATA_SIZE];
                ScmObj (*after)(ScmObj, void**);

                memcpy(data,
                       (ScmObj*)cont + CONT_FRAME_SIZE,
                       cont->size * sizeof(void*));
                after = (ScmObj (*)(ScmObj, void**))cont->pc;
                sp = (ScmObj*)cont;
                cont = cont->prev;
                env = cont->env;
                SAVE_REGS();
                val0 = after(val0, data);
                RESTORE_REGS();
                continue;
            }
            
            POP_CONT();
            continue;
        }

        FETCH_INSN(code);
        
        if (!SCM_VM_INSNP(code)) {
            if (SCM_SOURCE_INFOP(code)) {
                /* source info.  ignore it. */
                continue;
            } else {
                /* load literal object to the register */
                val0 = code;
                continue;
            }
        }

        /* VM instructions */
        switch(SCM_VM_INSN_CODE(code)) {

        case SCM_VM_PUSH:
            {
                PUSH_ARG(val0);
                continue;
            }
        case SCM_VM_PRE_CALL:
            {
                ScmObj prep = SCM_CAR(pc), next = SCM_CDR(pc);
                PUSH_CONT(next);
                PUSH_ENV_HDR();
                pc = prep;
                continue;
            }
        case SCM_VM_PRE_TAIL:
            {
                PUSH_ENV_HDR();
                continue;
            }
        case SCM_VM_TAIL_CALL:;
        case SCM_VM_CALL:
            {
                int nargs = SCM_VM_INSN_ARG(code);
                int tailp = (SCM_VM_INSN_CODE(code)==SCM_VM_TAIL_CALL);
                int argcnt;

                SAVE_REGS();
                argcnt = adjust_argument_frame(vm, val0, nargs);
                RESTORE_REGS();
                if (tailp) {
                    /* discard the caller's argument frame, and shift
                       the callee's argument frame there. */
                    ScmObj *to = (ScmObj*)argp - ENV_SIZE(env->size);
                    memmove(to, argp, ENV_SIZE(argcnt)*sizeof(ScmObj));
                    argp = (ScmEnvFrame*)to;
                    sp = to + ENV_SIZE(argcnt);
                }

                if (SCM_SUBRP(val0)) {
                    env = argp;
                    SAVE_REGS();
                    val0 = SCM_SUBR(val0)->func(argp->data, argcnt,
                                                SCM_SUBR(val0)->data);
                    RESTORE_REGS();
                } else {
                    env = argp;
                    env->up = SCM_CLOSURE(val0)->env;
                    pc = SCM_CLOSURE(val0)->code;
                }
                continue;
            }
        case SCM_VM_GREF:
            {
                ScmObj sym;
                
                VM_ASSERT(SCM_PAIRP(pc));
                sym = SCM_CAR(pc);

                if (SCM_GLOCP(sym)) {
                    val0 = SCM_GLOC(sym)->value;
                    if (val0 == SCM_UNBOUND) {
                        VM_ERR(("unbound variable: %S",
                                SCM_OBJ(SCM_GLOC(sym)->name)));
                    }
                } else if (SCM_SYMBOLP(sym)) {
                    ScmGloc *gloc =
                        Scm_FindBinding(vm->module, SCM_SYMBOL(sym), 0);
                    if (gloc == NULL) {
                        VM_ERR(("unbound variable: %S", sym));
                    }
                    val0 = gloc->value;
                    if (val0 == SCM_UNBOUND) {
                        VM_ERR(("unbound variable: %S", sym));
                    }
                    /* memorize gloc */
                    /* TODO: make it MT safe! */
                    SCM_SET_CAR(pc, SCM_OBJ(gloc));
                }
                pc = SCM_CDR(pc);
                continue;
            }
        case SCM_VM_LREF:
            {
                int dep = SCM_VM_INSN_ARG0(code);
                int off = SCM_VM_INSN_ARG1(code);
                ScmEnvFrame *e = env;
                
                while (dep-- > 0) {
                    VM_ASSERT(e != NULL);
                    e = e->up;
                }
                VM_ASSERT(e != NULL);
                VM_ASSERT(e->size > off);
                val0 = e->data[off];
                continue;
            }
        case SCM_VM_TAILBIND:
            {
                /* TODO: concrete implementation of frame shifting */
                int nlocals = SCM_VM_INSN_ARG(code);
                ScmObj info;

                FETCH_INSN(info); /* discard it */
                argp->size = env->size;
                argp->up = env->up;
                argp->info = env->info;
                env = argp;
                continue;
            }
        case SCM_VM_LET:
            {
                int nlocals = SCM_VM_INSN_ARG(code), i;
                ScmObj info;

                VM_ASSERT(SCM_PAIRP(pc));
                FETCH_INSN(info);
                PUSH_LOCAL_ENV(nlocals, info);
                continue;
            }
        case SCM_VM_POPENV:
            {
                VM_ASSERT(env != NULL);
                POP_LOCAL_ENV();
                continue;
            }
        case SCM_VM_SET:
            {
                ScmObj location, val;
                VM_ASSERT(SCM_PAIRP(pc));
                
                location = SCM_CAR(pc);
                if (SCM_GLOCP(location)) {
                    SCM_GLOC(location)->value = val0;
                } else if (SCM_VM_INSNP(location)
                           && SCM_VM_INSN_CODE(location) == SCM_VM_LREF) {
                    ScmEnvFrame *e = env;
                    int dep = SCM_VM_INSN_ARG0(location);
                    int off = SCM_VM_INSN_ARG1(location);
                    
                    while (dep-- > 0) {
                        VM_ASSERT(e != NULL);
                        e = e->up;
                    }
                    VM_ASSERT(e != NULL);
                    VM_ASSERT(e->size > off);
                    e->data[off] = val0;
                } else if (SCM_SYMBOLP(location)) {
                    ScmGloc *gloc = Scm_FindBinding(vm->module,
                                                    SCM_SYMBOL(location), 1);
                    if (gloc == NULL) {
                        VM_ERR(("symbol not defined: %S", location));
                    }
                    gloc->value = val0;
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
        case SCM_VM_DEFINE:
            {
                ScmObj var;

                VM_ASSERT(SCM_PAIRP(pc));
                var = SCM_CAR(pc);
                VM_ASSERT(SCM_SYMBOLP(var));
                pc = SCM_CDR(pc);
                Scm_Define(vm->module, SCM_SYMBOL(var), val0);
                val0 = var;
                continue;
            }
        case SCM_VM_IF:
            {
                if (SCM_FALSEP(val0)) { pc = SCM_CDR(pc); }
                else                  { pc = SCM_CAR(pc); }
                continue;
            }
        case SCM_VM_LAMBDA:
            {
                ScmObj info, body;
                
                VM_ASSERT(SCM_PAIRP(pc));
                info = SCM_CAR(pc);
                pc = SCM_CDR(pc);
                VM_ASSERT(SCM_PAIRP(pc));
                body = SCM_CAR(pc);
                pc = SCM_CDR(pc);

                /* move environment to the heap. */
                SAVE_REGS();
                env = save_env(vm);
                val0 = Scm_MakeClosure(SCM_VM_INSN_ARG0(code),
                                       SCM_VM_INSN_ARG1(code),
                                       body,
                                       env,
                                       info);
                RESTORE_REGS();
                continue;
            }

            /* Inlined procedures */
        case SCM_VM_NOT:
            {
                val0 = SCM_MAKE_BOOL(SCM_FALSEP(val0));
                continue;
            }
        case SCM_VM_CONS:
            {
                ScmObj ca;
                POP_ARG(ca);
                val0 = Scm_Cons(ca, val0);
                continue;
            }
        case SCM_VM_CAR:
            {
                if (!SCM_PAIRP(val0))
                    VM_ERR(("pair required, but got %S", val0));
                val0 = SCM_CAR(val0);
                continue;
            }
        case SCM_VM_CDR:
            {
                if (!SCM_PAIRP(val0))
                    VM_ERR(("pair required, but got %S", val0));
                val0 = SCM_CDR(val0);
                continue;
            }
        case SCM_VM_LIST:
            {
                int nargs = SCM_VM_INSN_ARG(code);
                ScmObj cp = SCM_NIL;
                if (nargs > 0) {
                    ScmObj arg;
                    cp = Scm_Cons(val0, cp);
                    while (--nargs > 0) {
                        POP_ARG(arg);
                        cp = Scm_Cons(arg, cp);
                    }
                }
                val0 = cp;
                continue;
            }
        case SCM_VM_LIST_STAR:
            {
                continue;
            }
        case SCM_VM_EQ:
            {
                ScmObj item;
                POP_ARG(item);
                val0 = SCM_MAKE_BOOL(item == val0);
                continue;
            }
        case SCM_VM_EQV:
            {
                ScmObj item;
                POP_ARG(item);
                val0 = Scm_EqvP(item, val0);
                continue;
            }
        case SCM_VM_MEMV:
            {
                ScmObj item;
                POP_ARG(item);
                val0 = Scm_Memv(item, val0);
                continue;
            }
        case SCM_VM_APPEND:
            {
                continue;
            }
        case SCM_VM_PROMISE: 
            {
                val0 = Scm_MakePromise(val0);
                continue;
            }
        case SCM_VM_VEC:
            {
                int nargs = SCM_VM_INSN_ARG(code), i;
                ScmObj vec = Scm_MakeVector(nargs, SCM_UNDEFINED);
                if (nargs > 0) {
                    ScmObj arg = val0;
                    for (i=nargs-1; i > 0; i--) {
                        SCM_VECTOR_ELEMENT(vec, i) = arg;
                        POP_ARG(arg);
                    }
                    SCM_VECTOR_ELEMENT(vec, 0) = arg;
                }
                val0 = vec;
                continue;
            }
        case SCM_VM_APP_VEC:
            {
                continue;
            }
        case SCM_VM_VEC_LEN:
            {
                int siz;
                if (!SCM_VECTORP(val0))
                    VM_ERR(("vector expected, but got %S\n", val0));
                siz = SCM_VECTOR_SIZE(val0);
                val0 = SCM_MAKE_INT(siz);
                continue;
            }
        case SCM_VM_VEC_REF:
            {
                ScmObj vec;
                int k;
                POP_ARG(vec);
                if (!SCM_VECTORP(vec))
                    VM_ERR(("vector expected, but got %S\n", vec));
                if (!SCM_INTP(val0))
                    VM_ERR(("integer expected, but got %S\n", val0));
                k = SCM_INT_VALUE(val0);
                if (k < 0 || k >= SCM_VECTOR_SIZE(vec))
                    VM_ERR(("index out of range: %d\n", k));
                val0 = SCM_VECTOR_ELEMENT(vec, k);
                continue;
            }
        case SCM_VM_VEC_SET:
            {
                ScmObj vec, ind;
                int k;
                POP_ARG(ind);
                POP_ARG(vec);
                if (!SCM_VECTORP(vec))
                    VM_ERR(("vector expected, but got %S\n", vec));
                if (!SCM_INTP(ind))
                    VM_ERR(("integer expected, but got %S\n", ind));
                k = SCM_INT_VALUE(ind);
                if (k < 0 || k >= SCM_VECTOR_SIZE(vec))
                    VM_ERR(("index out of range: %d\n", k));
                SCM_VECTOR_ELEMENT(vec, k) = val0;
                val0 = SCM_UNDEFINED;
                continue;

            }
        default:
            Scm_Panic("Illegal vm instruction: %08x",  SCM_VM_INSN_CODE(code));
        }
    }
}

/*==================================================================
 * Function application from C
 */

static void arrange_application(ScmObj proc, ScmObj args, int numargs)
{
    DECL_REGS;
    ScmObj cp, code = SCM_NIL, tail;

    /* This is inefficient, but for now ... */
    SCM_APPEND1(code, tail, SCM_VM_INSN(SCM_VM_PRE_TAIL));
    SCM_FOR_EACH(cp, args) {
        SCM_APPEND1(code, tail, SCM_CAR(cp));
        SCM_APPEND1(code, tail, SCM_VM_INSN(SCM_VM_PUSH));
    }
    SCM_APPEND1(code, tail, proc);
    SCM_APPEND1(code, tail, SCM_VM_INSN1(SCM_VM_CALL, numargs));
    argp = (ScmEnvFrame *)sp;
    PUSH_CONT(code);
    SAVE_REGS();
}

/* The Scm_VMApply family is supposed to be called in SUBR.  It doesn't really
   applies the function in it.  Instead, it modifies the VM state so that
   the specified function will be called immediately after this SUBR
   returns to the VM.   The return value of Scm_VMApply is just a PROC,
   but it should be returned as the return value of SUBR, which will be
   used by the VM.
 */
ScmObj Scm_VMApply(ScmObj proc, ScmObj args)
{
    DECL_REGS;
    int numargs = Scm_Length(args);
    
    if (!SCM_PROCEDUREP(proc))
        Scm_Error("procedure required, but got %S", proc);
    if (numargs < 0)
        Scm_Error("improper list not allowed: %S", args);
    arrange_application(proc, args, numargs);
    return proc;
}

ScmObj Scm_VMApply0(ScmObj proc)
{
    /* TODO: can be optimized */
    return Scm_VMApply(proc, SCM_NIL);
}

ScmObj Scm_VMApply1(ScmObj proc, ScmObj arg)
{
    /* TODO: can be optimized */
    return Scm_VMApply(proc, SCM_LIST1(arg));
}

ScmObj Scm_VMApply2(ScmObj proc, ScmObj arg1, ScmObj arg2)
{
    /* TODO: can be optimized */
    return Scm_VMApply(proc, SCM_LIST2(arg1, arg2));
}

/* TODO: write proper environment object!
   for now, we ignore env argument. */
ScmObj Scm_VMEval(ScmObj expr, ScmObj e)
{
    DECL_REGS;
    ScmObj v = Scm_Compile(expr, SCM_NIL, SCM_COMPILE_NORMAL);
    argp = (ScmEnvFrame *)sp;
    PUSH_CONT(v);
    SAVE_REGS();
    return SCM_UNDEFINED;
}

/* User level eval().  */
ScmObj Scm_Eval(ScmObj expr, ScmObj e)
{
    vm_reset(); /* shouldn't reset, actually.  we need
                   some mechanism to save the current vm state,
                   and recover it upon leaving this continuation. */
    theVM->pc = Scm_Compile(expr, SCM_NIL, SCM_COMPILE_NORMAL);
    run_loop();
}

/* Adjusts sp so that a VM function can be called outside of a subr.
   Usually followed by Scm_VM* function. */
/* TODO: ugly interface.  any good alternative? */
void Scm_VMPrepareCall(void)
{
    DECL_REGS;
}

/*
 * C-continuations.
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

/* Arrange C function AFTER to be called after the procedure returns.
 * Usually followed by Scm_VMApply* function.
 */
void Scm_VMPushCC(ScmObj (*after)(ScmObj result, void **data),
                  void **data, int datasize)
{
    DECL_REGS;
    int i;
    ScmContFrame *cc = (ScmContFrame*)sp;
    sp += CONT_FRAME_SIZE;
    cc->prev = cont;
    cc->argp = NULL;
    cc->size = datasize;
    cc->pc = SCM_OBJ(after);
    cc->env = env;
    for (i=0; i<datasize; i++) {
        PUSH_ARG(SCM_OBJ(data[i]));
    }
    cont = cc;
    SAVE_REGS();
}

/*=================================================================
 * Dynamic handlers
 */

static ScmObj dynwind_before_cc(ScmObj result, void **data);
static ScmObj dynwind_body_cc(ScmObj result, void **data);
static ScmObj dynwind_after_cc(ScmObj result, void **data);

ScmObj Scm_VMDynamicWind(ScmObj before, ScmObj body, ScmObj after)
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
    return Scm_VMApply0(before);
}

static ScmObj dynwind_before_cc(ScmObj result, void **data)
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
    return Scm_VMApply0(body);
}

static ScmObj dynwind_body_cc(ScmObj result, void **data)
{
    ScmObj after = SCM_OBJ(data[0]);
    ScmObj prev  = SCM_OBJ(data[1]);
    theVM->handlers = prev;
    Scm_VMPushCC(dynwind_after_cc, (void**)&result, 1);
    return Scm_VMApply0(after);
}

static ScmObj dynwind_after_cc(ScmObj result, void **data)
{
    return SCM_OBJ(data[0]);
}

/*=================================================================
 * Exception handling
 */

void default_exception_handler(ScmObj e)
{
    ScmObj stack = Scm_VMGetStack(theVM), cp;
    ScmPort *err = SCM_VM_CURRENT_ERROR_PORT(theVM);
    int depth = 0;
    
    if (SCM_EXCEPTIONP(e) && SCM_STRINGP(SCM_EXCEPTION_DATA(e))) {
        SCM_PUTCSTR("*** ERROR: ", err);
        SCM_PUTS(SCM_STRING(SCM_EXCEPTION_DATA(e)), err);
        SCM_PUTNL(err);
    } else {
        SCM_PUTCSTR("*** ERROR: (unknown exception type)\n", err);
    }
    
    SCM_PUTCSTR("Stack Trace:\n", err);
    SCM_PUTCSTR("_______________________________________\n", err);
    SCM_FOR_EACH(cp, stack) {
        Scm_Printf(SCM_PORT(err), "%3d   %66.1S\n",
                   depth++, SCM_CAR(cp));
    }
}

ScmObj throw_exception_cc(ScmObj result, void **data)
{
    ScmObj handlers = SCM_OBJ(data[0]);
    if (!SCM_NULLP(handlers)) {
        ScmObj proc = SCM_CDAR(handlers);
        void *data = SCM_CDR(handlers);
        theVM->handlers = SCM_CDR(handlers);
        Scm_VMPushCC(throw_exception_cc, &data, 1);
        return Scm_VMApply0(proc);
    }

    if (theVM->escape) {
        longjmp(theVM->escape->jbuf, 1);
    } else {
        /* No error handler */
        exit(1);
    }
    return SCM_UNDEFINED;       /* NOTREACHED */
}

ScmObj Scm_VMThrowException(ScmObj exception)
{
    void *data = theVM->handlers;
    if (SCM_PROCEDUREP(theVM->errorHandler)) {
        Scm_VMPushCC(throw_exception_cc, &data, 1);
        return Scm_VMApply1(theVM->errorHandler, exception);
    } else {
        default_exception_handler(exception);
    }   
    return throw_exception_cc(SCM_UNDEFINED, &data);
}

/*==============================================================
 * Continuation
 */

struct cont_data {
    ScmContFrame *cont;
    ScmObj handlers;
};

static ScmObj throw_cont_cc(ScmObj, void **);

static ScmObj throw_cont_body(ScmObj cur_handlers, /* dynamic handlers of
                                                      current continuation */
                              ScmObj dest_handlers, /* dynamic handlers of
                                                       target continuation */
                              ScmContFrame *cont, /* target continuation */
                              ScmObj args)        /* args to pass to the
                                                     target continuation */ 
{
    void *data[4];

    /*
     * first, check to see if we need to evaluate dynamic handlers.
     */
    if (cur_handlers != dest_handlers) {
        while (SCM_PAIRP(cur_handlers)) {
            SCM_ASSERT(SCM_PAIRP(SCM_CAR(cur_handlers)));
            if (SCM_CAAR(cur_handlers) == SCM_FALSE)
                continue;       /* this is an error handler */
            if (SCM_FALSEP(Scm_Memq(SCM_CAR(cur_handlers), dest_handlers))) {
                /* evaluate "after" handlers of the current continuation */
                data[0] = (void*)SCM_CDR(cur_handlers);
                data[1] = (void*)dest_handlers;
                data[2] = (void*)cont;
                data[3] = (void*)args;
                theVM->handlers = SCM_CDR(cur_handlers);
                Scm_VMPushCC(throw_cont_cc, data, 4);
                Scm_VMApply0(SCM_CDAR(cur_handlers));
                return SCM_UNDEFINED;
            } else {
                /* the destination is in the same dynamic environment, so
                   we don't need to go further */
                break;
            }
        }
        while (SCM_PAIRP(dest_handlers)) {
            SCM_ASSERT(SCM_PAIRP(SCM_CAR(dest_handlers)));
            if (SCM_CAAR(dest_handlers) == SCM_FALSE)
                continue;       /* this is an error handler */
            if (SCM_FALSEP(Scm_Memq(SCM_CAR(dest_handlers), cur_handlers))) {
                /* evaluate "before" handlers of the target continuation */
                data[0] = (void*)cur_handlers;
                data[1] = (void*)SCM_CDR(dest_handlers);
                data[2] = (void*)cont;
                data[3] = (void*)args;
                theVM->handlers = SCM_CDR(dest_handlers);
                Scm_VMPushCC(throw_cont_cc, data, 4);
                Scm_VMApply0(SCM_CAR(SCM_CAR(dest_handlers)));
                return SCM_UNDEFINED;
            } else {
                break;
            }
        }
    }

    /*
     * now, install the target continuation
     */
    theVM->pc = cont->pc;
    theVM->env = cont->env;
    theVM->cont = cont->prev;
    theVM->handlers = dest_handlers;

    return args;
}

static ScmObj throw_cont_cc(ScmObj result, void **data)
{
    ScmObj cur_handlers = SCM_OBJ(data[0]);
    ScmObj dest_handlers = SCM_OBJ(data[1]);
    ScmContFrame *cont = (ScmContFrame *)data[2];
    ScmObj args = SCM_OBJ(data[3]);

    return throw_cont_body(cur_handlers, dest_handlers, cont, args);
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

ScmObj Scm_VMCallCC(ScmObj proc)
{
    ScmObj contproc;
    struct cont_data *data;

    if (!SCM_PROCEDUREP(proc) || SCM_PROCEDURE_REQUIRED(proc) != 1)
        Scm_Error("Procedure taking one argument is required, but got: %S",
                  proc);

    /* Copy continuation to the heap.   I'd like to try non-copying
     * method later, but for now...
     */
    Scm_Panic("Scm_VMCallCC!\n");
    return SCM_UNDEFINED;
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
                SCM_APPEND1(stack, stacktail,
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
            SCM_APPEND1(stack, stacktail, e->info);
        }

        if (!c) break;
        e = c->env;
        pc = c->pc;
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
    Scm_Printf(out, "       up=%p size=%d\n", env->up, env->size);
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

    Scm_Printf(out, "VM %p -----------------------------------------------------------\n", vm);
    Scm_Printf(out, "   pc: %#65.1S\n", vm->pc);
    Scm_Printf(out, "   sp: %p\n", vm->sp);
    Scm_Printf(out, " argp: %p\n", vm->argp);
    Scm_Printf(out, "  btm: %p\n", vm->stack);
    Scm_Printf(out, " val0: %#65.1S\n", vm->val0);

    Scm_Printf(out, " envs:\n");
    while (env) {
        dump_env(env, out);
        env = env->up;
    }
    
    Scm_Printf(out, "conts:\n");
    while (cont) {
        Scm_Printf(out, "   %p\n", cont);
        Scm_Printf(out, "              env = %p\n", cont->env);
        Scm_Printf(out, "             argp = %p[%d]\n", cont->argp, cont->size);
        if (cont->argp) {
            Scm_Printf(out, "               pc = %#50.1S\n", cont->pc);
        } else {
            Scm_Printf(out, "               pc = {cproc %p}\n", cont->pc);
        }
        cont = cont->prev;
    }

    Scm_Printf(out, "dynenv: %S\n", vm->handlers);
}

