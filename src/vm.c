/*
 * vm.c - evaluator
 *
 *  Copyright(C) 2000-2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: vm.c,v 1.159 2002-07-05 09:36:56 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/memory.h"
#include "gauche/class.h"
#include "gauche/exception.h"

#include <unistd.h>
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif

#ifndef EX_SOFTWARE
/* SRFI-22 requires this. */
#define EX_SOFTWARE 70
#endif

/* An object to mark the boundary frame */
static ScmObj boundaryFrameMark;

/* return true if cont is a boundary continuation frame */
#define BOUNDARY_FRAME_P(cont) (SCM_EQ((cont)->info, boundaryFrameMark))

/*
 * The VM. 
 *
 *   VM encapsulates the dynamic status of the current exection.
 *   In Gauche, there's always one active virtual machine per thread,
 *   referred by Scm_VM().   From Scheme, VM is seen as a <thread> object.
 *
 *   All the VMs are chained to a global list vmList.  It is necessary
 *   since GC may not be able to see the thread specific data.
 *
 *   From Scheme, VM is viewed as <thread> object.  The class definition
 *   is in thrlib.stub.
 */

static ScmVM *rootVM;           /* VM for primodial thread */
static ScmObj vmList;           /* list of all VMs */
static ScmInternalMutex vmListMutex; /* mutex for vmList. */

#ifdef GAUCHE_USE_PTHREAD
static pthread_key_t vm_key;
#define theVM   ((ScmVM*)pthread_getspecific(vm_key))
#else
static ScmVM *theVM;
#endif  /* !GAUCHE_USE_PTHREAD */

static void save_stack(ScmVM *vm);

static ScmSubr default_exception_handler_rec;
#define DEFAULT_EXCEPTION_HANDLER  SCM_OBJ(&default_exception_handler_rec)
static ScmObj throw_cont_calculate_handlers(ScmEscapePoint *, ScmVM *);
static ScmObj throw_cont_body(ScmObj, ScmEscapePoint*, ScmObj);

/*
 * Constructor
 *   base can be NULL iff called from Scm_Init.
 */

ScmVM *Scm_NewVM(ScmVM *base,
                 ScmModule *module,
                 ScmObj name)
{
    ScmVM *v = SCM_NEW(ScmVM);
    int i;
    
    SCM_SET_CLASS(v, SCM_CLASS_VM);
    (void)SCM_INTERNAL_MUTEX_INIT(v->vmlock);
    (void)SCM_INTERNAL_COND_INIT(v->cond);
    v->state = SCM_VM_NEW;
    v->name = name;
    v->specific = SCM_FALSE;
    v->thunk = NULL;
    v->result = SCM_UNDEFINED;
    v->resultException = SCM_UNDEFINED;
    v->module = module ? module : base->module;
    v->cstack = base ? base->cstack : NULL;
    
    v->curin  = SCM_PORT(Scm_Stdin());
    v->curout = SCM_PORT(Scm_Stdout());
    v->curerr = SCM_PORT(Scm_Stderr());

    v->compilerFlags = 0;
    v->runtimeFlags = 0;

    v->stack = SCM_NEW2(ScmObj*, SCM_VM_STACK_SIZE * sizeof(ScmObj));
    v->sp = v->stack;
    v->stackSize = SCM_VM_STACK_SIZE;
    v->stackBase = v->stack;
    v->stackEnd = v->stack + v->stackSize;

    v->env = NULL;
    v->argp = (ScmEnvFrame*)v->stack;
    v->cont = NULL;
    v->pc = SCM_NIL;
    v->val0 = SCM_UNDEFINED;
    for (i=0; i<SCM_VM_MAX_VALUES; i++) v->vals[i] = SCM_UNDEFINED;
    v->numVals = 1;
    
    v->handlers = SCM_NIL;

    v->exceptionHandler = DEFAULT_EXCEPTION_HANDLER;
    v->escapePoint = NULL;
    v->escapeReason = SCM_VM_ESCAPE_NONE;
    v->escapeData[0] = NULL;
    v->escapeData[1] = NULL;
    v->defaultEscapeHandler = SCM_FALSE;

    v->load_history = SCM_NIL;
    v->load_next = SCM_NIL;
    v->load_port = SCM_FALSE;

    for (i=0; i<SCM_VM_SIGQ_SIZE; i++) v->sigQueue[i] = -1;
    v->sigQueueHead = v->sigQueueTail = 0;
    v->sigOverflow = 0;
    v->sigPending = SCM_NIL;
    v->sigHandlers =  SCM_NIL;
    sigemptyset(&v->sigMask);
    
    return v;
}

ScmObj Scm_VMGetResult(ScmVM *vm)
{
    ScmObj head = SCM_NIL, tail;
    int i;
    if (vm->numVals == 0) return SCM_NIL;
    SCM_APPEND1(head, tail, vm->val0);
    for (i=1; i<vm->numVals; i++) {
        SCM_APPEND1(head, tail, vm->vals[i-1]);
    }
    return head;
}

void Scm_VMSetResult(ScmObj obj)
{
    ScmVM *vm = theVM;
    vm->val0 = obj;
    vm->numVals = 1;
}

/*
 * Current VM.
 */
ScmVM *Scm_VM(void)
{
    return theVM;
}

/*
 * Clean up VM when thread exits.
 * NB: when this callback is invoked, pthread's thread specific data
 * is already cleared, so theVM returns NULL.  We can't do much---even
 * Scm_Error is not available.  
 */
#ifdef GAUCHE_USE_PTHREAD
static void vm_cleanup(void *data)
{
    ScmVM *vm = SCM_VM(data);
    /* Remove this VM from the global list. */
    if (pthread_mutex_lock(&vmListMutex) == EDEADLK) {
        Scm_Panic("dead lock in vm_cleanup.");
    }
    /* NB: Assuming Scm_DeleteX doesn't throw an error for EQ comparison. */
    vmList = Scm_DeleteX(SCM_OBJ(vm), vmList, SCM_CMP_EQ);
    pthread_mutex_unlock(&vmListMutex);

    /* Change this VM state to TERMINATED, and signals the change
       to the waiting threads. */
    if (pthread_mutex_lock(&vm->vmlock) == EDEADLK) {
        Scm_Panic("dead lock in vm_cleanup.");
    }
    vm->state = SCM_VM_TERMINATED;
    pthread_cond_broadcast(&vm->cond);
    pthread_mutex_unlock(&vm->vmlock);
}
#endif /* GAUCHE_USE_PTHREAD */

/*
 * Initialization.  This should be called after modules are initialized.
 */
void Scm__InitVM(void)
{
#ifdef GAUCHE_USE_PTHREAD
    if (pthread_key_create(&vm_key, vm_cleanup) != 0) {
        Scm_Panic("pthread_key_create failed.");
    }
    rootVM = Scm_NewVM(NULL, Scm_SchemeModule(),
                       SCM_MAKE_STR_IMMUTABLE("root"));
    if (pthread_setspecific(vm_key, rootVM) != 0) {
        Scm_Panic("pthread_setspecific failed.");
    }
    rootVM->thread = pthread_self();
#else   /* !GAUCHE_USE_PTHREAD */
    rootVM = theVM = Scm_NewVM(NULL, Scm_SchemeModule(),
                               SCM_MAKE_STR_IMMUTABLE("root"));
#endif  /* !GAUCHE_USE_PTHREAD */
    rootVM->state = SCM_VM_RUNNABLE;
    vmList = SCM_LIST1(SCM_OBJ(rootVM));
    (void)SCM_INTERNAL_MUTEX_INIT(vmListMutex);

    boundaryFrameMark = SCM_MAKE_STR("boundary-frame");
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
#define DECL_REGS             DECL_REGS_INT(/**/)
#define DECL_REGS_VOLATILE    DECL_REGS_INT(volatile)

#define DECL_REGS_INT(VOLATILE)                 \
    ScmVM *VOLATILE vm = theVM;                 \
    VOLATILE ScmObj pc = vm->pc;                \
    ScmContFrame *VOLATILE cont = vm->cont;     \
    ScmEnvFrame *VOLATILE env = vm->env;        \
    ScmEnvFrame *VOLATILE argp = vm->argp;      \
    ScmObj *VOLATILE sp = vm->sp;               \
    VOLATILE ScmObj val0 = vm->val0

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

/* return true if ptr points into the stack area */
#define IN_STACK_P(ptr)                         \
    ((ptr) >= vm->stackBase && (ptr) < vm->stackEnd)

#define RESTORE_REGS()                          \
    do {                                        \
        pc = vm->pc;                            \
        env = vm->env;                          \
        argp = vm->argp;                        \
        cont = vm->cont;                        \
        sp = vm->sp;                            \
    } while (0)

/* Check if stack has room at least size bytes. */
#define CHECK_STACK(size)                       \
    do {                                        \
        if (sp >= vm->stackEnd - size) {        \
            SAVE_REGS();                        \
            save_stack(vm);                     \
            RESTORE_REGS();                     \
        }                                       \
    } while (0)

/* Push a continuation frame.  next_pc is the PC from where execution
   will be resumed.  */
#define PUSH_CONT(old_pc, next_pc)                      \
    do {                                                \
        ScmContFrame *newcont = (ScmContFrame*)sp;      \
        newcont->prev = cont;                           \
        newcont->env = env;                             \
        newcont->argp = argp;                           \
        newcont->size = sp - (ScmObj*)argp;             \
        newcont->pc = next_pc;                          \
        newcont->info = old_pc;                         \
        cont = newcont;                                 \
        sp += CONT_FRAME_SIZE;                          \
        argp = (ScmEnvFrame*)sp;                        \
    } while (0)

/* pop a continuation frame, i.e. return from a procedure. */
#define POP_CONT()                                                      \
    do {                                                                \
        if (cont->argp == NULL) {                                       \
            void *data__[SCM_CCONT_DATA_SIZE];                          \
            ScmObj (*after__)(ScmObj, void**);                          \
            memcpy(data__, (ScmObj*)cont + CONT_FRAME_SIZE,             \
                   cont->size * sizeof(void*));                         \
            after__ = (ScmObj (*)(ScmObj, void**))cont->pc;             \
            if (IN_STACK_P((ScmObj*)cont)) sp = (ScmObj*)cont;          \
            env = cont->env;                                            \
            argp = (ScmEnvFrame*)sp;                                    \
            pc = SCM_NIL;                                               \
            cont = cont->prev;                                          \
            SAVE_REGS();                                                \
            val0 = after__(val0, data__);                               \
            RESTORE_REGS();                                             \
        } else if (IN_STACK_P((ScmObj*)cont)) {                         \
            sp   = (ScmObj*)cont->argp + cont->size;                    \
            env  = cont->env;                                           \
            argp = cont->argp;                                          \
            pc   = cont->pc;                                            \
            cont = cont->prev;                                          \
        } else {                                                        \
            int size__ = cont->size;                                    \
            sp = vm->stackBase;                                         \
            env = cont->env;                                            \
            pc = cont->pc;                                              \
            if (cont->argp) {                                           \
                memmove(sp, cont->argp, size__*sizeof(ScmObj*));        \
                argp = (ScmEnvFrame *)sp;                               \
                sp = (ScmObj*)argp + size__;                            \
            }                                                           \
            cont = cont->prev;                                          \
        }                                                               \
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
        if (sp < vm->stackBase)                 \
            sp = vm->stackBase;                 \
    } while (0)

#define VM_DUMP(delimiter)                      \
    SAVE_REGS();                                \
    fprintf(stderr, delimiter);                 \
    Scm_VMDump(theVM)

#define VM_ASSERT(expr)                                                 \
    do {                                                                \
        if (!(expr))  {                                                 \
            SAVE_REGS();                                                \
            fprintf(stderr, "\"%s\", line %d: Assertion failed: %s\n",  \
                    __FILE__, __LINE__, #expr);                         \
            Scm_VMDump(theVM);                                          \
            Scm_Panic("exitting...\n");                                 \
        }                                                               \
    } while (0)

#define VM_ERR(errargs)                         \
   do {                                         \
      pc = prevpc;                              \
      SAVE_REGS();                              \
      Scm_Error errargs;                        \
   } while (0)

/* to take advantage of GCC's `computed goto' feature
   (see gcc.info, "Labels as Values") */
#ifdef __GNUC__
#define SWITCH(val) goto *dispatch_table[val];
#define CAT2(a, b)  a##b
#define CASE(insn)  CAT2(LABEL_, insn) :
#define DEFAULT     LABEL_DEFAULT :
#else /* !__GNUC__ */
#define SWITCH(val) switch (val)
#define CASE(insn)  case insn :
#define DEFAULT     default :
#endif

/* check the argument count is OK for call to PROC.  if PROC takes &rest
 * args, fold those arguments to the list.  Returns adjusted size of
 * the argument frame.
 */
#define ADJUST_ARGUMENT_FRAME(proc, caller_args, callee_args)           \
    do {                                                                \
    int i, reqargs, restarg;                                            \
                                                                        \
    reqargs = SCM_PROCEDURE_REQUIRED(proc);                             \
    restarg = SCM_PROCEDURE_OPTIONAL(proc);                             \
    callee_args  = reqargs + (restarg? 1 : 0);                          \
                                                                        \
    if (restarg) {                                                      \
        ScmObj p = SCM_NIL, a;                                          \
        if (caller_args < reqargs) {                                    \
            SAVE_REGS();                                                \
            Scm_Error("wrong number of arguments for %S"                \
                      " (required %d, got %d)",                         \
                      proc, reqargs, caller_args);                      \
        }                                                               \
        /* fold &rest args */                                           \
        for (i = reqargs; i < caller_args; i++) {                       \
            POP_ARG(a);                                                 \
            p = Scm_Cons(a, p);                                         \
        }                                                               \
        argp->data[reqargs] = p;                                        \
        sp = (ScmObj*)argp + ENV_SIZE(callee_args);                     \
    } else {                                                            \
        if (caller_args != reqargs) {                                   \
            SAVE_REGS();                                                \
            Scm_Error("wrong number of arguments for %S"                \
                      " (required %d, got %d)",                         \
                      proc, reqargs, caller_args);                      \
        }                                                               \
    }                                                                   \
    argp->info = SCM_PROCEDURE_INFO(proc);                              \
    argp->size = callee_args;                                           \
    } while (0)

/* Signal check */
#define SIGCHECK                                        \
    do {                                                \
        if (vm->sigQueueTail != vm->sigQueueHead) {     \
            Scm_SigCheck(vm);                           \
        }                                               \
    } while (0)

/*
 * main loop of VM
 */
static void run_loop()
{
    DECL_REGS;
    ScmObj code = SCM_NIL;
    ScmObj prevpc = SCM_NIL;

#ifdef __GNUC__
    static void *dispatch_table[256] = {
#define DEFINSN(insn, name, nargs)   && CAT2(LABEL_, insn),
#include "gauche/vminsn.h"
#undef DEFINSN
    };
#endif /* __GNUC__ */    
    
    for (;;) {
        /*VM_DUMP("");*/
        SIGCHECK;

        /* See if we're at the end of procedure.  It's safer to use
           !SCM_PAIRP(pc) than SCM_NULLP(pc), but the latter is faster.
           (the former makes nqueen.scm 2% slower) */
        if (SCM_NULLP(pc)) {
            if (cont == NULL || BOUNDARY_FRAME_P(cont)) {
                SAVE_REGS();
                return; /* no more continuations */
            }
            POP_CONT();
            continue;
        }

        prevpc = pc;
        FETCH_INSN(code);
        
        if (!SCM_VM_INSNP(code)) {
            /* literal object */
            val0 = code;
            vm->numVals = 1;
            continue;
        }

        /* VM instructions */
        SWITCH(SCM_VM_INSN_CODE(code)) {

            CASE(SCM_VM_PUSH) {
                CHECK_STACK(0);
                PUSH_ARG(val0);
                continue;
            }
            CASE(SCM_VM_POP) {
                POP_ARG(val0);
                continue;
            }
            CASE(SCM_VM_DUP) {
                ScmObj arg = *(sp-1);
                CHECK_STACK(0);
                PUSH_ARG(arg);
                continue;
            }
            CASE(SCM_VM_PRE_CALL) {
                ScmObj prep = SCM_CAR(pc), next = SCM_CDR(pc);
                /* Note: The call instruction will push extra word for
                   next-method, hence +1 */
                int reqstack = CONT_FRAME_SIZE+ENV_SIZE(SCM_VM_INSN_ARG(code))+1;
                CHECK_STACK(reqstack);
                if (!SCM_NULLP(next)) {
                    PUSH_CONT(prevpc, next);
                }
                PUSH_ENV_HDR();
                pc = prep;
                continue;
            }
            CASE(SCM_VM_PRE_TAIL) {
                /* Note: The call instruction will push extra word for
                   next-method, hence +1 */
                int reqstack = ENV_SIZE(SCM_VM_INSN_ARG(code))+1;
                CHECK_STACK(reqstack);
                PUSH_ENV_HDR();
                continue;
            }
            CASE(SCM_VM_CHECK_STACK) {
                int reqstack = SCM_VM_INSN_ARG(code);
                CHECK_STACK(reqstack);
                continue;
            }
            CASE(SCM_VM_TAIL_CALL) ; /* FALLTHROUGH */
            CASE(SCM_VM_CALL) {
                int nargs = SCM_VM_INSN_ARG(code);
                int argcnt, proctype;
                ScmObj nm = SCM_FALSE;

                if (!SCM_PROCEDUREP(val0)) VM_ERR(("bad procedure: %S", val0));
                /*
                 * Step 1. Preprocess for generic function
                 */
                proctype = SCM_PROCEDURE_TYPE(val0);
                if (proctype == SCM_PROC_GENERIC) {
                    if (SCM_GENERICP(val0)) {
                        /* pure generic application */
                        ScmObj mm
                            = Scm_ComputeApplicableMethods(SCM_GENERIC(val0),
                                                           argp->data, nargs);
                        if (!SCM_NULLP(mm)) {
                            mm = Scm_SortMethods(mm, argp->data, nargs);
                            nm = Scm_MakeNextMethod(SCM_GENERIC(val0),
                                                    SCM_CDR(mm),
                                                    argp->data, nargs, TRUE);
                            val0 = SCM_CAR(mm);
                            proctype = SCM_PROC_METHOD;
                        }
                    } else {
                        /* use scheme-defined MOP */
                        ScmObj args = SCM_NIL, arg;
                        int i;
                        for (i=0; i<nargs; i++) {
                            POP_ARG(arg);
                            args = Scm_Cons(arg, args);
                        }
                        Scm_VMApply2(SCM_OBJ(&Scm_GenericApplyGeneric),
                                     val0, args);
                        continue;
                    }
                } else if (proctype == SCM_PROC_NEXT_METHOD) {
                    ScmNextMethod *n = SCM_NEXT_METHOD(val0);
                    if (nargs == 0) {
                        CHECK_STACK(n->nargs+1);
                        memcpy(sp, n->args, sizeof(ScmObj)*n->nargs);
                        sp += n->nargs;
                        nargs = n->nargs;
                    }
                    if (SCM_NULLP(n->methods)) {
                        val0 = SCM_OBJ(n->generic);
                        proctype = SCM_PROC_GENERIC;
                    } else {
                        nm = Scm_MakeNextMethod(n->generic,
                                                SCM_CDR(n->methods),
                                                argp->data, nargs, TRUE);
                        val0 = SCM_CAR(n->methods);
                        proctype = SCM_PROC_METHOD;
                    }
                }
                /*
                 * Step 2. Prepare argument frame
                 */
                if (proctype != SCM_PROC_GENERIC) {
                    ADJUST_ARGUMENT_FRAME(val0, nargs, argcnt);
                } else {
                    argcnt = nargs;
                }
                if (SCM_VM_INSN_CODE(code)==SCM_VM_TAIL_CALL) {
                    /* discard the caller's argument frame, and shift
                       the callee's argument frame there. */
                    ScmObj *to;
                    if (IN_STACK_P((ScmObj*)cont)) {
                        if (cont->argp) {
                            to = (ScmObj*)cont + CONT_FRAME_SIZE;
                        } else {
                            /* C continuation */
                            to = (ScmObj*)cont + CONT_FRAME_SIZE + cont->size;
                        }
                    } else {
                        /* continuation has been saved, which means the
                           stack has no longer useful information. */
                        to = vm->stackBase;
                    }
                    memmove(to, argp, ENV_SIZE(argcnt)*sizeof(ScmObj));
                    argp = (ScmEnvFrame*)to;
                    sp = to + ENV_SIZE(argcnt);
                }
                vm->numVals = 1; /* default */

                /*
                 * Step 3. Call
                 */
                env = argp;
                argp = (ScmEnvFrame*)sp;
                if (proctype == SCM_PROC_SUBR) {
                    SAVE_REGS();
                    val0 = SCM_SUBR(val0)->func(env->data, argcnt,
                                                SCM_SUBR(val0)->data);
                    RESTORE_REGS();
                } else if (proctype == SCM_PROC_CLOSURE) {
                    env->up = SCM_CLOSURE(val0)->env;
                    pc = SCM_CLOSURE(val0)->code;
                } else if (proctype == SCM_PROC_GENERIC) {
                    /* we have no applicable methods.  call fallback fn. */
                    SAVE_REGS();
                    val0 = SCM_GENERIC(val0)->fallback(env->data,
                                                       nargs,
                                                       SCM_GENERIC(val0));
                    RESTORE_REGS();
                } else if (proctype == SCM_PROC_METHOD) {
                    ScmMethod *m = SCM_METHOD(val0);
                    VM_ASSERT(!SCM_FALSEP(nm));
                    if (m->func) {
                        /* C-defined method */
                        SAVE_REGS();
                        val0 = m->func(SCM_NEXT_METHOD(nm),
                                       env->data,
                                       argcnt,
                                       m->data);
                        RESTORE_REGS();
                    } else {
                        /* Scheme-defined method.  next-method arg is passed
                           as the last arg (note that rest arg is already
                           folded. */
                        PUSH_ARG(SCM_OBJ(nm));
                        env->up = m->env;
                        env->size++; /* for next-method */
                        pc = SCM_OBJ(m->data);
                    }
                } else {
                    Scm_Panic("(VM_CALL) something wrong internally");
                }
                continue;
            }
            CASE(SCM_VM_GREF) {
                ScmGloc *gloc;
                
                VM_ASSERT(SCM_PAIRP(pc));
                val0 = SCM_CAR(pc);

                if (!SCM_GLOCP(val0)) {
                    VM_ASSERT(SCM_IDENTIFIERP(val0));
                    gloc = Scm_FindBinding(SCM_IDENTIFIER(val0)->module,
                                           SCM_IDENTIFIER(val0)->name,
                                           FALSE);
                    if (gloc == NULL) {
                        VM_ERR(("unbound variable: %S",
                                SCM_IDENTIFIER(val0)->name));
                    }
                    /* memorize gloc */
                    /* TODO: make it MT safe! */
                    SCM_SET_CAR(pc, SCM_OBJ(gloc));
                } else {
                    gloc = SCM_GLOC(val0);
                }
                val0 = SCM_GLOC_GET(gloc);
                if (val0 == SCM_UNBOUND) {
                    VM_ERR(("unbound variable: %S", SCM_OBJ(gloc->name)));
                } else if (SCM_AUTOLOADP(val0)) {
                    SAVE_REGS();
                    val0 = Scm_LoadAutoload(SCM_AUTOLOAD(val0));
                    RESTORE_REGS();
                }
                pc = SCM_CDR(pc);
                continue;
            }
            CASE(SCM_VM_LREF0) { val0 = env->data[0]; continue; }
            CASE(SCM_VM_LREF1) { val0 = env->data[1]; continue; }
            CASE(SCM_VM_LREF2) { val0 = env->data[2]; continue; }
            CASE(SCM_VM_LREF3) { val0 = env->data[3]; continue; }
            CASE(SCM_VM_LREF4) { val0 = env->data[4]; continue; }
            CASE(SCM_VM_LREF10) { val0 = env->up->data[0]; continue; }
            CASE(SCM_VM_LREF11) { val0 = env->up->data[1]; continue; }
            CASE(SCM_VM_LREF12) { val0 = env->up->data[2]; continue; }
            CASE(SCM_VM_LREF13) { val0 = env->up->data[3]; continue; }
            CASE(SCM_VM_LREF14) { val0 = env->up->data[4]; continue; }
            CASE(SCM_VM_LREF) {
                int dep = SCM_VM_INSN_ARG0(code);
                int off = SCM_VM_INSN_ARG1(code);
                ScmEnvFrame *e = env;

                for (; dep > 0; dep--) {
                    VM_ASSERT(e != NULL);
                    e = e->up;
                }
                VM_ASSERT(e != NULL);
                VM_ASSERT(e->size > off);
                val0 = e->data[off];
                continue;
            }
            CASE(SCM_VM_TAILBIND) {
                ScmObj *to, *from;
                ScmObj info;
                int env_size;
                FETCH_INSN(info); /* dummy info. discard it for now. */
                
                /* shift env frame. */
                /* TODO: check if continuation was captured */
                argp->size = env->size;
                argp->up = env->up;
                argp->info = env->info;
                to = (ScmObj*)argp - ENV_SIZE(argp->size) - CONT_FRAME_SIZE;
                from = (ScmObj *)argp;
                env_size = env->size;

                POP_CONT();     /* recover argp */
                memmove(to, from, ENV_SIZE(env_size) * sizeof(ScmObj *));
                env = (ScmEnvFrame *)to;
                sp = to + ENV_SIZE(env_size);
                continue;
            }
            CASE(SCM_VM_LET) {
                int nlocals = SCM_VM_INSN_ARG(code);
                int size = CONT_FRAME_SIZE + ENV_SIZE(nlocals);
                ScmObj info, body;
                CHECK_STACK(size);
                VM_ASSERT(SCM_PAIRP(pc));
                FETCH_INSN(info);
                VM_ASSERT(SCM_PAIRP(pc));
                FETCH_INSN(body);
                if (!SCM_NULLP(pc)) {
                    PUSH_CONT(prevpc, pc);
                }
                PUSH_LOCAL_ENV(nlocals, info);
                pc = body;
                continue;
            }
            CASE(SCM_VM_POPENV) {
                VM_ASSERT(env != NULL);
                POP_LOCAL_ENV();
                continue;
            }
            CASE(SCM_VM_GSET) {
                ScmObj loc;
                VM_ASSERT(SCM_PAIRP(pc));
                
                loc = SCM_CAR(pc);
                if (SCM_GLOCP(loc)) {
                    SCM_GLOC_SET(SCM_GLOC(loc), val0);
                } else {
                    ScmGloc *gloc;
                    VM_ASSERT(SCM_IDENTIFIERP(loc));
                    /* The third arg of this call should be TRUE
                       (stay_in_module).  See the discussion about modules
                       in compile.c.  For now, this one is more convenient. */
                    gloc = Scm_FindBinding(SCM_IDENTIFIER(loc)->module,
                                           SCM_IDENTIFIER(loc)->name,
                                           FALSE);
                    if (gloc == NULL) VM_ERR(("symbol not defined: %S", loc));
                    SCM_GLOC_SET(gloc, val0);
                    /* memorize gloc */
                    /* TODO: make it MT safe! */
                    SCM_SET_CAR(pc, SCM_OBJ(gloc));
                }
                pc = SCM_CDR(pc);
                continue;
            }
            CASE(SCM_VM_LSET0) { env->data[0] = val0; continue; }
            CASE(SCM_VM_LSET1) { env->data[1] = val0; continue; }
            CASE(SCM_VM_LSET2) { env->data[2] = val0; continue; }
            CASE(SCM_VM_LSET3) { env->data[3] = val0; continue; }
            CASE(SCM_VM_LSET4) { env->data[4] = val0; continue; }
            CASE(SCM_VM_LSET) {
                int dep = SCM_VM_INSN_ARG0(code);
                int off = SCM_VM_INSN_ARG1(code);
                ScmEnvFrame *e = env;

                for (; dep > 0; dep--) {
                    VM_ASSERT(e != NULL);
                    e = e->up;
                }
                VM_ASSERT(e != NULL);
                VM_ASSERT(e->size > off);
                e->data[off] = val0;
                continue;
            }
            CASE(SCM_VM_NOP) {
                continue;
            }
            CASE(SCM_VM_DEFINE) {
                ScmObj var; ScmSymbol *name;
                VM_ASSERT(SCM_PAIRP(pc));
                var = SCM_CAR(pc);
                VM_ASSERT(SCM_IDENTIFIERP(var));
                pc = SCM_CDR(pc);
                Scm_Define(SCM_IDENTIFIER(var)->module,
                           (name = SCM_IDENTIFIER(var)->name), val0);
                val0 = SCM_OBJ(name);
                continue;
            }
            CASE(SCM_VM_DEFINE_CONST) {
                ScmObj var; ScmSymbol *name;
                VM_ASSERT(SCM_PAIRP(pc));
                var = SCM_CAR(pc);
                VM_ASSERT(SCM_IDENTIFIERP(var));
                pc = SCM_CDR(pc);
                Scm_DefineConst(SCM_IDENTIFIER(var)->module,
                                (name = SCM_IDENTIFIER(var)->name), val0);
                val0 = SCM_OBJ(name);
                continue;
            }
            CASE(SCM_VM_IF) {
                if (SCM_FALSEP(val0)) { pc = SCM_CDR(pc); }
                else                  { pc = SCM_CAR(pc); }
                continue;
            }
            CASE(SCM_VM_LAMBDA) {
                ScmObj info, body;
                
                VM_ASSERT(SCM_PAIRP(pc));
                info = SCM_CAR(pc);
                pc = SCM_CDR(pc);
                VM_ASSERT(SCM_PAIRP(pc));
                body = SCM_CAR(pc);
                pc = SCM_CDR(pc);

                /* preserve environment */
                SAVE_REGS();
                val0 = Scm_MakeClosure(SCM_VM_INSN_ARG0(code),
                                       SCM_VM_INSN_ARG1(code),
                                       body,
                                       info);
                RESTORE_REGS();
                continue;
            }
            CASE(SCM_VM_VALUES_BIND) {
                /* TODO: clean up stack management */
                int reqargs = SCM_VM_INSN_ARG0(code);
                int restarg = SCM_VM_INSN_ARG1(code);
                int size = CONT_FRAME_SIZE + ENV_SIZE(reqargs + restarg);
                int i = 0, argsize;
                ScmObj rest = SCM_NIL, tail = SCM_NIL, info, body;

                CHECK_STACK(size);
                FETCH_INSN(info);
                if (vm->numVals < reqargs) {
                    VM_ERR(("received fewer values than expected"));
                } else if (!restarg && vm->numVals > reqargs) {
                    VM_ERR(("received more values than expected"));
                }
                argsize = reqargs + (restarg? 1 : 0);
                FETCH_INSN(body);

                if (!SCM_NULLP(pc)) {
                    PUSH_CONT(prevpc, pc);
                }

                PUSH_ENV_HDR();
                if (reqargs > 0) {
                    PUSH_ARG(val0);
                    i++;
                } else if (restarg && vm->numVals > 0) {
                    SCM_APPEND1(rest, tail, val0);
                    i++;
                }
                for (; i < reqargs; i++) {
                    PUSH_ARG(vm->vals[i-1]);
                }
                if (restarg) {
                    for (; i < vm->numVals; i++) {
                        SCM_APPEND1(rest, tail, vm->vals[i-1]);
                    }
                    PUSH_ARG(rest);
                }
                vm->numVals = 1;

                argp->up = env;
                argp->size = argsize;
                argp->info = info;
                env = argp;
                argp = (ScmEnvFrame *)sp;
                pc = body;
                continue;
            }
            CASE(SCM_VM_QUOTE_INSN) {
                VM_ASSERT(SCM_PAIRP(pc));
                val0 = SCM_CAR(pc);
                pc = SCM_CDR(pc);
                continue;
            }

            /* Inlined procedures */
            CASE(SCM_VM_CONS) {
                ScmObj ca;
                POP_ARG(ca);
                SAVE_REGS();
                val0 = Scm_Cons(ca, val0);
                continue;
            }
            CASE(SCM_VM_CAR) {
                if (!SCM_PAIRP(val0))
                    VM_ERR(("pair required, but got %S", val0));
                val0 = SCM_CAR(val0);
                continue;
            }
            CASE(SCM_VM_CDR) {
                if (!SCM_PAIRP(val0))
                    VM_ERR(("pair required, but got %S", val0));
                val0 = SCM_CDR(val0);
                continue;
            }
            CASE(SCM_VM_LIST) {
                int nargs = SCM_VM_INSN_ARG(code);
                ScmObj cp = SCM_NIL;
                if (nargs > 0) {
                    ScmObj arg;
                    SAVE_REGS();
                    cp = Scm_Cons(val0, cp);
                    while (--nargs > 0) {
                        POP_ARG(arg);
                        SAVE_REGS();
                        cp = Scm_Cons(arg, cp);
                    }
                }
                val0 = cp;
                continue;
            }
            CASE(SCM_VM_LIST_STAR) {
                int nargs = SCM_VM_INSN_ARG(code);
                ScmObj cp = SCM_NIL;
                if (nargs > 0) {
                    ScmObj arg;
                    cp = val0;
                    while (--nargs > 0) {
                        POP_ARG(arg);
                        SAVE_REGS();
                        cp = Scm_Cons(arg, cp);
                    }
                }
                val0 = cp;
                continue;
            }
            CASE(SCM_VM_NOT) {
                val0 = SCM_MAKE_BOOL(SCM_FALSEP(val0));
                continue;
            }
            CASE(SCM_VM_NULLP) {
                val0 = SCM_MAKE_BOOL(SCM_NULLP(val0));
                continue;
            }
            CASE(SCM_VM_EQ) {
                ScmObj item;
                POP_ARG(item);
                val0 = SCM_MAKE_BOOL(item == val0);
                continue;
            }
            CASE(SCM_VM_EQV) {
                ScmObj item;
                POP_ARG(item);
                SAVE_REGS();
                val0 = SCM_MAKE_BOOL(Scm_EqvP(item, val0));
                continue;
            }
            CASE(SCM_VM_MEMQ) {
                ScmObj item;
                POP_ARG(item);
                SAVE_REGS();
                val0 = Scm_Memq(item, val0);
                continue;
            }
            CASE(SCM_VM_MEMV) {
                ScmObj item;
                POP_ARG(item);
                SAVE_REGS();
                val0 = Scm_Memv(item, val0);
                continue;
            }
            CASE(SCM_VM_ASSQ) {
                ScmObj item;
                POP_ARG(item);
                SAVE_REGS();
                val0 = Scm_Assq(item, val0);
                continue;
            }
            CASE(SCM_VM_ASSV) {
                ScmObj item;
                POP_ARG(item);
                SAVE_REGS();
                val0 = Scm_Assv(item, val0);
                continue;
            }
            CASE(SCM_VM_PAIRP) {
                val0 = SCM_MAKE_BOOL(SCM_PAIRP(val0));
                continue;
            }
            CASE(SCM_VM_CHARP) {
                val0 = SCM_MAKE_BOOL(SCM_CHARP(val0));
                continue;
            }
            CASE(SCM_VM_EOFP) {
                val0 = SCM_MAKE_BOOL(SCM_EOFP(val0));
                continue;
            }
            CASE(SCM_VM_STRINGP) {
                val0 = SCM_MAKE_BOOL(SCM_STRINGP(val0));
                continue;
            }
            CASE(SCM_VM_SYMBOLP) {
                val0 = SCM_MAKE_BOOL(SCM_SYMBOLP(val0));
                continue;
            }
            CASE(SCM_VM_APPEND) {
                int nargs = SCM_VM_INSN_ARG(code);
                ScmObj cp = SCM_NIL, arg;
                if (nargs > 0) {
                    cp = val0;
                    while (--nargs > 0) {
                        POP_ARG(arg);
                        SAVE_REGS();
                        if (Scm_Length(arg) < 0)
                            VM_ERR(("list required, but got %S\n", arg));
                        cp = Scm_Append2(arg, cp);
                    }
                }
                val0 = cp;
                continue;
            }
            CASE(SCM_VM_REVERSE) {
                SAVE_REGS();
                val0 = Scm_Reverse(val0);
                continue;
            }
            CASE(SCM_VM_PROMISE) {
                SAVE_REGS();
                val0 = Scm_MakePromise(val0);
                continue;
            }
            CASE(SCM_VM_SETTER) {
                SAVE_REGS();
                if (!SCM_PROCEDUREP(val0))
                    VM_ERR(("procedure required, but got %S\n", val0));
                val0 = Scm_Setter(SCM_PROCEDURE(val0));
                RESTORE_REGS();
                continue;
            }
            CASE(SCM_VM_VALUES) {
                int nargs = SCM_VM_INSN_ARG(code), i;
                if (nargs >= SCM_VM_MAX_VALUES)
                    VM_ERR(("values got too many args"));
                VM_ASSERT(nargs -1 <= sp - vm->stackBase);
                if (nargs > 0) {
                    for (i = nargs-1; i>0; i--) {
                        vm->vals[i-1] = val0;
                        POP_ARG(val0);
                    }
                }
                vm->numVals = nargs;
                continue;
            }
            CASE(SCM_VM_VEC) {
                int nargs = SCM_VM_INSN_ARG(code), i;
                ScmObj vec;
                SAVE_REGS();
                vec = Scm_MakeVector(nargs, SCM_UNDEFINED);
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
            CASE(SCM_VM_APP_VEC) {
                int nargs = SCM_VM_INSN_ARG(code);
                ScmObj cp = SCM_NIL, arg;
                if (nargs > 0) {
                    cp = val0;
                    while (--nargs > 0) {
                        POP_ARG(arg);
                        SAVE_REGS();
                        if (Scm_Length(arg) < 0)
                            VM_ERR(("list required, but got %S\n", arg));
                        cp = Scm_Append2(arg, cp);
                    }
                }
                SAVE_REGS();
                val0 = Scm_ListToVector(cp);
                continue;
            }
            CASE(SCM_VM_VEC_LEN) {
                int siz;
                if (!SCM_VECTORP(val0))
                    VM_ERR(("vector expected, but got %S\n", val0));
                siz = SCM_VECTOR_SIZE(val0);
                val0 = SCM_MAKE_INT(siz);
                continue;
            }
            CASE(SCM_VM_VEC_REF) {
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
            CASE(SCM_VM_VEC_SET) {
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
            CASE(SCM_VM_NUMEQ2) {
                ScmObj arg;
                POP_ARG(arg);
                if (SCM_INTP(val0) && SCM_INTP(arg)) {
                    val0 = SCM_MAKE_BOOL(val0 == arg);
                } else {
                    SAVE_REGS();
                    val0 = SCM_MAKE_BOOL(Scm_NumEq(arg, val0));
                }
                continue;
            }
            CASE(SCM_VM_NUMLT2) {
                ScmObj arg;
                POP_ARG(arg);
                SAVE_REGS();
                val0 = SCM_MAKE_BOOL(Scm_NumCmp(arg, val0) < 0);
                continue;
            }
            CASE(SCM_VM_NUMLE2) {
                ScmObj arg;
                POP_ARG(arg);
                SAVE_REGS();
                val0 = SCM_MAKE_BOOL(Scm_NumCmp(arg, val0) <= 0);
                continue;
            }
            CASE(SCM_VM_NUMGT2) {
                ScmObj arg;
                POP_ARG(arg);
                SAVE_REGS();
                val0 = SCM_MAKE_BOOL(Scm_NumCmp(arg, val0) > 0);
                continue;
            }
            CASE(SCM_VM_NUMGE2) {
                ScmObj arg;
                POP_ARG(arg);
                SAVE_REGS();
                val0 = SCM_MAKE_BOOL(Scm_NumCmp(arg, val0) >= 0);
                continue;
            }
            CASE(SCM_VM_NUMADD2) {
                ScmObj arg;
                POP_ARG(arg);
                SAVE_REGS();
                val0 = Scm_Add(arg, val0, SCM_NIL);
                continue;
            }
            CASE(SCM_VM_NUMSUB2) {
                ScmObj arg;
                POP_ARG(arg);
                SAVE_REGS();
                val0 = Scm_Subtract(arg, val0, SCM_NIL);
                continue;
            }
            CASE(SCM_VM_NUMADDI) {
                long imm = SCM_VM_INSN_ARG(code);
                if (SCM_INTP(val0)) {
                    imm += SCM_INT_VALUE(val0);
                    if (SCM_SMALL_INT_FITS(imm)) {
                        val0 = SCM_MAKE_INT(imm);
                    } else {
                        SAVE_REGS();
                        val0 = Scm_MakeInteger(imm);
                    }
                } else {
                    SAVE_REGS();
                    val0 = Scm_Add(SCM_MAKE_INT(imm), val0, SCM_NIL);
                }
                continue;
            }
            CASE(SCM_VM_NUMSUBI) {
                long imm = SCM_VM_INSN_ARG(code);
                if (SCM_INTP(val0)) {
                    imm -= SCM_INT_VALUE(val0);
                    if (SCM_SMALL_INT_FITS(imm)) {
                        val0 = SCM_MAKE_INT(imm);
                    } else {
                        SAVE_REGS();
                        val0 = Scm_MakeInteger(imm);
                    }
                } else {
                    SAVE_REGS();
                    val0 = Scm_Subtract(SCM_MAKE_INT(imm), val0, SCM_NIL);
                }
                continue;
            }
            CASE(SCM_VM_READ_CHAR) {
                int nargs = SCM_VM_INSN_ARG(code), ch = 0;
                ScmPort *port;
                if (nargs == 1) {
                    if (!SCM_IPORTP(val0))
                        VM_ERR(("read-char: input port required: %S", val0));
                    port = SCM_PORT(val0);
                } else {
                    port = SCM_CURIN;
                }
                SAVE_REGS();
                SCM_GETC(ch, port);
                RESTORE_REGS();
                val0 = (ch < 0)? SCM_EOF : SCM_MAKE_CHAR(ch);
                continue;
            }
            CASE(SCM_VM_WRITE_CHAR) {
                int nargs = SCM_VM_INSN_ARG(code);
                ScmObj ch;
                ScmPort *port;
                if (nargs == 2) {
                    if (!SCM_OPORTP(val0))
                        VM_ERR(("write-char: output port required: %S", val0));
                    port = SCM_PORT(val0);
                    POP_ARG(ch);
                } else {
                    port = SCM_CUROUT;
                    ch = val0;
                }
                if (!SCM_CHARP(ch))
                    VM_ERR(("write-char: character required: %S", ch));
                SAVE_REGS();
                SCM_PUTC(SCM_CHAR_VALUE(ch), port);
                RESTORE_REGS();
                val0 = SCM_MAKE_INT(1);
                continue;
            }
            CASE(SCM_VM_SLOT_REF) {
                ScmObj obj;
                POP_ARG(obj);
                CHECK_STACK(CONT_FRAME_SIZE);
                PUSH_CONT(prevpc, pc);
                SAVE_REGS();
                val0 = Scm_VMSlotRef(obj, val0, FALSE);
                RESTORE_REGS();
                pc = SCM_NIL;
                continue;
            }
            CASE(SCM_VM_SLOT_SET) {
                ScmObj obj, slot;
                POP_ARG(slot);
                POP_ARG(obj);
                CHECK_STACK(CONT_FRAME_SIZE);
                PUSH_CONT(prevpc, pc);
                SAVE_REGS();
                val0 = Scm_VMSlotSet(obj, slot, val0);
                RESTORE_REGS();
                pc = SCM_NIL;
                continue;
            }
#ifndef __GNUC__
            DEFAULT
                Scm_Panic("Illegal vm instruction: %08x",
                          SCM_VM_INSN_CODE(code));
#endif
        }
    }
}
/* End of run_loop */

/*==================================================================
 * Stack management
 */

/* move the current chain of environments from the stack to the heap.
   if there're continuation frames which point to the moved env, those
   pointers are adjusted as well. */
static inline ScmEnvFrame *save_env(ScmVM *vm,
                                    ScmEnvFrame *env_begin,
                                    ScmContFrame *cont_begin)
{
    ScmEnvFrame *e = env_begin, *prev = NULL, *head = env_begin, *s;
    ScmContFrame *c = cont_begin;
    ScmCStack *cstk;
    ScmEscapePoint *eh;
    
    int size;
    for (; IN_STACK_P((ScmObj*)e); e = e->up) {
        size = ENV_SIZE(e->size) * sizeof(ScmObj);
        s = SCM_NEW2(ScmEnvFrame*, size);
        memcpy(s, e, size);
        for (c = cont_begin; c; c = c->prev) {
            if (c->env == e) {
                c->env = s;
            }
        }
        for (cstk = vm->cstack; cstk; cstk = cstk->prev) {
            for (c = cstk->cont; c; c = c->prev) {
                if (!IN_STACK_P((ScmObj*)c)) break;
                if (c->env == e) c->env = s;
            }
        }
        for (eh = vm->escapePoint; eh; eh = eh->prev) {
            for (c = eh->cont; c; c = c->prev) {
                if (!IN_STACK_P((ScmObj*)c)) break;
                if (c->env == e) c->env = s;
            }
        }
        if (e == env_begin) head = s;
        if (prev) prev->up = s;
        prev = s;
    }
    return head;
}

/* Copy the continuation to the heap. 
   Note that the naive copy of the stack won't work, since
   - Environment frame in it may be moved later, and corresponding
     pointers must be adjusted, and
   - The stack frame contains a pointer value to other frames,
     which will be invalid if the continuation is resumed by
     the different thread.
 */
static void save_cont(ScmVM *vm, ScmContFrame *cont_begin)
{
    ScmContFrame *c = cont_begin, *prev = NULL;
    ScmCStack *cstk;
    ScmEscapePoint *ep;
    
    for (; IN_STACK_P((ScmObj*)c); c = c->prev) {
        ScmEnvFrame *e = save_env(vm, c->env, c);
        int size = (CONT_FRAME_SIZE + c->size) * sizeof(ScmObj);
        ScmContFrame *csave = SCM_NEW2(ScmContFrame*, size);

        if (c->env == vm->env) vm->env = e;
        if (c == vm->cont) vm->cont = csave;
        if (c->argp) {
            memcpy(csave, c, CONT_FRAME_SIZE * sizeof(ScmObj));
            memcpy((void**)csave + CONT_FRAME_SIZE,
                   c->argp, c->size * sizeof(ScmObj));
            csave->argp =(ScmEnvFrame*)((void **)csave + CONT_FRAME_SIZE);
        } else {
            /* C continuation */
            memcpy(csave, c, (CONT_FRAME_SIZE + c->size) * sizeof(ScmObj));
        }
        for (cstk = vm->cstack; cstk; cstk = cstk->prev) {
            if (cstk->cont == c) cstk->cont = csave;
        }
        for (ep = vm->escapePoint; ep; ep = ep->prev) {
            if (ep->cont == c) ep->cont = csave;
        }
        if (prev) prev->prev = csave;
        prev = csave;
    }
}

static void save_stack(ScmVM *vm)
{
    ScmObj *p;
#if 0    
    struct timeval t0, t1;
    fprintf(stderr, "save_stack\n");
    gettimeofday(&t0, NULL);
#endif
    
    vm->env = save_env(vm, vm->env, vm->cont);
    save_cont(vm, vm->cont);
    memmove(vm->stackBase, vm->argp,
            (vm->sp - (ScmObj*)vm->argp) * sizeof(ScmObj*));
    vm->sp -= (ScmObj*)vm->argp - vm->stackBase;
    vm->argp = (ScmEnvFrame*)vm->stackBase;
    /* Clear the stack.  This removes bogus pointers and accelerates GC */
    for (p = vm->sp; p < vm->stackEnd; p++) *p = NULL;
#if 0
    gettimeofday(&t1, NULL);
    fprintf(stderr, "elapsed = %d\n",
            (t1.tv_sec - t0.tv_sec)*1000000+(t1.tv_usec - t0.tv_usec));
#endif
}

ScmEnvFrame *Scm_GetCurrentEnv(void)
{
    ScmVM *vm = theVM;
    return vm->env = save_env(vm, vm->env, vm->cont);
}

/*==================================================================
 * Function application from C
 */

/* called from Scm_VMApply, this effectively generates the VM code
   to call proc with args, and sets up the continuation so that
   the code will be executed after the current executing subr returns. */
static void arrange_application(ScmObj proc, ScmObj args, int numargs)
{
    DECL_REGS;
    ScmObj cp, code = SCM_NIL, tail;

    /* This is inefficient, but for now ... */
#if defined(FUNCTION_STACK_CHECK)
    SCM_APPEND1(code, tail, SCM_VM_INSN1(SCM_VM_CHECK_STACK, CONT_FRAME_SIZE+ENV_SIZE(numargs)));
#endif
    SCM_APPEND1(code, tail, SCM_VM_INSN1(SCM_VM_PRE_TAIL, numargs));
    SCM_FOR_EACH(cp, args) {
        if (SCM_VM_INSNP(SCM_CAR(cp))) {
            SCM_APPEND1(code, tail, SCM_VM_INSN(SCM_VM_QUOTE_INSN));
        }
        SCM_APPEND1(code, tail, SCM_CAR(cp));
        SCM_APPEND1(code, tail, SCM_VM_INSN(SCM_VM_PUSH));
    }
    SCM_APPEND1(code, tail, proc);
    SCM_APPEND1(code, tail, SCM_VM_INSN1(SCM_VM_CALL, numargs));
    PUSH_CONT(code, code);
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

/* support proc. for eval.  compile expr in the module nmodule,
   ensuring the env is reset to omodule afterwards */
static ScmObj compile_for_eval(ScmObj expr,
                               ScmModule *nmodule,
                               ScmModule *omodule)
{
    ScmObj v = SCM_NIL;
    SCM_UNWIND_PROTECT {
        theVM->module = nmodule;
        v = Scm_Compile(expr, SCM_NIL, SCM_COMPILE_NORMAL);
    }
    SCM_WHEN_ERROR {
        theVM->module = omodule;
        SCM_NEXT_HANDLER;
        /*NOTREACHED*/
    }
    SCM_END_PROTECT;
    theVM->module = omodule;
    return v;
}

/* For now, we only supports a module as the evaluation environment */
ScmObj Scm_VMEval(ScmObj expr, ScmObj e)
{
    DECL_REGS;
    ScmObj v = SCM_NIL;
    if (SCM_UNBOUNDP(e)) {
        /* if env is not given, just use the current env */
        v = Scm_Compile(expr, SCM_NIL, SCM_COMPILE_NORMAL);
    } else if (!SCM_MODULEP(e)) {
        Scm_Error("module required, but got %S", e);
    } else {
        v = compile_for_eval(expr, SCM_MODULE(e), theVM->module);
    }
    argp = (ScmEnvFrame *)sp;
    PUSH_CONT(v, v);
    vm->numVals = 1;
    SAVE_REGS();
    return SCM_UNDEFINED;
}

/*-------------------------------------------------------------
 * User level eval and apply.
 *   When the C routine wants the Scheme code to return to the
 *   routine, instead of using C-continuation, the continuation
 *   "cross the border" of C-stack and Scheme-stack.  This
 *   border has peculiar characteristics.   Once the Scheme
 *   returns, continuations saved during the execution of the
 *   Scheme code becomes invalid.
 *
 *   At the implementation level, this boundary is kept in a
 *   structure ScmCStack.
 */

/* Border gate.  All the C->Scheme calls should go through here.
 *
 *   The current C stack information is saved in cstack.  The
 *   current VM stack information is saved (as a continuation
 *   frame pointer) in cstack.cont.
 */
static ScmObj user_eval_inner(ScmObj program)
{
    DECL_REGS_VOLATILE;
    ScmCStack cstack;

    /* Push extra continuation.  This continuation frame is a 'boundary
       frame' and marked by info == boundaryFrameMark.   VM loop knows
       it should return to C frame when it sees a boundary frame.
       A boundary frame also keeps the unfinished argument frame at
       the point when Scm_Eval or Scm_Apply is called. */
    CHECK_STACK(CONT_FRAME_SIZE);
    PUSH_CONT(boundaryFrameMark, pc);
    pc = program;
    SAVE_REGS();

    cstack.prev = vm->cstack;
    cstack.cont = vm->cont;
    vm->cstack = &cstack;
    
  restart:
    vm->escapeReason = SCM_VM_ESCAPE_NONE;
    if (sigsetjmp(cstack.jbuf, TRUE) == 0) {
        run_loop();
        val0 = vm->val0;
        if (vm->cont == cstack.cont) {
            RESTORE_REGS();
            POP_CONT();
            SAVE_REGS();
        }
    } else {
        /* An escape situation happened. */
        if (vm->escapeReason == SCM_VM_ESCAPE_CONT) {
             ScmEscapePoint *ep = (ScmEscapePoint*)vm->escapeData[0];
            if (ep->cstack == vm->cstack) {
                ScmObj handlers = throw_cont_calculate_handlers(ep, vm);
                vm->val0 = throw_cont_body(handlers, ep, vm->escapeData[1]);
                vm->pc = SCM_NIL;
                goto restart;
            } else {
                SCM_ASSERT(vm->cstack && vm->cstack->prev);
                vm->cont = cstack.cont;
                val0 = vm->val0;
                RESTORE_REGS();
                POP_CONT();
                SAVE_REGS();
                vm->cstack = vm->cstack->prev;
                siglongjmp(vm->cstack->jbuf, 1);
            }
        } else if (vm->escapeReason == SCM_VM_ESCAPE_ERROR) {
            ScmEscapePoint *ep = (ScmEscapePoint*)vm->escapeData[0];
            if (ep && ep->cstack == vm->cstack) {
                vm->cont = ep->cont;
                vm->pc = SCM_NIL;
                goto restart;
            } else if (vm->cstack->prev == NULL) {
                /* This loop is the outermost C stack, and nobody will
                   capture the error.  Usually this means we're running
                   scripts.  We can safely exit here, for the dynamic
                   stack is already rewind. */
                exit(EX_SOFTWARE);
            } else {
                /* Jump again until C stack is recovered.  We sould pop
                   the extra continuation frame so that the VM stack
                   is consistent. */
                vm->cont = cstack.cont;
                val0 = vm->val0;
                RESTORE_REGS();
                POP_CONT();
                SAVE_REGS();
                vm->cstack = vm->cstack->prev;
                siglongjmp(vm->cstack->jbuf, 1);
            }
        } else {
            Scm_Panic("invalid longjmp");
        }
        /* NOTREACHED */
    }
    vm->cstack = vm->cstack->prev;
    return vm->val0;
}

ScmObj Scm_Eval(ScmObj expr, ScmObj e)
{
    ScmObj v = SCM_NIL;
    if (SCM_UNBOUNDP(e)) {
        /* if env is not given, just use the current env */
        v = Scm_Compile(expr, SCM_NIL, SCM_COMPILE_NORMAL);
    } else if (!SCM_MODULEP(e)) {
        Scm_Error("module required, but got %S", e);
    } else {
        v = compile_for_eval(expr, SCM_MODULE(e), theVM->module);
    }
    if (theVM->compilerFlags & SCM_COMPILE_SHOWRESULT) {
        Scm_Printf(theVM->curerr, "== %#S\n", v);
    }
    return user_eval_inner(v);
}

ScmObj Scm_Apply(ScmObj proc, ScmObj args)
{
    ScmObj code = SCM_NIL, tail = SCM_NIL, cp;
    int nargs = 0;

    /*TODO: need this?*/
    SCM_APPEND1(code, tail, SCM_VM_INSN1(SCM_VM_CHECK_STACK, Scm_Length(args)));
    SCM_FOR_EACH(cp, args) {
        SCM_APPEND1(code, tail, SCM_CAR(cp));
        SCM_APPEND1(code, tail, SCM_VM_INSN(SCM_VM_PUSH));
        nargs++;
    }
    SCM_APPEND1(code, tail, proc);
    SCM_APPEND1(code, tail, SCM_VM_INSN1(SCM_VM_CALL, nargs));
    return user_eval_inner(SCM_LIST2(SCM_VM_INSN1(SCM_VM_PRE_CALL, nargs),code));
}

/* Arrange C function AFTER to be called after the procedure returns.
 * Usually followed by Scm_VMApply* function.
 */
void Scm_VMPushCC(ScmObj (*after)(ScmObj result, void **data),
                  void **data, int datasize)
{
    DECL_REGS;
    int i;
    ScmContFrame *cc;

    CHECK_STACK(CONT_FRAME_SIZE+datasize);
    cc = (ScmContFrame*)sp;
    sp += CONT_FRAME_SIZE;
    cc->prev = cont;
    cc->argp = NULL;
    cc->size = datasize;
    cc->pc = SCM_OBJ(after);
    cc->env = env;
    cc->info = SCM_NIL;
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
    ScmVM *vm = theVM;
    ScmObj after = SCM_OBJ(data[0]);
    ScmObj prev  = SCM_OBJ(data[1]);
    void *d[3];

    vm->handlers = prev;
    d[0] = (void*)result;
    d[1] = (void*)vm->numVals;
    if (vm->numVals > 1) {
        ScmObj *array = SCM_NEW2(ScmObj*, (vm->numVals-1)*sizeof(ScmObj));
        memcpy(array, vm->vals, sizeof(ScmObj)*(vm->numVals-1));
        d[2] = (void*)array;
    }
    Scm_VMPushCC(dynwind_after_cc, d, 3);
    return Scm_VMApply0(after);
}

static ScmObj dynwind_after_cc(ScmObj result, void **data)
{
    ScmObj val0 = SCM_OBJ(data[0]);
    ScmVM *vm = theVM;
    int nvals = (int)data[1];
    vm->numVals = nvals;
    if (nvals > 1) {
        SCM_ASSERT(nvals <= SCM_VM_MAX_VALUES);
        memcpy(vm->vals, data[2], sizeof(ScmObj)*(nvals-1));
    }
    return val0;
}

/* C-friendly wrapper */
ScmObj Scm_VMDynamicWindC(ScmObj (*before)(ScmObj *args, int nargs, void *data),
                          ScmObj (*body)(ScmObj *args, int nargs, void *data),
                          ScmObj (*after)(ScmObj *args, int nargs, void *data),
                          void *data)
{
    ScmObj beforeproc, bodyproc, afterproc;
    beforeproc =
        before ? Scm_MakeSubr(before, data, 0, 0, SCM_FALSE) : Scm_NullProc();
    afterproc =
        after ? Scm_MakeSubr(after, data, 0, 0, SCM_FALSE) : Scm_NullProc();
    bodyproc =
        body ? Scm_MakeSubr(body, data, 0, 0, SCM_FALSE) : Scm_NullProc();
    
    return Scm_VMDynamicWind(beforeproc, bodyproc, afterproc);
}


/*=================================================================
 * Exception handling
 */

/* Conceptually, exception handling is nothing more than a particular
 * combination of dynamic-wind and call/cc.   Gauche implements a parts
 * of it so that it will be efficient and safer to use.
 *
 * The most basic layer consists of those three functions (SRFI-18).
 *
 *  current-exception-handler
 *  with-exception-handler
 *  raise
 *
 * Their behavior is explained well in the following Scheme code,
 * if we ignore the messy part to keep C stack sane.
 * Suppose a system variable %xh keeps the list of exception handlers.
 *
 *  (define (current-exception-handler) (car %xh))
 *
 *  (define (raise exn)
 *    (receive r ((car %xh) exn)
 *      (when (uncontinuable-exception? exn)
 *        (set! %xh (cdr %xh))
 *        (error "returned from uncontinuable exception"))
 *      (apply values r)))
 *
 *  (define (with-exception-handler handler thunk)
 *    (let ((prev %xh))
 *      (dynamic-wind
 *        (lambda () (set! %xh (cons handler)))
 *        thunk
 *        (lambda () (set! %xh prev)))))
 *
 * In C level, the chain of the handlers are represented in the chain
 * of ScmExcapePoints.
 *
 * Note that this model assumes an exception handler returns unless it
 * explictly invokes continuation captured elsewhere.   In reality,
 * "error" exceptions are not supposed to return (hence it is checked
 * in raise).  Gauche provides two more useful exception handling
 * constructs that automates such continuation capturing.
 *
 * (define (with-error-handler handler thunk)
 *   (call/cc
 *     (lambda (cont)
 *       (let ((prev-handler (current-exception-handler)))
 *         (with-exception-handler
 *           (lambda (exn)
 *             (if (error? exn)
 *                 (call-with-values (handler exn) cont)
 *                 (prev-handler exn)))
 *           thunk)))))
 *
 * In the actual implementation,
 *
 *  - No "real" continuation procedure is created, but a lightweight
 *    mechanism is used.  The lightweight mechanism is similar to
 *    "one-shot" callback (call/1cc in Chez Scheme).
 *  - The error handler chain is kept in vm->escapePoint
 *  - There are messy lonjmp/setjmp stuff involved to keep C stack sane.
 */

/*
 * Default exception handler
 *  This is what we have as the system default, and also
 *  what with-error-handler installs as an exception handler.
 */

void Scm_VMDefaultExceptionHandler(ScmObj e)
{
    ScmVM *vm = theVM;
    ScmEscapePoint *ep = vm->escapePoint;
    ScmObj hp;

    if (ep) {
        ScmObj target, current;
        ScmObj result, rvals[SCM_VM_MAX_VALUES];
        int numVals, i;

        vm->escapePoint = ep->prev;
        vm->cont = ep->cont;

        /* Call the error handler and save the results. */
        result = Scm_Apply(ep->ehandler, SCM_LIST1(e));
        if ((numVals = vm->numVals) > 1) {
            for (i=0; i<numVals-1; i++) rvals[i] = vm->vals[i];
        }
        target = ep->handlers;
        current = vm->handlers;
        /* Call dynamic handlers */
        for (hp = current; SCM_PAIRP(hp) && hp != target; hp = SCM_CDR(hp)) {
            ScmObj proc = SCM_CDAR(hp);
            vm->handlers = SCM_CDR(hp);
            Scm_Apply(proc, SCM_NIL);
        }
        /* Install the continuation */
        for (i=0; i<numVals; i++) vm->vals[i] = rvals[i];
        vm->numVals = numVals;
        vm->val0 = result;
    } else {
        if (SCM_PROCEDUREP(vm->defaultEscapeHandler)) {
            Scm_Apply(vm->defaultEscapeHandler, SCM_LIST1(e));
        } else {
            Scm_ReportError(e);
        }
        /* unwind the dynamic handlers */
        SCM_FOR_EACH(hp, vm->handlers) {
            ScmObj proc = SCM_CDAR(hp);
            vm->handlers = SCM_CDR(hp);
            Scm_Apply(proc, SCM_NIL);
        }
    }

    if (vm->cstack) {
        vm->escapeReason = SCM_VM_ESCAPE_ERROR;
        vm->escapeData[0] = ep;
        vm->escapeData[1] = e;
        siglongjmp(vm->cstack->jbuf, 1);
    } else {
        exit(EX_SOFTWARE);
    }
}

static ScmObj default_exception_handler_body(ScmObj *argv, int argc, void *data)
{
    SCM_ASSERT(argc == 1);
    Scm_VMDefaultExceptionHandler(argv[0]);
    return SCM_UNDEFINED;       /*NOTREACHED*/
}

static SCM_DEFINE_STRING_CONST(default_exception_handler_name,
                               "default-exception-handler",
                               25, 25); /* strlen("default-exception-handler") */
static SCM_DEFINE_SUBR(default_exception_handler_rec, 1, 0,
                       SCM_OBJ(&default_exception_handler_name),
                       default_exception_handler_body, NULL, NULL);

/*
 * Entry point of throwing exception.
 *
 *  This function may be called from Scheme function raise or throw,
 *  or C-function Scm_Error families and signal handler.   So we can't
 *  push the handler to continuation and return; we have to use Scm_Apply
 *  to run the handler.
 */
ScmObj Scm_VMThrowException(ScmObj exception)
{
    ScmVM *vm = theVM;
    ScmEscapePoint *ep = vm->escapePoint;
    
    vm->runtimeFlags &= ~SCM_ERROR_BEING_HANDLED;

    if (vm->exceptionHandler != DEFAULT_EXCEPTION_HANDLER) {
        vm->val0 = Scm_Apply(vm->exceptionHandler, SCM_LIST1(exception));
        if (SCM_ERRORP(exception)) {
            /* the user-installed exception handler returned while it
               shouldn't.  In order to prevent infinite loop, we should
               pop the erroneous handler.  For now, we just reset
               the current exception handler. */
            vm->exceptionHandler = DEFAULT_EXCEPTION_HANDLER;
            Scm_Error("user-defined exception handler returned on non-continuable exception %S", exception);
        }
        return vm->val0;
    } else if (!SCM_ERRORP(exception)) {
        /* The system's default handler does't care about
           continuable exception.  See if there's a user-defined
           exception handler in the chain.  */
        for (; ep; ep = ep->prev) {
            if (ep->xhandler != DEFAULT_EXCEPTION_HANDLER) {
                return Scm_Apply(ep->xhandler, SCM_LIST1(exception));
            }
        }
    }
    Scm_VMDefaultExceptionHandler(exception);
    /* this never returns */
    return SCM_UNDEFINED;       /* dummy */
}

/*
 * with-error-handler
 */
static ScmObj install_ehandler(ScmObj *args, int nargs, void *data)
{
    ScmEscapePoint *ep = (ScmEscapePoint*)data;
    ScmVM *vm = theVM;
    vm->exceptionHandler = DEFAULT_EXCEPTION_HANDLER;
    vm->escapePoint = ep;
    return SCM_UNDEFINED;
}

static ScmObj discard_ehandler(ScmObj *args, int nargs, void *data)
{
    ScmEscapePoint *ep = (ScmEscapePoint *)data;
    theVM->escapePoint = ep->prev;
    theVM->exceptionHandler = ep->xhandler;
    return SCM_UNDEFINED;
}

ScmObj Scm_VMWithErrorHandler(ScmObj handler, ScmObj thunk)
{
    ScmVM *vm = theVM;
    ScmEscapePoint *ep = SCM_NEW(ScmEscapePoint);
    ScmObj before, after;

    /* NB: we can save pointer to the stack area (vm->cont) to ep->cont,
     * since such ep is always accessible via vm->escapePoint chain and
     * ep->cont is redirected whenever the continuation is captured while
     * ep is valid.
     */
    ep->prev = vm->escapePoint;
    ep->ehandler = handler;
    ep->handlers = vm->handlers;
    ep->cstack = vm->cstack;
    ep->xhandler = vm->exceptionHandler;
    ep->cont = vm->cont;
    
    vm->escapePoint = ep; /* This will be done in install_ehandler, but
                             make sure ep is visible from save_cont
                             to redirect ep->cont */
    before = Scm_MakeSubr(install_ehandler, ep, 0, 0, SCM_FALSE);
    after  = Scm_MakeSubr(discard_ehandler, ep, 0, 0, SCM_FALSE);
    return Scm_VMDynamicWind(before, thunk, after);
}

/* 
 * with-exception-handler
 *
 *   This primitive gives the programmer whole responsibility of
 *   dealing with exceptions.
 */

static ScmObj install_xhandler(ScmObj *args, int nargs, void *data)
{
    theVM->exceptionHandler = SCM_OBJ(data);
    return SCM_UNDEFINED;
}

ScmObj Scm_VMWithExceptionHandler(ScmObj handler, ScmObj thunk)
{
    ScmObj current = theVM->exceptionHandler;
    ScmObj before = Scm_MakeSubr(install_xhandler, handler, 0, 0, SCM_FALSE);
    ScmObj after  = Scm_MakeSubr(install_xhandler, current, 0, 0, SCM_FALSE);
    return Scm_VMDynamicWind(before, thunk, after);
}

/*==============================================================
 * Call With Current Continuation
 */

/* Figure out which before and after thunk should be called. */
static ScmObj throw_cont_calculate_handlers(ScmEscapePoint *ep, /*target*/
                                            ScmVM *vm)
{
    ScmObj target  = Scm_Reverse(ep->handlers);
    ScmObj current = vm->handlers;
    ScmObj h = SCM_NIL, t = SCM_NIL, p;

    SCM_FOR_EACH(p, current) {
        SCM_ASSERT(SCM_PAIRP(SCM_CAR(p)));
        if (!SCM_FALSEP(Scm_Memq(SCM_CAR(p), target))) break;
        /* push 'after' handlers to be called */
        SCM_APPEND1(h, t, SCM_CDAR(p));
    }
    SCM_FOR_EACH(p, target) {
        SCM_ASSERT(SCM_PAIRP(SCM_CAR(p)));
        if (!SCM_FALSEP(Scm_Memq(SCM_CAR(p), current))) continue;
        /* push 'before' handlers to be called */
        SCM_APPEND1(h, t, SCM_CAAR(p));
    }
    return h;
}

static ScmObj throw_cont_cc(ScmObj, void **);

static ScmObj throw_cont_body(ScmObj handlers,    /* after/before thunks
                                                     to be called */
                              ScmEscapePoint *ep, /* target continuation */
                              ScmObj args)        /* args to pass to the
                                                     target continuation */ 
{
    void *data[3];
    int nargs, i;
    ScmObj ap;
    ScmVM *vm = theVM;

    /*
     * first, check to see if we need to evaluate dynamic handlers.
     */
    if (SCM_PAIRP(handlers)) {
        data[0] = (void*)SCM_CDR(handlers);
        data[1] = (void*)ep;
        data[2] = (void*)args;
        Scm_VMPushCC(throw_cont_cc, data, 3);
        return Scm_VMApply0(SCM_CAR(handlers));
    }

    /*
     * now, install the target continuation
     */
    vm->pc = SCM_NIL;
    vm->cont = ep->cont;

    nargs = Scm_Length(args);
    if (nargs == 1) {
        return SCM_CAR(args);
    } else if (nargs < 1) {
        return SCM_UNDEFINED;
    } else if (nargs >= SCM_VM_MAX_VALUES) {
        Scm_Error("too many values passed to the continuation");
    }

    for (i=0, ap=SCM_CDR(args); SCM_PAIRP(ap); i++, ap=SCM_CDR(ap)) {
        vm->vals[i] = SCM_CAR(ap);
    }
    vm->numVals = nargs;
    return SCM_CAR(args);
}

static ScmObj throw_cont_cc(ScmObj result, void **data)
{
    ScmObj handlers = SCM_OBJ(data[0]);
    ScmEscapePoint *ep = (ScmEscapePoint *)data[1];
    ScmObj args = SCM_OBJ(data[2]);
    return throw_cont_body(handlers, ep, args);
}

/* Body of the continuation SUBR */
static ScmObj throw_continuation(ScmObj *argframe, int nargs, void *data)
{
    ScmEscapePoint *ep = (ScmEscapePoint*)data;
    ScmVM *vm = theVM;
    ScmObj args = argframe[0];

    if (vm->cstack != ep->cstack) {
        ScmCStack *cstk;
        for (cstk = vm->cstack; cstk; cstk = cstk->prev) {
            if (ep->cstack == cstk) break;
        }
        if (cstk == NULL) {
            Scm_Error("a continuation is thrown outside of it's extent: %p",
                      ep);
        } else {
            /* Rewind C stack */
            vm->escapeReason = SCM_VM_ESCAPE_CONT;
            vm->escapeData[0] = ep;
            vm->escapeData[1] = args;
            siglongjmp(vm->cstack->jbuf, 1);
        }
    } else {
        ScmObj handlers_to_call = throw_cont_calculate_handlers(ep, vm);
        vm->handlers = ep->handlers;
        return throw_cont_body(handlers_to_call, ep, args);
    }
    return SCM_UNDEFINED; /*dummy*/
}

ScmObj Scm_VMCallCC(ScmObj proc)
{
    ScmObj contproc;
    ScmEscapePoint *ep;
    ScmVM *vm = theVM;

    if (!SCM_PROCEDUREP(proc)
        || (!SCM_PROCEDURE_OPTIONAL(proc) && SCM_PROCEDURE_REQUIRED(proc) != 1)
        || (SCM_PROCEDURE_OPTIONAL(proc) && SCM_PROCEDURE_REQUIRED(proc) > 1))
        Scm_Error("Procedure taking one argument is required, but got: %S",
                  proc);

    save_cont(vm, vm->cont);
    ep = SCM_NEW(ScmEscapePoint);
    ep->prev = NULL;
    ep->ehandler = SCM_FALSE;
    ep->cont = vm->cont;
    ep->handlers = vm->handlers;
    ep->cstack = vm->cstack;

    contproc = Scm_MakeSubr(throw_continuation, ep, 0, 1,
                            SCM_MAKE_STR("continuation"));
    return Scm_VMApply1(proc, contproc);
}

/*==============================================================
 * Values
 */

ScmObj Scm_Values(ScmObj args)
{
    ScmVM *vm = theVM;
    ScmObj cp;
    int nvals;
    
    if (!SCM_PAIRP(args)) {
        vm->numVals = 0;
        return SCM_UNDEFINED;
    }
    nvals = 1;
    SCM_FOR_EACH(cp, SCM_CDR(args)) {
        vm->vals[nvals-1] = SCM_CAR(cp);
        if (nvals++ >= SCM_VM_MAX_VALUES) {
            Scm_Error("too many values: %S", args);
        }
    }
    vm->numVals = nvals;
    return SCM_CAR(args);
}

ScmObj Scm_Values2(ScmObj val0, ScmObj val1)
{
    ScmVM *vm = theVM;
    vm->numVals = 2;
    vm->vals[0] = val1;
    return val0;
}

ScmObj Scm_Values3(ScmObj val0, ScmObj val1, ScmObj val2)
{
    ScmVM *vm = theVM;
    vm->numVals = 3;
    vm->vals[0] = val1;
    vm->vals[1] = val2;
    return val0;
}

ScmObj Scm_Values4(ScmObj val0, ScmObj val1, ScmObj val2, ScmObj val3)
{
    ScmVM *vm = theVM;
    vm->numVals = 4;
    vm->vals[0] = val1;
    vm->vals[1] = val2;
    vm->vals[2] = val3;
    return val0;
}

ScmObj Scm_Values5(ScmObj val0, ScmObj val1, ScmObj val2, ScmObj val3, ScmObj val4)
{
    ScmVM *vm = theVM;
    vm->numVals = 5;
    vm->vals[0] = val1;
    vm->vals[1] = val2;
    vm->vals[2] = val3;
    vm->vals[3] = val4;
    return val0;
}

/*==============================================================
 * Thread interface
 */

/* Creation.  In the "NEW" state, a VM is allocated but actual thread
   is not created. */
ScmObj Scm_MakeThread(ScmProcedure *thunk, ScmObj name)
{
    ScmVM *current = theVM, *vm;

    if (SCM_PROCEDURE_REQUIRED(thunk) != 0) {
        Scm_Error("thunk required, but got %S", thunk);
    }
    vm = Scm_NewVM(current, current->module, name);
    vm->thunk = thunk;
    (void)SCM_INTERNAL_MUTEX_LOCK(vmListMutex);
    vmList = Scm_Cons(SCM_OBJ(vm), vmList);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(vmListMutex);
    return SCM_OBJ(vm);
}

/* Start a thread.  If the VM is in "NEW" state, create a new thread and
   make it run.

   With pthread, the real thread is started as "detached" mode; i.e. once
   the thread exits, the resources allocated for the thread by the system
   is collected, including the result of the thread.  During this
   deconstruction phase, the handler vm_cleanup() runs and saves the
   thread result to the ScmVM structure.  If nobody cares about the
   result of the thread, ScmVM structure will eventually be GCed.
   This is to prevent exitted thread's system resources from being
   uncollected.
 */
#ifdef GAUCHE_USE_PTHREAD
static void *thread_entry(void *vm)
{
    if (pthread_setspecific(vm_key, vm) != 0) {
        /* NB: at this point, theVM is not set and we can't use Scm_Error. */
        Scm_Panic("pthread_setspecific failed");
    }
    SCM_UNWIND_PROTECT {
        SCM_VM(vm)->result = Scm_Apply(SCM_OBJ(SCM_VM(vm)->thunk), SCM_NIL);
    } SCM_WHEN_ERROR {
        ScmObj exc;
        switch (SCM_VM(vm)->escapeReason) {
        case SCM_VM_ESCAPE_CONT:
            /* TODO: sets appropriate exception to resultException
               instead of panic */
            Scm_Panic("continuation thrown in different thread");
        default:
            Scm_Panic("unknown escape");
        case SCM_VM_ESCAPE_ERROR:
            exc = Scm_MakeThreadException(SCM_CLASS_UNCAUGHT_EXCEPTION, SCM_VM(vm));
            SCM_THREAD_EXCEPTION(exc)->data = SCM_OBJ(SCM_VM(vm)->escapeData[1]);
            SCM_VM(vm)->resultException = exc;
        }
    } SCM_END_PROTECT;
    return NULL;
}
#endif /* GAUCHE_USE_PTHREAD */

ScmObj Scm_ThreadStart(ScmVM *vm)
{
#ifdef GAUCHE_USE_PTHREAD
    int err_state = FALSE, err_create = FALSE;
    pthread_attr_t thattr;
    (void)SCM_INTERNAL_MUTEX_LOCK(vm->vmlock);
    if (vm->state != SCM_VM_NEW) {
        err_state = TRUE;
    } else {
        SCM_ASSERT(vm->thunk);
        vm->state = SCM_VM_RUNNABLE;
        pthread_attr_init(&thattr);
        pthread_attr_setdetachstate(&thattr, TRUE);
        if (pthread_create(&vm->thread, &thattr, thread_entry, vm) != 0) {
            vm->state = SCM_VM_NEW;
            err_create = TRUE;
        }
        pthread_attr_destroy(&thattr);
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(vm->vmlock);
    if (err_state) Scm_Error("attempt to start an already-started thread: %S", vm);
    if (err_create) Scm_Error("couldn't start a new thread: %S", vm);
#else  /*!GAUCHE_USE_PTHREAD*/
    Scm_Error("not implemented!\n");
#endif /*GAUCHE_USE_PTHREAD*/
    return SCM_OBJ(vm);
}

/* Thread join */
ScmObj Scm_ThreadJoin(ScmVM *target, ScmObj timeout, ScmObj timeoutval)
{
#ifdef GAUCHE_USE_PTHREAD
    struct timespec ts, *pts;
    ScmObj result = SCM_FALSE, resultx = SCM_FALSE;
    int intr = FALSE, tout = FALSE;
    
    pts = Scm_GetTimeSpec(timeout, &ts);
    (void)SCM_INTERNAL_MUTEX_LOCK(target->vmlock);
    while (target->state != SCM_VM_TERMINATED) {
        if (pts) {
            int tr = pthread_cond_timedwait(&(target->cond), &(target->vmlock), pts);
            if (tr == ETIMEDOUT) { tout = TRUE; break; }
            else if (tr == EINTR) { intr = TRUE; break; }
        } else {
            pthread_cond_wait(&(target->cond), &(target->vmlock));
        }
    }
    if (!tout) { result = target->result; resultx = target->resultException; }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(target->vmlock);
    if (intr) Scm_SigCheck(theVM);
    if (tout) {
        if (SCM_UNBOUNDP(timeoutval)) {
            ScmObj e = Scm_MakeThreadException(SCM_CLASS_JOIN_TIMEOUT_EXCEPTION, target);
            result = Scm_VMThrowException(e);
        } else {
            result = timeoutval;
        }
    } else if (SCM_EXCEPTIONP(resultx)) {
        result = Scm_VMThrowException(resultx);
    }
    return result;
#else  /*!GAUCHE_USE_PTHREAD*/
    Scm_Error("not implemented!\n");
    return SCM_UNDEFINED;
#endif /*!GAUCHE_USE_PTHREAD*/
}

/* Thread yield */
ScmObj Scm_ThreadYield(void)
{
#ifdef GAUCHE_USE_PTHREAD
#if defined(HAVE_SCHED_H) && defined(_POSIX_PRIORITY_SCHEDULING)
    sched_yield();
#else  /*!HAVE_SCHED_H*/
    /* what can I do? */
#endif /*!HAVE_SCHED_H*/
#else  /*!GAUCHE_USE_PTHREAD*/
    Scm_Error("not implemented!\n");
#endif /*!GAUCHE_USE_PTHREAD*/
    return SCM_UNDEFINED;
}

/* Thread sleep */
ScmObj Scm_ThreadSleep(ScmObj timeout)
{
#ifdef GAUCHE_USE_PTHREAD
    struct timespec ts, *pts;
    ScmInternalCond dummyc = PTHREAD_COND_INITIALIZER;
    ScmInternalMutex dummym = PTHREAD_MUTEX_INITIALIZER;
    int intr;
    pts = Scm_GetTimeSpec(timeout, &ts);
    if (pts == NULL) Scm_Error("thread-sleep! can't take #f as a timeout value");
    pthread_mutex_lock(&dummym);
    if (pthread_cond_timedwait(&dummyc, &dummym, pts) == EINTR) {
        intr = TRUE;
    }
    pthread_mutex_unlock(&dummym);
    if (intr) Scm_SigCheck(theVM);
#else  /*!GAUCHE_USE_PTHREAD*/
    Scm_Error("not implemented!\n");
#endif /*!GAUCHE_USE_PTHREAD*/
    return SCM_UNDEFINED;
}

/* Thread terminate */
ScmObj Scm_ThreadTerminate(ScmVM *target)
{
    Scm_Error("not implemented!\n");
    return SCM_UNDEFINED;
}

/*==============================================================
 * Debug features.
 */

/*
 * Stack trace.
 *
 *   The "lite" version returns a list of source information of
 *   continuation frames.
 *
 *   The full stack trace is consisted by a list of pair of
 *   source information and environment vector.  Environment vector
 *   is a copy of content of env frame, with the first element
 *   be the environment info.   Environment vector may be #f if
 *   the continuation frame doesn't have associated env.
 */

ScmObj Scm_VMGetStackLite(ScmVM *vm)
{
    ScmContFrame *c = vm->cont;
    ScmObj stack = SCM_NIL, tail = SCM_NIL;
    ScmObj info;

    if (SCM_PAIRP(vm->pc)) {
        info = Scm_Assq(SCM_SYM_SOURCE_INFO, SCM_PAIR_ATTR(vm->pc));
        if (SCM_PAIRP(info)) SCM_APPEND1(stack, tail, SCM_CDR(info));
    }
    
    while (c) {
        if (SCM_PAIRP(c->info)) {
            info = Scm_Assq(SCM_SYM_SOURCE_INFO, SCM_PAIR_ATTR(c->info));
            if (SCM_PAIRP(info)) SCM_APPEND1(stack, tail, SCM_CDR(info));
        }
        c = c->prev;
    }
    return stack;
}

#define DEFAULT_ENV_TABLE_SIZE  64

struct EnvTab {
    struct EnvTabEntry {
        ScmEnvFrame *env;
        ScmObj vec;
    } entries[DEFAULT_ENV_TABLE_SIZE];
    int numEntries;
};

static ScmObj env2vec(ScmEnvFrame *env, struct EnvTab *etab)
{
    int i;
    ScmObj vec;
    
    if (!env) return SCM_FALSE;
    for (i=0; i<etab->numEntries; i++) {
        if (etab->entries[i].env == env) {
            return etab->entries[i].vec;
        }
    }
    vec = Scm_MakeVector(env->size+2, SCM_FALSE);
    SCM_VECTOR_ELEMENT(vec, 0) = env2vec(env->up, etab);
    SCM_VECTOR_ELEMENT(vec, 1) = env->info;
    for (i=0; i<env->size; i++) {
        SCM_VECTOR_ELEMENT(vec, i+2) = env->data[i];
    }
    if (etab->numEntries < DEFAULT_ENV_TABLE_SIZE) {
        etab->entries[etab->numEntries].env = env;
        etab->entries[etab->numEntries].vec = vec;
        etab->numEntries++;
    }
    return vec;
}

ScmObj Scm_VMGetStack(ScmVM *vm)
{
    ScmContFrame *c = vm->cont;
    ScmObj stack = SCM_NIL, tail = SCM_NIL;
    ScmObj info, evec;
    struct EnvTab envTab;

    envTab.numEntries = 0;
    if (SCM_PAIRP(vm->pc)) {
        info = Scm_Assq(SCM_SYM_SOURCE_INFO, SCM_PAIR_ATTR(vm->pc));
        if (SCM_PAIRP(info)) info = SCM_CDR(info);
        SCM_APPEND1(stack, tail, Scm_Cons(info, env2vec(vm->env, &envTab)));
    }
    
    for (; c; c = c->prev) {
        if (!SCM_PAIRP(c->info)) continue;
        info = Scm_Assq(SCM_SYM_SOURCE_INFO, SCM_PAIR_ATTR(c->info));
        if (SCM_PAIRP(info)) info = SCM_CDR(info);
        evec = env2vec(c->env, &envTab);
        SCM_APPEND1(stack, tail, Scm_Cons(info, evec));
    }
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

void Scm__VMInsnWrite(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    struct insn_info *info;
    int param0, param1;
    char buf[50];
    int insn = SCM_VM_INSN_CODE(obj);
    SCM_ASSERT(insn >= 0 && insn < SCM_VM_NUM_INSNS);

    info = &insn_table[insn];
    switch (info->nparams) {
    case 0:
        snprintf(buf, 50, "#<%s>", info->name);
        break;
    case 1:
        param0 = SCM_VM_INSN_ARG(obj);
        snprintf(buf, 50, "#<%s %d>", info->name, param0);
        break;
    case 2:
        param0 = SCM_VM_INSN_ARG0(obj);
        param1 = SCM_VM_INSN_ARG1(obj);
        snprintf(buf, 50, "#<%s %d,%d>", info->name, param0, param1);
        break;
    default:
        Scm_Panic("something screwed up");
    }
    SCM_PUTZ(buf, -1, out);
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
    ScmEnvFrame *env = vm->env;
    ScmContFrame *cont = vm->cont;
    ScmCStack *cstk = vm->cstack;
    ScmEscapePoint *ep = vm->escapePoint;

    Scm_Printf(out, "VM %p -----------------------------------------------------------\n", vm);
    Scm_Printf(out, "   pc: %#65.1S\n", vm->pc);
    Scm_Printf(out, "   sp: %p  base: %p  [%p-%p]\n", vm->sp, vm->stackBase,
               vm->stack, vm->stackEnd);
    Scm_Printf(out, " argp: %p\n", vm->argp);
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
        Scm_Printf(out, "             info = %#50.1S\n", cont->info);
        cont = cont->prev;
    }

    Scm_Printf(out, "C stacks:\n");
    while (cstk) {
        Scm_Printf(out, "  %p: prev=%p, cont=%p\n",
                   cstk, cstk->prev, cstk->cont);
        cstk = cstk->prev;
    }
    Scm_Printf(out, "Escape points:\n");
    while (ep) {
        Scm_Printf(out, "  %p: cont=%p, handler=%#20.1S\n",
                   ep, ep->cont, ep->ehandler);
        ep = ep->prev;
    }
    Scm_Printf(out, "dynenv: %S\n", vm->handlers);
}

