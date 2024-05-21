/*
 * vm.c - virtual machine
 *
 *   Copyright (c) 2000-2024  Shiro Kawai  <shiro@acm.org>
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
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/priv/configP.h"
#include "gauche/exception.h"
#include "gauche/priv/builtin-syms.h"
#include "gauche/priv/codeP.h"
#include "gauche/priv/vmP.h"
#include "gauche/priv/glocP.h"
#include "gauche/priv/identifierP.h"
#include "gauche/priv/parameterP.h"
#include "gauche/priv/promiseP.h"
#include "gauche/code.h"
#include "gauche/vminsn.h"
#include "gauche/prof.h"
#include "gauche/precomp.h"

/* Experimental code to use custom mark procedure for stack gc.
   Currently it doens't show any improvement, so we disable it
   by default. */
#ifdef USE_CUSTOM_STACK_MARKER
#include "gc_mark.h"

static void **vm_stack_free_list;
static int vm_stack_kind;
static int vm_stack_mark_proc;
#endif /*USE_CUSTOM_STACK_MARKER*/

#ifdef HAVE_SCHED_H
#include <sched.h>
#endif

#ifndef EX_SOFTWARE
/* SRFI-22 requires this. */
#define EX_SOFTWARE 70
#endif

/* Prompt tag.  Class initialization is done in class.c */
static void prompt_tag_print(ScmObj obj, ScmPort *out,
                             ScmWriteContext *ctx SCM_UNUSED)
{
    Scm_Printf(out, "#<prompt-tag %S>", SCM_PROMPT_TAG(obj)->name);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_PromptTagClass,
                         prompt_tag_print, NULL, NULL, NULL,
                         SCM_CLASS_DEFAULT_CPL);

static ScmPromptTag defaultPromptTag; /* initialized in initVM */

struct ScmContinuationPromptRec {
    ScmContFrame *bottom;
    ScmContFrame *bottom_top;
    struct ScmContinuationPromptRec *prev;
};

/* bitflags for ScmContFrame->marker */
enum {
      SCM_CONT_SHIFT_MARKER = (1L<<0),
      SCM_CONT_RESET_MARKER = (1L<<1),
      SCM_CONT_DYNWIND_MARKER = (1L<<2),
};

static void push_prompt_cont(ScmVM*, ScmObj, ScmObj);
static void push_boundary_cont(ScmVM*, ScmObj, ScmObj);

/* return true if cont is a boundary continuation frame */
#define BOUNDARY_FRAME_P(cont) ((cont)->marker & SCM_CONT_RESET_MARKER)

/* return true if cont has the end marker of partial continuation */
#define MARKER_FRAME_P(cont)   ((cont)->marker & SCM_CONT_SHIFT_MARKER)

/* return true if cont is of dynamic-wind body */
#define DYNWIND_FRAME_P(cont)  ((cont)->marker & SCM_CONT_DYNWIND_MARKER)


/* A stub VM code to make VM return immediately */
static ScmWord return_code[] = { SCM_VM_INSN(SCM_VM_RET) };
#define PC_TO_RETURN  return_code

/* A dummy env frame to indicate C Continuation */
static ScmEnvFrame ccEnvMark = {
    NULL,                       /* up */
    SCM_FALSE,                  /* info */
    0                           /* size */
};

#define C_CONTINUATION_P(cont)  ((cont)->env == &ccEnvMark)

/* If you have ScmContFrame *cont and you'll do something that can trigger
   saving frames, you have to call this first to ensure that your
   cont frame keeps pointing a valid frame. */
#define ENSURE_SAVE_CONT(cont)                  \
    do {                                        \
        save_cont(vm);                          \
        if (FORWARDED_CONT_P(cont)) {           \
            cont = FORWARDED_CONT(cont);        \
        }                                       \
    } while (0)

/* Unique uninterned symbol to indicate continuation procedure. */
static ScmObj continuation_symbol = SCM_UNBOUND;

/* Unique dynamic env keys for system use.  We use uninterned symbol for
   this purpose, for it is easier while debugging.
   They're set during initialization. */
static ScmObj denv_key_exception_handler = SCM_UNBOUND;
static ScmObj denv_key_dynamic_handler = SCM_UNBOUND;
static ScmObj denv_key_parameterization = SCM_UNBOUND;
static ScmObj denv_key_expression_name = SCM_UNBOUND;

/* A dummy compiled code structure used as 'fill-in', when Scm_Apply
   is called without any VM code running.  See Scm_Apply below. */
static ScmCompiledCode internal_apply_compiled_code =
    SCM_COMPILED_CODE_CONST_INITIALIZER(NULL, 0, 0, 0, 0,
                                        SCM_SYM_INTERNAL_APPLY,
                                        SCM_NIL, SCM_FALSE,
                                        SCM_FALSE, SCM_FALSE);

/* This saves offset of each instruction handler, initialized by
   the first call to run_loop.  The info can be used for detailed
   profiling. */
static unsigned long vminsn_offsets[SCM_VM_NUM_INSNS] = { 0, };

/*
 * The VM.
 *
 *   VM encapsulates the dynamic status of the current execution.
 *   In Gauche, there's always one active virtual machine per thread,
 *   referred by Scm_VM().
 *
 *   From Scheme, VM is viewed as <thread> object.  The class definition
 *   is in thrlib.stub.
 */

static ScmVM *rootVM = NULL;         /* VM for primodial thread */
static ScmHashCore vm_table;         /* VMs other than primordial one is
                                        registered to this hashtable, in order
                                        to avoid being GC-ed. */
static ScmInternalMutex vm_table_mutex;
static void vm_register(ScmVM *vm);
static void vm_unregister(ScmVM *vm);

static u_long vm_numeric_id = 0;    /* used for Scm_VM->vmid */
static ScmInternalMutex vm_id_mutex;

#define CALL_TRACE_SIZE_MIN 512
#define CALL_TRACE_SIZE_MAX 65535
static u_long vm_call_trace_size = 0; /* global default */

#ifdef GAUCHE_USE_PTHREADS
static pthread_key_t vm_key;
#define theVM   ((ScmVM*)pthread_getspecific(vm_key))
#elif  GAUCHE_USE_WTHREADS
static DWORD vm_key;
#define theVM   ((ScmVM*)TlsGetValue(vm_key))
#else  /* !GAUCHE_USE_PTHREADS && !GAUCHE_USE_WTHREADS */
static ScmVM *theVM;
#endif /* !GAUCHE_USE_PTHREADS */

static void save_stack(ScmVM *vm);

static ScmObj find_dynamic_env(ScmVM *vm, ScmObj key, ScmObj fallback);
static void push_dynamic_env(ScmVM *vm, ScmObj key, ScmObj val);

static ScmSubr default_exception_handler_rec;
#define DEFAULT_EXCEPTION_HANDLER  SCM_OBJ(&default_exception_handler_rec)
static ScmObj throw_cont_calculate_handlers(ScmObj target, ScmObj current);
static ScmObj get_dynamic_handlers(ScmVM*);
static void   set_dynamic_handlers(ScmVM*, ScmObj);
static ScmObj push_dynamic_handlers(ScmVM*, ScmObj, ScmObj, ScmObj);
static ScmObj pop_dynamic_handlers(ScmVM*);
static void   call_dynamic_handlers(ScmVM*, ScmObj target, ScmObj current);
static ScmObj throw_cont_body(ScmObj, ScmEscapePoint*, ScmObj);
static void   process_queued_requests(ScmVM *vm);
static void   vm_finalize(ScmObj vm, void *data);
static int    check_arglist_tail_for_apply(ScmVM *vm, ScmObj restargs, int max_count);

static ScmEnvFrame *get_env(ScmVM *vm);
static ScmObj get_denv(ScmVM *vm);

static void   call_error_reporter(ScmObj e);
static ScmObj call_abort_handler(ScmObj, ScmObj);

/*#define COUNT_INSN_FREQUENCY*/
#ifdef COUNT_INSN_FREQUENCY
#include "vmstat.c"
#endif /*COUNT_INSN_FREQUENCY*/

/*
 * Constructor
 *
 *   PROTO argument is treated as a prototype for the new VM, i.e.
 *   some of default values are 'inherited' from PROTO.
 *
 *   VM should be 'attached' to the running OS thread before being
 *   used.  The root thread is always attached to the primordial thread
 *   at the initialization stage (see Scm__InitVM()).   For other threads,
 *   it depends on whether the thread is created from Gauche side or not.
 *
 *   If the thread is created from Gauche side (i.e. by Scm_MakeThread()
 *   C API or make-thread Scheme API), attaching is handled automatically
 *   by Gauche.
 *
 *   If the thread is created by other means, the VM should be attached
 *   to the thread by Scm_AttachVM() API.   The VMs attached by this are
 *   somewhat different than the ones attached by Gauche; such VM can't
 *   be passed to thread-join, for example.   This type of VM is for
 *   the applications that want to evaluate Gauche program in their own
 *   thread.
 *   NOTE: the thread should still be created by Boehm-GC's pthread_create,
 *   for it is the only way for GC to see the thread's stack.
 */

ScmVM *Scm_NewVM(ScmVM *proto, ScmObj name)
{
    ScmVM *v = SCM_NEW(ScmVM);

    SCM_SET_CLASS(v, SCM_CLASS_VM);
    v->state = SCM_VM_NEW;
    (void)SCM_INTERNAL_MUTEX_INIT(v->vmlock);
    (void)SCM_INTERNAL_COND_INIT(v->cond);
    v->canceller = NULL;
    v->inspector = NULL;
    v->name = name;
    v->specific = SCM_FALSE;
    v->thunk = NULL;
    v->result = SCM_UNDEFINED;
    v->resultException = SCM_UNDEFINED;
    v->module = proto ? proto->module : Scm_SchemeModule();
    v->cstack = NULL;

    v->threadLocals = Scm__MakeVMThreadLocalTable(proto);

    v->compilerFlags = proto? proto->compilerFlags : 0;
    v->runtimeFlags = proto? proto->runtimeFlags : 0;
    v->attentionRequest = 0;
    v->signalPending = 0;
    v->finalizerPending = 0;
    v->stopRequest = 0;

#ifdef USE_CUSTOM_STACK_MARKER
    v->stack = (ScmObj*)GC_generic_malloc((SCM_VM_STACK_SIZE+1)*sizeof(ScmObj),
                                          vm_stack_kind);
    *v->stack++ = SCM_OBJ(v);
#else  /*!USE_CUSTOM_STACK_MARKER*/
    v->stack = SCM_NEW_ARRAY(ScmObj, SCM_VM_STACK_SIZE);
#endif /*!USE_CUSTOM_STACK_MARKER*/
    v->sp = v->stack;
    v->stackBase = v->stack;
    v->stackEnd = v->stack + SCM_VM_STACK_SIZE;
#if GAUCHE_FFX
    v->fpstack = SCM_NEW_ATOMIC_ARRAY(ScmFlonum, SCM_VM_STACK_SIZE);
    v->fpstackEnd = v->fpstack + SCM_VM_STACK_SIZE;
    v->fpsp = v->fpstack;
#endif /* GAUCHE_FFX */

    v->env = NULL;
    v->argp = v->stack;
    v->cont = NULL;
    v->pc = PC_TO_RETURN;
    v->base = NULL;
    v->val0 = SCM_UNDEFINED;
    for (int i=0; i<SCM_VM_MAX_VALUES; i++) v->vals[i] = SCM_UNDEFINED;
    v->numVals = 1;
    v->trampoline = -1;

    /* Thread inherits dynamic environment.
       TRANSIENT: For the full srfi-226 compatibility, we can just share
       the denv.  However, for the backward compatible mode where parameters
       aren't shared between threads, we copy denv.  We'll eventually
       drop copying.  */
    v->denv = get_denv(proto);

    v->dynamicHandlers = SCM_NIL;

    v->escapePoint = NULL;
    v->escapeReason = SCM_VM_ESCAPE_NONE;
    v->escapeData[0] = NULL;
    v->escapeData[1] = NULL;
    v->errorHandlerContinuable = FALSE;
    v->customErrorReporter = (proto? proto->customErrorReporter : SCM_FALSE);
#if GAUCHE_SPLIT_STACK
    v->lastErrorCont = NULL;
#endif /*GAUCHE_SPLIT_STACK*/

    v->evalSituation = SCM_VM_EXECUTING;

    sigemptyset(&v->sigMask);
    Scm_SignalQueueInit(&v->sigq);

    /* stats */
    v->stat.sovCount = 0;
    v->stat.sovTime = 0;
    v->stat.loadStat = SCM_NIL;
    v->profilerRunning = FALSE;
    v->prof = NULL;

    (void)SCM_INTERNAL_THREAD_INIT(v->thread);

#if defined(GAUCHE_USE_WTHREADS)
    v->winCleanup = NULL;
#endif /*defined(GAUCHE_USE_WTHREADS)*/

    (void)SCM_INTERNAL_MUTEX_LOCK(vm_id_mutex);
    v->vmid = vm_numeric_id++;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(vm_id_mutex);

    v->callTrace = (vm_call_trace_size
                    ? Scm__MakeCallTraceQueue(vm_call_trace_size)
                    : NULL);
    v->codeCache = NULL;

    v->currentPrompt = NULL;
    v->resetChain = SCM_NIL;

    Scm_RegisterFinalizer(SCM_OBJ(v), vm_finalize, NULL);
    return v;
}

/*
 * Taking a snapshot of VM.
 *   Copying VM is mainly to preserve a snapshot of VM for analysis.
 *   Note that internal states of some system construct (e.g. mutex) aren't
 *   be copied, so it is mainly for diagnostics; do not count on the
 *   copied VM to run independently from the original.
 */

ScmVM *Scm_VMTakeSnapshot(ScmVM *master)
{
    ScmVM *v = SCM_NEW(ScmVM);

    SCM_SET_CLASS(v, SCM_CLASS_VM);
    v->state = master->state;
    (void)SCM_INTERNAL_MUTEX_INIT(v->vmlock);
    (void)SCM_INTERNAL_COND_INIT(v->cond);
    v->canceller = master->canceller;
    v->inspector = master->inspector;
    v->name = master->name;
    v->specific = master->specific;
    v->thunk = master->thunk;
    v->result = master->result;
    v->resultException = master->resultException;
    v->module = master->module;
    v->cstack = master->cstack;

    v->threadLocals = Scm__MakeVMThreadLocalTable(master);

    v->compilerFlags = master->compilerFlags;
    v->runtimeFlags = master->runtimeFlags;
    v->attentionRequest = master->attentionRequest;
    v->signalPending = master->signalPending;
    v->finalizerPending = master->finalizerPending;
    v->stopRequest = master->stopRequest;

#ifdef USE_CUSTOM_STACK_MARKER
    v->stack = (ScmObj*)GC_generic_malloc((SCM_VM_STACK_SIZE+1)*sizeof(ScmObj),
                                          vm_stack_kind);
    *v->stack++ = SCM_OBJ(v);
#else  /*!USE_CUSTOM_STACK_MARKER*/
    v->stack = SCM_NEW_ARRAY(ScmObj, SCM_VM_STACK_SIZE);
#endif /*!USE_CUSTOM_STACK_MARKER*/
    v->sp = v->stack;
    v->stackBase = v->stack;
    v->stackEnd = v->stack + SCM_VM_STACK_SIZE;
    memcpy(v->stack, master->stack, SCM_VM_STACK_SIZE*sizeof(ScmObj));

#if GAUCHE_FFX
    v->fpstack = SCM_NEW_ATOMIC_ARRAY(ScmFlonum, SCM_VM_STACK_SIZE);
    v->fpstackEnd = v->fpstack + SCM_VM_STACK_SIZE;
    v->fpsp = v->fpstack;
    memcpy(v->fpstack, master->fpstack, SCM_VM_STACK_SIZE*sizeof(ScmFlonum));
#endif /* GAUCHE_FFX */

    v->env = master->env;
    v->argp = master->argp;
    v->cont = master->cont;
    v->pc = master->pc;
    v->base = master->base;
    v->val0 = master->val0;
    for (int i=0; i<SCM_VM_MAX_VALUES; i++) v->vals[i] = master->vals[i];
    v->numVals = master->numVals;
    v->trampoline = master->trampoline;
    v->denv = master->denv;
    v->dynamicHandlers = master->dynamicHandlers;

    v->escapePoint = master->escapePoint;
    v->escapeReason = master->escapeReason;
    v->escapeData[0] = master->escapeData[0];
    v->escapeData[1] = master->escapeData[1];
    v->errorHandlerContinuable = master->errorHandlerContinuable;
    v->customErrorReporter = master->customErrorReporter;
#if GAUCHE_SPLIT_STACK
    v->lastErrorCont = master->lastErrorCont;
#endif /*GAUCHE_SPLIT_STACK*/

    v->evalSituation = master->evalSituation;

    sigemptyset(&v->sigMask);
    Scm_SignalQueueInit(&v->sigq); /* TODO: Copy signal queue content */

    /* stats */
    v->stat.sovCount = master->stat.sovCount;
    v->stat.sovTime = master->stat.sovTime;
    v->stat.loadStat = master->stat.loadStat;
    v->profilerRunning = master->profilerRunning;
    v->prof = master->prof;     /* TODO: Should we copy this? */

    v->thread = master->thread;

#if defined(GAUCHE_USE_WTHREADS)
    v->winCleanup = master->winCleanup;
#endif /*defined(GAUCHE_USE_WTHREADS)*/

    v->vmid = master->vmid;
    v->callTrace = (master->callTrace
                    ? Scm__CopyCallTraceQueue(master->callTrace)
                    : NULL);
    v->codeCache = NULL;        /* We might need to copy this as well
                                   if we want to debug JIT code cache */

    v->currentPrompt = master->currentPrompt;
    v->resetChain = master->resetChain;
    /* NB: We don't register the finalizer vm_finalize to the snapshot,
       for we do not want the associated system resources to be cleaned
       up when the snapshot is GCed. */
    return v;
}

/* Attach the thread to the current thread.
   See the notes of Scm_NewVM above.
   Returns TRUE on success, FALSE on failure. */
int Scm_AttachVM(ScmVM *vm)
{
    if (theVM != NULL) {
        /* The current thread already has another VM attached. */
        return FALSE;
    }
    /* NB: We want to check if this VM has already attached to another
       thread or not. */

    if (!SCM_INTERNAL_THREAD_SETSPECIFIC(Scm_VMKey(), vm)) return FALSE;

    if (!SCM_INTERNAL_THREAD_INITIALIZED_P(vm->thread)) {
#ifdef GAUCHE_USE_WTHREADS
        /* GetCurrentThread() on Windows returns a pseudo handle
           indicating 'myself', which can't be usable from other thread.
           We need a special care.  The resulting HANDLE should be closed,
           which is done in the finalizer. */
        HANDLE t;
        BOOL r = DuplicateHandle(GetCurrentProcess(), /* source process */
                                 GetCurrentThread(),  /* source handle */
                                 GetCurrentProcess(), /* target process */
                                 &t,
                                 0, FALSE, DUPLICATE_SAME_ACCESS);
        if (!r) {
            Scm_SysError("Getting current thread handle failed");
        }
        vm->thread = t;
#else  /* GAUCHE_USE_PTHREADS */
        vm->thread = SCM_INTERNAL_THREAD_GETCURRENT();
#endif /* GAUCHE_USE_PTHREADS */
    }
    vm->state = SCM_VM_RUNNABLE;
    vm_register(vm);
    return TRUE;
}

/* If the current VM is attached by Scm_AttachVM() rather than Scheme
   creating a thread, this needs to be called once you've done with the
   VM (typically just before the thread terminates).
 */
void Scm_DetachVM(ScmVM *vm)
{
    if (vm != NULL) {
        (void)SCM_INTERNAL_THREAD_SETSPECIFIC(Scm_VMKey(), NULL);
        vm_unregister(vm);
    }
}

int Scm_VMGetNumResults(ScmVM *vm)
{
    return vm->numVals;
}

ScmObj Scm_VMGetResult(ScmVM *vm)
{
    ScmObj head = SCM_NIL, tail = SCM_NIL;
    if (vm->numVals == 0) return SCM_NIL;
    SCM_APPEND1(head, tail, vm->val0);
    for (int i=1; i<vm->numVals; i++) {
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

/* Some macros inserts Scm_VM() in its output.  If such macros are expanded
   below, we can safely replace Scm_VM() to theVM. */
#define Scm_VM() theVM

/*
 * Get VM key
 */
#if   defined(GAUCHE_USE_PTHREADS)
pthread_key_t Scm_VMKey(void)
{
    return vm_key;
}
#elif defined(GAUCHE_USE_WTHREADS)
DWORD Scm_VMKey(void)
{
    return vm_key;
}
#endif /*GAUCHE_USE_WTHREADS*/

/* Warn if VM is terminated by uncaught exception, and GC-ed without
   joining.  It is clearly an unexpected case and worth reporting. */
static void vm_finalize(ScmObj obj, void *data SCM_UNUSED)
{
    ScmVM *vm = SCM_VM(obj);
    ScmObj re = vm->resultException;

    if (SCM_UNCAUGHT_EXCEPTION_P(re)) {
        Scm_Warn("A thread %S (%lu) died a lonely death"
                 " with an uncaught exception %S.",
                 vm->name, vm->vmid, SCM_THREAD_EXCEPTION(re)->data);
    }
#ifdef GAUCHE_USE_WTHREADS
    if (vm->thread != INVALID_HANDLE_VALUE) {
        CloseHandle(vm->thread);
        vm->thread = INVALID_HANDLE_VALUE;
    }
#endif /*GAUCHE_USE_WTHREADS*/
}

/* Thread specific storage may not be scanned by GC.  We keep pointers
   to the live VMs in the global hashtable. */
static void vm_register(ScmVM *vm)
{
    SCM_INTERNAL_MUTEX_LOCK(vm_table_mutex);
    ScmDictEntry *e = Scm_HashCoreSearch(&vm_table, (intptr_t)vm,
                                         SCM_DICT_CREATE);
    (void)SCM_DICT_SET_VALUE(e, SCM_TRUE);
    SCM_INTERNAL_MUTEX_UNLOCK(vm_table_mutex);
}

static void vm_unregister(ScmVM *vm)
{
    SCM_INTERNAL_MUTEX_LOCK(vm_table_mutex);
    (void)Scm_HashCoreSearch(&vm_table, (intptr_t)vm, SCM_DICT_DELETE);
    SCM_INTERNAL_MUTEX_UNLOCK(vm_table_mutex);
}

/*====================================================================
 * VM interpreter
 *
 *  Interprets intermediate code CODE on VM.
 */

/*
 * Micro-operations
 */

/* fetching */
#define INCR_PC                 (PC++)
#define FETCH_LOCATION(var)     ((var) = (ScmWord*)*PC)
#define FETCH_OPERAND(var)      ((var) = SCM_OBJ(*PC))
#define FETCH_OPERAND_PUSH      (*SP++ = SCM_OBJ(*PC))

#ifndef COUNT_INSN_FREQUENCY
#define FETCH_INSN(var)         ((var) = *PC++)
#else
#define FETCH_INSN(var)         ((var) = fetch_insn_counting(vm, var))
#endif

/* For sanity check in debugging mode */
#ifdef PARANOIA
#define CHECK_STACK_PARANOIA(n)  CHECK_STACK(n)
#else
#define CHECK_STACK_PARANOIA(n)  (void)(n) /*dummy - avoid unused var warning*/
#endif

/* Hint for gcc -- at this moment, using __builtin_expect doesn't
   do any good (except for SCM_PROF_COUNT_CALL). I'll try this
   later on. */
#if 1
#define MOSTLY_FALSE(expr)  __builtin_expect(expr, 0)
#else
#define MOSTLY_FALSE(expr)  expr
#endif

/* Find the stack bottom next to the continuation frame.
   This macro should be applied only if CONT is in stack. */
#define CONT_FRAME_END(cont)  ((ScmObj*)(cont) + CONT_FRAME_SIZE)

/* check if *pc is an return instruction.  if so, some
   shortcuts are taken. */
#define TAIL_POS()         (*PC == SCM_VM_INSN(SCM_VM_RET))

/* push OBJ to the top of the stack
   The evaluation of OBJ may require to refer SP, so we can't
   just say *SP++ = (obj). */
#define PUSH_ARG(obj)      (*SP = (obj), SP++)

/* pop the top object of the stack and store it to VAR */
#define POP_ARG(var)       ((var) = *--SP)

#define SHIFT_FRAME(from, to, size)                     \
    do {                                                \
        ScmObj *f = (from), *t = (to);                  \
        int c;                                          \
        for (c=0; c<(size); c++, f++, t++) *t = *f;     \
    } while (0)

/* VM registers.  We benchmarked if keeping some of those registers
   in local variables makes VM loop run faster; however, it turned out
   that more local variables tended to make them spill from machine
   registers and didn't improve performance.  Having only 'vm', a pointer
   to the current VM, on register is enough. */
#define PC    (vm->pc)
#define SP    (vm->sp)
#define VAL0  (vm->val0)
#define ENV   (vm->env)
#define DENV  (vm->denv)
#define CONT  (vm->cont)
#define ARGP  (vm->argp)
#define BASE  (vm->base)

/* IN_STACK_P(ptr) returns true if ptr points into the active stack area.
   IN_FULL_STACK_P(ptr) returns true if ptr points into any part of the stack.
 */

#if GAUCHE_SPLIT_STACK
#define IN_STACK_P(ptr)                                 \
    ((ptr) >= vm->stackBase && (ptr) < vm->stackEnd)
#define IN_FULL_STACK_P(ptr)                            \
    ((ptr) >= vm->stack && (ptr) < vm->stackEnd)
#else  /*!GAUCHE_SPLIT_STACK*/
#define IN_STACK_P(ptr)                                         \
    ((unsigned long)((ptr) - vm->stack) < SCM_VM_STACK_SIZE)
#define IN_FULL_STACK_P(ptr) IN_STACK_P(ptr)
#endif /*!GAUCHE_SPLIT_STACK*/

/* Check if stack has room at least SIZE words.  If not, active frames
   are moved to the heap to make room. */
#define CHECK_STACK(size)                                       \
    do {                                                        \
        if (MOSTLY_FALSE(SP >= vm->stackEnd - (size))) {        \
            save_stack(vm);                                     \
        }                                                       \
    } while (0)

/* Push a continuation frame.  next_pc is the PC from where execution
   will be resumed.  */
#define PUSH_CONT(next_pc)                              \
    do {                                                \
        ScmContFrame *newcont = (ScmContFrame*)SP;      \
        newcont->prev = CONT;                           \
        newcont->env = ENV;                             \
        newcont->denv = DENV;                           \
        newcont->size = (int)(SP - ARGP);               \
        newcont->marker = 0;                            \
        newcont->cpc = PC;                              \
        newcont->pc = next_pc;                          \
        newcont->base = BASE;                           \
        CONT = newcont;                                 \
        SP += CONT_FRAME_SIZE;                          \
        ARGP = SP;                                      \
    } while (0)

/* pop a continuation frame, i.e. return from a procedure. */
#define POP_CONT()                                                      \
    do {                                                                \
        if (C_CONTINUATION_P(CONT)) {                                   \
            ScmContFrame *cont = CONT;                                  \
            ScmObj v = VAL0;                                            \
            if (IN_STACK_P((ScmObj*)cont)) {                            \
                SP = (ScmObj*)cont - cont->size;                        \
            }                                                           \
            ENV = NULL;                                                 \
            DENV = cont->denv;                                          \
            ARGP = SP;                                                  \
            PC = PC_TO_RETURN;                                          \
            BASE = cont->base;                                          \
            CONT = cont->prev;                                          \
            vm->ccont = cont->cpc;                                      \
            SCM_FLONUM_ENSURE_MEM(v);                                   \
            vm->trampoline = -1;                                        \
            ScmPContinuationProc *cproc = (ScmPContinuationProc*)cont->pc; \
            ScmObj *data = (ScmObj*)cont - cont->size;                  \
            VAL0 = cproc(vm, v, data);                                  \
            for (int argc = vm->trampoline; argc >= 0; argc = vm->trampoline) { \
                vm->trampoline = -1;                                    \
                VAL0 = SCM_SUBR(VAL0)->func(ARGP, argc, SCM_SUBR(VAL0)->data); \
            }                                                           \
        } else if (IN_STACK_P((ScmObj*)CONT)) {                         \
            SP   = (ScmObj*)CONT;                                       \
            ENV  = CONT->env;                                           \
            DENV = CONT->denv;                                          \
            ARGP = SP - CONT->size;                                     \
            PC   = CONT->pc;                                            \
            BASE = CONT->base;                                          \
            CONT = CONT->prev;                                          \
        } else {                                                        \
            int size__ = CONT->size;                                    \
            ARGP = SP = vm->stackBase;                                  \
            ENV = CONT->env;                                            \
            DENV = CONT->denv;                                          \
            PC = CONT->pc;                                              \
            BASE = CONT->base;                                          \
            if (size__) {                                               \
                ScmObj *s__ = (ScmObj*)CONT - size__;                   \
                ScmObj *d__ = SP;                                       \
                while (s__ < (ScmObj*)CONT) {                           \
                    *d__++ = *s__++;                                    \
                }                                                       \
                SP = d__;                                               \
            }                                                           \
            CONT = CONT->prev;                                          \
        }                                                               \
    } while (0)

/* return operation. */
#define RETURN_OP()                                     \
    do {                                                \
        if (CONT == NULL || BOUNDARY_FRAME_P(CONT)) {   \
            return; /* no more continuations */         \
        } else if (MARKER_FRAME_P(CONT)) {              \
            POP_CONT();                                 \
            /* the end of partial continuation */       \
            vm->cont = NULL;                            \
        } else {                                        \
            POP_CONT();                                 \
        }                                               \
    } while (0)

/* push environment header to finish the environment frame.
   env, sp, argp is updated. */
#define FINISH_ENV(info_, up_)                  \
    do {                                        \
        ScmEnvFrame *e__ = (ScmEnvFrame*)SP;    \
        e__->up = up_;                          \
        e__->info = info_;                      \
        e__->size = SP - ARGP;                  \
        SP += ENV_HDR_SIZE;                     \
        ARGP = SP;                              \
        ENV = e__;                              \
    } while (0)

/* extend the current environment by SIZE words.   used for LET. */
#define PUSH_LOCAL_ENV(size_, info_)            \
    do {                                        \
        int i__;                                \
        for (i__=0; i__<size_; i__++) {         \
            *SP++ = SCM_UNDEFINED;              \
        }                                       \
        FINISH_ENV(info_, ENV);                 \
    } while (0)

/* used for the inlined instruction which is supposed to be called at
   tail position (e.g. SLOT-REF).  This checks whether we're at the tail
   position or not, and if not, push a cont frame to make the operation
   a tail call. */
#define TAIL_CALL_INSTRUCTION()                 \
    do {                                        \
        if (!TAIL_POS()) {                      \
            CHECK_STACK(CONT_FRAME_SIZE);       \
            PUSH_CONT(PC);                      \
            PC = PC_TO_RETURN;                  \
        }                                       \
    } while (0)

/* global reference.  this piece of code is used for a few GREF-something
   combined instruction.
   NB: Unbound variable situation is detected and an error is thrown
   in Scm_GlocGetValue or Scm_IdentifierGlobalRef.
 */
#define GLOBAL_REF(v)                                                   \
    do {                                                                \
        ScmGloc *gloc;                                                  \
        FETCH_OPERAND(v);                                               \
        if (!SCM_GLOCP(v)) {                                            \
            VM_ASSERT(SCM_IDENTIFIERP(v));                              \
            v = Scm_IdentifierGlobalRef(SCM_IDENTIFIER(v), &gloc);      \
            /* memorize gloc */                                         \
            *PC = SCM_WORD(gloc);                                       \
        } else {                                                        \
            v = Scm_GlocGetValue(SCM_GLOC(v));                          \
        }                                                               \
        if (SCM_AUTOLOADP(v)) {                                         \
            v = Scm_ResolveAutoload(SCM_AUTOLOAD(v), 0);                \
        }                                                               \
        INCR_PC;                                                        \
    } while (0)

/* for debug */
#define VM_DUMP(delimiter)                      \
    fprintf(stderr, delimiter);                 \
    Scm_VMDump(vm)

#define VM_ASSERT(expr)                                                 \
    do {                                                                \
        if (!(expr)) {                                                  \
            fprintf(stderr, "\"%s\", line %d: Assertion failed: %s\n",  \
                    __FILE__, __LINE__, #expr);                         \
            Scm_VMDump(theVM);                                          \
            Scm_Panic("exiting...\n");                                  \
        }                                                               \
    } while (0)

#define VM_ERR(errargs)                         \
    do {                                        \
        Scm_Error errargs;                      \
    } while (0)

/* Discard the current procedure's local frame before performing a tail call.
   Just before the tail call, the typical stack position is like this:

   SP  >|      |
        | argN |
        |   :  |
   ARGP>| arg0 |
        | env  |
   ENV >| env  |
        |localM|
        |   :  |
        |local0|
   CONT>| cont |

  Arg0...argN is the arguments for the call, and local0...localM is the
  environment of the current procedure, which is no longer needed.
  We shift arg0...argN to just above the continuation frame.

  If the continuation frame has been saved (i.e. CONT is not pointing
  the stack area), then we know for sure that there's no valid data
  from the stack bottom to ARGP.  So we shift arg0...argN to the
  beginning of the stack.  We set ENV = NULL afterwards to prevent
  the debugging process from being confused---at the end of the procedure
  calling sequence, ENV is set to point to the newly formed environment
  frame out of arg0...argN.

  MEMO: this shifting used to be done after folding &rest arguments.
  Benchmark showed shifting first is slightly faster.
*/
#define DISCARD_ENV()                                                   \
    do {                                                                \
        ScmObj *to;                                                     \
        int argc = (int)(SP - ARGP);                                    \
        if (IN_STACK_P((ScmObj*)CONT)) {                                \
            to = CONT_FRAME_END(CONT);                                  \
        } else {                                                        \
            to = vm->stackBase;                                         \
        }                                                               \
        if (argc) {                                                     \
            ScmObj *t = to, *a = ARGP;                                  \
            int c;                                                      \
            for (c=0; c<argc; c++) *t++ = *a++;                         \
        }                                                               \
        ARGP = to;                                                      \
        SP = to + argc;                                                 \
        ENV = NULL;                                                     \
    } while (0)


/* inline expansion of number comparison. */
#define NUM_CMP(op, r)                                          \
    do {                                                        \
        ScmObj x_, y_ = VAL0;                                   \
        POP_ARG(x_);                                            \
        if (SCM_INTP(y_) && SCM_INTP(x_)) {                     \
            r = ((signed long)(intptr_t)x_ op (signed long)(intptr_t)y_); \
        } else if (SCM_FLONUMP(y_) && SCM_FLONUMP(x_)) {        \
            r = (SCM_FLONUM_VALUE(x_) op SCM_FLONUM_VALUE(y_)); \
        } else {                                                \
            r = (Scm_NumCmp(x_, y_) op 0);                      \
        }                                                       \
    } while (0)

#define NUM_CCMP(op, r)                                         \
    do {                                                        \
        ScmObj x_, y_ = VAL0;                                   \
        FETCH_OPERAND(x_);                                      \
        r = (SCM_FLONUM_VALUE(x_) op Scm_GetDouble(y_));        \
    } while (0)

/* We take advantage of GCC's `computed goto' feature
   (see gcc.info, "Labels as Values").
   'NEXT' or 'NEXT_PUSHCHECK' is placed at the end of most
   vm insn handlers to dispatch to the next instruction.
   We don't simply jump to the beginning of the dispatch
   table (hence the dispatching is handled by SWITCH macro),
   since the former performs worse in branch prediction;
   at least, if we have dispatches at the end of every handler,
   we can hope the branch predictor detect frequently used
   vm insn sequence.  'NEXT_PUSHCHECK' further reduces branch
   predictor failure - quite a few insns that yield a value
   in VAL0 is followed by PUSH instruction, so we detect it
   specially.  We still use fused insns (e.g. LREF0-PUSH) when
   the combination is very frequent - but for the less frequent
   instructions, NEXT_PUSHCHECK proved effective without introducing
   new fused vm insns.
*/
#ifdef __GNUC__
#define SWITCH(val) goto *dispatch_table[val];
#define CASE(insn)  SCM_CPP_CAT(LABEL_, insn) :
#define DEFAULT     LABEL_DEFAULT :
#define DISPATCH    /*empty*/
#define NEXT                                            \
    do {                                                \
        FETCH_INSN(code);                               \
        goto *dispatch_table[SCM_VM_INSN_CODE(code)];   \
    } while (0)
#define NEXT_PUSHCHECK                                  \
    do {                                                \
        FETCH_INSN(code);                               \
        if (code == SCM_VM_PUSH) {                      \
            PUSH_ARG(VAL0);                             \
            FETCH_INSN(code);                           \
        }                                               \
        goto *dispatch_table[SCM_VM_INSN_CODE(code)];   \
    } while (0)
#else /* !__GNUC__ */
#define SWITCH(val)    switch (val)
#define CASE(insn)     case insn :
#define DISPATCH       dispatch:
#define NEXT           goto dispatch
#define NEXT_PUSHCHECK goto dispatch
#endif

/* Check VM interrupt request. */
#define CHECK_INTR \
    do { if (vm->attentionRequest) goto process_queue; } while (0)

/* WNA - "Wrong Number of Arguments" handler.  The actual call is in vmcall.c.
   We handle the autocurrying magic here.

   PROC is the procedure object (guaranteed).
   NGIVEN is # of actual args on the VM stack.  The last several args may
   be folded in a list in APPLY_CALL context.  FOLDLEN holds the number of
   folded args.  In normal call context, FOLDLEN is -1.

   If the proc is curried, the VM stack state is ready to execute next op.
   Otherwise thie procedure won't return.
*/

static void wna(ScmVM *vm SCM_UNUSED,
                ScmObj proc,
                int ngiven,
                int foldlen SCM_UNUSED)
{
    int reqargs = SCM_PROCEDURE_REQUIRED(proc);
#if 0
    /* Disabled for now.  See proc.c (Scm_CurryProcedure) for the details. */
    if (SCM_PROCEDURE_CURRYING(proc) && ngiven < reqargs && ngiven > 0) {
        VAL0 = Scm_CurryProcedure(proc, ARGP, ngiven, foldlen);
        /*TODO: how should we count this path for profiling? */
    } else {
        Scm_Error("wrong number of arguments for %S (required %d, got %d)",

                  proc, reqargs, ngiven);
        /*NOTREACHED*/
    }
#else
    Scm_Error("wrong number of arguments for %S (required %d, got %d)",
              proc, reqargs, ngiven);
#endif
}

/* local_env_shift
   Called from LOCAL-ENV-SHIFT and LOCAL-ENV-JUMP insns (see vminsn.scm),
   and adjusts env frames for optimized local function call.
   This routine does two things
   - Creates a new local env frame from the values in the current stack.
     The size of frame can be determined by SP-ARGP.
   - Discard DEPTH env frames.
 */
static void local_env_shift(ScmVM *vm, int env_depth)
{
    int nargs = (int)(SP - ARGP);
    ScmEnvFrame *tenv = ENV;
    /* We can discard env_depth environment frames.
       There are several cases:
        - if the target env frame (TENV) is in stack:
         -- if the current cont frame is over TENV
            => shift argframe on top of the current cont frame
         -- otherwise => shift argframe on top of TENV
        - if TENV is in heap:
         -- if the current cont frame is in stack
            => shift argframe on top of the current cont frame
         -- otherwise => shift argframe at the stack base
    */
    while (env_depth-- > 0) {
        SCM_ASSERT(tenv);
        tenv = tenv->up;
    }

    ScmObj *to;
    if (IN_STACK_P((ScmObj*)tenv)) {
        if (IN_STACK_P((ScmObj*)CONT) && (((ScmObj*)CONT) > ((ScmObj*)tenv))) {
            to = CONT_FRAME_END(CONT);
         } else {
            to = ((ScmObj*)tenv) + ENV_HDR_SIZE;
        }
    } else {
        if (IN_STACK_P((ScmObj*)CONT)) {
            to = CONT_FRAME_END(CONT);
        } else {
            to = vm->stackBase; /* continuation has already been saved */
        }
    }
    if (nargs > 0 && to != ARGP) {
        ScmObj *t = to;
        ScmObj *a = ARGP;
        for (int c = 0; c < nargs; c++) {
            *t++ = *a++;
        }
    }
    ARGP = to;
    SP = to + nargs;
    if (nargs > 0) { FINISH_ENV(SCM_FALSE, tenv); }
    else           { ENV = tenv; }
}


/*===================================================================
 * Main loop of VM
 */
static void run_loop()
{
    ScmVM *vm = theVM;
    ScmWord code = 0;

#ifdef __GNUC__
    static void *dispatch_table[256] = {
#define DEFINSN(insn, name, nargs, type, flags)   && SCM_CPP_CAT(LABEL_, insn),
#include "vminsn.c"
#undef DEFINSN
    };
#endif /* __GNUC__ */

    /* Records the offset of each instruction handler from run_loop entry
       address.  They can be retrieved by gauche.internal#%vm-get-insn-offsets.
       Useful for tuning if used with machine instruction-level profiler. */
    if (vminsn_offsets[0] == 0) {
        /* No need to lock, for this is only executed when run_loop runs for
           the first time, which is in Scm_Init(). */
        for (int i=0; i<SCM_VM_NUM_INSNS; i++) {
            vminsn_offsets[i] =
                (unsigned long)((char*)dispatch_table[i] - (char*)run_loop);
        }
    }

    for (;;) {
        DISPATCH;
        /*VM_DUMP("");*/
        if (vm->attentionRequest) goto process_queue;
        FETCH_INSN(code);
        SWITCH(SCM_VM_INSN_CODE(code)) {
#define VMLOOP
#include "vminsn.c"
#undef  VMLOOP
#ifndef __GNUC__
        default:
            Scm_Panic("Illegal vm instruction: %08x",
                      SCM_VM_INSN_CODE(code));
#endif
        }
      process_queue:
        CHECK_STACK(CONT_FRAME_SIZE);
        PUSH_CONT(PC);
        process_queued_requests(vm);
        POP_CONT();
        NEXT;
    }
}
/* End of run_loop */

/*==================================================================
 * Stack management
 */

/* We have 'forwarding pointer' for env and cont frames being moved.
   Forwarding pointers are resolved within these internal routines
   and should never leak out.

   Forwarded pointer is marked by the 'size' field be set -1.
   Env->up or Cont->prev field holds the relocated frame.

   Invariance: forwarded pointer only appear in stack.  We skip some
   IN_STACK_P check because of it. */

#define FORWARDED_ENV_P(e)  ((e)&&((e)->size == -1))
#define FORWARDED_ENV(e)    ((e)->up)

#define FORWARDED_CONT_P(c) ((c)&&((c)->size == -1))
#define FORWARDED_CONT(c)   ((c)->prev)

/* Performance note: As of 0.8.4_pre1, each get_env call spends about
   1us to 4us on P4 2GHz machine with several benchmark suites.  The
   average env frames to be saved is less than 3.  The ratio of the pass1
   (env frame save) and the pass 2 (cont pointer adjustment) is somewhere
   around 2:1 to 1:2.  Inlining SCM_NEW call didn't help.

   This is a considerable amount of time, since save_env may be called
   the order of 10^6 times.   I'm not sure I can optimize this routine
   further without a radical change in stack management code.

   Better strategy is to put an effort in the compiler to avoid closure
   creation as much as possible.  */

/* Move the chain of env frames from the stack to the heap,
   replacing the in-stack frames for forwarding env frames.

   This routine just moves the env frames, but leaves pointers that
   point to moved frames intact (such pointers are found only in
   the in-stack contniuation frames, chained from vm->cont).
   It's the caller's responsibility to update those pointers.

   The env frames below the stackBase is also moved, to keep the invariance
   that heap would never contain a pointer into the stack.
*/
static inline ScmEnvFrame *save_env(ScmVM *vm, ScmEnvFrame *env_begin)
{
    ScmEnvFrame *e = env_begin, *prev = NULL, *next, *head = NULL, *saved;

    if (!IN_FULL_STACK_P((ScmObj*)e)) return e;

    do {
        long esize = (long)e->size;
        if (esize < 0) {
            /* forwaded frame */
            if (prev) prev->up = FORWARDED_ENV(e);
            return head;
        }

        ScmObj *d = SCM_NEW2(ScmObj*, ENV_SIZE(esize) * sizeof(ScmObj));
        ScmObj *s = (ScmObj*)e - esize;
        for (long i=esize; i>0; i--) {
            SCM_FLONUM_ENSURE_MEM(*s);
            *d++ = *s++;
        }
        *(ScmEnvFrame*)d = *e; /* copy env header */
        saved = (ScmEnvFrame*)d;
        if (prev) prev->up = saved;
        if (head == NULL) head = saved;
        next = e->up;
        e->up = prev = saved; /* forwarding pointer */
        e->size = -1;         /* indicates forwarded */
        e->info = SCM_FALSE;
        e = next;
    } while (IN_FULL_STACK_P((ScmObj*)e));
    return head;
}

static void save_cont_1(ScmVM *vm, ScmContFrame *c)
{
    if (!IN_FULL_STACK_P((ScmObj*)c)) return;

    ScmContFrame *prev = NULL;

    /* First pass */
    do {
        int size = (CONT_FRAME_SIZE + c->size) * sizeof(ScmObj);
        ScmObj *heap = SCM_NEW2(ScmObj*, size);
        ScmContFrame *csave = (ScmContFrame*)(heap + c->size);

        /* update env ptr if necessary */
        if (FORWARDED_ENV_P(c->env)) {
            c->env = FORWARDED_ENV(c->env);
        } else if (IN_FULL_STACK_P((ScmObj*)c->env)) {
            c->env = save_env(vm, c->env);
        }

        /* copy cont frame */
        if (!C_CONTINUATION_P(c)) {
            ScmObj *s = (ScmObj*)c - c->size;
            ScmObj *d = heap;
            if (c->size) {
                for (int i=c->size; i>0; i--) {
                    SCM_FLONUM_ENSURE_MEM(*s);
                    *d++ = *s++;
                }
            }
            *(ScmContFrame*)d = *c; /* copy the frame */
        } else {
            /* C continuation */
            ScmObj *s = (ScmObj*)c - c->size;
            ScmObj *d = heap;
            for (int i=CONT_FRAME_SIZE + c->size; i>0; i--) {
                /* NB: C continuation frame contains opaque pointer,
                   so we shouldn't ENSURE_MEM. */
                *d++ = *s++;
            }
        }

        /* for boundary frame, we also need to copy promptdata */
        if (BOUNDARY_FRAME_P(c)) {
            ScmPromptData *psrc = (ScmPromptData*)c->cpc;
            ScmPromptData *psave = SCM_NEW(ScmPromptData);
            *psave = *psrc;
            csave->cpc = &psave->dummy;
        }

        /* make the orig frame forwarded */
        if (prev) prev->prev = csave;
        prev = csave;

        ScmContFrame *tmp = c->prev;
        c->prev = csave;
        c->size = -1;
        c = tmp;
    } while (IN_FULL_STACK_P((ScmObj*)c));
}


/* Copy the continuation frames to the heap.
   We run two passes, first replacing cont frames with the forwarding
   cont frames, then updates the pointers to them.
   After save_cont, the only thing possibly left in the stack is the argument
   frame pointed by vm->argp.
 */
static void save_cont(ScmVM *vm)
{
    /* Save the environment chain first. */
    vm->env = save_env(vm, vm->env);

    /* First pass */
    save_cont_1(vm, vm->cont);
#if GAUCHE_SPLIT_STACK
    save_cont_1(vm, vm->lastErrorCont);
#endif /*GAUCHE_SPLIT_STACK*/

    /* Second pass */
    if (FORWARDED_CONT_P(vm->cont)) {
        vm->cont = FORWARDED_CONT(vm->cont);
    }
#if GAUCHE_SPLIT_STACK
    if (FORWARDED_CONT_P(vm->lastErrorCont)) {
        vm->lastErrorCont = FORWARDED_CONT(vm->lastErrorCont);
    }
#endif /*GAUCHE_SPLIT_STACK*/
    for (ScmCStack *cstk = vm->cstack; cstk; cstk = cstk->prev) {
        if (FORWARDED_CONT_P(cstk->cont)) {
            cstk->cont = FORWARDED_CONT(cstk->cont);
        }
    }
    for (ScmEscapePoint *ep = vm->escapePoint; ep; ep = ep->prev) {
        if (FORWARDED_CONT_P(ep->cont)) {
            ep->cont = FORWARDED_CONT(ep->cont);
        }
    }
    vm->stackBase = vm->stack;
}

static void save_stack(ScmVM *vm)
{
#if HAVE_GETTIMEOFDAY
    int stats = SCM_VM_RUNTIME_FLAG_IS_SET(vm, SCM_COLLECT_VM_STATS);
    struct timeval t0, t1;

    if (stats) {
        gettimeofday(&t0, NULL);
    }
#endif

    save_cont(vm);
    memmove(vm->stackBase, vm->argp,
            (vm->sp - (ScmObj*)vm->argp) * sizeof(ScmObj*));
    vm->sp -= (ScmObj*)vm->argp - vm->stackBase;
    vm->argp = vm->stackBase;
    /* Clear the stack.  This removes bogus pointers and accelerates GC */
    for (ScmObj *p = vm->sp; p < vm->stackEnd; p++) *p = NULL;

#if HAVE_GETTIMEOFDAY
    if (stats) {
        gettimeofday(&t1, NULL);
        vm->stat.sovCount++;
        vm->stat.sovTime +=
            (t1.tv_sec - t0.tv_sec)*1000000+(t1.tv_usec - t0.tv_usec);
    }
#endif
}

static ScmEnvFrame *get_env(ScmVM *vm)
{
    ScmEnvFrame *e = save_env(vm, vm->env);
    if (e != vm->env) {
        vm->env = e;
        for (ScmContFrame *c = vm->cont; IN_FULL_STACK_P((ScmObj*)c); c = c->prev) {
            if (FORWARDED_ENV_P(c->env)) {
                c->env = FORWARDED_ENV(c->env);
            }
        }
    }
    return e;
}

/* Obtain calling thread's dynamic environment.
 * To support thread parameters, we have to copy the parameter alist.
 */
ScmObj get_denv(ScmVM *vm)
{
    if (vm == NULL) return SCM_NIL;

    ScmObj h = SCM_NIL, t = SCM_NIL;
    ScmObj orig = vm->denv, cp;

    /* Inner parameterization list usually shares its tail with outer
       parameterization, and we need to preserve that relationship.
       This table maps old parameter pair -> new parameter pair.
     */
    ScmHashCore ptable;
    Scm_HashCoreInitSimple(&ptable, SCM_HASH_EQ, 0, NULL);

    SCM_FOR_EACH(cp, orig) {
        if (!SCM_PAIRP(SCM_CAR(cp))) continue; /* can't happen, just in case */
        if (SCM_EQ(SCM_CAAR(cp), denv_key_parameterization)) {
            ScmObj hh = SCM_NIL, tt = SCM_NIL, ccp;
            SCM_FOR_EACH(ccp, SCM_CDAR(cp)) {
                ScmObj p = SCM_CAR(ccp);  /* (<parameter> <value>) */
                ScmDictEntry *e = Scm_HashCoreSearch(&ptable,
                                                     (intptr_t)p,
                                                     SCM_DICT_GET);
                if (e) {
                    SCM_APPEND1(hh, tt, SCM_DICT_VALUE(e));
                    break;
                }
                SCM_ASSERT(SCM_PRIMITIVE_PARAMETER_P(SCM_CAR(p)));
                ScmPrimitiveParameter *param =
                    SCM_PRIMITIVE_PARAMETER(SCM_CAR(p));
                ScmObj newp = p;
                if (!Scm_PrimitiveParameterSharedP(param)) {
                    newp = Scm_Cons(SCM_CAR(newp), SCM_CDR(newp));
                }
                e = Scm_HashCoreSearch(&ptable, (intptr_t)p, SCM_DICT_CREATE);
                (void)SCM_DICT_SET_VALUE(e, newp);
                SCM_APPEND1(hh, tt, newp);
            }
            SCM_APPEND1(h, t, Scm_Cons(SCM_CAAR(cp), hh));
        } else {
            SCM_APPEND1(h, t, SCM_CAR(cp));
        }
    }
    return h;
}

/* When VM stack has incomplete stack frame (that is, SP != ARGP or
 * *PC != SCM_VM_RET), and we need to run something on VM, we should
 * preserve this incomplete frame.  Pushing an extra continuation
 * frame does the job.  We set the PC to point to RET instruction,
 * so the next time the control returns to the calling VM loop,
 * the first thing it would do is to pop this extra continuation
 * frame (unless other thigs are pushed onto the VM stack by VMPushCC).
 * Returns TRUE if a new extra frame is pushed, FALSE if not.
 *
 * Caveat: This function changes the next instruction to be executed.
 * It is a problem if this is called during VM insturctions such as
 * CAR-PUSH, which expects the next instruction to make use of the
 * pushed value---if we make RET to be executed instead of the original
 * instruction, the pushed value will be lost.  So, the caller of this
 * function needs to see if the frame is actually pushed, and call
 * Scm__VMUnprotectStack below to recover the original instruction.
 */
int Scm__VMProtectStack(ScmVM *vm)
{
    if (vm->sp != vm->argp || *vm->pc != SCM_VM_INSN(SCM_VM_RET)) {
        CHECK_STACK(CONT_FRAME_SIZE);
        PUSH_CONT(PC);
        vm->pc = PC_TO_RETURN;
        return TRUE;
    } else {
        return FALSE;
    }
}

/* The inverse of Scm__VMProtectStack.  This is required if you called
 * Scm__VMProtectStack _in_the_middle_of_VM_instruction_execution_.
 * The VM instruction may push things after that, counting on the fact
 * that subsequent instructoins use the pushed item.  However,
 * Scm__VMProtectStack makes the next instruction to be executed
 * to RET.  This makes the thing pushed by the current instruction be
 * discarded immediately, before the original subsequent instructions
 * seeing it.  Calling Scm__VMUnprotectStack restores the original next
 * instruction (assuming it's properly paired up with Scm__VMProtectStack).
 */
void Scm__VMUnprotectStack(ScmVM *vm)
{
    SCM_ASSERT(vm->pc == PC_TO_RETURN);
    POP_CONT();
}

#if GAUCHE_FFX
/* Move all the FLONUM_REGs to heap and clear the fpstack.
   We cache small number of visited env frames to avoid duplicate scanning
   (if there are more env frames, linear search in the cache gets even
   more costly than duplicate scanning).
 */

#define ENV_CACHE_SIZE 32

#undef COUNT_FLUSH_FPSTACK

#ifdef COUNT_FLUSH_FPSTACK
static int flush_fpstack_count = 0;
static u_long flush_fpstack_time = 0;
static void print_flush_fpstack_count(void*z)
{
    fprintf(stderr, "fpstack count = %d  time = %ldus (avg %fus)\n",
            flush_fpstack_count, flush_fpstack_time,
            flush_fpstack_time/(double)flush_fpstack_count);
}
#endif

void Scm_VMFlushFPStack(ScmVM *vm)
{
    ScmEnvFrame *visited[ENV_CACHE_SIZE];
    int visited_index = 0;
#ifdef COUNT_FLUSH_FPSTACK
    struct timeval t0, t1;
    gettimeofday(&t0, NULL);
#endif

    /* first, scan value registers and incomplete frames */
    SCM_FLONUM_ENSURE_MEM(VAL0);
    for (int i=0; i<SCM_VM_MAX_VALUES; i++) {
        SCM_FLONUM_ENSURE_MEM(vm->vals[i]);
    }
    if (IN_FULL_STACK_P(ARGP)) {
        for (ScmObj *p = ARGP; p < SP; p++) SCM_FLONUM_ENSURE_MEM(*p);
    }

    /* scan the main environment chain */
    ScmEnvFrame *e = ENV;
    while (IN_FULL_STACK_P((ScmObj*)e)) {
        for (int i = 0; i < visited_index; i++) {
            if (visited[i] == e) goto next;
        }
        if (visited_index < ENV_CACHE_SIZE) {
            visited[visited_index++] = e;
        }

        for (int i = 0; i < e->size; i++) {
            ScmObj *p = &ENV_DATA(e, i);
            SCM_FLONUM_ENSURE_MEM(*p);
        }
      next:
        e = e->up;
    }

    /* scan the env chains grabbed by cont chain */
    ScmContFrame *c = CONT;
    while (IN_FULL_STACK_P((ScmObj*)c)) {
        e = c->env;
        while (IN_FULL_STACK_P((ScmObj*)e)) {
            for (int i = 0; i < visited_index; i++) {
                if (visited[i] == e) goto next2;
            }
            if (visited_index < ENV_CACHE_SIZE) {
                visited[visited_index++] = e;
            }
            for (int i = 0; i < e->size; i++) {
                ScmObj *p = &ENV_DATA(e, i);
                SCM_FLONUM_ENSURE_MEM(*p);
            }
          next2:
            e = e->up;
        }
        if (IN_FULL_STACK_P((ScmObj*)c) && c->size > 0) {
            ScmObj *p = (ScmObj*)c - c->size;
            for (int i=0; i<c->size; i++, p++) SCM_FLONUM_ENSURE_MEM(*p);
        }
        c = c->prev;
    }
#if GAUCHE_SPLIT_STACK
    if ((c = vm->lastErrorCont) != NULL) {
        while (IN_FULL_STACK_P((ScmObj*)c)) {
            e = c->env;
            while (IN_FULL_STACK_P((ScmObj*)e)) {
                for (int i = 0; i < visited_index; i++) {
                    if (visited[i] == e) goto next3;
                }
                if (visited_index < ENV_CACHE_SIZE) {
                    visited[visited_index++] = e;
                }
                for (int i = 0; i < e->size; i++) {
                    ScmObj *p = &ENV_DATA(e, i);
                    SCM_FLONUM_ENSURE_MEM(*p);
                }
            next3:
                e = e->up;
            }
            if (IN_FULL_STACK_P((ScmObj*)c) && c->size > 0) {
                ScmObj *p = (ScmObj*)c - c->size;
                for (int i=0; i<c->size; i++, p++) SCM_FLONUM_ENSURE_MEM(*p);
            }
            c = c->prev;
        }
    }
#endif

    vm->fpsp = vm->fpstack;

#ifdef COUNT_FLUSH_FPSTACK
    flush_fpstack_count++;
    gettimeofday(&t1, NULL);
    flush_fpstack_time +=
        (t1.tv_sec - t0.tv_sec)*1000000+(t1.tv_usec - t0.tv_usec);
#endif
}
#undef ENV_CACHE_SIZE

#endif /*GAUCHE_FFX*/

/*
 * Dynamic environment public API
 */

static void push_dynamic_env(ScmVM *vm, ScmObj key, ScmObj val)
{
    /* If we have duplicate key in the current continuation frame,
       we need an extra care not to increase the dynenv chain.
       (See the discussion in srfi-226)
     */
    ScmContFrame *c = vm->cont;
    ScmObj limit = (c? c->denv : SCM_NIL);

    for (ScmObj p = vm->denv; SCM_PAIRP(p); p = SCM_CDR(p)) {
        if (SCM_EQ(p, limit)) break;
        if (SCM_EQ(SCM_CAAR(p), key)) {
            /* Remove duplicate key */
            ScmObj h = SCM_NIL, t = SCM_NIL;
            for (ScmObj q = vm->denv; !SCM_EQ(q, p); q = SCM_CDR(q)) {
                SCM_APPEND1(h, t, SCM_CAR(q));
            }
            SCM_APPEND(h, t, SCM_CDR(p));
            vm->denv = h;
        }
    }
    vm->denv = Scm_Acons(key, val, vm->denv);
}

/* Public API */
void Scm_VMPushDynamicEnv(ScmObj key, ScmObj val)
{
    push_dynamic_env(theVM, key, val);
}

static ScmObj find_dynamic_env(ScmVM *vm, ScmObj key, ScmObj fallback)
{
    ScmObj p = Scm_Assq(key, vm->denv);
    if (SCM_PAIRP(p)) return SCM_CDR(p);
    else return fallback;
}

/* Public API */
ScmObj Scm_VMFindDynamicEnv(ScmObj key, ScmObj fallback)
{
    return find_dynamic_env(theVM, key, fallback);
}


/*==================================================================
 * Function application from C
 */

/* The Scm_VMApply family is supposed to be called in SUBR.  It
   doesn't really applies the function in it.  Instead, it modifies
   the VM state so that the specified function will be called
   immediately after this SUBR returns to the VM.  The return value of
   Scm_VMApply is just a PROC, but it should be returned as the return
   value of SUBR, which will be used by the VM.

   The PC is modifed to point to a CALL instruction, and arguments
   are pushed onto the stack.  So, after returning to VM, it immediately
   calls VAL0 (which is PROC) with the pushed arguments.

   The Scm_pc_Apply* are more "internal" API that are only supposed
   to be called from the machine generated code.

   NB: We don't check proc is a procedure or not.  It can be a
   non-procedure object, because of the object-apply hook.

   NB: If we know that PROC is a subr and the number of arguments
   matches it expects, we use "trampoline" mode---the arguments
   are stored above SP, and vm->trampoline is set to the number
   of arguments.  In that case, VM skips the argument-folding
   and dispatching code, and immediately invokes returned subr.
   This is important for precompiled-to-C code, for it tends to
   tail-call known subrs (we need to use trampoline to ensure the
   stack won't grow).  We avoid to use trampoline when
   vm->attentionRequest is set, though, for the interrupt handling
   should be done while VM is a consistent state, but the trampoline
   is a kind of transitive state.
 */

/* Static VM instruction arrays.
   Scm_VMApplyN modifies VM's pc to point it. */

static ScmWord apply_calls[][2] = {
    { SCM_VM_INSN1(SCM_VM_TAIL_CALL, 0),
      SCM_VM_INSN(SCM_VM_RET) },
    { SCM_VM_INSN1(SCM_VM_TAIL_CALL, 1),
      SCM_VM_INSN(SCM_VM_RET) },
    { SCM_VM_INSN1(SCM_VM_TAIL_CALL, 2),
      SCM_VM_INSN(SCM_VM_RET) },
    { SCM_VM_INSN1(SCM_VM_TAIL_CALL, 3),
      SCM_VM_INSN(SCM_VM_RET) },
    { SCM_VM_INSN1(SCM_VM_TAIL_CALL, 4),
      SCM_VM_INSN(SCM_VM_RET) },
};

static ScmWord apply_callN[2] = {
    SCM_VM_INSN1(SCM_VM_TAIL_APPLY, 2),
    SCM_VM_INSN(SCM_VM_RET)
};

ScmObj Scm_VMApply(ScmObj proc, ScmObj args)
{
    int numargs = Scm_Length(args);
    int reqstack;
    ScmVM *vm = theVM;

    if (numargs < 0) Scm_Error("improper list not allowed: %S", args);
    SCM_ASSERT(TAIL_POS());
    SCM_ASSERT(ARGP == SP);
#if 0
    reqstack = ENV_SIZE(numargs) + 1;
    if (reqstack >= SCM_VM_STACK_SIZE) {
        /* there's no way we can accept that many arguments */
        Scm_Error("too many arguments (%d) to apply", numargs);
    }
    CHECK_STACK(reqstack);

    ScmObj cp;
    SCM_FOR_EACH(cp, args) {
        PUSH_ARG(SCM_CAR(cp));
    }
    if (numargs <= 4) {
        PC = apply_calls[numargs];
    } else {
        PC = SCM_NEW_ARRAY(ScmWord, 2);
        PC[0] = SCM_VM_INSN1(SCM_VM_TAIL_CALL, numargs);
        PC[1] = SCM_VM_INSN(SCM_VM_RET);
    }
    return proc;
#else
    reqstack = ENV_SIZE(1) + 1;
    CHECK_STACK(reqstack);
    PUSH_ARG(proc);
    PC = apply_callN;
    return Scm_CopyList(args);
#endif
}

/* Shortcuts for common cases */
ScmObj Scm_pc_Apply0(ScmVM *vm, ScmObj proc)
{
    if (SCM_SUBRP(proc)
        && SCM_PROCEDURE_REQUIRED(proc) == 0
        && SCM_PROCEDURE_OPTIONAL(proc) == 0
        && !vm->attentionRequest) {
        vm->trampoline = 0;
        return proc;
    }
    vm->pc = apply_calls[0];
    return proc;
}

ScmObj Scm_VMApply0(ScmObj proc)
{
    return Scm_pc_Apply0(theVM, proc);
}

ScmObj Scm_pc_Apply1(ScmVM *vm, ScmObj proc, ScmObj arg)
{
    CHECK_STACK(1);
    if (SCM_SUBRP(proc)
        && SCM_PROCEDURE_REQUIRED(proc) == 1
        && SCM_PROCEDURE_OPTIONAL(proc) == 0
        && !vm->attentionRequest) {
        vm->trampoline = 1;
        SP[0] = arg;
        return proc;
    }
    PUSH_ARG(arg);
    PC = apply_calls[1];
    return proc;
}

ScmObj Scm_VMApply1(ScmObj proc, ScmObj arg)
{
    return Scm_pc_Apply1(theVM, proc, arg);
}

ScmObj Scm_pc_Apply2(ScmVM *vm, ScmObj proc, ScmObj arg1, ScmObj arg2)
{
    CHECK_STACK(2);
    if (SCM_SUBRP(proc)
        && SCM_PROCEDURE_REQUIRED(proc) == 2
        && SCM_PROCEDURE_OPTIONAL(proc) == 0
        && !vm->attentionRequest) {
        vm->trampoline = 2;
        SP[0] = arg1;
        SP[1] = arg2;
        return proc;
    }
    PUSH_ARG(arg1);
    PUSH_ARG(arg2);
    PC = apply_calls[2];
    return proc;
}

ScmObj Scm_VMApply2(ScmObj proc, ScmObj arg1, ScmObj arg2)
{
    return Scm_pc_Apply2(theVM, proc, arg1, arg2);
}

ScmObj Scm_pc_Apply3(ScmVM *vm, ScmObj proc,
                     ScmObj arg1, ScmObj arg2, ScmObj arg3)
{
    CHECK_STACK(3);
    if (SCM_SUBRP(proc)
        && SCM_PROCEDURE_REQUIRED(proc) == 3
        && SCM_PROCEDURE_OPTIONAL(proc) == 0
        && !vm->attentionRequest) {
        vm->trampoline = 3;
        SP[0] = arg1;
        SP[1] = arg2;
        SP[2] = arg3;
        return proc;
    }
    PUSH_ARG(arg1);
    PUSH_ARG(arg2);
    PUSH_ARG(arg3);
    PC = apply_calls[3];
    return proc;
}

ScmObj Scm_VMApply3(ScmObj proc, ScmObj arg1, ScmObj arg2, ScmObj arg3)
{
    return Scm_pc_Apply3(theVM, proc, arg1, arg2, arg3);
}

ScmObj Scm_pc_Apply4(ScmVM *vm, ScmObj proc,
                     ScmObj arg1, ScmObj arg2, ScmObj arg3, ScmObj arg4)
{
    CHECK_STACK(4);
    if (SCM_SUBRP(proc)
        && SCM_PROCEDURE_REQUIRED(proc) == 4
        && SCM_PROCEDURE_OPTIONAL(proc) == 0
        && !vm->attentionRequest) {
        vm->trampoline = 4;
        SP[0] = arg1;
        SP[1] = arg2;
        SP[2] = arg3;
        SP[3] = arg4;
        return proc;
    }
    PUSH_ARG(arg1);
    PUSH_ARG(arg2);
    PUSH_ARG(arg3);
    PUSH_ARG(arg4);
    PC = apply_calls[4];
    return proc;
}


ScmObj Scm_VMApply4(ScmObj proc,
                    ScmObj arg1, ScmObj arg2, ScmObj arg3, ScmObj arg4)
{
    return Scm_pc_Apply4(theVM, proc, arg1, arg2, arg3, arg4);
}

static ScmObj eval_restore_env(ScmObj *args SCM_UNUSED,
                               int argc SCM_UNUSED,
                               void *data)
{
    theVM->module = SCM_MODULE(data);
    return SCM_UNDEFINED;
}

/* For now, we only supports a module as the evaluation environment */
ScmObj Scm_VMEval(ScmObj expr, ScmObj e)
{
    int restore_module = SCM_MODULEP(e);
    ScmVM *vm = theVM;

    ScmObj v = Scm_Compile(expr, e);
    if (SCM_VM_COMPILER_FLAG_IS_SET(theVM, SCM_COMPILE_SHOWRESULT)) {
        Scm_CompiledCodeDump(SCM_COMPILED_CODE(v));
    }

    vm->numVals = 1;
    if (restore_module) {
        /* if we swap the module, we need to make sure it is recovered
           after eval */
        ScmObj body = Scm_MakeClosure(v, NULL);
        ScmObj before = Scm_MakeSubr(eval_restore_env, SCM_MODULE(e),
                                     0, 0, SCM_SYM_EVAL_BEFORE);
        ScmObj after = Scm_MakeSubr(eval_restore_env, (void*)vm->module,
                                    0, 0, SCM_SYM_EVAL_AFTER);
        return Scm_VMDynamicWind(before, body, after);
    } else {
        /* shortcut */
        SCM_ASSERT(SCM_COMPILED_CODE_P(v));
        vm->base = SCM_COMPILED_CODE(v);
        vm->pc = SCM_COMPILED_CODE(v)->code;
        SCM_PROF_COUNT_CALL(vm, v);
        return SCM_UNDEFINED;
    }
}

/*
 * C continuation
 *
 *   This is a bit messy due to the historical evolution of C continuations.
 */

/* Common ccont frame constructor */
static ScmObj *new_ccont(ScmVM *vm, ScmPContinuationProc *after,
                         ScmWord *cpcdata, int datasize)
{
    CHECK_STACK(CONT_FRAME_SIZE+datasize);
    ScmObj *s = SP;
    ScmContFrame *cc = (ScmContFrame*)(s + datasize);
    cc->prev = CONT;
    cc->env = &ccEnvMark;
    cc->denv = DENV;
    cc->size = datasize;
    cc->marker = 0;
    cc->cpc = cpcdata;
    cc->pc = (ScmWord*)after;
    cc->base = BASE;
    CONT = cc;
    ARGP = SP = s + datasize + CONT_FRAME_SIZE;
    return s;
}

/* This is a trick to keep the backward compatibility. */
static ScmObj ccont_adapter(ScmVM *vm, ScmObj val0, ScmObj *data)
{
    ScmCContinuationProc *ccont = (ScmCContinuationProc*)vm->ccont;
    return ccont(val0, (void**)data);
}

/* Arrange C function AFTER to be called after the procedure returns.
 * Usually followed by Scm_VMApply* function.
 *
 * This is an 'old' protocol (after procedure does not take VM pointer).
 * For the backward compatibility, we support this type of C continuation
 * with this kludge:
 *
 *  - We save 'after' CCont pointer in cont->cpc, and set the PCont
 *    pointer (cont->pc) with ccont_adapter().
 *  - In POP_CONT, when we pop the C continuation frame, cont->cpc
 *    is saved in vm->ccont immediately before PCont procedure is
 *    called.
 *  - In ccont_adapter, the original 'after' pointer is retrieved
 *    from vm->ccont and called.
 *
 * This incurs a slight overhead for the old protocol, but does not
 * tax the new protocol.
 */
void Scm_VMPushCC(ScmCContinuationProc *after,
                  void **data, int datasize)
{
    ScmVM *vm = theVM;
    ScmObj *buf = new_ccont(vm, ccont_adapter, (ScmWord*)after, datasize);
    for (int i=0; i<datasize; i++) {
        buf[i] = SCM_OBJ(data[i]);
    }
}

/* Allocate SIZE words from the VM stack.  The stack may be packed
   to make room. */
ScmObj *Scm_pc_Alloca(ScmVM *vm, size_t size)
{
    CHECK_STACK(size);
    ScmObj *r = SP;
    SP += size;
    return r;
}

/* Like VMPushCC, but we just make room for data and return the
   pointer to it, letting caller to fill it.  Supposed to be
   used by machine-generated C code. */
ScmObj *Scm_pc_PushCC(ScmVM *vm, ScmPContinuationProc *after, int datasize)
{
    return new_ccont(vm, after, NULL, datasize);
}

/* A special internal procedure to push C continuation with dynamic
   handlers.  Only used for dynamic-wind. */
static ScmObj *push_dynamic_handler_cc(ScmVM *vm, ScmPContinuationProc *after,
                                       ScmObj dh, int datasize)
{
    ScmObj *data = new_ccont(vm, after, (ScmWord*)dh, datasize);
    CONT->marker |= SCM_CONT_DYNWIND_MARKER;
    return data;
}

/*
 * EscapePoint allocation
 */
static ScmEscapePoint *new_ep(ScmVM *vm,
                              ScmObj errorHandler,
                              int rewindBefore,
                              ScmObj promptTag,
                              ScmObj abortHandler)
{
    ScmEscapePoint *ep = SCM_NEW(ScmEscapePoint);
    ep->prev = vm->escapePoint;
    ep->ehandler = errorHandler;
    ep->cont = vm->cont;
    ep->denv = vm->denv;
    ep->dynamicHandlers = get_dynamic_handlers(vm);
    ep->cstack = vm->cstack;
    ep->xhandler = Scm_VMCurrentExceptionHandler();
    ep->resetChain = vm->resetChain;
    ep->partHandlers = SCM_NIL;
    ep->errorReporting =
        SCM_VM_RUNTIME_FLAG_IS_SET(vm, SCM_ERROR_BEING_REPORTED);
    ep->rewindBefore = rewindBefore;
    ep->promptTag = promptTag;
    ep->abortHandler = abortHandler;
    ep->bottom = NULL;
    return ep;
}

/*-------------------------------------------------------------
 * User level eval and apply.
 *   When the C routine wants the Scheme code to return to it,
 *   instead of using C-continuation, the continuation
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

static ScmObj user_eval_inner(ScmObj program,
                              ScmWord *codevec,
                              ScmObj promptTag,
                              ScmObj abortHandler)
{
    ScmCStack cstack;
    ScmVM * volatile vm = theVM;
    /* Save prev_pc, for the boundary continuation uses pc slot
       to mark the boundary. */
    ScmWord * volatile prev_pc = PC;
    ScmObj vmhandlers = get_dynamic_handlers(vm);
    ScmObj vmresetChain = vm->resetChain;

    if (SCM_FALSEP(promptTag)) {
        promptTag = SCM_OBJ(&defaultPromptTag);
    }

    /* Push extra continuation.  This continuation frame is a 'boundary
       frame' and marked by marker == SCM_CONT_RESET_MARKER.   VM loop knows
       it should return to C frame when it sees a boundary frame.
       A boundary frame also keeps the unfinished argument frame at
       the point when Scm_Eval or Scm_Apply is called. */
    push_boundary_cont(vm, promptTag, abortHandler);
    SCM_ASSERT(SCM_COMPILED_CODE_P(program));
    vm->base = SCM_COMPILED_CODE(program);
    if (codevec != NULL) {
        PC = codevec;
    } else {
        PC = vm->base->code;
        CHECK_STACK(vm->base->maxstack);
    }
    SCM_PROF_COUNT_CALL(vm, program);

    cstack.prev = vm->cstack;
    cstack.cont = vm->cont;
    vm->cstack = &cstack;

  restart:
    vm->escapeReason = SCM_VM_ESCAPE_NONE;
    if (sigsetjmp(cstack.jbuf, FALSE) == 0) {
        run_loop();             /* VM loop */
        if (vm->cont == cstack.cont) {
            POP_CONT();
            PC = prev_pc;
        } else if (vm->cont == NULL) {
            /* we're finished with executing partial continuation. */

            /* restore reset-chain for reset/shift */
            vm->resetChain = vmresetChain;

            /* save return values */
            ScmObj val0 = vm->val0;
            int nvals = vm->numVals;
            ScmObj *vals = NULL;
            if (nvals > 1) {
                vals = SCM_NEW_ARRAY(ScmObj, nvals-1);
                memcpy(vals, vm->vals, sizeof(ScmObj)*(nvals-1));
            }

            /* call dynamic handlers for returning to the caller */
            call_dynamic_handlers(vm, vmhandlers,
                                  get_dynamic_handlers(vm));

            /* restore return values */
            vm->val0 = val0;
            vm->numVals = nvals;
            if (vals != NULL) {
                memcpy(vm->vals, vals, sizeof(ScmObj)*(nvals-1));
            }

            vm->cont = cstack.cont;
            POP_CONT();
            PC = prev_pc;
        } else {
            /* If we come here, we've been executing a ghost continuation.
               The C world the ghost should return no longer exists, so we
               raise an error. */
            Scm_Error("attempt to return from a ghost continuation.");
        }
    } else {
        /* An escape situation happened. */
        if (vm->escapeReason == SCM_VM_ESCAPE_CONT) {
            ScmEscapePoint *ep = (ScmEscapePoint*)vm->escapeData[0];
            if (ep->cstack == vm->cstack) {
                ScmObj handlers =
                    throw_cont_calculate_handlers(ep->dynamicHandlers,
                                                  get_dynamic_handlers(vm));
                /* force popping continuation when restarted */
                vm->pc = PC_TO_RETURN;
                vm->val0 = throw_cont_body(handlers, ep, vm->escapeData[1]);
                goto restart;
            } else {
                SCM_ASSERT(vm->cstack && vm->cstack->prev);
                vm->cont = cstack.cont;
                POP_CONT();
                vm->cstack = vm->cstack->prev;
                siglongjmp(vm->cstack->jbuf, 1);
            }
        } else if (vm->escapeReason == SCM_VM_ESCAPE_ERROR) {
            ScmEscapePoint *ep = (ScmEscapePoint*)vm->escapeData[0];
            if (ep && ep->cstack == vm->cstack) {
                vm->cont = ep->cont;
                vm->denv = ep->denv;
                vm->pc = PC_TO_RETURN;
                /* restore reset-chain for reset/shift */
                if (ep->cstack) vm->resetChain = ep->resetChain;
                goto restart;
            } else if (vm->cstack->prev == NULL) {
                /* This loop is the outermost C stack, and nobody will
                   capture the error.  This happens when the base C code
                   calls Scm_EvalRec/ApplyRec, instead of Scm_Eval/Apply.
                   We can't return, since there's no way to pass the
                   error info.  We can only just exit.
                */
                Scm_Exit(EX_SOFTWARE);
            } else {
                /* Jump again until C stack is recovered.  We could pop
                   the extra continuation frame so that the VM stack
                   is consistent. */
                vm->cont = cstack.cont;
                POP_CONT();
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

void push_prompt_cont(ScmVM *vm, ScmObj promptTag, ScmObj abortHandler)
{
    /* We want to allocate ScmPromptData on stack.  If there's already
       an unfinished argument frame, however, we need to insert ScmPromptData
       before it (for other parts of code assumes unfinished argument frame
       immediately precedes cont frame).
    */
    CHECK_STACK(CONT_FRAME_SIZE + PROMPT_DATA_SIZE + (SP - ARGP));

    ScmPromptData *a = (ScmPromptData*)ARGP;

    if (SP - ARGP > 0) {
        for (ScmObj *s = SP-1, *d = SP+PROMPT_DATA_SIZE-1;
             s >= ARGP;
             s--, d--) {
            *d = *s;
        }
    }
    SP += PROMPT_DATA_SIZE;
    ARGP += PROMPT_DATA_SIZE;

    a->dummy = SCM_VM_INSN(SCM_VM_RET);
    a->abortHandler = abortHandler;
    a->dynamicHandlers = get_dynamic_handlers(vm);
    PUSH_CONT(SCM_PROMPT_TAG_PC(promptTag));
    CONT->cpc = &a->dummy;
}

void push_boundary_cont(ScmVM *vm, ScmObj promptTag, ScmObj abortHandler)
{
    /* Boundary continuation is a prompt cont with CONT_RESET_MARKER */
    push_prompt_cont(vm, promptTag, abortHandler);
    CONT->marker = SCM_CONT_RESET_MARKER;
}

/* API for recursive call to VM.  Exceptions are not captured.
   Returns the primary result.  To retrieve the rest of results,
   you have to use Scm_VMGetResult etc. */

ScmObj Scm_EvalRec(ScmObj expr, ScmObj e)
{
    ScmObj v = Scm_Compile(expr, e);
    SCM_COMPILED_CODE(v)->name = SCM_SYM_INTERNAL_EVAL;
    if (SCM_VM_COMPILER_FLAG_IS_SET(theVM, SCM_COMPILE_SHOWRESULT)) {
        Scm_CompiledCodeDump(SCM_COMPILED_CODE(v));
    }
    return user_eval_inner(v, NULL, SCM_FALSE, SCM_FALSE);
}

/* NB: The ApplyRec family can be called in an inner loop (e.g. the display
   callback from GLUT.)  So we don't want to allocate at all.  We put
   a temporary code vector on C stack.  It is OK, since once
   user_eval_inner returns it would never be reused.   However, tools
   that want to keep a pointer to a code vector would need to be aware
   of this case. */
static ScmObj apply_rec(ScmVM *vm, ScmObj proc, int nargs)
{
    ScmWord code[2];
    code[0] = SCM_WORD(SCM_VM_INSN1(SCM_VM_VALUES_APPLY, nargs));
    code[1] = SCM_WORD(SCM_VM_INSN(SCM_VM_RET));

    vm->val0 = proc;
    ScmObj program = vm->base?
            SCM_OBJ(vm->base) : SCM_OBJ(&internal_apply_compiled_code);
    return user_eval_inner(program, code, SCM_FALSE, SCM_FALSE);
}

ScmObj Scm_ApplyRec(ScmObj proc, ScmObj args)
{
    int nargs = Scm_Length(args);
    ScmVM *vm = theVM;

    if (nargs < 0) {
        Scm_Error("improper list not allowed: %S", args);
    }

    for (int i=0; i<nargs; i++) {
        if (i == SCM_VM_MAX_VALUES-1) {
            vm->vals[i] = args;
            break;
        }
        vm->vals[i] = SCM_CAR(args);
        args = SCM_CDR(args);
    }
    return apply_rec(vm, proc, nargs);
}

ScmObj Scm_ApplyRec0(ScmObj proc)
{
    return apply_rec(theVM, proc, 0);
}

ScmObj Scm_ApplyRec1(ScmObj proc, ScmObj arg0)
{
    ScmVM *vm = theVM;
    vm->vals[0] = arg0;
    return apply_rec(vm, proc, 1);
}

ScmObj Scm_ApplyRec2(ScmObj proc, ScmObj arg0, ScmObj arg1)
{
    ScmVM *vm = theVM;
    vm->vals[0] = arg0;
    vm->vals[1] = arg1;
    return apply_rec(vm, proc, 2);
}

ScmObj Scm_ApplyRec3(ScmObj proc, ScmObj arg0, ScmObj arg1, ScmObj arg2)
{
    ScmVM *vm = theVM;
    vm->vals[0] = arg0;
    vm->vals[1] = arg1;
    vm->vals[2] = arg2;
    return apply_rec(vm, proc, 3);
}

ScmObj Scm_ApplyRec4(ScmObj proc, ScmObj arg0, ScmObj arg1, ScmObj arg2,
                     ScmObj arg3)
{
    ScmVM *vm = theVM;
    vm->vals[0] = arg0;
    vm->vals[1] = arg1;
    vm->vals[2] = arg2;
    vm->vals[3] = arg3;
    return apply_rec(vm, proc, 4);
}

ScmObj Scm_ApplyRec5(ScmObj proc, ScmObj arg0, ScmObj arg1, ScmObj arg2,
                     ScmObj arg3, ScmObj arg4)
{
    ScmVM *vm = theVM;
    vm->vals[0] = arg0;
    vm->vals[1] = arg1;
    vm->vals[2] = arg2;
    vm->vals[3] = arg3;
    vm->vals[4] = arg4;
    return apply_rec(vm, proc, 5);
}

/*
 * Safe version of user-level Eval, Apply and Load.
 * Exceptions are caught and stored in ScmEvalPacket.
 */

enum {
    SAFE_EVAL,
    SAFE_EVAL_CSTRING,
    SAFE_APPLY
};

struct eval_packet_rec {
    ScmObj env;
    int kind;
    ScmObj arg0;      /* form (EVAL), proc (APPLY) */
    ScmObj args;      /* args (APPLY) */
    const char *cstr; /* cstring (EVAL_CSTRING) */
    ScmObj exception;
};

static ScmObj safe_eval_handler(ScmObj *args,
                                int nargs, void *data)
{
    SCM_ASSERT(nargs == 1);
    ((struct eval_packet_rec *)data)->exception = args[0];
    return SCM_UNDEFINED;
}

static ScmObj safe_eval_thunk(ScmObj *args SCM_UNUSED,
                              int nargs SCM_UNUSED,
                              void *data)
{
    struct eval_packet_rec *epak = (struct eval_packet_rec*)data;
    ScmObj r;

    switch (epak->kind) {
    case SAFE_EVAL_CSTRING:
        r = Scm_VMEval(Scm_ReadFromCString(epak->cstr), epak->env);
        break;
    case SAFE_EVAL:
        r = Scm_VMEval(epak->arg0, epak->env);
        break;
    case SAFE_APPLY:
        r = Scm_VMApply(epak->arg0, epak->args);
        break;
    default:
        Scm_Panic("safe_eval_subr: bad kind");
        return SCM_UNBOUND;     /* dummy */
    }
    /* If expressino was select-module, the current module may be changed. */
    epak->env = SCM_OBJ(SCM_CURRENT_MODULE());
    return r;
}

static ScmObj safe_eval_int(ScmObj *args SCM_UNUSED,
                            int nargs SCM_UNUSED,
                            void *data)
{
    ScmObj thunk   = Scm_MakeSubr(safe_eval_thunk, data, 0, 0, SCM_FALSE);
    ScmObj handler = Scm_MakeSubr(safe_eval_handler, data, 1, 0, SCM_FALSE);
    return Scm_VMWithErrorHandler(handler, thunk);
}

static int safe_eval_wrap(int kind, ScmObj arg0, ScmObj args,
                          const char *cstr, ScmObj env,
                          ScmEvalPacket *result)
{
    ScmVM *vm = theVM;

    struct eval_packet_rec epak;
    epak.env  = env;
    epak.kind = kind;
    epak.arg0 = arg0;
    epak.args = args;
    epak.cstr = cstr;
    epak.exception = SCM_UNBOUND;

    ScmObj proc = Scm_MakeSubr(safe_eval_int, &epak, 0, 0, SCM_FALSE);
    ScmObj r = Scm_ApplyRec(proc, SCM_NIL);

    if (SCM_UNBOUNDP(epak.exception)) {
        /* normal termination */
        if (result) {
            result->numResults = vm->numVals;
            result->results[0] = r;
            for (int i=1; i<vm->numVals; i++) {
                result->results[i] = vm->vals[i-1];
            }
            result->exception = SCM_FALSE;
            if (SCM_MODULEP(epak.env)) {
                result->module = SCM_MODULE(epak.env);
            }
        }
        return vm->numVals;
    } else {
        /* abnormal termination */
        if (result) {
            result->numResults = 0;
            result->exception = epak.exception;
        }
        return -1;
    }
}

int Scm_Eval(ScmObj form, ScmObj env, ScmEvalPacket *packet)
{
    return safe_eval_wrap(SAFE_EVAL, form, SCM_FALSE, NULL, env, packet);
}

int Scm_EvalCString(const char *expr, ScmObj env, ScmEvalPacket *packet)
{
    return safe_eval_wrap(SAFE_EVAL_CSTRING, SCM_FALSE, SCM_FALSE,
                          expr, env, packet);
}

int Scm_Apply(ScmObj proc, ScmObj args, ScmEvalPacket *packet)
{
    return safe_eval_wrap(SAFE_APPLY, proc, args, NULL, SCM_FALSE, packet);
}

/*
 * A subroutine to be called while executing apply instruction.
 * Apply needs to check the argument tail is a valid list.  However,
 * naively using ordinary procedures can trigger forcing of lazy
 * pair, which breaks VM state.
 *
 * When called, we know vm->sp[-1] contains the tail list of arguments.
 * It may be a lazy pair.  If so, we should force it in the safe environment.
 * Returns the length of the tail list.
 *
 * Currently, we force the lazy argument tail in very inefficient way,
 * assuming that such case is very rare.
 *
 * When max_limit is non-negative, we only check up that number of
 * arguments and bail. For the true length, the caller should call
 * again with max_limit == -1.
 */

int check_arglist_tail_for_apply(ScmVM *vm SCM_UNUSED, ScmObj z, int max_limit)
{
    int count = 0;
    static ScmObj length_proc = SCM_UNDEFINED;
    ScmObj tortoise = z;

    for (;;) {
        if (SCM_NULLP(z)) return count;
        if (SCM_LAZY_PAIR_P(z)) goto do_lazy_pair;
        if (!SCM_PAIRP(z)) goto bad_list;

        z = SCM_CDR(z);
        count++;

        if (SCM_NULLP(z)) return count;
        if (SCM_LAZY_PAIR_P(z)) goto do_lazy_pair;
        if (!SCM_PAIRP(z)) goto bad_list;

        z = SCM_CDR(z);
        tortoise = SCM_CDR(tortoise);
        if (z == tortoise) goto bad_list; /* circular */
        count++;

        if (max_limit >= 0 && count >= max_limit) return count;
    }

do_lazy_pair:
    {
        ScmEvalPacket result;
        SCM_BIND_PROC(length_proc, "length", Scm_GaucheModule());
        int nres = Scm_Apply(length_proc, SCM_LIST1(z), &result);
        if (nres == -1) Scm_Raise(result.exception, 0);
        SCM_ASSERT(nres == 1);
        SCM_ASSERT(SCM_INTP(result.results[0]));
        count += SCM_INT_VALUE(result.results[0]);
        return count;
    }
bad_list:
    Scm_Error("improper list not allowed: %S", tortoise);
}

/*=================================================================
 * Dynamic handlers
 */

/* On <handler-chain>
 *
 *   Dynamic handlers is a list of <handler-entry>.  Each <handler-entry>
 *   keeps the "before" and "after" thunk of the dynamic-wind.
 *
 *   <handler-entry> is ScmDynamicHandler object.  It should be
 *   treated as an opaque object except in vm.c
 *
 *   It has the following members:
 *
 *     <before-proc>
 *     <after-proc>
 *     <denv>
 *     <args>
 *
 *   <before-proc> and <after-proc> are before and after procedures.  With
 *   the basic dynamic-wind, they're thunks.  When optimized, <before-proc>
 *   and <after-proc> may take arguments <args> ....
 *   <denv> is the dynamic environment on which handlers should be called.
 */

/* Class stuff */
static void dynamic_handler_print(ScmObj obj, ScmPort *out,
                                        ScmWriteContext *ctx SCM_UNUSED)
{
    Scm_Printf(out, "#<dynamic-handler %S %S>",
               SCM_DYNAMIC_HANDLER(obj)->before,
               SCM_DYNAMIC_HANDLER(obj)->after);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_DynamicHandlerClass,
                         dynamic_handler_print, NULL, NULL, NULL,
                         SCM_CLASS_DEFAULT_CPL);

ScmObj make_dynamic_handler(ScmVM *vm, ScmObj before, ScmObj after, ScmObj args)
{
    ScmDynamicHandler *e = SCM_NEW(ScmDynamicHandler);
    SCM_SET_CLASS(e, SCM_CLASS_DYNAMIC_HANDLER);
    e->before = before;
    e->after = after;
    e->denv = vm->denv;
    e->args = args;
    return SCM_OBJ(e);
}

/* Handler-chain internal API */
static ScmObj get_dynamic_handlers(ScmVM *vm)
{
    return vm->dynamicHandlers;
}

static void set_dynamic_handlers(ScmVM *vm, ScmObj handlers)
{
    vm->dynamicHandlers = handlers;
}

static ScmObj push_dynamic_handlers(ScmVM *vm,
                                    ScmObj before, ScmObj after, ScmObj args)
{
    ScmObj entry = make_dynamic_handler(vm, before, after, args);
    set_dynamic_handlers(vm, Scm_Cons(entry, get_dynamic_handlers(vm)));
    return entry;
}

static ScmObj pop_dynamic_handlers(ScmVM *vm)
{
    ScmObj handlers = get_dynamic_handlers(vm);
    SCM_ASSERT(SCM_PAIRP(handlers));
    set_dynamic_handlers(vm, SCM_CDR(handlers));
    return SCM_CAR(handlers);
}

static void call_before_thunk(ScmVM *vm, ScmObj handler_entry)
{
    SCM_ASSERT(SCM_DYNAMIC_HANDLER_P(handler_entry));
    vm->denv = SCM_DYNAMIC_HANDLER(handler_entry)->denv;
    Scm_ApplyRec(SCM_DYNAMIC_HANDLER(handler_entry)->before,
                 SCM_DYNAMIC_HANDLER(handler_entry)->args);
}

static void call_after_thunk(ScmVM *vm, ScmObj handler_entry)
{
    SCM_ASSERT(SCM_DYNAMIC_HANDLER_P(handler_entry));
    vm->denv = SCM_DYNAMIC_HANDLER(handler_entry)->denv;
    Scm_ApplyRec(SCM_DYNAMIC_HANDLER(handler_entry)->after,
                 SCM_DYNAMIC_HANDLER(handler_entry)->args);
}

static ScmObj vm_call_before_thunk(ScmVM *vm, ScmObj handler_entry)
{
    SCM_ASSERT(SCM_DYNAMIC_HANDLER_P(handler_entry));
    vm->denv = SCM_DYNAMIC_HANDLER(handler_entry)->denv;
    return Scm_VMApply(SCM_DYNAMIC_HANDLER(handler_entry)->before,
                       SCM_DYNAMIC_HANDLER(handler_entry)->args);
}

static ScmObj vm_call_after_thunk(ScmVM *vm, ScmObj handler_entry)
{
    SCM_ASSERT(SCM_DYNAMIC_HANDLER_P(handler_entry));
    vm->denv = SCM_DYNAMIC_HANDLER(handler_entry)->denv;
    return Scm_VMApply(SCM_DYNAMIC_HANDLER(handler_entry)->after,
                       SCM_DYNAMIC_HANDLER(handler_entry)->args);
}
/* End of handler-chain internal API */

/* public api */
void Scm_VMPushDynamicHandlers(ScmObj before, ScmObj after, ScmObj args)
{
    push_dynamic_handlers(theVM, before, after, args);
}

ScmObj Scm_VMGetDynamicHandlers()
{
    return get_dynamic_handlers(theVM);
}

void Scm_VMSetDynamicHandlers(ScmObj handlers)
{
    set_dynamic_handlers(theVM, handlers);
}

static ScmPContinuationProc dynwind_before_cc;
static ScmPContinuationProc dynwind_body_cc;
static ScmPContinuationProc dynwind_after_cc;

ScmObj Scm_VMDynamicWind(ScmObj before, ScmObj body, ScmObj after)
{
    ScmVM *vm = Scm_VM();
    /* NB: we don't check types of arguments, since they can be non-procedure
       objects with object-apply hooks. */
    ScmObj *data = Scm_pc_PushCC(vm, dynwind_before_cc, 3);
    data[0] = before;
    data[1] = body;
    data[2] = after;
    return Scm_VMApply0(before);
}

static ScmObj dynwind_before_cc(ScmVM *vm,
                                ScmObj result SCM_UNUSED,
                                ScmObj *data)
{
    ScmObj before = data[0];
    ScmObj body   = data[1];
    ScmObj after  = data[2];

    ScmObj dhentry = push_dynamic_handlers(vm, before, after, SCM_NIL);
    ScmObj *d = push_dynamic_handler_cc(vm, dynwind_body_cc, dhentry, 1);
    d[0] = after;
    return Scm_VMApply0(body);
}

static ScmObj dynwind_body_cc(ScmVM *vm, ScmObj result, ScmObj *data)
{
    ScmObj after = data[0];

    pop_dynamic_handlers(vm);

    /* Save return values.
       We could avoid malloc when numVals is small (we can push
       them directly onto the stack).  But our benchmark showed doing so
       actually gets slightly slower.  More branches may have a negative
       effect.  So we keep it simple here.
     */
    int nvals = vm->numVals;
    if (nvals > 1) {
        ScmObj *vals = SCM_NEW_ARRAY(ScmObj, nvals-1);
        memcpy(vals, vm->vals, sizeof(ScmObj)*(nvals-1));
        ScmObj *d = Scm_pc_PushCC(vm, dynwind_after_cc, 3);
        d[0] = result;
        d[1] = SCM_OBJ((intptr_t)nvals);
        d[2] = SCM_OBJ(vals);
    } else {
        ScmObj *d = Scm_pc_PushCC(vm, dynwind_after_cc, 2);
        d[0] = result;
        d[1] = SCM_OBJ((intptr_t)nvals);
    }
    return Scm_VMApply0(after);
}

static ScmObj dynwind_after_cc(ScmVM *vm, ScmObj result SCM_UNUSED,
                               ScmObj *data)
{
    /* Restore return values. */
    ScmObj val0 = SCM_OBJ(data[0]);
    int nvals = (int)(intptr_t)data[1];
    vm->numVals = nvals;
    if (nvals > 1) {
        ScmObj *vals = (ScmObj*)data[2];
        SCM_ASSERT(nvals <= SCM_VM_MAX_VALUES);
        memcpy(vm->vals, vals, sizeof(ScmObj)*(nvals-1));
    }
    return val0;
}

/*=================================================================
 * Exception handling
 */

/* Conceptually, exception handling is nothing more than a particular
 * combination of dynamic-wind and call/cc.   Gauche implements a parts
 * of it in C so that it will be efficient and safer to use.
 *
 * The most basic layer consists of these two functions:
 *
 *  with-exception-handler
 *  raise
 *
 * There is a slight problem, though.  These two functions are defined
 * both in srfi-18 (multithreads) and srfi-34 (exception handling), and
 * two disagrees in the semantics of raise.
 *
 * Srfi-18 requires an exception handler to be called with the same dynamic
 * environment as the one of the primitive that raises the exception.
 * That means when an exception handler is running, the current
 * exception handler is the running handler itself.  Naturally, calling
 * raise unconditionally within the exception handler causes infinite loop.
 *
 * Srfi-34 says that an exception handler is called with the same dynamic
 * environment where the exception is raised, _except_ that the current
 * exception handler is "popped", i.e. when an exception handler is running,
 * the current exception handler is the "outer" or "old" one.  Calling
 * raise within an exception handler passes the control to the outer
 * exception handler.
 *
 * We used to follow srfi-18 model, for it is more "primitive".  However,
 * R7RS adopted srfi-34 model, and there were enough confusions on entering
 * infinite loop, so we swithec to the srfi-34 model.
 * See Scm_VMThrowException.
 *
 * The following is a model of the current implementation, sans the messy
 * part of handling C stacks.  The exception handler chain is stored in
 * the dynamic environment.  Suppose %get-current-xhs retrieves the current
 * exception handler chain, and (%with-xhs new-xhs expr) calls expr with
 * new-xhs as the exception handler chain.
 *
 *  (define (current-exception-handler)
 *    (car (%get-current-xhs)))
 *
 *  (define (raise exn)
 *    (let1 xhs (%get-current-xhs)
 *      (receive r ((car xhs) exn)
 *        (when (uncontinuable-exception? exn)
 *          (%with-xhs (cdr xhs)
 *            (error "returned from uncontinuable exception")))
 *        (apply values r))))
 *
 *  (define (with-exception-handler handler thunk)
 *    (%with-xhs (cons handler (%get-current-xhs))
 *      (thunk)))
 *
 * Note that this model assumes an exception handler returns unless it
 * explicitly invokes continuation captured elsewhere.   In reality,
 * "error" exceptions are not supposed to return (hence it is checked
 * in raise).  Gauche provides another useful exception handling
 * constructs that automates such continuation capturing.  It can be
 * explained by the following code.
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

ScmObj Scm_VMDefaultExceptionHandler(ScmObj e)
{
    ScmVM *vm = theVM;
    ScmEscapePoint *ep = vm->escapePoint;

    if (ep) {
        /* There's an escape point defined by with-error-handler. */
        ScmObj vmhandlers = get_dynamic_handlers(vm);
        ScmObj result = SCM_FALSE, rvals[SCM_VM_MAX_VALUES];
        int numVals = 0;

        /* Save continuation chains into heap.  This has non-negligible
           overhead, but we need to keep all active EPs' cont field valid.
           Without this, a complication occurs when save_cont occurs during
           executing the error handler and it returns subsequently.
           See https://github.com/shirok/Gauche/issues/852 for the details.
         */
        save_cont(vm);

#if GAUCHE_SPLIT_STACK
        vm->lastErrorCont = vm->cont;
        vm->stackBase = vm->sp;
#endif

        /* To conform SRFI-34, the error handler (clauses in 'guard' form)
           should be executed with the same continuation and dynamic
           environment of the guard form itself.  That means the dynamic
           handlers should be rewound before we invoke the guard clause.

           If an error is raised within the dynamic handlers, it will be
           captured by the same error handler. */
        if (ep->rewindBefore) {
            call_dynamic_handlers(vm, ep->dynamicHandlers,
                                  get_dynamic_handlers(vm));
        }

        /* Pop the EP and run the error handler. */
        vm->escapePoint = ep->prev;

        vm->errorHandlerContinuable = FALSE;

        result = Scm_ApplyRec(ep->ehandler, SCM_LIST1(e));

        /* save return values */
        /* NB: for loop is slightly faster than memcpy */
        numVals = vm->numVals;
        if (numVals > 1) {
            for (int i=0; i<numVals-1; i++) rvals[i] = vm->vals[i];
        }

        /* call dynamic handlers to rewind */
        if (!ep->rewindBefore) {
            call_dynamic_handlers(vm, ep->dynamicHandlers,
                                  get_dynamic_handlers(vm));
        }

        /* If exception is reraised, the exception handler can return
           to the caller. */
        if (vm->errorHandlerContinuable) {
            vm->errorHandlerContinuable = FALSE;

            /* recover escape point */
            vm->escapePoint = ep;

            /* call dynamic handlers to reenter dynamic-winds */
            call_dynamic_handlers(vm, vmhandlers,
                                  get_dynamic_handlers(vm));

            /* reraise and return */
            Scm_VMPushExceptionHandler(ep->xhandler);
            vm->escapePoint = ep->prev;
            result = Scm_VMThrowException(vm, e, 0);
            Scm_VMPushExceptionHandler(DEFAULT_EXCEPTION_HANDLER);
            vm->escapePoint = ep;
            return result;
        }

        /* restore return values */
        vm->val0 = result;
        vm->numVals = numVals;
        if (numVals > 1) {
            for (int i=0; i<numVals-1; i++) vm->vals[i] = rvals[i];
        }

        /* Install the continuation */
        vm->cont = ep->cont;
        vm->denv = ep->denv;
        if (ep->errorReporting) {
            SCM_VM_RUNTIME_FLAG_SET(vm, SCM_ERROR_BEING_REPORTED);
        }
        /* restore reset-chain for reset/shift */
        if (ep->cstack) vm->resetChain = ep->resetChain;
    } else {
        /* We don't have an active error handler, so this is the fallback
           behavior.  Reports the error and rewind dynamic handlers and
           C stacks. */
        call_error_reporter(e);
        /* unwind the dynamic handlers */
        ScmObj hp;
        SCM_FOR_EACH(hp, get_dynamic_handlers(vm)) {
            ScmObj handler_entry = SCM_CAR(hp);
            set_dynamic_handlers(vm, SCM_CDR(hp));
            call_after_thunk(vm, handler_entry);
        }
    }

    SCM_ASSERT(vm->cstack);
    vm->escapeReason = SCM_VM_ESCAPE_ERROR;
    vm->escapeData[0] = ep;
    vm->escapeData[1] = e;
    siglongjmp(vm->cstack->jbuf, 1);
}

/* Call error reporter - either the custome one, or the default
   Scm_ReportError.  We set SCM_ERROR_BEING_REPORTED flag during it
   to prevent infinite loop. */
static void call_error_reporter(ScmObj e)
{
    ScmVM *vm = theVM;

    if (SCM_VM_RUNTIME_FLAG_IS_SET(vm, SCM_ERROR_BEING_REPORTED)) {
        /* An _uncaptured_ error occurred during reporting an error.
           We can't proceed, for it will cause infinite loop.
           Note that it is OK for an error to occur inside the error
           reporter, as far as the error is handled by user-installed
           handler.   The user-installed handler can even invoke a
           continuation that is captured outside; the flag is reset
           in such case.
           Be careful that it is possible that stderr is no longer
           available here (since it may be the very cause of the
           recursive error).  All we can do is to abort. */
        Scm_Abort("Unhandled error occurred during reporting an error.  Process aborted.\n");
    }

    SCM_VM_RUNTIME_FLAG_SET(vm, SCM_ERROR_BEING_REPORTED);
    SCM_UNWIND_PROTECT {
        if (SCM_PROCEDUREP(vm->customErrorReporter)) {
            Scm_ApplyRec(vm->customErrorReporter, SCM_LIST1(e));
        } else {
            Scm_ReportError(e, SCM_OBJ(SCM_CURERR));
        }
    }
    SCM_WHEN_ERROR {
        /* NB: this is called when a continuation captured outside is
           invoked inside the error reporter.   It may be invoked by
           the user's error handler.  */
        SCM_VM_RUNTIME_FLAG_CLEAR(vm, SCM_ERROR_BEING_REPORTED);
    }
    SCM_END_PROTECT;
    SCM_VM_RUNTIME_FLAG_CLEAR(vm, SCM_ERROR_BEING_REPORTED);
}

static ScmObj default_exception_handler_body(ScmObj *argv,
                                             int argc,
                                             void *data SCM_UNUSED)
{
    SCM_ASSERT(argc == 1);
    return Scm_VMDefaultExceptionHandler(argv[0]);
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
 *  This function can be called from Scheme function raise,
 *  or C-function Scm_Error families and signal handler.
 *  So there may be a raw C code in the continuation of this C call.
 *  Thus we can't use Scm_VMApply to call the user-defined exception
 *  handler.
 *  Note that this function may return.
 */
ScmObj Scm_VMThrowException(ScmVM *vm, ScmObj exception, u_long raise_flags)
{
    SCM_VM_RUNTIME_FLAG_CLEAR(vm, SCM_ERROR_BEING_HANDLED);

    /* SRFI-34/R7RS semantics - we invoke exception handler while the handler
       chain is popped. */
    ScmObj eh = Scm_VMPopExceptionHandler();

    if (eh != DEFAULT_EXCEPTION_HANDLER) {
        vm->val0 = Scm_ApplyRec(eh, SCM_LIST1(exception));
        if (SCM_SERIOUS_CONDITION_P(exception)
            || raise_flags&SCM_RAISE_NON_CONTINUABLE) {
            /* the user-installed exception handler returned while it
               shouldn't.  In order to prevent infinite loop, we should
               pop the erroneous handler.  For now, we just reset
               the current exception handler. */
            Scm_VMPushExceptionHandler(DEFAULT_EXCEPTION_HANDLER);
            Scm_Error("user-defined exception handler returned on non-continuable exception %S", exception);
        }
        /* Continuable exception. Recover exception handler settings. */
        Scm_VMPushExceptionHandler(eh);
        return vm->val0;
    }
    return Scm_VMDefaultExceptionHandler(exception);
}

/*
 * with-error-handler
 */
static ScmObj install_ehandler(ScmObj *args SCM_UNUSED,
                               int nargs SCM_UNUSED,
                               void *data)
{
    ScmEscapePoint *ep = (ScmEscapePoint*)data;
    ScmVM *vm = theVM;
    vm->escapePoint = ep;
    SCM_VM_RUNTIME_FLAG_CLEAR(vm, SCM_ERROR_BEING_REPORTED);
    return SCM_UNDEFINED;
}

static ScmObj discard_ehandler(ScmObj *args SCM_UNUSED,
                               int nargs SCM_UNUSED,
                               void *data)
{
    ScmEscapePoint *ep = (ScmEscapePoint *)data;
    ScmVM *vm = theVM;
    vm->escapePoint = ep->prev;
    if (ep->errorReporting) {
        SCM_VM_RUNTIME_FLAG_SET(vm, SCM_ERROR_BEING_REPORTED);
    }
    return SCM_UNDEFINED;
}

static ScmObj with_error_handler(ScmVM *vm, ScmObj handler,
                                 ScmObj thunk, int rewindBefore)
{
    ScmEscapePoint *ep = new_ep(vm, handler, rewindBefore,
                                SCM_FALSE, SCM_FALSE);
    vm->escapePoint = ep; /* This will be done in install_ehandler, but
                             make sure ep is visible from save_cont
                             to redirect ep->cont */

    /* We use default exception handler that does the main work.
       We no longer need to rely on before/after handlers to swap exception
       handlers, for it is handled automatically with vm->denv. */
    Scm_VMPushExceptionHandler(DEFAULT_EXCEPTION_HANDLER);

    ScmObj before = Scm_MakeSubr(install_ehandler, ep, 0, 0, SCM_FALSE);
    ScmObj after  = Scm_MakeSubr(discard_ehandler, ep, 0, 0, SCM_FALSE);
    return Scm_VMDynamicWind(before, thunk, after);
}

ScmObj Scm_VMWithErrorHandler(ScmObj handler, ScmObj thunk)
{
    return with_error_handler(theVM, handler, thunk, FALSE);
}

ScmObj Scm_VMWithGuardHandler(ScmObj handler, ScmObj thunk)
{
    return with_error_handler(theVM, handler, thunk, TRUE);
}

ScmObj Scm_VMReraise()
{
    theVM->errorHandlerContinuable = TRUE;
    return SCM_UNDEFINED;
}

/*
 * Returns internal denv keys (internal API)
 *   These keys are uninterned symbols so that they wouldn't conflict
 *   with user provided keys.
 */
ScmObj Scm__GetDenvKey(ScmDenvKeyName name)
{
    switch (name) {
    case SCM_DENV_KEY_EXCEPTION_HANDLER:
        return denv_key_exception_handler;
    case SCM_DENV_KEY_DYNAMIC_HANDLER:
        return denv_key_dynamic_handler;
    case SCM_DENV_KEY_PARAMETERIZATION:
        return denv_key_parameterization;
    case SCM_DENV_KEY_EXPRESSION_NAME:
        return denv_key_expression_name;
    }
    return SCM_UNDEFINED;       /* dummy */
}

/*
 * Exception handlers
 *
 *   This primitive gives the programmer whole responsibility of
 *   dealing with exceptions.
 *
 *   Exception handler chain is kept in DENV, with the key
 *   denv_key_exception_handler.  Its value is a pair,
 *   (<handler> . <prev-entry>) where <prev-entry> points to the
 *   previous entry or ().
 */

ScmObj Scm_VMCurrentExceptionHandler()
{
    ScmObj entry = Scm_VMFindDynamicEnv(denv_key_exception_handler, SCM_NIL);
    if (SCM_PAIRP(entry)) return SCM_CAR(entry);
    else return DEFAULT_EXCEPTION_HANDLER;
}

ScmObj Scm_VMExceptionHandlerStack()
{
    ScmObj entry = Scm_VMFindDynamicEnv(denv_key_exception_handler, SCM_NIL);
    return Scm_CopyList(entry);
}

void Scm_VMPushExceptionHandler(ScmObj handler)
{
    ScmObj prev = Scm_VMFindDynamicEnv(denv_key_exception_handler, SCM_NIL);
    Scm_VMPushDynamicEnv(denv_key_exception_handler,
                         Scm_Cons(handler, prev));
}

/* Set the current exception handler to the 'previous' one, returning
   the current handler.
   Note that this does not actually 'pop' the DENV; instead, we push the
   previous entry in DENV.
*/
ScmObj Scm_VMPopExceptionHandler()
{
    ScmObj current = Scm_VMFindDynamicEnv(denv_key_exception_handler, SCM_NIL);
    if (SCM_PAIRP(current)) {
        ScmObj h = SCM_CAR(current);
        Scm_VMPushDynamicEnv(denv_key_exception_handler, SCM_CDR(current));
        return h;
    } else {
        return DEFAULT_EXCEPTION_HANDLER;
    }
}

ScmObj Scm_VMWithExceptionHandler(ScmObj handler, ScmObj thunk)
{
    Scm_VMPushExceptionHandler(handler);
    return Scm_VMApply0(thunk);
}

/*==============================================================
 * Continuation prompt tag
 */

#define DEFAULT_PROMPT_TAG_NAME "default-prompt-tag"
static ScmString defaultPromptTagName =
    SCM_STRING_CONST_INITIALIZER(DEFAULT_PROMPT_TAG_NAME,
                                 sizeof(DEFAULT_PROMPT_TAG_NAME)-1,
                                 sizeof(DEFAULT_PROMPT_TAG_NAME)-1);

static void init_prompt_tag(ScmPromptTag *tag, ScmObj name)
{
    SCM_SET_CLASS(tag, SCM_CLASS_PROMPT_TAG);
    tag->name = name;
    tag->insn = SCM_VM_INSN(SCM_VM_RET);
}

ScmObj Scm_MakePromptTag(ScmObj name)
{
    ScmPromptTag *pt = SCM_NEW(ScmPromptTag);
    init_prompt_tag(pt, name);
    return SCM_OBJ(pt);
}

ScmObj Scm_DefaultPromptTag()
{
    return SCM_OBJ(&defaultPromptTag);
}

/*==============================================================
 * Continuation prompts
 */

ScmObj Scm_VMCallWithContinuationPrompt(ScmObj thunk,
                                        ScmObj promptTag,
                                        ScmObj abortHandler)
{
    if (SCM_FALSEP(promptTag)) promptTag = Scm_DefaultPromptTag();
    else if (!SCM_PROMPT_TAG_P(promptTag)) {
        SCM_TYPE_ERROR(promptTag, "prompt tag or #f");
    }

    push_prompt_cont(Scm_VM(), promptTag, abortHandler);
    return Scm_VMApply0(thunk);
}

static ScmContFrame *find_prompt_frame(ScmVM *vm, ScmObj promptTag)
{
    if (!SCM_PROMPT_TAG(promptTag)) promptTag = SCM_OBJ(&defaultPromptTag);
    ScmWord *pc = SCM_PROMPT_TAG_PC(promptTag);
    for (ScmContFrame *c = vm->cont; c; c = c->prev) {
        if (c->pc == pc) {
            return c;
         }
     }
     return NULL;
}


static ScmObj vm_abort_cc(ScmObj val0, void *data[]);

static ScmObj call_abort_handler(ScmObj abortHandler, ScmObj args)
{
    if (SCM_FALSEP(abortHandler)) {
        if (!(Scm_Length(args) == 1
              && SCM_PROCEDUREP(SCM_CAR(args))
              && SCM_PROCEDURE_REQUIRED(SCM_CAR(args)) == 0)) {
            Scm_Error("default abort-handler requires exactly one argument, "
                      "which must be a thunk, but got: %S", args);
        }
        return Scm_VMApply0(SCM_CAR(args));
    } else {
        /* TODO: In this case, we should run abortHandler _after_ the
           abortTo frame is popped.  We need some more tweaks to realize it.
        */
        return Scm_VMApply(abortHandler, args);
    }
}

static ScmObj vm_abort_body(ScmContFrame *abortTo, ScmObj args)
{
    ScmPromptData *pd = (ScmPromptData*)abortTo->cpc;
    ScmObj abortHandler = pd->abortHandler;
    return call_abort_handler(abortHandler, args);
}

static ScmObj vm_abort_cc(ScmObj val0 SCM_UNUSED, void *data[])
{
    ScmContFrame *abortTo = data[0];
    ScmPromptData *pd = (ScmPromptData*)abortTo->cpc;
    ScmObj targetHandlers = pd->dynamicHandlers;
    ScmVM *vm = theVM;
    ScmObj currentHandlers = get_dynamic_handlers(vm);
    if (targetHandlers != currentHandlers && SCM_PAIRP(currentHandlers)) {
        Scm_VMPushCC(vm_abort_cc, data, 2);
        ScmObj handler_entry = pop_dynamic_handlers(vm);
        return vm_call_after_thunk(vm, handler_entry);
    } else {
        ScmObj args = SCM_OBJ(data[1]);
        return vm_abort_body(abortTo, args);
    }
}


ScmObj Scm_VMAbortCurrentContinuation(ScmObj promptTag, ScmObj args)
{
    if (!(SCM_PROMPT_TAG_P(promptTag) || SCM_FALSEP(promptTag))) {
        SCM_TYPE_ERROR(promptTag, "prompt tag or #f");
    }
    ScmVM *vm = theVM;
    ScmContFrame *abortTo = find_prompt_frame(vm, promptTag);
    if (abortTo == NULL) {
        Scm_RaiseCondition(SCM_OBJ(SCM_CLASS_CONTINUATION_ERROR),
                           "prompt-tag", promptTag,
                           SCM_RAISE_CONDITION_MESSAGE,
                           "Attempt to abort to stale prompt tag: %S",
                           promptTag);
    }

    ScmPromptData *pd = (ScmPromptData*)abortTo->cpc;
    SCM_ASSERT(pd->dummy == SCM_VM_INSN(SCM_VM_RET));
    ScmObj targetHandlers = pd->dynamicHandlers;

    /* Discard continuation up to abortTo frame. */
    CONT = abortTo;

    ScmObj currentHandlers = get_dynamic_handlers(vm);
    if (targetHandlers != currentHandlers && SCM_PAIRP(currentHandlers)) {
        ENSURE_SAVE_CONT(abortTo);

        void *data[2];
        data[0] = abortTo;
        data[1] = args;
        Scm_VMPushCC(vm_abort_cc, data, 2);
        ScmObj handler_entry = pop_dynamic_handlers(vm);
        return vm_call_after_thunk(vm, handler_entry);
    } else {
        return vm_abort_body(abortTo, args);
    }
}

/*
 * Continuation marks
 */

SCM_DEFINE_BUILTIN_CLASS(Scm_ContinuationMarkSetClass,
                         NULL, NULL, NULL, NULL,
                         SCM_CLASS_OBJECT_CPL);

static ScmObj make_continuation_mark_set(ScmVM *vm,
                                         ScmContFrame *cont,
                                         ScmObj denv,
                                         ScmObj promptTag)
{
    if (!SCM_PROMPT_TAG_P(promptTag)) promptTag = SCM_OBJ(&defaultPromptTag);

    ScmContFrame *bottom = find_prompt_frame(vm, promptTag);
    if (bottom == NULL) {
        Scm_RaiseCondition(SCM_OBJ(SCM_CLASS_CONTINUATION_ERROR),
                           "prompt-tag", promptTag,
                           SCM_RAISE_CONDITION_MESSAGE,
                           "Stale prompt tag: %S",
                           promptTag);
    }

    ScmContinuationMarkSet *cm = SCM_NEW(ScmContinuationMarkSet);
    SCM_SET_CLASS(cm, SCM_CLASS_CONTINUATION_MARK_SET);
    cm->cont = cont;
    cm->denv = denv;
    cm->bottomDenv = bottom->denv;
    return SCM_OBJ(cm);
}

ScmObj Scm_ContinuationMarks(ScmObj contProc, ScmObj promptTag)
{
    if (!Scm_ContinuationP(contProc)) {
        SCM_TYPE_ERROR(contProc, "continuation");
    }
    ScmEscapePoint *ep = (ScmEscapePoint*)SCM_SUBR(contProc)->data;
    ScmContFrame *cont = ep->cont;
    return make_continuation_mark_set(theVM, cont, ep->denv, promptTag);
}


ScmObj Scm_CurrentContinuationMarks(ScmObj promptTag)
{
    ScmVM *vm = theVM;
    save_cont(vm);
    return make_continuation_mark_set(vm, CONT, DENV, promptTag);
}

ScmObj Scm_ContinuationMarkSetToList(const ScmContinuationMarkSet *cmset,
                                     ScmObj key, ScmObj promptTag)
{
    if (!SCM_PROMPT_TAG_P(promptTag)) promptTag = SCM_OBJ(&defaultPromptTag);
    if (cmset == NULL) {
        cmset = SCM_CONTINUATION_MARK_SET(Scm_CurrentContinuationMarks(promptTag));
    }

    ScmObj h = SCM_NIL, t = SCM_NIL;
    ScmContFrame *c = cmset->cont;
    ScmObj p = cmset->denv;

    while (c && c->denv == p) {
        /* no new marks in the current frame */
        c = c->prev;
    }
    while (SCM_PAIRP(p)) {
        if (p == cmset->bottomDenv) break;
        /* TODO: Bail out if we hit promptTag */
        if (SCM_CAAR(p) == key) {
            SCM_APPEND1(h, t, SCM_CDAR(p));
            /* skip to the next continuation frame */
            if (c == NULL) break;
            for (p = SCM_CDR(p); SCM_PAIRP(p); p = SCM_CDR(p)) {
                if (c->denv == p) {
                    do {
                        c = c->prev;
                    } while (c && c->denv == p);
                    break;
                }
            }
            continue;
        } else {
            p = SCM_CDR(p);
        }
    }
    return h;
}

/*==============================================================
 * Call With Current Continuation
 */

/* Figure out which before and after thunk should be called.

   In general, handler chains consist a tree.  For example,
   we capture a continuatoin at the chain F, then we the chain
   was popped to C, and we create other chains.  Now we want to
   invoke the captured continuation.  We have a handler tree like this:


           current -> I -- H -- G
                                 \
                                  C -- B -- A
                                 /
            target -> F -- E -- D


   Here, target is the captured continuation's chain, and current is
   the current head of the chain.

   We have to call the following handlers in this order:

           I's after handler
           H's after handler
           G's after handler
           D's before handler
           E's before handler
           F's before handler

   Returns a <handler-differential-list>, or <hdlist>, which is a list of
   (flag . <handler-chain>)
   The flag indicates which of 'before' or 'after' handler should be called,
   and also how the handler chain shoud be updated.

   If the flag is #f, the handler chain is set with CDR of
   <handler-chain>, then the AFTER handler of the CAR of <handler-chain>
   is called.

   If the flag is #t, the BEFORE handler of the CAR of <handler-chain>
   is called, then the vm's handler chain is set to <handler-chain>.
*/
static ScmObj throw_cont_calculate_handlers(ScmObj target, ScmObj current)
{
    ScmObj h = SCM_NIL, t = SCM_NIL, p;
    ScmObj h2 = SCM_NIL;

    /* shortcut */
    if (target == current) return SCM_NIL;

    SCM_FOR_EACH(p, current) {
        SCM_ASSERT(SCM_DYNAMIC_HANDLER_P(SCM_CAR(p)));
        if (!SCM_FALSEP(Scm_Memq(SCM_CAR(p), target))) break;
        /* push 'after' handlers to be called */
        SCM_APPEND1(h, t, Scm_Cons(SCM_FALSE, p));
    }
    SCM_FOR_EACH(p, target) {
        SCM_ASSERT(SCM_DYNAMIC_HANDLER_P(SCM_CAR(p)));
        if (!SCM_FALSEP(Scm_Memq(SCM_CAR(p), current))) break;
        /* push 'before' handlers to be called */
        h2 = Scm_Cons(Scm_Cons(SCM_TRUE, p), h2);
    }
    SCM_APPEND(h, t, h2);
    return h;
}

static void call_dynamic_handlers(ScmVM *vm, ScmObj target, ScmObj current)
{
    ScmObj hdlist = throw_cont_calculate_handlers(target, current);
    ScmObj p;
    SCM_FOR_EACH(p, hdlist) {
        ScmObj before_flag   = SCM_CAAR(p);
        ScmObj chain         = SCM_CDAR(p);
        ScmObj handler_entry = SCM_CAR(chain);
        if (SCM_FALSEP(before_flag)) {
            set_dynamic_handlers(vm, SCM_CDR(chain));
            call_after_thunk(vm, handler_entry);
        } else {
            call_before_thunk(vm, handler_entry);
            set_dynamic_handlers(vm, chain);
        }
    }
}

/* This is to execute pending dynamic handlers upon exit.
   NB: we ignore errors here.  It may not be accurate behavior, though.
   (A handler may intentionally raise an error to skip the rest of
   handlers).  We may change to break from SCM_FOR_EACH in case if
   we detect error in Scm_Apply. */
void Scm_VMFlushDynamicHandlers()
{
    ScmVM *vm = theVM;
    ScmObj hp;
    SCM_FOR_EACH(hp, get_dynamic_handlers(vm)) {
        ScmObj handler_entry = SCM_CAR(hp);
        set_dynamic_handlers(vm, SCM_CDR(hp));
        vm->denv = SCM_DYNAMIC_HANDLER(handler_entry)->denv;
        ScmObj proc = SCM_DYNAMIC_HANDLER(handler_entry)->after;
        ScmObj args = SCM_DYNAMIC_HANDLER(handler_entry)->args;
        Scm_Apply(proc, args, NULL);
    }
}


static ScmObj throw_cont_cc(ScmObj, void **);

static ScmObj reset_proc2(ScmObj result, void **data SCM_UNUSED);

static ScmObj throw_cont_body(ScmObj hdlist,      /*((flag . handler-chain)...)*/
                              ScmEscapePoint *ep, /* target continuation */
                              ScmObj args)        /* args to pass to the
                                                     target continuation */
{
    void *data[4];
    int nargs;
    ScmObj ap;
    ScmVM *vm = theVM;

    /*
     * first, check to see if we need to evaluate dynamic handlers.
     */
    if (SCM_PAIRP(hdlist)) {
        SCM_ASSERT(SCM_PAIRP(SCM_CAR(hdlist)));
        ScmObj before_flag = SCM_CAAR(hdlist);
        ScmObj chain       = SCM_CDAR(hdlist);

        data[0] = (void*)SCM_CDR(hdlist);
        data[1] = (void*)ep;
        data[2] = (void*)args;
        data[3] = (void*)(SCM_FALSEP(before_flag)? NULL : chain);
        Scm_VMPushCC(throw_cont_cc, data, 4);
        if (SCM_FALSEP(before_flag)) {
            /* after handler */
            set_dynamic_handlers(vm, SCM_CDR(chain));
            return vm_call_after_thunk(vm, SCM_CAR(chain));
        } else {
            /* before handler.  chain is restored in throw_cont_cc. */
            return vm_call_before_thunk(vm, SCM_CAR(chain));
        }
    }

    /*
     * If the target continuation is a full continuation, we can abandon
     * the current continuation.  However, if the target continuation is
     * partial, we must return to the current continuation after executing
     * the partial continuation.  The returning part is handled by
     * user_level_inner, but we have to make sure that our current continuation
     * won't be overwritten by execution of the partial continuation.
     *
     * NB: As an exception case, if we'll jump into reset,
     * we might reach to the end of partial continuation even though
     * the target continuation is a full continuation.
     */
    if (ep->cstack == NULL || SCM_PAIRP(ep->resetChain)) {
        save_cont(vm);
    }

    /*
     * now, install the target continuation
     */
    vm->pc = PC_TO_RETURN;
    vm->cont = ep->cont;
    vm->denv = ep->denv;

    /* restore reset-chain for reset/shift */
    if (ep->cstack) vm->resetChain = ep->resetChain;

    /* Fix memory leak of the empty partial continuation. */
    if (ep->cstack == NULL &&
        (ep->cont && (ep->cont->cpc == (SCM_PCTYPE)reset_proc2))) {
        ep->cont = NULL;
    }

    nargs = Scm_Length(args);
    if (nargs == 1) {
        vm->numVals = 1;
        return SCM_CAR(args);
    } else if (nargs < 1) {
        vm->numVals = 0;
        return SCM_UNDEFINED;
    } else if (nargs >= SCM_VM_MAX_VALUES) {
        Scm_Error("too many values passed to the continuation");
    }

    ap = SCM_CDR(args);
    for (int i=0; SCM_PAIRP(ap); i++, ap=SCM_CDR(ap)) {
        vm->vals[i] = SCM_CAR(ap);
    }
    vm->numVals = nargs;
    return SCM_CAR(args);
}

static ScmObj throw_cont_cc(ScmObj result SCM_UNUSED, void **data)
{
    ScmVM *vm = theVM;

    ScmObj hdlist = SCM_OBJ(data[0]);
    ScmEscapePoint *ep = (ScmEscapePoint *)data[1];
    ScmObj args = SCM_OBJ(data[2]);
    if (data[3]) {
        /* restore chain only after 'before' thunk. */
        ScmObj chain = SCM_OBJ(data[3]);
        set_dynamic_handlers(vm, chain);
    }
    return throw_cont_body(hdlist, ep, args);
}

/* Body of the continuation SUBR */
static ScmObj throw_continuation(ScmObj *argframe,
                                 int nargs SCM_UNUSED, void *data)
{
    ScmEscapePoint *ep = (ScmEscapePoint*)data;
    ScmObj args = argframe[0];
    ScmVM *vm = theVM;

    /* First, check to see if we need to rewind C stack.
       NB: If we are invoking a partial continuation (ep->cstack == NULL),
       we execute it on the current cstack. */
    if (ep->cstack && vm->cstack != ep->cstack) {
        ScmCStack *cs;
        for (cs = vm->cstack; cs; cs = cs->prev) {
            if (ep->cstack == cs) break;
        }

        /* If the continuation captured below the current C stack, we rewind
           to the captured stack first. */
        if (cs != NULL) {
            vm->escapeReason = SCM_VM_ESCAPE_CONT;
            vm->escapeData[0] = ep;
            vm->escapeData[1] = args;
            siglongjmp(vm->cstack->jbuf, 1);
        }
        /* If we're here, the continuation is 'ghost'---it was captured on
           a C stack that no longer exists, or that was in another thread.
           We'll execute the Scheme part of such a ghost continuation
           on the current C stack.  User_eval_inner will catch if we
           ever try to return to the stale C frame.

           Note that current vm->cstack chain may point to a continuation
           frame in vm stack, so we need to save the continuation chain
           first, since vm->cstack may be popped when other continuation
           is invoked during the execution of the target one.  (We may be
           able to save some memory access by checking vm->cstack chain to see
           if we really have such a frame, but just calling save_cont is
           easier and always safe.)
        */
        save_cont(vm);
    }

    /* check reset-chain to avoid the wrong return from partial
       continuation */
    if (ep->cstack == NULL && !SCM_PAIRP(ep->resetChain)) {
        Scm_Error("reset missing.");
    }

    ScmObj hdlist;
    ScmObj currentHandlers = get_dynamic_handlers(vm);
    if (ep->cstack) {
        /* for full continuation */
        hdlist = throw_cont_calculate_handlers(ep->dynamicHandlers,
                                               currentHandlers);
    } else {
        /* for partial continuation */
        hdlist
            = throw_cont_calculate_handlers(Scm_Append2(ep->partHandlers,
                                                        currentHandlers),
                                            currentHandlers);
    }
    return throw_cont_body(hdlist, ep, args);
}

ScmObj Scm_VMCallCC(ScmObj proc)
{
    ScmVM *vm = theVM;

    save_cont(vm);

    ScmEscapePoint *ep = new_ep(vm, SCM_FALSE, FALSE, SCM_FALSE, SCM_FALSE);
    ep->prev = NULL;
    ScmObj contproc = Scm_MakeSubr(throw_continuation, ep, 0, 1,
                                   continuation_symbol);
    return Scm_VMApply1(proc, contproc);
}

int Scm_ContinuationP(ScmObj proc)
{
    return (SCM_SUBRP(proc) && SCM_PROCEDURE_INFO(proc) == continuation_symbol);
}

/* call with partial continuation.  this corresponds to the 'shift' operator
   in shift/reset controls (Gasbichler&Sperber, "Final Shift for Call/cc",
   ICFP02.)   Note that we treat the boundary frame as the bottom of
   partial continuation. */
ScmObj Scm_VMCallPC(ScmObj proc)
{
    ScmVM *vm = theVM;

    /* save the continuation.  we only need to save the portion above the
       latest boundary frame (+environmentns pointed from them), but for now,
       we save everything to make things easier.  If we want to squeeze
       performance we'll optimize it later. */
    save_cont(vm);

    /* find the latest boundary frame */
    ScmContFrame *c, *cp;
    for (c = vm->cont, cp = NULL;
         c && !BOUNDARY_FRAME_P(c);
         cp = c, c = c->prev)
        /*empty*/;

    /* set the end marker of partial continuation */
    if (cp && !MARKER_FRAME_P(cp)) {
        cp->marker |= SCM_CONT_SHIFT_MARKER;
        /* also set the delimited flag in reset information */
        if (SCM_PAIRP(vm->resetChain)) {
            SCM_SET_CAR_UNCHECKED(SCM_CAR(vm->resetChain), SCM_TRUE);
        }
    }

    /* We need some special setup of EscapePoint for partial continaution.
       Hoping these tricks won't be necessary once we ovehauled partcont
       handling. */
    ScmEscapePoint *ep = new_ep(vm, SCM_FALSE, FALSE, SCM_FALSE, SCM_FALSE);
    ep->prev = NULL;
    ep->cont = (cp? vm->cont : NULL);
    ep->denv = c? c->denv : (cp? cp->denv : SCM_NIL);
    ep->dynamicHandlers = SCM_NIL; /* don't use for partial continuation */
    ep->cstack = NULL; /* so that the partial continuation can be run
                          on any cstack state. */
    ep->resetChain = (SCM_PAIRP(vm->resetChain)?
                      Scm_Cons(Scm_Cons(SCM_FALSE, SCM_NIL), SCM_NIL) :
                      SCM_NIL); /* used only to detect reset missing */

    /* get the dynamic handlers chain saved on reset */
    ScmObj reset_handlers = (SCM_PAIRP(vm->resetChain)?
                             SCM_CDAR(vm->resetChain) : SCM_NIL);

    /* cut the dynamic handlers chain from current to reset */
    ScmObj h = SCM_NIL, t = SCM_NIL, p;
    SCM_FOR_EACH(p, get_dynamic_handlers(vm)) {
        if (p == reset_handlers) break;
        SCM_APPEND1(h, t, SCM_CAR(p));
    }
    ep->partHandlers = h;

    /* call dynamic handlers for exiting reset */
    call_dynamic_handlers(vm, reset_handlers, get_dynamic_handlers(vm));

    ScmObj contproc = Scm_MakeSubr(throw_continuation, ep, 0, 1,
                                   continuation_symbol);
    /* Remove the saved continuation chain.
       NB: vm->cont can be NULL if we've been executing a partial continuation.
           It's ok, for a continuation pointed by cstack will be restored
           in user_eval_inner.
       NB: If the delimited flag in reset information is not set,
           we can consider we've been executing a partial continuation. */
    if (cp && (SCM_PAIRP(vm->resetChain) &&
               SCM_FALSEP(SCM_CAAR(vm->resetChain)))) {
        vm->cont = NULL;
    } else {
        vm->cont = c;
    }
    vm->denv = c? c->denv : (cp? cp->denv : SCM_NIL);

    return Scm_VMApply1(proc, contproc);
}

static ScmObj reset_proc2(ScmObj result, void **data SCM_UNUSED)
{
    return result;
}

static ScmObj reset_proc1(ScmObj *args SCM_UNUSED, int nargs SCM_UNUSED, void *data)
{
    ScmObj proc = SCM_OBJ(data);

    /* Add a continuation frame so that the end marker of partial
       continuation can be set. */
    Scm_VMPushCC(reset_proc2, NULL, 0);

    return Scm_VMApply0(proc);
}

ScmObj Scm_VMReset(ScmObj proc)
{
    ScmVM *vm = theVM;

    /* push reset-chain for reset/shift */
    vm->resetChain = Scm_Cons(Scm_Cons(SCM_FALSE, get_dynamic_handlers(vm)),
                              vm->resetChain);

    /* call a procedure */
    ScmObj ret = Scm_ApplyRec(proc, SCM_NIL);

    /* pop reset-chain for reset/shift */
    SCM_ASSERT(SCM_PAIRP(vm->resetChain));
    vm->resetChain = SCM_CDR(vm->resetChain);

    return ret;
}

ScmObj Scm_VMResetWithContFrameWrapper(ScmObj proc)
{
    ScmVM *vm = theVM;

    /* push reset-chain for reset/shift */
    vm->resetChain = Scm_Cons(Scm_Cons(SCM_FALSE, get_dynamic_handlers(vm)),
                              vm->resetChain);

    /* We call a wrapper procedure which adds a continuation frame so that
       the end marker of partial continuation can be set. */
    ScmObj proc1 = Scm_MakeSubr(reset_proc1, proc, 0, 0, SCM_FALSE);
    ScmObj ret = Scm_ApplyRec(proc1, SCM_NIL);

    /* pop reset-chain for reset/shift */
    SCM_ASSERT(SCM_PAIRP(vm->resetChain));
    vm->resetChain = SCM_CDR(vm->resetChain);

    return ret;
}

/*==============================================================
 * Unwind protect API
 */

long Scm_VMUnwindProtect(ScmVM *vm, ScmCStack *cstack)
{
    cstack->prev = vm->cstack;
    cstack->cont = NULL;
    vm->cstack = cstack;
    return sigsetjmp(cstack->jbuf, FALSE);
}

void Scm_VMNextHandler(ScmVM *vm)
{
    if (vm->cstack->prev) {
        vm->cstack = vm->cstack->prev;
        siglongjmp(vm->cstack->jbuf, 1);
    } else {
        Scm_Exit(1);
    }
}

void Scm_VMRewindProtect(ScmVM *vm)
{
    SCM_ASSERT(vm->cstack);
    vm->cstack = vm->cstack->prev;
}

/*==============================================================
 * Values
 */

ScmObj Scm_VMValues(ScmVM *vm, ScmObj args)
{
    if (!SCM_PAIRP(args)) {
        vm->numVals = 0;
        return SCM_UNDEFINED;
    }
    int nvals = 1;
    ScmObj cp;
    SCM_FOR_EACH(cp, SCM_CDR(args)) {
        vm->vals[nvals-1] = SCM_CAR(cp);
        if (nvals++ >= SCM_VM_MAX_VALUES) {
            Scm_Error("too many values: %S", args);
        }
    }
    vm->numVals = nvals;
    return SCM_CAR(args);
}

ScmObj Scm_Values(ScmObj args)
{
    return Scm_VMValues(theVM, args);
}

ScmObj Scm_VMValues2(ScmVM *vm, ScmObj val0, ScmObj val1)
{
    vm->numVals = 2;
    vm->vals[0] = val1;
    return val0;
}

ScmObj Scm_Values2(ScmObj val0, ScmObj val1)
{
    return Scm_VMValues2(theVM, val0, val1);
}

ScmObj Scm_VMValues3(ScmVM *vm, ScmObj val0, ScmObj val1, ScmObj val2)
{
    vm->numVals = 3;
    vm->vals[0] = val1;
    vm->vals[1] = val2;
    return val0;
}

ScmObj Scm_Values3(ScmObj val0, ScmObj val1, ScmObj val2)
{
    return Scm_VMValues3(theVM, val0, val1, val2);
}


ScmObj Scm_VMValues4(ScmVM *vm, ScmObj val0, ScmObj val1,
                     ScmObj val2, ScmObj val3)
{
    vm->numVals = 4;
    vm->vals[0] = val1;
    vm->vals[1] = val2;
    vm->vals[2] = val3;
    return val0;
}

ScmObj Scm_Values4(ScmObj val0, ScmObj val1, ScmObj val2, ScmObj val3)
{
    return Scm_VMValues4(theVM, val0, val1, val2, val3);
}

ScmObj Scm_VMValues5(ScmVM *vm, ScmObj val0, ScmObj val1,
                     ScmObj val2, ScmObj val3, ScmObj val4)
{
    vm->numVals = 5;
    vm->vals[0] = val1;
    vm->vals[1] = val2;
    vm->vals[2] = val3;
    vm->vals[3] = val4;
    return val0;
}

ScmObj Scm_Values5(ScmObj val0, ScmObj val1,
                   ScmObj val2, ScmObj val3, ScmObj val4)
{
    return Scm_VMValues5(theVM, val0, val1, val2, val3, val4);
}

ScmObj Scm_VMValuesFromArray(ScmVM *vm, ScmObj *argv, ScmSmallInt argc)
{
    if (argc == 0) {
        vm->numVals = 0;
        return SCM_UNDEFINED;
    }
    for (ScmSmallInt i=1; i<argc; i++) {
        if (i >= SCM_VM_MAX_VALUES) {
            Scm_Error("too many values (%d)", argc);
        }
        vm->vals[i-1] = argv[i];
    }
    vm->numVals = argc;
    return argv[0];
}

ScmObj Scm_ValuesFromArray(ScmObj *argv, ScmSmallInt argc)
{
    return Scm_VMValuesFromArray(theVM, argv, argc);
}

/*==================================================================
 * Queued handler processing.
 */

/* Signal handlers and finalizers are queued in VM when they
 * are requested, and processed when VM is in consistent state.
 * process_queued_requests() are called near the beginning of
 * VM loop, when the VM checks if there's any queued request.
 *
 * When this procedure is called, VM is in middle of any two
 * VM instructions.  We need to make sure the handlers won't
 * disturb the VM state.
 *
 * Conceptually, this procedure inserts handler invocations before
 * the current continuation.
 */

static ScmObj process_queued_requests_cc(ScmObj result SCM_UNUSED, void **data)
{
    /* restore the saved continuation of normal execution flow of VM */
    ScmVM *vm = theVM;

    vm->numVals = (int)(intptr_t)data[0];
    vm->val0 = data[1];
    if (vm->numVals > 1) {
        ScmObj cp = SCM_OBJ(data[2]);
        for (int i=0; i<vm->numVals-1; i++) {
            vm->vals[i] = SCM_CAR(cp);
            cp = SCM_CDR(cp);
        }
    }
    return vm->val0;
}

static void process_queued_requests(ScmVM *vm)
{
    void *data[3];

    /* preserve the current continuation */
    data[0] = (void*)(intptr_t)vm->numVals;
    data[1] = vm->val0;
    if (vm->numVals > 1) {
        ScmObj h = SCM_NIL, t = SCM_NIL;

        for (int i=0; i<vm->numVals-1; i++) {
            SCM_APPEND1(h, t, vm->vals[i]);
        }
        data[2] = h;
    } else {
        data[2] = NULL;
    }
    Scm_VMPushCC(process_queued_requests_cc, data, 3);

    /* NB: it is safe to turn off attentionRequest here; if attentionRequest
       is turned on again after this and before SigCheck() or FinalizerRun(),
       the new request is processed within these procedures; we'll enter
       process_queued_requests() again without anything to process, but
       that's an acceptable overhead. */
    vm->attentionRequest = FALSE;

    /* Process queued stuff.  Currently they call VM recursively,
       but we'd better to arrange them to be processed in the same
       VM level. */
    if (vm->signalPending)   Scm_SigCheck(vm);
    if (vm->finalizerPending) Scm_VMFinalizerRun(vm);

    /* VM STOP is required from other thread.
       See Scm_ThreadStop() in ext/threads/threads.c */
    if (vm->stopRequest) {
        SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(vm->vmlock);
        switch (vm->stopRequest) {
        case 0:
            /* stopRequest is canceled between the last check and
               LOCK_BEGIN. We do nothing. */
            break;
        case SCM_VM_REQUEST_SUSPEND:
            vm->stopRequest = 0;
            vm->state = SCM_VM_STOPPED;
            (void)SCM_INTERNAL_COND_BROADCAST(vm->cond);
            while (vm->state == SCM_VM_STOPPED) {
                /* Here the inspector thread examines VM state */
                (void)SCM_INTERNAL_COND_WAIT(vm->cond, vm->vmlock);
            }
            break;
        case SCM_VM_REQUEST_TERMINATE:
            vm->state = SCM_VM_TERMINATED;
            break;
        default:
            Scm_Panic("Unknown value in vm->stopRequest (%d).  Aborting.",
                      vm->stopRequest);
        }
        SCM_INTERNAL_MUTEX_SAFE_LOCK_END();
        if (vm->state == SCM_VM_TERMINATED) {
            SCM_INTERNAL_THREAD_EXIT(); /* this'll notify vm->cond. */
        }
    }
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
    ScmObj info = Scm_VMGetSourceInfo(vm->base, vm->pc);
    if (!SCM_FALSEP(info)) SCM_APPEND1(stack, tail, info);
    while (c) {
        info = Scm_VMGetSourceInfo(c->base, c->cpc);
        if (!SCM_FALSEP(info)) SCM_APPEND1(stack, tail, info);
        c = c->prev;
    }
    return stack;
}

/*
 * Call trace
 */
void Scm_SetCallTraceSize(u_long size)
{
    vm_call_trace_size = size;
}

ScmCallTrace *Scm__MakeCallTraceQueue(u_long size)
{
    if (size > CALL_TRACE_SIZE_MAX) size = CALL_TRACE_SIZE_MAX;
    else if (size < CALL_TRACE_SIZE_MIN) size = CALL_TRACE_SIZE_MIN;
    else {
        u_long n = 1;
        while (n < size) n <<= 1; /* never overflow as we check the size above */
        size = n;
    }

    ScmCallTrace *ct = SCM_NEW2(ScmCallTrace*,
                                sizeof(ScmCallTrace)
                                + (size-1)*sizeof(ScmCallTraceEntry));
    ct->size = size;
    ct->top = 0;
    for (u_long i=0; i<size; i++) {
        ct->entries[i].base = NULL;
        ct->entries[i].pc = NULL;
    }
    return ct;
}

ScmCallTrace *Scm__CopyCallTraceQueue(ScmCallTrace *master)
{
    u_long size = master->size;
    ScmCallTrace *ct = SCM_NEW2(ScmCallTrace*,
                                sizeof(ScmCallTrace)
                                + (size-1)*sizeof(ScmCallTraceEntry));
    ct->size = size;
    ct->top = master->top;
    for (u_long i=0; i<size; i++) {
        ct->entries[i].base = master->entries[i].base;
        ct->entries[i].pc = master->entries[i].pc;
    }
    return ct;
}

ScmObj Scm_VMGetCallTraceLite(ScmVM *vm)
{
    ScmObj trace = SCM_NIL, tail = SCM_NIL;
    ScmCallTrace *ct = vm->callTrace;

    if (ct) {
        ScmObj info = Scm_VMGetSourceInfo(vm->base, vm->pc);
        if (!SCM_FALSEP(info)) SCM_APPEND1(trace, tail, info);
        for (int i = ct->size - 1; i >= 0; i--) {
            int j = (ct->top + i) % ct->size;
            info = Scm_VMGetSourceInfo(ct->entries[j].base, ct->entries[j].pc);
            if (!SCM_FALSEP(info)) SCM_APPEND1(trace, tail, info);
        }
    }
    return trace;
}

#define DEFAULT_ENV_TABLE_SIZE  64

struct EnvTab {
    struct EnvTabEntry {
        ScmEnvFrame *env;
        ScmObj vec;
    } entries[DEFAULT_ENV_TABLE_SIZE];
    int numEntries;
};

#if 0 /* for now */
static ScmObj env2vec(ScmEnvFrame *env, struct EnvTab *etab)
{
    if (!env) return SCM_FALSE;
    for (int i=0; i<etab->numEntries; i++) {
        if (etab->entries[i].env == env) {
            return etab->entries[i].vec;
        }
    }
    ScmObj vec = Scm_MakeVector((int)env->size+2, SCM_FALSE);
    SCM_VECTOR_ELEMENT(vec, 0) = env2vec(env->up, etab);
    SCM_VECTOR_ELEMENT(vec, 1) = SCM_NIL; /*Scm_VMGetBindInfo(env->info);*/
    for (int i=0; i<env->size; i++) {
        SCM_VECTOR_ELEMENT(vec, i+2) = ENV_DATA(env, (env->size-i-1));
    }
    if (etab->numEntries < DEFAULT_ENV_TABLE_SIZE) {
        etab->entries[etab->numEntries].env = env;
        etab->entries[etab->numEntries].vec = vec;
        etab->numEntries++;
    }
    return vec;
}
#endif

ScmObj Scm_VMGetStack(ScmVM *vm SCM_UNUSED /* temporary*/)
{
#if 0 /* for now */
    ScmContFrame *c = vm->cont;
    ScmObj stack = SCM_NIL, tail = SCM_NIL;
    ScmObj info, evec;
    struct EnvTab envTab;

    envTab.numEntries = 0;
    if (SCM_PAIRP(vm->pc)) {
        info = Scm_VMGetSourceInfo(vm->pc);
        SCM_APPEND1(stack, tail, Scm_Cons(info, env2vec(vm->env, &envTab)));
    }

    for (; c; c = c->prev) {
        if (!SCM_PAIRP(c->info)) continue;
        info = Scm_VMGetSourceInfo(c->info);
        evec = env2vec(c->env, &envTab);
        SCM_APPEND1(stack, tail, Scm_Cons(info, evec));
    }
    return stack;
#endif
    return SCM_NIL;
}

/*
 * Dump VM internal state.
 *
 *  NB: ScmObj can't be NULL, but Scm_VMDump() may be called on a VM in a broken
 *  state, so we have checks for it too.  If an ScmObj value is NULL, it is
 *  displayed as #NULL.
 */

static ScmObj get_debug_info(ScmCompiledCode *base, SCM_PCTYPE pc)
{
    if (base == NULL
        || (pc < base->code || pc >= base->code + base->codeSize)) {
        return SCM_FALSE;
    }

    ScmObj di = Scm_CodeDebugInfo(base);
    int off = (int)(pc - base->code);
    ScmObj ip;
    SCM_FOR_EACH(ip, di) {
        ScmObj p = SCM_CAR(ip);
        if (!SCM_PAIRP(p) || !SCM_INTP(SCM_CAR(p))) continue;
        /* PC points to the next instruction,
           search for info entry right before it. */
        if (SCM_INT_VALUE(SCM_CAR(p)) < off) {
            return SCM_CDR(p);
        }
    }
    return SCM_FALSE;
}

ScmObj Scm_VMGetSourceInfo(ScmCompiledCode *base, SCM_PCTYPE pc)
{
    ScmObj info = get_debug_info(base, pc);
    if (SCM_PAIRP(info)) {
        ScmObj p = Scm_Assq(SCM_SYM_SOURCE_INFO, info);
        if (SCM_PAIRP(p)) return SCM_CDR(p);
    }
    return SCM_FALSE;
}

ScmObj Scm_VMGetBindInfo(ScmCompiledCode *base, SCM_PCTYPE pc)
{
    ScmObj info = get_debug_info(base, pc);
    if (SCM_PAIRP(info)) {
        ScmObj p = Scm_Assq(SCM_SYM_BIND_INFO, info);
        if (SCM_PAIRP(p)) return SCM_CDR(p);
    }
    return SCM_FALSE;
}


/* returns next env */
static ScmEnvFrame *dump_env(ScmVM *vm, ScmEnvFrame *env, ScmPort *out)
{
    if (env && (IN_STACK_P((ScmObj*)env) || GC_base(env))) {
        Scm_Printf(out, "   %p %55.1S\n", env, env->info);
        Scm_Printf(out, "       up=%p size=%d\n", env->up, env->size);
        Scm_Printf(out, "       [");
        for (int i=0; i<env->size; i++) {
            if (ENV_DATA(env, i)) {
                Scm_Printf(out, " %S", ENV_DATA(env, i));
            } else {
                Scm_Printf(out, " #NULL");
            }
        }
        Scm_Printf(out, " ]\n");
        return env->up;
    } else {
        Scm_Printf(out, "   %p #INVALID\n", env);
        return NULL;
    }
}

/* Show offset of given PC w.r.t to the beginning of the code of
   the current base.  Note that PC may not point to the code of the
   base.  */
static void dump_pc_offset(ScmWord *pc, ScmCompiledCode *base, ScmPort *out)
{
    if (base && base->code <= pc && pc < base->code + base->codeSize) {
        Scm_Printf(out, "[%5u(%p)]", (u_long)(pc - base->code), base->code);
    }
}

static void print_insn(ScmObj insn, ScmPort *out)
{
    u_int code = SCM_VM_INSN_CODE(insn);
    if (code >= SCM_VM_NUM_INSNS) {
        Scm_Printf(out, "invalid insn: %08x", SCM_WORD(insn));
    } else {
        switch (Scm_VMInsnNumParams(code)) {
        case 0:
            Scm_Printf(out, "%s", Scm_VMInsnName(code));
            break;
        case 1:
            Scm_Printf(out, "%s(%d)",
                       Scm_VMInsnName(code), SCM_VM_INSN_ARG(insn));
            break;
        case 2:
            Scm_Printf(out, "%s(%d,%d)",
                       Scm_VMInsnName(code),
                       SCM_VM_INSN_ARG0(insn),
                       SCM_VM_INSN_ARG1(insn));
            break;
        }
    }
}

static void print_insn_paren(ScmObj insn, ScmPort *out)
{
    Scm_Printf(out, " (");
    print_insn(insn, out);
    Scm_Printf(out, ")\n");
}

void Scm_VMDump(ScmVM *vm_to_dump)
{
    /* We first take a snapshot of the given VM, so that the act of
       dumping won't interfere with the state of the target.
     */
    ScmVM *vm = Scm_VMTakeSnapshot(vm_to_dump);
    ScmPort *out = SCM_CURERR;
    ScmEnvFrame *env = vm->env;
    ScmContFrame *cont = vm->cont;
    ScmCStack *cstk = vm->cstack;
    ScmEscapePoint *ep = vm->escapePoint;

    Scm_Printf(out, "VM %p -----------------------------------------------------------\n", vm);
    Scm_Printf(out, "   pc: %p  ", vm->pc);
    dump_pc_offset(vm->pc, vm->base, out);
    print_insn_paren(SCM_OBJ(*vm->pc), out);
    Scm_Printf(out, "   sp: %p  [%p-%p-%p]\n", vm->sp,
               vm->stack, vm->stackBase, vm->stackEnd);
    Scm_Printf(out, " argp: %p\n", vm->argp);
    if (vm->val0) {
        Scm_Printf(out, " val0: %#65.1S\n", vm->val0);
    } else {
        Scm_Printf(out, " val0: #NULL\n");
    }

    Scm_Printf(out, " envs:\n");
    while (env) {
        env = dump_env(vm, env, out);
    }

    Scm_Printf(out, "conts:\n");
    while (cont) {
        Scm_Printf(out, "   %p", cont);
        if (BOUNDARY_FRAME_P(cont)) {
            ScmPromptTag *t = SCM_PC_TO_PROMPT_TAG(cont->pc);
            SCM_ASSERT(t->insn == SCM_VM_INSN(SCM_VM_RET));
            Scm_Printf(out, "::%S\n", t);
        } else {
            Scm_Printf(out, "\n");
        }
        Scm_Printf(out, "              env = %p\n", cont->env);
        Scm_Printf(out, "             size = %d\n", cont->size);
        Scm_Printf(out, "             base = %p", cont->base);
        if (cont->base && SCM_COMPILED_CODE_P(cont->base)) {
            Scm_Printf(out, "  %S\n", cont->base->name);
        } else {
            Scm_Printf(out, "\n");
        }
        if (!C_CONTINUATION_P(cont)) {
            Scm_Printf(out, "               pc = %p", cont->pc);
            dump_pc_offset(cont->pc, cont->base, out);
            print_insn_paren(SCM_OBJ(*cont->pc), out);
        } else {
            Scm_Printf(out, "               pc = {cproc %p}\n", cont->pc);
        }
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
        if (ep->ehandler) {
            Scm_Printf(out, "  %p: cont=%p, handler=%#20.1S\n",
                       ep, ep->cont, ep->ehandler);
        } else {
            Scm_Printf(out, "  %p: cont=%p, handler=#NULL\n",
                       ep, ep->cont);
        }
        ep = ep->prev;
    }
    Scm_Printf(out, "dyn_handlers: %S\n", get_dynamic_handlers(vm));

    Scm_Printf(out, "reset-chain-length: %d\n", (int)Scm_Length(vm->resetChain));
    if (vm->base) {
        Scm_Printf(out, "Code:\n");
        Scm_CompiledCodeDump(vm->base);
    }
}

#ifdef USE_CUSTOM_STACK_MARKER
struct GC_ms_entry *vm_stack_mark(GC_word *addr,
                                  struct GC_ms_entry *mark_sp,
                                  struct GC_ms_entry *mark_sp_limit,
                                  GC_word env)
{
    struct GC_ms_entry *e = mark_sp;
    ScmObj *vmsb = ((ScmObj*)addr)+1;
    ScmVM *vm = (ScmVM*)*addr;
    int limit = vm->sp - vm->stackBase + 5;
    void *spb = (void *)vm->stackBase;
    void *sbe = (void *)(vm->stackBase + SCM_VM_STACK_SIZE);
    void *hb = GC_least_plausible_heap_addr;
    void *he = GC_greatest_plausible_heap_addr;

    for (int i=0; i<limit; i++, vmsb++) {
        ScmObj z = *vmsb;
        if ((hb < (void *)z && (void *)z < spb)
            || ((void *)z > sbe && (void *)z < he)) {
            e = GC_mark_and_push((void *)z, e, mark_sp_limit, (void *)addr);
        }
    }
    return e;
}
#endif /*USE_CUSTOM_STACK_MARKER*/


ScmObj Scm__VMInsnAddress(int insncode, _Bool offsetp)
{
    if (insncode < 0 || insncode >= SCM_VM_NUM_INSNS) {
        Scm_Error("Insn code out of range: %d", insncode);
    }
    intptr_t r = vminsn_offsets[insncode];
    if (!offsetp) r += (intptr_t)(void*)run_loop;
    return Scm_IntptrToInteger(r);
}

/*===============================================================
 * Initialization
 */

void Scm__InitVM(void)
{
#ifdef USE_CUSTOM_STACK_MARKER
    vm_stack_free_list = GC_new_free_list();
    vm_stack_mark_proc = GC_new_proc(vm_stack_mark);
    vm_stack_kind = GC_new_kind(vm_stack_free_list,
                                GC_MAKE_PROC(vm_stack_mark_proc, 0),
                                0, 0);
#endif /*USE_CUSTOM_STACK_MARKER*/

    Scm_HashCoreInitSimple(&vm_table, SCM_HASH_EQ, 8, NULL);
    SCM_INTERNAL_MUTEX_INIT(vm_table_mutex);
    SCM_INTERNAL_MUTEX_INIT(vm_id_mutex);

    /* Prepare dynamic environemnt keys
       NB: At this moment, symbols are not initialized.  However,
       we can safely allocate *uninterned* symbols, for it doesn't
       require hashtables. */
#define UNINTERNED(name) Scm_MakeSymbol(SCM_STRING(SCM_MAKE_STR(#name)), FALSE)
    denv_key_exception_handler = UNINTERNED(exception-handler);
    denv_key_dynamic_handler   = UNINTERNED(dynamic-handler);
    denv_key_parameterization  = UNINTERNED(parameterization);
    denv_key_expression_name   = UNINTERNED(expression-name);
    continuation_symbol        = UNINTERNED(continuation);

    /* Initialize statically allocated default prompt tag.
       Again, be aware that most modules are not initialized yet.
    */
    init_prompt_tag(&defaultPromptTag, SCM_OBJ(&defaultPromptTagName));

    /* Create root VM */
    rootVM = Scm_NewVM(NULL, SCM_MAKE_STR_IMMUTABLE("root"));
    rootVM->state = SCM_VM_RUNNABLE;
#ifdef GAUCHE_USE_PTHREADS
    if (pthread_key_create(&vm_key, NULL) != 0) {
        Scm_Panic("pthread_key_create failed.");
    }
    if (pthread_setspecific(vm_key, rootVM) != 0) {
        Scm_Panic("pthread_setspecific failed.");
    }
    rootVM->thread = pthread_self();
#elif  GAUCHE_USE_WTHREADS
    vm_key = TlsAlloc();
    if (vm_key == TLS_OUT_OF_INDEXES) {
        Scm_Panic("TlsAlloc failed");
    }
    if (!TlsSetValue(vm_key, (LPVOID)rootVM)) {
        Scm_Panic("TlsSetValue failed");
    }
    rootVM->thread = GetCurrentThread();
#else   /* no threads */
    theVM = rootVM;
#endif  /* no threads */

#ifdef COUNT_INSN_FREQUENCY
    Scm_AddCleanupHandler(dump_insn_frequency, NULL);
#endif /*COUNT_INSN_FREQUENCY*/

#ifdef COUNT_FLUSH_FPSTACK
    Scm_AddCleanupHandler(print_flush_fpstack_count, NULL);
#endif

    if (Scm_GetEnv("GAUCHE_ALLOW_UNDEFINED_TEST") != NULL) {
        SCM_VM_RUNTIME_FLAG_CLEAR(rootVM, SCM_CHECK_UNDEFINED_TEST);
    } else {
        SCM_VM_RUNTIME_FLAG_SET(rootVM, SCM_CHECK_UNDEFINED_TEST); /* default */
    }
    if (Scm_GetEnv("GAUCHE_LEGACY_DEFINE") != NULL) {
        SCM_VM_COMPILER_FLAG_SET(rootVM, SCM_COMPILE_LEGACY_DEFINE);
    }
    if (Scm_GetEnv("GAUCHE_MUTABLE_LITERALS") != NULL) {
        SCM_VM_COMPILER_FLAG_SET(rootVM, SCM_COMPILE_MUTABLE_LITERALS);
    }
    /* NB: In 0.9.10, we warn srfi-N feature ID only when requested.
       We'll reverse the default in the later releases. */
    SCM_VM_COMPILER_FLAG_SET(rootVM, SCM_COMPILE_SRFI_FEATURE_ID);
    if (Scm_GetEnv("GAUCHE_WARN_SRFI_FEATURE_ID") != NULL) {
        SCM_VM_COMPILER_FLAG_CLEAR(rootVM, SCM_COMPILE_SRFI_FEATURE_ID);
    }
    else if (Scm_GetEnv("GAUCHE_ALLOW_SRFI_FEATURE_ID") != NULL) {
        SCM_VM_COMPILER_FLAG_SET(rootVM, SCM_COMPILE_SRFI_FEATURE_ID);
    }
}
