/*
 * vm.h - Virtual machine
 *
 *   Copyright (c) 2000-2025  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_VM_H
#define GAUCHE_VM_H

/* Size of stack per VM (in words). */
#define SCM_VM_STACK_SIZE      10000

/* Maximum # of values allowed for multiple value return */
#define SCM_VM_MAX_VALUES      20

/* Finalizer queue size */
#define SCM_VM_FINQ_SIZE       32

#define SCM_PCTYPE ScmWord*

#if (defined(ITIMER_PROF) && defined(SIGPROF)) || defined(GAUCHE_WINDOWS)
/* define if you want to use profiler */
#define GAUCHE_PROFILE
#endif /* (defined(ITIMER_PROF) && defined(SIGPROF)) || defined(GAUCHE_WINDOWS) */

#ifdef __GNUC__
/* in GNUC, we inline Scm_VMReturnFlonum using statement expressions. */
# if GAUCHE_FFX
SCM_EXTERN void Scm_VMFlushFPStack(ScmVM *vm);
#   define Scm_VMReturnFlonum(_val)                                     \
        ({ ScmVM *vm__ = Scm_VM(); ScmFlonum *fp;                       \
           if (vm__->fpsp == vm__->fpstackEnd) Scm_VMFlushFPStack(vm__);\
           fp = vm__->fpsp++;                                           \
           SCM_FLONUM_VALUE(fp) = (_val);                               \
           SCM_MAKE_FLONUM_REG(fp);                                     \
        })
# else  /* !GAUCHE_FFX */
#   define Scm_VMReturnFlonum(val)  Scm_MakeFlonum(val)
# endif /* !GAUCHE_FFX */
#else  /* !__GNUC__ */
#   define Scm_VMReturnFlonum(val)  Scm_MakeFlonum(val)
#endif /* !__GNUC__ */

/* Actual structure is defined in code.h */
typedef struct ScmCompiledCodeRec ScmCompiledCode;

/* Actual structure is defined in priv/vmP.h */
typedef struct ScmCallTraceRec ScmCallTrace;

/* Actual structure is defined in priv/vmP.h */
typedef struct ScmPromptTagRec ScmPromptTag;
typedef struct ScmThreadLocalRec ScmThreadLocal;
typedef struct ScmVMThreadLocalTableRec ScmVMThreadLocalTable;

/*
 * Static environment frame
 *
 *   :        :
 *   +--------+
 *   | size=N |
 *   |  info  |
 *   |...up...|<--- ScmEnvFrame* envp
 *   |arg[N-1]|
 *   |arg[N-2]|
 *   :        :
 *   | arg[0] |
 *   +--------+
 *   :        :
 */

typedef struct ScmEnvFrameRec {
    struct ScmEnvFrameRec *up;  /* static link */
    ScmObj info;                /* reserved */
    ScmWord size;               /* size of the frame (excluding header) */
} ScmEnvFrame;

#define ENV_HDR_SIZE   3        /* envframe header size */
#define ENV_SIZE(size)   ((size)+ENV_HDR_SIZE)
#define ENV_FP(env)        (((ScmObj*)(env))-((env)->size))
#define ENV_DATA(env, num) (*(((ScmObj*)(env))-(num)-1))

/*
 * Dynamic environment
 *
 *  Dynamic environment is just an alist pointed from vm->denv.
 *  It is a part of continuation---when a continuation frame is pushed,
 *  the tip of denv is saved with it.  When a continuation frame is
 *  popped, the saved denv is restored.
 *  This means that you don't ever need to worry about popping denv.
 */

SCM_EXTERN void   Scm_VMPushDynamicEnv(ScmObj key, ScmObj val);
SCM_EXTERN ScmObj Scm_VMFindDynamicEnv(ScmObj key, ScmObj fallback);

typedef struct ScmContinuationMarkSetRec ScmContinuationMarkSet;

SCM_CLASS_DECL(Scm_ContinuationMarkSetClass);
#define SCM_CLASS_CONTINUATION_MARK_SET (&Scm_ContinuationMarkSetClass)
#define SCM_CONTINUATION_MARK_SET(obj)  ((ScmContinuationMarkSet*)obj)
#define SCM_CONTINUATION_MARK_SET_P(obj) SCM_ISA(obj,SCM_CLASS_CONTINUATION_MARK_SET)

SCM_EXTERN ScmObj Scm_ContinuationMarks(ScmObj contProc, ScmObj promptTag);
SCM_EXTERN ScmObj Scm_CurrentContinuationMarks(ScmObj promptTag);
SCM_EXTERN ScmObj Scm_ContinuationMarkSetToList(const ScmContinuationMarkSet *,
                                                ScmObj, ScmObj);

/*
 * Continuation frame
 *
 *  Continuation is represented as a chain of ScmContFrames.
 *
 *   :        :
 *   +--------+
 *   |  base  |
 *   |   pc   |
 *   |  cpc   |
 *   | marker |
 *   | size=N |
 *   |  denv  |
 *   |  env   |
 *   |..prev..|<--- ScmContFrame* cont
 *   |arg[N-1]|
 *   |arg[N-2]|
 *   :        :
 *   | arg[0] |
 *   +--------+
 *   :        :
 *
 *  If env is a special value (&ccEnvMark), this is a C continuation.
 *  Some fields are used differently:
 *
 *   |  base   |
 *   |   pc    |  <-- PCont procedure
 *   |  cpc    |  <-- CCont procedure or NULL
 *   | marker  |
 *   | size=N  |
 *   |  denv   |
 *   |  env    |  <-- &ccEnvMark
 *   |..prev.. |
 *   |data[N-1]|
 *   |data[N-2]|
 *   :         :
 *   | data[0] |
 *   +---------+
 *
 *  The difference between CCont and PCont is purely historical.
 *  CCont is the original C continuation.  It takes one result and
 *  the data array.  PCont is introduced to support precompile-to-C
 *  code, and it takes ScmVM* in addition to the result and data array.
 *
 *  When C continuation frame is popped, the value of cpc is temporaily
 *  saved in vm->ccont, and PCont procedure is invoked with vm, val0,
 *  and pointer to the data array.  If the C continuation frame is
 *  pushed with Scm_pc_PushCC, vm->ccont is NULL (unused).
 *  If the C continuation frame is pushed with Scm_VMPushCC, PCont
 *  procedure is set to ccont_adapter() which invokes vm->ccont as
 *  CCont procedure.
 *
 *  A special case is a C continuation frame pushed for execution of
 *  body thunk of dynamic-wind.  It contains #<dynamic-handler>,
 *  so that stack rewinder can find dynamic handlers.
 */

/* NB: The size of continuation frame (in words) is embedded in the precompiled
 * compile.scm in order to calculate maximum stack size in procedure.
 * Precompilation can be done on a different platform from the target,
 * so the size of continuation frame in words must be kept the same across
 * different platforms.  Hence the #if's in size and marker field.
 * We could've done it by using full word for size and marker, but one more
 * word in each continuation frame impacts performance.
 */

typedef struct ScmContFrameRec {
    struct ScmContFrameRec *prev; /* previous frame */
    ScmEnvFrame *env;             /* saved environment */
    ScmObj denv;                  /* dynamic environment links */
#if SIZEOF_LONG == 4
    long size : 30;               /* size of argument frame */
    u_long marker : 2;            /* end marker of partial continuation */
#else
    int size;                     /* size of argument frame */
    int marker;                   /* end marker of partial continuation */
#endif
    SCM_PCTYPE cpc;               /* current PC (for debugging info) */
    SCM_PCTYPE pc;                /* next PC */
    ScmCompiledCode *base;        /* base register value */
} ScmContFrame;

#define CONT_FRAME_SIZE  (sizeof(ScmContFrame)/sizeof(ScmObj))

SCM_EXTERN void Scm_CallCC(ScmObj body);

SCM_EXTERN int  Scm__VMProtectStack(ScmVM *vm);
SCM_EXTERN void Scm__VMUnprotectStack(ScmVM *vm);

/*
 * Thread Locals
 *  See also priv/vmP.h for private definitions
 */

SCM_CLASS_DECL(Scm_ThreadLocalClass);
#define SCM_CLASS_THREAD_LOCAL  (&Scm_ThreadLocalClass)
#define SCM_THREAD_LOCAL(obj)   ((ScmThreadLocal*)obj)
#define SCM_THREAD_LOCAL_P(obj) SCM_ISA(obj,SCM_CLASS_THREAD_LOCAL)

/* Flag value for Scm_MakeThreadLocal */
enum {
    /* Whether the initial value is inherited from the parent thread.
       Thread locals are noninheritable by default, while parameters are
       inheritable. */
    SCM_THREAD_LOCAL_INHERITABLE = (1UL << 0)

};

SCM_EXTERN ScmThreadLocal *Scm_MakeThreadLocal(ScmObj name,
                                               ScmObj initval,
                                               u_long flags);
SCM_EXTERN ScmObj Scm_ThreadLocalRef(ScmVM *vm,
                                     const ScmThreadLocal *tl);
SCM_EXTERN ScmObj Scm_ThreadLocalSet(ScmVM *vm,
                                     const ScmThreadLocal *tl,
                                     ScmObj val);


/*
 * Prompt tags
 */

SCM_CLASS_DECL(Scm_PromptTagClass);
#define SCM_CLASS_PROMPT_TAG   (&Scm_PromptTagClass)
#define SCM_PROMPT_TAG(obj)    ((ScmPromptTag*)obj)
#define SCM_PROMPT_TAG_P(obj)  SCM_XTYPEP(obj, SCM_CLASS_PROMPT_TAG)

SCM_EXTERN ScmObj Scm_MakePromptTag(ScmObj name);
SCM_EXTERN ScmObj Scm_DefaultPromptTag();

typedef struct ScmContinuationPromptRec ScmContinuationPrompt;

/*
 * Identifier
 *
 *   Identifier wraps a symbol with its lexical environment.  This
 *   object is used in hygienic macro expansion (see macro.c), and
 *   also used as a placeholder in a global variable reference/assignment
 *   (see compile.c).
 *
 *   NB: Identifier's API and usage will likely be changed in future.
 *   It shouldn't be used directly from applications.
 */

/* The definition is in gauche/priv/identifierP.h (hidden) */
typedef struct ScmIdentifierRec ScmIdentifier;

SCM_CLASS_DECL(Scm_IdentifierClass);
#define SCM_CLASS_IDENTIFIER    (&Scm_IdentifierClass)

#define SCM_IDENTIFIER(obj)     ((ScmIdentifier*)(obj))
#define SCM_IDENTIFIERP(obj)    SCM_XTYPEP(obj, SCM_CLASS_IDENTIFIER)

/* Create an identifier.
   NAME can be a symbol or an identifier.
   MOD is the toplevel module to close; can be NULL to use the current module.
   ENV is the local local environment (list of frames) to close. */
SCM_EXTERN ScmObj Scm_MakeIdentifier(ScmObj name,
                                     ScmModule *mod,
                                     ScmObj env);
/* Returns the minimal local environment (list of frames) where this
   identifier is bound.  If the identifier isn't bound locally, it would be ().
   The returned value may not be the same as ENV argument passed to the
   constructor; we truncate irrelevant frames. */
SCM_EXTERN ScmObj Scm_IdentifierEnv(ScmIdentifier *id);
SCM_EXTERN ScmObj Scm_WrapIdentifier(ScmIdentifier *id);
SCM_EXTERN int    Scm_IdentifierBindingEqv(ScmIdentifier *id,
                                           ScmSymbol *sym,
                                           ScmObj env);

SCM_EXTERN ScmIdentifier *Scm_OutermostIdentifier(ScmIdentifier *id);
SCM_EXTERN ScmSymbol     *Scm_UnwrapIdentifier(ScmIdentifier *id);
SCM_EXTERN ScmGloc       *Scm_IdentifierGlobalBinding(ScmIdentifier *id);
SCM_EXTERN ScmObj         Scm_IdentifierGlobalRef(ScmIdentifier *id,
                                                  ScmGloc **pgloc);
SCM_EXTERN void           Scm_IdentifierGlobalSet(ScmIdentifier *id,
                                                  ScmObj val,
                                                  ScmGloc **pgloc);

/*
 * Escape handling
 */

/*
 * C stack record
 *
 *  A chain of C stack to rewind.  C stack is captured by Scm_Sigsetjmp
 *  and rewound by Scm_Siglongjmp; see signal.c.
 */
typedef struct ScmCStackRec {
    struct ScmCStackRec *prev;
    ScmContFrame *cont;
    sigjmp_buf jbuf;
    sigset_t mask;
} ScmCStack;

/* ScmEscapePoint definition is in vmP.h */
typedef struct ScmEscapePointRec ScmEscapePoint;

/*
 * Signal queue
 *
 *  Following the Unix tradition, gauche doesn't really queue the signals;
 *  the C-level signal handler only records the fact that a specific
 *  signal has been delivered.  VM loop examines the record and invoke
 *  appropriate Scheme-level signal handlers.  Even if the same signal
 *  arrives more than one time before VM loop checks the record, the handler
 *  is invoked only once.
 *
 *  The C-level signal handler does count each signal until it is cleared
 *  by VM loop.  If one kind of signal arrives more than a certain limit
 *  (set/get by Scm_{Set|Get}SignalPendingLimit), Gauche thinks something
 *  went wrong and bail out.   It is useful, for example, to interrupt
 *  unresponsive program from the terminal.
 */

/* NB: AT least on FreeBSD, RT signals are not counted in NSIG.  We should
   use _SIG_MAXSIG+1 instead to track all signals. */
#if defined(_SIG_MAXSIG) && (_SIG_MAXSIG+1 > NSIG)
#define SCM_NSIG (_SIG_MAXSIG+1)
#else  /*!_SIG_MAXSIG*/
#define SCM_NSIG NSIG
#endif /*!_SIG_MAXSIG*/

typedef struct ScmSignalQueueRec {
    union {
        unsigned char dummy[NSIG];
        unsigned char *sigcounts;
    };
    ScmObj pending;        /* pending signal handlers */
} ScmSignalQueue;

SCM_EXTERN void Scm_SignalQueueInit(ScmSignalQueue* q);
SCM_EXTERN int  Scm_GetSignalPendingLimit(void);
SCM_EXTERN void Scm_SetSignalPendingLimit(int val);

/* NB: Obsoleted macro, but kept for backward compatibility.
   Better API should be provided in future. */
#define SCM_SIGPENDING(vm) \
    ((vm)->queueNotEmpty&SCM_VM_SIGQ_MASK)

#define SCM_SIGCHECK(vm) \
    do { if (vm->signalPending) Scm_SigCheck(vm); } while (0)

SCM_EXTERN void   Scm_SigCheck(ScmVM *vm);

/*
 * Finalizers
 *
 *  Finalizers are queued inside GC.  We disable automatic finalizer
 *  invocation of GC and only set the flag on VM when finalizers are
 *  queued.  VM loop check the flag and calls Scm_VMFinalizerRun()
 *  to run the finalizers.  (If VM is not running, C library must
 *  call it explicitly to run finalizers).
 */

SCM_EXTERN ScmObj Scm_VMFinalizerRun(ScmVM *vm);

/*
 * Statistics
 *
 *  Not much stats are collected yet, but will grow in future.
 *  Stats collections are only active if SCM_COLLECT_VM_STATS
 *  runtime flag is TRUE.
 *  Stats are collected per-VM (i.e. per-thread), but currently
 *  we don't have an API to gather them.
 */

typedef struct ScmVMStatRec {
    /* Stack overflow handler */
    u_long     sovCount; /* # of stack overflow */
    double     sovTime;  /* cumulated time of stack ov handling */

    /* Load statistics chain */
    ScmObj     loadStat;
} ScmVMStat;

/* The profiler structure is defined in prof.h */
typedef struct ScmVMProfilerRec ScmVMProfiler;

/* Some experimental stuff (native.c) */
typedef struct ScmCodeCacheRec ScmCodeCache;

/*
 * VM structure
 *
 *  In Gauche, each thread has a VM.  Indeed, the Scheme object
 *  <thread> is ScmVM in C.
 *
 *  Most fields of VM are private to the thread that owns the VM.
 *  Only the fields marked as "PUBLIC" should be modified by other
 *  thread, and only with obtaining the lock by VMLOCK mutex.
 *  (Note that some fields like "name" and "specific" are not marked
 *  as PUBLIC although they can be referenced or modified by other
 *  thread (via Scheme call thread-specific-set! etc.)   It is the
 *  user program's responsibility to use a mutex.)
 *
 *  When you need inspect other thread's private data (like stack
 *  trace), make the thread is either stopped or terminated, or
 *  you may get inconsistent result.
 */

struct ScmVMRec {
    SCM_HEADER;
    ScmInternalThread thread;   /* the system thread executing this VM. */
    int state;                  /* thread state. PUBLIC. */
    ScmInternalMutex  vmlock;   /* mutex to be used to lock this VM
                                   structure.  PUBLIC. */
    ScmInternalCond cond;       /* the condition variable to wait for state
                                   change of this VM.  PUBLIC. */
    ScmVM *canceller;           /* the thread which called thread-terminate!
                                   on this thread.  PUBLIC. */
    ScmVM *inspector;           /* the thread which requested to stop this
                                   thread.  PUBLIC. */
    ScmObj name;                /* Scheme thread name. */
    ScmObj specific;            /* Scheme thread specific data. */
    ScmProcedure *thunk;        /* Entry point of this VM. */
    ScmObj result;              /* Result of thunk. */
    ScmObj resultException;     /* Exception that causes the thread to terminate.*/
    ScmModule *module;          /* current global namespace.  note that this
                                   is used only in compilation. */
    ScmCStack *cstack;          /* current escape point.  see the comment of
                                   "C stack rewinding" below. */
    u_long runtimeFlags;        /* Runtime flags */
    u_long compilerFlags;       /* Compiler flags */
    intptr_t attentionRequest;  /* Flag if VM needs to check signal queue,
                                   finalizer queue, or stop request.
                                   This flag can be turned on asynchronously.
                                   Only this VM can turn off this flag. */
    intptr_t signalPending;     /* Flag if there are pending signals.
                                   Turned on by sig_handle(), turned off
                                   by Scm_SigCheck(), both in signal.c. */
    intptr_t finalizerPending;  /* Flag if there are pending finalizers.
                                   Turned on by finalizable() callback,
                                   and turned off by Scm_VMFinalizerRun(),
                                   both in core.c */
    intptr_t stopRequest;       /* Flag if there is a pending stop request.
                                   See enum ScmThreadStopRequest below
                                   for the possible values.
                                   Turned on by Scm_ThreadStop() or
                                   Scm_ThreadTerminate in
                                   ext/threads/threads.c, and turned off by
                                   process_queued_requests() in vm.c */

    ScmVMThreadLocalTable *threadLocals; /* thread local table */

    /* Registers */
    ScmCompiledCode *base;      /* Current executing closure's code packet. */
    SCM_PCTYPE pc;              /* Program pointer.  Points into the code
                                   vector. (base->code) */
    ScmEnvFrame *env;           /* Current environment.                      */
    ScmObj denv;                /* Current dynamic environment. */
    ScmContFrame *cont;         /* Current continuation.                     */
    ScmObj *argp;               /* Current argument pointer.  Points
                                   to the incomplete environment frame
                                   being accumulated.  This is a part of
                                   continuation.                             */
    void *ccont;                /* This is to support old C continuation
                                   protocol with the new protool.  It is
                                   set with CCont procedure right before
                                   PCont procedure is called.  See the
                                   comment on Scm_VMPushCC in vm.c.
                                */
    ScmObj val0;                /* Value register.                           */
    ScmObj vals[SCM_VM_MAX_VALUES]; /* Value register for multiple values */
    int    numVals;             /* # of values */
    int    trampoline;          /* Usually -1. C-compiled subr may set >=0
                                   before returning to trampoline to another
                                   subr. */

    int    joinCount;           /* how many times this thread is join!-ed? */

    ScmObj dynamicHandlers;     /* chain of active dynamic handlers          */

    ScmObj *sp;                 /* stack pointer */
    ScmObj *stack;              /* bottom of allocated stack area */
    ScmObj *stackBase;          /* base of current stack area  */
    ScmObj *stackEnd;           /* end of current stack area */
#if GAUCHE_SPLIT_STACK
    /* EXPERIMENTAL: Save the continuation when an error occurs, used for
       better error diagnostics.  Reset by the "cross the border" APIs
       such as Scm_Eval().  This can point into the stack, but must be
       saved when save_cont() is called.
     */
    ScmContFrame *lastErrorCont;
#endif /*GAUCHE_SPLIT_STACK*/

#if GAUCHE_FFX
    ScmFlonum *fpsp;            /* flonum stack pointer.  we call it 'stack'
                                   for historical reasons, but it's more like
                                   a nursery. */
    ScmFlonum *fpstack;         /* flonum stack */
    ScmFlonum *fpstackEnd;      /* flonum stack limit */
#endif

    /* Escape handling */
    ScmObj floatingEscapePoints; /* List of escapePoints that point to
                                    in-stack continuation frames.  We only need
                                    it so that we can adjust pointers to cont
                                    frames when they are moved to the heap.
                                    The list is cleared once cont frames
                                    are moved to the heap.
                                 */
    int escapeReason;            /* temporary storage to pass data across
                                    longjmp(). */
    void *escapeData[2];         /* ditto. */
    int errorHandlerContinuable; /* A transient flag, set by the error handler
                                    as a result of 'guard' expansion to tell
                                    that the control should return to 'raise'
                                    after handler returns.  See %reraise
                                    in libexc.scm as well. */

    /* Custom debugger or error reporter */
    ScmObj customErrorReporter; /* If set, Scm_ReportError (report-error) calls
                                   this procedure with an exception object.
                                   The default behavior is to show the error
                                   type, message, and the stack trace.
                                   Alter this only if you want to customize
                                   it per thread.
                                 */

    /* Program information */
    int    evalSituation;       /* eval situation (related to eval-when) */

    /* Signal information */
    ScmSignalQueue sigq;
    sigset_t sigMask;           /* current signal mask */

    /* Statistics */
    ScmVMStat stat;
    int profilerRunning;
    ScmVMProfiler *prof;

#if defined(GAUCHE_USE_WTHREADS)
    ScmWinCleanup *winCleanup; /* mimic pthread_cleanup_* */
#endif /*defined(GAUCHE_USE_WTHREADS)*/

    u_long vmid;                /* Numerical ID, mainly for debugging aid.
                                   Can be recycled, so don't use this to
                                   identify thread programtically.
                                   Set by vm_register. */

    ScmCallTrace *callTrace;
    ScmCodeCache *codeCache;

    /* for reset/shift */
    ScmContinuationPrompt *currentPrompt;
    ScmObj resetChain;          /* list of reset information,
                                   where reset information is
                                   (delimited . <dynamic handlers chain>).
                                   the delimited flag is set when 'shift'
                                   appears in 'reset' and the end marker of
                                   partial continuation is set. */

    /* for additional stack trace */
    ScmContFrame *errorCont;    /* continuation saved on error.
                                   this is used to display stack trace
                                   that is dropped during import. */
};

SCM_EXTERN ScmVM *Scm_NewVM(ScmVM *proto, ScmObj name);
SCM_EXTERN int    Scm_AttachVM(ScmVM *vm);
SCM_EXTERN void   Scm_DetachVM(ScmVM *vm);
SCM_EXTERN void   Scm_VMDump(ScmVM *vm);
SCM_EXTERN ScmObj Scm_VMDefaultExceptionHandler(ScmObj exc);
SCM_EXTERN ScmObj Scm_VMCurrentExceptionHandler();
SCM_EXTERN ScmObj Scm_VMExceptionHandlerStack();
SCM_EXTERN void   Scm_VMPushExceptionHandler(ScmObj eh);
SCM_EXTERN ScmObj Scm_VMPopExceptionHandler();
SCM_EXTERN ScmObj Scm_VMWithExceptionHandler(ScmObj eh, ScmObj thunk);
SCM_EXTERN ScmObj Scm_VMThrowException(ScmVM *vm, ScmObj exc, u_long flags);
SCM_EXTERN ScmObj Scm_VMGetSourceInfo(ScmCompiledCode *code, SCM_PCTYPE pc);
SCM_EXTERN ScmObj Scm_VMGetBindInfo(ScmCompiledCode *code, SCM_PCTYPE pc);
SCM_EXTERN void   Scm_VMSetResult(ScmObj obj);
SCM_EXTERN ScmVM *Scm_VMTakeSnapshot(ScmVM *master);

SCM_CLASS_DECL(Scm_VMClass);
#define SCM_CLASS_VM              (&Scm_VMClass)

#if   defined(GAUCHE_USE_PTHREADS)
SCM_EXTERN pthread_key_t Scm_VMKey(void);
#elif defined(GAUCHE_USE_WTHREADS)
SCM_EXTERN DWORD Scm_VMKey(void);
#endif

/* Value of vm->state */
enum {
    SCM_VM_NEW,                 /* This VM is just created and not attached
                                   to the running thread.  vm->thread is not
                                   initialized. */
    SCM_VM_RUNNABLE,            /* This VM is attached to a thread which is
                                   runnable or blocked. */
    SCM_VM_STOPPED,             /* The thread attached to this VM is stopped
                                   by the inspector thread for debugging. */
    SCM_VM_TERMINATED           /* The thread attached to this VM is
                                   terminated. */
};

/* Value of vm->evalSituation */
enum {
    SCM_VM_EXECUTING,           /* we're evaluating the form interactively. */
    SCM_VM_LOADING,             /* we're loading the forms */
    SCM_VM_COMPILING            /* we're batch-compiling the forms */
};

/* Value of vm->stopRequest.  Zero means no request. */
typedef enum {
    SCM_VM_REQUEST_SUSPEND = 1L,   /* Set by Scm_ThreadStop */
    SCM_VM_REQUEST_TERMINATE = 2L, /* Set by Scm_ThreadTerminate */
} ScmVMStopRequest;

/*
 * C stack rewinding
 *  (These macros interacts with VM internals, so must be used
 *  with care.)
 *
 *  These macros should be used if you want to guarantee certain
 *  cleanup is called when the C-stack is "rewind".   So it's like
 *  C-version of dynamic-wind.   The typical usage will be like
 *  the following:
 *
 *   SCM_UNWIND_PROTECT {
 *     preprocess
 *     main operation
 *   } SCM_WHEN_ERROR {
 *     clean up code on abnormal situation
 *     SCM_NEXT_HANDLER;
 *   } SCM_END_PROTECT;
 *   clean up code on normal situation
 *
 *  Note that this construct does not install exception handler or
 *  error handler by itself.   The handler installed by with-error-handler
 *  or with-exception-handler is invoked, and then the SCM_WHEN_ERROR
 *  part is called while the C stack is rewind.
 *  If you want to handle error as well, you should install error handler
 *  by yourself (and deinstall it in the cleanup code).
 *
 *  In general, you MUST call SCM_NEXT_HANDLER in the SCM_WHEN_ERROR clause.
 *  In other words, you shouldn't use SCM_UNWIND_PROTECT as "ignore-errors"
 *  construct.  The C stack is rewind not only at the error situation, but
 *  also when acontinuation is thrown in the main operation.  Except certain
 *  special occasions, stopping C-stack rewinding may cause semantic
 *  inconsistency.   Besides, we don't save signal mask in SCM_UNWIND_PROTECT
 *  for the performance reason; assuming the mask is eventually recovered
 *  by the core exception handling mechanism (see vm.c).
 */

#define SCM_UNWIND_PROTECT                      \
    do {                                        \
       ScmCStack cstack;                        \
       cstack.prev = Scm_VM()->cstack;          \
       cstack.cont = NULL;                      \
       Scm_VM()->cstack = &cstack;              \
       if (sigsetjmp(cstack.jbuf, FALSE) == 0) {

#define SCM_WHEN_ERROR                          \
       } else {

#define SCM_NEXT_HANDLER                                        \
           do {                                                 \
               if (Scm_VM()->cstack->prev) {                    \
                   Scm_VM()->cstack = Scm_VM()->cstack->prev;   \
                   siglongjmp(Scm_VM()->cstack->jbuf, 1);       \
               }                                                \
               else Scm_Exit(1);                                \
           } while (0)

#define SCM_END_PROTECT                                 \
       }                                                \
       Scm_VM()->cstack = Scm_VM()->cstack->prev;       \
    } while (0)

SCM_EXTERN long Scm_VMUnwindProtect(ScmVM *vm, ScmCStack *cstack);
SCM_EXTERN void Scm_VMNextHandler(ScmVM *vm);
SCM_EXTERN void Scm_VMRewindProtect(ScmVM *vm);

SCM_EXTERN ScmObj Scm_VMGetDynamicHandlers(void);
SCM_EXTERN void   Scm_VMSetDynamicHandlers(ScmObj handlers);
SCM_EXTERN void   Scm_VMFlushDynamicHandlers(void);

/*
 * Runtime flags
 */
enum {
    SCM_ERROR_BEING_HANDLED  = (1L<<0), /* we're in an error handler */
    SCM_ERROR_BEING_REPORTED = (1L<<1), /* we're in an error reporter */
    SCM_LOAD_VERBOSE         = (1L<<2), /* report loading files */
    SCM_CASE_FOLD            = (1L<<3), /* symbols are case insensitive */
    SCM_LIMIT_MODULE_MUTATION = (1L<<4),/* disable set! to modify the
                                           global binding in the other
                                           module */
    SCM_COLLECT_VM_STATS     = (1L<<5), /* enable statistics collection
                                           (incurs runtime overhead) */
    SCM_COLLECT_LOAD_STATS   = (1L<<6), /* log the stats of file load
                                           timings (incurs runtime overhead) */
    SCM_CHECK_UNDEFINED_TEST = (1L<<7), /* check if #<undef> appears as
                                           the test value in branch */
    SCM_SAFE_STRING_CURSORS = (1L<<8)   /* Always use large cursors for
                                           extra validation. */
};

#define SCM_VM_RUNTIME_FLAG_IS_SET(vm, flag) ((vm)->runtimeFlags & (flag))
#define SCM_VM_RUNTIME_FLAG_SET(vm, flag)    ((vm)->runtimeFlags |= (flag))
#define SCM_VM_RUNTIME_FLAG_CLEAR(vm, flag)  ((vm)->runtimeFlags &= ~(flag))

/*
 * C-continuation
 */

typedef ScmObj ScmCContinuationProc(ScmObj result, void **data);

SCM_EXTERN void Scm_VMPushCC(ScmCContinuationProc *func,
                             void **data,
                             int datasize);

/*
 * Compiler flags
 */

enum {
    SCM_COMPILE_NOINLINE_GLOBALS = (1L<<0),/* Do not inline global procs
                                              (implies NOINLINE_INLINER, too) */
    SCM_COMPILE_NOINLINE_LOCALS = (1L<<1), /* Do not inline local procs */
    SCM_COMPILE_NOINLINE_CONSTS = (1L<<2), /* Do not inline constants */
    SCM_COMPILE_NOSOURCE = (1L<<3),        /* Do not insert source info */
    SCM_COMPILE_SHOWRESULT = (1L<<4),      /* Display each result of
                                              compilation */
    SCM_COMPILE_NOCOMBINE = (1L<<5),       /* Do not combine instructions */
    SCM_COMPILE_NO_POST_INLINE_OPT = (1L<<6), /* Do not run post-inline
                                                 optimization (pass3). */
    SCM_COMPILE_NO_LIFTING = (1L<<7),      /* Do not run lambda lifting pass
                                              (pass4). */
    SCM_COMPILE_INCLUDE_VERBOSE = (1L<<8), /* Report expansion of 'include' */
    SCM_COMPILE_NOINLINE_SETTERS = (1L<<9), /* Do not inline setters */
    SCM_COMPILE_NODISSOLVE_APPLY = (1L<<10),/* Do not dissolve APPLY
                                              (pass2/dissolve-apply) */
    SCM_COMPILE_LEGACY_DEFINE = (1L<<11),  /* Do not insert toplevel binding
                                              at compile-time. */
    SCM_COMPILE_MUTABLE_LITERALS = (1L<<12),/* Literal pairs are mutable */
    SCM_COMPILE_SRFI_FEATURE_ID = (1L<<13), /* Allow srfi-N feature id in
                                               cond-expand */
    SCM_COMPILE_NOINLINE_INLINER = (1L<<14)/* (internal) Do not invoke custom
                                              inliner and ASM inliners.
                                              hybrid macro is still expanded.
                                              used for macroexpand-all */
};

#define SCM_VM_COMPILER_FLAG_IS_SET(vm, flag) ((vm)->compilerFlags & (flag))
#define SCM_VM_COMPILER_FLAG_SET(vm, flag)    ((vm)->compilerFlags |= (flag))
#define SCM_VM_COMPILER_FLAG_CLEAR(vm, flag)  ((vm)->compilerFlags &= ~(flag))

/*
 * Compiler internal APIs
 */

SCM_EXTERN ScmObj Scm_Compile(ScmObj program, ScmObj mod);

SCM_EXTERN ScmObj Scm_CallSyntaxCompiler(ScmObj syn, ScmObj from, ScmObj env);
SCM_EXTERN ScmObj Scm_CallMacroExpander(ScmMacro *mac, ScmObj expr, ScmObj env);
SCM_EXTERN ScmObj Scm_CallMacroExpanderOld(ScmMacro *mac, ScmObj expr, ScmObj env);
SCM_EXTERN int    Scm_HasInlinerP(ScmObj obj);
SCM_EXTERN ScmObj Scm_CallProcedureInliner(ScmObj obj, ScmObj form, ScmObj env);

/* This is in module.c, but it's not for public, so declaration is here. */
SCM_EXTERN ScmModule* Scm_GaucheInternalModule(void);

#endif /* GAUCHE_VM_H */
