/*
 * vm.h - Virtual machine
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
 *  $Id: vm.h,v 1.98.2.16 2005-01-15 00:40:02 shirok Exp $
 */

#ifndef GAUCHE_VM_H
#define GAUCHE_VM_H

/* Size of stack per VM (in words). */
#define SCM_VM_STACK_SIZE      10000

/* Maximum # of values allowed for multiple value return */
#define SCM_VM_MAX_VALUES      20

/* Signal queue size */
#define SCM_VM_SIGQ_SIZE       32

/* Finalizer queue size */
#define SCM_VM_FINQ_SIZE       32

/* QueueNotEmpty flag */
#define SCM_VM_SIGQ_MASK       1
#define SCM_VM_FINQ_MASK       2

#define SCM_PCTYPE ScmWord*

/*
 * Compiled code packet
 */

typedef struct ScmCompiledCodeRec {
    SCM_HEADER;
    ScmWord *code;              /* Code vector.  this is allocated as atomic,
                                   to prevent GC from scanning it. */
    ScmObj *constants;          /* Constant vector.  this isn't used during
                                   execution, but kept here so that the
                                   constants in the code vector won't be
                                   GC-ed. */
    int codeSize;               /* size of code vector */
    int constantSize;           /* size of constant vector */
    int maxstack;               /* maximum runtime stack depth */
    ScmObj info;                /* debug info.  alist of instruction offset
                                   and info. */
    ScmObj argInfo;             /* If this code is the body of the closure,
                                   keeps a list of args.  #f otherwise. */
    ScmObj parent;              /* ScmCompiledCode if this code is compiled
                                   within other code chunk.  #f otherwise. */
} ScmCompiledCode;

SCM_CLASS_DECL(Scm_CompiledCodeClass);
#define SCM_CLASS_COMPILED_CODE   (&Scm_CompiledCodeClass)

#define SCM_COMPILED_CODE(obj)    ((ScmCompiledCode*)(obj))
#define SCM_COMPILED_CODE_P(obj)  SCM_XTYPEP(obj, SCM_CLASS_COMPILED_CODE)
#define SCM_COMPILED_CODE_ARG_INFO(obj) (SCM_COMPILED_CODE(obj)->argInfo)

SCM_EXTERN ScmObj Scm_PackCode(ScmObj code);
SCM_EXTERN void Scm_CompiledCodeDump(ScmCompiledCode *cc);
SCM_EXTERN ScmObj Scm_CompiledCodeToList(ScmCompiledCode *cc);
SCM_EXTERN ScmObj Scm_MakeCompiledCode(ScmWord *code, int codeSize,
                                       int maxstack, ScmObj argInfo,
                                       ScmObj info);
SCM_EXTERN void   Scm_VMExecuteToplevels(ScmCompiledCode *cv[]);

/*
 * Environment frame
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
    int size;                   /* size of the frame (excluding header) */
} ScmEnvFrame;

#define ENV_HDR_SIZE   3        /* envframe header size */
#define ENV_SIZE(size)   ((size)+ENV_HDR_SIZE)
#define ENV_FP(env)        (((ScmObj*)(env))-((env)->size))
#define ENV_DATA(env, num) (*(((ScmObj*)(env))-(num)-1))

/*
 * Continuation frame
 *
 *  Continuation is represented as a chain of ScmContFrames.
 *  If argp == NULL && size >= 0, the frame is C continuation.
 */

typedef struct ScmContFrameRec {
    struct ScmContFrameRec *prev; /* previous frame */
    ScmEnvFrame *env;             /* saved environment */
    ScmObj *argp;                 /* saved argument pointer */
    int size;                     /* size of argument frame */
    SCM_PCTYPE pc;                /* next PC */
    ScmCompiledCode *base;        /* base register value */
} ScmContFrame;

#define CONT_FRAME_SIZE  (sizeof(ScmContFrame)/sizeof(ScmObj))

SCM_EXTERN void Scm_CallCC(ScmObj body);

/*
 * Syntactic closure
 *
 *   Syntactic closure encapsulates compile-time environment for
 *   hygienic macro expansion.
 *   See Bawden & Rees, Syntactic Closures, MIT AI Memo 1049, June 1988.
 */

typedef struct ScmSyntacticClosureRec {
    ScmObj env;                 /* compile-time environment */
    ScmObj literals;            /* literal symbols */
    ScmObj expr;                /* expression */
} ScmSyntacticClosure;

SCM_CLASS_DECL(Scm_SyntacticClosureClass);
#define SCM_CLASS_SYNTACTIC_CLOSURE   (&Scm_SyntacticClosureClass)

#define SCM_SYNTACTIC_CLOSURE(obj)   ((ScmSyntacticClosure*)(obj))
#define SCM_SYNTACTIC_CLOSURE_P(obj) SCM_XTYPEP(obj, SCM_CLASS_SYNTACTIC_CLOSURE)

SCM_EXTERN ScmObj Scm_MakeSyntacticClosure(ScmObj env,
                                           ScmObj literals,
                                           ScmObj expr);

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

typedef struct ScmIdentifierRec {
    SCM_HEADER;
    ScmSymbol *name;
    ScmModule *module;
    ScmObj env;
} ScmIdentifier;

SCM_CLASS_DECL(Scm_IdentifierClass);
#define SCM_CLASS_IDENTIFIER    (&Scm_IdentifierClass)

#define SCM_IDENTIFIER(obj)     ((ScmIdentifier*)(obj))
#define SCM_IDENTIFIERP(obj)    SCM_XTYPEP(obj, SCM_CLASS_IDENTIFIER)

SCM_EXTERN ScmObj Scm_MakeIdentifier(ScmSymbol *name, ScmObj env);
SCM_EXTERN ScmObj Scm_CopyIdentifier(ScmIdentifier *id);
SCM_EXTERN int    Scm_IdentifierBindingEqv(ScmIdentifier *id, ScmSymbol *sym,
					   ScmObj env);
SCM_EXTERN int    Scm_FreeVariableEqv(ScmObj var, ScmObj sym, ScmObj env);

/* temporary: this should be proper Scm_MakeIdentifer after adoption of
   the new compiler */
SCM_EXTERN ScmObj Scm_MakeIdentifierWithModule(ScmSymbol *name, ScmObj env,
                                               ScmModule *mod);

/*
 * Escape handling
 */

/*
 * C stack record
 */
typedef struct ScmCStackRec {
    struct ScmCStackRec *prev;
    ScmContFrame *cont;
    sigjmp_buf jbuf;
} ScmCStack;

/*
 * Escape point
 *
 *  EscapePoint structure keeps certain point of continuation chain
 *  where control can be transferred.   This structure is used for
 *  saved continuations, as well as error handlers.
 */
typedef struct ScmEscapePointRec {
    struct ScmEscapePointRec *prev;
    ScmObj ehandler;            /* handler closure */
    ScmContFrame *cont;         /* saved continuation */
    ScmObj handlers;            /* saved dynamic handler chain */
    ScmCStack *cstack;          /* C stack */
    ScmObj xhandler;            /* saved exception handler */
    int errorReporting;         /* state of SCM_VM_ERROR_REPORTING flag
                                   when this ep is captured.  The flag status
                                   should be restored when the control
                                   transferred to this escape point. */
} ScmEscapePoint;


/* Escape types */
#define SCM_VM_ESCAPE_NONE   0
#define SCM_VM_ESCAPE_ERROR  1
#define SCM_VM_ESCAPE_CONT   2
#define SCM_VM_ESCAPE_EXIT   3

/*
 * Parameters
 *
 *  Parameters keep thread-local state.   It is called 'fluids' in some
 *  Scheme implementations.  A thread inherits the parameters from its
 *  creator.   
 */

typedef struct ScmVMParameterTableRec {
    int numParameters;
    int numAllocated;
    ScmObj *vector;
    int *ids;
} ScmVMParameterTable;

SCM_EXTERN void Scm_ParameterTableInit(ScmVMParameterTable *table,
                                       ScmVM *base);

SCM_EXTERN int Scm_MakeParameterSlot(ScmVM *vm, int *newid);
SCM_EXTERN ScmObj Scm_ParameterRef(ScmVM *vm, int index, int id);
SCM_EXTERN ScmObj Scm_ParameterSet(ScmVM *vm, int index, int id, ScmObj value);

/*
 * Signal queue
 *
 *  Gauche installs singnal handlers that simply queues the signal.
 *  See signal.c for the implementaiton.
 *  Signal queue is a fixed size, since we can't really extend it
 *  within the system's signal handler.  For the time being, overflowed
 *  signals are simply discarded.
 */

typedef struct ScmSignalQueueRec {
    int queue[SCM_VM_SIGQ_SIZE];/* Ring buffer for pending signals */
    unsigned int head;     /* points to the queue head */
    unsigned int tail;     /* points to the queue tail */
    unsigned int overflow; /* flag to indicate queue overflow */
    ScmObj pending;        /* pending signal handlers */
} ScmSignalQueue;

SCM_EXTERN void Scm_SignalQueueInit(ScmSignalQueue* q);

/* NB: Obsoleted macro, but kept for backward compatibility.
   Better API should be provided in future. */
#define SCM_SIGPENDING(vm) \
    ((vm)->queueNotEmpty&SCM_VM_SIGQ_MASK)

#define SCM_SIGCHECK(vm) \
    do { if (vm->queueNotEmpty&SCM_VM_SIGQ_MASK) Scm_SigCheck(vm); } while (0)

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
} ScmVMStat;

/*
 * VM structure
 *
 *  In Gauche, each thread has a VM.  Indeed, the Scheme object
 *  <thread> is ScmVM in C.
 *
 *  Most fields of VM are private to the thread that owns the VM.
 *  Only the fields marked as "PUBLIC" should be modified by other
 *  thread, and only with obtaining the lock by VMLOCK mutex.
 *
 *  Note that some fields like "name" and "specific" are not marked
 *  as PUBLIC although they can be referenced or modified by other
 *  thread (via Scheme call thread-specific-set! etc.)   It is the
 *  user program's responsibility to use a mutex.
 *  When you should introspect other thread (like stack trace), make
 *  sure you stopped that thread, or you may get inconsistent result.
 */

struct ScmVMRec {
    SCM_HEADER;
#ifdef GAUCHE_USE_PTHREADS
    pthread_t thread;           /* the thread executing this VM. */
#endif /*!GAUCHE_USE_PTHREADS*/
    int state;                  /* thread state. PUBLIC. */
    ScmInternalMutex  vmlock;   /* mutex to be used to lock this VM
                                   structure.  PUBLIC. */
    ScmInternalCond cond;       /* the condition variable to wait for state
                                   change of this VM.  PUBLIC. */
    ScmVM *canceller;           /* the thread that called thread-terminate!
                                   on this thread.  PUBLIC. */
    ScmObj name;                /* Scheme thread name. */
    ScmObj specific;            /* Scheme thread specific data. */
    ScmProcedure *thunk;        /* Entry point of this VM. */
    ScmObj result;              /* Result of thunk. */
    ScmObj resultException;     /* Exception that causes the thread to terminate.*/
    ScmModule *module;          /* current global namespace.  note that this
                                   is used only in compilation. */
    ScmCStack *cstack;          /* current escape point.  see the comment of
                                   "C stack rewinding" below. */
    unsigned int runtimeFlags;  /* Runtime flags */
    unsigned int compilerFlags; /* Compiler flags */
    unsigned int queueNotEmpty; /* Bitmask if sigq or finq is not empty */

    ScmPort *curin;             /* current input port */
    ScmPort *curout;            /* current output port */
    ScmPort *curerr;            /* current error port */
    ScmVMParameterTable parameters; /* parameter table */

    /* Registers */
    ScmCompiledCode *base;      /* Current executing closure's code packet. */
    SCM_PCTYPE pc;              /* Program pointer.  Points into the code
                                   vector. (base->code) */
    ScmEnvFrame *env;           /* Current environment.                      */
    ScmContFrame *cont;         /* Current continuation.                     */
    ScmObj *argp;               /* Current argument pointer.  Points
                                   to the incomplete environment frame
                                   being accumulated.  This is a part of
                                   continuation.                             */
    ScmObj val0;                /* Value register.                           */
    ScmObj vals[SCM_VM_MAX_VALUES]; /* Value register for multiple values */
    int    numVals;             /* # of values */

    ScmObj handlers;            /* chain of active dynamic handlers          */

    ScmObj *sp;                 /* stack pointer */
    ScmObj *stack;              /* bottom of allocated stack area */
    ScmObj *stackBase;          /* base of current stack area  */
    ScmObj *stackEnd;           /* end of current stack area */

    /* Escape handling */
    ScmObj exceptionHandler;    /* the current exception handler installed by
                                   with-exception-handler. */
    ScmEscapePoint *escapePoint;/* chain of escape points (a kind of one-shot
                                   continuation).  used by system's default
                                   exception handler to escape from the error
                                   handlers. */
    int escapeReason;           /* temporary storage to pass data across
                                   longjmp(). */
    void *escapeData[2];        /* ditto. */

    /* Custom debugger */
    ScmObj defaultEscapeHandler;

    /* Program information */
    ScmObj load_next;           /* list of the directories to be searched */
    ScmObj load_history;        /* history of the nested load */
    ScmObj load_port;           /* current port from which we are loading */

    /* Signal information */
    ScmSignalQueue sigq;
    sigset_t sigMask;           /* current signal mask */

    /* Statistics */
    ScmVMStat stat;
};

SCM_EXTERN ScmVM *Scm_NewVM(ScmVM *base, ScmModule *module, ScmObj name);
SCM_EXTERN void   Scm_VMDump(ScmVM *vm);
SCM_EXTERN void   Scm_VMDefaultExceptionHandler(ScmObj);
SCM_EXTERN ScmObj Scm_VMGetSourceInfo(ScmCompiledCode *code, SCM_PCTYPE pc);
SCM_EXTERN ScmObj Scm_VMGetBindInfo(ScmCompiledCode *code, SCM_PCTYPE pc);

SCM_CLASS_DECL(Scm_VMClass);
#define SCM_CLASS_VM              (&Scm_VMClass)

#ifdef GAUCHE_USE_PTHREADS
SCM_EXTERN pthread_key_t Scm_VMKey(void);
#endif

/* Value of vm->state */
enum {
    SCM_VM_NEW,                 /* This VM is just created and not attached
                                   to the running thread.  vm->thread is not
                                   initialized. */
    SCM_VM_RUNNABLE,            /* This VM is attached to a thread which is
                                   runnable or blocked. */
    SCM_VM_BLOCKED,             /* The thread attached to this VM is stopped
                                   because of thread-yield! or thread-sleep!.
                                   Note that if the thread is blocked by
                                   system call, VM's state is still RUNNABLE.*/
    SCM_VM_TERMINATED           /* The thread attached to this VM is
                                   terminated. */
};

/*
 * VM instructions
 */
#define SCM_VM_INSN_TAG            0x0e

#define SCM_VM_INSNP(obj)            ((SCM_WORD(obj)&0x0f) == SCM_VM_INSN_TAG)
#define SCM_VM_INSN_CODE(obj)        ((SCM_WORD(obj)>>4)&0x0ff)
#define SCM_VM_INSN_ARG(obj)         ((signed long)SCM_WORD(obj) >> 12)

#define SCM_VM_INSN_ARG0(obj)        ((SCM_WORD(obj) >> 12) & 0x03ff)
#define SCM_VM_INSN_ARG1(obj)        ((SCM_WORD(obj) >> 22) & 0x03ff)

#define SCM_VM_INSN(code) \
    SCM_OBJ((long)((code)<<4)|SCM_VM_INSN_TAG)
#define SCM_VM_INSN1(code, arg) \
    SCM_OBJ((long)((arg) << 12) | ((code) << 4) | SCM_VM_INSN_TAG)
#define SCM_VM_INSN2(code, arg0, arg1) \
    SCM_OBJ((long)((arg1) << 22) | ((arg0) << 12) | ((code) << 4) | SCM_VM_INSN_TAG)

#define SCM_VM_INSN_ARG_MAX          ((1L<<(32-13))-1)
#define SCM_VM_INSN_ARG_MIN          (-SCM_VM_INSN_ARG_MAX)
#define SCM_VM_INSN_ARG_FITS(k) \
    (((k)<=SCM_VM_INSN_ARG_MAX)&&((k)>=SCM_VM_INSN_ARG_MIN))

/* Macros for transition to the packed code vector of NVM.
   In the packed code vector, VM insns are stored untagged.
   It eliminates the shift in the dispatcher. */
#define SCM_NVM_INSN_CODE(obj)       (SCM_WORD(obj)&0x0ff)
#define SCM_NVM_INSN_ARG(obj)        ((signed long)SCM_WORD(obj) >> 8)
#define SCM_NVM_INSN_ARG0(obj)       ((SCM_WORD(obj) >>  8) & 0x03ff)
#define SCM_NVM_INSN_ARG1(obj)       ((SCM_WORD(obj) >> 18) & 0x03ff)

#define SCM_NVM_INSN(code)           SCM_WORD(code)
#define SCM_NVM_INSN1(code, arg)     SCM_WORD((long)((arg)<<8) | (code))
#define SCM_NVM_INSN2(code, arg0, arg1)  \
    SCM_WORD((long)((arg1) << 18) | ((arg0) << 8) | (code))

/* Operand type */
enum {
    SCM_VM_OPERAND_NONE,        /* take no operand */
    SCM_VM_OPERAND_OBJ,         /* take ScmObj */
    SCM_VM_OPERAND_CODE,        /* take ScmCompiledCode */
    SCM_VM_OPERAND_ADDR,        /* take address of next code */
};

SCM_EXTERN void   Scm__VMInsnWrite(ScmObj insn, ScmPort *port,
				   ScmWriteContext *ctx);
SCM_EXTERN ScmObj Scm_VMInsnInspect(ScmObj obj);
SCM_EXTERN const char *Scm_VMInsnName(u_int code);
SCM_EXTERN int Scm_VMInsnNumParams(u_int code);
SCM_EXTERN int Scm_VMInsnOperandType(u_int code);
SCM_EXTERN int Scm_VMInsnNameToCode(ScmObj name);
SCM_EXTERN ScmWord Scm_VMInsnBuild(ScmObj insn);

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
 *  If you don't call SCM_NEXT_HANDLER in the SCM_WHEN_ERROR clause,
 *  the control is transferred after SCM_END_PROTECT.  It is not recommended
 *  unless you know what you're doing.   The C stack is rewind not only
 *  at the error situation, but also a continuation is thrown in the main
 *  operation.  Except certain special occasions, stopping C-stack rewinding
 *  may cause semantic inconsistency.
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
    SCM_COLLECT_VM_STATS     = (1L<<5)  /* enable statistics collection
                                           (incurs runtime overhead) */
};

#define SCM_VM_RUNTIME_FLAG_IS_SET(vm, flag) ((vm)->runtimeFlags & (flag))
#define SCM_VM_RUNTIME_FLAG_SET(vm, flag)    ((vm)->runtimeFlags |= (flag))
#define SCM_VM_RUNTIME_FLAG_CLEAR(vm, flag)  ((vm)->runtimeFlags &= ~(flag))

/*
 * C-continuation
 */

#define SCM_CCONT_DATA_SIZE 6

typedef struct ScmCContinuation {
    SCM_HEADER;
    ScmObj (*func)(ScmObj value, void **data);
    void *data[SCM_CCONT_DATA_SIZE];
} ScmCContinuation;

#define SCM_CCONT(obj)            ((ScmCContinuation*)(obj))
#define SCM_CCONTP(obj)           SCM_XTYPEP(obj, SCM_CLASS_CCONT)

SCM_CLASS_DECL(Scm_CContClass);
#define SCM_CLASS_CCONT           (&Scm_CContClass)

SCM_EXTERN void Scm_VMPushCC(ScmObj (*func)(ScmObj value, void **data),
			     void **data,
			     int datasize);

/*
 * Compiler context
 */

enum {
    SCM_COMPILE_STMT,           /* Statement context.  The value of this
                                   expression will be discarded. */
    SCM_COMPILE_TAIL,           /* This is a tail expression. */
    SCM_COMPILE_NORMAL          /* Normal calling sequence. */
};

typedef struct ScmCompilerPacketRec {
    ScmObj constants;
} ScmCompilerContext;

/*
 * Compiler flags
 */

enum {
    SCM_COMPILE_NOINLINE = (1L<<0), /* Do not inline procedures */
    SCM_COMPILE_NOSOURCE = (1L<<1), /* Do not insert source info */
    SCM_COMPILE_SHOWRESULT = (1L<<2), /* Display each result of compilation */
};

#define SCM_VM_COMPILER_FLAG_IS_SET(vm, flag) ((vm)->compilerFlags & (flag))
#define SCM_VM_COMPILER_FLAG_SET(vm, flag)    ((vm)->compilerFlags |= (flag))
#define SCM_VM_COMPILER_FLAG_CLEAR(vm, flag)  ((vm)->compilerFlags &= ~(flag))

/*
 * Compiler internal APIs
 */

SCM_EXTERN ScmObj Scm_Compile(ScmObj program, ScmObj mod);
SCM_EXTERN ScmObj Scm_CompileBody(ScmObj form, ScmObj env, int context);
SCM_EXTERN ScmObj Scm_CompileLookupEnv(ScmObj sym, ScmObj env, int op);

SCM_EXTERN ScmObj Scm_CallSyntaxCompiler(ScmObj syn, ScmObj from, ScmObj env);
SCM_EXTERN ScmObj Scm_CallMacroExpander(ScmMacro *mac, ScmObj expr, ScmObj env);
SCM_EXTERN ScmObj Scm_CallMacroExpanderOld(ScmMacro *mac, ScmObj expr, ScmObj env);
SCM_EXTERN int    Scm_HasInlnierP(ScmObj obj);
SCM_EXTERN ScmObj Scm_CallProcedureInliner(ScmObj obj, ScmObj form, ScmObj env);

/*
 * Inline assembler stuff
 *   These are used in Gauche core to inline VM assembly code for
 *   some built-in procedures.
 */

SCM_EXTERN ScmObj Scm_MakeInlineAsmForm(ScmObj form, ScmObj insn, ScmObj args);
SCM_EXTERN ScmObj Scm_SimpleAsmInliner(ScmObj subr, ScmObj form, ScmObj env,
                                       void *data);

#endif /* GAUCHE_VM_H */
