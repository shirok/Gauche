/*
 * vm.h - Virtual machine
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
 *  $Id: vm.h,v 1.67 2002-05-23 06:21:58 shirok Exp $
 */

#ifndef GAUCHE_VM_H
#define GAUCHE_VM_H

#define SCM_VM_MAX_VALUES      20
#define SCM_VM_SIGQ_SIZE       32

/* Local variable access:
 *   Regardless of frame allocation scheme, local variables are always
 *   accessed like this:
 *
 * (define (foo x y)
 *    ;; here, x is env->data[0], y is env->data[1]
 *    (let ((a ...) (b ...))
 *       ;; here, a is env->data[0], b is env->data[1],
 *       ;;       x is env->up->data[0], and so on.
 *       (lambda (r s)
 *          ;; here, r is env->data[0], s is env->data[1],
 *          ;;       a is env->up->data[0], x is env->up->up->data[0].
 *
 *     ....
 *
 * If frames are allocated on the stack first, they are copied to the
 * heap when a closure is created.   Once copied, it stays on the heap
 * until it's garbage collected.
 *
 * The toplevel frame is always a frame of size 1, containing the toplevel
 * module in data[0].   You can tell it's toplevel if env->up is NULL.
 */

typedef struct ScmEnvFrameRec {
    struct ScmEnvFrameRec *up;  /* static link */
    ScmObj info;                /* source code info for debug */
    int size;                   /* size of the frame */
    ScmObj data[1];             /* variable length */
} ScmEnvFrame;

#define ENV_HDR_SIZE   3        /* envframe header size */
#define ENV_SIZE(size)   ((size)+ENV_HDR_SIZE)

SCM_EXTERN ScmEnvFrame *Scm_GetCurrentEnv(void);

/*
 * Continuation
 *
 *  Continuation is represented as a chain of ScmContFrames.
 */

typedef struct ScmContFrameRec {
    struct ScmContFrameRec *prev; /* previous frame */
    ScmEnvFrame *env;             /* saved environment */
    ScmEnvFrame *argp;            /* saved argument pointer */
    int size;                     /* size of argument frame */
    ScmObj pc;                    /* next PC */
    ScmObj info;                  /* debug info */
} ScmContFrame;

#define CONT_FRAME_SIZE  (sizeof(ScmContFrame)/sizeof(ScmObj))

SCM_EXTERN void Scm_CallCC(ScmObj body);

/*
 * Identifier
 *
 *   Identifier wraps a symbol with its lexical environment.  This
 *   object is used in hygienic macro expansion (see macro.c), and
 *   also used as a placeholder in a global variable reference/assignment
 *   (see compile.c).
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

/*
 * Source info
 *
 *   It is inserted in the compiled code by the compiler, and used
 *   by error handlers to obtain debugging information.  See compile.c
 *   for details.
 *
 *   Will be obsoleted.
 */

typedef struct ScmSourceInfoRec {
    SCM_HEADER;
    ScmObj info;
    struct ScmSourceInfoRec *up;
} ScmSourceInfo;

SCM_CLASS_DECL(Scm_SourceInfoClass);
#define SCM_CLASS_SOURCE_INFO    (&Scm_SourceInfoClass)

#define SCM_SOURCE_INFO(obj)     ((ScmSourceInfo*)(obj))
#define SCM_SOURCE_INFOP(obj)    SCM_XTYPEP(obj, SCM_CLASS_SOURCE_INFO)

SCM_EXTERN ScmObj Scm_MakeSourceInfo(ScmObj info, ScmSourceInfo *up);

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
} ScmEscapePoint;


/* Escape types */
#define SCM_VM_ESCAPE_NONE   0
#define SCM_VM_ESCAPE_ERROR  1
#define SCM_VM_ESCAPE_CONT   2
#define SCM_VM_ESCAPE_EXIT   3

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
#ifdef GAUCHE_USE_PTHREAD
    pthread_t thread;           /* the thread executing this VM. */
#endif /*!GAUCHE_USE_PTHREAD*/
    int state;                  /* thread state. PUBLIC. */
    ScmInternalMutex  vmlock;   /* mutex to be used to lock this VM
                                   structure.  PUBLIC. */
    ScmInternalCond cond;       /* the condition variable to wait for state
                                   change of this VM.  PUBLIC. */
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

    ScmPort *curin;             /* current input port */
    ScmPort *curout;            /* current output port */
    ScmPort *curerr;            /* current error port */

    /* Registers */
    ScmObj pc;                  /* Program pointer.  Points list of
                                   instructions to be executed.              */
    ScmEnvFrame *env;           /* Current environment.                      */
    ScmContFrame *cont;         /* Current continuation.                     */
    ScmEnvFrame *argp;          /* Current argument pointer.  Points
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
    int stackSize;

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
    ScmObj load_requiring;      /* history of the nested require */

    /* Signal information */
    int sigQueue[SCM_VM_SIGQ_SIZE];/* Ring buffer for pending signals */
    unsigned int sigQueueHead;  /* points to the queue head */
    unsigned int sigQueueTail;  /* points to the queue tail */
    unsigned int sigOverflow;   /* flag to indicate queue overflow */
    ScmObj sigHandlers;         /* assoc list of signal handlers */
    ScmObj sigPending;          /* pending signal handlers */
    sigset_t sigMask;           /* current signal mask */
};

SCM_EXTERN ScmVM *Scm_NewVM(ScmVM *base, ScmModule *module, ScmObj name);
SCM_EXTERN void Scm_VMDump(ScmVM *vm);
SCM_EXTERN void Scm_VMDefaultExceptionHandler(ScmObj);

SCM_CLASS_DECL(Scm_VMClass);
#define SCM_CLASS_VM              (&Scm_VMClass)

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

enum {
#define DEFINSN(sym, nam, nparams)  sym,
#include "vminsn.h"
#undef DEFINSN
    SCM_VM_NUM_INSNS
};

SCM_EXTERN void   Scm__VMInsnWrite(ScmObj insn, ScmPort *port,
				   ScmWriteContext *ctx);
SCM_EXTERN ScmObj Scm_VMInsnInspect(ScmObj obj);

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
       cstack.cont = Scm_VM()->cont;            \
       Scm_VM()->cstack = &cstack;              \
       if (sigsetjmp(cstack.jbuf, TRUE) == 0) {
           
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
    SCM_ERROR_BEING_HANDLED = (1L<<0), /* we're in an error handler */
    SCM_LOAD_VERBOSE        = (1L<<1), /* report loading files */
    SCM_CASE_FOLD           = (1L<<2)  /* symbols are case insensitive */
};

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

/*
 * Compiler flags
 */

enum {
    SCM_COMPILE_NOINLINE = (1L<<0), /* Do not inline procedures */
    SCM_COMPILE_NOSOURCE = (1L<<1), /* Do not insert source info */
    SCM_COMPILE_SHOWRESULT = (1L<<2) /* Display each result of compilation */
};

#endif /* GAUCHE_VM_H */
