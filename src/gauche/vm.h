/*
 * vm.h - Virtual machine
 *
 *  Copyright(C) 2000 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: vm.h,v 1.42 2001-09-08 13:13:45 shirok Exp $
 */

#ifndef GAUCHE_VM_H
#define GAUCHE_VM_H

#define SCM_VM_MAX_VALUES      20

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

extern ScmEnvFrame *Scm_GetCurrentEnv(void);

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
} ScmContFrame;

#define CONT_FRAME_SIZE  5

extern void Scm_CallCC(ScmObj body);

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

extern ScmClass Scm_IdentifierClass;
#define SCM_CLASS_IDENTIFIER    (&Scm_IdentifierClass)

#define SCM_IDENTIFIER(obj)     ((ScmIdentifier*)(obj))
#define SCM_IDENTIFIERP(obj)    SCM_XTYPEP(obj, SCM_CLASS_IDENTIFIER)

extern ScmObj Scm_MakeIdentifier(ScmSymbol *name, ScmObj env);
extern ScmObj Scm_CopyIdentifier(ScmIdentifier *id);
extern int Scm_IdentifierBindingEqv(ScmIdentifier *id, ScmSymbol *sym, ScmObj env);
extern int Scm_FreeVariableEqv(ScmObj var, ScmObj sym, ScmObj env);

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

extern ScmClass Scm_SourceInfoClass;
#define SCM_CLASS_SOURCE_INFO    (&Scm_SourceInfoClass)

#define SCM_SOURCE_INFO(obj)     ((ScmSourceInfo*)(obj))
#define SCM_SOURCE_INFOP(obj)    SCM_XTYPEP(obj, SCM_CLASS_SOURCE_INFO)

extern ScmObj Scm_MakeSourceInfo(ScmObj info, ScmSourceInfo *up);

/*
 * Escape handling
 */

/*
 * C stack record
 */
typedef struct ScmCStackRec {
    struct ScmCStackRec *prev;
    ScmContFrame *cont;
    jmp_buf jbuf;
} ScmCStack;

/*
 * Escape handler
 */
typedef struct ScmEscapePointRec {
    struct ScmEscapePointRec *prev;
    ScmObj ehandler;            /* handler closure */
    ScmContFrame *cont;         /* saved continuation */
    ScmObj handlers;            /* saved dynamic handler chain */
    ScmCStack *cstack;          /* C stack */
} ScmEscapePoint;


/* Escape types */
#define SCM_VM_ESCAPE_NONE   0
#define SCM_VM_ESCAPE_ERROR  1
#define SCM_VM_ESCAPE_CONT   2

/*
 * VM structure
 */

struct ScmVMRec {
    SCM_HEADER;
    ScmVM *parent;
    ScmModule *module;          /* current global namespace */
    ScmCStack *cstack;     /* current escape point */

    unsigned int compilerFlags; /* Compiler flags */
    unsigned int errorFlags;    /* Error flags */

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
    ScmEscapePoint *escapePoint;
    int escapeReason;
    void *escapeData[2];
};

extern ScmVM *Scm_SetVM(ScmVM *vm);
extern ScmVM *Scm_NewVM(ScmVM *base, ScmModule *module);
extern void Scm_VMDump(ScmVM *vm);
extern void Scm_VMDefaultExceptionHandler(ScmObj, void *);

extern ScmClass Scm_VMClass;
#define SCM_CLASS_VM              (&Scm_VMClass)

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
    SCM_OBJ(((code)<<4)|SCM_VM_INSN_TAG)
#define SCM_VM_INSN1(code, arg) \
    SCM_OBJ(((arg) << 12) | ((code) << 4) | SCM_VM_INSN_TAG)
#define SCM_VM_INSN2(code, arg0, arg1) \
    SCM_OBJ(((arg1) << 22) | ((arg0) << 12) | ((code) << 4) | SCM_VM_INSN_TAG)

#define SCM_VM_INSN_ARG_MAX          ((1L<<((SIZEOF_LONG*8)-13))-1)
#define SCM_VM_INSN_ARG_MIN          (-SCM_VM_INSN_ARG_MAX)
#define SCM_VM_INSN_ARG_FITS(k) \
    (((k)<=SCM_VM_INSN_ARG_MAX)&&((k)>=SCM_VM_INSN_ARG_MIN))

enum {
#define DEFINSN(sym, nam, nparams)  sym,
#include "vminsn.h"
#undef DEFINSN
    SCM_VM_NUM_INSNS
};

extern void Scm__VMInsnWrite(ScmObj insn, ScmPort *port, ScmWriteContext *ctx);
extern ScmObj Scm_VMInsnInspect(ScmObj obj);

/*
 * Error handling
 *
 *  These macros interacts with VM internals, so must be used
 *  with care.
 */

#define SCM_PUSH_ERROR_HANDLER                  \
    do {                                        \
       ScmCStack cstack;                        \
       cstack.prev = Scm_VM()->cstack;          \
       cstack.cont = Scm_VM()->cont;            \
       Scm_VM()->cstack = &cstack;              \
       if (setjmp(cstack.jbuf) == 0) {
           
#define SCM_WHEN_ERROR                          \
       } else {

#define SCM_PROPAGATE_ERROR                                     \
           do {                                                 \
               if (Scm_VM()->cstack->prev) {                    \
                   Scm_VM()->cstack = Scm_VM()->cstack->prev;   \
                   longjmp(Scm_VM()->cstack->jbuf, 1);          \
               }                                                \
               else exit(1);                                    \
           } while (0)

#define SCM_POP_ERROR_HANDLER                           \
       }                                                \
       Scm_VM()->cstack = Scm_VM()->cstack->prev;       \
    } while (0)

/* flag for vm->errorFlag */
enum {
    SCM_ERROR_BEING_HANDLED = (1L<<0) /* we're in an error handler */
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

extern ScmClass Scm_CContClass;
#define SCM_CLASS_CCONT           (&Scm_CContClass)

extern void Scm_VMPushCC(ScmObj (*func)(ScmObj value, void **data),
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

/* for test */
#define ENABLE_STACK_CHECK  1
#undef EXPLICIT_STACK_CHECK
#undef FUNCTION_STACK_CHECK

#endif /* GAUCHE_VM_H */
