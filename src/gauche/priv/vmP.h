/*
 * vmP.h - ScmVM private API
 *
 *   Copyright (c) 2017-2022  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PRIV_VMP_H
#define GAUCHE_PRIV_VMP_H

#include "gauche/vm.h"

SCM_DECL_BEGIN

/*
 * Tagged continuation frame
 *
 *   A continuation frame can be tagged with call-with-continuation-prompt,
 *   and it marks the bottom of a delimited continuation chain.
 *   The tagged continuation frame is indicated by SCM_CONT_RESET_MARKER
 *   bit in the marker field.
 *
 *   A prompt tag is an ScmObj to distinguish tagged continuation frame.
 *   ScmContFrame->pc points to &(ScmPromptTag->insn), which contains dummy
 *   RET instruction (so that it won't confuse codes that inspect VM state.)
 *
 *   A tagged continuation frame also points ScmPromptData from
 *   ScmContFrame->cpc.  ScmPromptData is not an ScmObj.  It is a struct
 *   to hold abort handler and some dynamic states.  The first word of
 *   ScmPromptData contains a dummy RET instruction, so that it won't confuse
 *   codes that inspect VM state.
 */

struct ScmPromptTagRec {
    SCM_HEADER;
    ScmObj name;                /* arbitrary object for debugging */
    ScmWord insn;               /* a dummy field contains RET insn.
                                   We make cont->pc point to here
                                   for the prompt-tag marked continuation,
                                   so that VM won't crash if it tries
                                   to execute it accidentally. */
};

#define SCM_PROMPT_TAG_PC(ptag)   (&SCM_PROMPT_TAG(ptag)->insn)

typedef struct ScmPromptDataRec {
    ScmWord dummy;              /* RET insn */
    ScmObj abortHandler;        /* abort handler */
    ScmObj dynamicHandlers;     /* dynamic-wind handler chain */
} ScmPromptData;

/*
 * Continuation mark set
 *
 *   Continuation mark set is an opaque object to extract a set of continuation
 *   marks.  In Gauche, it is just a wrapper of the dynamic environment
 *   and continuation chain; continuation-mark-set->list will do the actual
 *   extraction work.
 */

struct ScmContinuationMarkSetRec {
    SCM_HEADER;
    ScmContFrame *cont;
    ScmObj denv;                /* vm->denv */
};

/* To obtain denv keys */
typedef enum {
    SCM_DENV_KEY_EXCEPTION_HANDLER,
    SCM_DENV_KEY_PARAMETERIZATION,
} ScmDenvKeyName;

SCM_EXTERN ScmObj Scm__GetDenvKey(ScmDenvKeyName name);

/*
 * Escape point
 *
 *  EscapePoint (EP) structure keeps certain point of continuation chain
 *  where control can be transferred.   This structure is used for
 *  saved continuations, as well as error handlers.
 *
 *  Normally EPs forms a single list, linked by the prev pointer.
 *  vm->escapePoint points a 'current' EP, whose ehandler is the current
 *  error handler.
 *
 *  However, this simple structure does not work in all cases.
 *  When an error is signalled, we pop EP before executing the error
 *  handler so that an error raised within the error handler will be
 *  handled by the handler of outer EP.
 *
 *    Suppose the current EP is EP0.
 *
 *    (with-error-handler    ;; <- (1)this installs EP1. EP1->cont captures
 *                           ;;        one-shot continuation of this expr.
 *       (lambda (e) ...)    ;; <- (3)this is executed while EP0 is current
 *      (lambda () ...)      ;; <- (2)this is executed while EP1 is current
 *
 *  If the error handler returns, we pass its result to the continuation of
 *  with-error-handler, which is kept in EP1->cont.  The problem arises
 *  if a stack overflow occurs within the error handler, continuation frames
 *  in the stack are relocated to the heap, but EP1->cont isn't updated
 *  since it is out of vm->escapePoint chain.
 *
 *  'Floating' pointer is used to catch such case.  When an EP is popped
 *  before an error handler is ivoked, EP0's floating pointer is set to
 *  point EP1.  When a new EP is pushed, it inherits the previous EP's
 *  floating pointer.  With this scheme, active floating EPs are always
 *  reachable from vm->esapePoint->floating chain.  (NB: the chain length
 *  can be more than 1, if with-error-handler is used within an error handler
 *  and an error is signalled in its body.
 */
typedef struct ScmEscapePointRec {
    struct ScmEscapePointRec *prev;
    ScmObj ehandler;            /* handler closure */
    ScmContFrame *cont;         /* saved continuation */
    ScmObj handlers;            /* saved dynamic handler chain */
    ScmCStack *cstack;          /* vm->cstack when escape point is created.
                                   this will be used to rewind cstack.
                                   this is NULL for partial continuations,
                                   for they can be executed on anywhere
                                   w.r.t. cstack. */
    ScmObj xhandler;            /* saved exception handler */
    ScmObj resetChain;          /* for reset/shift */
    ScmObj partHandlers;        /* for reset/shift */
    int errorReporting;         /* state of SCM_VM_ERROR_REPORTING flag
                                   when this ep is captured.  The flag status
                                   should be restored when the control
                                   transferred to this escape point. */
    int rewindBefore;           /* EXPERIMENTAL: if TRUE, dynamic handlers
                                   are rewound after an exception is raised
                                   and before the exception handler is called.
                                   If FALSE, the exception handler is called
                                   first, then the dynamic handlers are
                                   rewound.   SRFI-18 model and legacy
                                   with-error-handler uses the latter model,
                                   but SRFI-34's guard needs the former model.
                                */
} ScmEscapePoint;

/* Escape types */
#define SCM_VM_ESCAPE_NONE   0
#define SCM_VM_ESCAPE_ERROR  1
#define SCM_VM_ESCAPE_CONT   2
#define SCM_VM_ESCAPE_EXIT   3

/*
 * Call Trace
 */

typedef struct ScmCallTraceEntryRec {
    ScmCompiledCode *base;
    SCM_PCTYPE pc;
} ScmCallTraceEntry;

struct ScmCallTraceRec {
    u_long size;                  /* size of the array, must be 2^n */
    u_long top;                   /* index to the next entry */
    ScmCallTraceEntry entries[1]; /* variable length */
};

ScmCallTrace *Scm__MakeCallTraceQueue(u_long size);


/* For BF and BT instructions, we check for #<undef>.
   This macro assumes to the local variable VM holding ScmVM*.
 */
#define SCM_CHECKED_FALSEP(obj) \
    (SCM_FALSEP(obj) || (SCM_UNDEFINEDP(obj)&&Scm_VMUndefinedBool(vm)))
SCM_EXTERN int Scm_VMUndefinedBool(ScmVM*); /* in boolean.c */


/*
 * Thread Locals
 *   We keep the definition private, so that we can extend it later.
 */

struct ScmThreadLocalRec {
    SCM_HEADER;
    ScmObj name;                /* for debugging. #f or symbol. */
    ScmSize index;              /* negative for inheritable */
    ScmObj initialValue;
    u_long flags;
};

/* Each VM has vector of parameter values.  vm->parameters points to this.
   The vector is extended on demand.
   We might swap this to more sophisticated data structure than
   a simple flat vector in future.
 */
typedef struct ScmVMThreadLocalVectorRec {
    ScmSize size;
    ScmObj *vector;
} ScmVMThreadLocalVector;

struct ScmVMThreadLocalTableRec {
    ScmVMThreadLocalVector vs[2];
};

enum {
      SCM_THREAD_LOCAL_VECTOR_INHERITABLE = 0,
      SCM_THREAD_LOCAL_VECTOR_NONINHERITABLE = 1
};

SCM_EXTERN ScmVMThreadLocalTable *Scm__MakeVMThreadLocalTable(ScmVM *base);

SCM_DECL_END

#endif /*GAUCHE_PRIV_VMP_H*/
