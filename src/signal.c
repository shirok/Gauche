/*
 * signal.c - signal handling
 *
 *  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, disribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: signal.c,v 1.3 2002-01-15 21:05:22 shirok Exp $
 */

#include <signal.h>
#include "gauche.h"
#include "gauche/vm.h"

/* Signals
 *
 *  C-application that embeds Gauche can specify a set of signals
 *  that Gauche should handle.
 *
 *  Gauche sets its internal signal handlers for them.  The signal
 *  is queued to the signal buffer of the thread that caught the
 *  signal.
 *
 *  VM calls Scm_SigCheck() at the "safe" point, which flushes
 *  the signal queue and make a list of handlers to be called.
 */

/*
 * System's signal handler - just enqueue the signal
 */

static void sig_handle(int signum)
{
    ScmVM *vm = Scm_VM();
    if (vm->sigOverflow) return;
    
    if (vm->sigQueueHead <= vm->sigQueueTail) {
        vm->sigQueue[vm->sigQueueTail++] = signum;
        if (vm->sigQueueTail >= SCM_VM_SIGQ_SIZE) {
            vm->sigQueueTail = 0;
        }
    } else {
        vm->sigQueue[vm->sigQueueTail++] = signum;
        if (vm->sigQueueTail >= vm->sigQueueHead) {
            vm->sigOverflow++;
        }
    }
}

/*
 * Called from VM's safe point to flush the queued signals.
 * VM already checks there's a pending signal in the queue.
 */
void Scm_SigCheck(ScmVM *vm)
{
    int i;
    ScmObj sigh, tail;

    sigprocmask(SIG_BLOCK, &vm->sigMask, NULL);
    /* NB: if an error occurs during the critical section,
       some signals may be lost. */
    SCM_UNWIND_PROTECT {
        tail = vm->sigPending;
        if (!SCM_NULLP(tail)) tail = Scm_LastPair(tail);
        while (vm->sigQueueHead != vm->sigQueueTail) {
            int signum = vm->sigQueue[vm->sigQueueHead++];
            if (vm->sigQueueHead >= SCM_VM_SIGQ_SIZE) vm->sigQueueHead = 0;
            sigh = Scm_Assq(SCM_MAKE_INT(signum), vm->sigHandlers);
            if (SCM_PAIRP(sigh)) {
                if (SCM_NULLP(tail)) {
                    tail = Scm_Cons(SCM_CDR(sigh), SCM_NIL);
                } else {
                    SCM_SET_CDR(tail, Scm_Cons(SCM_CDR(sigh), SCM_NIL));
                    tail = SCM_CDR(tail);
                }
            }
        }
    }
    SCM_WHEN_ERROR {
        sigprocmask(SIG_UNBLOCK, &vm->sigMask, NULL);
        SCM_NEXT_HANDLER;
    }
    SCM_END_PROTECT;
    /* TODO: signal overflow handling */
    vm->sigOverflow = 0;
    sigprocmask(SIG_UNBLOCK, &vm->sigMask, NULL);
}


