/*
 * thread.c - Scheme thread API
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

#include <gauche.h>
#include <gauche/vm.h>
#include <gauche/extend.h>
#include <gauche/exception.h>
#include "threads.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif

/*==============================================================
 * Thread interface
 */

/* Creation.  In the "NEW" state, a VM is allocated but actual thread
   is not created. */
ScmObj Scm_MakeThread(ScmProcedure *thunk, ScmObj name)
{
    ScmVM *current = Scm_VM();

    if (SCM_PROCEDURE_REQUIRED(thunk) != 0) {
        Scm_Error("thunk required, but got %S", thunk);
    }
    ScmVM *vm = Scm_NewVM(current, name);
    vm->thunk = thunk;
    return SCM_OBJ(vm);
}

/* Common routine for proper shutdown of a thread.  Usually this is called
   by the thread owning VM, via pthread_cleanup mechanism.  However, in the
   emergency shutdown, the canceller thread may call this
   (see Scm_ThreadTerminate below).
   Caller must hold vm->vmlock */
static void thread_cleanup_inner(ScmVM *vm)
{
    /* Change this VM state to TERMINATED, and signals the change
       to the waiting threads. */
    vm->state = SCM_VM_TERMINATED;
    if (vm->canceller) {
        /* This thread is cancelled. */
        ScmObj e = Scm_MakeThreadException(
            SCM_CLASS_TERMINATED_THREAD_EXCEPTION, vm);
        SCM_THREAD_EXCEPTION(e)->data = SCM_OBJ(vm->canceller);
        vm->resultException = e;
    }
    SCM_INTERNAL_COND_BROADCAST(vm->cond);
}

/* Called by pthread_cleanup mechanism.   After this, Scm_VM() won't return
   a valid VM pointer.  */
static void thread_cleanup(void *data)
{
    ScmVM *vm = SCM_VM(data);
    SCM_INTERNAL_MUTEX_LOCK(vm->vmlock);
    thread_cleanup_inner(vm);
    SCM_INTERNAL_MUTEX_UNLOCK(vm->vmlock);
    Scm_DetachVM(vm);
}

#if defined(GAUCHE_HAS_THREADS)
static SCM_INTERNAL_THREAD_PROC_RETTYPE thread_entry(void *data)
{
    ScmVM *vm = SCM_VM(data);

    if (!Scm_AttachVM(vm)) {
        vm->resultException =
            Scm_MakeError(SCM_MAKE_STR("attaching VM to thread failed"));
        thread_cleanup(vm);
    } else {
        SCM_INTERNAL_THREAD_CLEANUP_PUSH(thread_cleanup, vm);
        SCM_UNWIND_PROTECT {
            vm->result = Scm_ApplyRec(SCM_OBJ(vm->thunk), SCM_NIL);
        } SCM_WHEN_ERROR {
            switch (vm->escapeReason) {
            case SCM_VM_ESCAPE_CONT:
                /*TODO: better message*/
                vm->resultException =
                    Scm_MakeError(SCM_MAKE_STR("stale continuation thrown"));
                break;
            case SCM_VM_ESCAPE_ERROR: {
                /* Error handling is delegated to the thread that calls
                   thread-join!. */
                ScmObj exc =
                    Scm_MakeThreadException(SCM_CLASS_UNCAUGHT_EXCEPTION, vm);
                SCM_THREAD_EXCEPTION(exc)->data = SCM_OBJ(vm->escapeData[1]);
                vm->resultException = exc;
                break;
            }
            default:
                Scm_Panic("unknown escape");
            }
        } SCM_END_PROTECT;
        SCM_INTERNAL_THREAD_CLEANUP_POP();
    }
    return SCM_INTERNAL_THREAD_PROC_RETVAL;
}

/* The default signal mask on the thread creation */
static struct threadRec {
    int dummy;                  /* required to place this in data area */
    sigset_t defaultSigmask;
} threadrec = { 0 };
#endif /* defined(GAUCHE_HAS_THREADS) */

/* Start a thread.  If the VM is in "NEW" state, create a new thread and
   make it run.

   With pthread, the real thread is started as "detached" mode; i.e. once
   the thread exits, the resources allocated for the thread by the system
   is collected, including the result of the thread.  During this
   deconstruction phase, the handler thread_cleanup() runs and saves the
   thread result to the ScmVM structure.  If nobody cares about the
   result of the thread, ScmVM structure will eventually be GCed.
   This is to prevent exitted thread's system resources from being
   uncollected.
 */
ScmObj Scm_ThreadStart(ScmVM *vm)
{
    int err_state = FALSE, err_create = FALSE;

    (void)SCM_INTERNAL_MUTEX_LOCK(vm->vmlock);
    if (vm->state != SCM_VM_NEW) {
        err_state = TRUE;
    } else {
        SCM_ASSERT(vm->thunk);
        vm->state = SCM_VM_RUNNABLE;
#if defined(GAUCHE_USE_PTHREADS)
        {
            pthread_attr_t thattr;
            sigset_t omask;
            pthread_attr_init(&thattr);
            pthread_attr_setdetachstate(&thattr, PTHREAD_CREATE_DETACHED);
            pthread_sigmask(SIG_SETMASK, &threadrec.defaultSigmask, &omask);
            if (pthread_create(&vm->thread, &thattr, thread_entry, vm) != 0) {
                vm->state = SCM_VM_NEW;
                err_create = TRUE;
            }
            pthread_sigmask(SIG_SETMASK, &omask, NULL);
            pthread_attr_destroy(&thattr);
        }
#elif defined(GAUCHE_USE_WTHREADS)
        {
            HANDLE h = GC_CreateThread(NULL, /* security */
                                       0,    /* default stack size */
                                       thread_entry, /* start proc */
                                       vm,           /* parameter */
                                       0,            /* flags */
                                       NULL);        /* thread id receiver */
            if (h != NULL) {
                vm->thread = h;
            } else {
                vm->state = SCM_VM_NEW;
                err_create = TRUE;
            }
        }
#else  /* no threads */
        Scm_Error("Thread is not available.");
#endif
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(vm->vmlock);
    if (err_state) Scm_Error("attempt to start an already-started thread: %S", vm);
    if (err_create) Scm_Error("couldn't start a new thread: %S", vm);
    return SCM_OBJ(vm);
}

/* Thread join */
ScmObj Scm_ThreadJoin(ScmVM *target, ScmObj timeout, ScmObj timeoutval)
{
#ifdef GAUCHE_HAS_THREADS
    ScmTimeSpec ts;
    ScmObj result = SCM_FALSE, resultx = SCM_FALSE;
    int intr = FALSE, tout = FALSE;

    ScmTimeSpec *pts = Scm_GetTimeSpec(timeout, &ts);
    SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(target->vmlock);
    while (target->state != SCM_VM_TERMINATED) {
        if (pts) {
            int tr = SCM_INTERNAL_COND_TIMEDWAIT(target->cond, target->vmlock, pts);
            if (tr == SCM_INTERNAL_COND_TIMEDOUT) { tout = TRUE; break; }
            else if (tr == SCM_INTERNAL_COND_INTR) { intr = TRUE; break; }
        } else {
            SCM_INTERNAL_COND_WAIT(target->cond, target->vmlock);
        }
    }
    if (!tout) {
        result = target->result; resultx = target->resultException;
        target->resultException = SCM_FALSE; /* clear it */
    }
    SCM_INTERNAL_MUTEX_SAFE_LOCK_END();
    if (intr) Scm_SigCheck(Scm_VM());
    if (tout) {
        if (SCM_UNBOUNDP(timeoutval)) {
            ScmObj e = Scm_MakeThreadException(SCM_CLASS_JOIN_TIMEOUT_EXCEPTION, target);
            result = Scm_Raise(e);
        } else {
            result = timeoutval;
        }
    } else if (SCM_CONDITIONP(resultx)) {
        result = Scm_Raise(resultx);
    }
    return result;
#else  /*!GAUCHE_HAS_THREADS*/
    Scm_Error("not implemented!\n");
    return SCM_UNDEFINED;
#endif /*!GAUCHE_HAS_THREADS*/
}

/* Attempt to stop other thread.   See process_queued_requests() in vm.c
   for the target VM operation.

   VM Stop protocol:

             target                           inspector
  ------------------------------------------------------------------------
                                    * LOCK(target->vmlock)
                                    * Check if target is already inspected
                                    * Set target's STOP flag
                                    * Wait on target->cond
                                      until target->state becomes STOPPED
   * If STOP flag is up,
     set self->state to STOPPED
     and signal self->cond
     then wait on self->cond until
     target->state changes from STOPPED
                                    * Inspect target VM state
                                    * Set taget->state to RUNNABLE,
                                      then signal state->cond
   * Resume execution
  -------------------------------------------------------------------------
 */

ScmObj Scm_ThreadStop(ScmVM *target, ScmObj timeout, ScmObj timeoutval)
{
#ifdef GAUCHE_HAS_THREADS
    ScmTimeSpec ts;
    ScmVM *vm = Scm_VM();
    ScmVM *taker = NULL;
    int timedout;
    int invalid_state = FALSE;
    ScmTimeSpec *pts = Scm_GetTimeSpec(timeout, &ts);

 retry:
    timedout = FALSE;
    SCM_INTERNAL_MUTEX_LOCK(target->vmlock);
    if (target->state != SCM_VM_RUNNABLE
        && target->state != SCM_VM_STOPPED) {
        invalid_state = TRUE;
    } else if (target->inspector != NULL
        && target->inspector != vm
        && target->inspector->state != SCM_VM_TERMINATED) {
        taker = target->inspector;
    } else {
        /* NB: if target->inspector == vm, it means we have requested to
           stop the thread before, and haven't called ThreadCont yet.
           In that case we don't need to set stopRequest again; either target
           thread already cleared it and stopped/going to stop, or
           the target thread hasn't seen the flag and it's still ON. */
        if (target->inspector != vm) {
            target->inspector = vm;
            target->stopRequest = SCM_VM_REQUEST_SUSPEND;
            target->attentionRequest = TRUE;
        }
        while (target->state != SCM_VM_STOPPED && !timedout) {
            if (pts) {
                timedout = SCM_INTERNAL_COND_TIMEDWAIT(target->cond,
                                                       target->vmlock,
                                                       pts);
            } else {
                SCM_INTERNAL_COND_WAIT(target->cond, target->vmlock);
            }
        }
    }
    SCM_INTERNAL_MUTEX_UNLOCK(target->vmlock);

    if (invalid_state) {
        Scm_Error("cannot stop a thread %S since it is in neither runnable nor stopped state",
                  target);
    }
    if (taker != NULL) {
        Scm_Error("target %S is already under inspection by %S", target, taker);
    }

    if (timedout == SCM_INTERNAL_COND_INTR) { Scm_SigCheck(vm); goto retry; }
    if (timedout == SCM_INTERNAL_COND_TIMEDOUT) return timeoutval;
    return SCM_OBJ(target);
#else  /*!GAUCHE_HAS_THREADS*/
    Scm_Error("not implemented!\n");
    return SCM_UNDEFINED;
#endif /*!GAUCHE_HAS_THREADS*/
}


/* Release inspected thread */
ScmObj Scm_ThreadCont(ScmVM *target)
{
#ifdef GAUCHE_HAS_THREADS
    int not_stopped = FALSE;
    ScmVM *stopped_by_other = NULL;

    SCM_INTERNAL_MUTEX_LOCK(target->vmlock);
    if (target->inspector == NULL) {
        not_stopped = TRUE;
    } else if (target->inspector != Scm_VM()
               && target->inspector->state != SCM_VM_TERMINATED) {
        stopped_by_other = target->inspector;
    } else {
        target->inspector = NULL;
        target->state = SCM_VM_RUNNABLE;
        target->stopRequest = 0;
        SCM_INTERNAL_COND_BROADCAST(target->cond);
    }
    SCM_INTERNAL_MUTEX_UNLOCK(target->vmlock);
    if (not_stopped) Scm_Error("target %S is not stopped", target);
    if (stopped_by_other) Scm_Error("target %S is stopped by other thread %S",
                                    target, stopped_by_other);
    return SCM_OBJ(target);
#else  /*!GAUCHE_HAS_THREADS*/
    Scm_Error("not implemented!\n");
    return SCM_UNDEFINED;
#endif /*!GAUCHE_HAS_THREADS*/
}

/* Thread sleep */
ScmObj Scm_ThreadSleep(ScmObj timeout)
{
#ifdef GAUCHE_HAS_THREADS
    ScmTimeSpec ts;
    ScmInternalCond dummyc;
    ScmInternalMutex dummym;
    int intr = FALSE;

    SCM_INTERNAL_COND_INIT(dummyc);
    SCM_INTERNAL_MUTEX_INIT(dummym);
    ScmTimeSpec *pts = Scm_GetTimeSpec(timeout, &ts);
    if (pts == NULL) Scm_Error("thread-sleep! can't take #f as a timeout value");
    SCM_INTERNAL_MUTEX_LOCK(dummym);
    if (SCM_INTERNAL_COND_TIMEDWAIT(dummyc, dummym, pts) == SCM_INTERNAL_COND_INTR) {
        intr = TRUE;
    }
    SCM_INTERNAL_MUTEX_UNLOCK(dummym);
    SCM_INTERNAL_MUTEX_DESTROY(dummym);
    SCM_INTERNAL_COND_DESTROY(dummyc);
    if (intr) Scm_SigCheck(Scm_VM());
#else  /*!GAUCHE_HAS_THREADS*/
    Scm_Error("not implemented!\n");
#endif /*!GAUCHE_HAS_THREADS*/
    return SCM_UNDEFINED;
}

/* Thread termination

   We try to terminate the thread gracefully as possible.
   First, we use vm->stopRequest mechanism.  If the target thread is
   in VM loop, it responds to the flag and terminates itself.
   If that fails, then we use more agressive means.

   TODO: We should probably make it configurable whether to use the
   forcible termination---it is too dangerous.
 */

/* Caller must hold target->vmlock.  return TRUE if the target terminates
   gracefully. */
static int wait_for_termination(ScmVM *target)
{
    ScmTimeSpec ts;
    int r;
    ScmObj t = Scm_MakeFlonum(0.001); /* 1ms. somewhat arbitrary */
    Scm_GetTimeSpec(t, &ts);
    do {
        r = SCM_INTERNAL_COND_TIMEDWAIT(target->cond, target->vmlock, &ts);
    } while (r != SCM_INTERNAL_COND_TIMEDOUT
             && target->state != SCM_VM_TERMINATED);
    return (r == 0);
}

ScmObj Scm_ThreadTerminate(ScmVM *target)
{
    ScmVM *vm = Scm_VM();
    if (target == vm) {
        /* self termination */
        (void)SCM_INTERNAL_MUTEX_LOCK(target->vmlock);
        if (target->canceller == NULL) {
            target->canceller = vm;
        }
        (void)SCM_INTERNAL_MUTEX_UNLOCK(target->vmlock);
        /* Need to unlock before calling pthread_exit(), or the cleanup
           routine can't obtain the lock */
        SCM_INTERNAL_THREAD_EXIT();
        /*NOTREACHED*/
    }

    (void)SCM_INTERNAL_MUTEX_LOCK(target->vmlock);
    if (target->state == SCM_VM_RUNNABLE || target->state == SCM_VM_STOPPED) {
        do {
            /* This ensures only the first call of thread-terminate! on a
               thread is in effect. */
            if (target->canceller == NULL) {
                target->canceller = vm;

                /* First try */
                target->stopRequest = SCM_VM_REQUEST_TERMINATE;
                target->attentionRequest = TRUE;
                if (wait_for_termination(target)) break;

                /* Second try */
                SCM_ASSERT(target->thread);
#if defined(GAUCHE_USE_PTHREADS)
# if defined(GAUCHE_PTHREAD_SIGNAL)
                pthread_kill(target->thread, GAUCHE_PTHREAD_SIGNAL);
# endif /*defined(GAUCHE_PTHREAD_SIGNAL)*/
#elif defined(GAUCHE_USE_WTHREADS)
                /* TODO: implement signal mechanism using an event */
#endif  /* defined(GAUCHE_USE_WTHREADS) */
                if (wait_for_termination(target)) break;

                /* Last resort */
                thread_cleanup_inner(target);
#if defined(GAUCHE_USE_PTHREADS)
                pthread_cancel(target->thread);
#elif defined(GAUCHE_USE_WTHREADS)
                TerminateThread(target->thread, 0);
#endif
            }
        } while (0);
    }
    /* target either is terminated or hasn't been run */
    target->state = SCM_VM_TERMINATED;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(target->vmlock);
    return SCM_UNDEFINED;
}

/*
 * Initialization.
 */
void Scm_Init_threads(ScmModule *mod)
{
#ifdef GAUCHE_USE_PTHREADS
    sigfillset(&threadrec.defaultSigmask);
# if defined(GAUCHE_PTHREAD_SIGNAL)
    sigdelset(&threadrec.defaultSigmask, GAUCHE_PTHREAD_SIGNAL);
# endif /*defined(GAUCHE_PTHRAD_SIGNAL)*/
#endif /*GAUCHE_USE_PTHREADS*/
}
