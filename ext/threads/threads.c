/*
 * thread.c - Scheme thread API
 *
 *   Copyright (c) 2000-2010  Shiro Kawai  <shiro@acm.org>
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

static ScmObj thread_error_handler(ScmObj *args, int nargs, void *data)
{
    /* For now, uncaptured error causes thread termination with
       setting <uncaught-exception> to the resultException field.
       It is handled in thread_entry(), so we don't need to do anything
       here. */
    return SCM_UNDEFINED;
}

static SCM_DEFINE_STRING_CONST(thread_error_handler_NAME, "thread-error-handler", 20, 20);
static SCM_DEFINE_SUBR(thread_error_handler_STUB, 1, 0, SCM_OBJ(&thread_error_handler_NAME), thread_error_handler, NULL, NULL);

/* Creation.  In the "NEW" state, a VM is allocated but actual thread
   is not created. */
ScmObj Scm_MakeThread(ScmProcedure *thunk, ScmObj name)
{
    ScmVM *current = Scm_VM(), *vm;

    if (SCM_PROCEDURE_REQUIRED(thunk) != 0) {
        Scm_Error("thunk required, but got %S", thunk);
    }
    vm = Scm_NewVM(current, name);
    vm->thunk = thunk;
    vm->defaultEscapeHandler = SCM_OBJ(&thread_error_handler_STUB);
    return SCM_OBJ(vm);
}

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

#ifdef GAUCHE_USE_PTHREADS
static void thread_cleanup(void *data)
{
    ScmVM *vm = SCM_VM(data);
    ScmObj exc;
    
    /* Change this VM state to TERMINATED, and signals the change
       to the waiting threads. */
    if (pthread_mutex_lock(&vm->vmlock) == EDEADLK) {
        Scm_Panic("dead lock in vm_cleanup.");
    }
    vm->state = SCM_VM_TERMINATED;
    if (vm->canceller) {
        /* This thread is cancelled. */
        exc = Scm_MakeThreadException(SCM_CLASS_TERMINATED_THREAD_EXCEPTION, vm);
        SCM_THREAD_EXCEPTION(exc)->data = SCM_OBJ(vm->canceller);
        vm->resultException = exc;
    }
    pthread_cond_broadcast(&vm->cond);
    pthread_mutex_unlock(&vm->vmlock);
}

static void *thread_entry(void *data)
{
    ScmVM *vm = SCM_VM(data);
    pthread_cleanup_push(thread_cleanup, vm);
    if (pthread_setspecific(Scm_VMKey(), vm) != 0) {
        /* NB: at this point, theVM is not set and we can't use Scm_Error. */
        vm->resultException =
            Scm_MakeError(SCM_MAKE_STR("pthread_setspecific failed"));
    } else {
        SCM_UNWIND_PROTECT {
            vm->result = Scm_ApplyRec(SCM_OBJ(vm->thunk), SCM_NIL);
        } SCM_WHEN_ERROR {
            ScmObj exc;
            switch (vm->escapeReason) {
            case SCM_VM_ESCAPE_CONT:
                /*TODO: better message*/
                vm->resultException =
                    Scm_MakeError(SCM_MAKE_STR("stale continuation thrown"));
                break;
            default:
                Scm_Panic("unknown escape");
            case SCM_VM_ESCAPE_ERROR:
                exc = Scm_MakeThreadException(SCM_CLASS_UNCAUGHT_EXCEPTION, vm);
                SCM_THREAD_EXCEPTION(exc)->data = SCM_OBJ(vm->escapeData[1]);
                vm->resultException = exc;
                Scm_ReportError(SCM_OBJ(vm->escapeData[1]));
                break;
            }
        } SCM_END_PROTECT;
    }
    pthread_cleanup_pop(TRUE);
    return NULL;
}

/* The default signal mask on the thread creation */
static struct threadRec {
    int dummy;                  /* required to place this in data area */
    sigset_t defaultSigmask;
} threadrec = { 0 };
#endif /* GAUCHE_USE_PTHREADS */

ScmObj Scm_ThreadStart(ScmVM *vm)
{
#ifdef GAUCHE_USE_PTHREADS
    int err_state = FALSE, err_create = FALSE;
    pthread_attr_t thattr;
    sigset_t omask;

    (void)SCM_INTERNAL_MUTEX_LOCK(vm->vmlock);
    if (vm->state != SCM_VM_NEW) {
        err_state = TRUE;
    } else {
        SCM_ASSERT(vm->thunk);
        vm->state = SCM_VM_RUNNABLE;
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
    (void)SCM_INTERNAL_MUTEX_UNLOCK(vm->vmlock);
    if (err_state) Scm_Error("attempt to start an already-started thread: %S", vm);
    if (err_create) Scm_Error("couldn't start a new thread: %S", vm);
#else  /*!GAUCHE_USE_PTHREADS*/
    Scm_Error("not implemented!\n");
#endif /*GAUCHE_USE_PTHREADS*/
    return SCM_OBJ(vm);
}

/* Thread join */
ScmObj Scm_ThreadJoin(ScmVM *target, ScmObj timeout, ScmObj timeoutval)
{
#ifdef GAUCHE_USE_PTHREADS
    struct timespec ts, *pts;
    ScmObj result = SCM_FALSE, resultx = SCM_FALSE;
    int intr = FALSE, tout = FALSE;
    
    pts = Scm_GetTimeSpec(timeout, &ts);
    SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(target->vmlock);
    while (target->state != SCM_VM_TERMINATED) {
        if (pts) {
            int tr = pthread_cond_timedwait(&(target->cond), &(target->vmlock), pts);
            if (tr == ETIMEDOUT) { tout = TRUE; break; }
            else if (tr == EINTR) { intr = TRUE; break; }
        } else {
            pthread_cond_wait(&(target->cond), &(target->vmlock));
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
#else  /*!GAUCHE_USE_PTHREADS*/
    Scm_Error("not implemented!\n");
    return SCM_UNDEFINED;
#endif /*!GAUCHE_USE_PTHREADS*/
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
#ifdef GAUCHE_USE_PTHREADS
    struct timespec ts, *pts;
    ScmVM *vm = Scm_VM();
    ScmVM *taker = NULL;
    int timedout;
    int invalid_state = FALSE;
    pts = Scm_GetTimeSpec(timeout, &ts);

 retry:
    timedout = FALSE;
    pthread_mutex_lock(&target->vmlock);
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
            target->stopRequest = TRUE;
            target->attentionRequest = TRUE;
        }
        while (target->state != SCM_VM_STOPPED && !timedout) {
            if (pts) {
                timedout = pthread_cond_timedwait(&target->cond,
                                                  &target->vmlock,
                                                  pts);
            } else {
                pthread_cond_wait(&target->cond, &target->vmlock);
            }
        }
    }
    pthread_mutex_unlock(&target->vmlock);

    if (invalid_state) {
        Scm_Error("cannot stop a thread %S since it is in neither runnable nor stopped state",
                  target);
    }
    if (taker != NULL) {
        Scm_Error("target %S is already under inspection by %S", target, taker);
    }

    if (timedout == EINTR) { Scm_SigCheck(vm); goto retry; }
    if (timedout == ETIMEDOUT) return timeoutval;
    return SCM_OBJ(target);
#else  /*!GAUCHE_USE_PTHREADS*/
    Scm_Error("not implemented!\n");
    return SCM_UNDEFINED;
#endif /*!GAUCHE_USE_PTHREADS*/
}


/* Release inspected thread */
ScmObj Scm_ThreadCont(ScmVM *target)
{
#ifdef GAUCHE_USE_PTHREADS
    int not_stopped = FALSE;
    ScmVM *stopped_by_other = NULL;

    pthread_mutex_lock(&target->vmlock);
    if (target->inspector == NULL) {
        not_stopped = TRUE;
    } else if (target->inspector != Scm_VM()
               && target->inspector->state != SCM_VM_TERMINATED) {
        stopped_by_other = target->inspector;
    } else {
        target->inspector = NULL;
        target->state = SCM_VM_RUNNABLE;
        target->stopRequest = FALSE;
        pthread_cond_broadcast(&target->cond);
    }
    pthread_mutex_unlock(&target->vmlock);
    if (not_stopped) Scm_Error("target %S is not stopped", target);
    if (stopped_by_other) Scm_Error("target %S is stopped by other thread %S",
                                    target, stopped_by_other);
    return SCM_OBJ(target);
#else  /*!GAUCHE_USE_PTHREADS*/
    Scm_Error("not implemented!\n");
    return SCM_UNDEFINED;
#endif /*!GAUCHE_USE_PTHREADS*/
}

/* Thread sleep */
ScmObj Scm_ThreadSleep(ScmObj timeout)
{
#ifdef GAUCHE_USE_PTHREADS
    struct timespec ts, *pts;
    ScmInternalCond dummyc = PTHREAD_COND_INITIALIZER;
    ScmInternalMutex dummym = PTHREAD_MUTEX_INITIALIZER;
    int intr = FALSE;
    pts = Scm_GetTimeSpec(timeout, &ts);
    if (pts == NULL) Scm_Error("thread-sleep! can't take #f as a timeout value");
    pthread_mutex_lock(&dummym);
    if (pthread_cond_timedwait(&dummyc, &dummym, pts) == EINTR) {
        intr = TRUE;
    }
    pthread_mutex_unlock(&dummym);
    if (intr) Scm_SigCheck(Scm_VM());
#else  /*!GAUCHE_USE_PTHREADS*/
    Scm_Error("not implemented!\n");
#endif /*!GAUCHE_USE_PTHREADS*/
    return SCM_UNDEFINED;
}

/* Thread terminate */
ScmObj Scm_ThreadTerminate(ScmVM *target)
{
#ifdef GAUCHE_USE_PTHREADS
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
        pthread_exit(NULL);
    } else {
        (void)SCM_INTERNAL_MUTEX_LOCK(target->vmlock);
        /* This ensures only the first call of thread-terminate! on a thread
           is in effect. */
        if (target->canceller == NULL) {
            target->canceller = vm;
            pthread_cancel(target->thread);
        }
        (void)SCM_INTERNAL_MUTEX_UNLOCK(target->vmlock);
    }
#else  /*!GAUCHE_USE_PTHREADS*/
    Scm_Error("not implemented!\n");
#endif /*!GAUCHE_USE_PTHREADS*/
    return SCM_UNDEFINED;
}

/*
 * Initialization.
 */
void Scm_Init_threads(ScmModule *mod)
{
#ifdef GAUCHE_USE_PTHREADS
    sigfillset(&threadrec.defaultSigmask);
#endif /*GAUCHE_USE_PTHREADS*/
}

