/*
 * thread.c - Scheme thread API
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
 *  $Id: thread.c,v 1.1 2002-07-06 07:13:15 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/vm.h"
#include "gauche/exception.h"

#include <unistd.h>
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
    ScmVM *current = Scm_VM(), *vm;

    if (SCM_PROCEDURE_REQUIRED(thunk) != 0) {
        Scm_Error("thunk required, but got %S", thunk);
    }
    vm = Scm_NewVM(current, current->module, name);
    vm->thunk = thunk;
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
    if (pthread_setspecific(Scm_VMKey(), vm) != 0) {
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
    if (intr) Scm_SigCheck(Scm_VM());
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
    if (intr) Scm_SigCheck(Scm_VM());
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

