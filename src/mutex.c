/*
 * mutex.c - Scheme-level synchronization devices
 *
 *  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: mutex.c,v 1.9 2002-05-17 10:36:29 shirok Exp $
 */

#include <math.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"

/*=====================================================
 * Mutex
 */

static ScmObj mutex_allocate(ScmClass *klass, ScmObj initargs);
static void   mutex_print(ScmObj mutex, ScmPort *port, ScmWriteContext *ctx);

SCM_DEFINE_BASE_CLASS(Scm_MutexClass, ScmMutex, 
                      mutex_print, NULL, NULL, mutex_allocate,
                      SCM_CLASS_DEFAULT_CPL);

#ifdef GAUCHE_USE_PTHREAD
static void mutex_finalize(GC_PTR obj, GC_PTR data)
{
    ScmMutex *mutex = SCM_MUTEX(obj);
    pthread_mutex_destroy(&(mutex->mutex));
    pthread_cond_destroy(&(mutex->cv));
}
#endif /* GAUCHE_USE_PTHREAD */

static ScmObj mutex_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmMutex *mutex = SCM_ALLOCATE(ScmMutex, klass);
    SCM_SET_CLASS(mutex, klass);
#ifdef GAUCHE_USE_PTHREAD
    {
        GC_finalization_proc ofn; GC_PTR ocd;
        pthread_mutex_init(&(mutex->mutex), NULL);
        pthread_cond_init(&(mutex->cv), NULL);
        GC_REGISTER_FINALIZER(mutex, mutex_finalize, NULL, &ofn, &ocd);
    }
#else  /*!GAUCHE_USE_PTHREAD*/
    (void)SCM_INTERNAL_MUTEX_INIT(mutex->mutex);
#endif /*!GAUCHE_USE_PTHREAD*/
    mutex->name = SCM_FALSE;
    mutex->specific = SCM_UNDEFINED;
    mutex->locked = FALSE;
    mutex->owner = NULL;
    return SCM_OBJ(mutex);
}

static void mutex_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmMutex *mutex = SCM_MUTEX(obj);
    ScmVM *vm;
    ScmObj name;
    int locked;

    (void)SCM_INTERNAL_MUTEX_LOCK(mutex->mutex);
    locked = mutex->locked;
    vm = mutex->owner;
    name = mutex->name;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(mutex->mutex);
    
    if (SCM_FALSEP(name)) Scm_Printf(port, "#<mutex %p ", mutex);
    else                  Scm_Printf(port, "#<mutex %S ", name);
    if (locked) {
        if (vm) {
            if (vm->state == SCM_VM_TERMINATED) {
                Scm_Printf(port, "unlocked/abandoned>");
            } else {
                Scm_Printf(port, "locked/owned by %S>", vm);
            }
        } else {
            Scm_Printf(port, "locked/not-owned>");
        }
    } else {
        Scm_Printf(port, "unlocked/not-abandoned>");
    }
}

/*
 * Make mutex
 */
ScmObj Scm_MakeMutex(ScmObj name)
{
    ScmObj m = mutex_allocate(SCM_CLASS_MUTEX, SCM_NIL);
    SCM_MUTEX(m)->name = name;
    return m;
}

/*
 * Lock and unlock mutex
 */

ScmObj Scm_MutexLock(ScmMutex *mutex, ScmObj timeout, ScmVM *owner)
{
#ifdef GAUCHE_USE_PTHREAD
    struct timespec ts, *pts;
    ScmObj r = SCM_TRUE;
    int intr = FALSE, abandoned = FALSE;
    
    pts = Scm_GetTimeSpec(timeout, &ts);
    if (SCM_INTERNAL_MUTEX_LOCK(mutex->mutex) != 0) {
        Scm_Error("mutex-lock!: failed to lock");
    }
    while (mutex->locked) {
        if (mutex->owner && mutex->owner->state == SCM_VM_TERMINATED) {
            abandoned = TRUE;
        }
        if (pts) {
            int tr = pthread_cond_timedwait(&(mutex->cv), &(mutex->mutex), pts);
            if (tr == ETIMEDOUT) { r = SCM_FALSE; break; }
            else if (tr == EINTR) { intr = TRUE; break; }
        } else {
            pthread_cond_wait(&(mutex->cv), &(mutex->mutex));
        }
    }
    if (SCM_TRUEP(r)) {
        mutex->locked = TRUE;
        mutex->owner = owner;
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(mutex->mutex);
    if (intr) Scm_SigCheck(Scm_VM());
    /*TODO: need to throw abandoned mutex exception */
    if (abandoned) Scm_Error("abandoned mutex exception");
    return r;
#else  /* !GAUCHE_USE_PTHREAD */
    return SCM_TRUE;            /* dummy */
#endif /* !GAUCHE_USE_PTHREAD */
}

ScmObj Scm_MutexUnlock(ScmMutex *mutex, ScmConditionVariable *cv, ScmObj timeout)
{
    ScmObj r = SCM_TRUE;
#ifdef GAUCHE_USE_PTHREAD
    struct timespec ts, *pts;
    ScmVM *vm = Scm_VM();
    int intr = FALSE;
    
    pts = Scm_GetTimeSpec(timeout, &ts);
    if (SCM_INTERNAL_MUTEX_LOCK(mutex->mutex) != 0) {
        Scm_Error("mutex-unlock!: failed to lock");
    }
    mutex->locked = FALSE;
    mutex->owner = NULL;
    pthread_cond_signal(&(mutex->cv));
    if (cv) {
        if (pts) {
            int tr = pthread_cond_timedwait(&(cv->cv), &(mutex->mutex), pts);
            if (tr == ETIMEDOUT)  { r = SCM_FALSE; }
            else if (tr == EINTR) { intr = TRUE; }
        } else {
            pthread_cond_wait(&(cv->cv), &(mutex->mutex));
        }
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(mutex->mutex);
    if (intr) Scm_SigCheck(Scm_VM());
#endif /* GAUCHE_USE_PTHREAD */
    return r;
}

/*=====================================================
 * Condition variable
 */

static ScmObj cv_allocate(ScmClass *klass, ScmObj initargs);
static void   cv_print(ScmObj cv, ScmPort *port, ScmWriteContext *ctx);

SCM_DEFINE_BASE_CLASS(Scm_ConditionVariableClass, ScmConditionVariable,
                      cv_print, NULL, NULL, cv_allocate,
                      SCM_CLASS_DEFAULT_CPL);

#ifdef GAUCHE_USE_PTHREAD
static void cv_finalize(GC_PTR obj, GC_PTR data)
{
    ScmConditionVariable *cv = SCM_CONDITION_VARIABLE(obj);
    pthread_cond_destroy(&(cv->cv));
}
#endif /* GAUCHE_USE_PTHREAD */

static ScmObj cv_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmConditionVariable *cv = SCM_ALLOCATE(ScmConditionVariable, klass);
    SCM_SET_CLASS(cv, klass);
#ifdef GAUCHE_USE_PTHREAD
    {
        GC_finalization_proc ofn; GC_PTR ocd;
        pthread_cond_init(&(cv->cv), NULL);
        GC_REGISTER_FINALIZER(cv, cv_finalize, NULL, &ofn, &ocd);
    }
#else  /*!GAUCHE_USE_PTHREAD*/
    (void)SCM_INTERNAL_COND_INIT(cv->cv);
#endif /*!GAUCHE_USE_PTHREAD*/
    cv->name = SCM_FALSE;
    cv->specific = SCM_UNDEFINED;
    return SCM_OBJ(cv);
}

static void cv_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmConditionVariable *cv = SCM_CONDITION_VARIABLE(obj);
    ScmObj name = cv->name;
    if (SCM_FALSEP(name)) Scm_Printf(port, "#<condition-variable %p>", cv);
    else                  Scm_Printf(port, "#<condition-variable %S>", name);
}

/*
 * Make condition variable
 */
ScmObj Scm_MakeConditionVariable(ScmObj name)
{
    ScmObj cv = cv_allocate(SCM_CLASS_CONDITION_VARIABLE, SCM_NIL);
    SCM_CONDITION_VARIABLE(cv)->name = name;
    return cv;
}

ScmObj Scm_ConditionVariableSignal(ScmConditionVariable *cond)
{
#ifdef GAUCHE_USE_PTHREAD
    pthread_cond_signal(&(cond->cv));
#endif
    return SCM_UNDEFINED;
}

ScmObj Scm_ConditionVariableBroadcast(ScmConditionVariable *cond)
{
#ifdef GAUCHE_USE_PTHREAD
    pthread_cond_broadcast(&(cond->cv));
#endif
    return SCM_UNDEFINED;
}


