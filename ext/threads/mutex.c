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
 *  $Id: mutex.c,v 1.1 2002-07-14 09:53:38 shirok Exp $
 */

#include <math.h>
#include "gauche.h"
#include "gauche/class.h"
#include "gauche/exception.h"

/*=====================================================
 * Mutex
 */

static ScmObj mutex_allocate(ScmClass *klass, ScmObj initargs);
static void   mutex_print(ScmObj mutex, ScmPort *port, ScmWriteContext *ctx);

SCM_DEFINE_BASE_CLASS(Scm_MutexClass, ScmMutex, 
                      mutex_print, NULL, NULL, mutex_allocate,
                      SCM_CLASS_DEFAULT_CPL);

#ifdef GAUCHE_USE_PTHREADS
static void mutex_finalize(GC_PTR obj, GC_PTR data)
{
    ScmMutex *mutex = SCM_MUTEX(obj);
    pthread_mutex_destroy(&(mutex->mutex));
    pthread_cond_destroy(&(mutex->cv));
}
#endif /* GAUCHE_USE_PTHREADS */

static ScmObj mutex_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmMutex *mutex = SCM_ALLOCATE(ScmMutex, klass);
    SCM_SET_CLASS(mutex, klass);
#ifdef GAUCHE_USE_PTHREADS
    {
        GC_finalization_proc ofn; GC_PTR ocd;
        pthread_mutex_init(&(mutex->mutex), NULL);
        pthread_cond_init(&(mutex->cv), NULL);
        GC_REGISTER_FINALIZER(mutex, mutex_finalize, NULL, &ofn, &ocd);
    }
#else  /*!GAUCHE_USE_PTHREADS*/
    (void)SCM_INTERNAL_MUTEX_INIT(mutex->mutex);
#endif /*!GAUCHE_USE_PTHREADS*/
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
 * Accessors
 */
static ScmObj sym_not_owned;
static ScmObj sym_abandoned;
static ScmObj sym_not_abandoned;

static ScmObj mutex_state_get(ScmMutex *mutex)
{
    ScmObj r;
    (void)SCM_INTERNAL_MUTEX_LOCK(mutex->mutex);
    if (mutex->locked) {
        if (mutex->owner) {
            if (mutex->owner->state == SCM_VM_TERMINATED) r = sym_abandoned;
            else r = SCM_OBJ(mutex->owner);
        } else {
            r = sym_not_owned;
        }
    } else {
        r = sym_not_abandoned;
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(mutex->mutex);
    return r;
}

static ScmObj mutex_name_get(ScmMutex *mutex)
{
    return mutex->name;
}

static void mutex_name_set(ScmMutex *mutex, ScmObj name)
{
    mutex->name = name;
}

static ScmObj mutex_specific_get(ScmMutex *mutex)
{
    return mutex->specific;
}

static ScmObj mutex_specific_set(ScmMutex *mutex, ScmObj value)
{
    mutex->specific = value;
}

static ScmClassStaticSlotSpec mutex_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", mutex_name_get, mutex_name_set),
    SCM_CLASS_SLOT_SPEC("state", mutex_state_get, NULL),
    SCM_CLASS_SLOT_SPEC("specific", mutex_specific_get, mutex_specific_set),
    { NULL }
};

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
#ifdef GAUCHE_USE_PTHREADS
    struct timespec ts, *pts;
    ScmObj r = SCM_TRUE;
    ScmVM *abandoned = NULL;
    int intr = FALSE;
    
    pts = Scm_GetTimeSpec(timeout, &ts);
    if (SCM_INTERNAL_MUTEX_LOCK(mutex->mutex) != 0) {
        Scm_Error("mutex-lock!: failed to lock");
    }
    while (mutex->locked) {
        if (mutex->owner && mutex->owner->state == SCM_VM_TERMINATED) {
            abandoned = mutex->owner;
            mutex->locked = FALSE;
            break;
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
    if (abandoned) {
        ScmObj exc = Scm_MakeThreadException(SCM_CLASS_ABANDONED_MUTEX_EXCEPTION, abandoned);
        SCM_THREAD_EXCEPTION(exc)->data = SCM_OBJ(mutex);
        r = Scm_VMThrowException(exc);
    }
    return r;
#else  /* !GAUCHE_USE_PTHREADS */
    return SCM_TRUE;            /* dummy */
#endif /* !GAUCHE_USE_PTHREADS */
}

ScmObj Scm_MutexUnlock(ScmMutex *mutex, ScmConditionVariable *cv, ScmObj timeout)
{
    ScmObj r = SCM_TRUE;
#ifdef GAUCHE_USE_PTHREADS
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
#endif /* GAUCHE_USE_PTHREADS */
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

#ifdef GAUCHE_USE_PTHREADS
static void cv_finalize(GC_PTR obj, GC_PTR data)
{
    ScmConditionVariable *cv = SCM_CONDITION_VARIABLE(obj);
    pthread_cond_destroy(&(cv->cv));
}
#endif /* GAUCHE_USE_PTHREADS */

static ScmObj cv_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmConditionVariable *cv = SCM_ALLOCATE(ScmConditionVariable, klass);
    SCM_SET_CLASS(cv, klass);
#ifdef GAUCHE_USE_PTHREADS
    {
        GC_finalization_proc ofn; GC_PTR ocd;
        pthread_cond_init(&(cv->cv), NULL);
        GC_REGISTER_FINALIZER(cv, cv_finalize, NULL, &ofn, &ocd);
    }
#else  /*!GAUCHE_USE_PTHREADS*/
    (void)SCM_INTERNAL_COND_INIT(cv->cv);
#endif /*!GAUCHE_USE_PTHREADS*/
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
 * Accessors
 */

static ScmObj cv_name_get(ScmConditionVariable *cv)
{
    return cv->name;
}

static void cv_name_set(ScmConditionVariable *cv, ScmObj name)
{
    cv->name = name;
}

static ScmObj cv_specific_get(ScmConditionVariable *cv)
{
    return cv->specific;
}

static ScmObj cv_specific_set(ScmConditionVariable *cv, ScmObj val)
{
    cv->specific = val;
}

static ScmClassStaticSlotSpec cv_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", cv_name_get, cv_name_set),
    SCM_CLASS_SLOT_SPEC("specific", cv_specific_get, cv_specific_set),
    { NULL }
};

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
#ifdef GAUCHE_USE_PTHREADS
    pthread_cond_signal(&(cond->cv));
#endif
    return SCM_UNDEFINED;
}

ScmObj Scm_ConditionVariableBroadcast(ScmConditionVariable *cond)
{
#ifdef GAUCHE_USE_PTHREADS
    pthread_cond_broadcast(&(cond->cv));
#endif
    return SCM_UNDEFINED;
}

/*
 * Initialization
 */

void Scm_Init_mutex(ScmModule *mod)
{
    sym_not_owned     = SCM_INTERN("not-owned");
    sym_abandoned     = SCM_INTERN("abandoned");
    sym_not_abandoned = SCM_INTERN("not-abandoned");
    Scm_InitBuiltinClass(&Scm_MutexClass, "<mutex>", mutex_slots, sizeof(ScmMutex), mod);
    Scm_InitBuiltinClass(&Scm_ConditionVariableClass, "<condition-variable>", cv_slots, sizeof(ScmConditionVariable), mod);
}
