/*
 * mutex.c - Scheme-level synchronization devices
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

#include <math.h>
#include <gauche.h>
#include <gauche/class.h>
#include <gauche/exception.h>
#include "threads.h"

/*=====================================================
 * Mutex
 */

static ScmObj mutex_allocate(ScmClass *klass, ScmObj initargs);
static void   mutex_print(ScmObj mutex, ScmPort *port, ScmWriteContext *ctx);

static ScmClass *default_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_TopClass), NULL
};

SCM_DEFINE_BASE_CLASS(Scm_MutexClass, ScmMutex,
                      mutex_print, NULL, NULL, mutex_allocate,
                      default_cpl);

static void mutex_finalize(ScmObj obj, void* data)
{
    ScmMutex *mutex = SCM_MUTEX(obj);
    SCM_INTERNAL_MUTEX_DESTROY(mutex->mutex);
    SCM_INTERNAL_COND_DESTROY(mutex->cv);
}

static ScmObj mutex_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmMutex *mutex = SCM_NEW_INSTANCE(ScmMutex, klass);
    SCM_INTERNAL_MUTEX_INIT(mutex->mutex);
    SCM_INTERNAL_COND_INIT(mutex->cv);
    Scm_RegisterFinalizer(SCM_OBJ(mutex), mutex_finalize, NULL);
    mutex->name = SCM_FALSE;
    mutex->specific = SCM_UNDEFINED;
    mutex->locked = FALSE;
    mutex->owner = NULL;
    mutex->locker_proc = mutex->unlocker_proc = SCM_FALSE;
    return SCM_OBJ(mutex);
}

static void mutex_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmMutex *mutex = SCM_MUTEX(obj);

    (void)SCM_INTERNAL_MUTEX_LOCK(mutex->mutex);
    int locked = mutex->locked;
    ScmVM *vm = mutex->owner;
    ScmObj name = mutex->name;
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

static void mutex_specific_set(ScmMutex *mutex, ScmObj value)
{
    mutex->specific = value;
}

static ScmClassStaticSlotSpec mutex_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", mutex_name_get, mutex_name_set),
    SCM_CLASS_SLOT_SPEC("state", mutex_state_get, NULL),
    SCM_CLASS_SLOT_SPEC("specific", mutex_specific_get, mutex_specific_set),
    SCM_CLASS_SLOT_SPEC_END()
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
#ifdef GAUCHE_HAS_THREADS
    ScmTimeSpec ts;
    ScmObj r = SCM_TRUE;
    ScmVM *abandoned = NULL;
    int intr = FALSE;

    ScmTimeSpec *pts = Scm_GetTimeSpec(timeout, &ts);
    SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(mutex->mutex);
    while (mutex->locked) {
        if (mutex->owner && mutex->owner->state == SCM_VM_TERMINATED) {
            abandoned = mutex->owner;
            mutex->locked = FALSE;
            break;
        }
        if (pts) {
            int tr = SCM_INTERNAL_COND_TIMEDWAIT(mutex->cv, mutex->mutex, pts);
            if (tr == SCM_INTERNAL_COND_TIMEDOUT) { r = SCM_FALSE; break; }
            else if (tr == SCM_INTERNAL_COND_INTR) { intr = TRUE; break; }
        } else {
            SCM_INTERNAL_COND_WAIT(mutex->cv, mutex->mutex);
        }
    }
    if (SCM_TRUEP(r)) {
        mutex->locked = TRUE;
        mutex->owner = owner;
    }
    SCM_INTERNAL_MUTEX_SAFE_LOCK_END();
    if (intr) Scm_SigCheck(Scm_VM());
    if (abandoned) {
        ScmObj exc = Scm_MakeThreadException(SCM_CLASS_ABANDONED_MUTEX_EXCEPTION, abandoned);
        SCM_THREAD_EXCEPTION(exc)->data = SCM_OBJ(mutex);
        r = Scm_Raise(exc);
    }
    return r;
#else  /* !GAUCHE_HAS_THREADS */
    return SCM_TRUE;            /* dummy */
#endif /* !GAUCHE_HAS_THREADS */
}

ScmObj Scm_MutexUnlock(ScmMutex *mutex, ScmConditionVariable *cv, ScmObj timeout)
{
    ScmObj r = SCM_TRUE;
#ifdef GAUCHE_HAS_THREADS
    ScmTimeSpec ts;
    int intr = FALSE;

    ScmTimeSpec *pts = Scm_GetTimeSpec(timeout, &ts);
    SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(mutex->mutex);
    mutex->locked = FALSE;
    mutex->owner = NULL;
    SCM_INTERNAL_COND_SIGNAL(mutex->cv);
    if (cv) {
        if (pts) {
            int tr = SCM_INTERNAL_COND_TIMEDWAIT(cv->cv, mutex->mutex, pts);
            if (tr == SCM_INTERNAL_COND_TIMEDOUT)  { r = SCM_FALSE; }
            else if (tr == SCM_INTERNAL_COND_INTR) { intr = TRUE; }
        } else {
            SCM_INTERNAL_COND_WAIT(cv->cv, mutex->mutex);
        }
    }
    SCM_INTERNAL_MUTEX_SAFE_LOCK_END();
    if (intr) Scm_SigCheck(Scm_VM());
#endif /* GAUCHE_HAS_THREADS */
    return r;
}

static ScmObj mutex_locker(ScmObj *args, int argc, void *mutex)
{
    return Scm_MutexLock((ScmMutex*)mutex, SCM_FALSE, Scm_VM());
}


ScmObj Scm_MutexLocker(ScmMutex *mutex)
{
    ScmObj p = mutex->locker_proc;
    if (SCM_FALSEP(p)) {
        /* safe; race is ok here */
        p = Scm_MakeSubr(mutex_locker, (void*)mutex, 0, 0, SCM_FALSE);
        mutex->locker_proc = p;
    }
    return p;
}

static ScmObj mutex_unlocker(ScmObj *args, int argc, void *mutex)
{
    return Scm_MutexUnlock((ScmMutex*)mutex, NULL, SCM_FALSE);
}

ScmObj Scm_MutexUnlocker(ScmMutex *mutex)
{
    ScmObj p = mutex->unlocker_proc;
    if (SCM_FALSEP(p)) {
        /* safe; race is ok here */
        p = Scm_MakeSubr(mutex_unlocker, (void*)mutex, 0, 0, SCM_FALSE);
        mutex->unlocker_proc = p;
    }
    return p;
}

/*=====================================================
 * Condition variable
 */

static ScmObj cv_allocate(ScmClass *klass, ScmObj initargs);
static void   cv_print(ScmObj cv, ScmPort *port, ScmWriteContext *ctx);

SCM_DEFINE_BASE_CLASS(Scm_ConditionVariableClass, ScmConditionVariable,
                      cv_print, NULL, NULL, cv_allocate,
                      default_cpl);

static void cv_finalize(ScmObj obj, void *data)
{
    ScmConditionVariable *cv = SCM_CONDITION_VARIABLE(obj);
    SCM_INTERNAL_COND_DESTROY(cv->cv);
}

static ScmObj cv_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmConditionVariable *cv = SCM_NEW_INSTANCE(ScmConditionVariable, klass);
    SCM_INTERNAL_COND_INIT(cv->cv);
    Scm_RegisterFinalizer(SCM_OBJ(cv), cv_finalize, NULL);
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

static void cv_specific_set(ScmConditionVariable *cv, ScmObj val)
{
    cv->specific = val;
}

static ScmClassStaticSlotSpec cv_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", cv_name_get, cv_name_set),
    SCM_CLASS_SLOT_SPEC("specific", cv_specific_get, cv_specific_set),
    SCM_CLASS_SLOT_SPEC_END()
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
    SCM_INTERNAL_COND_SIGNAL(cond->cv);
    return SCM_UNDEFINED;
}

ScmObj Scm_ConditionVariableBroadcast(ScmConditionVariable *cond)
{
    SCM_INTERNAL_COND_BROADCAST(cond->cv);
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
    Scm_InitStaticClass(&Scm_MutexClass, "<mutex>", mod, mutex_slots, 0);
    Scm_InitStaticClass(&Scm_ConditionVariableClass, "<condition-variable>", mod, cv_slots, 0);
}
