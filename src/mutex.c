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
 *  $Id: mutex.c,v 1.3 2002-05-14 09:36:03 shirok Exp $
 */

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
    mutex->locker = NULL;
    return SCM_OBJ(mutex);
}

static void mutex_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmMutex *mutex = SCM_MUTEX(obj);
    ScmVM *vm = mutex->locker;
    ScmObj name = mutex->name;
    
    if (SCM_FALSEP(name)) Scm_Printf(port, "#<mutex %p ", mutex);
    else                  Scm_Printf(port, "#<mutex %S ", name);
    if (vm) {
        if (vm->state == SCM_VM_TERMINATED) {
            Scm_Printf(port, "abandoned");
        } else {
            Scm_Printf(port, "locked by %S", vm);
        }
    } else {
        Scm_Printf(port, "unlocked");
    }
    Scm_Printf(port, ">");
}

/*
 * Mame mutex
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

ScmObj Scm_MutexLock(ScmMutex *mutex)
{
#ifdef GAUCHE_USE_PTHREAD
    ScmVM *vm = Scm_VM();
    if (SCM_INTERNAL_MUTEX_LOCK(mutex->mutex) != 0) {
        Scm_Error("mutex-lock!: failed to lock");
    }
    while (mutex->locker) {
        pthread_cond_wait(&(mutex->cv), &(mutex->mutex));
    }
    mutex->locker = vm;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(mutex->mutex);
#endif /* GAUCHE_USE_PTHREAD */
    return SCM_OBJ(mutex);
}

ScmObj Scm_MutexUnlock(ScmMutex *mutex)
{
#ifdef GAUCHE_USE_PTHREAD
    ScmVM *vm = Scm_VM();
    if (SCM_INTERNAL_MUTEX_LOCK(mutex->mutex) != 0) {
        Scm_Error("mutex-unlock!: failed to lock");
    }
    mutex->locker = NULL;
    pthread_cond_signal(&(mutex->cv));
    (void)SCM_INTERNAL_MUTEX_UNLOCK(mutex->mutex);
#endif /* GAUCHE_USE_PTHREAD */
    return SCM_OBJ(mutex);
}

