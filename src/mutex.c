/*
 * mutex.c - mutex and condition variables
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
 *  $Id: mutex.c,v 1.1 2002-03-28 19:50:54 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"

/*
 * Mutex
 *
 * NB: Pthread mutex is used just to lock while changing and checking
 * Scheme state.
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
}
#endif /* GAUCHE_USE_PTHREAD */

static ScmObj mutex_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmMutex *mutex = SCM_ALLOCATE(ScmMutex, klass);
    SCM_SET_CLASS(mutex, klass);
#ifdef GAUCHE_USE_PTHREAD
    {
        pthread_mutexattr_t mattr;
        GC_finalization_proc ofn; GC_PTR ocd;
        
        pthread_mutexattr_init(&mattr);
        pthread_mutexattr_setkind_np(&mattr, PTHREAD_MUTEX_RECURSIVE_NP);
        pthread_mutex_init(&(mutex->mutex), &mattr);
        pthread_mutexattr_destroy(&mattr);
        GC_REGISTER_FINALIZER(mutex, mutex_finalize, NULL, &ofn, &ocd);
    }
#else  /*!GAUCHE_USE_PTHREAD*/
    SCM_INTERNAL_MUTEX_INIT(mutex->mutex);
#endif /*!GAUCHE_USE_PTHREAD*/
    mutex->specific = SCM_UNDEFINED;
    mutex->owner = NULL;
    mutex->status = SCM_MUTEX_UNLOCKED;
    return SCM_OBJ(mutex);
}

static void mutex_print(ScmObj mutex, ScmPort *port, ScmWriteContext *ctx)
{
    ScmVM *vm = SCM_MUTEX(mutex)->owner;
    Scm_Printf(port, "#<mutex %p ", mutex);
    if (vm) {
        SCM_ASSERT(SCM_MUTEX(mutex)->status == SCM_MUTEX_LOCKED);
        Scm_Printf(port, "locked by %S", vm);
    } else if (SCM_MUTEX(mutex)->status == SCM_MUTEX_UNLOCKED) {
        Scm_Printf(port, "unlocked");
    } else {
        Scm_Printf(port, "abandoned");
    }
    Scm_Printf(port, ">");
}

/*
 * Lock and unlock mutex
 */

struct mutex_packet {
    ScmVM *vm;
    ScmMutex *mutex;
};

static ScmObj insert_mutex(void *data)
{
    struct mutex_packet *packet = (struct mutex_packet *)data;
    packet->vm->mutexes =
        Scm_Cons(SCM_OBJ(packet->mutex), packet->vm->mutexes);
    return SCM_UNDEFINED;
}

static ScmObj delete_mutex(void *data)
{
    struct mutex_packet *packet = (struct mutex_packet *)data;
    packet->vm->mutexes =
        Scm_DeleteX(SCM_OBJ(packet->mutex), packet->vm->mutexes, SCM_CMP_EQ);
    return SCM_UNDEFINED;
}

ScmObj Scm_MutexLock(ScmMutex *mutex)
{
    ScmVM *vm = Scm_VM();
    int r;
    struct mutex_packet packet;
    if ((r = SCM_INTERNAL_MUTEX_LOCK(mutex->mutex)) != 0) {
        if (r == EDEADLK) {
            /* The calling thread should already have a lock. */
            if (mutex->owner != vm) {
                Scm_Error("mutex-lock!: incomprehensible deadlock situation while attempting to lock %S", mutex);
            }
            return SCM_OBJ(mutex);
        } else {
            Scm_Error("mutex-lock!: failed to lock");
        }
    }
    if (mutex->status == SCM_MUTEX_UNLOCKED) {
        mutex->status = SCM_MUTEX_LOCKED;
    }
    packet.vm = vm;
    packet.mutex = mutex;
    Scm_WithLock(&(vm->vmlock), insert_mutex, &packet, "mutex-lock!");
    mutex->owner = vm;
    return SCM_OBJ(mutex);
}

ScmObj Scm_MutexUnlock(ScmMutex *mutex)
{
    ScmVM *vm = Scm_VM();
    struct mutex_packet packet;
    if (SCM_INTERNAL_MUTEX_UNLOCK(mutex->mutex) != 0) {
        Scm_Error("attempt to unlock %S which is locked by the different thread", mutex);
    }
    packet.vm = vm;
    packet.mutex = mutex;
    Scm_WithLock(&(vm->vmlock), delete_mutex, &packet, "mutex-unlock!");
    mutex->owner = NULL;
    return SCM_OBJ(mutex);
}

