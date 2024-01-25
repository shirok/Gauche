/*
 * pthread.h - pthread primitives
 *
 *   Copyright (c) 2000-2024  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PTHREAD_H
#define GAUCHE_PTHREAD_H

/* Abstract thread implementation based on pthreads. */

#include <pthread.h>

/* ScmInternalThread - a handle to the system's thread.

   The variable of type ScmInternalThread must be initialized by
   SCM_INTERNAL_THREAD_INIT.  You can check if the variable is
   initialized or not by SCM_INTERNAL_THREAD_INITIALIZED_P.

   SCM_INTERNAL_THREAD_GETCURRENT() should return the current thread.
   SCM_INTERNAL_THREAD_SETSPECIFIC(key, val) sets the value to the
   thread-local storage identified by key.  It should evaluate
   to TRUE on success, FALSE on error.

   SCM_INTERNAL_THREAD_CLEANUP_{PUSH|POP} is an abstraction similar
   to pthread_cleanup_{push|pop}.  We assume no Scheme error is
   signalled between these macros.

   SCM_INTERNAL_THREAD_PROC_RET{TYPE|VAL} specify return type and value
   of the thread entry procedure.  See ext/threads/threads.c.
 */
typedef pthread_t ScmInternalThread;
#define SCM_INTERNAL_THREAD_INIT(thr)   ((thr) = (pthread_t)NULL)
#define SCM_INTERNAL_THREAD_INITIALIZED_P(thr)  ((thr) != (pthread_t)NULL)
#define SCM_INTERNAL_THREAD_GETCURRENT()  pthread_self()
#define SCM_INTERNAL_THREAD_SETSPECIFIC(key, val) \
    (pthread_setspecific((key), (val)) == 0)
#define SCM_INTERNAL_THREAD_EXIT()        pthread_exit(NULL)

#define SCM_INTERNAL_THREAD_CLEANUP_PUSH(fn, data) \
    pthread_cleanup_push(fn, data)
#define SCM_INTERNAL_THREAD_CLEANUP_POP() \
    pthread_cleanup_pop(1)

#define SCM_INTERNAL_THREAD_PROC_RETTYPE   void*
#define SCM_INTERNAL_THREAD_PROC_RETVAL    NULL

/* On POSIX systems, when available, we reserve one signal to notify
   other threads to terminate.
   NB: POSIX requires at least 8 realtime signals, but cygwin doesn't
   provide them (on cygwin, SIGRTMIN == SIGRTMAX).   We use SIGPWR
   instead.
*/
#if defined(SIGRTMIN) && !defined(GAUCHE_PTHREAD_SIGNAL)
#  if !defined(__CYGWIN__)
#    define GAUCHE_PTHREAD_SIGNAL (SIGRTMIN+5)
#  else
#    define GAUCHE_PTHREAD_SIGNAL SIGPWR
#  endif
#endif /*defined(SIGRTMIN) && !defined(GAUCHE_PTHREAD_SIGNAL)*/

/* ScmInternalMutex - a mutex structure.

   The variable of type ScmInternaMutex must be initialized by
   SCM_INTERNAL_MUTEX_INIT at runtime.  If a static initializer is
   needed, SCM_INTERNAL_MUTEX_INITIALIZER---but it's merely for making
   the compiler happy, and does not guarantee the mutex is initialized.
   You still need to use SCM_INTERNAL_MUTEX_INIT.   The resources
   associated to the mutex would be released by SCM_INTERNAL_MUTEX_DESTROY.

   To lock/unlock the mutex, SCM_INTERNAL_MUTEX_LOCK and
   SCM_INTERNAL_MUTEX_UNLOCK can be used.   They should be used
   with care, especially the thread should not be canceled between
   them.

   If you need to have a cancellable code in the critical
   section, you should use SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN and
   SCM_INTERNAL_MUTEX_SAFE_LOCK_END.  They use cleanup mechanism
   provided by the underlying system, such as pthread_cleanup_push/pop,
   to make sure the mutex is unlocked when the thread terminates.
   These macros must be used in the pair at the same syntactic scope,
   for they can be expanded to opening block and closing block tokens,
   respectively.
 */
typedef pthread_mutex_t ScmInternalMutex;
#define SCM_INTERNAL_MUTEX_INIT(mutex)    pthread_mutex_init(&(mutex), NULL)
#define SCM_INTERNAL_MUTEX_LOCK(mutex)    pthread_mutex_lock(&(mutex))
#define SCM_INTERNAL_MUTEX_UNLOCK(mutex)  pthread_mutex_unlock(&(mutex))
#define SCM_INTERNAL_MUTEX_DESTROY(mutex) pthread_mutex_destroy(&(mutex))
#define SCM_INTERNAL_MUTEX_INITIALIZER    PTHREAD_MUTEX_INITIALIZER

SCM_EXTERN void Scm__MutexCleanup(void *); /* in core.c */

/* Mutex operation with cleanup.  The dummy statement before
   pthread_cleanup_pop() allows a label to be placed before
   SAFE_LOCK_END macro.  Without that, it could be an error
   if pthread_cleanup_pop expands into something beginning with
   closing brace. */
#define SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(mutex)               \
    pthread_mutex_lock(&(mutex));                               \
    pthread_cleanup_push(Scm__MutexCleanup, &(mutex))
#define SCM_INTERNAL_MUTEX_SAFE_LOCK_END() /*dummy*/; pthread_cleanup_pop(1)

/* ScmInternalFastlock - fast lock
 *
 *  We use this type of lock when we know lock contention is rare.
 *  If the system provides some sort of spinlock, it would be a
 *  nice fit.  Otherwise the implementation can make these an alias
 *  to SCM_INTERNAL_MUTEX_*.
 */
#ifdef HAVE_PTHREAD_SPINLOCK_T
typedef pthread_spinlock_t ScmInternalFastlock;
#define SCM_INTERNAL_FASTLOCK_INIT(spin) \
    pthread_spin_init(&(spin), PTHREAD_PROCESS_PRIVATE)
#define SCM_INTERNAL_FASTLOCK_LOCK(spin) \
    pthread_spin_lock(&(spin))
#define SCM_INTERNAL_FASTLOCK_UNLOCK(spin) \
    pthread_spin_unlock(&(spin))
#define SCM_INTERNAL_FASTLOCK_DESTROY(spin) \
    pthread_spin_destroy(&(spin))
#define SCM_INTERNAL_FASTLOCK_FINALIZATION_NOT_REQUIRED
#else  /*!HAVE_PTHREAD_SPINLOCK_T*/
/* We don't provide fast lock */
typedef pthread_mutex_t ScmInternalFastlock;
#define SCM_INTERNAL_FASTLOCK_INIT(fl)   SCM_INTERNAL_MUTEX_INIT(fl)
#define SCM_INTERNAL_FASTLOCK_LOCK(fl)   SCM_INTERNAL_MUTEX_LOCK(fl)
#define SCM_INTERNAL_FASTLOCK_UNLOCK(fl) SCM_INTERNAL_MUTEX_UNLOCK(fl)
#define SCM_INTERNAL_FASTLOCK_DESTROY(fl) SCM_INTERNAL_MUTEX_DESTROY(fl)
#endif /*!HAVE_PTHREAD_SPINLOCK_T*/

/* ScmInternalCond - condition variable
 *
 *  Pthreads-style condition variables.  SCM_INTERNAL_COND_INITIALIZER
 *  may be used for the placeholder of static variables (but the
 *  variable still needs to be initialized by SCM_INTERNAL_COND_INIT.
 *
 *  SCM_INTERNAL_COND_TIMEDWAIT should return either 0 (signalled),
 *  SCM_INTERNAL_COND_TIMEDOUT, or SCM_INTERNAL_COND_INTR.
 */
typedef pthread_cond_t ScmInternalCond;
#define SCM_INTERNAL_COND_INIT(cond)        pthread_cond_init(&(cond), NULL)
#define SCM_INTERNAL_COND_SIGNAL(cond)      pthread_cond_signal(&(cond))
#define SCM_INTERNAL_COND_BROADCAST(cond)   pthread_cond_broadcast(&(cond))
#define SCM_INTERNAL_COND_WAIT(cond, mutex) pthread_cond_wait(&(cond), &(mutex))
#define SCM_INTERNAL_COND_TIMEDWAIT(cond, mutex, ptimespec) \
    pthread_cond_timedwait(&(cond), &(mutex), (ptimespec))
#define SCM_INTERNAL_COND_DESTROY(cond)     pthread_cond_destroy(&(cond))
#define SCM_INTERNAL_COND_INITIALIZER       PTHREAD_COND_INITIALIZER
#define SCM_INTERNAL_COND_TIMEDOUT          ETIMEDOUT
#define SCM_INTERNAL_COND_INTR              EINTR

/* Issues a full memory barrier */
#define SCM_INTERNAL_SYNC()                 __sync_synchronize()

#endif /* GAUCHE_PTHREAD_H */
