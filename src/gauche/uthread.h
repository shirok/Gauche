/*
 * uthread.h - user level thread primitives
 *
 *   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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

/*
 * This is currently a dummy header file used by configurations without
 * threading support.   This *may* grow into something that allows user-
 * level preemptive threadings, though it is very low priority, since
 * more and more platforms provides system-level threads.
 */

#ifndef GAUCHE_UTHREAD_H
#define GAUCHE_UTHREAD_H

/* Abstract thread API */

#undef GAUCHE_HAS_THREADS  /*define this if the system supports real threads*/

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
typedef int ScmInternalThread;
#define SCM_INTERNAL_THREAD_INIT(thr)      (0)
#define SCM_INTERNAL_THREAD_INITIALIZED_P(thr)  TRUE
#define SCM_INTERNAL_THRAED_GETCURRENT()   (0)
#define SCM_INTERNAL_THREAD_SETSPECIFIC(key, val) TRUE
#define SCM_INTERNAL_THREAD_EXIT()         _exit(0)

#define SCM_INTERNAL_THREAD_CLEANUP_PUSH(fn, data) {
#define SCM_INTERNAL_THREAD_CLEANUP_POP()          }

#define SCM_INTERNAL_THREAD_PROC_RETTYPE   void*
#define SCM_INTERNAL_THREAD_PROC_RETVAL    NULL

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
typedef int ScmInternalMutex;
#define SCM_INTERNAL_MUTEX_INIT(mutex)     (0)
#define SCM_INTERNAL_MUTEX_LOCK(mutex)     (0)
#define SCM_INTERNAL_MUTEX_UNLOCK(mutex)   (0)
#define SCM_INTERNAL_MUTEX_DESTROY(mutex)  (0)
#define SCM_INTERNAL_MUTEX_INITIALIZER     (0)

#define SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(mutex) /*empty*/
#define SCM_INTERNAL_MUTEX_SAFE_LOCK_END()        /*empty*/

/* ScmInternalFastlock - fast lock
 *
 *  We use this type of lock when we know lock contention is rare.
 *  If the system provides some sort of spinlock, it would be a
 *  nice fit.  Otherwise the implementation can make these an alias
 *  to SCM_INTERNAL_MUTEX_*.
 */
typedef int ScmInternalFastlock;
#define SCM_INTERNAL_FASTLOCK_INIT(fl)     (0)
#define SCM_INTERNAL_FASTLOCK_LOCK(fl)     (0)
#define SCM_INTERNAL_FASTLOCK_UNLOCK(fl)   (0)
#define SCM_INTERNAL_FASTLOCK_DESTROY(fl)  (0)

/* ScmInternalCond - condition variable
 *
 *  Pthreads-style condition variables.  SCM_INTERNAL_COND_INITIALIZER
 *  may be used for the placeholder of static variables (but the
 *  variable still needs to be initialized by SCM_INTERNAL_COND_INIT.
 *
 *  SCM_INTERNAL_COND_TIMEDWAIT should return either 0 (signalled),
 *  SCM_INTERNAL_COND_TIMEDOUT, or SCM_INTERNAL_COND_INTR.
 */
typedef int ScmInternalCond;
#define SCM_INTERNAL_COND_INIT(cond)       (0)
#define SCM_INTERNAL_COND_SIGNAL(cond)     (0)
#define SCM_INTERNAL_COND_BROADCAST(cond)  (0)
#define SCM_INTERNAL_COND_WAIT(cond, mutex) (0)
#define SCM_INTERNAL_COND_TIMEDWAIT(cond, mutex, timespec) (0)
#define SCM_INTERNAL_COND_DESTROY(cond)    (0)
#define SCM_INTERNAL_COND_INITIALIZER      (0)
#define SCM_INTERNAL_COND_TIMEDOUT         (0)
#define SCM_INTERNAL_COND_INTR             (0)

/* Issues a full memory barrier */
#define SCM_INTERNAL_SYNC()

#endif /* GAUCHE_UTHREAD_H */
