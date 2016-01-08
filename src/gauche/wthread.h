/*
 * wthread.h - win32 thread primitives
 *
 *   Copyright (c) 2011-2015  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_WTHREAD_H
#define GAUCHE_WTHREAD_H

#define GAUCHE_HAS_THREADS 1

/* Win32 thread primitive adaptations */

/* Thread */
typedef HANDLE ScmInternalThread;
#define SCM_INTERNAL_THREAD_INIT(thr)   ((thr) = INVALID_HANDLE_VALUE)
#define SCM_INTERNAL_THREAD_INITIALIZED_P(thr) \
    ((thr) != INVALID_HANDLE_VALUE)
#define SCM_INTERNAL_THREAD_GETCURRENT()  GetCurrentThread()
#define SCM_INTERNAL_THREAD_SETSPECIFIC(key, val) TlsSetValue(key, val)
#define SCM_INTERNAL_THREAD_EXIT()        Scm__WinThreadExit()

SCM_EXTERN void Scm__WinThreadExit(void);

/* to mimic pthread_cleanup things */
typedef struct ScmWinCleanupRec {
    void (*cleanup)(void *data);
    void *data;
    struct ScmWinCleanupRec *prev;
} ScmWinCleanup;

#define SCM_INTERNAL_THREAD_CLEANUP_PUSH(fn_, data_) \
    do { ScmWinCleanup cup__;                     \
         ScmVM *vm__ = Scm_VM();                  \
         cup__.cleanup = (fn_);                   \
         cup__.data = (data_);                    \
         cup__.prev = vm__->winCleanup;           \
         vm__->winCleanup = &cup__

#define SCM_INTERNAL_THREAD_CLEANUP_POP() \
        vm__->winCleanup = cup__.prev;    \
        cup__.cleanup(cup__.data);        \
    } while (0)

#define SCM_INTERNAL_THREAD_PROC_RETTYPE   DWORD WINAPI
#define SCM_INTERNAL_THREAD_PROC_RETVAL    0

/* Mutex */
typedef HANDLE ScmInternalMutex;
#define SCM_INTERNAL_MUTEX_INIT(mutex)    ((mutex) = Scm__WinCreateMutex())
#define SCM_INTERNAL_MUTEX_LOCK(mutex)    Scm__WinMutexLock(mutex)
#define SCM_INTERNAL_MUTEX_UNLOCK(mutex)  ReleaseMutex(mutex)
#define SCM_INTERNAL_MUTEX_DESTROY(mutex) CloseHandle(mutex)
#define SCM_INTERNAL_MUTEX_INITIALIZER    INVALID_HANDLE_VALUE

/* helper functions - in system.c  */
SCM_EXTERN HANDLE Scm__WinCreateMutex(void);
SCM_EXTERN int Scm__WinMutexLock(HANDLE);

SCM_EXTERN void Scm__MutexCleanup(void *); /* in core.c */

/* TODO: We need to implement the safe cancel mechanism
   and then handle this safe lock stuff.  For now, just a placeholder. */
#define SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(mutex) \
    SCM_INTERNAL_MUTEX_LOCK(mutex);               \
    SCM_INTERNAL_THREAD_CLEANUP_PUSH(Scm__MutexCleanup, &mutex)

#define SCM_INTERNAL_MUTEX_SAFE_LOCK_END() \
    SCM_INTERNAL_THREAD_CLEANUP_POP()

/* Condition variable

   Native condition variable support is only available on Windows Vista
   and later.  We don't want to drop XP support (yet), so we avoid using
   it.  Instead we emulate posix condition variable semantics.
   We follow the SignalObjectAndWait solution shown in
   <http://www1.cse.wustl.edu/~schmidt/win32-cv-1.html>
*/
typedef struct ScmInternalCondRec {
   int numWaiters;                  /* # of waiting threads */
   CRITICAL_SECTION numWaitersLock; /* protect numWaiters */
   HANDLE mutex;                    /* protect cond operations */
   HANDLE sem;                      /* to queue waiting threads */
   HANDLE done;                     /* an auto-reset event for a waiter
                                       threads to tell CV that it's
                                       finished examining the condition. */
   int broadcast;                   /* flag to indicate whether we're
                                       bloadcasting or just signalling. */
} ScmInternalCond;
#define SCM_INTERNAL_COND_INIT(cond)        Scm__InternalCondInit(&(cond))
#define SCM_INTERNAL_COND_SIGNAL(cond)      Scm__InternalCondSignal(&(cond))
#define SCM_INTERNAL_COND_BROADCAST(cond)   Scm__InternalCondBroadcast(&(cond))
#define SCM_INTERNAL_COND_WAIT(cond, mutex) \
    Scm__InternalCondWait(&(cond), &(mutex), NULL)
#define SCM_INTERNAL_COND_TIMEDWAIT(cond, mutex, ptimespec) \
    Scm__InternalCondWait(&(cond), &(mutex), (ptimespec))
#define SCM_INTERNAL_COND_DESTROY(cond)     Scm__InternalCondDestroy(&(cond))
#define SCM_INTERNAL_COND_INITIALIZER       {0}
#define SCM_INTERNAL_COND_TIMEDOUT          1
#define SCM_INTERNAL_COND_INTR              2

/* alternative timespec.  the definition is in system.h  */
#if defined(__MINGW64_VERSION_MAJOR)
#define ScmTimeSpecRec timespec
#else  /*!defined(__MINGW64_VERSION_MAJOR)*/
struct ScmTimeSpecRec;
#endif /*!defined(__MINGW64_VERSION_MAJOR)*/

SCM_EXTERN void Scm__InternalCondInit(ScmInternalCond *cond);
SCM_EXTERN int  Scm__InternalCondSignal(ScmInternalCond *cond);
SCM_EXTERN int  Scm__InternalCondBroadcast(ScmInternalCond *cond);
SCM_EXTERN int  Scm__InternalCondWait(ScmInternalCond *cond,
                                      ScmInternalMutex *mutex,
                                      struct ScmTimeSpecRec *pts);
SCM_EXTERN void Scm__InternalCondDestroy(ScmInternalCond *cond);

#if defined(__MINGW64_VERSION_MAJOR_)
#undef ScmTimeSpecRec
#endif /*defined(__MINGW64_VERSION_MAJOR)*/

/* We don't provide fast lock */
typedef HANDLE ScmInternalFastlock;
#define SCM_INTERNAL_FASTLOCK_INIT(fl)   SCM_INTERNAL_MUTEX_INIT(fl)
#define SCM_INTERNAL_FASTLOCK_LOCK(fl)   SCM_INTERNAL_MUTEX_LOCK(fl)
#define SCM_INTERNAL_FASTLOCK_UNLOCK(fl) SCM_INTERNAL_MUTEX_UNLOCK(fl)
#define SCM_INTERNAL_FASTLOCK_DESTROY(fl) SCM_INTERNAL_MUTEX_DESTROY(fl)

/* Issues a full memory barrier */
#if 0        /* MinGW doesn't seem to have MemoryBarrier() */
#define SCM_INTERNAL_SYNC()                 MemoryBarrier()
#else
#define SCM_INTERNAL_SYNC()                     \
    do {                                        \
        long dummy = 0;                         \
        InterlockedExchange(&dummy, 1);         \
    } while (0)
#endif

#endif /* GAUCHE_WTHREAD_H */
