/*
 * thread.h - Gauche threads support
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

#ifndef GAUCHE_THREAD_H
#define GAUCHE_THREAD_H

/*---------------------------------------------------------
 * Scheme-level thread API
 */

/* flag for Scm_ThreadTerminate */
enum {
    SCM_THREAD_TERMINATE_FORCIBLE = (1L<<0), /* If graceful termination fails,
                                                take the extreme means to
                                                terminate the thread. */
    SCM_THREAD_TERMINATE_SCHEDULE = (1L<<1), /* Request termination but
                                                do not wait. */
};

/* flag for Scm_ThreadStart */
enum {
    SCM_THREAD_START_TRYSTART = 1
};

SCM_EXTERN ScmObj Scm_MakeThread(ScmProcedure *thunk, ScmObj name);
SCM_EXTERN ScmObj Scm_ThreadStart(ScmVM *vm, u_long flags);
SCM_EXTERN ScmObj Scm_ThreadJoin(ScmVM *vm, ScmObj timeout, ScmObj timeoutval);
SCM_EXTERN ScmObj Scm_ThreadStop(ScmVM *vm, ScmObj timeout, ScmObj timeoutval);
SCM_EXTERN ScmObj Scm_ThreadCont(ScmVM *vm);
SCM_EXTERN ScmObj Scm_ThreadSleep(ScmObj timeout);
SCM_EXTERN ScmObj Scm_ThreadTerminate(ScmVM *vm, u_long flags);

/*---------------------------------------------------------
 * SYNCHRONIZATION DEVICES
 *
 *  Scheme-level synchrnization devices (ScmMutex, ScmConditionVariable,
 *  and ScmRWLock) are built on top of lower-level synchronization devices
 *  (ScmInternalMutex and ScmInternalCond).
 */

/*
 * Scheme condition variable.
 */
typedef struct ScmConditionVariableRec {
    SCM_INSTANCE_HEADER;
    ScmInternalCond cv;
    ScmObj name;
    ScmObj specific;
} ScmConditionVariable;

SCM_CLASS_DECL(Scm_ConditionVariableClass);
#define SCM_CLASS_CONDITION_VARIABLE  (&Scm_ConditionVariableClass)
#define SCM_CONDITION_VARIABLE(obj)   ((ScmConditionVariable*)obj)
#define SCM_CONDITION_VARIABLE_P(obj) SCM_XTYPEP(obj, SCM_CLASS_CONDITION_VARIABLE)

SCM_EXTERN ScmObj Scm_MakeConditionVariable(ScmObj name);
SCM_EXTERN ScmObj Scm_ConditionVariableSignal(ScmConditionVariable *cond);
SCM_EXTERN ScmObj Scm_ConditionVariableBroadcast(ScmConditionVariable *cond);

/*
 * Scheme mutex.
 *    locked=FALSE  owner=dontcare       unlocked/not-abandoned
 *    locked=TRUE   owner=NULL           locked/not-owned
 *    locked=TRUE   owner=active vm      locked/owned
 *    locked=TRUE   owner=terminated vm  unlocked/abandoned
 */
typedef struct ScmMutexRec {
    SCM_INSTANCE_HEADER;
    ScmInternalMutex mutex;
    ScmInternalCond  cv;
    ScmObj name;
    ScmObj specific;
    int   locked;
    ScmVM *owner;              /* the thread who owns this lock; may be NULL */
    ScmObj locker_proc;        /* subr thunk to lock this mutex */
    ScmObj unlocker_proc;      /* subr thunk to unlock this mutex */
} ScmMutex;

SCM_CLASS_DECL(Scm_MutexClass);
#define SCM_CLASS_MUTEX        (&Scm_MutexClass)
#define SCM_MUTEX(obj)         ((ScmMutex*)obj)
#define SCM_MUTEXP(obj)        SCM_XTYPEP(obj, SCM_CLASS_MUTEX)

SCM_EXTERN ScmObj Scm_MakeMutex(ScmObj name);
SCM_EXTERN ScmObj Scm_MutexLock(ScmMutex *mutex, ScmObj timeout, ScmVM *owner);
SCM_EXTERN ScmObj Scm_MutexUnlock(ScmMutex *mutex, ScmConditionVariable *cv, ScmObj timeout);
SCM_EXTERN ScmObj Scm_MutexLocker(ScmMutex *mutex);
SCM_EXTERN ScmObj Scm_MutexUnlocker(ScmMutex *mutex);

#endif /*GAUCHE_THREAD_H*/
