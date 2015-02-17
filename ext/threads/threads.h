/*
 * threads.h - Gauche threads support
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

#ifndef GAUCHE_THREADS_H
#define GAUCHE_THREADS_H

#if defined(EXTTHREADS_EXPORTS)
#define LIBGAUCHE_EXT_BODY
#endif
#include <gauche/extern.h>      /* redefine SCM_EXTERN */

/*---------------------------------------------------------
 * Scheme-level thread API
 */

extern ScmObj Scm_MakeThread(ScmProcedure *thunk, ScmObj name);
extern ScmObj Scm_ThreadStart(ScmVM *vm);
extern ScmObj Scm_ThreadJoin(ScmVM *vm, ScmObj timeout, ScmObj timeoutval);
extern ScmObj Scm_ThreadStop(ScmVM *vm, ScmObj timeout, ScmObj timeoutval);
extern ScmObj Scm_ThreadCont(ScmVM *vm);
extern ScmObj Scm_ThreadSleep(ScmObj timeout);
extern ScmObj Scm_ThreadTerminate(ScmVM *vm);

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

ScmObj Scm_MakeConditionVariable(ScmObj name);
ScmObj Scm_ConditionVariableSignal(ScmConditionVariable *cond);
ScmObj Scm_ConditionVariableBroadcast(ScmConditionVariable *cond);

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

ScmObj Scm_MakeMutex(ScmObj name);
ScmObj Scm_MutexLock(ScmMutex *mutex, ScmObj timeout, ScmVM *owner);
ScmObj Scm_MutexUnlock(ScmMutex *mutex, ScmConditionVariable *cv, ScmObj timeout);
ScmObj Scm_MutexLocker(ScmMutex *mutex);
ScmObj Scm_MutexUnlocker(ScmMutex *mutex);

/*
 * Scheme reader/writer lock.
 */
typedef struct ScmRWLockRec {
    SCM_HEADER;
    ScmInternalMutex mutex;
    ScmInternalCond cond;
    ScmObj name;
    ScmObj specific;
    int numReader;
    int numWriter;
} ScmRWLock;

SCM_CLASS_DECL(Scm_RWLockClass);
#define SCM_CLASS_RWLOCK       (&Scm_RWLockClass)
#define SCM_RWLOCK(obj)        ((ScmRWLock*)obj)
#define SCM_RWLOCKP(obj)       SCM_XTYPEP(obj, SCM_CLASS_RWLOCK)

ScmObj Scm_MakeRWLock(ScmObj name);


#endif /*GAUCHE_THREADS_H*/
