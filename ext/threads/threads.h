/*
 * threads.h - Gauche threads support
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
 *  $Id: threads.h,v 1.1 2002-07-15 10:54:45 shirok Exp $
 */

#ifndef GAUCHE_THREADS_H
#define GAUCHE_THREADS_H

/*---------------------------------------------------------
 * Scheme-level thread API
 */

extern ScmObj Scm_MakeThread(ScmProcedure *thunk, ScmObj name);
extern ScmObj Scm_ThreadStart(ScmVM *vm);
extern ScmObj Scm_ThreadJoin(ScmVM *vm, ScmObj timeout, ScmObj timeoutval);
extern ScmObj Scm_ThreadYield(void);
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
    SCM_HEADER;
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
    SCM_HEADER;
    ScmInternalMutex mutex;
    ScmInternalCond  cv;
    ScmObj name;
    ScmObj specific;
    int   locked;
    ScmVM *owner;              /* the thread who owns this lock; may be NULL */
} ScmMutex;

SCM_CLASS_DECL(Scm_MutexClass);
#define SCM_CLASS_MUTEX        (&Scm_MutexClass)
#define SCM_MUTEX(obj)         ((ScmMutex*)obj)
#define SCM_MUTEXP(obj)        SCM_XTYPEP(obj, SCM_CLASS_MUTEX)

ScmObj Scm_MakeMutex(ScmObj name);
ScmObj Scm_MutexLock(ScmMutex *mutex, ScmObj timeout, ScmVM *owner);
ScmObj Scm_MutexUnlock(ScmMutex *mutex, ScmConditionVariable *cv, ScmObj timeout);

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
