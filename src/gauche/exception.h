/*
 * exception.h - more exception classes
 *
 *  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, disribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: exception.h,v 1.1 2002-05-18 04:08:23 shirok Exp $
 */

#ifndef GAUCHE_EXCEPTION_H
#define GAUCHE_EXCEPTION_H

/* Thread exceptions */
typedef struct ScmThreadExceptionRec {
    SCM_HEADER;
    ScmVM *thread;              /* the thread that caused the exception */
    ScmObj data;                /* additional data.
                                   <join-timeout-exception> : n/a
                                   <abandoned-mutex-exception> : mutex
                                   <terminated-thread-exception> : n/a
                                   <uncaught-exception> : exception
                                */
} ScmThreadException;

SCM_CLASS_DECL(Scm_ThreadExceptionClass);
#define SCM_CLASS_THREAD_EXCEPTION        (&Scm_ThreadExceptionClass)
#define SCM_CLASS_THREAD_EXCEPTION_P(obj) Scm_TypeP(obj, SCM_CLASS_THREAD_EXCEPTION)

SCM_CLASS_DECL(Scm_JoinTimeoutExceptionClass);
#define SCM_CLASS_JOIN_TIMEOUT_EXCEPTION   (&Scm_JoinTimeoutExceptionClass)
#define SCM_CLASS_JOIN_TIMEOUT_EXCEPTION_P Scm_TypeP(obj, SCM_CLASS_JOIN_TIMEOUT_EXCEPTION)

SCM_CLASS_DECL(Scm_AbandonedMutexExceptionClass);
#define SCM_CLASS_ABANDONED_MUTEX_EXCEPTION   (&Scm_AbandonedMutexExceptionClass)
#define SCM_CLASS_ABANDONED_MUTEX_EXCEPTION_P Scm_TypeP(obj, SCM_CLASS_ABANDONED_MUTEX_EXCEPTION)

SCM_CLASS_DECL(Scm_TerminatedThreadExceptionClass);
#define SCM_CLASS_TERMINATED_THREAD_EXCEPTION    (&Scm_TerminatedThreadExceptionClass)
#define SCM_CLASS_TERMINATED_THREAD_EXCEPTION_P  Scm_TypeP(obj, SCM_CLASS_TERMINATED_THREAD_EXCEPTION)

SCM_CLASS_DECL(Scm_UncaughtExceptionClass);
#define SCM_CLASS_UNCAUGHT_EXCEPTION    (&Scm_UncaughtExceptionClass)
#define SCM_CLASS_UNCAUGHT_EXCEPTION_P  Scm_TypeP(obj, SCM_CLASS_UNCAUGHT_EXCEPTION)

SCM_EXTERN ScmObj Scm_MakeThreadException(ScmClass*, ScmVM*);

#endif /*GAUCHE_EXCEPTION_H*/
