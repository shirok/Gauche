/*
 * exception.h - more exception classes
 *
 *   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
 *
 *  $Id: exception.h,v 1.4 2003-07-05 03:29:13 shirok Exp $
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
#define SCM_CLASS_THREAD_EXCEPTION  (&Scm_ThreadExceptionClass)
#define SCM_THREAD_EXCEPTION_P(obj) SCM_ISA(obj, SCM_CLASS_THREAD_EXCEPTION)
#define SCM_THREAD_EXCEPTION(obj)   ((ScmThreadException*)(obj))

SCM_CLASS_DECL(Scm_JoinTimeoutExceptionClass);
#define SCM_CLASS_JOIN_TIMEOUT_EXCEPTION (&Scm_JoinTimeoutExceptionClass)
#define SCM_JOIN_TIMEOUT_EXCEPTION_P     SCM_ISA(obj, SCM_CLASS_JOIN_TIMEOUT_EXCEPTION)

SCM_CLASS_DECL(Scm_AbandonedMutexExceptionClass);
#define SCM_CLASS_ABANDONED_MUTEX_EXCEPTION (&Scm_AbandonedMutexExceptionClass)
#define SCM_ABANDONED_MUTEX_EXCEPTION_P     SCM_ISA(obj, SCM_CLASS_ABANDONED_MUTEX_EXCEPTION)

SCM_CLASS_DECL(Scm_TerminatedThreadExceptionClass);
#define SCM_CLASS_TERMINATED_THREAD_EXCEPTION (&Scm_TerminatedThreadExceptionClass)
#define SCM_TERMINATED_THREAD_EXCEPTION_P     SCM_ISA(obj, SCM_CLASS_TERMINATED_THREAD_EXCEPTION)

SCM_CLASS_DECL(Scm_UncaughtExceptionClass);
#define SCM_CLASS_UNCAUGHT_EXCEPTION (&Scm_UncaughtExceptionClass)
#define SCM_UNCAUGHT_EXCEPTION_P     SCM_ISA(obj, SCM_CLASS_UNCAUGHT_EXCEPTION)

SCM_EXTERN ScmObj Scm_MakeThreadException(ScmClass*, ScmVM*);

#endif /*GAUCHE_EXCEPTION_H*/
