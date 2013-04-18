/*
 * exception.h - more exception classes
 *
 *   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_EXCEPTION_H
#define GAUCHE_EXCEPTION_H

/* Condition class hierarchy

  <condition> ; srfi-35
    +- <compound-condition>
    +- <serious-condition> ; srfi-35
    |    +- <serious-compound-condition> ; also inherits <compound-condition>
    +- <message-condition> ; srfi-35
    |    +- <error>             ; srfi-35, also inherits <serious-condition>
    |         +- <system-error>
    |         +- <unhandled-signal-error>
    |         +- <read-error> ; srfi-36
    |         +- <io-error>   ; srfi-36
    |              +- <port-error> ; srfi-36
    |              |    +- <io-read-error>   ; srfi-36
    |              |    +- <io-write-error>  ; srfi-36
    |              |    +- <io-closed-error> ; srfi-36
    |              |    +- <io-unit-error>
    |              +- <filename-error> ; srfi-36 (*)
    |                   +- <malformed-filename-error>  ; srfi-36 (*)
    |                   +- <file-protection-error>     ; srfi-36 (*)
    |                   |    +- <file-is-read-only-error> ; srfi-36 (*)
    |                   +- <file-already-exists-error> ; srfi-36 (*)
    |                   +- <no-such-file-error>        ; srfi-36 (*)
    +- <thread-exception> ; srfi-18
         +- <join-timeout-exception>      ; srfi-18
         +- <abandoned-mutex-exception>   ; srfi-18
         +- <terminated-thread-exception> ; srfi-18
         +- <uncaught-exception>          ; srfi-18

 (*) - not implemented yet
*/

/*---------------------------------------------------
 * Base conditions.
 */

/* <condition> : root of all condition types. */
typedef ScmInstance ScmCondition;

SCM_CLASS_DECL(Scm_ConditionClass);
#define SCM_CLASS_CONDITION        (&Scm_ConditionClass)
#define SCM_CONDITIONP(obj)        SCM_ISA(obj, SCM_CLASS_CONDITION)

SCM_EXTERN int Scm_ConditionHasType(ScmObj c, ScmObj k);

/* <message-condition> : condition with message. */
typedef struct ScmMessageConditionRec {
    ScmCondition common;
    ScmObj message;             /* message */
} ScmMessageCondition;

SCM_CLASS_DECL(Scm_MessageConditionClass);
#define SCM_CLASS_MESSAGE_CONDITION  (&Scm_MessageConditionClass)
#define SCM_MESSAGE_CONDITION_P(obj) SCM_ISA(obj, SCM_CLASS_MESSAGE_CONDITION)
#define SCM_MESSAGE_CONDITION(obj)   ((ScmMessageCondition*)(obj))

/* <serious-condition> : condition which can't be restarted */
typedef ScmCondition ScmSeriousCondition;

SCM_CLASS_DECL(Scm_SeriousConditionClass);
#define SCM_CLASS_SERIOUS_CONDITION  (&Scm_SeriousConditionClass)
#define SCM_SERIOUS_CONDITION_P(obj) SCM_ISA(obj, SCM_CLASS_SERIOUS_CONDITION)
#define SCM_SERIOUS_CONDITION(obj)   ((ScmSeriousCondition*)(obj))

/*---------------------------------------------------
 * Errors
 */

/* <error>: root of all errors. */
typedef ScmMessageCondition ScmError;

SCM_CLASS_DECL(Scm_ErrorClass);
#define SCM_CLASS_ERROR            (&Scm_ErrorClass)
#define SCM_ERRORP(obj)            SCM_ISA(obj, SCM_CLASS_ERROR)
#define SCM_ERROR(obj)             ((ScmError*)(obj))
#define SCM_ERROR_MESSAGE(obj)     SCM_ERROR(obj)->message

SCM_EXTERN ScmObj Scm_MakeError(ScmObj message);

/* <system-error>: error from system calls */
typedef struct ScmSystemErrorRec {
    ScmError common;
    int error_number;           /* errno */
} ScmSystemError;

SCM_CLASS_DECL(Scm_SystemErrorClass);
#define SCM_CLASS_SYSTEM_ERROR     (&Scm_SystemErrorClass)
#define SCM_SYSTEM_ERROR(obj)      ((ScmSystemError*)(obj))
#define SCM_SYSTEM_ERROR_P(obj)    SCM_ISA(obj, SCM_CLASS_SYSTEM_ERROR)

SCM_EXTERN ScmObj Scm_MakeSystemError(ScmObj message, int error_num);

/* <unhandled-signal-error>: unhandled signal */
typedef struct ScmUnhandledSignalErrorRec {
    ScmError common;
    int signal;                 /* signal number */
} ScmUnhandledSignalError;

SCM_CLASS_DECL(Scm_UnhandledSignalErrorClass);
#define SCM_CLASS_UNHANDLED_SIGNAL_ERROR  (&Scm_UnhandledSignalErrorClass)
#define SCM_UNHANDLED_SIGNAL_ERROR(obj)   ((ScmUnhandledSignalError*)(obj))
#define SCM_UNHANDLED_SIGNAL_ERROR_P(obj) SCM_ISA(obj, SCM_CLASS_UNHANDLED_SIGNAL_ERROR)

/* <read-error>: error from the reader */
typedef struct ScmReadErrorRec {
    ScmError common;
    ScmPort *port;              /* input port where we're reading from. */
    int line;                   /* line number (if available), or -1 */
} ScmReadError;

SCM_CLASS_DECL(Scm_ReadErrorClass);
#define SCM_CLASS_READ_ERROR     (&Scm_ReadErrorClass)
#define SCM_READ_ERROR(obj)      ((ScmReadError*)(obj))
#define SCM_READ_ERROR_P(obj)    SCM_ISA(obj, SCM_CLASS_READ_ERROR)

SCM_EXTERN ScmObj Scm_MakeReadError(ScmObj message, ScmPort *p, int line);

/* <io-error>: abstract class for I/O related error. */
typedef ScmError ScmIOError;

SCM_CLASS_DECL(Scm_IOErrorClass);
#define SCM_CLASS_IO_ERROR       (&Scm_IOErrorClass)
#define SCM_IO_ERROR_P(obj)      SCM_ISA(obj, SCM_CLASS_IO_ERROR)

/* <port-error>: Port related error, inherits <io-error> */
typedef struct ScmPortErrorRec {
    ScmIOError common;
    ScmPort *port;              /* The port where I/O error occurs */
} ScmPortError;

SCM_CLASS_DECL(Scm_PortErrorClass);
#define SCM_CLASS_PORT_ERROR     (&Scm_PortErrorClass)
#define SCM_PORT_ERROR(obj)      ((ScmPortError*)(obj))
#define SCM_PORT_ERROR_P(obj)    SCM_ISA(obj, SCM_CLASS_PORT_ERROR)

/* <io-read-error>, <io-write-error>, <io-closed-error> :
   subclasses of port-error */

typedef ScmPortError ScmIOReadError;
typedef ScmPortError ScmIOWriteError;
typedef ScmPortError ScmIOClosedError;
typedef ScmPortError ScmIOUnitError;

SCM_CLASS_DECL(Scm_IOReadErrorClass);
#define SCM_CLASS_IO_READ_ERROR      (&Scm_IOReadErrorClass)
SCM_CLASS_DECL(Scm_IOWriteErrorClass);
#define SCM_CLASS_IO_WRITE_ERROR     (&Scm_IOWriteErrorClass)
SCM_CLASS_DECL(Scm_IOClosedErrorClass);
#define SCM_CLASS_IO_CLOSED_ERROR    (&Scm_IOClosedErrorClass)
SCM_CLASS_DECL(Scm_IOUnitErrorClass);
#define SCM_CLASS_IO_UNIT_ERROR      (&Scm_IOUnitErrorClass)

/*---------------------------------------------------
 * Compounders
 */

/* compound condition may automatically be an instance of
   <serious-compound-condition> if any of its component
   exception is an instance of <serious-condition>.

   Compound condition never 'nests', i.e. any member of conditions
   isn't a compound condition itself. */

typedef struct ScmCompoundConditionRec {
    ScmCondition common;
    ScmObj conditions;          /* list of simple conditions */
} ScmCompoundCondition;

SCM_CLASS_DECL(Scm_CompoundConditionClass);
#define SCM_CLASS_COMPOUND_CONDITION   (&Scm_CompoundConditionClass)
#define SCM_COMPOUND_CONDITION(obj)    ((ScmCompoundCondition*)(obj))
#define SCM_COMPOUND_CONDITION_P(obj)  SCM_ISA(obj, SCM_CLASS_COMPOUND_CONDITION)

SCM_CLASS_DECL(Scm_SeriousCompoundConditionClass);
#define SCM_CLASS_SERIOUS_COMPOUND_CONDITION (&Scm_SeriousCompoundConditionClass)
#define SCM_SERIOUS_COMPOUND_CONDITION_P(obj) SCM_ISA(obj, SCM_CLASS_SERIOUS_COMPOUND_CONDITION)

SCM_EXTERN ScmObj Scm_MakeCompoundCondition(ScmObj conditions);

/*---------------------------------------------------
 * Thread exceptions
 */

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

SCM_EXTERN ScmObj Scm_MakeThreadException(ScmClass*, ScmVM*);

SCM_CLASS_DECL(Scm_ThreadExceptionClass);
#define SCM_CLASS_THREAD_EXCEPTION  (&Scm_ThreadExceptionClass)
#define SCM_THREAD_EXCEPTION_P(obj) SCM_ISA(obj, SCM_CLASS_THREAD_EXCEPTION)
#define SCM_THREAD_EXCEPTION(obj)   ((ScmThreadException*)(obj))

SCM_CLASS_DECL(Scm_JoinTimeoutExceptionClass);
#define SCM_CLASS_JOIN_TIMEOUT_EXCEPTION (&Scm_JoinTimeoutExceptionClass)
#define SCM_JOIN_TIMEOUT_EXCEPTION_P(obj) SCM_ISA(obj, SCM_CLASS_JOIN_TIMEOUT_EXCEPTION)

SCM_CLASS_DECL(Scm_AbandonedMutexExceptionClass);
#define SCM_CLASS_ABANDONED_MUTEX_EXCEPTION (&Scm_AbandonedMutexExceptionClass)
#define SCM_ABANDONED_MUTEX_EXCEPTION_P(obj) SCM_ISA(obj, SCM_CLASS_ABANDONED_MUTEX_EXCEPTION)

SCM_CLASS_DECL(Scm_TerminatedThreadExceptionClass);
#define SCM_CLASS_TERMINATED_THREAD_EXCEPTION (&Scm_TerminatedThreadExceptionClass)
#define SCM_TERMINATED_THREAD_EXCEPTION_P(obj) SCM_ISA(obj, SCM_CLASS_TERMINATED_THREAD_EXCEPTION)

SCM_CLASS_DECL(Scm_UncaughtExceptionClass);
#define SCM_CLASS_UNCAUGHT_EXCEPTION (&Scm_UncaughtExceptionClass)
#define SCM_UNCAUGHT_EXCEPTION_P(obj) SCM_ISA(obj, SCM_CLASS_UNCAUGHT_EXCEPTION)


#endif /*GAUCHE_EXCEPTION_H*/
