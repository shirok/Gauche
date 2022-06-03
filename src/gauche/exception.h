/*
 * exception.h - more exception classes
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
    |                   +- <io-read-error>   ; srfi-36
    |                        +- <io-decoding-error>  ; (srfi-186)
    |                   +- <io-write-error>  ; srfi-36
    |                        +- <io-encoding-error>  ; (srfi-186)
    |                   +- <io-closed-error> ; srfi-36
    |                   +- <io-unit-error>
    |                   +- <io-invalid-position-error> ; srfi-192
    +- <thread-exception> ; srfi-18
    |    +- <join-timeout-exception>      ; srfi-18
    |    +- <abandoned-mutex-exception>   ; srfi-18
    |    +- <terminated-thread-exception> ; srfi-18
    |    +- <uncaught-exception>          ; srfi-18
    +- <mixin-condition>
         +- <load-condition-mixin> ; compounded to an error during loading
         +- <compile-error-mixin>  ; compounded to an error during compiling
         +- <io-filename-error>    ; srfi-36; compounded to <system-error> (*)
             +- <io-malformed-filename-error>     ; ditto (*)
             +- <io-protection-error>             ; ditto (*)
             |    +- <io-file-is-read-only-error> ; ditto (*)
             +- <io-file-already-exists-error>    ; ditto (*)
             +- <io-no-such-file-error>           ; ditto (*)

 SRFI-35 does not make distinction between primary inheritance and mixin
 inheritance; the <mixin-condition> subtree is Gauche's convention.  The
 default error reporting routine treats mixins specially.

 (*) Those classes are defined, but not yet used.
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
/* R7RS requires error condition to store 'message' and 'irritants' separately.
   We can't add fields during 0.9 to keep ABI compatibility.  So, as the
   temporary solution, we clam them into the 'message' fields.  The message
   field now contain a list of one or more elements, as follows:
    (<preformatted-message> <message-prefix> <irritant> ...)
   Where <preformatted-message> is a string and that's what we get
   to access "message" slot of a error condition.  <message-prefix> and
   <irritant>s are just what is passed to "error" procedure:
      (error <message-prefix> <irritant> ...)
   For errorf and the family, it's tricky to separate <irritant>s.  For now
   we preformat the message and treat it as if it was given as the
   <message-prefix>, with no irritants.

   In Scheme level we won't show this structure.  If Scheme code accesses
   the message slot, we return <preformatted-message>.

   C code that access 'message' slot directly needs to be modified.
 */

typedef struct ScmMessageConditionRec {
    ScmCondition common;
    ScmObj message;  /* message - should be accessed via Scm_ConditionMessage */
} ScmMessageCondition;

SCM_CLASS_DECL(Scm_MessageConditionClass);
#define SCM_CLASS_MESSAGE_CONDITION  (&Scm_MessageConditionClass)
#define SCM_MESSAGE_CONDITION_P(obj) SCM_ISA(obj, SCM_CLASS_MESSAGE_CONDITION)
#define SCM_MESSAGE_CONDITION(obj)   ((ScmMessageCondition*)(obj))

/* default 'print' procedure for class definition */
SCM_EXTERN void Scm_MessageConditionPrint(ScmObj obj, ScmPort *port,
                                          ScmWriteContext *ctx);

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
#define SCM_ERROR_MESSAGE(obj)     Scm_ConditionMessage(obj)

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
    ScmObj  auxinfo;            /* alist of auxiliary info, depending on the
                                   subclases.  */
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
typedef ScmPortError ScmIODecodingError;
typedef ScmPortError ScmIOEncodingError;
typedef ScmPortError ScmIOInvalidPositionError;

SCM_CLASS_DECL(Scm_IOReadErrorClass);
#define SCM_CLASS_IO_READ_ERROR      (&Scm_IOReadErrorClass)
SCM_CLASS_DECL(Scm_IOWriteErrorClass);
#define SCM_CLASS_IO_WRITE_ERROR     (&Scm_IOWriteErrorClass)
SCM_CLASS_DECL(Scm_IOClosedErrorClass);
#define SCM_CLASS_IO_CLOSED_ERROR    (&Scm_IOClosedErrorClass)
SCM_CLASS_DECL(Scm_IOUnitErrorClass);
#define SCM_CLASS_IO_UNIT_ERROR      (&Scm_IOUnitErrorClass)

SCM_CLASS_DECL(Scm_IODecodingErrorClass);
#define SCM_CLASS_IO_DECODING_ERROR  (&Scm_IODecodingErrorClass)
SCM_CLASS_DECL(Scm_IOEncodingErrorClass);
#define SCM_CLASS_IO_ENCODING_ERROR  (&Scm_IOEncodingErrorClass)
SCM_CLASS_DECL(Scm_IOInvalidPositionErrorClass);
#define SCM_CLASS_IO_INVALID_POSITION_ERROR  (&Scm_IOInvalidPositionErrorClass)

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
SCM_EXTERN ScmObj Scm_ExtractSimpleCondition(ScmObj condition, ScmClass *type);

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

/*---------------------------------------------------
 * Mixins
 */

/* Mixin conditions are used to be compounded to the main <error> condition
   to add some more context.  It is not a mixin classes in ordinary sense,
   since it won't be used for multiple inheritance; instead, an instance
   of the mixin class is compounded at runtime.  See, for example,
   the 'compile' function in compile.scm mixing <compile-error-mixin>
   into the error raised during compilation.  */

SCM_CLASS_DECL(Scm_MixinConditionClass);
#define SCM_CLASS_MIXIN_CONDITION   (&Scm_MixinConditionClass)
#define SCM_MIXIN_CONDITION_P(obj)  SCM_ISA(obj, SCM_CLASS_MIXIN_CONDITION)

typedef struct ScmLoadConditionMixinRec {
    ScmCondition common;
    ScmObj history;             /* current-load-history */
    ScmObj port;                /* current-load-port */
} ScmLoadConditionMixin;

SCM_CLASS_DECL(Scm_LoadConditionMixinClass);
#define SCM_CLASS_LOAD_CONDITION_MIXIN (&Scm_LoadConditionMixinClass)
#define SCM_LOAD_CONDITION_MIXIN_P(obj) SCM_ISA(obj, SCM_CLASS_LOAD_CONDITION_MIXIN)
#define SCM_LOAD_CONDITION_MIXIN(obj)  ((ScmLoadConditionMixin*)(obj))

typedef struct ScmCompileErrorMixinRec {
    ScmCondition condition;
    ScmObj expr;                /* offending expr */
} ScmCompileErrorMixin;

SCM_CLASS_DECL(Scm_CompileErrorMixinClass);
#define SCM_CLASS_COMPILE_ERROR_MIXIN (&Scm_CompileErrorMixinClass)
#define SCM_COMPILE_ERROR_MIXIN_P(obj) SCM_ISA(obj, SCM_CLASS_COMPILE_ERROR_MIXIN)
#define SCM_COMPILE_ERROR_MIXIN(obj)   ((ScmCompileErrorMixin*)(obj))

/* This struct is shared among all subclasses of &i/o-filename-error. */
typedef struct ScmFilenameErrorMixinRec {
    ScmCondition common;
    ScmObj filename;            /* offending name */
} ScmFilenameErrorMixin;

SCM_CLASS_DECL(Scm_FilenameErrorMixinClass);
#define SCM_CLASS_FILENAME_ERROR_MIXIN (&Scm_FilenameErrorMixinClass)
#define SCM_FILENAME_ERROR_MIXIN_P(obj) SCM_ISA(obj, SCM_CLASS_FILENAME_ERROR_MIXIN)
#define SCM_FILENAME_ERROR_MIXIN(obj)   ((ScmFilenameErrorMixin*)(obj))

SCM_CLASS_DECL(Scm_FilenameErrorMixinClass);
SCM_CLASS_DECL(Scm_MalformedFilenameErrorMixinClass);
SCM_CLASS_DECL(Scm_FileProtectionErrorMixinClass);
SCM_CLASS_DECL(Scm_FileIsReadOnlyErrorMixinClass);
SCM_CLASS_DECL(Scm_FileAlreadyExistsErrorMixinClass);
SCM_CLASS_DECL(Scm_NoSuchFileErrorMixinClass);

#endif /*GAUCHE_EXCEPTION_H*/
