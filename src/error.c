/*
 * error.c - error handling
 *
 *  Copyright(C) 2000-2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: error.c,v 1.31 2002-05-19 10:37:07 shirok Exp $
 */

#include <errno.h>
#include <string.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"
#include "gauche/exception.h"

/*-----------------------------------------------------------
 * Exception class hierarchy
 *
 *   SRFI-18 semantics allows exception to be "continuable",
 *   that is, an exception handler may return to the
 *   continuation of the primitive that raised an exception.
 *   One of the useful way to have such exception is just to ignore
 *   certain conditions.  In some cases, however, it is not
 *   desirable for exception handlers to return.
 *
 *   In Gauche, two class <exception> and <error> is defined;
 *   the former can be used for continuable exception.  The latter
 *   can be used for uncontinuable exception.  The Gauche primitive
 *   checks if the exception handler returns on <error> exception,
 *   and handles it properly.  See Scm_VMThrowException.
 *   Note that the user program can throw any Scheme object.
 *   Scheme objects except <error> are treated as continuable exception.
 *
 *   Exception class hierarchy:
 *
 *    <exception>
 *      +- <debug-break>
 *      +- <thread-exception>
 *           +- <join-timeout-exception>
 *           +- <abandoned-mutex-exception>
 *           +- <terminated-thread-exception>
 *           +- <uncaught-exception>
 *    <error>
 *      +- <system-error>
 *      +- <uncontinuable-exception-error>
 */

static ScmObj thread_exception_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj error_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj sys_error_allocate(ScmClass *klass, ScmObj initargs);

static ScmClass *exception_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_ThreadExceptionClass),
    SCM_CLASS_STATIC_PTR(Scm_ExceptionClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

static ScmClass *error_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_ErrorClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

/* Abstract exception classes */
SCM_DEFINE_ABSTRACT_CLASS(Scm_ExceptionClass, SCM_CLASS_DEFAULT_CPL);
SCM_DEFINE_ABSTRACT_CLASS(Scm_ThreadExceptionClass, exception_cpl+1);

/* Thread exceptions */

static void thread_exception_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmClass *k = SCM_CLASS_OF(obj);
    ScmThreadException *exc = SCM_THREAD_EXCEPTION(obj);
    if (SCM_UNDEFINEDP(exc->data)) {
        Scm_Printf(port, "#<%A %S>",
                   Scm__InternalClassName(k),
                   exc->thread);
    } else {
        Scm_Printf(port, "#<%A %S %S>",
                   Scm__InternalClassName(k),
                   exc->thread,
                   exc->data);
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_JoinTimeoutExceptionClass,
                         thread_exception_print, NULL, NULL,
                         thread_exception_allocate,
                         exception_cpl);
SCM_DEFINE_BUILTIN_CLASS(Scm_AbandonedMutexExceptionClass,
                         thread_exception_print, NULL, NULL,
                         thread_exception_allocate,
                         exception_cpl);
SCM_DEFINE_BUILTIN_CLASS(Scm_TerminatedThreadExceptionClass,
                         thread_exception_print, NULL, NULL,
                         thread_exception_allocate,
                         exception_cpl);
SCM_DEFINE_BUILTIN_CLASS(Scm_UncaughtExceptionClass,
                         thread_exception_print, NULL, NULL,
                         thread_exception_allocate,
                         exception_cpl);

/* Error class */

static void error_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmClass *k = SCM_CLASS_OF(obj);
    Scm_Printf(port, "#<%A \"%30.1A\">",
               Scm__InternalClassName(k), SCM_ERROR_MESSAGE(obj));
}

SCM_DEFINE_BUILTIN_CLASS(Scm_ErrorClass,
                         error_print, NULL, NULL,
                         error_allocate,
			 error_cpl+1);

SCM_DEFINE_BUILTIN_CLASS(Scm_SystemErrorClass,
                         error_print, NULL, NULL,
                         sys_error_allocate,
			 error_cpl);

/* Application-exit is not exactly an error, but implemented here. */
static void appexit_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmApplicationExit *e = (ScmApplicationExit*)(obj);
    Scm_Printf(port, "#<application-exit %d>", e->code);
}

static ScmObj appexit_allocate(ScmClass *, ScmObj);

SCM_DEFINE_BUILTIN_CLASS(Scm_ApplicationExitClass,
                         appexit_print, NULL, NULL,
                         appexit_allocate,
			 exception_cpl+1);
                         
/*
 * Constructors
 */

static ScmObj thread_exception_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmThreadException *e = SCM_ALLOCATE(ScmThreadException, klass);
    SCM_SET_CLASS(e, klass);
    e->thread = NULL;
    e->data = SCM_UNDEFINED;
    return SCM_OBJ(e);
}

ScmObj Scm_MakeThreadException(ScmClass *klass, ScmVM *thread)
{
    ScmThreadException *e = (ScmThreadException*)thread_exception_allocate(klass, SCM_NIL);
    e->thread = thread;
    return SCM_OBJ(e);
}

static ScmObj error_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmError *e = SCM_ALLOCATE(ScmError, klass);
    SCM_SET_CLASS(e, klass);
    e->message = SCM_FALSE;
    return SCM_OBJ(e);
}

ScmObj Scm_MakeError(ScmObj message)
{
    ScmError *e = SCM_NEW(ScmError);
    SCM_SET_CLASS(e, SCM_CLASS_ERROR);
    e->message = message;
    return SCM_OBJ(e);
}

static ScmObj sys_error_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmSystemError *e = SCM_ALLOCATE(ScmSystemError, klass);
    SCM_SET_CLASS(e, klass);
    e->common.message = SCM_FALSE;
    e->error_number = 0;
    return SCM_OBJ(e);
}

ScmObj Scm_MakeSystemError(ScmObj message, int en)
{
    ScmSystemError *e = SCM_NEW(ScmSystemError);
    SCM_SET_CLASS(e, SCM_CLASS_SYSTEM_ERROR);
    e->common.message = message;
    e->error_number = en;
    return SCM_OBJ(e);
}

static ScmObj appexit_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmApplicationExit *e = SCM_ALLOCATE(ScmApplicationExit, klass);
    SCM_SET_CLASS(e, klass);
    e->code = 0;
    return SCM_OBJ(e);
}

ScmObj Scm_MakeApplicationExit(int code)
{
    ScmApplicationExit *e = SCM_NEW(ScmApplicationExit);
    SCM_SET_CLASS(e, SCM_CLASS_APPLICATION_EXIT);
    e->code = code;
    return SCM_OBJ(e);
}

/*
 * Accessor 
 */

static ScmObj thread_exception_thread_get(ScmThreadException *exc)
{
    return (exc->thread? SCM_OBJ(exc->thread) : SCM_FALSE);
}

static ScmObj thread_exception_data_get(ScmThreadException *exc)
{
    return exc->data;
}

static ScmObj error_message_get(ScmError *err)
{
    return err->message;
}

static void error_message_set(ScmError *err, ScmObj message)
{
    err->message = message;
}

static ScmObj sys_error_errno_get(ScmSystemError *err)
{
    return Scm_MakeInteger(err->error_number);
}

static void sys_error_errno_set(ScmSystemError *err, ScmObj num)
{
    if (!SCM_INTP(num)) Scm_Error("integer required, but got %S", num);
    err->error_number = SCM_INT_VALUE(num);
}

static ScmObj appexit_code_get(ScmApplicationExit *e)
{
    return Scm_MakeInteger(e->code);
}

static void appexit_code_set(ScmApplicationExit *e, ScmObj num)
{
    if (!SCM_INTP(num)) Scm_Error("integer required, but got %S", num);
    e->code = SCM_INT_VALUE(num);
}


static ScmClassStaticSlotSpec join_timeout_exception_slots[] = {
    SCM_CLASS_SLOT_SPEC("thread", thread_exception_thread_get, NULL),
    { NULL }
};

static ScmClassStaticSlotSpec error_slots[] = {
    SCM_CLASS_SLOT_SPEC("message",
                        error_message_get,
                        error_message_set),
    { NULL }
};

static ScmClassStaticSlotSpec sys_error_slots[] = {
    SCM_CLASS_SLOT_SPEC("message",
                        error_message_get,
                        error_message_set),
    SCM_CLASS_SLOT_SPEC("errno",
                        sys_error_errno_get,
                        sys_error_errno_set),
    { NULL }
};

static ScmClassStaticSlotSpec appexit_slots[] = {
    SCM_CLASS_SLOT_SPEC("code",
                        appexit_code_get,
                        appexit_code_set),
    { NULL }
};


/*
 * Initializing
 */

void Scm__InitExceptions(void)
{
    ScmModule *mod = Scm_GaucheModule();
    Scm_InitBuiltinClass(&Scm_ExceptionClass, "<exception>",
			 NULL, 0, mod);
    Scm_InitBuiltinClass(&Scm_ErrorClass, "<error>",
                         error_slots, sizeof(ScmError), mod);
    Scm_InitBuiltinClass(&Scm_SystemErrorClass, "<system-error>",
                         sys_error_slots, sizeof(ScmSystemError), mod);
    Scm_InitBuiltinClass(&Scm_ApplicationExitClass, "<application-exit>",
                         appexit_slots, sizeof(ScmApplicationExit), mod);
}

/*================================================================
 * Error handling
 *
 *   The interaction with dynamic environment of VM is handled by
 *   Scm_VMThrowException() in vm.c.   These routines provide
 *   application interface.
 */

/*
 * C-like interface
 */

void Scm_Error(const char *msg, ...)
{
    ScmObj e;
    va_list args;

    if (Scm_VM()->runtimeFlags & SCM_ERROR_BEING_HANDLED) {
        e = Scm_MakeError(SCM_MAKE_STR("Error occurred in error handler"));
        Scm_VMThrowException(e);
    }
    Scm_VM()->runtimeFlags |= SCM_ERROR_BEING_HANDLED;
    
    SCM_UNWIND_PROTECT {
        ScmObj ostr = Scm_MakeOutputStringPort();
        va_start(args, msg);
        Scm_Vprintf(SCM_PORT(ostr), msg, args);
        va_end(args);
        e = Scm_MakeError(Scm_GetOutputString(SCM_PORT(ostr)));
    }
    SCM_WHEN_ERROR {
        /* TODO: should check continuation? */
        e = Scm_MakeError(SCM_MAKE_STR("Error occurred in error handler"));
    }
    SCM_END_PROTECT;
    Scm_VMThrowException(e);
    Scm_Panic("Scm_Error: Scm_VMThrowException returned.  something wrong.");
}

/*
 * Just for convenience to report a system error.   Add strerror() message
 * after the provided message.
 */

void Scm_SysError(const char *msg, ...)
{
    ScmObj e;
    va_list args;
    int en = errno;
    ScmObj syserr = SCM_MAKE_STR_COPYING(strerror(en));
    
    SCM_UNWIND_PROTECT {
        ScmObj ostr = Scm_MakeOutputStringPort();
        va_start(args, msg);
        Scm_Vprintf(SCM_PORT(ostr), msg, args);
        va_end(args);
        SCM_PUTZ(": ", -1, ostr);
        SCM_PUTS(syserr, ostr);
        e = Scm_MakeSystemError(Scm_GetOutputString(SCM_PORT(ostr)), en);
    }
    SCM_WHEN_ERROR {
        /* TODO: should check continuation */
        e = Scm_MakeError(SCM_MAKE_STR("Error occurred in error handler"));
    }
    SCM_END_PROTECT;
    Scm_VMThrowException(e);
}

/*
 * Those versions are called from Scheme.  Do not use them from C.
 */

/* SRFI-23 compatible error */
ScmObj Scm_SError(ScmString *reason, ScmObj args)
{
    volatile ScmObj e;

    SCM_UNWIND_PROTECT {
        ScmObj ostr = Scm_MakeOutputStringPort();
        ScmObj ap;
        Scm_Write(SCM_OBJ(reason), ostr, SCM_WRITE_DISPLAY);
        SCM_FOR_EACH(ap, args) {
            SCM_PUTC(' ', ostr);
            Scm_Write(SCM_CAR(ap), ostr, SCM_WRITE_WRITE);
        }
        e = Scm_MakeError(Scm_GetOutputString(SCM_PORT(ostr)));
    }
    SCM_WHEN_ERROR {
        /* TODO: should check continuation? */
        e = Scm_MakeError(SCM_MAKE_STR("Error occurred in error handler"));
    }
    SCM_END_PROTECT;
    return Scm_VMThrowException(e);
}

/* format & error */
ScmObj Scm_FError(ScmObj fmt, ScmObj args)
{
    volatile ScmObj e;

    SCM_UNWIND_PROTECT {
        ScmObj ostr = Scm_MakeOutputStringPort();
        if (SCM_STRINGP(fmt)) {
            Scm_Format(ostr, SCM_STRING(fmt), args);
        } else {
            /* this shouldn't happen, but we tolerate it. */
            Scm_Write(fmt, ostr, SCM_WRITE_WRITE);
        }
        e = Scm_MakeError(Scm_GetOutputString(SCM_PORT(ostr)));
    }
    SCM_WHEN_ERROR {
        /* TODO: should check continuation? */
        e = Scm_MakeError(SCM_MAKE_STR("Error occurred in error handler"));
    }
    SCM_END_PROTECT;
    return Scm_VMThrowException(e);
}

