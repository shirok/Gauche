/*
 * error.c - error handling
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: error.c,v 1.26 2002-01-02 21:16:12 shirok Exp $
 */

#include <errno.h>
#include <string.h>
#include "gauche.h"
#include "gauche/class.h"

/*-----------------------------------------------------------
 * Exception class hierarchy
 */

static ScmClass *exception_cpl[] = {
    SCM_CLASS_ERROR,
    SCM_CLASS_EXCEPTION,
    SCM_CLASS_TOP,
    NULL
};

/* Exception class is just an abstract class */
SCM_DEFINE_ABSTRACT_CLASS(Scm_ExceptionClass, SCM_CLASS_DEFAULT_CPL);

/* Error class */

static void error_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmClass *k = SCM_CLASS_OF(obj);
    ScmObj name = k->name;
    int n;
    
    SCM_PUTZ("#<", 2, port);
    if (SCM_SYMBOLP(name)
        && SCM_STRING_LENGTH(SCM_SYMBOL_NAME(name)) > 2 
        && SCM_STRING_START(SCM_SYMBOL_NAME(name))[0] == '<') {
            SCM_PUTZ(SCM_STRING_START(SCM_SYMBOL_NAME(name))+1,
                     SCM_STRING_LENGTH(SCM_SYMBOL_NAME(name))-2, port);
    } else {
        Scm_Write(name, SCM_OBJ(port), SCM_WRITE_DISPLAY);
    }
    SCM_PUTZ(" ", 1, port);
    n = Scm_WriteLimited(SCM_ERROR_MESSAGE(obj), SCM_OBJ(port),
                         SCM_WRITE_WRITE, 30);
    if (n < 0) SCM_PUTZ(" ...", 4, port);
    SCM_PUTZ(">", 1, port);
}

static ScmObj error_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj sys_error_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj dom_error_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj read_error_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_ErrorClass,
                         error_print, NULL, NULL,
                         error_allocate,
                         exception_cpl+1);

SCM_DEFINE_BUILTIN_CLASS(Scm_SystemErrorClass,
                         error_print, NULL, NULL,
                         sys_error_allocate,
                         exception_cpl);

SCM_DEFINE_BUILTIN_CLASS(Scm_DomainErrorClass,
                         error_print, NULL, NULL,
                         dom_error_allocate,
                         exception_cpl);

SCM_DEFINE_BUILTIN_CLASS(Scm_ReaderErrorClass,
                         error_print, NULL, NULL,
                         read_error_allocate,
                         exception_cpl);

/*
 * Constructors
 */

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

static ScmObj dom_error_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmDomainError *e = SCM_ALLOCATE(ScmDomainError, klass);
    SCM_SET_CLASS(e, klass);
    e->common.message = SCM_FALSE;
    e->argument = NULL;
    e->value = SCM_FALSE;
    return SCM_OBJ(e);
}

static ScmObj read_error_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmReaderError *e = SCM_ALLOCATE(ScmReaderError, klass);
    SCM_SET_CLASS(e, klass);
    e->common.message = SCM_FALSE;
    e->port = NULL;
    e->line = 0;
    return SCM_OBJ(e);
}

/*
 * Accessor 
 */

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

static ScmObj dom_error_argument_get(ScmDomainError *err)
{
    return SCM_MAKE_STR_IMMUTABLE(err->argument);
}

static void dom_error_argument_set(ScmDomainError *err, ScmObj arg)
{
    if (!SCM_STRINGP(arg)) Scm_Error("string required, but got %S", arg);
    err->argument = Scm_GetStringConst(SCM_STRING(arg));
}

static ScmObj dom_error_value_get(ScmDomainError *err)
{
    return err->value;
}

static void dom_error_value_set(ScmDomainError *err, ScmObj value)
{
    err->value = value;
}

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

static ScmClassStaticSlotSpec dom_error_slots[] = {
    SCM_CLASS_SLOT_SPEC("message",
                        error_message_get,
                        error_message_set),
    SCM_CLASS_SLOT_SPEC("argument",
                        dom_error_argument_get,
                        dom_error_argument_set),
    SCM_CLASS_SLOT_SPEC("value",
                        dom_error_value_get,
                        dom_error_value_set),
    { NULL }
};

/*
 * Initializing
 */

void Scm__InitExceptions(void)
{
    ScmModule *mod = Scm_GaucheModule();
    Scm_InitBuiltinClass(&Scm_ExceptionClass, "<exception>", NULL, 0, mod);
    Scm_InitBuiltinClass(&Scm_ErrorClass, "<error>",
                         error_slots, sizeof(ScmError), mod);
    Scm_InitBuiltinClass(&Scm_SystemErrorClass, "<system-error>",
                         sys_error_slots, sizeof(ScmSystemError), mod);
    Scm_InitBuiltinClass(&Scm_DomainErrorClass, "<domain-error>",
                         dom_error_slots, sizeof(ScmDomainError), mod);
    Scm_InitBuiltinClass(&Scm_ReaderErrorClass, "<reader-error>",
                         NULL, sizeof(ScmReaderError), mod);
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

