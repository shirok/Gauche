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
 *  $Id: error.c,v 1.17 2001-09-12 10:46:23 shirok Exp $
 */

#include <errno.h>
#include <string.h>
#include "gauche.h"
#include "gauche/class.h"

/*-----------------------------------------------------------
 * Exception class hierarchy
 *
 *  <exception>
 *     +-- <signal>
 *     +-- <error>
 *           +-- <system-error>
 */

static void exception_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
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
    n = Scm_WriteLimited(SCM_EXCEPTION_MESSAGE(obj), SCM_OBJ(port),
                         SCM_WRITE_WRITE, 30);
    if (n < 0) SCM_PUTZ(" ...", 4, port);
    SCM_PUTZ(">", 1, port);
}

static ScmObj exception_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_ExceptionClass,
                         exception_print, NULL, NULL,
                         exception_allocate,
                         SCM_CLASS_DEFAULT_CPL);

static ScmClass *exception_cpl[] = {
    SCM_CLASS_ERROR,
    SCM_CLASS_EXCEPTION,
    SCM_CLASS_TOP,
    NULL
};

SCM_DEFINE_BUILTIN_CLASS(Scm_ErrorClass,
                         exception_print, NULL, NULL,
                         exception_allocate,
                         exception_cpl+1);

SCM_DEFINE_BUILTIN_CLASS(Scm_SignalClass,
                         exception_print, NULL, NULL,
                         exception_allocate,
                         exception_cpl+1);

SCM_DEFINE_BUILTIN_CLASS(Scm_SysErrorClass,
                         exception_print, NULL, NULL,
                         exception_allocate,
                         exception_cpl);

static ScmObj key_message;

/*
 * Constructors
 */

static ScmObj exception_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmException *e;
    ScmObj obj;
    
    int nslots = klass->numInstanceSlots;
    e = SCM_NEW2(ScmException*,
                 sizeof(ScmException) + sizeof(ScmObj)*nslots);
    SCM_SET_CLASS(e, klass);
    e->message = Scm_GetKeyword(key_message, initargs, SCM_FALSE);
    e->data = SCM_FALSE;
    return SCM_OBJ(e);
}

ScmObj Scm_MakeError(ScmObj message)
{
    ScmException *e = SCM_NEW(ScmException);
    SCM_SET_CLASS(e, SCM_CLASS_ERROR);
    e->message = message;
    e->data = SCM_FALSE;
    return SCM_OBJ(e);
}

ScmObj Scm_MakeSysError(ScmObj message, int en)
{
    ScmException *e = SCM_NEW(ScmException);
    SCM_SET_CLASS(e, SCM_CLASS_SYS_ERROR);
    e->message = message;
    e->data = Scm_MakeInteger(en);
    return SCM_OBJ(e);
}

/*
 * Accessor 
 */

static ScmObj exception_message_get(ScmException *exn)
{
    return exn->message;
}

static void exception_message_set(ScmException *exn, ScmObj message)
{
    exn->message = message;
}

static ScmObj sys_error_errno_get(ScmException *exn)
{
    return exn->data;
}

static ScmClassStaticSlotSpec exception_slots[] = {
    SCM_CLASS_SLOT_SPEC("message",
                        exception_message_get,
                        exception_message_set, SCM_FALSE),
    { NULL }
};

static ScmClassStaticSlotSpec sys_error_slots[] = {
    SCM_CLASS_SLOT_SPEC("message",
                        exception_message_get,
                        exception_message_set, SCM_FALSE),
    SCM_CLASS_SLOT_SPEC("errno",
                        sys_error_errno_get,
                        NULL, SCM_FALSE),
    { NULL }
};

/*
 * Initializing
 */

void Scm__InitExceptions(void)
{
    ScmModule *mod = Scm_GaucheModule();
    key_message = SCM_MAKE_KEYWORD("message");
    Scm_InitBuiltinClass(&Scm_ExceptionClass, "<exception>",
                         exception_slots, mod);
    Scm_InitBuiltinClass(&Scm_ErrorClass, "<error>",
                         exception_slots, mod);
    Scm_InitBuiltinClass(&Scm_SysErrorClass, "<sys-error>",
                         sys_error_slots, mod);
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

    if (Scm_VM()->errorFlags & SCM_ERROR_BEING_HANDLED) {
        e = Scm_MakeError(SCM_MAKE_STR("Error occurred in error handler"));
        Scm_VMThrowException(e);
    }
    Scm_VM()->errorFlags |= SCM_ERROR_BEING_HANDLED;
    
    SCM_PUSH_ERROR_HANDLER {
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
    SCM_POP_ERROR_HANDLER;
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
    
    SCM_PUSH_ERROR_HANDLER {
        ScmObj ostr = Scm_MakeOutputStringPort();
        va_start(args, msg);
        Scm_Vprintf(SCM_PORT(ostr), msg, args);
        va_end(args);
        SCM_PUTZ(": ", -1, ostr);
        SCM_PUTS(syserr, ostr);
        e = Scm_MakeSysError(Scm_GetOutputString(SCM_PORT(ostr)), en);
    }
    SCM_WHEN_ERROR {
        /* TODO: should check continuation */
        e = Scm_MakeError(SCM_MAKE_STR("Error occurred in error handler"));
    }
    SCM_POP_ERROR_HANDLER;
    Scm_VMThrowException(e);
}

/*
 * Those versions are called from Scheme.  Do not use them from C.
 */

/* SRFI-23 compatible error */
ScmObj Scm_SError(ScmString *reason, ScmObj args)
{
    volatile ScmObj e;

    SCM_PUSH_ERROR_HANDLER {
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
    SCM_POP_ERROR_HANDLER;
    return Scm_VMThrowException(e);
}

/* format & error */
ScmObj Scm_FError(ScmObj fmt, ScmObj args)
{
    volatile ScmObj e;

    SCM_PUSH_ERROR_HANDLER {
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
    SCM_POP_ERROR_HANDLER;
    return Scm_VMThrowException(e);
}

