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
 *  $Id: error.c,v 1.15 2001-06-30 09:42:38 shirok Exp $
 */

#include <errno.h>
#include <string.h>
#include "gauche.h"

/*
 * Class stuff
 */

static void exception_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<exception %p:%30.0S>", obj, SCM_EXCEPTION_DATA(obj));
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_ExceptionClass, exception_print);

/*
 * Constructor
 */

ScmObj Scm_MakeException(int continuable, ScmObj data)
{
    ScmException *e = SCM_NEW(ScmException);
    SCM_SET_CLASS(e, SCM_CLASS_EXCEPTION);
    e->continuable = continuable;
    e->data = data;
    return SCM_OBJ(e);
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
        e = Scm_MakeException(FALSE,
                              SCM_MAKE_STR("Error occurred in error handler"));
        Scm_VMThrowException(e);
    }
    Scm_VM()->errorFlags |= SCM_ERROR_BEING_HANDLED;
    
    SCM_PUSH_ERROR_HANDLER {
        ScmObj ostr = Scm_MakeOutputStringPort();
        va_start(args, msg);
        Scm_Vprintf(SCM_PORT(ostr), msg, args);
        va_end(args);
        e = Scm_MakeException(FALSE, Scm_GetOutputString(SCM_PORT(ostr)));
    }
    SCM_WHEN_ERROR {
        e = Scm_MakeException(FALSE,
                              SCM_MAKE_STR("Error occurred in error handler"));
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
    ScmObj syserr = SCM_MAKE_STR_COPYING(strerror(errno));
    
    SCM_PUSH_ERROR_HANDLER {
        ScmObj ostr = Scm_MakeOutputStringPort();
        va_start(args, msg);
        Scm_Vprintf(SCM_PORT(ostr), msg, args);
        va_end(args);
        SCM_PUTZ(": ", -1, ostr);
        SCM_PUTS(syserr, ostr);
        e = Scm_MakeException(FALSE, Scm_GetOutputString(SCM_PORT(ostr)));
    }
    SCM_WHEN_ERROR {
        e = Scm_MakeException(FALSE,
                              SCM_MAKE_STR("Error occurred in error handler"));
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
        e = Scm_MakeException(FALSE, Scm_GetOutputString(SCM_PORT(ostr)));
    }
    SCM_WHEN_ERROR {
        e = Scm_MakeException(FALSE,
                              SCM_MAKE_STR("Error occurred in error handler"));
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
        e = Scm_MakeException(FALSE, Scm_GetOutputString(SCM_PORT(ostr)));
    }
    SCM_WHEN_ERROR {
        e = Scm_MakeException(FALSE,
                              SCM_MAKE_STR("Error occurred in error handler"));
    }
    SCM_POP_ERROR_HANDLER;
    return Scm_VMThrowException(e);
}

