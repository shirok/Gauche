/*
 * error.c - error handling
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, ditribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: error.c,v 1.6 2001-02-10 12:42:07 shiro Exp $
 */

#include <errno.h>
#include <string.h>
#include "gauche.h"

/*
 * Class stuff
 */

static int exception_print(ScmObj obj, ScmPort *port, int mode)
{
    return Scm_Printf(port, "#<exception %p:%30.0S>",
                      obj, SCM_EXCEPTION_DATA(obj));
}

SCM_DEFCLASS(Scm_ExceptionClass, "<exception>",
             exception_print, SCM_CLASS_DEFAULT_CPL);

/*
 * Constructor
 */

ScmObj Scm_MakeException(ScmObj data)
{
    ScmException *e = SCM_NEW(ScmException);
    SCM_SET_CLASS(e, SCM_CLASS_EXCEPTION);
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
    
    SCM_PUSH_ERROR_HANDLER {
        ScmObj ostr = Scm_MakeOutputStringPort();
        va_start(args, msg);
        Scm_Vprintf(SCM_PORT(ostr), msg, args);
        va_end(args);
        e = Scm_MakeException(Scm_GetOutputString(SCM_PORT(ostr)));
    }
    SCM_WHEN_ERROR {
        e = Scm_MakeException(SCM_MAKE_STR("Error occurred in error handler"));
    }
    SCM_POP_ERROR_HANDLER;
    Scm_VMThrowException(e);
}

/*
 * Just for convenience to report a system error.   Add strerror() message
 * after the provided message.
 */

void Scm_SysError(const char *msg, ...)
{
    ScmObj e;
    va_list args;
    ScmObj syserr = Scm_MakeString(strerror(errno), -1, -1);
    
    SCM_PUSH_ERROR_HANDLER {
        ScmObj ostr = Scm_MakeOutputStringPort();
        va_start(args, msg);
        Scm_Vprintf(SCM_PORT(ostr), msg, args);
        va_end(args);
        SCM_PUTCSTR(": ", ostr);
        SCM_PUTS(syserr, ostr);
        e = Scm_MakeException(Scm_GetOutputString(SCM_PORT(ostr)));
    }
    SCM_WHEN_ERROR {
        e = Scm_MakeException(SCM_MAKE_STR("Error occurred in error handler"));
    }
    SCM_POP_ERROR_HANDLER;
    Scm_VMThrowException(e);
}

/*
 * The version called from Scheme
 */
ScmObj Scm_SError(ScmObj fmt, ScmObj args)
{
    ScmObj e;

    SCM_PUSH_ERROR_HANDLER {
        ScmObj ostr = Scm_MakeOutputStringPort();
        if (SCM_STRINGP(fmt)) {
            Scm_Format(ostr, SCM_STRING(fmt), args);
        } else {
            /* this shouldn't happen, but we tolerate it. */
            Scm_Write(fmt, ostr, SCM_PRINT_WRITE);
        }
        e = Scm_MakeException(Scm_GetOutputString(SCM_PORT(ostr)));
    }
    SCM_WHEN_ERROR {
        e = Scm_MakeException(SCM_MAKE_STR("Error occurred in error handler"));
    }
    SCM_POP_ERROR_HANDLER;
    return Scm_VMThrowException(e);
}

