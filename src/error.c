/*
 * error.c - error handling
 *
 *   Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
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
 *  $Id: error.c,v 1.44 2004-02-03 13:12:28 shirok Exp $
 */

#include <errno.h>
#include <string.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"
#include "gauche/exception.h"
#include "gauche/vm.h"
#include "gauche/builtin-syms.h"

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

/*
 * Constructors
 */

ScmObj Scm_MakeThreadException(ScmClass *klass, ScmVM *thread)
{
    ScmThreadException *e = SCM_NEW(ScmThreadException);
    SCM_SET_CLASS(e, klass);
    e->thread = thread;
    e->data = SCM_UNDEFINED;
    return SCM_OBJ(e);
}

ScmObj Scm_MakeError(ScmObj message)
{
    ScmError *e = SCM_NEW(ScmError);
    SCM_SET_CLASS(e, SCM_CLASS_ERROR);
    e->message = message;
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

/*
 * Initializing
 */

extern void Scm_Init_exclib(ScmModule *module);

void Scm__InitExceptions(void)
{
    ScmModule *mod = Scm_GaucheModule();
    Scm_Init_exclib(mod);
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
    ScmVM *vm = Scm_VM();
    va_list args;

    if (SCM_VM_RUNTIME_FLAG_IS_SET(vm, SCM_ERROR_BEING_HANDLED)) {
        e = Scm_MakeError(SCM_MAKE_STR("Error occurred in error handler"));
        Scm_VMThrowException(e);
    }
    SCM_VM_RUNTIME_FLAG_SET(vm, SCM_ERROR_BEING_HANDLED);
    
    SCM_UNWIND_PROTECT {
        ScmObj ostr = Scm_MakeOutputStringPort(TRUE);
        va_start(args, msg);
        Scm_Vprintf(SCM_PORT(ostr), msg, args, TRUE);
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
        ScmObj ostr = Scm_MakeOutputStringPort(TRUE);
        va_start(args, msg);
        Scm_Vprintf(SCM_PORT(ostr), msg, args, TRUE);
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
 * Just print warning
 *  TODO: customize behavior
 */

void Scm_Warn(const char *msg, ...)
{
    va_list args;
    ScmObj ostr = Scm_MakeOutputStringPort(TRUE);
    va_start(args, msg);
    Scm_Vprintf(SCM_PORT(ostr), msg, args, TRUE);
    va_end(args);
    Scm_Printf(SCM_CURERR, "WARNING: %A\n", Scm_GetOutputString(SCM_PORT(ostr)));
    Scm_Flush(SCM_CURERR);
}

/*
 * Those versions are called from Scheme.  Do not use them from C.
 */

/* SRFI-23 compatible error.
   We tolerate reason not to be a string.  It is not encouraged,
   but some third-party code may use different error API, and
   signaling an error as the first arg isn't a string would obscure
   the problem. */
ScmObj Scm_SError(ScmObj reason, ScmObj args)
{
    volatile ScmObj e;

    SCM_UNWIND_PROTECT {
        ScmObj ostr = Scm_MakeOutputStringPort(TRUE);
        ScmObj ap;
        Scm_Write(reason, ostr, SCM_WRITE_DISPLAY);
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
        ScmObj ostr = Scm_MakeOutputStringPort(TRUE);
        if (SCM_STRINGP(fmt)) {
            Scm_Format(SCM_PORT(ostr), SCM_STRING(fmt), args, TRUE);
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

/* format & warn */
void Scm_FWarn(ScmString *fmt, ScmObj args)
{
    ScmObj ostr = Scm_MakeOutputStringPort(TRUE);
    Scm_Format(SCM_PORT(ostr), fmt, args, TRUE);
    Scm_Printf(SCM_CURERR, "WARNING: %A\n", Scm_GetOutputString(SCM_PORT(ostr)));
    Scm_Flush(SCM_CURERR);
}

/*
 * Default error reporter
 */

#define STACK_DEPTH_LIMIT 30

static void report_error_inner(ScmVM *vm, ScmObj e)
{
    ScmObj stack = Scm_VMGetStackLite(vm), cp;
    ScmPort *err = SCM_VM_CURRENT_ERROR_PORT(vm);
    int depth = 0;

    if (SCM_ERRORP(e) && SCM_STRINGP(SCM_ERROR_MESSAGE(e))) {
        SCM_PUTZ("*** ERROR: ", -1, err);
        SCM_PUTS(SCM_STRING(SCM_ERROR_MESSAGE(e)), err);
        SCM_PUTNL(err);
    } else {
        SCM_PUTZ("*** ERROR: unhandled exception: ", -1, err);
        Scm_Printf(SCM_PORT(err), "%S\n", e);
    }
    SCM_PUTZ("Stack Trace:\n", -1, err);
    SCM_PUTZ("_______________________________________\n", -1, err);
    SCM_FOR_EACH(cp, stack) {
        Scm_Printf(SCM_PORT(err), "%3d  %66.1S\n", depth++, SCM_CAR(cp));
        if (SCM_PAIRP(SCM_CAR(cp))) {
            ScmObj srci = Scm_PairAttrGet(SCM_PAIR(SCM_CAR(cp)),
                                          SCM_SYM_SOURCE_INFO, SCM_FALSE);
            if (SCM_PAIRP(srci) && SCM_PAIRP(SCM_CDR(srci))) {
                Scm_Printf(SCM_PORT(err), "        At line %S of %S\n",
                           SCM_CADR(srci), SCM_CAR(srci));
            } else {
                Scm_Printf(SCM_PORT(err), "        [unknown location]\n");
            }
        } else {
            Scm_Printf(SCM_PORT(err), "\n");
        }
        if (depth >= STACK_DEPTH_LIMIT) {
            Scm_Printf(SCM_PORT(err), "... (more stack dump truncated)\n");
            break;
        }
    }
    /* NB: stderr is autoflushed by default, but in case err is replaced
       by some other port, we explicitly flush it. */
    SCM_FLUSH(err);
}

void Scm_ReportError(ScmObj e)
{
    ScmVM *vm = Scm_VM();

    if (SCM_VM_RUNTIME_FLAG_IS_SET(vm, SCM_ERROR_BEING_REPORTED)) {
        /* An _uncaptured_ error occurred during reporting an error.
           We can't proceed, for it will cause infinite loop.
           Note that it is OK for an error to occur inside the error
           reporter, as far as the error is handled by user-installed
           handler.   The user-installed handler can even invoke a
           continuation that is captured outside; the flag is reset
           in such case. 
           Be careful that it is possible that stderr is no longer
           available here (since it may be the very cause of the
           recursive error).  All we can do is to abort. */
        Scm_Abort("Unhandled error occurred during reporting an error.  Process aborted.\n");
    }

    SCM_VM_RUNTIME_FLAG_SET(vm, SCM_ERROR_BEING_REPORTED);
    SCM_UNWIND_PROTECT {
        if (SCM_PROCEDUREP(vm->defaultEscapeHandler)) {
            Scm_Apply(vm->defaultEscapeHandler, SCM_LIST1(e));
        } else {
            report_error_inner(vm, e);
        }
    }
    SCM_WHEN_ERROR {
        /* NB: this is called when a continuation captured outside is
           invoked inside the error reporter.   It may be invoked by
           the user's error handler.  */
        SCM_VM_RUNTIME_FLAG_CLEAR(vm, SCM_ERROR_BEING_REPORTED);
    }
    SCM_END_PROTECT;
    SCM_VM_RUNTIME_FLAG_CLEAR(vm, SCM_ERROR_BEING_REPORTED);
}

