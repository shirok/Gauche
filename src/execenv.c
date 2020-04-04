/*
 * execenv.c - housekeeping execution environment
 *
 *   Copyright (c) 2020  Shiro Kawai  <shiro@acm.org>
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

/* 
 * Sets up various bindings and parameters to access runtime environment
 */


#define LIBGAUCHE_BODY
#include "gauche.h"

static const ScmPrimitiveParameter *command_line = NULL;
static const ScmPrimitiveParameter *os_command_line = NULL;
static const ScmPrimitiveParameter *script_file = NULL;

/*=============================================================
 * Command line arguments
 */

/* 
 * Scm_InitCommandLine is to be called by an application to initialize
 * *program-name*, *argv* and (command-line).  argv[0] becomes *program-name*,
 * and *argv* gets the rest of arguments.
 *
 * Note that this is likely to be called before VM loop starts.
 * Scm_Error isn't much useful yet.  In case of error, we emit warning.
 *
 * NB: `os-command-line' is a provisional name.  We'll adjust it as how
 * command-line srfi comes out.
 */
#if GAUCHE_API_VERSION < 1000
ScmObj Scm_InitCommandLine(int argc, const char *argv[])
{
    return Scm_InitCommandLine2(argc, argv, SCM_COMMAND_LINE_SCRIPT);
}
ScmObj Scm_InitCommandLine2(int argc, const char *argv[], int kind)
#else  /* GAUCHE_API_VERSION >= 1000 */
ScmObj Scm_InitCommandLine(int argc, const char *argv[], int kind)
#endif /* GAUCHE_API_VERSION >= 1000 */
{
    ScmObj args = Scm_CStringArrayToList(argv, argc, SCM_STRING_IMMUTABLE);

    if (kind & SCM_COMMAND_LINE_OS) {
        Scm_PrimitiveParameterSet(Scm_VM(), os_command_line, args);
    }
    if (kind & SCM_COMMAND_LINE_SCRIPT) {
        Scm_PrimitiveParameterSet(Scm_VM(), command_line, args);

        /* For the backward compatibility */
        SCM_DEFINE(Scm_UserModule(), "*program-name*",
                   SCM_NULLP(args)? SCM_FALSE : SCM_CAR(args));
        SCM_DEFINE(Scm_UserModule(), "*argv*",
                   SCM_NULLP(args)? SCM_NIL : SCM_CDR(args));

    }
    return args;
}

/*
 * Initialization
 */

#include "gauche/priv/parameterP.h"

void Scm__InitExecenv(void)
{
    /* (command-line) is R7RS.  We realize it as a a parameter.  */
    command_line = Scm_BindPrimitiveParameter(Scm_GaucheModule(),
                                              "command-line",
                                              SCM_FALSE, 0);
    os_command_line = Scm_BindPrimitiveParameter(Scm_GaucheModule(),
                                                 "os-command-line",
                                                 SCM_NIL, 0);
    /* script-file is set by 'load'.  */
    script_file = Scm_BindPrimitiveParameter(Scm_GaucheModule(),
                                             "script-file",
                                             SCM_FALSE, 0);
}


