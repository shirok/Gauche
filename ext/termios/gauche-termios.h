/*
 * termios.h - termios interface
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_TERMIOS_H
#define GAUCHE_TERMIOS_H

#include <gauche.h>

#if !defined(GAUCHE_WINDOWS)

#include <termios.h>

#if   defined(HAVE_PTY_H)
#include <pty.h>
#endif

#if   defined(HAVE_UTIL_H)
#include <util.h>
#endif

/* libutil.h is superseded by bsd/libutil.h */
#if   defined(HAVE_BSD_LIBUTIL_H)
#include <bsd/libutil.h>
#elif defined(HAVE_LIBUTIL_h)
#include <libutil.h>
#endif

#if   defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

#if defined(EXTTERMIOS_EXPORTS)
#define LIBGAUCHE_EXT_BODY
#endif
#include <gauche/extern.h>

typedef struct ScmSysTermiosRec {
    SCM_HEADER;
    struct termios term;
} ScmSysTermios;

SCM_CLASS_DECL(Scm_SysTermiosClass);
#define SCM_CLASS_SYS_TERMIOS   (&Scm_SysTermiosClass)
#define SCM_SYS_TERMIOS(obj)    ((ScmSysTermios*)(obj))
#define SCM_SYS_TERMIOS_P(obj)  (SCM_XTYPEP(obj, SCM_CLASS_SYS_TERMIOS))

ScmObj Scm_MakeSysTermios(void);

#ifdef HAVE_OPENPTY
ScmObj Scm_Openpty(ScmObj slaveterm);
#endif
#ifdef HAVE_FORKPTY
ScmObj Scm_Forkpty(ScmObj slaveterm);
ScmObj Scm_ForkptyAndExec(ScmString *file, ScmObj args, ScmObj iomap,
                          ScmObj slaveterm, ScmSysSigset *mask);
#endif

#endif /* !defined(GAUCHE_WINDOWS) */

void Scm_Init_termios(void);

#endif /* GAUCHE_TERMIOS_H */
