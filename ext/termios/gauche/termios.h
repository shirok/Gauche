/*
 * termios.h - termios interface
 *
 *  Copyright(C) 2001-2002 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: termios.h,v 1.1 2003-04-14 10:11:55 shirok Exp $
 */

#ifndef GAUCHE_TERMIOS_H
#define GAUCHE_TERMIOS_H

#include <gauche.h>
#include <termios.h>
#ifdef HAVE_PTY_H
#include <pty.h>
#endif
#ifdef HAVE_UTIL_H
#include <util.h>
#endif
#ifdef HAVE_LIBUTIL_H
#include <libutil.h>
#endif

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
#endif

#endif /* GAUCHE_TERMIOS_H */
