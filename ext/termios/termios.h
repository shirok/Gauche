/*
 * termios.h - termios interface
 *
 *  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: termios.h,v 1.1 2001-09-07 11:35:30 shirok Exp $
 */

#ifndef GAUCHE_TERMIOS_H
#define GAUCHE_TERMIOS_H

#include <gauche.h>
#include <termios.h>

typedef struct ScmSysTermiosRec {
    SCM_HEADER;
    struct termios term;
} ScmSysTermios;

extern ScmClass Scm_SysTermiosClass;
#define SCM_CLASS_SYS_TERMIOS   (&Scm_SysTermiosClass)
#define SCM_SYS_TERMIOS(obj)    ((ScmSysTermios*)(obj))
#define SCM_SYS_TERMIOS_P(obj)  (SCM_XTYPEP(obj, SCM_CLASS_SYS_TERMIOS))

extern ScmObj Scm_MakeSysTermios(void);

#endif /* GAUCHE_TERMIOS_H */
