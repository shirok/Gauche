/*
 * fcntl.h - fcntl interface
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
 *  $Id: fcntl.h,v 1.2 2002-02-08 09:10:57 shirok Exp $
 */

#ifndef GAUCHE_FCNTL_H
#define GAUCHE_FCNTL_H

#include <gauche.h>
#include <fcntl.h>

typedef struct ScmSysFlockRec {
    SCM_HEADER;
    struct flock lock;
} ScmSysFlock;

SCM_CLASS_DECL(Scm_SysFlockClass);
#define SCM_CLASS_SYS_FLOCK   (&Scm_SysFlockClass)
#define SCM_SYS_FLOCK(obj)    ((ScmSysFlock*)(obj))
#define SCM_SYS_FLOCK_P(obj)  (SCM_XTYPEP(obj, SCM_CLASS_SYS_FLOCK))

extern ScmObj Scm_MakeSysFlock(void);
extern ScmObj Scm_SysFcntl(ScmObj port_or_fd, int op, ScmObj arg);

#endif /* GAUCHE_FCNTL_H */
