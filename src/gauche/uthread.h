/*
 * uthread.h - user level thread primitives
 *
 *  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: uthread.h,v 1.3 2002-03-14 11:20:22 shirok Exp $
 */

#ifndef GAUCHE_UTHREAD_H
#define GAUCHE_UTHREAD_H

typedef int ScmInternalMutex;
#define SCM_INTERNAL_MUTEX_INIT(mutex)     /* empty (for now) */
#define SCM_INTERNAL_MUTEX_LOCK(mutex)     /* empty (for now) */
#define SCM_INTERNAL_MUTEX_UNLOCK(mutex)   /* empty (for now) */

#endif /* GAUCHE_UTHREAD_H */
