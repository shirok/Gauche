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
 *  $Id: uthread.h,v 1.5 2002-04-24 23:18:15 shirok Exp $
 */

#ifndef GAUCHE_UTHREAD_H
#define GAUCHE_UTHREAD_H

typedef int ScmInternalMutex;
#define SCM_INTERNAL_MUTEX_INIT(mutex)     (0)
#define SCM_INTERNAL_MUTEX_LOCK(mutex)     (0)
#define SCM_INTERNAL_MUTEX_UNLOCK(mutex)   (0)

typedef int ScmInternalCond;
#define SCM_INTERNAL_COND_INIT(cond)       (0)
#define SCM_INTERNAL_COND_SIGNAL(cond)     (0)
#define SCM_INTERNAL_COND_BROADCAST(cond)  (0)
#define SCM_INTERNAL_COND_WAIT(cond, mutex) (0)
#define SCM_INTERNAL_COND_DESTROY(cond)    (0)

#endif /* GAUCHE_UTHREAD_H */
