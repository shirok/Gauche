/*
 * pthread.h - pthread primitives
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
 *  $Id: pthread.h,v 1.2 2002-03-13 10:45:54 shirok Exp $
 */

#ifndef GAUCHE_PTHREAD_H
#define GAUCHE_PTHREAD_H

#include <pthread.h>

typedef pthread_t       ScmInternalThread;
typedef pthread_mutex_t ScmInternalMutex;

#endif /* GAUCHE_PTHREAD_H */
