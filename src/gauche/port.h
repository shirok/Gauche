/*
 * gauche/port.h - Port inline macros
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
 *  $Id: port.h,v 1.4 2002-10-12 13:56:01 shirok Exp $
 */

/*
 * Defines several macros that inlines primitive port opertaions.
 * Used in Gauche internally.  Not for general use.
 */

/* Mutex of ports:
 *  SRFI-18 requires the system to serialize access to ports.
 *  I need to implement a few tricks to avoid this requirement
 *  from affecting performance too much.
 *
 *  Each port has a recursive lock, that is, the same thread can
 *  lock the port many times; the port will be fully unlocked
 *  when the thread calls unlock as many times as it called lock.
 *
 *  The port functions may call-back Scheme code or other Gauche
 *  C API that may take arbitrarily long time to execute, and may
 *  raise an error.  It prevents us from using simple mutex; we
 *  need to use CVs, and need to save dynamic context to revert
 *  lock state in case an error is thrown during processing.
 *
 *  If implemented naively it costs too much, since in most
 *  cases the port operation is trivial; such as fetching some
 *  bytes from memory and incrementing a counter.   Only in some
 *  occasions the operation involves system calls or calling other
 *  Gauche C functions.   In the following macro, mutex call is done
 *  only when the caller hasn't been locked the port.
 *
 *  As you see, PORT_UNLOCK doesn't do any mutex ops.  I count on
 *  that the pointer assignment is atomic; if so, the race condition
 *  won't happen.  If not, Boehm GC won't work anyway.
 *
 *  Furthermore, some port functions have 'unsafe' mode, which 
 *  should be called when the caller is sure that the thread
 *  already locked the port.
 */

/* TODO: what if a thread having a lock of the port died before unlocking it?
 * Currently, PORT_LOCK checks if the lock holder is terminated or not.
 * However, the dead thread may have left the port state inconsistent.
 * I need to set the cancellation points carefully...
 */

#define PORT_LOCK(p, vm)                                        \
    do {                                                        \
        if (p->lockOwner != vm) {                               \
            (void)SCM_INTERNAL_MUTEX_LOCK(p->mutex);            \
            while (p->lockOwner != NULL) {                      \
                if (p->lockOwner->state == SCM_VM_TERMINATED) { \
                    break;                                      \
                }                                               \
                (void)SCM_INTERNAL_COND_WAIT(p->cv, p->mutex);  \
            }                                                   \
            p->lockOwner = vm;                                  \
            p->lockCount = 0;       /* for safety */            \
            (void)SCM_INTERNAL_MUTEX_UNLOCK(p->mutex);          \
        } else {                                                \
            p->lockCount++;                                     \
        }                                                       \
    } while (0)

#define PORT_UNLOCK(p)                                  \
    do {                                                \
        if (--p->lockCount <= 0) {                      \
            p->lockOwner = NULL;                        \
            (void)SCM_INTERNAL_COND_SIGNAL(p->cv);      \
        }                                               \
    } while (0) 

#define PORT_SAFE_CALL(p, call)                         \
    do {                                                \
        SCM_UNWIND_PROTECT {                            \
            call;                                       \
        } SCM_WHEN_ERROR {                              \
            PORT_UNLOCK(p);                             \
            SCM_NEXT_HANDLER;                           \
        } SCM_END_PROTECT;                              \
    } while (0)



