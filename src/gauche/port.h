/*
 * gauche/port.h - Port inline macros
 *
 *   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
 *  $Id: port.h,v 1.7 2004-02-02 10:43:37 shirok Exp $
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
      if (!(p->flags&SCM_PORT_PRIVATE)) {                       \
        if (p->lockOwner != vm) {                               \
          (void)SCM_INTERNAL_MUTEX_LOCK(p->mutex);              \
          while (p->lockOwner != NULL) {                        \
            if (p->lockOwner->state == SCM_VM_TERMINATED) {     \
              break;                                            \
            }                                                   \
            (void)SCM_INTERNAL_COND_WAIT(p->cv, p->mutex);      \
          }                                                     \
          p->lockOwner = vm;                                    \
          p->lockCount = 0;       /* for safety */              \
          (void)SCM_INTERNAL_MUTEX_UNLOCK(p->mutex);            \
        } else {                                                \
          p->lockCount++;                                       \
        }                                                       \
      }                                                         \
    } while (0)

/* Assumes the calling thread has the lock */
#define PORT_UNLOCK(p)                                  \
    do {                                                \
      if (!(p->flags&SCM_PORT_PRIVATE)) {               \
        if (--p->lockCount <= 0) {                      \
          p->lockOwner = NULL;                          \
          (void)SCM_INTERNAL_COND_SIGNAL(p->cv);        \
        }                                               \
      }                                                 \
    } while (0) 

#define PORT_SAFE_CALL(p, call)                 \
    do {                                        \
      if (!(p->flags&SCM_PORT_PRIVATE)) {       \
        SCM_UNWIND_PROTECT {                    \
          call;                                 \
        } SCM_WHEN_ERROR {                      \
          PORT_UNLOCK(p);                       \
          SCM_NEXT_HANDLER;                     \
        } SCM_END_PROTECT;                      \
      }                                         \
    } while (0)

#define PORT_LOCKED(p, vm) \
   (((p)->flags&SCM_PORT_PRIVATE)||((p)->lockOwner == (vm)))


