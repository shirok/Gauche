/*
 * portP.h - Port private API
 *
 *   Copyright (c) 2013-2020  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PRIV_PORTP_H
#define GAUCHE_PRIV_PORTP_H

#include "gauche/priv/writerP.h"

/*================================================================
 * Real port structure
 *
 *  ScmPort is actually ScmPortImpl.
 */

/*
 * Regardless of the port type, the port structure caches at most
 * one character, in order to realize `peek-char' (Scheme) or `Ungetc' (C)
 * operation.   'scratch', 'scrcnt', and 'ungotten' fields are used for
 * that purpose, and outside routine shouldn't touch these fields.
 * See portapi.c for the detailed semantics.
 *
 * Supporting custom ports (r6rs, srfi-181) complicates the interaction
 * between an 'ungotten' character and port position.  The position of custom
 * ports can be arbitrary object, and it is opaque outside of the custom
 * port implementation.  So the port layer can't adjust the position.
 * Instead, peek-char/byte has to prefetch the position to remember.
 */

typedef struct ScmPortImplRec {
    SCM_PORT_HEADER;

    char scratch[SCM_CHAR_MAX_BYTES]; /* incomplete buffer */

    ScmChar ungotten;           /* ungotten character.
                                   SCM_CHAR_INVALID if empty. */
    ScmObj savedPos;            /* When we peek-char/byte on custom port,
                                   we need to cache the position. */

    ScmInternalFastlock lock;   /* for port mutex */
    ScmVM *lockOwner;           /* for port mutex; owner of the lock */
    int lockCount;              /* for port mutex; # of recursive locks */

    ScmWriteState *writeState;  /* used internally */

    /* Input counters.  these doesn't take account of ungetting and
       seeking: Ungetting doesn't affect those counters (you can think
       that ungetting are handled above the counting layer).
       Seeking invalidates counters; if you seek, the values of the counters
       become bogus.
       We don't have character counter, since it is difficult to track
       (read-line uses byte read; see Scm_ReadLine in portapi.c).
     */
    ScmSize line;               /* line counter */
    ScmSize bytes;              /* byte counter */

    /* The source or the sink of the port.   Use specialized accessor
       functions to retrieve one of those union members. */
    union {
        ScmPortBuffer buf;      /* buffered port */
        ScmPortInputString istr;
        ScmDString ostr;        /* output string port */
        ScmPortVTable vt;       /* virtual port */
    } src;

    /* Port attibutes.  Use Scm_PortAttr* API to access. */
    ScmObj attrs;
    
} ScmPortImpl;

#define P_(p)   ((ScmPortImpl*)(p))

/*================================================================
 * Some private APIs
 */

SCM_EXTERN void Scm__InstallCodingAwarePortHook(ScmPort *(*)(ScmPort*, const char*));

/* Windows-specific initialization */
#if defined(GAUCHE_WINDOWS)
void Scm__SetupPortsForWindows(int has_console);
#endif /*defined(GAUCHE_WINDOWS)*/

#define PORT_WALKER_P(port) \
    (SCM_PORTP(port) && (SCM_PORT(port)->flags & SCM_PORT_WALKING))

#define PORT_WRITESS_P(port) \
    (SCM_PORTP(port) && (SCM_PORT(port)->flags & SCM_PORT_WRITESS))

#define PORT_RECURSIVE_P(port) \
    (P_(port)->writeState != NULL)

#define PORT_LOCK_OWNER_P(port, vm) \
    (P_(port)->lockOwner == (vm))

/* Internal intreface to retrieve src member.
   For public use, we have Scm_PortBufferStruct() etc. */
#define PORT_BUF(port)     (&P_(port)->src.buf)
#define PORT_ISTR(port)    (&P_(port)->src.istr)
#define PORT_OSTR(port)    (&P_(port)->src.ostr)
#define PORT_VT(port)      (&P_(port)->src.vt)
    
/*================================================================
 * Locking the ports
 *
 *  Since most of the public APIs locks the ports, you don't usually
 *  need to lock the ports by yourself.   The following macros
 *  shouldn't be used casually.
 *
 *  Port locking overhead is critical to the I/O performance.
 *  The following macros are designed carefully so that it minimizes
 *  the call to the system-level lock primitives, under the assumption
 *  that port access never conflicts in the performance critical code.
 *  (It doesn't make much sense for multiple threads to write to the
 *  same port, since the outputs are mixed in unpredictable way---except
 *  a casual debug print to stderr, but I don't believe performance
 *  critical part does that.)
 *
 *  The port's lock state is kept in a single pointer, port->lockOwner.
 *  It points to the owner of the port, or NULL if the port is unlocked.
 *  Unlocking the port is a single atomic operation, port->lockOwner = NULL,
 *  hence PORT_UNLOCK doesn't need mutex to do that.
 *
 *  To lock the port, the thread needs to grab a system-level lock
 *  (spinlock if available, mutex otherwise) to check the lockOwner
 *  pointer.  If the port is locked, the thread yields CPU and
 *  try again later.
 *
 *  It is possible that lockOwner slot changes its value to NULL during
 *  a thread is trying to lock the port, since PORT_UNLOCK doesn't obtain
 *  the system-level lock.  If it happens, the thread trying to lock
 *  the port would wait extra timeslice.  Not a big deal.
 *
 *  Note that we cannot use a condition variable to let the locking thread
 *  wait on it.  If we use CV, unlocking becomes two-step operation
 *  (set lockOwner to NULL, and call cond_signal), so it is no longer
 *  atomic.  We would need to get system-level lock in PORT_UNLOCK as well.
 */

/* Lock a port P.  Can perform recursive lock. */
#define PORT_LOCK(p, vm)                                        \
    do {                                                        \
        if (P_(p)->lockOwner != vm) {                           \
          for (;;) {                                            \
              ScmVM* owner__;                                   \
              (void)SCM_INTERNAL_FASTLOCK_LOCK(P_(p)->lock);    \
              owner__ = P_(p)->lockOwner;                       \
              if (owner__ == NULL                               \
                  || (owner__->state == SCM_VM_TERMINATED)) {   \
                  P_(p)->lockOwner = vm;                        \
                  P_(p)->lockCount = 1;                         \
              }                                                 \
              (void)SCM_INTERNAL_FASTLOCK_UNLOCK(P_(p)->lock);  \
              if (P_(p)->lockOwner == vm) break;                \
              Scm_YieldCPU();                                   \
          }                                                     \
      } else {                                                  \
            P_(p)->lockCount++;                                 \
      }                                                         \
    } while (0)

/* Unlock a port P.  Assumes the calling thread has the lock */
#define PORT_UNLOCK(p)                                  \
    do {                                                \
        if (--P_(p)->lockCount <= 0) {                  \
            SCM_INTERNAL_SYNC();                        \
            P_(p)->lockOwner = NULL;                    \
        } \
    } while (0)

/* Should be used while P is locked by calling thread.
   Evaluate C statement CALL, making sure the port is unlocked in case
   CALL raises an error.
   CLEANUP is a C stmt called no matter CALL succeeds or not.
   TODO: we may be able to utilize SCM_PORT_PRIVATE flag to avoid
   SCM_UNWIND_PROTECT overhead. */
#define PORT_SAFE_CALL(p, call, cleanup)        \
    do {                                        \
       SCM_UNWIND_PROTECT {                     \
           call;                                \
           cleanup;                             \
       } SCM_WHEN_ERROR {                       \
           cleanup;                             \
           PORT_UNLOCK(p);                      \
           SCM_NEXT_HANDLER;                    \
       } SCM_END_PROTECT;                       \
    } while (0)

#define PORT_LOCKED(p, vm) ((P_(p)->lockOwner == (vm)))

/* Should be used in the constructor of private ports.
   Mark the port locked by vm, so that it can be used exclusively by
   the vm. */

#define PORT_PRELOCK(p, vm)                     \
   do {                                         \
       P_(p)->lockOwner = vm;                   \
       P_(p)->lockCount = 1;                    \
   } while (0)


#endif /*GAUCHE_PRIV_PORTP_H*/
