/*
 * Adaptor for atomic operations
 *
 * If GC uses libatomic_ops, we also use it; otherwise, we use C11 intrinsics.
 *
 * Two typedefs are provided:
 *
 *  ScmAtomicWord - A type of a value that can be used for atomic operations.
 *  ScmAtomicVar  - Location must be declared with this type to be operated
 *                  on atomically.
 *
 * APIs
 *  Scm_AtomicLoad(ScmAtomicVar *loc) -> ScmAtomicWord
 *     Load the value atomically.
 *  Scm_AtomicStore(ScmAtomicVar *loc, ScmAtomicWord) -> void
 *     Store the value atomically.
 *  Scm_AtomicStoreFull(ScmAtomicVar *loc, ScmAtomicWord) -> void
 *     Store the value atomically and perform memory barrier.
 *  Scm_AtomicCompareExchange(ScmAtomicVar *loc, ScmAtomicWord *expected,
 *                            ScmAtomicWord newval) -> bool
 *     Compare *expected and *loc, and if they match, store newval to *loc.
 *     Otherwise, the value of *loc is stored in *expected.
 *     Returns TRUE if *loc is updated, FALSE if not.
 *  Scm_AtomicExchange(ScmAtomicVar *loc, ScmAtomicWord newval) -> ScmAtomicWord
 *     Set newval to *loc, and returns the previous value of *loc
 *  Scm_AtomicThreadFence() -> void
 *     Synchronize memory.
 */

#ifndef GAUCHE_PRIV_ATOMICP_H
#define GAUCHE_PRIV_ATOMICP_H

#if GC_BUILTIN_ATOMIC

#  if HAVE_STDATOMIC_H
/*
 * C11 atomics
 */
#include <stdatomic.h>

typedef ScmWord ScmAtomicWord;
typedef volatile _Atomic ScmAtomicWord ScmAtomicVar;

#define Scm_AtomicStore(loc, val)     atomic_store(loc, val)
#define Scm_AtomicStoreFull(loc, val) atomic_store(loc, val)
#define Scm_AtomicLoad(loc)           atomic_load(loc)
#define Scm_AtomicCompareExchange(loc, expectedloc, newval)  \
    atomic_compare_exchange_strong(loc, expectedloc, newval)
#define Scm_AtomicExchange(loc, newval) atomic_exchange(loc, newval)
#define Scm_AtomicThreadFence()       atomic_thread_fence(__ATOMIC_SEQ_CST)

#  else /* GC_BUILTIN_ATOMIC && !HAVE_STDATOMIC_H */
/*
 * If GC_BUILTIN_ATOMIC, we know __atomic_* primitives are available
 */

typedef ScmWord ScmAtomicWord;
typedef volatile ScmAtomicWord ScmAtomicVar;


#define Scm_AtomicStore(loc, val)     __atomic_store_n(loc, val, __ATOMIC_SEQ_CST)
#define Scm_AtomicStoreFull(loc, val) __atomic_store_n(loc, val, __ATOMIC_SEQ_CST)
#define Scm_AtomicLoad(loc)           __atomic_load(loc, __ATOMIC_SEQ_CST)
#define Scm_AtomicCompareExchange(loc, expectedloc, newval) \
    __atomic_compare_exchange_n(loc, expectedloc, newval, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)
#define Scm_AtomicExchange(loc, newval) __atomic_exchange_n(loc, newval, __ATOMIC_SEQ_CST)
#define Scm_AtomicThreadFence()       __atomic_thread_fence(__ATOMIC_SEQ_CST)

#  endif /* GC_BUILTIN_ATOMIC && !HAVE_STDATOMIC_H */

#else /* !GC_BUILTIN_ATOMIC */
/*
 * Use libatomic_ops
 */


/* Workaround for sh4 */
/*
 * ABI wise, I'd say SuperH is difficult to support, and libatomic_ops
 * is not well supported.
 *
 * I believe that using gUSA, we could improve libatomic_ops
 * implementation for SH-4 (compare_and_swap, etc.).  But, those
 * are incompatible to SH-4A, SMP machines.
 *
 * I believe that SH-4A, architecture wise, breaks SH-4 ABI already.
 *
 * If SH-4A machine insists as if it were SH4 (ABI), we can't use
 * gUSA, nor ll/sc equivalents (movli.l/movco.l, IIRC), either.
 * That's totally a mess.
 *
 * Only workaround for both of SH-4 and SH-4A is to downgrade to
 * pthread implementation, so that it will work reliably.
 *
 * -- gniibe  2012-11-27
 *
 */
#if defined(__SH4__)
#define AO_USE_PTHREAD_DEFS 1
#endif
/* Workaround for armel */
/*
 * It is unfortunate that libatomic_ops is not well supported
 * for ARM architectures.  It could be understandable as there
 * are so many variants in "ARM".
 *
 * For __ARMEL__ (which means ARM_ARCH_4T, in Debian), there is no
 * hardware support for atomic operations, unfortunately.
 *
 * NOTE:
 * It is ARMv6 which introduced LDREX/STREX (exclusives).
 * It is ARMv7 which introduced DMB/DSB instructions (memory barrier).
 *
 *      -- gniibe  2012-11-27
 */
#if defined(__ARMEL__)
#define AO_USE_PTHREAD_DEFS 1
#endif

#include "atomic_ops.h"

typedef AO_t ScmAtomicWord;
typedef volatile AO_t ScmAtomicVar;

#define Scm_AtomicStore(loc, val)     AO_store(loc, val)
#define Scm_AtomicStoreFull(loc, val) AO_store_full(loc, val)
#define Scm_AtomicLoad(loc)           AO_load(loc)

#if defined(__GNUC__)
#  define Scm_AtomicCompareExchange(loc, expectedloc, newval)           \
    ({                                                                  \
        AO_t expected__ = *expectedloc;                                 \
        AO_t oldval__ =                                                 \
            AO_fetch_compare_and_swap_full(loc, expected__, newval);    \
        *expectedloc = oldval__;                                        \
        expected__ == oldval__;                                         \
      })

#  define Scm_AtomicExchange(loc, newval)                               \
    ({                                                                  \
        AO_t current__;                                                 \
        do {                                                            \
            current__ = *loc;                                           \
        } while (AO_compare_and_swap_full(loc, current__, newval));     \
      })

#else /*!__GNUC__*/
#  define Scm_AtomicCompareExchange(loc, expectedloc, newval) \
    Scm__AtomicCompareExchange(loc, expectedloc, newval)
#  define Scm_AtomicExchange(loc, newval) \
    Scm__AtomicExchange(loc, newval)

#  define SCM_ATOMIC_NEED_HELPER 1
extern int Scm__AtomicCompareExchange(ScmAtomicVar *loc,
                                      ScmAtomicWord *expectedloc,
                                      ScmAtomicWord newval);
extern int Scm__AtomicExchange(ScmAtomicVar *loc, ScmAtomicWord newval);

#endif /*!__GNUC__*/




#define Scm_AtomicThreadFence()       AO_nop_full()

#endif

#endif /*GAUCHE_PRIV_ATOMICP_H*/
