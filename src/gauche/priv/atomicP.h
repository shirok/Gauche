/*
 * Adaptor for libatomic_opts
 *
 * Sources that want to use libatomic_ops features must include this file
 * instead of libatomic_ops.h directly.
 */

#ifndef GAUCHE_PRIV_ATOMICP_H
#define GAUCHE_PRIV_ATOMICP_H

/* Workaround for sh4 */
/*
 * ABI wise, I'd say SuperH is difficult to support, and libatomic_ops
 * is not well supported.
 *
 * I believe that using gUSA, we could improve libatomic_ops
 * implementation for SH-4 (compare_and_swap, etc.).  But, those
 * are imcompatible to SH-4A, SMP machines.
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
 * hardware support for atomic operations, unfortunatelly.
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

#endif /*GAUCHE_PRIV_ATOMICP_H*/
