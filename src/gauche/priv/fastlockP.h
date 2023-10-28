/*
 * Spinlock implementation
 *
 * For pthreads, we just use pthread_spinlock_t.
 * Windows only provides fastlock for drivers, so we roll our own.
 */

#ifndef GAUCHE_PRIV_FASTLOCKP_H
#define GAUCHE_PRIV_FASTLOCKP_H

#if GAUCHE_WINDOWS

#include "gauche/priv/atomicP.h"

struct win_spinlock_rec {
    ScmAtomicVar lock_state;
};

#endif /*GAUCHE_WINDOWS*/

#endif /*GAUCHE_PRIV_FASTLOCKP_H*/
