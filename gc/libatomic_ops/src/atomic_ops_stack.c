/*
 * Copyright (c) 2005 Hewlett-Packard Development Company, L.P.
 *
 * This file may be redistributed and/or modified under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2, or (at your option) any later version.
 *
 * It is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License in the
 * file COPYING for more details.
 */

#if defined(HAVE_CONFIG_H)
# include "config.h"
#endif

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define AO_REQUIRE_CAS
#include "atomic_ops_stack.h"

#ifdef AO_USE_ALMOST_LOCK_FREE

  void AO_pause(int); /* defined in atomic_ops.c */

/* LIFO linked lists based on compare-and-swap.  We need to avoid       */
/* the case of a node deletion and reinsertion while I'm deleting       */
/* it, since that may cause my CAS to succeed eventhough the next       */
/* pointer is now wrong.  Our solution is not fully lock-free, but it   */
/* is good enough for signal handlers, provided we have a suitably low  */
/* bound on the number of recursive signal handler reentries.           */
/* A list consists of a first pointer and a blacklist                   */
/* of pointer values that are currently being removed.  No list element */
/* on the blacklist may be inserted.  If we would otherwise do so, we   */
/* are allowed to insert a variant that differs only in the least       */
/* significant, ignored, bits.  If the list is full, we wait.           */

/* Crucial observation: A particular padded pointer x (i.e. pointer     */
/* plus arbitrary low order bits) can never be newly inserted into      */
/* a list while it's in the corresponding auxiliary data structure.     */

/* The second argument is a pointer to the link field of the element    */
/* to be inserted.                                                      */
/* Both list headers and link fields contain "perturbed" pointers, i.e. */
/* pointers with extra bits "or"ed into the low order bits.             */
void
AO_stack_push_explicit_aux_release(volatile AO_t *list, AO_t *x,
                                   AO_stack_aux *a)
{
  AO_t x_bits = (AO_t)x;
  AO_t next;

  /* No deletions of x can start here, since x is not currently in the  */
  /* list.                                                              */
 retry:
# if AO_BL_SIZE == 2
  {
    /* Start all loads as close to concurrently as possible. */
    AO_t entry1 = AO_load(a -> AO_stack_bl);
    AO_t entry2 = AO_load(a -> AO_stack_bl + 1);
    if (entry1 == x_bits || entry2 == x_bits)
      {
        /* Entry is currently being removed.  Change it a little.       */
          ++x_bits;
          if ((x_bits & AO_BIT_MASK) == 0)
            /* Version count overflowed;         */
            /* EXTREMELY unlikely, but possible. */
            x_bits = (AO_t)x;
        goto retry;
      }
  }
# else
  {
    int i;
    for (i = 0; i < AO_BL_SIZE; ++i)
      {
        if (AO_load(a -> AO_stack_bl + i) == x_bits)
          {
            /* Entry is currently being removed.  Change it a little.   */
              ++x_bits;
              if ((x_bits & AO_BIT_MASK) == 0)
                /* Version count overflowed;         */
                /* EXTREMELY unlikely, but possible. */
                x_bits = (AO_t)x;
            goto retry;
          }
      }
  }
# endif
  /* x_bits is not currently being deleted */
  do
    {
      next = AO_load(list);
      *x = next;
    }
  while (AO_EXPECT_FALSE(!AO_compare_and_swap_release(list, next, x_bits)));
}

/*
 * I concluded experimentally that checking a value first before
 * performing a compare-and-swap is usually beneficial on X86, but
 * slows things down appreciably with contention on Itanium.
 * Since the Itanium behavior makes more sense to me (more cache line
 * movement unless we're mostly reading, but back-off should guard
 * against that), we take Itanium as the default.  Measurements on
 * other multiprocessor architectures would be useful.  (On a uniprocessor,
 * the initial check is almost certainly a very small loss.) - HB
 */
#ifdef __i386__
# define PRECHECK(a) (a) == 0 &&
#else
# define PRECHECK(a)
#endif

AO_t *
AO_stack_pop_explicit_aux_acquire(volatile AO_t *list, AO_stack_aux * a)
{
  unsigned i;
  int j = 0;
  AO_t first;
  AO_t * first_ptr;
  AO_t next;

 retry:
  first = AO_load(list);
  if (0 == first) return 0;
  /* Insert first into aux black list.                                  */
  /* This may spin if more than AO_BL_SIZE removals using auxiliary     */
  /* structure a are currently in progress.                             */
  for (i = 0; ; )
    {
      if (PRECHECK(a -> AO_stack_bl[i])
          AO_compare_and_swap_acquire(a->AO_stack_bl+i, 0, first))
        break;
      ++i;
      if ( i >= AO_BL_SIZE )
        {
          i = 0;
          AO_pause(++j);
        }
    }
  assert(i < AO_BL_SIZE);
  assert(a -> AO_stack_bl[i] == first);
  /* First is on the auxiliary black list.  It may be removed by        */
  /* another thread before we get to it, but a new insertion of x       */
  /* cannot be started here.                                            */
  /* Only we can remove it from the black list.                         */
  /* We need to make sure that first is still the first entry on the    */
  /* list.  Otherwise it's possible that a reinsertion of it was        */
  /* already started before we added the black list entry.              */
# if defined(__alpha__) && (__GNUC__ == 4)
    if (first != AO_load_acquire(list))
                        /* Workaround __builtin_expect bug found in     */
                        /* gcc-4.6.3/alpha causing test_stack failure.  */
# else
    if (AO_EXPECT_FALSE(first != AO_load_acquire(list)))
                        /* Workaround test failure on AIX, at least, by */
                        /* using acquire ordering semantics for this    */
                        /* load.  Probably, it is not the right fix.    */
# endif
  {
    AO_store_release(a->AO_stack_bl+i, 0);
    goto retry;
  }
  first_ptr = AO_REAL_NEXT_PTR(first);
  next = AO_load(first_ptr);
# if defined(__alpha__) && (__GNUC__ == 4)
    if (!AO_compare_and_swap_release(list, first, next))
# else
    if (AO_EXPECT_FALSE(!AO_compare_and_swap_release(list, first, next)))
# endif
  {
    AO_store_release(a->AO_stack_bl+i, 0);
    goto retry;
  }
  assert(*list != first);
  /* Since we never insert an entry on the black list, this cannot have */
  /* succeeded unless first remained on the list while we were running. */
  /* Thus its next link cannot have changed out from under us, and we   */
  /* removed exactly one entry and preserved the rest of the list.      */
  /* Note that it is quite possible that an additional entry was        */
  /* inserted and removed while we were running; this is OK since the   */
  /* part of the list following first must have remained unchanged, and */
  /* first must again have been at the head of the list when the        */
  /* compare_and_swap succeeded.                                        */
  AO_store_release(a->AO_stack_bl+i, 0);
  return first_ptr;
}

#else /* ! USE_ALMOST_LOCK_FREE */

/* Better names for fields in AO_stack_t */
#define ptr AO_val2
#define version AO_val1

#if defined(AO_HAVE_compare_double_and_swap_double)

#ifdef LINT2
  volatile /* non-static */ AO_t AO_noop_sink;
#endif

void AO_stack_push_release(AO_stack_t *list, AO_t *element)
{
    AO_t next;

    do {
      next = AO_load(&(list -> ptr));
      *element = next;
    } while (AO_EXPECT_FALSE(!AO_compare_and_swap_release(&(list -> ptr),
                                                      next, (AO_t)element)));
    /* This uses a narrow CAS here, an old optimization suggested       */
    /* by Treiber.  Pop is still safe, since we run into the ABA        */
    /* problem only if there were both intervening "pop"s and "push"es. */
    /* In that case we still see a change in the version number.        */
#   ifdef LINT2
      /* Instruct static analyzer that element is not lost. */
      AO_noop_sink = (AO_t)element;
#   endif
}

AO_t *AO_stack_pop_acquire(AO_stack_t *list)
{
#   ifdef __clang__
      AO_t *volatile cptr;
                        /* Use volatile to workaround a bug in          */
                        /* clang-1.1/x86 causing test_stack failure.    */
#   else
      AO_t *cptr;
#   endif
    AO_t next;
    AO_t cversion;

    do {
      /* Version must be loaded first.  */
      cversion = AO_load_acquire(&(list -> version));
      cptr = (AO_t *)AO_load(&(list -> ptr));
      if (cptr == 0) return 0;
      next = *cptr;
    } while (AO_EXPECT_FALSE(!AO_compare_double_and_swap_double_release(list,
                                        cversion, (AO_t)cptr,
                                        cversion+1, (AO_t)next)));
    return cptr;
}


#elif defined(AO_HAVE_compare_and_swap_double)

/* Needed for future IA64 processors.  No current clients? */

#if !defined(CPPCHECK)
# error Untested!  Probably does not work.
#endif

/* We have a wide CAS, but only does an AO_t-wide comparison.   */
/* We can't use the Treiber optimization, since we only check   */
/* for an unchanged version number, not an unchanged pointer.   */
void AO_stack_push_release(AO_stack_t *list, AO_t *element)
{
    AO_t version;

    do {
      AO_t next_ptr;

      /* Again version must be loaded first, for different reason.      */
      version = AO_load_acquire(&(list -> version));
      next_ptr = AO_load(&(list -> ptr));
      *element = next_ptr;
    } while (!AO_compare_and_swap_double_release(
                           list, version,
                           version+1, (AO_t) element));
}

AO_t *AO_stack_pop_acquire(AO_stack_t *list)
{
    AO_t *cptr;
    AO_t next;
    AO_t cversion;

    do {
      cversion = AO_load_acquire(&(list -> version));
      cptr = (AO_t *)AO_load(&(list -> ptr));
      if (cptr == 0) return 0;
      next = *cptr;
    } while (!AO_compare_double_and_swap_double_release
                    (list, cversion, (AO_t) cptr, cversion+1, next));
    return cptr;
}


#endif /* AO_HAVE_compare_and_swap_double */

#endif /* ! USE_ALMOST_LOCK_FREE */
