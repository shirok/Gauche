/*
 * arith.h - macros for arithmetic
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
 *  $Id: arith.h,v 1.1 2002-06-18 06:06:16 shirok Exp $
 */

#ifndef GAUCHE_ARITH_H
#define GAUCHE_ARITH_H

/*
 * This file defines some basic integer arithmetic operations.
 * Most modern CPUs have efficient way to perform those operations.
 */

/* some useful constants */
#define SCM_ULONG_MAX      ((u_long)(-1L)) /* to be configured */
#define WORD_BITS          (SIZEOF_LONG * 8)
#define HALF_BITS          (WORD_BITS/2)
#define HALF_WORD          (1L<<HALF_BITS)

#ifndef LONG_MIN
#define LONG_MIN           ((long)(1L<<(WORD_BITS-1)))
#endif
#ifndef LONG_MAX
#define LONG_MAX           (-(LONG_MIN+1))
#endif

#define LOMASK             (HALF_WORD-1)
#define HIMASK             (~LOMASK)
#define LO(word)           ((word) & LOMASK)
#define HI(word)           (((word) >> HALF_BITS)&LOMASK)

/*-----------------------------------------------------------------
 * UADD(r, c, x, y)
 *  u_long : r, x, c, y;
 *  r <- x + y + c  mod wordsize
 *  c <- 1 if carry, 0 otherwise
 */

/* Portable version */
#define UADD(r, c, x, y)                                        \
  do {                                                          \
    (r) = (x) + (y) + (c);                                      \
    (c) = ((r)<(x) || ((r)==(x) && ((y)>0||(c)>0)))? 1 : 0;     \
  } while (0)

/*-----------------------------------------------------------------
 * USUB(r, c, x, y)
 *  u_long : r, x, c, y;
 *  r <- x - y - c  mod wordsize
 *  c <- 1 if borrow, 0 otherwise
 */

/* Portable version */
#define USUB(r, c, x, y)                                        \
  do {                                                          \
    (r) = (x) - (y) - (c);                                      \
    (c) = ((r)>(x) || ((r)==(x) && ((y)>0||(c)>0)))? 1 : 0;     \
  } while (0)

/*-----------------------------------------------------------------
 * UMUL(hi, lo, x y)
 *  u_long : hi, lo, x, y;
 *  [hi, lo] <- x * y
 */

/* Portable version */
#define UMUL(hi, lo, x, y)                                              \
    do {                                                                \
        u_long xl_ = LO(x), xh_ = HI(x), yl_ = LO(y), yh_ = HI(y);      \
        u_long t1_, t2_, t3_, t4_;                                      \
        lo = xl_ * yl_;                                                 \
        t1_ = xl_ * yh_;                                                \
        t2_ = xh_ * yl_;                                                \
        hi = xh_ * yh_;                                                 \
        t3_ = t1_ + t2_;                                                \
        if (t3_ < t1_) hi += HALF_WORD;                                 \
        hi += HI(t3_);                                                  \
        t4_ = LO(t3_) << HALF_BITS;                                     \
        lo += t4_;                                                      \
        if (lo < t4_) hi++;                                             \
    } while (0)


#if 0
#undef UMUL
/* example of processor-specific optimization */
#define UMUL(hi, lo, x, y)                                              \
    do {                                                                \
        asm("movl %2, %%eax; mull %3; movl %%edx, %0; movl %%eax, %1"   \
            : "=g" (hi), "=g" (lo)                                      \
            : "g" (x), "g" (y)                                          \
            : "%eax", "%edx");                                          \
    } while (0)
#endif


#endif /* GAUCHE_ARITH_H */
