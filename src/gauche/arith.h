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
 *  $Id: arith.h,v 1.2 2002-06-22 08:39:10 shirok Exp $
 */

#ifndef GAUCHE_ARITH_H
#define GAUCHE_ARITH_H

/*
 * This file defines some basic integer arithmetic operations.
 * Most modern CPUs have efficient way to perform those operations.
 *
 * Not intended for global use; i.e. only Gauche internal code uses these.
 * Only the features needed by Gauche's guts are implemented.  You see
 * the provided macros are not symmetric.
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
 * UADD(r, c, x, y)      unsigned word add with carry
 *  u_long : r, c, x, y;
 *  r <- x + y + c  mod wordsize
 *  c <- 1 if carry, 0 otherwise
 */

#ifndef UADD
/* Portable version */
#define UADD(r, c, x, y)                                        \
  do {                                                          \
    (r) = (x) + (y) + (c);                                      \
    (c) = ((r)<(x) || ((r)==(x) && ((y)>0||(c)>0)))? 1 : 0;     \
  } while (0)
#endif /*UADD*/

/*-----------------------------------------------------------------
 * UADDOV(r, c, x, y)    unsigned word add with overflow check
 *  u_long : r, c, x, y;
 *  if x + y overflows, c = 1
 *  else r <- x + y, c = 0
 */

#ifndef UADDOV
/* Portable version */
#define UADDOV(r, c, x, y)                      \
  do {                                          \
    (r) = (x) + (y);                            \
    (c) = (r) < (x);                            \
  } while (0)
#endif /*UADDOV*/

/*-----------------------------------------------------------------
 * SADDOV(r, c, x, y)     signed word addition with overflow check
 *  long : r, c, x, y;
 *  if x + y overflows, c = 1,
 *  else r <- x + y, c = 0
 */

#ifndef SADDOV
/* Portable version */
#define SADDOV(r, c, x, y)                      \
  do {                                          \
    (r) = (x) + (y);                            \
    if ((x) >= 0) {                             \
      if ((y) >= 0 && (r) < 0) (c) = 1;         \
      else (c) = 0;                             \
    } else {                                    \
      if ((y) < 0 && (r) >= 0) (c) = 1;         \
      else (c) = 0;                             \
    }                                           \
  } while (0)
#endif /*SADDOV*/

/*-----------------------------------------------------------------
 * USUB(r, c, x, y)        unsigned word subtract with borrow
 *  u_long : r, x, c, y;
 *  r <- x - y - c  mod wordsize
 *  c <- 1 if borrow, 0 otherwise
 */

#ifndef USUB
/* Portable version */
#define USUB(r, c, x, y)                                        \
  do {                                                          \
    (r) = (x) - (y) - (c);                                      \
    (c) = ((r)>(x) || ((r)==(x) && ((y)>0||(c)>0)))? 1 : 0;     \
  } while (0)
#endif /*USUB*/

/*-----------------------------------------------------------------
 * USUBOV(r, c, x, y)      unsigned word subtract with overflow check
 *  u_long : r, c, x, y;
 *  if x - y overflows, c = 1
 *  else r <- x - y, c = 0
 */

#ifndef USUBOV
/* Portable version */
#define USUBOV(r, c, x, y)                      \
  do {                                          \
    (r) = (x) - (y);                            \
    (c) = (r) > (x);                            \
  } while (0)
#endif /*USUBOV*/

/*-----------------------------------------------------------------
 * SSUBOV(r, c, x, y)     signed word subtract without borrow
 *  long : r, c, x, y;
 *  if x - y overflows, c = 1,
 *  else r <- x - y, c = 0
 *  Note that borrow is _not_ propagated.
 */

#ifndef SSUBOV
/* Portable version */
#define SSUBOV(r, c, x, y)                      \
  do {                                          \
    (r) = (x) - (y);                            \
    if ((x) >= 0) {                             \
      if ((y) < 0 && (r) <= 0) (c) = 1;         \
      else (c) = 0;                             \
    } else {                                    \
      if ((y) >= 0 && (r) > 0) (c) = 1;         \
      else (c) = 0;                             \
    }                                           \
  } while (0)
#endif /*SSUBOV*/

/*-----------------------------------------------------------------
 * UMUL(hi, lo, x, y)       unsigned word multiply
 *  u_long : hi, lo, x, y;
 *  [hi, lo] <- x * y
 */

#ifndef UMUL
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
#endif /*UMUL*/

/*-----------------------------------------------------------------
 * UMULOV(r, c, x, y)      unsigned word multiply with overflow check
 *  u_long : r, x, y
 *  int : c
 *  if x * y overflows, c = 1
 *  else r <- x * y, c = 0
 */

#ifndef UMULOV
#define UMULOV(r, c, x, y)                              \
    do {                                                \
        if ((x)==0 || (y)==0) { (c) = (r) = 0; }        \
        else {                                          \
            (r) = (x) * (y);                            \
            (c) = ((r)<(x) || (r)<(y));                 \
        }                                               \
    } while (0)
#endif /*UMULOV*/

/*-----------------------------------------------------------------
 * SMULOV(r, c, x, y)      signed word multiply with overflow check
 *  long : r, x, y
 *  int : c
 *  if x * y overflows, c = 1
 *  else r <- x * y, c = 0
 */

#ifndef SMULOV
#define SMULOV(r, c, x, y)                              \
    do {                                                \
        u_long t0_;                                     \
        if ((x) >= 0) {                                 \
            if ((y) >= 0) {                             \
                UMULOV(t0_, c, x, y);                   \
                if (t0_ > LONG_MAX) (c) = 1;            \
                else (r) = t0_;                         \
            } else {                                    \
                UMULOV(t0_, c, x, -y);                  \
                if (t0_ > LONG_MAX+1UL) (c) = 1;        \
                else (r) = -t0_;                        \
            }                                           \
        } else {                                        \
            if ((y) >= 0) {                             \
                UMULOV(t0_, c, -x, y);                  \
                if (t0_ > LONG_MAX+1UL) (c) = 1;        \
                else (r) = -t0_;                        \
            } else {                                    \
                UMULOV(t0_, c, -x, -y);                 \
                if (t0_ > LONG_MAX) (c) = 1;            \
                else (r) = t0_;                         \
            }                                           \
        }                                               \
    } while (0)
#endif /*SMULOV*/


#endif /* GAUCHE_ARITH_H */
