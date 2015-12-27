/*
 * arith.h - macros for arithmetic
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

/* Include processor-specific macros */
/* NB: MinGW64 gcc uses SIZEOF_LONG == 4 even on x86_64, and caused some
   argument register mismatch in the asm code in arith_x86_64.h.
   Ideally we should fix the macro caller to use variables with proper
   width of integer.  But for the time being, we use i386 asm code
   on MinGW64/x86_64 for quick workaround. */
#if defined(SCM_TARGET_I386) || (defined(SCM_TARGET_X86_64) && SIZEOF_LONG == 4)
#include "arith_i386.h"
#elif defined(SCM_TARGET_X86_64)
#include "arith_x86_64.h"
#endif

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
 * UADDOV(r, v, x, y)    unsigned word add with overflow check
 *  u_long : r, v, x, y;
 *  if x + y overflows, v = 1
 *  else r <- x + y, v = 0
 */

#ifndef UADDOV
/* Portable version */
#define UADDOV(r, v, x, y)                      \
  do {                                          \
    (r) = (x) + (y);                            \
    (v) = ((r) < (x))? 1 : 0;                   \
  } while (0)
#endif /*UADDOV*/

/*-----------------------------------------------------------------
 * SADDOV(r, v, x, y)     signed word addition with overflow check
 *  long : r, v, x, y;
 *  if x + y overflows, v = 1 or -1 depending on the sign of the result
 *  else r <- x + y, v = 0
 */

#ifndef SADDOV
/* Portable version */
#define SADDOV(r, v, x, y)                      \
  do {                                          \
    (r) = (x) + (y);                            \
    if ((x) >= 0) {                             \
      if ((y) >= 0 && (r) < 0) (v) = 1;         \
      else (v) = 0;                             \
    } else {                                    \
      if ((y) < 0 && (r) >= 0) (v) = -1;        \
      else (v) = 0;                             \
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
 * USUBOV(r, v, x, y)      unsigned word subtract with overflow check
 *  u_long : r, v, x, y;
 *  if x - y overflows, v = 1
 *  else r <- x - y, v = 0
 */

#ifndef USUBOV
/* Portable version */
#define USUBOV(r, v, x, y)                      \
  do {                                          \
    (r) = (x) - (y);                            \
    (v) = ((r) > (x))? 1 : 0;                   \
  } while (0)
#endif /*USUBOV*/

/*-----------------------------------------------------------------
 * SSUBOV(r, v, x, y)     signed word subtract without borrow
 *  long : r, v, x, y;
 *  if x - y overflows, c = 1 or -1 depending on the sign of the result
 *  else r <- x - y, v = 0
 */

#ifndef SSUBOV
/* Portable version */
#define SSUBOV(r, v, x, y)                      \
  do {                                          \
    (r) = (x) - (y);                            \
    if ((x) >= 0) {                             \
      if ((y) < 0 && (r) <= 0) (v) = 1;         \
      else (v) = 0;                             \
    } else {                                    \
      if ((y) >= 0 && (r) > 0) (v) = -1;        \
      else (v) = 0;                             \
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
 * UMULOV(r, v, x, y)      unsigned word multiply with overflow check
 *  u_long : r, x, y
 *  int : v
 *  if x * y overflows, v = 1
 *  else r <- x * y, v = 0
 */

#ifndef UMULOV
#define UMULOV(r, v, x, y)                              \
    do {                                                \
        if ((x)==0 || (y)==0) { (v) = (r) = 0; }        \
        else {                                          \
            u_long t5_;                                 \
            UMUL(t5_, r, x, y);                         \
            (v) = (t5_)? 1 : 0;                         \
        }                                               \
    } while (0)
#endif /*UMULOV*/

/*-----------------------------------------------------------------
 * SMULOV(r, v, x, y)      signed word multiply with overflow check
 *  long : r, x, y
 *  int : v
 *  if x * y overflows, v = 1 or -1 depending on the sign of the result
 *  else r <- x * y, v = 0
 */

#ifndef SMULOV
#define SMULOV(r, v, x, y)                                      \
    do {                                                        \
        u_long t6_;                                             \
        if ((x) >= 0) {                                         \
            if ((y) >= 0) {                                     \
                UMULOV(t6_, v, x, y);                           \
                if ((v) || t6_ > LONG_MAX) (v) = 1;             \
                else (r) = t6_;                                 \
            } else {                                            \
                UMULOV(t6_, v, x, -y);                          \
                if ((v) || t6_ > LONG_MAX+1UL) (v) = -1;        \
                else (r) = -(long)t6_;                          \
            }                                                   \
        } else {                                                \
            if ((y) >= 0) {                                     \
                UMULOV(t6_, v, -x, y);                          \
                if ((v) || t6_ > LONG_MAX+1UL) (v) = -1;        \
                else (r) = -(long)t6_;                          \
            } else {                                            \
                UMULOV(t6_, v, -x, -y);                         \
                if ((v) || t6_ > LONG_MAX) (v) = 1;             \
                else (r) = t6_;                                 \
            }                                                   \
        }                                                       \
    } while (0)
#endif /*SMULOV*/


#endif /* GAUCHE_ARITH_H */
