/*
 * arith_aarch64.h - AArch64 (ARM64) specific arithmetic macros
 *
 *   Author: Shiro Kawai  <shiro@acm.org>
 *
 *   Absolutely No Warranty.  Public Domain.
 */

/* Included by arith.h if SCM_TARGET_AARCH64 is defined.

   SCM_ENABLE_ALL_ARITH_ASMS is not defined usually, so that we exclude macros
   that didn't show performance improvements.  It is defined by bench-arith
   only to compare performance.
 */

#ifdef __GNUC__

/* AArch64 flag conventions (differ from x86):
 *   adds/subs set NZCV.
 *   For subtract, C=1 means NO borrow (opposite of x86).
 *   'cs' == C set == carry (adds) / no-borrow (subs).
 *   'cc' == C clear == no-carry (adds) / borrow (subs).
 *   'vs'/'vc' == signed overflow set/clear.
 *   'mi'/'pl' == result negative / non-negative (N=1/0).
 *
 * All operands are u_long/long, which is 64-bit on AArch64 LP64.
 * The 'r' constraint together with a 64-bit C type makes GCC emit
 * the 'x' (64-bit) register names.
 */

/*-----------------------------------------------------------------
 * UADD(r, c, x, y)      unsigned word add with carry
 *  u_long : r, c, x, y;
 *  r <- x + y + c  mod wordsize
 *  c <- 1 if carry, 0 otherwise
 */

#ifdef SCM_ENABLE_ALL_ARITH_ASMS

/* cmp %2, #1 sets C = (c >= 1); since c in {0,1}, this makes C = c.
   adcs computes x + y + C_in and updates NZCV.
   adc harvests the resulting carry into the new c. */
#define UADD(r, c, x, y)                        \
    asm("cmp %2, #1;"                           \
        "adcs %0, %3, %4;"                      \
        "adc %1, xzr, xzr;"                     \
        : "=&r" (r), "=&r" (c)                  \
        : "1" (c), "r" (x), "r" (y)             \
        : "cc")

#endif //SCM_ENABLE_ALL_ARITH_ASMS

/*-----------------------------------------------------------------
 * UADDOV(r, v, x, y)    unsigned word add with overflow check
 *  u_long : r, v, x, y;
 *  if x + y overflows, v = 1
 *  else r <- x + y, v = 0
 */

#ifdef SCM_ENABLE_ALL_ARITH_ASMS

#define UADDOV(r, v, x, y)                      \
    asm("adds %0, %2, %3;"                      \
        "cset %1, cs;"                          \
        : "=&r" (r), "=&r" (v)                  \
        : "r" (x), "r" (y)                      \
        : "cc")

#endif //SCM_ENABLE_ALL_ARITH_ASMS

/*-----------------------------------------------------------------
 * SADDOV(r, v, x, y)     signed word addition with overflow check
 *  long : r, v, x, y;
 *  if x + y overflows, v = 1 or -1 depending on the sign of the result
 *  else r <- x + y, v = 0
 */

/* cset gives v = 1 iff V flag set (overflow), else 0.
   cneg negates v when PL (result non-negative).  Case analysis:
     V=0        : v=0 -> cneg is a no-op (still 0)          -> 0
     V=1, N=1   : v=1, MI -> keep                           -> +1 (positive overflow)
     V=1, N=0   : v=1, PL -> negate                         -> -1 (negative overflow) */
#define SADDOV(r, v, x, y)                      \
    asm("adds %0, %2, %3;"                      \
        "cset %1, vs;"                          \
        "cneg %1, %1, pl;"                      \
        : "=&r" (r), "=&r" (v)                  \
        : "r" (x), "r" (y)                      \
        : "cc")

/*-----------------------------------------------------------------
 * USUB(r, c, x, y)        unsigned word subtract with borrow
 *  u_long : r, x, c, y;
 *  r <- x - y - c  mod wordsize
 *  c <- 1 if borrow, 0 otherwise
 */

/* We need C_in = 1 (no borrow) when c=0, and C_in = 0 (borrow) when c=1.
   'cmp xzr, %2' computes 0 - c: C = 1 iff 0 >= c (unsigned), i.e., c=0.
   sbcs then does x - y - (1 - C) = x - y - c.
   cset %1, cc yields 1 iff borrow (C_out clear). */
#define USUB(r, c, x, y)                                \
    asm("cmp xzr, %2;"                                  \
        "sbcs %0, %3, %4;"                              \
        "cset %1, cc;"                                  \
        : "=&r" (r), "=&r" (c)                          \
        : "1" (c), "r" ((u_long)x), "r" ((u_long)y)     \
        : "cc")

/*-----------------------------------------------------------------
 * USUBOV(r, v, x, y)      unsigned word subtract with overflow check
 *  u_long : r, v, x, y;
 *  if x - y overflows, v = 1
 *  else r <- x - y, v = 0
 */

#ifdef SCM_ENABLE_ALL_ARITH_ASMS

#define USUBOV(r, v, x, y)                      \
    asm("subs %0, %2, %3;"                      \
        "cset %1, cc;"                          \
        : "=&r" (r), "=&r" (v)                  \
        : "r" (x), "r" (y)                      \
        : "cc")

#endif // SCM_ENABLE_ALL_ARITH_ASMS

/*-----------------------------------------------------------------
 * SSUBOV(r, v, x, y)     signed word subtract with overflow check
 *  long : r, v, x, y;
 *  if x - y overflows, v = 1 or -1 depending on the sign of the result
 *  else r <- x - y, v = 0
 */

#ifdef SCM_ENABLE_ALL_ARITH_ASMS

/* Same trick as SADDOV: v = (V?1:0), then negate if result non-negative. */
#define SSUBOV(r, v, x, y)                      \
    asm("subs %0, %2, %3;"                      \
        "cset %1, vs;"                          \
        "cneg %1, %1, pl;"                      \
        : "=&r" (r), "=&r" (v)                  \
        : "r" (x), "r" (y)                      \
        : "cc")

#endif //SCM_ENABLE_ALL_ARITH_ASMS

/*-----------------------------------------------------------------
 * UMUL(hi, lo, x, y)       unsigned word multiply
 *  u_long : hi, lo, x, y;
 *  [hi, lo] <- x * y
 */

#ifdef SCM_ENABLE_ALL_ARITH_ASMS

#define UMUL(hi, lo, x, y)                      \
    asm("mul %1, %2, %3;"                       \
        "umulh %0, %2, %3;"                     \
        : "=&r" (hi), "=&r" (lo)                \
        : "r" (x), "r" (y))

#endif //SCM_ENABLE_ALL_ARITH_ASMS

/*-----------------------------------------------------------------
 * UMULOV(r, v, x, y)      unsigned word multiply with overflow check
 *  u_long : r, x, y, v
 *  if x * y overflows, v = 1
 *  else r <- x * y, v = 0
 */

#ifdef SCM_ENABLE_ALL_ARITH_ASMS

/* umulh yields the upper 64 bits of the 128-bit product; nonzero iff overflow. */
#define UMULOV(r, v, x, y)                      \
    asm("umulh %1, %2, %3;"                     \
        "mul %0, %2, %3;"                       \
        "cmp %1, #0;"                           \
        "cset %1, ne;"                          \
        : "=&r" (r), "=&r" (v)                  \
        : "r" (x), "r" (y)                      \
        : "cc")

#endif //SCM_ENABLE_ALL_ARITH_ASMS

/*-----------------------------------------------------------------
 * SMULOV(r, v, x, y)      signed word multiply with overflow check
 *  long : r, x, y
 *  int : v
 *  if x * y overflows, v = 1 or -1 depending on the sign of the result
 *  else r <- x * y, v = 0
 */

#ifdef SCM_ENABLE_ALL_ARITH_ASMS

/* Signed overflow iff smulh(x,y) != asr(mul(x,y), 63).
   If overflowed, the sign of the true (128-bit) product is sign(smulh):
     smulh >= 0 -> true product positive -> positive overflow, v = 1
     smulh <  0 -> true product negative -> negative overflow, v = -1 */
#define SMULOV(r, v, x, y)                      \
    asm("smulh %1, %2, %3;"                     \
        "mul %0, %2, %3;"                       \
        "cmp %1, %0, asr #63;"                  \
        "b.eq 0f;"                              \
        "cmp %1, #0;"                           \
        "b.lt 1f;"                              \
        "mov %1, #1; b 2f;"                     \
        "1: mov %1, #-1; b 2f;"                 \
        "0: mov %1, #0;"                        \
        "2:"                                    \
        : "=&r" (r), "=&r" (v)                  \
        : "r" (x), "r" (y)                      \
        : "cc")

#endif //SCM_ENABLE_ALL_ARITH_ASMS

#endif /*__GNUC__*/
