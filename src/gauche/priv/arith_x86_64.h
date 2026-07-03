/*
 * arith_x86_64.h - x86_64 specific arithmetic macros
 *
 *   Author: Shiro Kawai  <shiro@acm.org>
 *
 *   Absolutely No Warranty.  Public Domain.
 */

/* Included by arith.h if SCM_TARGET_X86_64 is defined.

   SCM_ENABLE_ALL_ARITH_ASMS is not defined usually, so that we exclude macros
   that didn't show performance improvements.  It is defined by bench-arith
   only to compare performance.
 */

#ifdef __GNUC__

/*-----------------------------------------------------------------
 * UADD(r, c, x, y)      unsigned word add with carry
 *  u_long : r, c, x, y;
 *  r <- x + y + c  mod wordsize
 *  c <- 1 if carry, 0 otherwise
 */

#ifdef SCM_ENABLE_ALL_ARITH_ASMS

/* x or y can be immediate, in that case we can't use it directly
   in subq.  hence movq to rax/rdx. */
#define UADD(r, v, x, y)                        \
    asm("movq %2, %%rax\n\t"                    \
        "movq %3, %%rdx\n\t"                    \
        "cmpq $1, %1\n\t"                       \
        "cmc\n\t"                               \
        "adcq %%rdx, %%rax\n\t"                 \
        "movq %%rax, %0\n\t"                    \
        "movq $0, %1\n\t"                       \
        "rclq $1, %1\n\t"                       \
        : "=r" (r), "=r" (c)                    \
        : "g" (x), "g" (y), "1"(c)              \
        : "%rax", "%rdx", "cc")

#endif //SCM_ENABLE_ALL_ARITH_ASMS

/*-----------------------------------------------------------------
 * UADDOV(r, v, x, y)    unsigned word add with overflow check
 *  u_long : r, v, x, y;
 *  if x + y overflows, v = 1
 *  else r <- x + y, v = 0
 */

#ifdef SCM_ENABLE_ALL_ARITH_ASMS

/* x or y can be immediate, in that case we can't use it directly
   in subq.  hence movq to rax/rdx. */
#define UADDOV(r, v, x, y)                      \
    asm("movq %2, %%rax\n\t"                    \
        "movq %3, %%rdx\n\t"                    \
        "addq %%rdx, %%rax\n\t"                 \
        "movq %%rax, %0\n\t"                    \
        "rclq $1, %1\n\t"                       \
        : "=r" (r), "=r" (v)                    \
        : "g" (x), "g" (y), "1"(0)              \
        : "%rax", "%rdx", "cc")

#endif //SCM_ENABLE_ALL_ARITH_ASMS

/*-----------------------------------------------------------------
 * SADDOV(r, v, x, y)     signed word addition with overflow check
 *  long : r, v, x, y;
 *  if x + y overflows, v = 1 or -1 depending on the sign of the result
 *  else r <- x + y, v = 0
 */

/* x or y can be immediate, in that case we can't use it directly
   in subq.  hence movq to rax/rdx. */
#define SADDOV(r, v, x, y)                      \
    asm("movq %2, %%rax\n\t"                    \
        "movq %3, %%rdx\n\t"                    \
        "addq %%rdx, %%rax\n\t"                 \
        "movq %%rax, %0\n\t"                    \
        "jno 0f\n\t"                            \
        "jns 1f\n\t"                            \
        "movq $1, %1\n\t"                       \
        "jmp 0f\n\t"                            \
        "1: movq $-1, %1\n\t"                   \
        "0:"                                    \
        : "=r" (r), "=r" (v)                    \
        : "g" (x), "g" (y), "1"(0)              \
        : "%rax", "%rdx", "cc")

/*-----------------------------------------------------------------
 * USUB(r, c, x, y)        unsigned word subtract with borrow
 *  u_long : r, x, c, y;
 *  r <- x - y - c  mod wordsize
 *  c <- 1 if borrow, 0 otherwise
 */

/* x or y can be immediate, in that case we can't use it directly
   in subq.  hence movq to rax/rdx. */
#define USUB(r, c, x, y)                        \
    asm("shrq $1, %2\n\t"                       \
        "movq %3, %%rax\n\t"                    \
        "movq %4, %%rdx\n\t"                    \
        "sbbq %%rdx, %%rax\n\t"                 \
        "movq %%rax, %0\n\t"                    \
        "movq $0, %1\n\t"                       \
        "rclq $1, %1\n\t"                       \
        : "=r" (r), "=r"(c)                     \
        : "1" (c), "g" (x), "g" (y)             \
        : "%rax", "%rdx", "cc")

/*-----------------------------------------------------------------
 * USUBOV(r, v, x, y)      unsigned word subtract with overflow check
 *  u_long : r, v, x, y;
 *  if x - y overflows, v = 1
 *  else r <- x - y, v = 0
 */


#ifdef SCM_ENABLE_ALL_ARITH_ASMS

/* x or y can be immediate, in that case we can't use it directly
   in subq.  hence movq to rax/rdx. */
#define USUBOV(r, v, x, y)                      \
    asm("movq %2, %%rax\n\t"                    \
        "movq %3, %%rdx\n\t"                    \
        "subq %%rdx, %%rax\n\t"                 \
        "movq %%rax, %0\n\t"                    \
        "rcl  $1, %1\n\t"                       \
        : "=r" (r), "=r" (v)                    \
        : "g" (x), "g" (y), "1"(0)              \
        : "%rax", "%rdx", "cc")

#endif //SCM_ENABLE_ALL_ARITH_ASMS

/*-----------------------------------------------------------------
 * SSUBOV(r, v, x, y)     signed word subtract without borrow
 *  long : r, v, x, y;
 *  if x - y overflows, c = 1 or -1 depending on the sign of the result
 *  else r <- x - y, v = 0
 */

/* x or y can be immediate, in that case we can't use it directly
   in subq.  hence movq to rax/rdx. */
#define SSUBOV(r, v, x, y)                      \
    asm("movq %2, %%rax\n\t"                    \
        "movq %3, %%rdx\n\t"                    \
        "subq %%rdx, %%rax\n\t"                 \
        "movq %%rax, %0\n\t"                    \
        "jno 0f\n\t"                            \
        "jns 1f\n\t"                            \
        "mov $1, %1\n\t"                        \
        "jmp 0f\n\t"                            \
        "1: mov $-1, %1\n\t"                    \
        "0:"                                    \
        : "=&r" (r), "=&r" (v)                  \
        : "g" (x), "g" (y), "1"(0)              \
        : "%rax", "%rdx", "cc")

/*-----------------------------------------------------------------
 * UMUL(hi, lo, x, y)       unsigned word multiply
 *  u_long : hi, lo, x, y;
 *  [hi, lo] <- x * y
 */

#define UMUL(hi, lo, x, y)                      \
    asm("movq %2, %%rax\n\t"                    \
        "mulq %3\n\t"                           \
        "movq %%rax, %1\n\t"                    \
        "movq %%rdx, %0\n\t"                    \
        : "=r" (hi), "=r" (lo)                  \
        : "g" (x), "r" (y)                      \
        : "%rax", "%rdx", "cc")

/*-----------------------------------------------------------------
 * UMULOV(r, v, x, y)      unsigned word multiply with overflow check
 *  u_long : r, x, y
 *  int : v
 *  if x * y overflows, v = 1
 *  else r <- x * y, v = 0
 */

#define UMULOV(r, v, x, y)                      \
    asm("movq %2, %%rax\n\t"                    \
        "mulq %3\n\t"                           \
        "movq %%rax, %0\n\t"                    \
        "rcl  $1, %1\n\t"                       \
        : "=r" (r), "=r" (v)                    \
        : "r" (y), "r" (x), "1" (0)             \
        : "%rax", "%rdx", "cc")

/*-----------------------------------------------------------------
 * SMULOV(r, v, x, y)      signed word multiply with overflow check
 *  long : r, x, y
 *  int : v
 *  if x * y overflows, v = 1 or -1 depending on the sign of the result
 *  else r <- x * y, v = 0
 */

#ifdef SCM_ENABLE_ALL_ARITH_ASMS

#define SMULOV(r, v, x, y)                      \
    asm("movq %2, %%rax\n\t"                    \
        "imulq %3\n\t"                          \
        "movq %%rax, %0\n\t"                    \
        "jno 0f\n\t"                            \
        "cmpq $0, %%rdx\n\t"                    \
        "jl 1f\n\t"                             \
        "mov  $1, %1\n\t"                       \
        "jmp 0f\n\t"                            \
        "1: mov $-1, %1\n\t"                    \
        "0:"                                    \
        : "=r" (r), "=r" (v)                    \
        : "r" (y), "r" (x), "1" (0)             \
        : "%rax", "%rdx", "cc")

#endif //SCM_ENABLE_ALL_ARITH_ASMS

#endif /*__GNUC__*/
