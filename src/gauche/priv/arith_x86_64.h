/*
 * arith_x86_64.h - x86_64 specific arithmetic macros
 *
 *   Author: Shiro Kawai  <shiro@acm.org>
 *
 *   Absolutely No Warranty.  Public Domain.
 */

#ifdef __GNUC__

/*-----------------------------------------------------------------
 * UADD(r, c, x, y)      unsigned word add with carry
 *  u_long : r, c, x, y;
 *  r <- x + y + c  mod wordsize
 *  c <- 1 if carry, 0 otherwise
 */

/* x or y can be immediate, in that case we can't use it directly
   in subq.  hence movq to rax/rdx. */
#define UADD(r, v, x, y)                        \
    asm("movq %2, %%rax;"                       \
        "movq %3, %%rdx;"                       \
        "cmpq $1, %1;"                          \
        "cmc;"                                  \
        "adcq %%rdx, %%rax;"                    \
        "movq %%rax, %0;"                       \
        "movq $0, %1;"                          \
        "rclq $1, %1;"                          \
        : "=r" (r), "=r" (c)                    \
        : "g" (x), "g" (y), "1"(c)              \
        : "%rax", "%rdx", "cc")

/*-----------------------------------------------------------------
 * UADDOV(r, v, x, y)    unsigned word add with overflow check
 *  u_long : r, v, x, y;
 *  if x + y overflows, v = 1
 *  else r <- x + y, v = 0
 */

/* x or y can be immediate, in that case we can't use it directly
   in subq.  hence movq to rax/rdx. */
#define UADDOV(r, v, x, y)                      \
    asm("movq %2, %%rax;"                       \
        "movq %3, %%rdx;"                       \
        "addq %%rdx, %%rax;"                    \
        "movq %%rax, %0;"                       \
        "rclq $1, %1;"                          \
        : "=r" (r), "=r" (v)                    \
        : "g" (x), "g" (y), "1"(0)              \
        : "%rax", "%rdx", "cc")

/*-----------------------------------------------------------------
 * SADDOV(r, v, x, y)     signed word addition with overflow check
 *  long : r, v, x, y;
 *  if x + y overflows, v = 1 or -1 depending on the sign of the result
 *  else r <- x + y, v = 0
 */

/* x or y can be immediate, in that case we can't use it directly
   in subq.  hence movq to rax/rdx. */
#define SADDOV(r, v, x, y)                      \
    asm("movq %2, %%rax;"                       \
        "movq %3, %%rdx;"                       \
        "addq %%rdx, %%rax;"                    \
        "movq %%rax, %0;"                       \
        "jno 0f;"                               \
        "jns 1f;"                               \
        "movq $1, %1; jmp 0f;"                  \
        "1: movq $-1, %1;"                      \
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
    asm("shrq $1, %2;"                          \
        "movq %3, %%rax;"                       \
        "movq %4, %%rdx;"                       \
        "sbbq %%rdx, %%rax;"                    \
        "movq %%rax, %0;"                       \
        "movq $0, %1;"                          \
        "rclq $1, %1;"                          \
        : "=r" (r), "=r"(c)                     \
        : "1" (c), "g" (x), "g" (y)             \
        : "%rax", "%rdx", "cc")

/*-----------------------------------------------------------------
 * USUBOV(r, v, x, y)      unsigned word subtract with overflow check
 *  u_long : r, v, x, y;
 *  if x - y overflows, v = 1
 *  else r <- x - y, v = 0
 */

/* x or y can be immediate, in that case we can't use it directly
   in subq.  hence movq to rax/rdx. */
#define USUBOV(r, v, x, y)                      \
    asm("movq %2, %%rax;"                       \
        "movq %3, %%rdx;"                       \
        "subq %%rdx, %%rax;"                    \
        "movq %%rax, %0;"                       \
        "rcl  $1, %1;"                          \
        : "=r" (r), "=r" (v)                    \
        : "g" (x), "g" (y), "1"(0)              \
        : "%rax", "%rdx", "cc")

/*-----------------------------------------------------------------
 * SSUBOV(r, v, x, y)     signed word subtract without borrow
 *  long : r, v, x, y;
 *  if x - y overflows, c = 1 or -1 depending on the sign of the result
 *  else r <- x - y, v = 0
 */

/* x or y can be immediate, in that case we can't use it directly
   in subq.  hence movq to rax/rdx. */
#define SSUBOV(r, v, x, y)                      \
    asm("movq %2, %%rax;"                       \
        "movq %3, %%rdx;"                       \
        "subq %%rdx, %%rax;"                    \
        "movq %%rax, %0;"                       \
        "jno 0f;"                               \
        "jns 1f;"                               \
        "mov $1, %1; jmp 0f;"                   \
        "1: mov $-1, %1;"                       \
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
    asm("movq %2, %%rax;"                       \
        "mulq %3;"                              \
        "movq %%rax, %1;"                       \
        "movq %%rdx, %0;"                       \
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
    asm("movq %2, %%rax;"                       \
        "mulq %3;"                              \
        "movq %%rax, %0;"                       \
        "rcl  $1, %1;"                          \
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

#define SMULOV(r, v, x, y)                      \
    asm("movq %2, %%rax;"                       \
        "imulq %3;"                             \
        "movq %%rax, %0;"                       \
        "jno 0f;"                               \
        "cmpq $0, %%rdx;"                       \
        "jl 1f;"                                \
        "mov  $1, %1; jmp 0f;"                  \
        "1: mov $-1, %1;"                       \
        "0:"                                    \
        : "=r" (r), "=r" (v)                    \
        : "r" (y), "r" (x), "1" (0)             \
        : "%rax", "%rdx", "cc")

#endif /*__GNUC__*/
