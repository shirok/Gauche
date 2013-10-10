/*
 * arith_i386.h - i386 specific arithmetic macros
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

/* NB: in some context, the compiler failed to allocate registers
 * for 'r' and 'c', so I can't use "r" constraint for them.
 * The following code assumes 'r', 'c', 'x' and 'y' are stack-allocated
 * and to avoid both operands from being on memory.
 */
#define UADD(r, c, x, y)                        \
    asm("cmpl $1, %2;"                          \
        "cmc;"                                  \
        "movl %3, %%eax;"                       \
        "adcl %4, %%eax;"                       \
        "movl %%eax, %0;"                       \
        "movl $0, %%eax;"                       \
        "adcl $0, %%eax;"                       \
        "movl %%eax, %1;"                       \
        : "=&g" (r), "=&g" (c)                  \
        : "1" (c), "g"(x), "g"(y)               \
        : "%eax")

/*-----------------------------------------------------------------
 * UADDOV(r, v, x, y)    unsigned word add with overflow check
 *  u_long : r, v, x, y;
 *  if x + y overflows, v = 1
 *  else r <- x + y, v = 0
 */

#define UADDOV(r, v, x, y)                      \
    asm("movl %2, %0;"                          \
        "addl %3, %0;"                          \
        "movl $0, %1;"                          \
        "rcll $1, %1;"                          \
        : "=&r" (r), "=&r" (v)                  \
        : "g" (x), "g" (y))

/*-----------------------------------------------------------------
 * SADDOV(r, v, x, y)     signed word addition with overflow check
 *  long : r, v, x, y;
 *  if x + y overflows, v = 1 or -1 depending on the sign of the result
 *  else r <- x + y, v = 0
 */

#define SADDOV(r, v, x, y)                      \
    asm("movl $0, %1;"                          \
        "movl %2, %0;"                          \
        "addl %3, %0;"                          \
        "jno 0f;"                               \
        "jns 1f;"                               \
        "movl $1, %1; jmp 0f;"                  \
        "1: movl $-1, %1;"                      \
        "0:"                                    \
        : "=&r" (r), "=&r" (v)                  \
        : "g" (x), "g" (y))

/*-----------------------------------------------------------------
 * USUB(r, c, x, y)        unsigned word subtract with borrow
 *  u_long : r, x, c, y;
 *  r <- x - y - c  mod wordsize
 *  c <- 1 if borrow, 0 otherwise
 */

#define USUB(r, c, x, y)                        \
    asm("shrl $1, %2;"                          \
        "movl %3, %0;"                          \
        "sbbl %4, %0;"                          \
        "movl $0, %1;"                          \
        "rcll $1, %1;"                          \
        : "=&r" (r), "=&r"(c)                   \
        : "1" (c), "g" (x), "g" (y))

/*-----------------------------------------------------------------
 * USUBOV(r, v, x, y)      unsigned word subtract with overflow check
 *  u_long : r, v, x, y;
 *  if x - y overflows, v = 1
 *  else r <- x - y, v = 0
 */

#define USUBOV(r, v, x, y)                      \
    asm("movl %2, %0;"                          \
        "subl %3, %0;"                          \
        "movl $0, %1;"                          \
        "rcll $1, %1;"                          \
        : "=&r" (r), "=&r"(v)                   \
        : "g" (x), "g" (y))

/*-----------------------------------------------------------------
 * SSUBOV(r, v, x, y)     signed word subtract without borrow
 *  long : r, v, x, y;
 *  if x - y overflows, c = 1 or -1 depending on the sign of the result
 *  else r <- x - y, v = 0
 */

#define SSUBOV(r, v, x, y)                      \
    asm("movl $0, %1;"                          \
        "movl %2, %0;"                          \
        "subl %3, %0;"                          \
        "jno 0f;"                               \
        "jns 1f;"                               \
        "movl $1, %1; jmp 0f;"                  \
        "1: movl $-1, %1;"                      \
        "0:"                                    \
        : "=&r" (r), "=&r" (v)                  \
        : "g" (x), "g" (y))

/*-----------------------------------------------------------------
 * UMUL(hi, lo, x, y)       unsigned word multiply
 *  u_long : hi, lo, x, y;
 *  [hi, lo] <- x * y
 */

#define UMUL(hi, lo, x, y)                      \
    asm("movl %2, %%eax;"                       \
        "mull %3;"                              \
        "movl %%eax, %1;"                       \
        "movl %%edx, %0;"                       \
        : "=r" (hi), "=r" (lo)                  \
        : "g" (x), "r" (y)                      \
        : "%eax", "%edx")

/*-----------------------------------------------------------------
 * UMULOV(r, v, x, y)      unsigned word multiply with overflow check
 *  u_long : r, x, y
 *  int : v
 *  if x * y overflows, v = 1
 *  else r <- x * y, v = 0
 */
/* This and SMULOV is used in the gauche.uvector where lots of
   functions are inlined, and using "r" constraint sometimes makes
   gcc fail to allocate registers.  Thus we use "g" constraint. */

#define UMULOV(r, v, x, y)                      \
    asm("mull %2;"                              \
        "rcll $1, %1;"                          \
        : "=a" (r), "=r" (v)                    \
        : "r" (y), "0" (x), "1" (0)             \
        : "%edx")

/*-----------------------------------------------------------------
 * SMULOV(r, v, x, y)      signed word multiply with overflow check
 *  long : r, x, y
 *  int : v
 *  if x * y overflows, v = 1 or -1 depending on the sign of the result
 *  else r <- x * y, v = 0
 */

#define SMULOV(r, v, x, y)                      \
    asm("imull %2;"                             \
        "jno 0f;"                               \
        "cmp $0, %%edx;"                        \
        "jl 1f;"                                \
        "movl $1, %1; jmp 0f;"                  \
        "1: movl $-1, %1;"                      \
        "0:"                                    \
        : "=a" (r), "=r" (v)                    \
        : "r" (y), "0" (x), "1" (0)             \
        : "%edx")

#endif /*__GNUC__*/
