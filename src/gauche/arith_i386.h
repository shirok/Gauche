/*
 * arith_i386.h - i386 specific arithmetic macros
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
 *  $Id: arith_i386.h,v 1.1 2002-09-09 08:08:45 shirok Exp $
 */

#ifdef __GNUC__

/*-----------------------------------------------------------------
 * UADD(r, c, x, y)      unsigned word add with carry
 *  u_long : r, c, x, y;
 *  r <- x + y + c  mod wordsize
 *  c <- 1 if carry, 0 otherwise
 */

#define UADD(r, c, x, y) \
    asm("shrl $1, %2;" \
        "movl %3, %0;" \
        "adcl %4, %0;" \
        "movl $0, %1;" \
        "rcll $1, %1;" \
           :"=&r" (r), "=&r" (c) \
           :"1" (c), "g"(x), "g"(y))

/*-----------------------------------------------------------------
 * UADDOV(r, v, x, y)    unsigned word add with overflow check
 *  u_long : r, v, x, y;
 *  if x + y overflows, v = 1
 *  else r <- x + y, v = 0
 */

#define UADDOV(r, v, x, y) \
    asm("movl %2, %0;" \
        "addl %3, %0;" \
        "movl $0, %1;" \
        "rcll $1, %1;" \
           :"=&r" (r), "=&r" (v) \
           :"g" (x), "g" (y))

/*-----------------------------------------------------------------
 * SADDOV(r, v, x, y)     signed word addition with overflow check
 *  long : r, v, x, y;
 *  if x + y overflows, v = 1 or -1 depending on the sign of the result
 *  else r <- x + y, v = 0
 */

#define SADDOV(r, v, x, y) \
    asm("movl $0, %1;" \
        "movl %2, %0;" \
        "addl %3, %0;" \
        "jno 0f;" \
        "jns 1f;" \
        "movl $1, %1; jmp 0f;" \
        "1: movl $-1, %1;" \
        "0:" \
           :"=&r" (r), "=&r" (v) \
           :"g" (x), "g" (y))

/*-----------------------------------------------------------------
 * USUB(r, c, x, y)        unsigned word subtract with borrow
 *  u_long : r, x, c, y;
 *  r <- x - y - c  mod wordsize
 *  c <- 1 if borrow, 0 otherwise
 */

#define USUB(r, c, x, y) \
    asm("shrl $1, %2;" \
        "movl %3, %0;" \
        "sbbl %4, %0;" \
        "movl $0, %1;" \
        "rcll $1, %1;" \
          :"=&r" (r), "=&r"(c) \
          :"1" (c), "g" (x), "g" (y))

/*-----------------------------------------------------------------
 * USUBOV(r, v, x, y)      unsigned word subtract with overflow check
 *  u_long : r, v, x, y;
 *  if x - y overflows, v = 1
 *  else r <- x - y, v = 0
 */

#define USUBOV(r, v, x, y) \
    asm("movl %2, %0;" \
        "subl %3, %0;" \
        "movl $0, %1;" \
        "rcll $1, %1;" \
          :"=&r" (r), "=&r"(v) \
          :"g" (x), "g" (y))

/*-----------------------------------------------------------------
 * SSUBOV(r, v, x, y)     signed word subtract without borrow
 *  long : r, v, x, y;
 *  if x - y overflows, c = 1 or -1 depending on the sign of the result
 *  else r <- x - y, v = 0
 */

#define SSUBOV(r, v, x, y) \
    asm("movl $0, %1;" \
        "movl %2, %0;" \
        "subl %3, %0;" \
        "jno 0f;" \
        "jns 1f;" \
        "movl $1, %1; jmp 0f;" \
        "1: movl $-1, %1;" \
        "0:" \
           :"=&r" (r), "=&r" (v) \
           :"g" (x), "g" (y))

/*-----------------------------------------------------------------
 * UMUL(hi, lo, x, y)       unsigned word multiply
 *  u_long : hi, lo, x, y;
 *  [hi, lo] <- x * y
 */

#define UMUL(hi, lo, x, y) \
    asm("movl %2, %%eax;" \
        "mull %3;" \
        "movl %%eax, %1;" \
        "movl %%edx, %0;" \
           :"=r" (hi), "=r" (lo) \
           :"g" (x), "r" (y) \
           :"%eax", "%edx")

/*-----------------------------------------------------------------
 * UMULOV(r, v, x, y)      unsigned word multiply with overflow check
 *  u_long : r, x, y
 *  int : v
 *  if x * y overflows, v = 1
 *  else r <- x * y, v = 0
 */

#define UMULOV(r, v, x, y) \
    asm("movl %2, %%eax;" \
        "mull %3;" \
        "movl %%eax, %0;" \
        "movl $0, %1;" \
        "rcll $1, %1;" \
           :"=r" (r), "=&r" (v) \
           :"g" (x), "r" (y) \
           :"%eax", "%edx")

/*-----------------------------------------------------------------
 * SMULOV(r, v, x, y)      signed word multiply with overflow check
 *  long : r, x, y
 *  int : v
 *  if x * y overflows, v = 1 or -1 depending on the sign of the result
 *  else r <- x * y, v = 0
 */

#define SMULOV(r, v, x, y) \
    asm("movl $0, %1;" \
        "movl %2, %%eax;" \
        "imull %3;" \
        "movl %%eax, %0;" \
        "jno 0f;" \
        "cmp $0, %%edx;" \
        "jl 1f;" \
        "movl $1, %1; jmp 0f;" \
        "1: movl $-1, %1; "\
        "0:" \
           :"=r" (r), "=&r" (v) \
           :"g" (x), "r" (y) \
           :"%eax", "%edx")

#endif /*__GNUC__*/


