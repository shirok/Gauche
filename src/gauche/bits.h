/*
 * gauche/bits.h - Bit manipulation utilities
 *
 *   Copyright (c) 2007-2022  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_BITS_H
#define GAUCHE_BITS_H

/*============================================================
 * Bits
 */

/* ScmBits utilities are to manage bit array in an array of u_longs.

   The LSB of the first word is the bit #0.  The MSB of the first
   word is the bit #31 or #63, and the LSB of the second word is the
   bit #32 or #64, and so on.
   The user can use ScmBits as opaque structure and don't need to
   worry about the bit ordering.  For internal hackers: ScmBits are
   also used in bignum.c, and it must maintain this bit ordering.
   It is important that ScmBignum->values can be casted to ScmBits*.
*/

typedef u_long ScmBits;

/* Allocates and returns a bitmap that can hold NUMBITS.  Zero-cleared. */
SCM_EXTERN ScmBits *Scm_MakeBits(int numbits);

#define SCM_BITS_NUM_WORDS(size) \
    (((size)+SCM_WORD_BITS-1)/SCM_WORD_BITS)

#define SCM_BITS_TEST_IN_WORD(word, ind_w)  (0!=((word) & (1UL<<(ind_w))))
#define SCM_BITS_SET_IN_WORD(word, ind_w)   ((word) |= (1UL<<(ind_w)))
#define SCM_BITS_RESET_IN_WORD(word, ind_w) ((word) &= ~(1UL<<(ind_w)))

#define SCM_BITS_TEST(bits, index)                \
  SCM_BITS_TEST_IN_WORD((bits)[(index)/SCM_WORD_BITS], (index)%SCM_WORD_BITS)

#define SCM_BITS_SET(bits, index)                 \
  SCM_BITS_SET_IN_WORD((bits)[(index)/SCM_WORD_BITS], (index)%SCM_WORD_BITS)

#define SCM_BITS_RESET(bits, index)               \
  SCM_BITS_RESET_IN_WORD((bits)[(index)/SCM_WORD_BITS], (index)%SCM_WORD_BITS)

/* calculates a mask to extract bits between s (inclusive) and e (exclusive)
   from a word.  e==0 means e is just left of the word (i.e. bits up to
   the word boundary.  Assume 0 <= s < e' <= SCM_WORD_BITS, where e' = e
   for 1 <= e < SCM_WORD_BITS, and e' = SCM_WORD_BITS if e == 0. */
#define SCM_BITS_MASK(s, e) \
    (((e)? (1UL<<(e)) - 1 : (u_long)-1) & ~((1UL<<(s)) - 1))

/* works on the range of bits, from start (inclusive) to end (exclusive) */

SCM_EXTERN void   Scm_BitsFill(ScmBits *bits, int start, int end, int b);

SCM_EXTERN void   Scm_BitsCopyX(ScmBits *target, int tstart,
                                ScmBits *src, int sstart, int send);

typedef enum {
    SCM_BIT_AND,                /* r = a & b */
    SCM_BIT_IOR,                /* r = a | b */
    SCM_BIT_XOR,                /* r = a ^ b */
    SCM_BIT_EQV,                /* r = ~(a ^ b) */
    SCM_BIT_NAND,               /* r = ~(a & b) */
    SCM_BIT_NOR,                /* r = ~(a | b) */
    SCM_BIT_ANDC1,              /* r = ~a & b */
    SCM_BIT_ANDC2,              /* r = a & ~b */
    SCM_BIT_IORC1,              /* r = ~a | b */
    SCM_BIT_IORC2,              /* r = a | ~b */
    SCM_BIT_XORC1,              /* r = ~a ^ b */
    SCM_BIT_XORC2,              /* r = a ^ ~b */
    SCM_BIT_SRC1,               /* r = a */
    SCM_BIT_SRC2,               /* r = b */
    SCM_BIT_NOT1,               /* r = ~a */
    SCM_BIT_NOT2,               /* r = ~b */
} ScmBitOp;

SCM_EXTERN void   Scm_BitsOperate(ScmBits *r, ScmBitOp op,
                                  const ScmBits *a, const ScmBits *b,
                                  int start, int end);

SCM_EXTERN int    Scm_BitsEqual(const ScmBits *a, const ScmBits *b,
                                int start, int end);
SCM_EXTERN int    Scm_BitsIncludes(const ScmBits *a, const ScmBits *b,
                                   int start, int end);

SCM_EXTERN int    Scm_BitsCount0(const ScmBits *bits, int start, int end);
SCM_EXTERN int    Scm_BitsCount1(const ScmBits *bits, int start, int end);

SCM_EXTERN int    Scm_BitsLowest1(const ScmBits *bits, int start, int end);
SCM_EXTERN int    Scm_BitsLowest0(const ScmBits *bits, int start, int end);
SCM_EXTERN int    Scm_BitsHighest1(const ScmBits *bits, int start, int end);
SCM_EXTERN int    Scm_BitsHighest0(const ScmBits *bits, int start, int end);

#endif /*GAUCHE_BITS_H*/
