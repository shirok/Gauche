/*
 * gauche/bits_inline.h - Some speed-sensitive bit-manipulation routines
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

#ifndef GAUCHE_BITS_INLINE_H
#define GAUCHE_BITS_INLINE_H

/*
 * These routines are a part of 'bits' module (src/bits.c), but splitted
 * here so that other speed-sensitive modules can use them.
 * THIS IS NOT FOR GENERAL INCLUSION.  Only sources that needs these
 * routines should explicitly #include <gauche/bits_inline.h>.
 *
 * Some of these routines can be further optimized for specific
 * architectures if the CPU has special instructions.   If needed,
 * we'll gradually implement such specialized optimizations.
 */

/* Counts '1' bits within a word */
static inline u_long Scm__CountBitsInWord(u_long word)
{
#if SIZEOF_LONG == 4
    word = (word&0x55555555UL) + ((word>>1)&0x55555555UL);
    word = (word&0x33333333UL) + ((word>>2)&0x33333333UL);
    word = (word&0x0f0f0f0fUL) + ((word>>4)&0x0f0f0f0fUL);
    word *= 0x01010101UL;
    return word >> 24;
#else
    word = (word&0x5555555555555555UL) + ((word>>1)&0x5555555555555555UL);
    word = (word&0x3333333333333333UL) + ((word>>2)&0x3333333333333333UL);
    word = (word&0x0f0f0f0f0f0f0f0fUL) + ((word>>4)&0x0f0f0f0f0f0f0f0fUL);
    word *= 0x0101010101010101UL;
    return word >> 56;
#endif
}

/* Counts '1' bits within a word, _below_ the n-th bit (exclusive) */
static inline u_long Scm__CountBitsBelow(u_long word, int n)
{
    u_long mask = (1UL<<n)-1;
    return Scm__CountBitsInWord(word&mask);
}

/* Returns the bit number of the lowest '1' bit in the word, assuming
   there's at least one '1'. */
static inline int Scm__LowestBitNumber(u_long word)
{
    int n = 0;
    word ^= (word&(word-1));    /* leave the rightmost '1' only */

#if SIZEOF_LONG == 4
    if (word&0xffff0000) n += 16;
    if (word&0xff00ff00) n += 8;
    if (word&0xf0f0f0f0) n += 4;
    if (word&0xcccccccc) n += 2;
    if (word&0xaaaaaaaa) n += 1;
#else
    if (word&0xffffffff00000000) n += 32;
    if (word&0xffff0000ffff0000) n += 16;
    if (word&0xff00ff00ff00ff00) n += 8;
    if (word&0xf0f0f0f0f0f0f0f0) n += 4;
    if (word&0xcccccccccccccccc) n += 2;
    if (word&0xaaaaaaaaaaaaaaaa) n += 1;
#endif
    return n;
}

/* Returns the bit number of the highest '1' bit in the word, assuming
   there's at least one '1'. */
static inline int Scm__HighestBitNumber(u_long word)
{
    int n = 0;
    u_long z;

#if SIZEOF_LONG == 4
    if ((z = word&0xffff0000) != 0) { n += 16; word = z; }
    if ((z = word&0xff00ff00) != 0) { n += 8;  word = z; }
    if ((z = word&0xf0f0f0f0) != 0) { n += 4;  word = z; }
    if ((z = word&0xcccccccc) != 0) { n += 2;  word = z; }
    return (word&0xaaaaaaaa)? n+1 : n;
#else
    if ((z = word&0xffffffff00000000) != 0) { n += 32; word = z; }
    if ((z = word&0xffff0000ffff0000) != 0) { n += 16; word = z; }
    if ((z = word&0xff00ff00ff00ff00) != 0) { n += 8;  word = z; }
    if ((z = word&0xf0f0f0f0f0f0f0f0) != 0) { n += 4;  word = z; }
    if ((z = word&0xcccccccccccccccc) != 0) { n += 2;  word = z; }
    return (word&0xaaaaaaaaaaaaaaaa)? n+1 : n;
#endif
}



#endif /*GAUCHE_BITS_INLINE_H*/
