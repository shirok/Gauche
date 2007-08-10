/*
 * bits.c - Bit manipulation utilities
 *
 *   Copyright (c) 2007  Shiro Kawai  <shiro@acm.org>
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
 *
 *  $Id: bits.c,v 1.4 2007-08-10 01:19:36 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"

/*===================================================================
 * Construct, copy, fill
 */

ScmBits *Scm_MakeBits(int numbits)
{
    return SCM_NEW_ATOMIC_ARRAY(ScmBits, SCM_BITS_NUM_WORDS(numbits));
}

void Scm_BitsCopyX(ScmBits *target, int tstart,
                   ScmBits *src, int sstart, int send)
{
    if (tstart%SCM_WORD_BITS == 0
        && sstart%SCM_WORD_BITS == 0
        && send%SCM_WORD_BITS == 0) {
        /* easy path */
        int tw = tstart/SCM_WORD_BITS;
        int sw = sstart/SCM_WORD_BITS;
        int ew = send/SCM_WORD_BITS;
        while (sw < ew) target[tw++] = src[sw++];
    } else {
        /* TODO: make this more efficient. */
        int t, s;
        for (t=tstart, s=sstart; s < send; t++, s++) {
            if (SCM_BITS_TEST(src, s)) SCM_BITS_SET(target, t);
            else                       SCM_BITS_RESET(target, t);
        }
    }
}

void Scm_BitsFill(ScmBits *bits, int start, int end, int b)
{
    int sw = start / SCM_WORD_BITS;
    int ew = end   / SCM_WORD_BITS;
    int sb = start % SCM_WORD_BITS;
    int eb = end   % SCM_WORD_BITS;

    if (sw == ew) {
        u_long mask = ((1UL<<eb) - 1) & ~((1UL<<sb) - 1);
        if (b) bits[sw] |= mask;
        else   bits[sw] &= ~mask;
    } else {
        if (b) bits[sw] |= ~((1UL<<sb)-1);
        else   bits[sw] &= ((1UL<<sb)-1);
        for (sw++; sw < ew; sw++) {
            if (b) bits[sw] = ~0UL;
            else   bits[sw] = 0;
        }
        if (b) bits[ew] |= ((1UL<<eb)-1);
        else   bits[ew] &= ~((1UL<<eb)-1);
    }
}

void Scm_BitsOperate(ScmBits *r, ScmBitOp op,
                     const ScmBits *a, const ScmBits *b,
                     int s, int e)
{
    int sw = s/SCM_WORD_BITS;
    int sb = s%SCM_WORD_BITS;
    int ew = e/SCM_WORD_BITS;
    int eb = e%SCM_WORD_BITS;
    int w;

    /* NB: Not very optimized for speed.  Rewrite when we hit a bottleneck. */
    for (w = sw;w < ew + (eb?1:0); w++) {
        u_long z = 0;
        switch (op) {
        case SCM_BIT_AND:  z = a[w] & b[w];    break;
        case SCM_BIT_IOR:  z = a[w] | b[w];    break;
        case SCM_BIT_XOR:  z = a[w] ^ b[w];    break;
        case SCM_BIT_NAND: z = ~(a[w] & b[w]); break;
        case SCM_BIT_NOR:  z = ~(a[w] | b[w]); break;
        case SCM_BIT_EQV:  z = ~(a[w] ^ b[w]); break;
        case SCM_BIT_ANDC1:z = ~a[w] & b[w];   break;
        case SCM_BIT_ANDC2:z = a[w] & ~b[w];   break;
        case SCM_BIT_IORC1:z = ~a[w] | b[w];   break;
        case SCM_BIT_IORC2:z = a[w] | ~b[w];   break;
        case SCM_BIT_XORC1:z = ~a[w] ^ b[w];   break;
        case SCM_BIT_XORC2:z = a[w] ^ ~b[w];   break;
        case SCM_BIT_SRC1: z = a[w];           break;
        case SCM_BIT_SRC2: z = b[w];           break;
        case SCM_BIT_NOT1: z = ~a[w];          break;
        case SCM_BIT_NOT2: z = ~b[w];          break;
        }
        if (w == sw && sb != 0) z &= ~((1UL<<sb)-1);
        else if (w == ew)       z &= (1UL<<eb)-1;
        r[w] = z;
    }
}

/*===================================================================
 * Comparison
 */

int Scm_BitsEqual(const ScmBits *a, const ScmBits *b, int s, int e)
{
    int sw = s/SCM_WORD_BITS;
    int sb = s%SCM_WORD_BITS;
    int ew = e/SCM_WORD_BITS;
    int eb = e%SCM_WORD_BITS;

    if (sb) {
        if (((a[sw]^b[sw])&~((1UL<<sb)-1)) != 0) return FALSE;
        else sw++;
    }
    if (eb) {
        if (((a[ew]^b[ew])& ((1UL<<eb)-1)) != 0) return FALSE;
    }
    for (;sw < ew; sw++) {
        if ((a[sw]^b[sw]) != 0) return FALSE;
    }
    return TRUE;
}

/* Returns iff all '1' bits in B is also '1' in A. */
int Scm_BitsIncludes(const ScmBits *a, const ScmBits *b, int s, int e)
{
    int sw = s/SCM_WORD_BITS;
    int sb = s%SCM_WORD_BITS;
    int ew = e/SCM_WORD_BITS;
    int eb = e%SCM_WORD_BITS;

    if (sb) {
        if (((a[sw]^(a[sw]|b[sw]))&~((1UL<<sb)-1)) != 0) return FALSE;
        else sw++;
    }
    if (eb) {
        if (((a[ew]^(a[ew]|b[ew]))& ((1UL<<eb)-1)) != 0) return FALSE;
    }
    for (;sw < ew; sw++) {
        if ((a[sw]^(a[sw]|b[sw])) != 0) return FALSE;
    }
    return TRUE;
}

/*===================================================================
 * Bit counting
 */

/* count number of '1's in the given word */
static inline u_long count_bits(u_long word)
{
#if SIZEOF_LONG == 4
    word = (word&0x55555555UL) + ((word>>1)&0x55555555UL);
    word = (word&0x33333333UL) + ((word>>2)&0x33333333UL);
    word = (word&0x0f0f0f0fUL) + ((word>>4)&0x0f0f0f0fUL);
    word = (word&0x00ff00ffUL) + ((word>>8)&0x00ff00ffUL);
    word = (word&0x0000ffffUL) + ((word>>16)&0x0000ffffUL);
#else
    word = (word&0x5555555555555555UL) + ((word>>1)&0x5555555555555555UL);
    word = (word&0x3333333333333333UL) + ((word>>2)&0x3333333333333333UL);
    word = (word&0x0f0f0f0f0f0f0f0fUL) + ((word>>4)&0x0f0f0f0f0f0f0f0fUL);
    word = (word&0x00ff00ff00ff00ffUL) + ((word>>8)&0x00ff00ff00ff00ffUL);
    word = (word&0x0000ffff0000ffffUL) + ((word>>16)&0x0000ffff0000ffffUL);
    word = (word&0x00000000ffffffffUL) + ((word>>32)&0x00000000ffffffffUL);
#endif
    return word;
}

/* count number of '1's from the start-th bit (inclusive) and end-th
   bit (exclusiv) */
int Scm_BitsCount1(const ScmBits *bits, int start, int end)
{
    int sw = start / SCM_WORD_BITS;
    int ew = end   / SCM_WORD_BITS;
    int sb = start % SCM_WORD_BITS;
    int eb = end   % SCM_WORD_BITS;
    u_long num;

    if (sw == ew) {
        u_long mask = ((1UL<<eb) - 1) & ~((1UL<<sb) - 1);
        return count_bits(bits[sw]&mask);
    }

    num = count_bits(bits[sw] & ~((1UL<<sb)-1));
    for (sw++; sw < ew; sw++) {
        num += count_bits(bits[sw]);
    }
    return num + count_bits((bits[ew])&((1UL<<eb)-1));
}

int Scm_BitsCount0(const ScmBits *bits, int start, int end)
{
    int sw = start / SCM_WORD_BITS;
    int ew = end   / SCM_WORD_BITS;
    int sb = start % SCM_WORD_BITS;
    int eb = end   % SCM_WORD_BITS;
    u_long num;

    if (sw == ew) {
        u_long mask = ((1UL<<eb) - 1) & ~((1UL<<sb) - 1);
        return count_bits(~(bits[sw]&mask));
    }

    num = count_bits(~(bits[sw] & ~((1UL<<sb)-1)));
    for (sw++; sw < ew; sw++) {
        num += count_bits(~bits[sw]);
    }
    return num + count_bits(~((bits[ew])&((1UL<<eb)-1)));
}

/*===================================================================
 * Bit finding
 */

/* Returns the bit number of the lowest '1' bit in the word, assuming
   there's at least one '1'. */
static inline int lowest_bit_number(u_long word)
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
static inline int highest_bit_number(u_long word)
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

/* Returns the lowest bit number between start and end, or -1 if all
   the bits there is zero. */
int Scm_BitsLowest1(const ScmBits *bits, int start, int end)
{
    int sw = start/SCM_WORD_BITS;
    int sb = start%SCM_WORD_BITS;
    int ew = end/SCM_WORD_BITS;
    int eb = end%SCM_WORD_BITS;
    
    if (ew == sw) {
        u_long w = bits[sw] & (((1UL<<eb)-1) & ~((1UL<<sb)-1));
        if (w) return lowest_bit_number(w) + sw*SCM_WORD_BITS;
        else   return -1;
    } else {
        u_long w = bits[sw] & ~((1UL<<sb)-1);
        if (w) return lowest_bit_number(w) + sw*SCM_WORD_BITS;
        for (;sw < ew; sw++) {
            if (bits[sw]) return lowest_bit_number(bits[sw])+sw*SCM_WORD_BITS;
        }
        w = bits[ew] & ((1UL<<eb)-1);
        if (w) return lowest_bit_number(w) + ew*SCM_WORD_BITS;
        return -1;
    }
}

int Scm_BitsLowest0(const ScmBits *bits, int start, int end)
{
    int sw = start/SCM_WORD_BITS;
    int sb = start%SCM_WORD_BITS;
    int ew = end/SCM_WORD_BITS;
    int eb = end%SCM_WORD_BITS;
    
    if (ew == sw) {
        u_long w = ~bits[sw] & (((1UL<<eb)-1) & ~((1UL<<sb)-1));
        if (w) return lowest_bit_number(w) + sw*SCM_WORD_BITS;
        else   return -1;
    } else {
        u_long w = ~bits[sw] & ~((1UL<<sb)-1);
        if (w) return lowest_bit_number(w) + sw*SCM_WORD_BITS;
        for (;sw < ew; sw++) {
            if (~bits[sw]) return lowest_bit_number(~bits[sw])+sw*SCM_WORD_BITS;
        }
        w = ~bits[ew] & ((1UL<<eb)-1);
        if (w) return lowest_bit_number(w) + ew*SCM_WORD_BITS;
        return -1;
    }
}

/* Returns the highest bit number between start and end, or -1 if all
   the bits there is zero. */
int Scm_BitsHighest1(const ScmBits *bits, int start, int end)
{
    int sw = start/SCM_WORD_BITS;
    int sb = start%SCM_WORD_BITS;
    int ew = end/SCM_WORD_BITS;
    int eb = end%SCM_WORD_BITS;
    
    if (ew == sw) {
        u_long w = bits[sw] & (((1UL<<eb)-1) & ~((1UL<<sb)-1));
        if (w) return highest_bit_number(w) + sw*SCM_WORD_BITS;
        else   return -1;
    } else {
        u_long w = bits[sw] & ~((1UL<<sb)-1);
        if (w) return highest_bit_number(w) + sw*SCM_WORD_BITS;
        for (;sw < ew; sw++) {
            if (bits[sw]) return lowest_bit_number(bits[sw])+sw*SCM_WORD_BITS;
        }
        w = bits[ew] & ((1UL<<eb)-1);
        if (w) return highest_bit_number(w) + ew*SCM_WORD_BITS;
        return -1;
    }
}

int Scm_BitsHighest0(const ScmBits *bits, int start, int end)
{
    int sw = start/SCM_WORD_BITS;
    int sb = start%SCM_WORD_BITS;
    int ew = end/SCM_WORD_BITS;
    int eb = end%SCM_WORD_BITS;
    
    if (ew == sw) {
        u_long w = ~bits[sw] & (((1UL<<eb)-1) & ~((1UL<<sb)-1));
        if (w) return highest_bit_number(w) + sw*SCM_WORD_BITS;
        else   return -1;
    } else {
        u_long w = ~bits[sw] & ~((1UL<<sb)-1);
        if (w) return highest_bit_number(w) + sw*SCM_WORD_BITS;
        for (;sw < ew; sw++) {
            if (~bits[sw]) return lowest_bit_number(~bits[sw])+sw*SCM_WORD_BITS;
        }
        w = ~bits[ew] & ((1UL<<eb)-1);
        if (w) return highest_bit_number(w) + ew*SCM_WORD_BITS;
        return -1;
    }
}


