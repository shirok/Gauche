/*
 * bits.c - Bit manipulation utilities
 *
 *   Copyright (c) 2007-2015  Shiro Kawai  <shiro@acm.org>
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

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/bits_inline.h"

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

    /* NB: Not very optimized for speed.  Rewrite when we hit a bottleneck. */
    for (int w = sw; w < ew + (eb?1:0); w++) {
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

#define count_bits Scm__CountBitsInWord /* defined in bits_inline.h */

/* count number of '1's from the start-th bit (inclusive) and end-th
   bit (exclusiv) */
int Scm_BitsCount1(const ScmBits *bits, int start, int end)
{
    int sw = start  / SCM_WORD_BITS;
    int ew = (end-1)/ SCM_WORD_BITS;
    int sb = start  % SCM_WORD_BITS;
    int eb = end    % SCM_WORD_BITS;

    if (start == end) return 0;
    if (sw == ew) return count_bits(bits[sw] & SCM_BITS_MASK(sb, eb));

    u_long num = count_bits(bits[sw] & SCM_BITS_MASK(sb, 0));
    for (sw++; sw < ew; sw++) num += count_bits(bits[sw]);
    return num + (count_bits((bits[ew]) & SCM_BITS_MASK(0, eb)));
}

int Scm_BitsCount0(const ScmBits *bits, int start, int end)
{
    int sw = start  / SCM_WORD_BITS;
    int ew = (end-1)/ SCM_WORD_BITS;
    int sb = start  % SCM_WORD_BITS;
    int eb = end    % SCM_WORD_BITS;

    if (start == end) return 0;
    if (sw == ew) return count_bits(~bits[sw] & SCM_BITS_MASK(sb, eb));

    u_long num = count_bits(~bits[sw] & SCM_BITS_MASK(sb, 0));
    for (sw++; sw < ew; sw++) num += count_bits(~bits[sw]);
    return num + (count_bits(~bits[ew] & SCM_BITS_MASK(0, eb)));
}

/*===================================================================
 * Bit finding
 */

#define lowest  Scm__LowestBitNumber
#define highest Scm__HighestBitNumber

/* Returns the lowest bit number between start and end, or -1 if all
   the bits there is zero. */
int Scm_BitsLowest1(const ScmBits *bits, int start, int end)
{
    int sw = start/SCM_WORD_BITS;
    int sb = start%SCM_WORD_BITS;
    int ew = (end-1)/SCM_WORD_BITS;
    int eb = end%SCM_WORD_BITS;

    if (start == end) return -1;
    if (ew == sw) {
        u_long w = bits[sw] & SCM_BITS_MASK(sb, eb);
        if (w) return lowest(w) + sw*SCM_WORD_BITS;
        else   return -1;
    } else {
        u_long w = bits[sw] & SCM_BITS_MASK(sb, 0);
        if (w) return lowest(w) + sw*SCM_WORD_BITS;
        for (;sw < ew; sw++) {
            if (bits[sw]) return lowest(bits[sw])+sw*SCM_WORD_BITS;
        }
        w = bits[ew] & SCM_BITS_MASK(0, eb);
        if (w) return lowest(w) + ew*SCM_WORD_BITS;
        return -1;
    }
}

int Scm_BitsLowest0(const ScmBits *bits, int start, int end)
{
    int sw = start/SCM_WORD_BITS;
    int sb = start%SCM_WORD_BITS;
    int ew = (end-1)/SCM_WORD_BITS;
    int eb = end%SCM_WORD_BITS;

    if (start == end) return -1;
    if (ew == sw) {
        u_long w = ~bits[sw] & SCM_BITS_MASK(sb, eb);
        if (w) return lowest(w) + sw*SCM_WORD_BITS;
        else   return -1;
    } else {
        u_long w = ~bits[sw] & SCM_BITS_MASK(sb, 0);
        if (w) return lowest(w) + sw*SCM_WORD_BITS;
        for (;sw < ew; sw++) {
            if (~bits[sw]) return lowest(~bits[sw])+sw*SCM_WORD_BITS;
        }
        w = ~bits[ew] & SCM_BITS_MASK(0, eb);
        if (w) return lowest(w) + ew*SCM_WORD_BITS;
        return -1;
    }
}

/* Returns the highest bit number between start and end, or -1 if all
   the bits there is zero. */
int Scm_BitsHighest1(const ScmBits *bits, int start, int end)
{
    int sw = start/SCM_WORD_BITS;
    int sb = start%SCM_WORD_BITS;
    int ew = (end-1)/SCM_WORD_BITS;
    int eb = end%SCM_WORD_BITS;

    if (start == end) return -1;
    if (ew == sw) {
        u_long w = bits[sw] & SCM_BITS_MASK(sb, eb);
        if (w) return highest(w) + sw*SCM_WORD_BITS;
        else   return -1;
    } else {
        u_long w = bits[ew] & SCM_BITS_MASK(0, eb);
        if (w) return highest(w) + ew*SCM_WORD_BITS;
        for (ew--;sw < ew; ew--) {
            if (bits[ew]) return highest(bits[ew])+ew*SCM_WORD_BITS;
        }
        w = bits[sw] & SCM_BITS_MASK(sb, 0);
        if (w) return highest(w) + sw*SCM_WORD_BITS;
        return -1;
    }
}

int Scm_BitsHighest0(const ScmBits *bits, int start, int end)
{
    int sw = start/SCM_WORD_BITS;
    int sb = start%SCM_WORD_BITS;
    int ew = (end-1)/SCM_WORD_BITS;
    int eb = end%SCM_WORD_BITS;

    if (start == end) return -1;
    if (ew == sw) {
        u_long w = ~bits[sw] & SCM_BITS_MASK(sb, eb);
        if (w) return highest(w) + sw*SCM_WORD_BITS;
        else   return -1;
    } else {
        u_long w = ~bits[ew] & SCM_BITS_MASK(0, eb);
        if (w) return highest(w) + ew*SCM_WORD_BITS;
        for (ew--;sw < ew; ew--) {
            if (~bits[ew]) return highest(~bits[ew])+ew*SCM_WORD_BITS;
        }
        w = ~bits[sw] & SCM_BITS_MASK(sb, 0);
        if (w) return highest(w) + sw*SCM_WORD_BITS;
        return -1;
    }
}
