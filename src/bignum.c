/*
 * bignum.c - multiple precision exact integer arithmetic
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: bignum.c,v 1.9 2001-04-19 08:58:04 shiro Exp $
 */

#include <math.h>
#include <limits.h>
#include "gauche.h"

/* This is a very naive implementation.  For now, I think bignum
 * performance is not very important for the purpose of Gauche.
 */
/* Cf: Knuth: The Art of Computer Programming, sectin 4.3 */

#define SCM_ULONG_MAX      ((u_long)(-1L)) /* to be configured */
#define WORD_BITS          (SIZEOF_LONG * 8)
#define HALF_BITS          (WORD_BITS/2)

#ifndef LONG_MIN
#define LONG_MIN           ((long)(1L<<(WORD_BITS-1)))
#endif
#ifndef LONG_MAX
#define LONG_MAX           (-(LONG_MIN+1))
#endif

#define LOMASK             ((1L<<HALF_BITS)-1)
#define LO(word)           ((word) & LOMASK)
#define HI(word)           (((word) >> HALF_BITS)&LOMASK)

static ScmBignum *bignum_rshift(ScmBignum *br, ScmBignum *bx, int amount);
static ScmBignum *bignum_lshift(ScmBignum *br, ScmBignum *bx, int amount);

/*---------------------------------------------------------------------
 * Constructor
 */
static ScmBignum *make_bignum(int size)
{
    ScmBignum *b = SCM_NEW_ATOMIC2(ScmBignum*,
                                   sizeof(ScmBignum)+(size-1)*sizeof(long));
    SCM_SET_CLASS(b, SCM_CLASS_INTEGER);
    b->size = size;
    return b;
}

static ScmBignum *bignum_clear(ScmBignum *b)
{
    int i;
    for (i=0; i<b->size; i++) b->values[i] = 0;
    return b;
}

ScmObj Scm_MakeBignumFromSI(long val)
{
    ScmBignum *b;
    if (val == LONG_MIN) {
        b = make_bignum(2);
        b->sign = -1;
        b->values[0] = 0;
        b->values[1] = 1;
    } else if (val < 0) {
        b = make_bignum(1);
        b->sign = -1;
        b->values[0] = -val;
    } else {
        b = make_bignum(1);
        b->sign = 1;
        b->values[0] = val;
    }
    return SCM_OBJ(b);
}

ScmObj Scm_MakeBignumFromUI(u_long val)
{
    ScmBignum *b = make_bignum(1);
    b->sign = 1;
    b->values[0] = val;
    return SCM_OBJ(b);
}

ScmObj Scm_MakeBignumFromDouble(double val)
{
    double absval, fraction, fbit;
    int exponent, i, nwords;
    u_long lval = 0, mask;
    ScmBignum *b;

    if (val >= LONG_MIN && val <= LONG_MAX) {
        return Scm_MakeBignumFromSI((long)val);
    }
    /* NB: strangely, in all documents I saw, the behavior of frexp()
       when it is given a negative floating point number is not documented
       explicitly. */
    absval = fabs(val);
    fraction = frexp(absval, &exponent);
    fprintf(stderr, "fraction=%f, exponent=%d, ", fraction, exponent);
    for (mask = (1L<<(WORD_BITS-1)), fbit = 0.5;
         mask != 0 && fbit > 0.0 && fraction > 0;
         mask>>=1, fbit /= 2) {
        if (fraction >= fbit) {
            lval |= mask;
            fraction -= fbit;
        }
    }
    nwords = (exponent + WORD_BITS - 1)/WORD_BITS;
    fprintf(stderr, "nwords=%d, lval=%08x\n", nwords, lval);
    b = make_bignum(nwords);
    bignum_clear(b);
    b->sign = (val < 0)? -1 : 1;
    b->values[0] = lval;
    SCM_ASSERT(exponent >= WORD_BITS);
    bignum_lshift(b, b, exponent - WORD_BITS);
    return Scm_NormalizeBignum(b);
}

ScmObj Scm_BignumCopy(ScmBignum *b)
{
    int i;
    ScmBignum *c = make_bignum(b->size);
    c->sign = b->sign;
    for (i=0; i<b->size; i++) c->values[i] = b->values[i];
    return SCM_OBJ(c);
}

/*-----------------------------------------------------------------------
 * Conversion
 */

ScmObj Scm_NormalizeBignum(ScmBignum *b)
{
    int size = b->size;
    int i;
    for (i=size-1; i>0; i--) {
        if (b->values[i] == 0) size--;
        else break;
    }
    if (i==0) {
        if (b->sign == 0) {
            return SCM_MAKE_INT(0);
        }
        if (b->sign > 0 && b->values[0] <= SCM_SMALL_INT_MAX) {
            return SCM_MAKE_INT(b->values[0]);
        }
        if (b->sign < 0 && b->values[0] <= -SCM_SMALL_INT_MIN) {
            return SCM_MAKE_INT(-b->values[0]);
        }
    }
    b->size = size;
    return SCM_OBJ(b);
}

/* b must be normalized.  result is clipped between [LONG_MIN, LONG_MAX] */
long Scm_BignumToSI(ScmBignum *b) 
{
    if (b->sign >= 0) {
        long r = (long)b->values[0];
        return (r < 0)? LONG_MAX : r;
    } else if (b->values[0] == 0 && b->values[1]) {
        return LONG_MIN;
    } else {
        return -(long)b->values[0];
    }
}

/* b must be normalized.  result is rounded between [0, ULONG_MAX] */
u_long Scm_BignumToUI(ScmBignum *b) 
{
    if (b->sign >= 0) {
        return b->values[0];
    } else {
        return 0;
    }
}

double Scm_BignumToDouble(ScmBignum *b) /* b must be normalized */
{
    double r;
    switch (b->size) {
    case 0: r = 0.0; break;
    case 1: r = (double)b->values[0]; break;
    case 2:
        r = ldexp((double)b->values[1], WORD_BITS) + (double)b->values[0];
        break;
    default:
        r = ldexp((double)b->values[b->size-1], WORD_BITS*(b->size-1))
            + ldexp((double)b->values[b->size-2], WORD_BITS*(b->size-2))
            + ldexp((double)b->values[b->size-3], WORD_BITS*(b->size-3));
        break;
    }
    return (b->sign < 0)? -r : r;
}

/* return -b, normalized */
ScmObj Scm_BignumNegate(ScmBignum *b)
{
    ScmObj c = Scm_BignumCopy(b);
    SCM_BIGNUM_SIGN(c) = -SCM_BIGNUM_SIGN(c);
    return Scm_NormalizeBignum(SCM_BIGNUM(c));
}
    
/*-----------------------------------------------------------------------
 * Compare
 */

/* bx and by must be normalized */
int Scm_BignumCmp(ScmBignum *bx, ScmBignum *by)
{
    int i;
    
    if (bx->sign < by->sign) return -1;
    if (bx->sign > by->sign) return 1;
    if (bx->size < by->size) return (bx->sign > 0) ? -1 : 1;
    if (bx->size > by->size) return (bx->sign > 0) ? 1 : -1;

    for (i=bx->size-1; i>=0; i--) {
        if (bx->values[i] < by->values[i]) return (bx->sign > 0) ? -1 : 1;
        if (bx->values[i] > by->values[i]) return (bx->sign > 0) ? 1 : -1;
    }
    return 0;
}

/*-----------------------------------------------------------------------
 * Add & subtract
 */
static int bignum_safe_size_for_add(ScmBignum *x, ScmBignum *y)
{
    int xsize = SCM_BIGNUM_SIZE(x);
    int ysize = SCM_BIGNUM_SIZE(y);
    if (xsize > ysize) {
        if (x->values[xsize-1] == SCM_ULONG_MAX) return xsize+1;
        else return xsize;
    } else if (ysize > xsize) {
        if (y->values[ysize-1] == SCM_ULONG_MAX) return ysize+1;
        else return ysize;
    } else {
        return xsize+1;
    }
}

#define UADD(r, c, x, y)                        \
    r = x + y + c;                              \
    c = (r<x || (r==x && (y>0||c>0)))? 1 : 0

#define USUB(r, c, x, y)                        \
    r = x - y - c;                              \
    c = (r>x || (r==x && (y>0||c>0)))? 1 : 0

/* take 2's complement */
static ScmBignum *bignum_2scmpl(ScmBignum *br)
{
    int rsize = SCM_BIGNUM_SIZE(br);
    int i, c;
    for (i=0, c=1; i<rsize; i++) {
        long x = ~br->values[i];
        UADD(br->values[i], c, x, 0);
    }
    SCM_ASSERT(c == 0);
    return br;
}

/* br = abs(bx) + abs(by), assuming br has enough size. br and bx can be
   the same object. */
static ScmBignum *bignum_add_int(ScmBignum *br, ScmBignum *bx, ScmBignum *by)
{
    int rsize = SCM_BIGNUM_SIZE(br);
    int xsize = SCM_BIGNUM_SIZE(bx);
    int ysize = SCM_BIGNUM_SIZE(by);
    int i, c;
    u_long x, y;

    for (i=0, c=0; i<rsize; i++, xsize--, ysize--) {
        if (xsize <= 0) {
            if (ysize <= 0) {
                UADD(br->values[i], c, 0, 0);
                continue;
            }
            y = by->values[i];
            UADD(br->values[i], c, 0, y);
            continue;
        }
        if (ysize <= 0) {
            x = bx->values[i];
            UADD(br->values[i], c, x, 0);
            continue;
        }
        x = bx->values[i];
        y = by->values[i];
        UADD(br->values[i], c, x, y);
    }
    return br;
}

/* br = abs(bx) - abs(by), assuming br has enough size.  br and bx can be
   the same object. */
static ScmBignum *bignum_sub_int(ScmBignum *br, ScmBignum *bx, ScmBignum *by)
{
    int rsize = SCM_BIGNUM_SIZE(br);
    int xsize = SCM_BIGNUM_SIZE(bx);
    int ysize = SCM_BIGNUM_SIZE(by);
    int i, c;
    u_long x, y;

    for (i=0, c=0; i<rsize; i++, xsize--, ysize--) {
        if (xsize <= 0) {
            if (ysize <= 0) {
                USUB(br->values[i], c, 0, 0);
                continue;
            }
            y = by->values[i];
            USUB(br->values[i], c, 0, y);
            continue;
        }
        if (ysize <= 0) {
            x = bx->values[i];
            USUB(br->values[i], c, x, 0);
            continue;
        }
        x = bx->values[i];
        y = by->values[i];
        USUB(br->values[i], c, x, y);
    }
    if (c != 0) {
        bignum_2scmpl(br);
        br->sign = 0 - br->sign; /* flip sign */
    }
    return br;
}

/* returns bx + by, not normalized */
static ScmBignum *bignum_add(ScmBignum *bx, ScmBignum *by)
{
    int rsize = bignum_safe_size_for_add(bx, by);
    ScmBignum *br = make_bignum(rsize);
    br->sign = SCM_BIGNUM_SIGN(bx);
    if (SCM_BIGNUM_SIGN(bx) == SCM_BIGNUM_SIGN(by)) {
        bignum_add_int(br, bx, by);
    } else {
        bignum_sub_int(br, bx, by);
    }
    return br;
}

/* returns bx - by, not normalized */
static ScmBignum *bignum_sub(ScmBignum *bx, ScmBignum *by)
{
    int rsize = bignum_safe_size_for_add(bx, by);
    ScmBignum *br = make_bignum(rsize);
    br->sign = SCM_BIGNUM_SIGN(bx);
    if (SCM_BIGNUM_SIGN(bx) == SCM_BIGNUM_SIGN(by)) {
        bignum_sub_int(br, bx, by);
    } else {
        bignum_add_int(br, bx, by);
    }
    return br;
}

/* returns bx + y, not nomalized */
static ScmBignum *bignum_add_si(ScmBignum *bx, long y)
{
    long r, c;
    int i, rsize = bx->size+1;
    int yabs = ((y < 0)? -y : y);
    int ysign = ((y < 0)? -1 : 1);
    ScmBignum *br;

    if (y == 0) return bx;
    
    br = make_bignum(rsize);
    br->sign = bx->sign;
    if (SCM_BIGNUM_SIGN(bx) == ysign) {
        for (c=0, i=0; i<bx->size; i++) {
            UADD(br->values[i], c, bx->values[i], yabs);
            yabs = 0;
        }
    } else {
        for (c=0, i=0; i<bx->size; i++) {
            USUB(br->values[i], c, bx->values[i], yabs);
            yabs = 0;
        }
    }
    br->values[rsize-1] = c;
    return br;
}

ScmObj Scm_BignumAdd(ScmBignum *bx, ScmBignum *by)
{
    return Scm_NormalizeBignum(bignum_add(bx, by));
}

ScmObj Scm_BignumSub(ScmBignum *bx, ScmBignum *by)
{
    return Scm_NormalizeBignum(bignum_sub(bx, by));
}

ScmObj Scm_BignumAddSI(ScmBignum *bx, long y)
{
    return Scm_NormalizeBignum(bignum_add_si(bx, y));
}

ScmObj Scm_BignumSubSI(ScmBignum *bx, long y)
{
    return Scm_NormalizeBignum(bignum_add_si(bx, -y));
}

ScmObj Scm_BignumAddN(ScmBignum *bx, ScmObj args)
{
    ScmBignum *r = bx;
    for (;SCM_PAIRP(args); args = SCM_CDR(args)) {
        ScmObj v = SCM_CAR(args);
        if (SCM_INTP(v)) {
            r = bignum_add_si(r, SCM_INT_VALUE(v));
            continue;
        }
        if (SCM_BIGNUMP(v)) {
            r = bignum_add(r, SCM_BIGNUM(v));
            continue;
        }
        if (SCM_FLONUMP(v) || SCM_COMPLEXP(v)) {
            ScmObj z = Scm_MakeFlonum(Scm_BignumToDouble(r));
            return Scm_Add(Scm_Cons(z, args));
        }
        Scm_Error("number expected, but got %S", v);
    }
    return Scm_NormalizeBignum(r);
}

ScmObj Scm_BignumSubN(ScmBignum *bx, ScmObj args)
{
    ScmBignum *r = bx;
    for (;SCM_PAIRP(args); args = SCM_CDR(args)) {
        ScmObj v = SCM_CAR(args);
        if (SCM_INTP(v)) {
            r = bignum_add_si(r, -SCM_INT_VALUE(v));
            continue;
        }
        if (SCM_BIGNUMP(v)) {
            r = bignum_sub(r, SCM_BIGNUM(v));
            continue;
        }
        if (SCM_FLONUMP(v) || SCM_COMPLEXP(v)) {
            ScmObj z = Scm_MakeFlonum(Scm_BignumToDouble(r));
            return Scm_Subtract(z, v, SCM_CDR(args));
        }
        Scm_Error("number expected, but got %S", v);
    }
    return Scm_NormalizeBignum(r);
}

/*-----------------------------------------------------------------------
 * Shifter
 */

/* br = bx >> amount.  amount >= 0.  no normalization.  assumes br
   has enough size to hold the result.  br and bx can be the same object. */
static ScmBignum *bignum_rshift(ScmBignum *br, ScmBignum *bx, int amount)
{
    int nwords = amount / WORD_BITS;
    int nbits = amount % WORD_BITS;
    int i;
    
    if (bx->size <= nwords) {
        br->size = 0; br->values[0] = 0;
    } else {
        u_long prev = 0, x;
        for (i = nwords; i < bx->size; i--) {
            x = (bx->values[i] >> nbits) | prev;
            prev = (bx->values[i] << (WORD_BITS - nbits));
            br->values[i-nwords] = x;
        }
        br->size = bx->size - nwords;
        br->sign = bx->sign;
    }
    return br;
}

/* br = bx << amount, amount > 0.   no normalization.   assumes br
   has enough size.  br and bx can be the same object. */
static ScmBignum *bignum_lshift(ScmBignum *br, ScmBignum *bx, int amount)
{
    int nwords, nbits, i;
    u_long prev = 0, x, mask;
    
    if (amount == 0) {
        if (br != bx) {
            /* copy it */
            br->size = bx->size;
        }
        return br;
    }

    nwords = amount / WORD_BITS;
    nbits = amount % WORD_BITS;
    if (nbits == 0) {
        /* short path */
        for (i = bx->size-1; i>=0; i--) {
            if (br->size > i+nwords) br->values[i+nwords] = bx->values[i];
        }
        for (i = nwords-1; i>=0; i--) br->values[i] = 0;
    } else {
        mask = (1L<<nbits)-1;    
        for (i = bx->size-1; i >= 0; i--) {
            x = ((bx->values[i] >> (WORD_BITS - nbits)) & mask) | prev;
            prev = (bx->values[i] << nbits);
            if (br->size > i+nwords) br->values[i+nwords+1] = x;
        }
        br->values[nwords] = prev;
        for (i = nwords-1; i>=0; i--) br->values[i] = 0;
    }
    if (br != bx) {
        br->size = bx->size + nwords;
        br->sign = bx->sign;
    }
    return br;
}

/*-----------------------------------------------------------------------
 * Multiplication
 */

/* Multiply two unsigned long x and y, and save higher bits of result
   to hi and lower to lo.   Most modern CPUs must have a special instruction
   to do this.  The following is a portable, but extremely slow, version. */
#define UMUL(hi, lo, x, y)                                              \
    do {                                                                \
        u_long xl_ = LO(x), xh_ = HI(x), yl_ = LO(y), yh_ = HI(y);      \
        u_long t1_, t2_, t3_, t4_;                                      \
        lo = xl_ * yl_;                                                 \
        t1_ = xl_ * yh_;                                                \
        t2_ = xh_ * yl_;                                                \
        hi = xh_ * yh_;                                                 \
        t3_ = t1_ + t2_;                                                \
        if (t3_ < t1_) hi += (1L<<HALF_BITS);                           \
        hi += HI(t3_);                                                  \
        t4_ = LO(t3_) << HALF_BITS;                                     \
        lo += t4_;                                                      \
        if (lo < t4_) hi++;                                             \
    } while (0)

/* br += bx * y << off*WORD_BITS.   br must have enough size. */
static ScmBignum *bignum_mul_word(ScmBignum *br, ScmBignum *bx,
                                  u_long y, int off)
{
    u_long hi, lo, x, r0, r1, c;
    int i,j;
    
    for (i=0; i<bx->size; i++) {
        x = bx->values[i];
        UMUL(hi, lo, x, y);
        c = 0;

        r0 = br->values[i+off];
        UADD(r1, c, r0, lo);
        br->values[i+off] = r1;

        r0 = br->values[i+off+1];
        UADD(r1, c, r0, hi);
        br->values[i+off+1] = r1;

        for (j=i+off+2; c && j<br->size; j++) {
            r0 = br->values[j];
            UADD(r1, c, r0, 0);
            br->values[j] = r1;
        }
    }
    return br;
}

/* returns bx * by.  not normalized */
static ScmBignum *bignum_mul(ScmBignum *bx, ScmBignum *by)
{
    int i;
    ScmBignum *br = make_bignum(bx->size + by->size);
    bignum_clear(br);
    for (i=0; i<by->size; i++) {
        bignum_mul_word(br, bx, by->values[i], i);
    }
    br->sign = bx->sign * by->sign;
    return br;
}

static ScmBignum *bignum_mul_si(ScmBignum *bx, long y)
{
    ScmBignum *br;
    int yabs;
    
    if (y == 1) return bx;
    if (y == 0) {
        br = make_bignum(1);
        br->sign = 1;
        br->values[0] = 0;
        return br;
    }
    if (y == -1) {
        br = SCM_BIGNUM(Scm_BignumCopy(bx));
        br->sign = -br->sign;
        return br;
    }
    /* TODO: optimize for 2^n case !*/
    br = make_bignum(bx->size + 1); /* TODO: more accurate estimation */
    bignum_clear(br);
    yabs = (y<0)? -y:y;
    br->sign = bx->sign;
    bignum_mul_word(br, bx, yabs, 0);
    if (y<0) br->sign = -br->sign;
    return br;
}

ScmObj Scm_BignumMul(ScmBignum *bx, ScmBignum *by)
{
    ScmBignum *br = bignum_mul(bx, by);
    return Scm_NormalizeBignum(br);
}

ScmObj Scm_BignumMulSI(ScmBignum *bx, long y)
{
    ScmBignum *br = bignum_mul_si(bx, y);
    return Scm_NormalizeBignum(br);
}

ScmObj Scm_BignumMulN(ScmBignum *bx, ScmObj args)
{
    ScmBignum *r = bx;
    for (; SCM_PAIRP(args); args = SCM_CDR(args)) {
        ScmObj v = SCM_CAR(args);
        if (SCM_INTP(v)) {
            r = bignum_mul_si(r, SCM_INT_VALUE(v));
            continue;
        }
        if (SCM_BIGNUMP(v)) {
            r = bignum_mul(r, SCM_BIGNUM(v));
            continue;
        }
        if (SCM_FLONUMP(v) || SCM_COMPLEXP(v)) {
            ScmObj f = Scm_MakeFlonum(Scm_BignumToDouble(r));
            return Scm_Multiply(Scm_Cons(f, args));
        }
        Scm_Error("number expected, but got %S", v);
    }
    return Scm_NormalizeBignum(r);
}

/*-----------------------------------------------------------------------
 * Division
 */

/* returns # of bits in the leftmost '1' in the word, counting from MSB. */
static inline int div_normalization_factor(u_long w)
{
    int b = (1L<<(WORD_BITS-1)), c = 0;
    for (; b > 0; b>>=1, c++) {
        if (w & b) return c;
    }
    /* something got wrong here */
    Scm_Panic("bignum.c: div_normalization_factor: can't be here");
    return 0;                   /* dummy */
}

/* General case of division.  We use each half word as a digit. 
   Assumes digitsof(dividend) >= digitsof(divisor) > 1.
   Assumes enough digits are allocated to quotient and remainder. */
static void bignum_gdiv(ScmBignum *dividend, ScmBignum *divisor,
                        ScmBignum *quotient, ScmBignum *remainder)
{
    ScmBignum *u, *v;
    int d = div_normalization_factor(divisor->values[divisor->size-1]);
    int j, k, l, n, m, cy;
    u_long vn_1, vn_2, vv, uj;

#define U(n)   ((n%2)? LO(u->values[n/2]) : HI(u->values[n/2]))
#define SETU(n, v) \
    ((n%2)? \
     (u->values[n/2] = (u->values[n/2] & ~LOMASK)|(v & LOMASK)) : \
     (u->values[n/2] = (u->values[n/2] & LOMASK)|(v << HALF_BITS)))
#define V(n)   ((n%2)? LO(v->values[n/2]) : HI(v->values[n/2]))
#define SETQ(n, v) \
    ((n%2)? \
     (quotient->values[n/2] = (quotient->values[n/2] & ~LOMASK)|(v & LOMASK)) : \
     (quotient->values[n/2] = (quotient->values[n/2] & LOMASK)|(v << HALF_BITS)))
#define R(n)   ((n%2)? LO(remainder->values[n/2]) : HI(remainder->values[n/2]))

    /* normalize */
    u = make_bignum(dividend->size + 1);
    v = make_bignum(divisor->size);
    bignum_lshift(u, dividend, d);
    bignum_lshift(v, divisor, d);
    n = divisor->size*2;
    m = dividend->size*2 - n;
    vn_1 = V(n-1);
    vn_2 = V(n-2);

    for (j = m; j >= 0; j++) {
        u_long uu = (U(j+n) << HALF_BITS) + U(j+n-1);
        u_long qq = uu/vn_1;
        u_long rr = uu%vn_1;
        if (qq == (1L<<HALF_BITS)) { qq--; rr += vn_1; }
        while ((qq*vn_2 > (rr<<HALF_BITS)+vn_2) && (rr < (1L<<HALF_BITS))) {
            qq--; rr += vn_1;
        }
        cy = 0;
        for (k = 0; k < n; k++) {
            vv = qq * V(k);
            uj = U(j+k) - vv - cy;
            cy =  (uj > U(j+k))? -1 : 0;
            SETU(j+k, uj);
        }
        uj = U(j+n) - cy;
        cy = (uj > U(j+n))? -1 : 0;
        SETU(j+n, uj);
        
        if (cy < 0) {
            qq--;
            cy = 0;
            for (k = 0; k < n; k++) {
                vv = V(k);
                uj = U(j+k) + vv + cy;
                cy = (uj < U(j+k))? 1 : 0;
                SETU(j+k, uj);
            }
            uj = U(j+n) + cy;
            SETU(j+n, uj);
        }
        SETQ(j, qq);
    }

    
}

/* Fast path if divisor fits in a half word.  Quotient remains in the
   dividend's memory.   Remainder returned.  Quotient not normalized. */
static u_long bignum_sdiv(ScmBignum *dividend, u_long divisor)
{
    int n = dividend->size - 1;
    u_long *pu = dividend->values;
    u_long q0 = 0, r0 = 0, q1, r1;

    for (; n > 0; n--) {
        q1 = pu[n] / divisor + q0;
        r1 = ((pu[n] % divisor) << HALF_BITS) + HI(pu[n-1]);
        q0 = ((r1 / divisor) << HALF_BITS);
        r0 = r1 % divisor;
        pu[n] = q1;
        pu[n-1] = (r0 << HALF_BITS) + LO(pu[n-1]);
    }
    q1 = pu[0] / divisor + q0;
    r1 = pu[0] % divisor;
    pu[0] = q1;
    return r1;
}

ScmObj Scm_BignumDivSI(ScmBignum *dividend, long divisor, long *remainder)
{
    u_long dd = (divisor < 0)? -divisor : divisor;
    u_long rr;
    ScmBignum *q = SCM_BIGNUM(Scm_BignumCopy(dividend));

    if (dd < (1L<<HALF_BITS)) {
        rr = bignum_sdiv(q, dd);
    } else {
        Scm_Error("sorry, not supported\n");
    }
    if (remainder) *remainder = (q->sign < 0)? -rr : rr;
    return Scm_NormalizeBignum(q);
}

/*-----------------------------------------------------------------------
 * Printing
 */

ScmObj Scm_BignumToString(ScmBignum *b, int radix)
{
    static const char tab[] = "0123456789abcdefghijklmnopqrstuvwxyz";
    ScmObj h = SCM_NIL, t = SCM_NIL;
    ScmBignum *q;
    long rem;
    if (radix < 2 || radix > 36)
        Scm_Error("radix out of range: %d", radix);
    q = SCM_BIGNUM(Scm_BignumCopy(b));
    for (;q->size > 0;) {
        rem = bignum_sdiv(q, radix);
        SCM_ASSERT(rem >= 0 && rem < radix);
        SCM_APPEND1(h, t, SCM_MAKE_CHAR(tab[rem]));
        for (; q->values[q->size-1] == 0 && q->size > 0; q->size--)
            ;
    }
    if (q->sign < 0) SCM_APPEND1(h, t, SCM_MAKE_CHAR('-'));
    return Scm_ListToString(Scm_ReverseX(h));
}

int Scm_DumpBignum(ScmBignum *b, ScmPort *out)
{
    int i;
    Scm_Printf(out, "#<bignum ");
    if (b->sign < 0) SCM_PUTC('-', out);
    for (i=b->size-1; i>=0; i--) {
        Scm_Printf(out, "%08x ", b->values[i]);
    }
    SCM_PUTC('>', out);
    return 0;
}
