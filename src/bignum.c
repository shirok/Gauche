/*
 * bignum.c - multiple precision exact integer arithmetic
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

/* Bignum library.  Not optimized well yet---I think bignum performance
 * is not very critical for Gauche, except a few special cases (like
 * the cases used in the numeric I/O routine).  So the implementation
 * emphasizes robustness rather than performance.
 *
 * Bignum is represented by ScmBignum structure.  There are "normalized"
 * and "denormalized" bignums.   Scheme part only sees the normalized
 * bignums.  Normalized bignum uses the minimum words to represent the
 * given number, and no normalized bignums for the numbers that can be
 * representable in fixnum.   Most external bignum API expects normalized
 * bignums, and return normalized bignums.   Normalized bignums should
 * be seen as read-only construct.
 *
 * Denormalized bignums are used to keep intermediate results.
 * Denormalized forms shouldn't "leak out" to the Scheme world; but
 * can be useful to write efficient routine.
 */
/* Cf: Knuth: The Art of Computer Programming, sectin 4.3 */

/* They say AIX requires this to be the first thing in the file, so
   I include gauche/config.h before the real "gauche.h" */
#include <gauche/config.h>
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
#pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#else
# if HAVE_ALLOCA_H
#  include <alloca.h>
# endif
# if HAVE_MALLOC_H
/* MinGW helds alloca() in "malloc.h" instead of "alloca.h" */
#  include <malloc.h>
# endif
#endif

#include <stdlib.h>
#include <math.h>
#include <limits.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/priv/arith.h"
#include "gauche/bits.h"
#include "gauche/bits_inline.h"
#include "gauche/bignum.h"

#undef min
#define min(x, y)   (((x) < (y))? (x) : (y))
#undef max
#define max(x, y)   (((x) > (y))? (x) : (y))

static ScmBignum *bignum_rshift(ScmBignum *br, const ScmBignum *bx, int amount);
static ScmBignum *bignum_lshift(ScmBignum *br, const ScmBignum *bx, int amount);
static int bignum_safe_size_for_add(const ScmBignum *x, const ScmBignum *y);
static ScmBignum *bignum_add_int(ScmBignum *br, const ScmBignum *bx, const ScmBignum *by);
static ScmBignum *bignum_2scmpl(ScmBignum *br);

/*---------------------------------------------------------------------
 * Constructor
 *
 *   Scm_MakeBignum* always returns bignum, possibly denormalized.
 */
static ScmBignum *bignum_clear(ScmBignum *b)
{
    for (u_int i=0; i<b->size; i++) b->values[i] = 0;
    return b;
}

#define BIGNUM_SIZE(size) (sizeof(ScmBignum)+((size)-1)*sizeof(long))

static ScmBignum *make_bignum(int size)
{
    if (size < 0) Scm_Error("invalid bignum size (internal error): %d", size);
    if (size > (int)SCM_BIGNUM_MAX_DIGITS) Scm_Error("too large bignum");
    ScmBignum *b = SCM_NEW_ATOMIC2(ScmBignum*, BIGNUM_SIZE(size));
    SCM_SET_CLASS(b, SCM_CLASS_INTEGER);
    b->size = size;
    b->sign = 1;
    return bignum_clear(b);
}


/* Allocate temporary bignum in the current function's stack frame
   if alloca() is available. */
#ifdef HAVE_ALLOCA
#define ALLOC_TEMP_BIGNUM(var_, size_)                  \
    (var_) = SCM_BIGNUM(alloca(BIGNUM_SIZE(size_)));    \
    SCM_SET_CLASS(var_, SCM_CLASS_INTEGER);             \
    (var_)->size = (size_);                             \
    (var_)->sign = 1;                                   \
    bignum_clear(var_)
#else  /*!HAVE_ALLOCA*/
#define ALLOC_TEMP_BIGNUM(var_, size_) (var_) = make_bignum(size_);
#endif /*!HAVE_ALLOCA*/

ScmObj Scm_MakeBignumFromSI(long val)
{
    ScmBignum *b;
    if (val == LONG_MIN) {
        b = make_bignum(1);
        b->sign = -1;
        b->values[0] = (unsigned long)LONG_MAX+1;
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

/* If sign > 0 or sign < 0, values[] has absolute value.
   If sign == 0, values[] has 2's complement signed representation */
ScmObj Scm_MakeBignumFromUIArray(int sign, const u_long *values, int size)
{
    ScmBignum *b = make_bignum(size);
    if (sign != 0) {
        b->sign = (sign > 0)? 1 : -1;
        for (int i=0; i<size; i++) b->values[i] = values[i];
    } else {
        int nonzerop = FALSE;
        for (int i=0; i<size; i++) {
            if ((b->values[i] = values[i]) != 0) nonzerop = TRUE;
        }
        if (nonzerop) {
            if (values[size-1] <= LONG_MAX) b->sign = 1;
            else { b->sign = -1;  bignum_2scmpl(b); }
        } else {
            b->sign = 0;
        }
    }
    return SCM_OBJ(b);
}

ScmObj Scm_MakeBignumFromDouble(double val)
{
    if (LONG_MIN <= val
#if SIZEOF_LONG == 4
        && val <= LONG_MAX
#else
        && val <= nextafter((double)LONG_MAX, 0.0)
#endif
        )
        return Scm_MakeBignumFromSI((long)val);

    int exponent, sign;
    ScmObj mantissa = Scm_DecodeFlonum(val, &exponent, &sign);
    if (!SCM_NUMBERP(mantissa)) {
        Scm_Error("can't convert %lf to an integer", val);
    }
    ScmObj b = Scm_Ash(mantissa, exponent);
    if (sign < 0) b = Scm_Negate(b);
    /* always returns bignum */
    if (SCM_INTP(b)) {
        return Scm_MakeBignumFromSI(SCM_INT_VALUE(b));
    } else {
        return b;
    }
}

ScmObj Scm_BignumCopy(const ScmBignum *b)
{
    ScmBignum *c = make_bignum(b->size);
    c->sign = b->sign;
    for (u_int i=0; i<b->size; i++) c->values[i] = b->values[i];
    return SCM_OBJ(c);
}

/*-----------------------------------------------------------------------
 * Conversion
 */

/* Modifies B and return it. */
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
        if (b->sign > 0 && b->values[0] <= (u_long)SCM_SMALL_INT_MAX) {
            return SCM_MAKE_INT(b->values[0]);
        }
        if (b->sign < 0 && b->values[0] <= (u_long)-SCM_SMALL_INT_MIN) {
            return SCM_MAKE_INT(-((signed long)b->values[0]));
        }
    }
    b->size = size;
    return SCM_OBJ(b);
}

/* b must be normalized.  */
long Scm_BignumToSI(const ScmBignum *b, int clamp, int *oor)
{
    if (clamp == SCM_CLAMP_NONE && oor != NULL) *oor = FALSE;
    if (b->sign >= 0) {
        if (b->values[0] > LONG_MAX || b->size >= 2) {
            if (clamp & SCM_CLAMP_HI) return LONG_MAX;
            else goto err;
        } else {
            return (long)b->values[0];
        }
    } else {
        if (b->values[0] > (u_long)LONG_MAX+1 || b->size >= 2) {
            if (clamp & SCM_CLAMP_LO) return LONG_MIN;
            else goto err;
        } else {
            return -(long)b->values[0];
        }
    }
  err:
    if (clamp == SCM_CLAMP_NONE && oor != NULL) {
        *oor = TRUE;
    } else {
        Scm_Error("argument out of range: %S", SCM_OBJ(b));
    }
    return 0;
}

/* b must be normalized. */
u_long Scm_BignumToUI(const ScmBignum *b, int clamp, int *oor)
{
    if (clamp == SCM_CLAMP_NONE && oor != NULL) *oor = FALSE;
    if (b->sign >= 0) {
        if (b->size >= 2) {
            if (clamp & SCM_CLAMP_HI) return SCM_ULONG_MAX;
            else goto err;
        } else {
            return b->values[0];
        }
    } else {
        if (clamp & SCM_CLAMP_LO) return 0;
        else goto err;
    }
  err:
    if (clamp == SCM_CLAMP_NONE && oor != NULL) {
        *oor = TRUE;
    } else {
        Scm_Error("argument out of range: %S", SCM_OBJ(b));
    }
    return 0;
}

#if SIZEOF_LONG == 4
/* we need special routines for int64 */
ScmInt64 Scm_BignumToSI64(const ScmBignum *b, int clamp, int *oor)
{
#if SCM_EMULATE_INT64
    ScmInt64 r = {0, 0};
    if (clamp == SCM_CLAMP_NONE && oor != NULL) *oor = FALSE;
    if (b->sign > 0) {
        if (b->size > 2 || b->values[1] > LONG_MAX) {
            if (!(clamp & SCM_CLAMP_HI)) goto err;
            SCM_SET_INT64_MAX(r);
        } else {
            r.lo = b->values[0];
            if (b->size == 2) r.hi = b->values[1];
        }
    } else if (b->sign < 0) {
        if (b->size > 2 || b->values[1] > (u_long)LONG_MAX + 1) {
            if (!(clamp&SCM_CLAMP_LO)) goto err;
            SCM_SET_INT64_MIN(r);
        } else {
            b = SCM_BIGNUM(Scm_BignumComplement(b));
            r.lo = b->values[0];
            if (b->size == 2) r.hi = b->values[1];
            else              r.hi = -1;
        }
    }
    return r;
#else  /*!SCM_EMULATE_INT64*/
    int64_t r = 0;
    if (clamp == SCM_CLAMP_NONE && oor != NULL) *oor = FALSE;
    if (b->sign > 0) {
        if (b->size == 1) {
            r = b->values[0];
        } else if (b->size > 2 || b->values[1] > LONG_MAX) {
            if (!(clamp & SCM_CLAMP_HI)) goto err;
            SCM_SET_INT64_MAX(r);
        } else {
            r = ((int64_t)b->values[1] << 32) + (uint64_t)b->values[0];
        }
    } else { /* b->sign < 0 */
        if (b->size == 1) {
            r = -(int64_t)b->values[0];
        } else if (b->size > 2 || (b->values[1] > LONG_MAX && b->values[0] > 0)) {
            if (!(clamp&SCM_CLAMP_LO)) goto err;
            SCM_SET_INT64_MIN(r);
        } else {
            r = -(int64_t)(((int64_t)b->values[1] << 32) + (uint64_t)b->values[0]);
        }
    }
    return r;
#endif /*!SCM_EMULATE_INT64*/
  err:
    if (clamp == SCM_CLAMP_NONE && oor != NULL) {
        *oor = TRUE;
    } else {
        Scm_Error("argument out of range: %S", SCM_OBJ(b));
    }
    return r;
}

ScmUInt64 Scm_BignumToUI64(const ScmBignum *b, int clamp, int *oor)
{
#if SCM_EMULATE_INT64
    ScmInt64 r = {0, 0};
    if (clamp == SCM_CLAMP_NONE && oor != NULL) *oor = FALSE;
    if (b->sign > 0) {
        if (b->size > 2) {
            if (!(clamp&SCM_CLAMP_HI)) goto err;
            SCM_SET_UINT64_MAX(r);
        } else {
            r.lo = b->values[0];
            if (b->size == 2) r.hi = b->values[1];
        }
    } else if (b->sign < 0) {
        if (!(clamp&SCM_CLAMP_LO)) goto err;
    }
    return r;
#else  /*!SCM_EMULATE_INT64*/
    uint64_t r = 0;
    if (clamp == SCM_CLAMP_NONE && oor != NULL) *oor = FALSE;
    if (b->sign > 0) {
        if (b->size > 2) {
            if (!(clamp&SCM_CLAMP_HI)) goto err;
            SCM_SET_UINT64_MAX(r);
        } else if (b->size == 2) {
            r = (((uint64_t)b->values[1]) << 32) + (uint64_t)b->values[0];
        } else {
            r = (uint64_t)b->values[0];
        }
    } else { /* b->sign < 0 */
        if (!(clamp&SCM_CLAMP_LO)) goto err;
    }
    return r;
#endif /*!SCM_EMULATE_INT64*/
  err:
    if (clamp == SCM_CLAMP_NONE && oor != NULL) {
        *oor = TRUE;
    } else {
        Scm_Error("argument out of range: %S", SCM_OBJ(b));
    }
    return r;
}
#endif /* SIZEOF_LONG == 4 */


extern double Scm__EncodeDouble(u_long, u_long, int, int);

/* Converts a bignum b to a double.  b must be normalized.
   We don't rely on double arithmetic, for it may result
   an error in the LSB from multiple roundings.  Instead we
   extract bits directly from the bignum values array.
   (We use ScmBits API by casting b->values to ScmBits*).
 */
double Scm_BignumToDouble(const ScmBignum *b)
{
    ScmBits *bits = (ScmBits*)b->values;
    ScmBits dst[2];

    /* first, filter out a special case. */
    if (b->size == 0) return 0.0;

    int maxbit = Scm_BitsHighest1(bits, 0, b->size*WORD_BITS);
    int exponent = maxbit+1023;
#if SIZEOF_LONG >= 8
    SCM_ASSERT(maxbit >= 54);   /* because b is normalized */
    dst[0] = 0;
    Scm_BitsCopyX(dst, 0, bits, maxbit-52, maxbit);
    /* Rounding.  We have to round up if maxbit-53 == 1, EXCEPT
       the special case where maxbit-52==0, maxbit-53==1, and all
       bits below are 0 (round-to-even rule) */
    if (SCM_BITS_TEST(bits, maxbit-53)
        && ((dst[0]&1) == 1
            || (Scm_BitsCount1(bits, 0, maxbit-53) > 0))) {
        dst[0]++;
        if (dst[0] >= (1UL<<52)) {
            /* Overflow.  We mask the hidden bit, then shift. */
            dst[0] &= ~(1UL<<52);
            dst[0] >>= 1;
            exponent++;
        }
    }
    if (exponent > 2046) return Scm__EncodeDouble(0, 0, 2047, (b->sign < 0));
    else return Scm__EncodeDouble(0, dst[0], exponent, (b->sign < 0));
#else  /*SIZEOF_LONG == 4 */
    dst[0] = dst[1] = 0;
    if (maxbit < 53) {
        Scm_BitsCopyX(dst, 52-maxbit, bits, 0, maxbit);
    } else {
        Scm_BitsCopyX(dst, 0, bits, maxbit-52, maxbit);
        /* Rounding.  See the above comment. */
        if (SCM_BITS_TEST(bits, maxbit-53)
            && ((dst[0]&1) == 1
                || (maxbit > 53 && Scm_BitsCount1(bits, 0, maxbit-53) > 0))) {
            u_long inc = dst[0] + 1;
            if (inc < dst[0]) dst[1]++;
            dst[0] = inc;
            if (dst[1] >= (1UL<<(52-32))) {
                /* Overflow.  We mask the hidden bit, then shift. */
                dst[1] &= ~(1UL<<(52-32));
                dst[0] = (dst[0] >> 1) | (dst[1]&1 << 31);
                dst[1] >>= 1;
                exponent++;
            }
        }
    }
    if (exponent > 2046) return Scm__EncodeDouble(0, 0, 2047, (b->sign < 0));
    else return Scm__EncodeDouble(dst[0], dst[1], exponent, (b->sign < 0));
#endif /*SIZEOF_LONG==4*/
}

/* return -b, normalized */
ScmObj Scm_BignumNegate(const ScmBignum *b)
{
    ScmObj c = Scm_BignumCopy(b);
    SCM_BIGNUM_SIGN(c) = -SCM_BIGNUM_SIGN(c);
    return Scm_NormalizeBignum(SCM_BIGNUM(c));
}

/*-----------------------------------------------------------------------
 * Compare
 */

/* bx and by must be normalized */
int Scm_BignumCmp(const ScmBignum *bx, const ScmBignum *by)
{
    if (bx->sign < by->sign) return -1;
    if (bx->sign > by->sign) return 1;
    if (bx->size < by->size) return (bx->sign > 0) ? -1 : 1;
    if (bx->size > by->size) return (bx->sign > 0) ? 1 : -1;

    for (int i=(int)bx->size-1; i>=0; i--) {
        if (bx->values[i] < by->values[i]) return (bx->sign > 0) ? -1 : 1;
        if (bx->values[i] > by->values[i]) return (bx->sign > 0) ? 1 : -1;
    }
    return 0;
}

/* compare absolute values.  assume bx and by are nomalized. */
int Scm_BignumAbsCmp(const ScmBignum *bx, const ScmBignum *by)
{
    if (bx->size < by->size) return -1;
    if (bx->size > by->size) return 1;
    for (int i=(int)bx->size-1; i>=0; i--) {
        if (bx->values[i] < by->values[i]) return -1;
        if (bx->values[i] > by->values[i]) return 1;
    }
    return 0;
}

/* Compare bx + off and by.  All arguments must be positive.  bx and by
   must be normalized.  off may be denormalized if it is created directly
   by Scm_MakeBignumFromUI call.
   Expect bx >> off for most cases.
   Screen out the obvious case without actually calculating bx+off.
   Experimentary, the following set of conditions avoid 93% of cases from
   doing actual bignum addition. */
int Scm_BignumCmp3U(const ScmBignum *bx,
                    const ScmBignum *off,
                    const ScmBignum *by)
{
    u_int xsize = SCM_BIGNUM_SIZE(bx), ysize = SCM_BIGNUM_SIZE(by);
    u_int osize = SCM_BIGNUM_SIZE(off);

    if (xsize > ysize) return 1;
    if (xsize < ysize) {
        if (osize < ysize && by->values[ysize-1] > 1) {
            return -1;
        }
        if (osize == ysize) {
            if (off->values[osize-1] > by->values[ysize-1]) return 1;
            if (off->values[osize-1] < by->values[ysize-1]-1) return -1;
        }
        /* fallthrough */
    } else {
        /* xsize == ysize */
        u_long w, c = 0;
        if (osize > ysize) return 1;
        if (bx->values[xsize-1] > by->values[ysize-1]) return 1;
        if (osize < xsize) {
            if (bx->values[xsize-1] < by->values[ysize-1]-1) return -1;
        } else {
            /* osize == xsize */
            u_long xx = bx->values[xsize-1], oo = off->values[osize-1];
            UADD(w, c, xx, oo);
            if (c > 0 || w > by->values[ysize-1]) return 1;
            if (w < by->values[ysize-1] - 1) return -1;
        }
        /* fallthrough */
    }
    u_int tsize = bignum_safe_size_for_add(bx, off);
    ScmBignum *br;
    ALLOC_TEMP_BIGNUM(br, tsize);
    bignum_add_int(br, bx, off);

    if (br->size < by->size) return -1;
    for (int i=(int)br->size-1; i>=0; i--) {
        if (i >= (int)by->size) {
            if (br->values[i]) return 1;
            continue;
        }
        if (br->values[i] < by->values[i]) return -1;
        if (br->values[i] > by->values[i]) return 1;
    }
    return 0;
}

/*-----------------------------------------------------------------------
 * Add & subtract
 */
static int bignum_safe_size_for_add(const ScmBignum *x, const ScmBignum *y)
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

/* take 2's complement */
static ScmBignum *bignum_2scmpl(ScmBignum *br)
{
    int rsize = SCM_BIGNUM_SIZE(br);
    u_long c = 1;
    for (int i=0; i<rsize; i++) {
        unsigned long x = ~br->values[i];
        UADD(br->values[i], c, x, 0);
    }
    return br;
}

ScmObj Scm_BignumComplement(const ScmBignum *bx)
{
    ScmBignum *r = SCM_BIGNUM(Scm_BignumCopy(bx));
    return SCM_OBJ(bignum_2scmpl(r));
}

/* br = abs(bx) + abs(by), assuming br has enough size. br and bx can be
   the same object. */
static ScmBignum *bignum_add_int(ScmBignum *br,
                                 const ScmBignum *bx, const ScmBignum *by)
{
    int rsize = SCM_BIGNUM_SIZE(br);
    int xsize = SCM_BIGNUM_SIZE(bx);
    int ysize = SCM_BIGNUM_SIZE(by);
    u_long c = 0;

    for (int i=0; i<rsize; i++, xsize--, ysize--) {
        if (xsize <= 0) {
            if (ysize <= 0) {
                UADD(br->values[i], c, 0, 0);
                continue;
            }
            u_long y = by->values[i];
            UADD(br->values[i], c, 0, y);
            continue;
        }
        if (ysize <= 0) {
            u_long x = bx->values[i];
            UADD(br->values[i], c, x, 0);
            continue;
        }
        u_long x = bx->values[i];
        u_long y = by->values[i];
        UADD(br->values[i], c, x, y);
    }
    return br;
}

/* br = abs(bx) - abs(by), assuming br has enough size.  br and bx can be
   the same object. */
static ScmBignum *bignum_sub_int(ScmBignum *br,
                                 const ScmBignum *bx, const ScmBignum *by)
{
    int rsize = SCM_BIGNUM_SIZE(br);
    int xsize = SCM_BIGNUM_SIZE(bx);
    int ysize = SCM_BIGNUM_SIZE(by);
    u_long c = 0;

    for (int i=0; i<rsize; i++, xsize--, ysize--) {
        if (xsize <= 0) {
            if (ysize <= 0) {
                USUB(br->values[i], c, 0, 0);
                continue;
            }
            u_long y = by->values[i];
            USUB(br->values[i], c, 0, y);
            continue;
        }
        if (ysize <= 0) {
            u_long x = bx->values[i];
            USUB(br->values[i], c, x, 0);
            continue;
        }
        u_long x = bx->values[i];
        u_long y = by->values[i];
        USUB(br->values[i], c, x, y);
    }
    if (c != 0) {
        bignum_2scmpl(br);
        br->sign = 0 - br->sign; /* flip sign */
    }
    return br;
}

/* returns bx + by, not normalized */
static ScmBignum *bignum_add(const ScmBignum *bx, const ScmBignum *by)
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
static ScmBignum *bignum_sub(const ScmBignum *bx, const ScmBignum *by)
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
static ScmBignum *bignum_add_si(const ScmBignum *bx, long y)
{
    long c = 0;
    u_int rsize = bx->size+1;
    u_long yabs = ((y < 0)? -y : y);
    int ysign = ((y < 0)? -1 : 1);
    ScmBignum *br = make_bignum(rsize);
    br->sign = bx->sign;
    if (SCM_BIGNUM_SIGN(bx) == ysign) {
        for (u_int i=0; i<bx->size; i++) {
            UADD(br->values[i], c, bx->values[i], yabs);
            yabs = 0;
        }
    } else {
        for (u_int i=0; i<bx->size; i++) {
            USUB(br->values[i], c, bx->values[i], yabs);
            yabs = 0;
        }
    }
    br->values[rsize-1] = c;
    return br;
}

ScmObj Scm_BignumAdd(const ScmBignum *bx, const ScmBignum *by)
{
    return Scm_NormalizeBignum(bignum_add(bx, by));
}

ScmObj Scm_BignumSub(const ScmBignum *bx, const ScmBignum *by)
{
    return Scm_NormalizeBignum(bignum_sub(bx, by));
}

ScmObj Scm_BignumAddSI(const ScmBignum *bx, long y)
{
    if (y == 0) return SCM_OBJ(bx);
    else return Scm_NormalizeBignum(bignum_add_si(bx, y));
}

ScmObj Scm_BignumSubSI(const ScmBignum *bx, long y)
{
    if (y == 0) return SCM_OBJ(bx);
    else return Scm_NormalizeBignum(bignum_add_si(bx, -y));
}

/*-----------------------------------------------------------------------
 * Shifter
 */

/* br = bx >> amount.  amount >= 0.  no normalization.  assumes br
   has enough size to hold the result.  br and bx can be the same object. */
static ScmBignum *bignum_rshift(ScmBignum *br, const ScmBignum *bx, int amount)
{
    u_int nwords = amount / WORD_BITS;
    u_int nbits = amount % WORD_BITS;
    int i;

    if (bx->size <= nwords) {
        br->size = 0; br->values[0] = 0;
    } else if (nbits == 0) {
        for (i = (int)nwords; i < (int)bx->size; i++) {
            br->values[i-nwords] = bx->values[i];
        }
        br->size = bx->size - nwords;
        br->sign = bx->sign;
    } else {
        u_long x;
        for (i = (int)nwords; i < (int)bx->size-1; i++) {
            x = (bx->values[i+1]<<(WORD_BITS-nbits))|(bx->values[i]>>nbits);
            br->values[i-nwords] = x;
        }
        br->values[i-nwords] = bx->values[i]>>nbits;
        br->size = bx->size - nwords;
        br->sign = bx->sign;
    }
    return br;
}

/* br = bx << amount, amount > 0.   no normalization.   assumes br
   has enough size.  br and bx can be the same object. */
static ScmBignum *bignum_lshift(ScmBignum *br, const ScmBignum *bx, int amount)
{
    int nwords = amount / WORD_BITS;
    int nbits = amount % WORD_BITS;

    if (nbits == 0) {
        /* short path */
        for (int i = (int)bx->size-1; i>=0; i--) {
            if ((int)br->size > i+nwords) br->values[i+nwords] = bx->values[i];
        }
        for (int i = nwords-1; i>=0; i--) br->values[i] = 0;
    } else {
        if (br->size > bx->size + nwords) {
            br->values[bx->size+nwords] =
                bx->values[bx->size-1]>>(WORD_BITS-nbits);
        }
        int i;
        for (i = (int)bx->size-1; i > 0; i--) {
            u_long x = (bx->values[i]<<nbits)|(bx->values[i-1]>>(WORD_BITS-nbits));
            if ((int)br->size > i+nwords) br->values[i+nwords] = x;
        }
        br->values[nwords] = bx->values[0]<<nbits;
        for (i = nwords-1; i>=0; i--) br->values[i] = 0;
    }
    if (br != bx) {
        br->sign = bx->sign;
    }
    return br;
}

/*-----------------------------------------------------------------------
 * Multiplication
 */

/* br += bx * (y << off*WORD_BITS).   br must have enough size. */
static ScmBignum *bignum_mul_word(ScmBignum *br, const ScmBignum *bx,
                                  u_long y, int off)
{
    for (int i=0; i<bx->size; i++) {
        u_long hi, lo, r1;
        u_long x = bx->values[i];
        UMUL(hi, lo, x, y);

        u_long c = 0;
        u_long r0 = br->values[i+off];
        UADD(r1, c, r0, lo);
        br->values[i+off] = r1;

        r0 = br->values[i+off+1];
        UADD(r1, c, r0, hi);
        br->values[i+off+1] = r1;

        for (int j=i+off+2; c && j<br->size; j++) {
            r0 = br->values[j];
            UADD(r1, c, r0, 0);
            br->values[j] = r1;
        }
    }
    return br;
}

/* returns bx * by.  not normalized */
static ScmBignum *bignum_mul(const ScmBignum *bx, const ScmBignum *by)
{
    ScmBignum *br = make_bignum(bx->size + by->size);
    for (u_int i=0; i<by->size; i++) {
        bignum_mul_word(br, bx, by->values[i], i);
    }
    br->sign = bx->sign * by->sign;
    return br;
}

/* return bx * y,  y != 0 and y != 1 */
static ScmBignum *bignum_mul_si(const ScmBignum *bx, long y)
{
    if (y == -1) {
        ScmBignum *br = SCM_BIGNUM(Scm_BignumCopy(bx));
        br->sign = -br->sign;
        return br;
    }
    /* TODO: optimize for 2^n case !*/
    ScmBignum *br;
    br = make_bignum(bx->size + 1); /* TODO: more accurate estimation */
    u_long yabs = (y<0)? -y:y;
    br->sign = bx->sign;
    bignum_mul_word(br, bx, yabs, 0);
    if (y<0) br->sign = -br->sign;
    return br;
}

ScmObj Scm_BignumMul(const ScmBignum *bx, const ScmBignum *by)
{
    ScmBignum *br = bignum_mul(bx, by);
    return Scm_NormalizeBignum(br);
}

ScmObj Scm_BignumMulSI(const ScmBignum *bx, long y)
{
    if (y == 1) return SCM_OBJ(bx);
    else if (y == 0) return SCM_MAKE_INT(0);
    else return Scm_NormalizeBignum(bignum_mul_si(bx, y));
}

/*-----------------------------------------------------------------------
 * Division
 */

/* returns # of bits in the leftmost '1' in the word, counting from MSB. */
static inline int div_normalization_factor(u_long w)
{
    u_long b = (1L<<(WORD_BITS-1)), c = 0;
    for (; b > 0; b>>=1, c++) {
        if (w & b) return c;
    }
    /* something got wrong here */
    Scm_Panic("bignum.c: div_normalization_factor: can't be here");
    return 0;                   /* dummy */
}

/* General case of division.  We use each half word as a digit.
   Assumes digitsof(dividend) >= digitsof(divisor) > 1.
   Assumes enough digits are allocated to quotient.
   Remainder is returned (not normalized) */
static ScmBignum *bignum_gdiv(const ScmBignum *dividend,
                              const ScmBignum *divisor,
                              ScmBignum *quotient)
{
    int d = div_normalization_factor(divisor->values[divisor->size-1]);
    int n, m;
    u_long vv, uj, uj2, cy;

#define DIGIT(num, n) (((n)%2)? HI((num)->values[(n)/2]) : LO((num)->values[(n)/2]))
#define DIGIT2(num, n) \
    (((n)%2)?  \
     ((LO((num)->values[(n)/2+1])<<HALF_BITS)|HI((num)->values[(n)/2])): \
     (num)->values[(n)/2])
#define SETDIGIT(num, n, v) \
    (((n)%2)? \
     (num->values[(n)/2] = (num->values[(n)/2] & LOMASK)|((v) << HALF_BITS)) :\
     (num->values[(n)/2] = (num->values[(n)/2] & HIMASK)|((v) & LOMASK)))
#define SETDIGIT2(num, n, v)                                             \
    (((n)%2)?                                                            \
     ((num->values[(n)/2] = LO(num->values[(n)/2])|((v)<<HALF_BITS)),    \
      (num->values[(n)/2+1] = (num->values[(n)/2+1] & HIMASK)|HI(v))) : \
     (num->values[(n)/2] = (v)))

    /* normalize */
    ScmBignum *u, *v;
    u = make_bignum(dividend->size + 1); /* will be returned as a remainder */
    ALLOC_TEMP_BIGNUM(v, divisor->size);
    if (d >= HALF_BITS) {
        d -= HALF_BITS;
        n = divisor->size*2 - 1;
        m = dividend->size*2 - n;
    } else {
        n = divisor->size*2;
        m = dividend->size*2 - n;
    }
    bignum_lshift(u, dividend, d);
    bignum_lshift(v, divisor, d);
    u_long vn_1 = DIGIT(v, n-1);
    u_long vn_2 = DIGIT(v, n-2);
#undef DIV_DEBUG
#ifdef DIV_DEBUG
    Scm_Printf(SCM_CUROUT, "shift=%d, n=%d, m=%d\n", d, n, m);
    Scm_Printf(SCM_CUROUT, "u="); Scm_DumpBignum(u, SCM_CUROUT);
    Scm_Printf(SCM_CUROUT, "\nv="); Scm_DumpBignum(v, SCM_CUROUT);
    Scm_Printf(SCM_CUROUT, "\nvn_1=%08lx, vn_2=%08lx\n", vn_1, vn_2);
#endif

    for (int j = m; j >= 0; j--) {
        u_long uu = (DIGIT(u, j+n) << HALF_BITS) + DIGIT(u, j+n-1);
        u_long qq = uu/vn_1;
        u_long rr = uu%vn_1;
#ifdef DIV_DEBUG
        Scm_Printf(SCM_CUROUT, "loop on j=%d, uu=%08lx, qq=%08lx, rr=%08lx\n",
                   j, uu, qq, rr);
#endif
        while (qq >= HALF_WORD) { qq--; rr += vn_1; }
        while ((qq*vn_2 > (rr<<HALF_BITS)+DIGIT(u, j+n-2)) && (rr < HALF_WORD)) {
            qq--; rr += vn_1;
        }
#ifdef DIV_DEBUG
        Scm_Printf(SCM_CUROUT, "estimate uu=%08lx, qq=%08lx, rr=%08lx\n",
                   uu, qq, rr);
#endif
        cy = 0;
        for (int k = 0; k < n; k++) {
            vv = qq * DIGIT(v, k);
            uj = DIGIT2(u, j+k);
            uj2 = uj - vv - cy;
            cy =  (uj2 > uj)? HALF_WORD : 0;
            SETDIGIT2(u, j+k, uj2);
        }
#ifdef DIV_DEBUG
        Scm_Printf(SCM_CUROUT, "subtract cy = %d, ", cy);
        Scm_Printf(SCM_CUROUT, "u="); Scm_DumpBignum(u, SCM_CUROUT);
        Scm_Printf(SCM_CUROUT, "\n");
#endif
        if (cy) {
            qq--;
            cy = 0;
            for (int k = 0; k < n; k++) {
                vv = DIGIT(v, k);
                uj = DIGIT(u, j+k) + vv + cy;
                cy = (uj >= HALF_WORD)? 1 : 0;
                SETDIGIT(u, j+k, uj);
            }
            uj = DIGIT(u, j+n) + cy;
            SETDIGIT(u, j+n, uj);
        }
        SETDIGIT(quotient, j, qq);
    }
    bignum_rshift(u, u, d);
#ifdef DIV_DEBUG
    Scm_Printf(SCM_CUROUT, "quot q="); Scm_DumpBignum(quotient, SCM_CUROUT);
    Scm_Printf(SCM_CUROUT, "\nrem  u="); Scm_DumpBignum(u, SCM_CUROUT);
    Scm_Printf(SCM_CUROUT, "\n");
#endif
    return u;
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

/* assuming dividend is normalized. */
ScmObj Scm_BignumDivSI(const ScmBignum *dividend, long divisor, long *remainder)
{
    u_long dd = (divisor < 0)? -divisor : divisor;
    u_long rr;
    int d_sign = (divisor < 0)? -1 : 1;
    ScmBignum *q;

    if (dd < HALF_WORD) {
        q = SCM_BIGNUM(Scm_BignumCopy(dividend));
        rr = bignum_sdiv(q, dd);
    } else {
        ScmBignum *bv = SCM_BIGNUM(Scm_MakeBignumFromSI(dd));
        ScmBignum *br;
        q = make_bignum(dividend->size + 1);
        br = bignum_gdiv(dividend, bv, q);
        rr = br->values[0];
    }
    if (remainder) {
        *remainder = ((dividend->sign < 0)? -(signed long)rr : (signed long)rr);
    }
    q->sign = dividend->sign * d_sign;
    return Scm_NormalizeBignum(q);
}

/* If we only need rem(bignum, fixnum), we don't need to copy bignum
   to keep quotient. */
long Scm_BignumRemSI(const ScmBignum *dividend, long divisor)
{
#if (SIZEOF_LONG == 4) && HAVE_UINT64_T
    /* ILP32 with 64bit integer - easy one. */
    uint64_t dd = (divisor < 0)? -divisor : divisor;
    int sign = SCM_BIGNUM_SIGN(dividend);
    int k = SCM_BIGNUM_SIZE(dividend) - 1;
    uint64_t m = 0;
    for (;k >= 0; k--) {
        uint64_t x = (uint64_t)dividend->values[k] + (m << WORD_BITS);
        m = x % dd;
    }
    return (long)m * sign;
#else /* SIZEOF_LONG >= 4 || !HAVE_UINT64_T*/
    /* Here we need double-machine-word `rem` machine-word. */
    u_long dd = (divisor < 0)? -divisor : divisor;
    int sign = SCM_BIGNUM_SIGN(dividend);
    int k = SCM_BIGNUM_SIZE(dividend) - 1;
    int shift = WORD_BITS - Scm__HighestBitNumber(dd) - 1;
    u_long m = 0;
    for (;k >= 0; k--) {
        u_long x = dividend->values[k];
        int total_shift = 0;
        /* we calculate [m;x] modulo dd.  since always m < dd it's safe to
           shift m first. */
        while (total_shift < WORD_BITS) {
            int s = ((total_shift + shift < WORD_BITS)?
                     shift : WORD_BITS-total_shift);
            m = (m << s) | (x >> (WORD_BITS - s));
            x <<= s;
            m %= dd;
            total_shift += s;
        }
    }
    return (long)m * sign;
#endif
}

/* assuming dividend and divisor is normalized.  returns quotient and
   remainder */
ScmObj Scm_BignumDivRem(const ScmBignum *dividend, const ScmBignum *divisor)
{
    /* special case */
    if (Scm_BignumAbsCmp(dividend, divisor) < 0) {
        return Scm_Cons(SCM_MAKE_INT(0), SCM_OBJ(dividend));
    }

    ScmBignum *q = make_bignum(dividend->size - divisor->size + 1);
    ScmBignum *r = bignum_gdiv(dividend, divisor, q);
    q->sign = dividend->sign * divisor->sign;
    r->sign = dividend->sign;

    return Scm_Cons(Scm_NormalizeBignum(q), Scm_NormalizeBignum(r));
}

/*-----------------------------------------------------------------------
 * Logical (bitwise) opertaions
 */

ScmObj Scm_BignumAsh(const ScmBignum *x, int cnt)
{
    if (cnt == 0) return SCM_OBJ(x);
    if (cnt > 0) {
        int rsize = SCM_BIGNUM_SIZE(x) + (cnt+WORD_BITS-1)/WORD_BITS;
        ScmBignum *r = make_bignum(rsize);
        return Scm_NormalizeBignum(bignum_lshift(r, x, cnt));
    } else {
        int rsize = SCM_BIGNUM_SIZE(x) + cnt/WORD_BITS;
        if (rsize < 1) {
            if (SCM_BIGNUM_SIGN(x) < 0) {
                return SCM_MAKE_INT(-1);
            } else {
                return SCM_MAKE_INT(0);
            }
        } else {
            if (SCM_BIGNUM_SIGN(x) < 0) {
                /* painful way */
                ScmObj r = Scm_Quotient(Scm_Add(SCM_OBJ(x), SCM_MAKE_INT(1)),
                                        Scm_Ash(SCM_MAKE_INT(1), -cnt),
                                        NULL);
                return Scm_Add(r, SCM_MAKE_INT(-1));
            } else {
                ScmBignum *r = make_bignum(rsize);
                return Scm_NormalizeBignum(bignum_rshift(r, x, -cnt));
            }
        }
    }
}

/* internal routine for logand.  z = x & y.  assumes z has enough size.
 * assumes x and y are in 2's complement form (sign is ignored).
 */
static ScmBignum *bignum_and(ScmBignum *z,
                             const ScmBignum *x, const ScmBignum *y,
                             int commsize, int xsize, int ysize)
{
    int i;
    for (i = 0; i < commsize; i++) {
        z->values[i] = x->values[i] & y->values[i];
    }
    if (i < xsize) {
        for (; i < xsize; i++) z->values[i] = x->values[i];
    } else if (i < ysize) {
        for (; i < ysize; i++) z->values[i] = y->values[i];
    }
    return z;
}

ScmObj Scm_BignumLogAnd(const ScmBignum *x, const ScmBignum *y)
{
    int xsize = SCM_BIGNUM_SIZE(x), xsign = SCM_BIGNUM_SIGN(x);
    int ysize = SCM_BIGNUM_SIZE(y), ysign = SCM_BIGNUM_SIGN(y);
    int zsize, minsize = min(xsize, ysize);

    if (xsign > 0) {
        if (ysign > 0) {
            ScmBignum *z = bignum_and(make_bignum(minsize), x, y, minsize,
                                      0, 0);
            return Scm_NormalizeBignum(z);
        } else {
            ScmBignum *yy = SCM_BIGNUM(Scm_BignumComplement(y));
            ScmBignum *z = bignum_and(make_bignum(xsize), x, yy, minsize,
                                      xsize, 0);
            return Scm_NormalizeBignum(z);
        }
    } else {
        if (ysign > 0) {
            ScmBignum *xx = SCM_BIGNUM(Scm_BignumComplement(x));
            ScmBignum *z = bignum_and(make_bignum(ysize), xx, y, minsize,
                                      0, ysize);
            return Scm_NormalizeBignum(z);
        } else {
            ScmBignum *xx = SCM_BIGNUM(Scm_BignumComplement(x));
            ScmBignum *yy = SCM_BIGNUM(Scm_BignumComplement(y));
            zsize = max(xsize, ysize);
            ScmBignum *z = bignum_and(make_bignum(zsize), xx, yy, minsize,
                                      xsize, ysize);
            SCM_BIGNUM_SIGN(z) = -1;
            bignum_2scmpl(z);
            return Scm_NormalizeBignum(z);
        }
    }
}

/* internal routine for logior.  z = x | y.  assumes z has enough size.
 * assumes x and y are in 2's complement form (sign is ignored).
 */
static ScmBignum *bignum_ior(ScmBignum *z,
                             const ScmBignum *x, const ScmBignum *y,
                             int commsize, int xsize, int ysize)
{
    int i;
    for (i = 0; i < commsize; i++) {
        z->values[i] = x->values[i] | y->values[i];
    }
    if (i < xsize) {
        for (; i < xsize; i++) z->values[i] = x->values[i];
    } else if (i < ysize) {
        for (; i < ysize; i++) z->values[i] = y->values[i];
    }
    return z;
}

ScmObj Scm_BignumLogIor(const ScmBignum *x, const ScmBignum *y)
{
    int xsize = SCM_BIGNUM_SIZE(x), xsign = SCM_BIGNUM_SIGN(x);
    int ysize = SCM_BIGNUM_SIZE(y), ysign = SCM_BIGNUM_SIGN(y);
    int minsize = min(xsize, ysize);

    if (xsign >= 0) {
        if (ysign >= 0) {
            int zsize = max(xsize, ysize);
            ScmBignum *z = bignum_ior(make_bignum(zsize), x, y, minsize,
                                      xsize, ysize);
            return Scm_NormalizeBignum(z);
        } else {
            ScmBignum *yy = SCM_BIGNUM(Scm_BignumComplement(y));
            ScmBignum *z = bignum_ior(make_bignum(ysize), x, yy, minsize,
                                      0, ysize);
            SCM_BIGNUM_SIGN(z) = -1;
            bignum_2scmpl(z);
            return Scm_NormalizeBignum(z);
        }
    } else {
        if (ysign >= 0) {
            ScmBignum *xx = SCM_BIGNUM(Scm_BignumComplement(x));
            ScmBignum *z = bignum_ior(make_bignum(xsize), xx, y, minsize,
                                      xsize, 0);
            SCM_BIGNUM_SIGN(z) = -1;
            bignum_2scmpl(z);
            return Scm_NormalizeBignum(z);
        } else {
            ScmBignum *xx = SCM_BIGNUM(Scm_BignumComplement(x));
            ScmBignum *yy = SCM_BIGNUM(Scm_BignumComplement(y));
            ScmBignum *z = bignum_ior(make_bignum(minsize), xx, yy, minsize,
                                      0, 0);
            SCM_BIGNUM_SIGN(z) = -1;
            bignum_2scmpl(z);
            return Scm_NormalizeBignum(z);
        }
    }
}

ScmObj Scm_BignumLogXor(const ScmBignum *x, const ScmBignum *y)
{
    /* TODO: more efficient implementation */
    ScmObj xandy = Scm_BignumLogAnd(x, y);
    ScmObj xory  = Scm_BignumLogIor(x, y);
    return Scm_LogAnd(xory, Scm_LogNot(xandy));
}

int Scm_BignumLogCount(const ScmBignum *b)
{
    const ScmBignum *z = (SCM_BIGNUM_SIGN(b)>0)? b : SCM_BIGNUM(Scm_BignumComplement(b));
    int size = SCM_BIGNUM_SIZE(z) * SCM_WORD_BITS;

    ScmBits *bits = (ScmBits*)z->values;
    if (b->sign > 0) {
        return Scm_BitsCount1(bits, 0, size);
    } else {
        return Scm_BitsCount0(bits, 0, size);
    }
}


/*-----------------------------------------------------------------------
 * Printing
 */

ScmObj Scm_BignumToString(const ScmBignum *b, int radix, int use_upper)
{
    static const char ltab[] = "0123456789abcdefghijklmnopqrstuvwxyz";
    static const char utab[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    const char *tab = use_upper? utab : ltab;
    ScmObj h = SCM_NIL, t = SCM_NIL;
    if (radix < 2 || radix > 36)
        Scm_Error("radix out of range: %d", radix);
    ScmBignum *q = SCM_BIGNUM(Scm_BignumCopy(b));
    for (;q->size > 0;) {
        long rem = bignum_sdiv(q, radix);
        SCM_ASSERT(rem >= 0 && rem < radix);
        SCM_APPEND1(h, t, SCM_MAKE_CHAR(tab[rem]));
        for (; q->values[q->size-1] == 0 && q->size > 0; q->size--)
            ;
    }
    if (q->sign < 0) SCM_APPEND1(h, t, SCM_MAKE_CHAR('-'));
    return Scm_ListToString(Scm_ReverseX(h));
}

int Scm_DumpBignum(const ScmBignum *b, ScmPort *out)
{
    Scm_Printf(out, "#<bignum ");
    if (b->sign < 0) SCM_PUTC('-', out);
    for (int i=(int)b->size-1; i>=0; i--) {
        Scm_Printf(out, "%08lx ", b->values[i]);
    }
    SCM_PUTC('>', out);
    return 0;
}

/*-----------------------------------------------------------------------
 * Denormalized bignum API
 * These are provided for optimization of specific cases.
 */

/* Returns a bignum of specified size, initializing the least significant
   word by init. */
ScmBignum *Scm_MakeBignumWithSize(int size, u_long init)
{
    ScmBignum *b = make_bignum(size);
    b->values[0] = init;
    return b;
}

/* Calculate acc * coef + c and store the result to acc, if the result fits
   in acc.  If acc's size is not enough, allocate new bignum, which is at
   least sizeincr words bigger than acc.
   Returns the bignum that has the result, without normalizing.
   Acc need not be normalized. */
ScmBignum *Scm_BignumAccMultAddUI(ScmBignum *acc, u_long coef, u_long c)
{
    ScmBignum *r;
    u_int rsize = acc->size + 1;
    ALLOC_TEMP_BIGNUM(r, rsize);
    r->values[0] = c;
    bignum_mul_word(r, acc, coef, 0);
    if (r->values[rsize-1] == 0) {
        for (u_int i=0; i<acc->size; i++) {
            acc->values[i] = r->values[i];
        }
        return acc;
    } else {
        ScmBignum *rr;
        rr = make_bignum(rsize + 3); /* 3 is arbitrary size increment */
        rr->sign = acc->sign;
        for (u_int i=0; i<rsize; i++) {
            rr->values[i] = r->values[i];
        }
        return rr;
    }
}
