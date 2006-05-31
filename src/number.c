/*
 * number.c - numeric functions
 *
 *   Copyright (c) 2000-2006 Shiro Kawai, All rights reserved.
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
 *  $Id: number.c,v 1.126 2006-05-31 01:29:04 shirok Exp $
 */

#include <math.h>
#include <limits.h>
#include <string.h>
#include <ctype.h>
#include <float.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/bignum.h"
#include "gauche/scmconst.h"

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#ifdef HAVE_ISNAN
#define SCM_IS_NAN(x)  isnan(x)
#else
#define SCM_IS_NAN(x)  FALSE    /* we don't have a clue */
#endif

#ifdef HAVE_ISINF
#define SCM_IS_INF(x)  isinf(x)
#else
#define SCM_IS_INF(x)  ((x) != 0 && (x) == (x)/2.0)
#endif

#define RADIX_MIN 2
#define RADIX_MAX 36

/* Linux gcc have those, but the declarations aren't included unless
   __USE_ISOC9X is defined.  Just in case. */
#ifdef HAVE_TRUNC
extern double trunc(double);
#endif

#ifdef HAVE_RINT
extern double rint(double);
#define roundeven rint
#else
static double roundeven(double);
#endif

/*
 * Classes of Numeric Tower
 */

static ScmClass *numeric_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_RealClass),
    SCM_CLASS_STATIC_PTR(Scm_ComplexClass),
    SCM_CLASS_STATIC_PTR(Scm_NumberClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

static void number_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);

SCM_DEFINE_BUILTIN_CLASS(Scm_NumberClass, number_print, NULL, NULL, NULL,
                         numeric_cpl+3);
SCM_DEFINE_BUILTIN_CLASS(Scm_ComplexClass, number_print, NULL, NULL, NULL,
                         numeric_cpl+2);
SCM_DEFINE_BUILTIN_CLASS(Scm_RealClass, number_print, NULL, NULL, NULL,
                         numeric_cpl+1);
SCM_DEFINE_BUILTIN_CLASS(Scm_IntegerClass, number_print, NULL, NULL, NULL,
                         numeric_cpl);

/*=====================================================================
 *  Generic Arithmetic
 */

/* Some arithmetic operations calls the corresponding generic function
 * if the operand is not a number.
 */

/* Fallback Gf */
static ScmObj bad_number_method(ScmObj *args, int nargs, ScmGeneric *gf)
{
    const char *fn = (const char *)SCM_GENERIC_DATA(gf);
    if (nargs == 1) {
        Scm_Error("operation %s is not defined on object %S", fn, args[0]);
    } else if (nargs == 2) {
        Scm_Error("operation %s is not defined between %S and %S",
                  fn, args[0], args[1]);
    } else {
        Scm_Error("generic function for %s is called with args %S",
                  fn, Scm_ArrayToList(args, nargs));
    }
    return SCM_UNDEFINED;
}
static SCM_DEFINE_GENERIC(generic_add, bad_number_method, "+");
static SCM_DEFINE_GENERIC(generic_sub, bad_number_method, "-");
static SCM_DEFINE_GENERIC(generic_mul, bad_number_method, "*");
static SCM_DEFINE_GENERIC(generic_div, bad_number_method, "/");

/*=====================================================================
 *  Flonums
 */

ScmObj Scm_MakeFlonum(double d)
{
    ScmFlonum *f = SCM_NEW(ScmFlonum);
    SCM_SET_CLASS(f, SCM_CLASS_REAL);
    f->value = d;
    return SCM_OBJ(f);
}

ScmObj Scm_MakeFlonumToNumber(double d, int exact)
{
    if (exact && !SCM_IS_INF(d)) {
        /* see if d can be demoted to integer */
        double i, f;
        f = modf(d, &i);
        if (f == 0.0) {
            if (i > SCM_SMALL_INT_MAX || i < SCM_SMALL_INT_MIN) {
                return Scm_MakeBignumFromDouble(i);
            } else {
                return SCM_MAKE_INT((long)i);
            }
        }
    }
    return Scm_MakeFlonum(d);
}

/* Decompose flonum D into an integer mantissa F and exponent E, where
 *   -1022 <= E <= 1023,
 *    0 <= abs(F) < 2^53
 *    D = F * 2^(E - 53)
 * Some special cases:
 *    F = 0, E = 0 if D = 0.0 or -0.0
 *    F = #t if D is infinity (positive or negative)
 *    F = #f if D is NaN.
 * If D is normalized number, F >= 2^52.
 *
 * Cf. IEEE 754 Reference
 * http://babbage.cs.qc.edu/courses/cs341/IEEE-754references.html
 */
union ieee_double {
    double d;
    struct {
#ifdef DOUBLE_ARMENDIAN
        /* ARM's mixed endian.  TODO: what if we have LP64 ARM? */
        unsigned long mant0:20;
        unsigned int exp:11;
        unsigned int sign:1;
        unsigned long mant1:32;
#else  /*!DOUBLE_ARMENDIAN*/
#ifdef WORDS_BIGENDIAN
#if SIZEOF_LONG >= 8
        unsigned int sign:1;
        unsigned int exp:11;
        unsigned long mant:52;
#else  /*SIZEOF_LONG < 8*/
        unsigned int sign:1;
        unsigned int exp:11;
        unsigned long mant0:20;
        unsigned long mant1:32;
#endif /*SIZEOF_LONG < 8*/
#else  /*!WORDS_BIGENDIAN*/
#if SIZEOF_LONG >= 8
        unsigned long mant:52;
        unsigned int  exp:11;
        unsigned int  sign:1;
#else  /*SIZEOF_LONG < 8*/
        unsigned long mant1:32;
        unsigned long mant0:20;
        unsigned int  exp:11;
        unsigned int  sign:1;
#endif /*SIZEOF_LONG < 8*/
#endif /*!WORDS_BIGENDIAN*/
#endif /*!DOUBLE_ARMENDIAN*/
    } components;
};

ScmObj Scm_DecodeFlonum(double d, int *exp, int *sign)
{
    union ieee_double dd;
    ScmObj f;
    
    dd.d = d;

    *sign = (dd.components.sign? -1 : 1);

    /* Check exceptional cases */
    if (dd.components.exp == 0x7ff) {
        *exp = 0;
        if (
#if SIZEOF_LONG >= 8
            dd.components.mant == 0
#else  /*SIZEOF_LONG < 8*/
            dd.components.mant0 == 0 && dd.components.mant1 == 0
#endif /*SIZEOF_LONG < 8*/
            ) {
            return SCM_TRUE;  /* infinity */
        } else {
            return SCM_FALSE; /* NaN */
        }
    }

    *exp  = (dd.components.exp? dd.components.exp - 0x3ff - 52 : -0x3fe - 52);
    
#if SIZEOF_LONG >= 8
    {
        unsigned long lf = dd.components.mant;
        if (dd.components.exp > 0) {
            lf += (1L<<52);     /* hidden bit */
        }
        f = Scm_MakeInteger(lf);
    }
#else  /*SIZEOF_LONG < 8*/
    {
        unsigned long values[2];
        values[0] = dd.components.mant1;
        values[1] = dd.components.mant0;
        if (dd.components.exp > 0) {
            values[1] += (1L<<20); /* hidden bit */
        }
        f = Scm_NormalizeBignum(SCM_BIGNUM(Scm_MakeBignumFromUIArray(1, values, 2)));
    }
#endif /*SIZEOF_LONG < 8*/
    return f;
}

/*=======================================================================
 *  Complex numbers
 */

ScmObj Scm_MakeCompnum(double r, double i)
{
    ScmComplex *c = SCM_NEW_ATOMIC(ScmComplex);
    SCM_SET_CLASS(c, SCM_CLASS_COMPLEX);
    c->real = r;
    c->imag = i;
    return SCM_OBJ(c);
}

ScmObj Scm_MakeComplex(double r, double i)
{
    if (i == 0.0) return Scm_MakeFlonum(r);
    else          return Scm_MakeCompnum(r, i);
}

ScmObj Scm_MakeComplexPolar(double mag, double angle)
{
    double real = mag * cos(angle);
    double imag = mag * sin(angle);
    if (imag == 0.0) return Scm_MakeFlonum(real);
    else             return Scm_MakeCompnum(real, imag);
}

double Scm_RealPart(ScmObj z)
{
    double m;
    if (SCM_REALP(z)) {
        m = Scm_GetDouble(z);
    } else if (!SCM_COMPLEXP(z)) {
        Scm_Error("number required, but got %S", z);
        m = 0.0;                /* dummy */
    } else {
        m = SCM_COMPLEX_REAL(z);
    }
    return m;
}

double Scm_ImagPart(ScmObj z)
{
    double m = 0.0;
    if (SCM_COMPLEXP(z)) {
        m = SCM_COMPLEX_IMAG(z);
    } else if (!SCM_REALP(z)) {
        Scm_Error("number required, but got %S", z);
    }
    return m;
}

double Scm_Magnitude(ScmObj z)
{
    double m;
    if (SCM_REALP(z)) {
        m = fabs(Scm_GetDouble(z));
    } else if (!SCM_COMPLEXP(z)) {
        Scm_Error("number required, but got %S", z);
        m = 0.0;                /* dummy */
    } else {
        double r = SCM_COMPLEX_REAL(z);
        double i = SCM_COMPLEX_IMAG(z);
        m = sqrt(r*r+i*i);
    }
    return m;
}

double Scm_Angle(ScmObj z)
{
    double a;
    if (SCM_REALP(z)) {
        a = (Scm_Sign(z) < 0)? M_PI : 0.0;
    } else if (!SCM_COMPLEXP(z)) {
        Scm_Error("number required, but got %S", z);
        a = 0.0;                /* dummy */
    } else {
        double r = SCM_COMPLEX_REAL(z);
        double i = SCM_COMPLEX_IMAG(z);
        a = atan2(i, r);
    }
    return a;
}

/*=======================================================================
 *  Coertion
 */

ScmObj Scm_MakeInteger(long i)
{
    if (i >= SCM_SMALL_INT_MIN && i <= SCM_SMALL_INT_MAX) {
        return SCM_MAKE_INT(i);
    } else {
        return Scm_MakeBignumFromSI(i);
    }
}

ScmObj Scm_MakeIntegerU(u_long i)
{
    if (i <= (u_long)SCM_SMALL_INT_MAX) return SCM_MAKE_INT(i);
    else return Scm_MakeBignumFromUI(i);
}

/* Convert scheme integer to C integer */
long Scm_GetIntegerClamp(ScmObj obj, int clamp, int *oor)
{
    if (clamp == SCM_CLAMP_NONE && oor != NULL) *oor = FALSE;
    if (SCM_INTP(obj)) return SCM_INT_VALUE(obj);
    else if (SCM_BIGNUMP(obj)) {
        return Scm_BignumToSI(SCM_BIGNUM(obj), clamp, oor);
    }
    else if (SCM_FLONUMP(obj)) {
        double v = SCM_FLONUM_VALUE(obj);
        if (v > (double)LONG_MAX) {
            if (clamp & SCM_CLAMP_HI) return LONG_MAX;
            else goto err;
        }
        if (v < (double)LONG_MIN) {
            if (clamp & SCM_CLAMP_LO) return LONG_MIN;
            else goto err;
        }
        return (long)v;
    }
  err:
    if (clamp == SCM_CLAMP_NONE && oor != NULL) {
        *oor = TRUE;
    } else {
        Scm_Error("argument out of range: %S", obj);
    }
    return 0;
}

u_long Scm_GetIntegerUClamp(ScmObj obj, int clamp, int *oor)
{
    if (clamp == SCM_CLAMP_NONE && oor != NULL) *oor = FALSE;
    if (SCM_INTP(obj)) {
        if (SCM_INT_VALUE(obj) < 0) {
            if (clamp & SCM_CLAMP_LO) return 0;
            else goto err;
        }
        return SCM_INT_VALUE(obj);
    } else if (SCM_BIGNUMP(obj)) {
        return Scm_BignumToUI(SCM_BIGNUM(obj), clamp, oor);
    }
    else if (SCM_FLONUMP(obj)) {
        double v = SCM_FLONUM_VALUE(obj);
        if (v > (double)ULONG_MAX) {
            if (clamp & SCM_CLAMP_HI) return ULONG_MAX;
            else goto err;
        }
        if (v < 0.0) {
            if (clamp & SCM_CLAMP_LO) return 0;
            else goto err;
        }
        return (u_long)v;
    }
  err:
    if (clamp == SCM_CLAMP_NONE && oor != NULL) {
        *oor = TRUE;
    } else {
        Scm_Error("argument out of range: %S", obj);
    }
    return 0;
}

/* 32bit integer specific */
ScmInt32 Scm_GetInteger32Clamp(ScmObj obj, int clamp, int *oor)
{
#if SIZEOF_LONG == 4
    return (ScmInt32)Scm_GetIntegerClamp(obj, clamp, oor);
#else  /* SIZEOF_LONG >= 8 */
    if (clamp == SCM_CLAMP_NONE && oor != NULL) *oor = FALSE;
    /* NB: we denote the constant directly here.  (1L<<31) fails on
       Alpha machines, since the compiler somehow calculates the constant
       in 32bit integer even it has 'L'.  We have to write (1LL<<31), but
       I'm afraid that it's not portable. */
    if (SCM_INTP(obj)) {
        long r = SCM_INT_VALUE(obj);
        if (r < -0x80000000L) {
            if (clamp & SCM_CLAMP_LO) return -0x80000000L;
            goto err;
        }
        if (r > 0x7fffffffL) {
            if (clamp & SCM_CLAMP_HI) return 0x7fffffffL;
            goto err;
        }
        return r;
    } else if (SCM_BIGNUMP(obj)) {
        if (SCM_BIGNUM_SIGN(obj) < 0) {
            if (clamp & SCM_CLAMP_LO) return -0x80000000L;
            goto err;
        } else {
            if (clamp & SCM_CLAMP_HI) return 0x7fffffffL;
            goto err;
        }
    }
  err:
    if (clamp == SCM_CLAMP_NONE && oor != NULL) {
        *oor = TRUE;
    } else {
        Scm_Error("argument out of range: %S", obj);
    }
    return 0;
#endif /* SIZEOF_LONG >= 8 */
}

ScmUInt32 Scm_GetIntegerU32Clamp(ScmObj obj, int clamp, int *oor)
{
#if SIZEOF_LONG == 4
    return (ScmUInt32)Scm_GetIntegerUClamp(obj, clamp, oor);
#else  /* SIZEOF_LONG >= 8 */
    if (clamp == SCM_CLAMP_NONE && oor != NULL) *oor = FALSE;
    if (SCM_INTP(obj)) {
        long r = SCM_INT_VALUE(obj);
        if (r < 0) {
            if (clamp & SCM_CLAMP_LO) return 0;
            goto err;
        }
        if (r > 0xffffffffUL) {
            if (clamp & SCM_CLAMP_HI) return 0xffffffffUL;
            goto err;
        }
        return r;
    } else if (SCM_BIGNUMP(obj)) {
        if (SCM_BIGNUM_SIGN(obj) < 0) {
            if (clamp & SCM_CLAMP_LO) return 0;
            goto err;
        } else {
            if (clamp & SCM_CLAMP_HI) return 0xffffffffUL;
            goto err;
        }
    }
  err:
    if (clamp == SCM_CLAMP_NONE && oor != NULL) {
        *oor = TRUE;
    } else {
        Scm_Error("argument out of range: %S", obj);
    }
    return 0;
#endif /* SIZEOF_LONG >= 8 */
}


#if SIZEOF_LONG == 4
/* we need special routines */
ScmObj Scm_MakeInteger64(ScmInt64 i)
{
#if SCM_EMULATE_INT64
    u_long val[2];
    if (i.hi == 0) return Scm_MakeInteger(i.lo);
    val[0] = i.lo;
    val[1] = i.hi;
    return Scm_MakeBignumFromUIArray(0, val, 2); /* bignum checks sign */
#else /*SCM_EMULATE_INT64*/
    u_long val[2];
    val[0] = (uint64_t)i & ULONG_MAX;
    val[1] = (uint64_t)i >> 32;
    if (val[1] == 0 && val[0] <= LONG_MAX) return Scm_MakeInteger(val[0]);
    return Scm_NormalizeBignum(SCM_BIGNUM(Scm_MakeBignumFromUIArray(0, val, 2)));
#endif
}

ScmObj Scm_MakeIntegerU64(ScmUInt64 i)
{
#if SCM_EMULATE_INT64
    u_long val[2];
    if (i.hi == 0) return Scm_MakeIntegerU(i.lo);
    val[0] = i.lo;
    val[1] = i.hi;
    return Scm_MakeBignumFromUIArray(1, val, 2);
#else /*SCM_EMULATE_INT64*/
    u_long val[2];
    val[0] = (uint64_t)i & ULONG_MAX;
    val[1] = (uint64_t)i >> 32;
    if (val[1] == 0) return Scm_MakeIntegerU(val[0]);
    return Scm_MakeBignumFromUIArray(1, val, 2);
#endif
}

ScmInt64 Scm_GetInteger64Clamp(ScmObj obj, int clamp, int *oor)
{
#if SCM_EMULATE_INT64
    ScmInt64 r = {0, 0};
    if (clamp == SCM_CLAMP_NONE && oor != NULL) *oor = FALSE;
    if (SCM_INTP(obj)) {
        long v = SCM_INT_VALUE(obj);
        r.lo = v;
        if (v < 0) r.hi = ULONG_MAX;
        return r;
    }
    if (SCM_BIGNUMP(obj)) {
        return Scm_BignumToSI64(SCM_BIGNUM(obj), clamp, oor);
    }
    if (SCM_FLONUMP(obj)) {
        if (Scm_NumCmp(obj, SCM_2_63) >= 0) {
            if (!(clamp&SCM_CLAMP_HI)) goto err;
            SCM_SET_INT64_MAX(r);
            return r;
        } else if (Scm_NumCmp(obj, SCM_MINUS_2_63) < 0) {
            if (!(clamp&SCM_CLAMP_LO)) goto err;
            SCM_SET_INT64_MIN(r);
            return r;
        } else {
            ScmObj b = Scm_MakeBignumFromDouble(SCM_FLONUM_VALUE(obj));
            return Scm_BignumToSI64(SCM_BIGNUM(b), clamp, oor);
        }
    }
#else /*!SCM_EMULATE_INT64*/
    ScmInt64 r = 0;
    if (clamp == SCM_CLAMP_NONE && oor != NULL) *oor = FALSE;
    if (SCM_INTP(obj)) return (ScmInt64)SCM_INT_VALUE(obj);
    if (SCM_BIGNUMP(obj)) {
        return Scm_BignumToSI64(SCM_BIGNUM(obj), clamp, oor);
    }
    if (SCM_FLONUMP(obj)) {
        int64_t maxval, minval;
        double v;
        
        SCM_SET_INT64_MAX(maxval);
        SCM_SET_INT64_MIN(minval);
        v = SCM_FLONUM_VALUE(obj);
        if (v > (double)maxval) {
            if (!(clamp&SCM_CLAMP_HI)) goto err;
            return maxval;
        } else if (v < (double)minval) {
            if (!(clamp&SCM_CLAMP_LO)) goto err;
            return minval;
        } else {
            return (long)v;
        }
    }
#endif /*!SCM_EMULATE_INT64*/
  err:
    if (clamp == SCM_CLAMP_NONE && oor != NULL) {
        *oor = TRUE;
    } else {
        Scm_Error("argument out of range: %S", obj);
    }
    return r;
}
                               
ScmUInt64 Scm_GetIntegerU64Clamp(ScmObj obj, int clamp, int *oor)
{
#if SCM_EMULATE_INT64
    ScmUInt64 r = {0, 0};
    if (clamp == SCM_CLAMP_NONE && oor != NULL) *oor = FALSE;
    if (SCM_INTP(obj)) {
        long v = SCM_INT_VALUE(obj);
        if (v < 0) {
            if (!(clamp&SCM_CLAMP_LO)) goto err;
        } else {
            r.lo = v;
        }
        return r;
    }
    if (SCM_BIGNUMP(obj)) {
        return Scm_BignumToUI64(SCM_BIGNUM(obj), clamp, oor);
    }
    if (SCM_FLONUMP(obj)) {
        if (Scm_NumCmp(obj, SCM_2_64) >= 0) {
            if (!(clamp&SCM_CLAMP_HI)) goto err;
            SCM_SET_UINT64_MAX(r);
            return r;
        } else if (SCM_FLONUM_VALUE(obj) < 0) {
            if (!(clamp&SCM_CLAMP_LO)) goto err;
            return r;
        } else {
            ScmObj b = Scm_MakeBignumFromDouble(SCM_FLONUM_VALUE(obj));
            return Scm_BignumToUI64(SCM_BIGNUM(b), clamp, oor);
        }
    }
#else /*!SCM_EMULATE_INT64*/
    ScmInt64 r = 0;
    if (clamp == SCM_CLAMP_NONE && oor != NULL) *oor = FALSE;
    if (SCM_INTP(obj)) {
        long v = SCM_INT_VALUE(obj);
        if (v < 0) {
            if (!(clamp&SCM_CLAMP_LO)) goto err;
            return 0;
        } else {
            return (ScmUInt64)v;
        }
    }
    if (SCM_BIGNUMP(obj)) {
        return Scm_BignumToUI64(SCM_BIGNUM(obj), clamp, oor);
    }
    if (SCM_FLONUMP(obj)) {
        double v = SCM_FLONUM_VALUE(obj);
        uint64_t maxval;

        if (v < 0) {
            if (!(clamp&SCM_CLAMP_LO)) goto err;
            return 0;
        }
        SCM_SET_UINT64_MAX(maxval);
        if (v > (double)maxval) {
            if (!(clamp&SCM_CLAMP_HI)) goto err;
            return maxval;
        } else {
            return (uint32_t)v;
        }
    }
#endif
  err:
    if (clamp == SCM_CLAMP_NONE && oor != NULL) {
        *oor = TRUE;
    } else {
        Scm_Error("argument out of range: %S", obj);
    }
    return r;
}
                               
#endif /* SIZEOF_LONG == 4 */

double Scm_GetDouble(ScmObj obj)
{
    if (SCM_FLONUMP(obj)) return SCM_FLONUM_VALUE(obj);
    else if (SCM_INTP(obj)) return (double)SCM_INT_VALUE(obj);
    else if (SCM_BIGNUMP(obj)) return Scm_BignumToDouble(SCM_BIGNUM(obj));
    else return 0.0;
}

/*
 *   Generic Methods
 */

/* Predicates */

int Scm_IntegerP(ScmObj obj)
{
    if (SCM_INTP(obj) || SCM_BIGNUMP(obj)) return TRUE;
    if (SCM_FLONUMP(obj)) {
        double d = SCM_FLONUM_VALUE(obj);
        double f, i;
        if ((f = modf(d, &i)) == 0.0) return TRUE;
        return FALSE;
    }
    if (SCM_COMPLEXP(obj)) return FALSE;
    Scm_Error("number required, but got %S", obj);
    return FALSE;           /* dummy */
}

int Scm_OddP(ScmObj obj)
{
    if (SCM_INTP(obj)) {
        return (SCM_INT_VALUE(obj)&1);
    }
    if (SCM_BIGNUMP(obj)) {
        return (SCM_BIGNUM(obj)->values[0] & 1);
    }
    if (SCM_FLONUMP(obj) && Scm_IntegerP(obj)) {
        return (fmod(SCM_FLONUM_VALUE(obj), 2.0) != 0.0);
    }
    Scm_Error("integer required, but got %S", obj);
    return FALSE;       /* dummy */
    
}

/* Unary Operator */

ScmObj Scm_Abs(ScmObj obj)
{
    if (SCM_INTP(obj)) {
        long v = SCM_INT_VALUE(obj);
        if (v < 0) obj = SCM_MAKE_INT(-v);
    } else if (SCM_BIGNUMP(obj)) {
        if (SCM_BIGNUM_SIGN(obj) < 0) {
            obj = Scm_BignumCopy(SCM_BIGNUM(obj));
            SCM_BIGNUM_SIGN(obj) = 1;
        }
    } else if (SCM_FLONUMP(obj)) {
        double v = SCM_FLONUM_VALUE(obj);
        if (v < 0) obj = Scm_MakeFlonum(-v);
    } else if (SCM_COMPLEXP(obj)) {
        double r = SCM_COMPLEX_REAL(obj);
        double i = SCM_COMPLEX_IMAG(obj);
        double a = sqrt(r*r+i*i);
        return Scm_MakeFlonum(a);
    } else {
        Scm_Error("number required: %S", obj);
    }
    return obj;
}

/* Return -1, 0 or 1 when arg is minus, zero or plus, respectively.
   used to implement zero?, positive? and negative? */
int Scm_Sign(ScmObj obj)
{
    long r = 0;
    
    if (SCM_INTP(obj)) {
        r = SCM_INT_VALUE(obj);
        if (r > 0) r = 1;
        else if (r < 0) r = -1;
    } else if (SCM_BIGNUMP(obj)) {
        r = SCM_BIGNUM_SIGN(obj);
    } else if (SCM_FLONUMP(obj)) {
        double v = SCM_FLONUM_VALUE(obj);
        if (v != 0.0) {
            r = (v > 0.0)? 1 : -1;
        }
    } else {
        /* NB: zero? can accept a complex number, but it is processed in
           the stub function.   see stdlib.stub */
        Scm_Error("real number required, but got %S", obj);
    }
    return r;
}

ScmObj Scm_Negate(ScmObj obj)
{
    if (SCM_INTP(obj)) {
        long v = SCM_INT_VALUE(obj);
        if (v == SCM_SMALL_INT_MIN) {
            obj = Scm_MakeBignumFromSI(-v);
        } else {
            obj = SCM_MAKE_INT(-v);
        }
    } else if (SCM_BIGNUMP(obj)) {
        obj = Scm_BignumNegate(SCM_BIGNUM(obj));
    } else if (SCM_FLONUMP(obj)) {
        obj = Scm_MakeFlonum(-SCM_FLONUM_VALUE(obj));
    } else if (SCM_COMPLEXP(obj)) {
        obj = Scm_MakeCompnum(-SCM_COMPLEX_REAL(obj),
                              -SCM_COMPLEX_IMAG(obj));
    } else {
        obj = Scm_Apply(SCM_OBJ(&generic_sub), SCM_LIST1(obj));
    }
    return obj;
}

ScmObj Scm_Reciprocal(ScmObj obj)
{
    if (SCM_INTP(obj)) {
        long val = SCM_INT_VALUE(obj);
        obj = Scm_MakeFlonum(1.0/(double)val);
    } else if (SCM_BIGNUMP(obj)) {
        double val = Scm_BignumToDouble(SCM_BIGNUM(obj));
        obj = Scm_MakeFlonum(1.0/val);
    } else if (SCM_FLONUMP(obj)) {
        double val = SCM_FLONUM_VALUE(obj);
        obj = Scm_MakeFlonum(1.0/val);
    } else if (SCM_COMPLEXP(obj)) {
        double r = SCM_COMPLEX_REAL(obj), r1;
        double i = SCM_COMPLEX_IMAG(obj), i1;
        double d;
        d = r*r + i*i;
        r1 = r/d;
        i1 = -i/d;
        obj = Scm_MakeComplex(r1, i1);
    } else {
        obj = Scm_Apply(SCM_OBJ(&generic_div), SCM_LIST1(obj));
    }
    return obj;
}

/*
 * Conversion operators
 */

ScmObj Scm_ExactToInexact(ScmObj obj)
{
    if (SCM_INTP(obj)) {
        obj = Scm_MakeFlonum((double)SCM_INT_VALUE(obj));
    } else if (SCM_BIGNUMP(obj)) {
        obj = Scm_MakeFlonum(Scm_BignumToDouble(SCM_BIGNUM(obj)));
    } else if (!SCM_FLONUMP(obj) && !SCM_COMPLEXP(obj)) {
        Scm_Error("number required: %S", obj);
    }
    return obj;
}

ScmObj Scm_InexactToExact(ScmObj obj)
{
    if (SCM_FLONUMP(obj)) {
        double d = SCM_FLONUM_VALUE(obj);
        if (d < SCM_SMALL_INT_MIN || d > SCM_SMALL_INT_MAX) {
            obj = Scm_MakeBignumFromDouble(d);
        } else {
            obj = SCM_MAKE_INT((long)d);
        }
    } else if (SCM_COMPLEXP(obj)) {
        Scm_Error("exact complex is not supported: %S", obj);
    } if (!SCM_INTP(obj) && !SCM_BIGNUMP(obj)) {
        Scm_Error("number required: %S", obj);
    }
    return obj;
}

/* Type conversion:
 *   `promote' means a conversion from lower number class to higher,
 *      e.g. fixnum -> bignum -> flonum -> complex.
 *   `demote' means a conversion from higher number class to lower,
 *      e.g. complex -> flonum -> bignum -> fixnum.
 */

ScmObj Scm_PromoteToBignum(ScmObj obj)
{
    if (SCM_INTP(obj)) return Scm_MakeBignumFromSI(SCM_INT_VALUE(obj));
    if (SCM_BIGNUMP(obj)) return obj;
    Scm_Panic("Scm_PromoteToBignum: can't be here");
    return SCM_UNDEFINED;       /* dummy */
}

ScmObj Scm_PromoteToFlonum(ScmObj obj)
{
    if (SCM_INTP(obj)) return Scm_MakeFlonum(SCM_INT_VALUE(obj));
    if (SCM_BIGNUMP(obj))
        return Scm_MakeFlonum(Scm_BignumToDouble(SCM_BIGNUM(obj)));
    if (SCM_FLONUMP(obj)) return obj;
    Scm_Panic("Scm_PromoteToFlonum: can't be here");
    return SCM_UNDEFINED;       /* dummy */
}

ScmObj Scm_PromoteToComplex(ScmObj obj)
{
    if (SCM_INTP(obj))
        return Scm_MakeCompnum((double)SCM_INT_VALUE(obj), 0.0);
    if (SCM_BIGNUMP(obj))
        return Scm_MakeCompnum(Scm_BignumToDouble(SCM_BIGNUM(obj)), 0.0);
    if (SCM_FLONUMP(obj))
        return Scm_MakeCompnum(SCM_FLONUM_VALUE(obj), 0.0);
    if (SCM_COMPLEXP(obj)) return obj;
    Scm_Panic("Scm_PromoteToComplex: can't be here");
    return SCM_UNDEFINED;       /* dummy */
}

/*===============================================================
 * Arithmetics
 */
/* The code of addition, subtraction, multiplication and division
   are somewhat ugly (they use the harmful goto!).  My intention is
   to keep intermediate result in C-native types whenever possible,
   so that I can avoid boxing/unboxing those numbers. */

#define APPLY_GENERIC_ARITH(v, gf, arg0, arg1, args)    \
  do {                                                  \
    v = Scm_Apply(SCM_OBJ(&gf), SCM_LIST2(arg0, arg1)); \
    if (SCM_NULLP(args)) return v;                      \
    arg1 = SCM_CAR(args);                               \
    args = SCM_CDR(args);                               \
    goto retry;                                         \
  } while (0)

/*
 * Addition and subtraction
 */

ScmObj Scm_Add(ScmObj arg0, ScmObj arg1, ScmObj args)
{
    long result_int;
    double result_real, result_imag;

  retry:
    result_int = 0;
    if (SCM_INTP(arg0)) {
        result_int = SCM_INT_VALUE(arg0);
        for (;;) {
            if (SCM_INTP(arg1)) {
                result_int += SCM_INT_VALUE(arg1);
                if (result_int > SCM_SMALL_INT_MAX 
                    || result_int < SCM_SMALL_INT_MIN) {
                    arg0 = Scm_MakeBignumFromSI(result_int);
                    break;
                }
            } else if (SCM_BIGNUMP(arg1)) {
                arg0 = Scm_BignumAdd(SCM_BIGNUM(Scm_MakeBignumFromSI(result_int)),
                                     SCM_BIGNUM(arg1));
                break;
            } else if (SCM_FLONUMP(arg1)) {
                result_real = (double)result_int;
                goto DO_FLONUM;
            } else if (SCM_COMPLEXP(arg1)) {
                result_real = (double)result_int;
                result_imag = 0.0;
                goto DO_COMPLEX;
            } else {
                APPLY_GENERIC_ARITH(arg0, generic_add,
                                    Scm_MakeInteger(result_int),
                                    arg1, args);
            }
            if (!SCM_PAIRP(args)) return Scm_MakeInteger(result_int);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
        if (!SCM_PAIRP(args)) return arg0;
        arg1 = SCM_CAR(args);
        args = SCM_CDR(args);
    }
    if (SCM_BIGNUMP(arg0)) {
        /* See if we should call generic version */
        if (SCM_NUMBERP(arg1)) {
            return Scm_BignumAddN(SCM_BIGNUM(arg0), Scm_Cons(arg1, args));
        } else {
            APPLY_GENERIC_ARITH(arg0, generic_add, arg0, arg1, args);
        }
    }
    if (SCM_FLONUMP(arg0)) {
        result_real = SCM_FLONUM_VALUE(arg0);
      DO_FLONUM:
        for (;;) {
            if (SCM_INTP(arg1)) {
                result_real += (double)SCM_INT_VALUE(arg1);
            } else if (SCM_BIGNUMP(arg1)) {
                result_real += Scm_BignumToDouble(SCM_BIGNUM(arg1));
            } else if (SCM_FLONUMP(arg1)) {
                result_real += SCM_FLONUM_VALUE(arg1);
            } else if (SCM_COMPLEXP(arg1)) {
                result_imag = 0.0;
                goto DO_COMPLEX;
            } else {
                APPLY_GENERIC_ARITH(arg0, generic_add,
                                    Scm_MakeFlonum(result_real),
                                    arg1, args);
            }
            if (!SCM_PAIRP(args)) return Scm_MakeFlonum(result_real);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    if (SCM_COMPLEXP(arg0)) {
        result_real = SCM_COMPLEX_REAL(arg0);
        result_imag = SCM_COMPLEX_IMAG(arg0);
      DO_COMPLEX:
        for (;;) {
            if (SCM_INTP(arg1)) {
                result_real += (double)SCM_INT_VALUE(arg1);
            } else if (SCM_BIGNUMP(arg1)) {
                result_real += Scm_BignumToDouble(SCM_BIGNUM(arg1));
            } else if (SCM_FLONUMP(arg1)) {
                result_real += SCM_FLONUM_VALUE(arg1);
            } else if (SCM_COMPLEXP(arg1)) {
                result_real += SCM_COMPLEX_REAL(arg1);
                result_imag += SCM_COMPLEX_IMAG(arg1);
            } else {
                APPLY_GENERIC_ARITH(arg0, generic_add,
                                    Scm_MakeComplex(result_real, result_imag),
                                    arg1, args);
            }
            if (!SCM_PAIRP(args)) {
                return Scm_MakeComplex(result_real, result_imag);
            }
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    APPLY_GENERIC_ARITH(arg0, generic_add,
                        arg0, arg1, args);
    return SCM_UNDEFINED;       /* NOTREACHED */
}

ScmObj Scm_Subtract(ScmObj arg0, ScmObj arg1, ScmObj args)
{
    long result_int;
    double result_real, result_imag;

  retry:
    result_int = 0;
    result_real = 0.0;
    result_imag = 0.0;
    if (SCM_INTP(arg0)) {
        result_int = SCM_INT_VALUE(arg0);
        for (;;) {
            if (SCM_INTP(arg1)) {
                result_int -= SCM_INT_VALUE(arg1);
                if (result_int < SCM_SMALL_INT_MIN
                    || result_int > SCM_SMALL_INT_MAX) {
                    ScmObj big = Scm_MakeBignumFromSI(result_int);
                    return Scm_BignumSubN(SCM_BIGNUM(big), args);
                }
            } else if (SCM_BIGNUMP(arg1)) {
                ScmObj big = Scm_MakeBignumFromSI(result_int);
                return Scm_BignumSubN(SCM_BIGNUM(big), Scm_Cons(arg1, args));
            } else if (SCM_FLONUMP(arg1)) {
                result_real = (double)result_int;
                goto DO_FLONUM;
            } else if (SCM_COMPLEXP(arg1)) {
                result_real = (double)result_int;
                goto DO_COMPLEX;
            } else {
                APPLY_GENERIC_ARITH(arg0, generic_sub,
                                    Scm_MakeInteger(result_int),
                                    arg1, args);
            }
            if (SCM_NULLP(args)) return SCM_MAKE_INT(result_int);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    if (SCM_BIGNUMP(arg0)) {
        if (SCM_NUMBERP(arg1)) {
            return Scm_BignumSubN(SCM_BIGNUM(arg0), Scm_Cons(arg1, args));
        } else {
            APPLY_GENERIC_ARITH(arg0, generic_sub, arg0, arg1, args);
        }
    }
    if (SCM_FLONUMP(arg0)) {
        result_real = SCM_FLONUM_VALUE(arg0);
      DO_FLONUM:
        for (;;) {
            if (SCM_INTP(arg1)) {
                result_real -= (double)SCM_INT_VALUE(arg1);
            } else if (SCM_BIGNUMP(arg1)) {
                result_real -= Scm_BignumToDouble(SCM_BIGNUM(arg1));
            } else if (SCM_FLONUMP(arg1)) {
                result_real -= SCM_FLONUM_VALUE(arg1);
            } else if (SCM_COMPLEXP(arg1)) {
                goto DO_COMPLEX;
            } else {
                APPLY_GENERIC_ARITH(arg0, generic_sub,
                                    Scm_MakeFlonum(result_real),
                                    arg1, args);
            }
            if (SCM_NULLP(args))
                return Scm_MakeFlonum(result_real);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    if (SCM_COMPLEXP(arg0)) {
        result_real = SCM_COMPLEX_REAL(arg0);
        result_imag = SCM_COMPLEX_IMAG(arg0);
      DO_COMPLEX:
        for (;;) {
            if (SCM_INTP(arg1)) {
                result_real -= (double)SCM_INT_VALUE(arg1);
            } else if (SCM_BIGNUMP(arg1)) {
                result_real -= Scm_BignumToDouble(SCM_BIGNUM(arg1));
            } else if (SCM_FLONUMP(arg1)) {
                result_real -= SCM_FLONUM_VALUE(arg1);
            } else if (SCM_COMPLEXP(arg1)) {
                result_real -= SCM_COMPLEX_REAL(arg1);
                result_imag -= SCM_COMPLEX_IMAG(arg1);
            } else {
                APPLY_GENERIC_ARITH(arg0, generic_sub,
                                    Scm_MakeComplex(result_real, result_imag),
                                    arg1, args);
            }
            if (SCM_NULLP(args))
                return Scm_MakeComplex(result_real, result_imag);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    APPLY_GENERIC_ARITH(arg0, generic_sub, arg0, arg1, args);
    return SCM_UNDEFINED;       /* NOTREACHED */
}

/*
 * Multiplication
 */

ScmObj Scm_Multiply(ScmObj arg0, ScmObj arg1, ScmObj args)
{
    long result_int;
    double result_real, result_imag;

  retry:
    if (SCM_INTP(arg0)) {
        result_int = SCM_INT_VALUE(arg0);
        for (;;) {
            if (SCM_INTP(arg1)) {
                long vv = SCM_INT_VALUE(arg1);
                long k = result_int * vv;
                /* TODO: need a better way to check overflow */
                if ((vv != 0 && k/vv != result_int)
                    || k < SCM_SMALL_INT_MIN
                    || k > SCM_SMALL_INT_MAX) {
                    ScmObj big = Scm_MakeBignumFromSI(result_int);
                    arg0 = Scm_BignumMulSI(SCM_BIGNUM(big), vv);
                    break;
                }
                result_int = k;
            } else if (SCM_BIGNUMP(arg1)) {
                arg0 = Scm_BignumMulSI(SCM_BIGNUM(arg1), result_int);
                break;
            } else if (SCM_FLONUMP(arg1)) {
                result_real = (double)result_int;
                goto DO_FLONUM;
            } else if (SCM_COMPLEXP(arg1)) {
                result_real = (double)result_int;
                result_imag = 0.0;
                goto DO_COMPLEX;
            } else {
                APPLY_GENERIC_ARITH(arg0, generic_mul,
                                    Scm_MakeInteger(result_int), arg1, args);
            }
            if (!SCM_PAIRP(args)) return Scm_MakeInteger(result_int);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
        if (!SCM_PAIRP(args)) return arg0;
        arg1 = SCM_CAR(args);
        args = SCM_CDR(args);
        goto retry;
    }
    if (SCM_BIGNUMP(arg0)) {
        return Scm_BignumMulN(SCM_BIGNUM(arg0), Scm_Cons(arg1, args));
    }
    if (SCM_FLONUMP(arg0)) {
        result_real = SCM_FLONUM_VALUE(arg0);
      DO_FLONUM:
        for (;;) {
            if (SCM_INTP(arg1)) {
                result_real *= (double)SCM_INT_VALUE(arg1);
            } else if (SCM_BIGNUMP(arg1)) {
                result_real *= Scm_BignumToDouble(SCM_BIGNUM(arg1));
            } else if (SCM_FLONUMP(arg1)) {
                result_real *= SCM_FLONUM_VALUE(arg1);
            } else if (SCM_COMPLEXP(arg1)) {
                result_imag = 0.0;
                goto DO_COMPLEX;
            } else {
                APPLY_GENERIC_ARITH(arg0, generic_mul,
                                    Scm_MakeFlonum(result_real), arg1, args);
            }
            if (!SCM_PAIRP(args)) return Scm_MakeFlonum(result_real);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    if (SCM_COMPLEXP(arg0)) {
        result_real = SCM_COMPLEX_REAL(arg0);
        result_imag = SCM_COMPLEX_IMAG(arg0);
      DO_COMPLEX:
        for (;;) {
            if (SCM_INTP(arg1)) {
                result_real *= (double)SCM_INT_VALUE(arg1);
                result_imag *= (double)SCM_INT_VALUE(arg1);
            } else if (SCM_BIGNUMP(arg1)) {
                double dd = Scm_BignumToDouble(SCM_BIGNUM(arg1));
                result_real *= dd;
                result_imag *= dd;
            } else if (SCM_FLONUMP(arg1)) {
                result_real *= SCM_FLONUM_VALUE(arg1);
                result_imag *= SCM_FLONUM_VALUE(arg1);
            } else if (SCM_COMPLEXP(arg1)) {
                double r = SCM_COMPLEX_REAL(arg1);
                double i = SCM_COMPLEX_IMAG(arg1);
                double t = result_real * r - result_imag * i;
                result_imag   = result_real * i + result_imag * r;
                result_real = t;
            } else {
                APPLY_GENERIC_ARITH(arg0, generic_mul,
                                    Scm_MakeComplex(result_real, result_imag),
                                    arg1, args);
            }
            if (!SCM_PAIRP(args)) {
                return Scm_MakeComplex(result_real, result_imag);
            }
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    APPLY_GENERIC_ARITH(arg0, generic_mul,
                        arg0, arg1, args);
    return SCM_UNDEFINED;       /* NOTREACHED */
}

/*
 * Division
 */

ScmObj Scm_Divide(ScmObj arg0, ScmObj arg1, ScmObj args)
{
    double result_real = 0.0, result_imag = 0.0;
    double div_real = 0.0, div_imag = 0.0;
    int exact = TRUE;

  retry:
    result_real = result_imag = div_real = div_imag = 0.0;
    if (SCM_INTP(arg0)) {
        result_real = (double)SCM_INT_VALUE(arg0);
        goto DO_FLONUM;
    }
    if (SCM_BIGNUMP(arg0)) {
        /* Try integer division first, and if remainder != 0, shift to
           inexact number */
        if (SCM_INTP(arg1)) {
            long rem;
            ScmObj div;

            if (SCM_EQ(arg1, SCM_MAKE_INT(0))) {
                if (SCM_BIGNUM_SIGN(arg0) > 0) {
                    result_real = 1.0 / 0.0; /* +inf */
                } else {
                    result_real = -1.0 / 0.0; /* -inf */
                }
                exact = FALSE;
                goto DO_FLONUM;
            }
            div = Scm_BignumDivSI(SCM_BIGNUM(arg0),
                                  SCM_INT_VALUE(arg1),
                                  &rem);
            if (rem != 0) {
                result_real = Scm_BignumToDouble(SCM_BIGNUM(arg0));
                exact = FALSE;
                goto DO_FLONUM;
            }
            if (SCM_NULLP(args)) return div;
            return Scm_Divide(div, SCM_CAR(args), SCM_CDR(args));
        }
        if (SCM_BIGNUMP(arg1)) {
            ScmObj divrem = Scm_BignumDivRem(SCM_BIGNUM(arg0), SCM_BIGNUM(arg1));
            if (SCM_CDR(divrem) != SCM_MAKE_INT(0)) {
                result_real = Scm_BignumToDouble(SCM_BIGNUM(arg0));
                exact = FALSE;
                goto DO_FLONUM;
            }
            if (SCM_NULLP(args)) return SCM_CAR(divrem);
            return Scm_Divide(SCM_CAR(divrem), SCM_CAR(args), SCM_CDR(args));
        }
        if (SCM_FLONUMP(arg1)) {
            exact = FALSE;
            result_real = Scm_BignumToDouble(SCM_BIGNUM(arg0));
            goto DO_FLONUM;
        }
        if (SCM_COMPLEXP(arg1)) {
            exact = FALSE;
            result_real = Scm_BignumToDouble(SCM_BIGNUM(arg0));
            goto DO_COMPLEX;
        }
        APPLY_GENERIC_ARITH(arg0, generic_div, arg0, arg1, args);
    }
    if (SCM_FLONUMP(arg0)) {
        result_real = SCM_FLONUM_VALUE(arg0);
        exact = FALSE;
      DO_FLONUM:
        for (;;) {
            if (SCM_INTP(arg1)) {
                div_real = (double)SCM_INT_VALUE(arg1);
            } else if (SCM_BIGNUMP(arg1)) {
                div_real = Scm_BignumToDouble(SCM_BIGNUM(arg1));
            } else if (SCM_FLONUMP(arg1)) {
                div_real = SCM_FLONUM_VALUE(arg1);
                exact = FALSE;
            } else if (SCM_COMPLEXP(arg1)) {
                goto DO_COMPLEX;
            } else {
                APPLY_GENERIC_ARITH(arg0, generic_div,
                                    Scm_MakeFlonumToNumber(result_real, exact),
                                    arg1, args);
            }
            result_real /= div_real;
            if (SCM_NULLP(args))
                return Scm_MakeFlonumToNumber(result_real, exact);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    if (SCM_COMPLEXP(arg0)) {
        double d, r, i;
        result_real = SCM_COMPLEX_REAL(arg0);
        result_imag = SCM_COMPLEX_IMAG(arg0);
      DO_COMPLEX:
        for (;;) {
            div_imag = 0.0;
            if (SCM_INTP(arg1)) {
                div_real = (double)SCM_INT_VALUE(arg1);
            } else if (SCM_BIGNUMP(arg1)) {
                div_real = Scm_BignumToDouble(SCM_BIGNUM(arg1));
            } else if (SCM_FLONUMP(arg1)) {
                div_real = SCM_FLONUM_VALUE(arg1);
            } else if (SCM_COMPLEXP(arg1)) {
                div_real = SCM_COMPLEX_REAL(arg1);
                div_imag = SCM_COMPLEX_IMAG(arg1);
            } else {
                APPLY_GENERIC_ARITH(arg0, generic_div,
                                    Scm_MakeComplex(result_real, result_imag),
                                    arg1, args);
            }
            d = div_real*div_real + div_imag*div_imag;
            r = (result_real*div_real + result_imag*div_imag)/d;
            i = (result_imag*div_real - result_real*div_imag)/d;
            result_real = r;
            result_imag = i;
            if (SCM_NULLP(args))
                return Scm_MakeComplex(result_real, result_imag);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    APPLY_GENERIC_ARITH(arg0, generic_div, arg0, arg1, args);
    return SCM_UNDEFINED;       /* NOTREACHED */
}

/*
 * Integer division
 *   Returns (quotient x y)
 *   If rem != NULL, sets *rem to be (remainder x y) as well.
 */
ScmObj Scm_Quotient(ScmObj x, ScmObj y, ScmObj *rem)
{
    double rx, ry;
    if (SCM_INTP(x)) {
        if (SCM_INTP(y)) {
            long q, r;
            if (SCM_INT_VALUE(y) == 0) goto DIVBYZERO;
            q = SCM_INT_VALUE(x)/SCM_INT_VALUE(y);
            if (rem) {
                r = SCM_INT_VALUE(x)%SCM_INT_VALUE(y);
                *rem = SCM_MAKE_INT(r);
            }
            return SCM_MAKE_INT(q);
        }
        if (SCM_BIGNUMP(y)) {
            if (rem) *rem = x;
            return SCM_MAKE_INT(0);
        }
        if (SCM_FLONUMP(y)) {
            rx = (double)SCM_INT_VALUE(x);
            ry = SCM_FLONUM_VALUE(y);
            if (ry != floor(ry)) goto BADARGY;
            goto DO_FLONUM;
        }
        goto BADARGY;
    } else if (SCM_BIGNUMP(x)) {
        if (SCM_INTP(y)) {
            long r;
            ScmObj q = Scm_BignumDivSI(SCM_BIGNUM(x), SCM_INT_VALUE(y), &r);
            if (rem) *rem = SCM_MAKE_INT(r);
            return q;
        } else if (SCM_BIGNUMP(y)) {
            ScmObj qr = Scm_BignumDivRem(SCM_BIGNUM(x), SCM_BIGNUM(y));
            if (rem) *rem = SCM_CDR(qr);
            return SCM_CAR(qr);
        } else if (SCM_FLONUMP(y)) {
            rx = Scm_BignumToDouble(SCM_BIGNUM(x));
            ry = SCM_FLONUM_VALUE(y);
            if (ry != floor(ry)) goto BADARGY;
            goto DO_FLONUM;
        }
        goto BADARGY;
    } else if (SCM_FLONUMP(x)) {
        rx = SCM_FLONUM_VALUE(x);
        if (rx != floor(rx)) goto BADARG;
        if (SCM_INTP(y)) {
            ry = (double)SCM_INT_VALUE(y);
        } else if (SCM_BIGNUMP(y)) {
            ry = Scm_BignumToDouble(SCM_BIGNUM(y));
        } else if (SCM_FLONUMP(y)) {
            ry = SCM_FLONUM_VALUE(y);
            if (ry != floor(ry)) goto BADARGY;
        } else {
            goto BADARGY;
        }
      DO_FLONUM:
        {
            double q;
            if (ry == 0.0) goto DIVBYZERO;
            q = roundeven(rx/ry);
            if (rem) {
                double rr = roundeven(rx - q*ry);
                *rem = Scm_MakeFlonum(rr);
            }
            return Scm_MakeFlonum(q);
        }
    } else {
        goto BADARG;
    }
  DIVBYZERO:
    Scm_Error("attempt to calculate a quotient by zero");
  BADARGY:
    x = y;
  BADARG:
    Scm_Error("integer required, but got %S", x);
    return SCM_UNDEFINED;       /* dummy */
}

/* Modulo and Reminder.
   TODO: on gcc, % works like reminder.  I'm not sure the exact behavior
   of % is defined in ANSI C.  Need to check it later. */
ScmObj Scm_Modulo(ScmObj x, ScmObj y, int remp)
{
    double rx, ry;
    if (SCM_INTP(x)) {
        if (SCM_INTP(y)) {
            long r;
            if (SCM_INT_VALUE(y) == 0) goto DIVBYZERO;
            r = SCM_INT_VALUE(x)%SCM_INT_VALUE(y);
            if (!remp && r) {
                if ((SCM_INT_VALUE(x) > 0 && SCM_INT_VALUE(y) < 0)
                    || (SCM_INT_VALUE(x) < 0 && SCM_INT_VALUE(y) > 0)) {
                    r += SCM_INT_VALUE(y);
                }
            }
            return SCM_MAKE_INT(r);
        }
        if (SCM_BIGNUMP(y)) {
            if (remp) {
                return x;
            } else {
                if ((SCM_INT_VALUE(x) < 0 && SCM_BIGNUM_SIGN(y) > 0)
                    || (SCM_INT_VALUE(x) > 0 && SCM_BIGNUM_SIGN(y) < 0)) {
                    return Scm_BignumAddSI(SCM_BIGNUM(y), SCM_INT_VALUE(x));
                } else {
                    return x;
                }
            }
        }
        rx = (double)SCM_INT_VALUE(x);
        if (SCM_FLONUMP(y)) {
            ry = SCM_FLONUM_VALUE(y);
            if (ry != floor(ry)) goto BADARGY;
            goto DO_FLONUM;
        }
        goto BADARGY;
    } else if (SCM_BIGNUMP(x)) {
        if (SCM_INTP(y)) {
            long iy = SCM_INT_VALUE(y);
            long rem;
            Scm_BignumDivSI(SCM_BIGNUM(x), iy, &rem);
            if (!remp
                && rem
                && ((SCM_BIGNUM_SIGN(x) < 0 && iy > 0)
                    || (SCM_BIGNUM_SIGN(x) > 0 && iy < 0))) {
                return SCM_MAKE_INT(iy + rem);
            }
            return SCM_MAKE_INT(rem);
        }
        if (SCM_BIGNUMP(y)) {
            ScmObj rem = SCM_CDR(Scm_BignumDivRem(SCM_BIGNUM(x), SCM_BIGNUM(y)));
            if (!remp
                && (rem != SCM_MAKE_INT(0))
                && (SCM_BIGNUM_SIGN(x) * SCM_BIGNUM_SIGN(y) < 0)) {
                if (SCM_BIGNUMP(rem)) {
                    return Scm_BignumAdd(SCM_BIGNUM(y), SCM_BIGNUM(rem));
                } else {
                    return Scm_BignumAddSI(SCM_BIGNUM(y), SCM_INT_VALUE(rem));
                }       
            }
            return rem;
        }
        rx = Scm_BignumToDouble(SCM_BIGNUM(x));
        if (SCM_FLONUMP(y)) {
            ry = SCM_FLONUM_VALUE(y);
            if (ry != floor(ry)) goto BADARGY;
            goto DO_FLONUM;
        }
        goto BADARGY;
    } else if (SCM_FLONUMP(x)) {
        double rem;
        rx = SCM_FLONUM_VALUE(x);
        if (rx != floor(rx)) goto BADARG;
        if (SCM_INTP(y)) {
            ry = (double)SCM_INT_VALUE(y);
        } else if (SCM_BIGNUMP(y)) {
            ry = Scm_BignumToDouble(SCM_BIGNUM(y));
        } else if (SCM_FLONUMP(y)) {
            ry = SCM_FLONUM_VALUE(y);
            if (ry != floor(ry)) goto BADARGY;
        } else {
            goto BADARGY;
        }
      DO_FLONUM:
        if (ry == 0.0) goto DIVBYZERO;
        rem = fmod(rx, ry);
        if (!remp && rem != 0.0) {
            if ((rx > 0 && ry < 0) || (rx < 0 && ry > 0)) {
                rem += ry;
            }
        }
        return Scm_MakeFlonum(rem);
    } else {
        goto BADARG;
    }
  DIVBYZERO:
    Scm_Error("attempt to take a modulo or remainder by zero");
  BADARGY:
    x = y;
  BADARG:
    Scm_Error("integer required, but got %S", x);
    return SCM_UNDEFINED;       /* dummy */
}

/*
 * Expt
 */

/* Integer power of 10.  It is extensively used during string->number
   and number->string operations.
   IEXPT10_TABLESIZ is ceil(-log10(ldexp(1.0, -1022-52))) + 2 */
/* NB: actually we need more margin here to handle denormalized numbers. */
#define IEXPT10_TABLESIZ  341
static ScmObj iexpt10_n[IEXPT10_TABLESIZ] = { NULL };
static int    iexpt10_initialized = FALSE;

static void iexpt10_init(void)
{
    int i;
    iexpt10_n[0] = SCM_MAKE_INT(1);
    iexpt10_n[1] = SCM_MAKE_INT(10);
    iexpt10_n[2] = SCM_MAKE_INT(100);
    iexpt10_n[3] = SCM_MAKE_INT(1000);
    iexpt10_n[4] = SCM_MAKE_INT(10000);
    iexpt10_n[5] = SCM_MAKE_INT(100000);
    iexpt10_n[6] = SCM_MAKE_INT(1000000);
    for (i=7; i<IEXPT10_TABLESIZ; i++) {
        iexpt10_n[i] = Scm_Multiply2(iexpt10_n[i-1], SCM_MAKE_INT(10));
    }
    iexpt10_initialized = TRUE;
}

#define IEXPT10_INIT() \
    do { if (!iexpt10_initialized) iexpt10_init(); } while (0)

/* short cut for exact numbers */
static ScmObj exact_expt(ScmObj x, ScmObj y)
{
    int sign = Scm_Sign(y);
    long iy;
    ScmObj r = SCM_MAKE_INT(1);

    if (sign == 0) return r;
    if (x == SCM_MAKE_INT(1)) return r;
    if (x == SCM_MAKE_INT(-1)) return Scm_OddP(y)? SCM_MAKE_INT(-1) : r;

    if (!SCM_INTP(y)) {
        /* who wants such a heavy calculation? */
        Scm_Error("exponent too big: %S", y);
    }
    iy = SCM_INT_VALUE(y);
    /* Shortcut for special cases */
    if (x == SCM_MAKE_INT(10) && iy > 0 && iy < IEXPT10_TABLESIZ) {
        IEXPT10_INIT();
        r = iexpt10_n[iy];
    } else if (x == SCM_MAKE_INT(2) && iy > 0) {
        r = Scm_Ash(SCM_MAKE_INT(1), iy);
    } else {
        if (iy < 0) iy = -iy;
        for (;;) {
            if (iy == 0) break;
            if (iy == 1) { r = Scm_Multiply2(r, x); break; }
            if (iy & 0x01) r = Scm_Multiply2(r, x);
            x = Scm_Multiply2(x, x);
            iy >>= 1;
        }
    }
    return (sign < 0)? Scm_Reciprocal(r) : r;
}

ScmObj Scm_Expt(ScmObj x, ScmObj y)
{
    double dx, dy;
    if (SCM_EXACTP(x) && SCM_EXACTP(y)) return exact_expt(x, y);
    if (!SCM_REALP(x)) Scm_Error("real number required, but got %S", x);
    if (!SCM_REALP(y)) Scm_Error("real number required, but got %S", y);
    dx = Scm_GetDouble(x);
    dy = Scm_GetDouble(y);
    if (dy == 0.0) {
        return Scm_MakeFlonum(1.0);
    } else if (dx < 0 && !Scm_IntegerP(y)) {
        /* x^y == exp(y * log(x)) = exp(y*log(|x|))*exp(y*arg(x)*i)
           if x is a negative real number, arg(x) == pi
        */
        double mag = exp(dy * log(-dx));
        double theta = dy * M_PI;
        return Scm_MakeComplex(mag * cos(theta), mag * sin(theta));
    } else {
        return Scm_MakeFlonum(pow(dx, dy));
    }
}

/*===============================================================
 * Comparison
 */

int Scm_NumEq(ScmObj arg0, ScmObj arg1)
{
    if (SCM_COMPLEXP(arg0)) {
        if (SCM_COMPLEXP(arg1)) {
            return ((SCM_COMPLEX_REAL(arg0) == SCM_COMPLEX_REAL(arg1))
                    && (SCM_COMPLEX_IMAG(arg0) == SCM_COMPLEX_IMAG(arg1)));
        }
        return FALSE;
    } else {
        if (SCM_COMPLEXP(arg1)) return FALSE;
        return (Scm_NumCmp(arg0, arg1) == 0);
    }
}

/* 2-arg comparison */
int Scm_NumCmp(ScmObj arg0, ScmObj arg1)
{
    ScmObj badnum;
    
    if (SCM_INTP(arg0)) {
        if (SCM_INTP(arg1)) {
            long r = SCM_INT_VALUE(arg0) - SCM_INT_VALUE(arg1);
            if (r < 0) return -1;
            if (r > 0) return 1;
            return 0;
        }
        if (SCM_FLONUMP(arg1)) {
            double r = SCM_INT_VALUE(arg0) - SCM_FLONUM_VALUE(arg1);
            if (r < 0) return -1;
            if (r > 0) return 1;
            return 0;
        }
        if (SCM_BIGNUMP(arg1))
            return Scm_BignumCmp(SCM_BIGNUM(Scm_MakeBignumFromSI(SCM_INT_VALUE(arg0))),
                                 SCM_BIGNUM(arg1));
        badnum = arg1;
    }
    else if (SCM_FLONUMP(arg0)) {
        if (SCM_INTP(arg1)) {
            double r = SCM_FLONUM_VALUE(arg0) - SCM_INT_VALUE(arg1);
            if (r < 0) return -1;
            if (r > 0) return 1;
            return 0;
        }
        if (SCM_FLONUMP(arg1)) {
            double r = SCM_FLONUM_VALUE(arg0) - SCM_FLONUM_VALUE(arg1);
            if (r < 0) return -1;
            if (r > 0) return 1;
            return 0;
        }
        if (SCM_BIGNUMP(arg1))
            return Scm_BignumCmp(SCM_BIGNUM(Scm_MakeBignumFromDouble(SCM_FLONUM_VALUE(arg0))),
                                 SCM_BIGNUM(arg1));
        badnum = arg1;
    }
    else if (SCM_BIGNUMP(arg0)) {
        if (SCM_INTP(arg1))
            return Scm_BignumCmp(SCM_BIGNUM(arg0),
                                 SCM_BIGNUM(Scm_MakeBignumFromSI(SCM_INT_VALUE(arg1))));
        if (SCM_FLONUMP(arg1))
            return Scm_BignumCmp(SCM_BIGNUM(arg0),
                                 SCM_BIGNUM(Scm_MakeBignumFromDouble(SCM_FLONUM_VALUE(arg1))));
        if (SCM_BIGNUMP(arg1))
            return Scm_BignumCmp(SCM_BIGNUM(arg0), SCM_BIGNUM(arg1));
        badnum = arg1;
    }
    else badnum = arg0;
    Scm_Error("real number required: %S", badnum);
    return 0;                    /* dummy */
}

void Scm_MinMax(ScmObj arg0, ScmObj args, ScmObj *min, ScmObj *max)
{
    int inexact = !SCM_EXACTP(arg0);
    ScmObj mi = arg0;
    ScmObj ma = arg0;
    
    for (;;) {
        if (!SCM_REALP(arg0))
            Scm_Error("real number required, but got %S", arg0);
        if (SCM_NULLP(args)) {
            if (min) {
                if (inexact && SCM_EXACTP(mi)) {
                    *min = Scm_ExactToInexact(mi);
                } else {
                    *min = mi;
                }
            }
            if (max) {
                if (inexact && SCM_EXACTP(ma)) {
                    *max = Scm_ExactToInexact(ma);
                } else {
                    *max = ma;
                }
            }
            return;
        }
        if (!SCM_EXACTP(SCM_CAR(args))) inexact = TRUE;
        if (min && Scm_NumCmp(mi, SCM_CAR(args)) > 0) {
            mi = SCM_CAR(args);
        } 
        if (max && Scm_NumCmp(ma, SCM_CAR(args)) < 0) {
            ma = SCM_CAR(args);
        }
        args = SCM_CDR(args);
    }
}

/*===============================================================
 * ROUNDING
 */

/* NB: rint() is not in POSIX, so the alternative is provided here.
   We don't use round(), for it behaves differently when the
   argument is exactly the halfway of two whole numbers. */
#ifdef HAVE_RINT
#define roundeven rint
#else  /* !HAVE_RINT */
static double roundeven(double v)
{
    double r;
    double frac = modf(v, &r);
    if (v > 0.0) {
        if (frac > 0.5) r += 1.0;
        else if (frac == 0.5) {
            if (fmod(r,2.0) != 0.0) r += 1.0;
        }
    } else {
        if (frac < -0.5) r -= 1.0;
        else if (frac == -0.5) {
            if (fmod(r,2.0) != 0.0) r -= 1.0;
        }
    }
    return r;
}
#endif /* !HAVE_RINT */

ScmObj Scm_Round(ScmObj num, int mode)
{
    double r = 0.0, v;
    
    if (SCM_EXACTP(num)) return num;
    if (!SCM_FLONUMP(num))
        Scm_Error("real number required, but got %S", num);
    v = SCM_FLONUM_VALUE(num);
    switch (mode) {
    case SCM_ROUND_FLOOR: r = floor(v); break;
    case SCM_ROUND_CEIL:  r = ceil(v); break;
    /* trunc is neither in ANSI nor in POSIX. */
#ifdef HAVE_TRUNC
    case SCM_ROUND_TRUNC: r = trunc(v); break;
#else
    case SCM_ROUND_TRUNC: r = (v < 0.0)? ceil(v) : floor(v); break;
#endif
    case SCM_ROUND_ROUND: r = roundeven(v); break;
    default: Scm_Panic("something screwed up");
    }
    return Scm_MakeFlonum(r);
}

/*===============================================================
 * Logical (bitwise) operations
 */

ScmObj Scm_Ash(ScmObj x, int cnt)
{
    if (SCM_INTP(x)) {
        long ix = SCM_INT_VALUE(x);
        if (cnt <= -(SIZEOF_LONG * 8)) {
            ix = (ix < 0)? -1 : 0;
            return Scm_MakeInteger(ix);
        } else if (cnt < 0) {
            if (ix < 0) {
                ix = ~((~ix) >> (-cnt));
            } else {
                ix >>= -cnt;
            }
            return Scm_MakeInteger(ix);
        } else if (cnt < (SIZEOF_LONG*8-3)) {
            if (ix < 0) {
                if (-ix < (SCM_SMALL_INT_MAX >> cnt)) {
                    ix <<= cnt;
                    return Scm_MakeInteger(ix);
                } 
            } else {
                if (ix < (SCM_SMALL_INT_MAX >> cnt)) {
                    ix <<= cnt;
                    return Scm_MakeInteger(ix);
                } 
            }
        }
        /* Here, we know the result must be a bignum. */
        {
            ScmObj big = Scm_MakeBignumFromSI(ix);
            return Scm_BignumAsh(SCM_BIGNUM(big), cnt);
        }
    } else if (SCM_BIGNUMP(x)) {
        return Scm_BignumAsh(SCM_BIGNUM(x), cnt);
    }
    Scm_Error("exact integer required, but got %S", x);
    return SCM_UNDEFINED;
}

ScmObj Scm_LogNot(ScmObj x)
{
    if (!SCM_EXACTP(x)) Scm_Error("exact integer required, but got %S", x);
    if (SCM_INTP(x)) {
        /* this won't cause an overflow */
        return SCM_MAKE_INT(~SCM_INT_VALUE(x));
    } else {
        return Scm_Negate(Scm_BignumAddSI(SCM_BIGNUM(x), 1));
    }
}

ScmObj Scm_LogAnd(ScmObj x, ScmObj y)
{
    if (!SCM_EXACTP(x)) Scm_Error("exact integer required, but got %S", x);
    if (!SCM_EXACTP(y)) Scm_Error("exact integer required, but got %S", y);
    if (SCM_INTP(x)) {
        if (SCM_INTP(y)) {
            return SCM_MAKE_INT(SCM_INT_VALUE(x) & SCM_INT_VALUE(y));
        } else if (SCM_INT_VALUE(x) >= 0 && SCM_BIGNUM_SIGN(y) >= 0) {
            return Scm_MakeInteger(SCM_INT_VALUE(x)&SCM_BIGNUM(y)->values[0]);
        }
        x = Scm_MakeBignumFromSI(SCM_INT_VALUE(x));
    } else if (SCM_INTP(y)) {
        if (SCM_INT_VALUE(y) >= 0 && SCM_BIGNUM_SIGN(x) >= 0) {
            return Scm_MakeInteger(SCM_INT_VALUE(y)&SCM_BIGNUM(x)->values[0]);
        }
        y = Scm_MakeBignumFromSI(SCM_INT_VALUE(y));        
    }
    return Scm_BignumLogAnd(SCM_BIGNUM(x), SCM_BIGNUM(y));
}

ScmObj Scm_LogIor(ScmObj x, ScmObj y)
{
    if (!SCM_EXACTP(x)) Scm_Error("exact integer required, but got %S", x);
    if (!SCM_EXACTP(y)) Scm_Error("exact integer required, but got %S", y);
    if (SCM_INTP(x)) {
        if (SCM_INTP(y))
            return SCM_MAKE_INT(SCM_INT_VALUE(x) | SCM_INT_VALUE(y));
        else
            x = Scm_MakeBignumFromSI(SCM_INT_VALUE(x));
    } else {
        if (SCM_INTP(y)) y = Scm_MakeBignumFromSI(SCM_INT_VALUE(y));
    }
    return Scm_BignumLogIor(SCM_BIGNUM(x), SCM_BIGNUM(y));
}


ScmObj Scm_LogXor(ScmObj x, ScmObj y)
{
    if (!SCM_EXACTP(x)) Scm_Error("exact integer required, but got %S", x);
    if (!SCM_EXACTP(y)) Scm_Error("exact integer required, but got %S", y);
    if (SCM_INTP(x)) {
        if (SCM_INTP(y))
            return SCM_MAKE_INT(SCM_INT_VALUE(x) ^ SCM_INT_VALUE(y));
        else
            x = Scm_MakeBignumFromSI(SCM_INT_VALUE(x));
    } else {
        if (SCM_INTP(y)) y = Scm_MakeBignumFromSI(SCM_INT_VALUE(y));
    }
    return Scm_BignumLogXor(SCM_BIGNUM(x), SCM_BIGNUM(y));
}

/*===============================================================
 * Number I/O
 */

/* contants frequently used in number I/O */
static double dexpt2_minus_52  = 0.0;  /* 2.0^-52 */
static double dexpt2_minus_53  = 0.0;  /* 2.0^-53 */

/* max N where 10.0^N can be representable exactly in double.
   it is max N where N * log2(5) < 53. */
#define MAX_EXACT_10_EXP  23

/* fast 10^n for limited cases */
static inline ScmObj iexpt10(int e)
{
    SCM_ASSERT(e < IEXPT10_TABLESIZ);
    return iexpt10_n[e];
}

/* integer power of R by N, N is rather small.
   Assuming everything is in range. */
static inline u_long ipow(int r, int n)
{
    u_long k;
    for (k=1; n>0; n--) k *= r;
    return k;
}

/* X * 10.0^N by double.
   10.0^N can be represented _exactly_ in double-precision floating point
   number in the range 0 <= N <= 23.
   If N is out of this range, a rounding error occurs, which will be
   corrected in the algorithmR routine below. */
static double raise_pow10(double x, int n)
{
    static double dpow10[] = { 1.0, 1.0e1, 1.0e2, 1.0e3, 1.0e4,
                               1.0e5, 1.0e6, 1.0e7, 1.0e8, 1.0e9,
                               1.0e10, 1.0e11, 1.0e12, 1.0e13, 1.0e14,
                               1.0e15, 1.0e16, 1.0e17, 1.0e18, 1.0e19,
                               1.0e20, 1.0e21, 1.0e22, 1.0e23 };
    if (n >= 0) {
        while (n > 23) {
            x *= 1.0e24;
            n -= 24;
        }
        return x*dpow10[n];
    } else {
        while (n < -23) {
            x /= 1.0e24;
            n += 24;
        }
        return x/dpow10[-n];
    }
}

/*
 * Number Printer
 *
 * This version implements Burger&Dybvig algorithm (Robert G. Burger
 * and and R. Kent Dybvig, "Priting Floating-Point Numbers Quickly and 
 * Accurately", PLDI '96, pp.108--116, 1996).
 */

/* compare x+d and y */
static inline int numcmp3(ScmObj x, ScmObj d, ScmObj y)
{
    ScmObj bx = SCM_BIGNUMP(x)? x : Scm_MakeBignumFromSI(SCM_INT_VALUE(x));
    ScmObj bd = SCM_BIGNUMP(d)? d : Scm_MakeBignumFromSI(SCM_INT_VALUE(d));
    ScmObj by = SCM_BIGNUMP(y)? y : Scm_MakeBignumFromSI(SCM_INT_VALUE(y));
    return Scm_BignumCmp3U(SCM_BIGNUM(bx), SCM_BIGNUM(bd), SCM_BIGNUM(by));
}

static void double_print(char *buf, int buflen, double val, int plus_sign)
{
    /* Handle a few special cases first.
       The notation of infinity is provisional; see how srfi-70 becomes. */
    if (val == 0.0) {
        if (plus_sign) strcpy(buf, "+0.0");
        else strcpy(buf, "0.0");
        return;
    } else if (SCM_IS_INF(val)) {
        if (val < 0.0) strcpy(buf, "#i-1/0");
        else if (plus_sign) strcpy(buf, "#i+1/0");
        else strcpy(buf, "#i1/0");
        return;
    } else if (SCM_IS_NAN(val)) {
        strcpy(buf, "#<nan>");
        return;
    }
    
    if (val < 0.0) *buf++ = '-', buflen--;
    else if (plus_sign) *buf++ = '+', buflen--;
    {
        /* variable names follows Burger&Dybvig paper. mp, mm for m+, m-.
           note that m+ == m- for most cases, and m+ == 2*m- for the rest.
           so we calculate m+ from m- for each iteration, using the flag
           mp2 as   m+ = mp? m- : 2*m-. */
        ScmObj f, r, s, mp, mm, q;
        int exp, sign, est, tc1, tc2, tc3, digs, point, round;
        int mp2 = FALSE, fixup = FALSE;

        IEXPT10_INIT();
        if (val < 0) val = -val;
        
        /* initialize r, s, m+ and m- */
        f = Scm_DecodeFlonum(val, &exp, &sign);
        round = !Scm_OddP(f);
        if (exp >= 0) {
            ScmObj be = Scm_Ash(SCM_MAKE_INT(1), exp);
            if (Scm_NumCmp(f, SCM_2_52) != 0) {
                r = Scm_Ash(f, exp+1);
                s = SCM_MAKE_INT(2);
                mp2= FALSE;
                mm = be;
            } else {
                r = Scm_Ash(f, exp+2);
                s = SCM_MAKE_INT(4);
                mp2 = TRUE;
                mm = be;
            }
        } else {
            if (exp == -1023 || Scm_NumCmp(f, SCM_2_52) != 0) {
                r = Scm_Ash(f, 1);
                s = Scm_Ash(SCM_MAKE_INT(1), -exp+1);
                mp2 = FALSE;
                mm = SCM_MAKE_INT(1);
            } else {
                r = Scm_Ash(f, 2);
                s = Scm_Ash(SCM_MAKE_INT(1), -exp+2);
                mp2 = TRUE;
                mm = SCM_MAKE_INT(1);
            }
        }

        /* estimate scale */
        est = (int)ceil(log10(val) - 0.1);
        if (est >= 0) {
            s = Scm_Multiply2(s, iexpt10(est));
        } else {
            ScmObj scale = iexpt10(-est);
            r =  Scm_Multiply2(r, scale);
            mm = Scm_Multiply2(mm, scale);
        }

        /* fixup.  avoid calculating m+ for obvious case. */
        if (Scm_NumCmp(r, s) >= 0) {
            fixup = TRUE;
        } else {
            mp = (mp2? Scm_Ash(mm, 1) : mm);
            if (round) {
                fixup = (numcmp3(r, mp, s) >= 0);
            } else {
                fixup = (numcmp3(r, mp, s) > 0);
            }
        }
        if (fixup) {
            s = Scm_Multiply2(s, SCM_MAKE_INT(10));
            est++;
        }
        
        /* Scm_Printf(SCM_CURERR, "est=%d, r=%S, s=%S, mp=%S, mm=%S\n",
           est, r, s, mp, mm); */

        /* determine position of decimal point.  we avoid exponential
           notation if exponent is small, i.e. 0.9 and 30.0 instead of
           9.0e-1 and 3.0e1.   The magic number 10 is arbitrary. */
        if (est < 10 && est > -3) {
            point = est; est = 1;
        } else {
            point = 1;
        }

        /* generate */
        if (point <= 0) {
            *buf++ = '0'; buflen--;
            *buf++ = '.', buflen--;
            for (digs=point;digs<0 && buflen>5;digs++) {
                *buf++ = '0'; buflen--;
            }
        }
        for (digs=1;buflen>5;digs++) {
            ScmObj r10 = Scm_Multiply2(r, SCM_MAKE_INT(10));
            q = Scm_Quotient(r10, s, &r);
            mm = Scm_Multiply2(mm, SCM_MAKE_INT(10));
            mp = (mp2? Scm_Ash(mm, 1) : mm);
            
            /* Scm_Printf(SCM_CURERR, "q=%S, r=%S, mp=%S, mm=%S\n",
               q, r, mp, mm);*/

            SCM_ASSERT(SCM_INTP(q));
            if (round) {
                tc1 = (Scm_NumCmp(r, mm) <= 0);
                tc2 = (numcmp3(r, mp, s) >= 0);
            } else {
                tc1 = (Scm_NumCmp(r, mm) < 0);
                tc2 = (numcmp3(r, mp, s) > 0);
            }
            if (!tc1) {
                if (!tc2) {
                    *buf++ = SCM_INT_VALUE(q) + '0';
                    if (digs == point) *buf++ = '.', buflen--;
                    continue;
                } else {
                    *buf++ = SCM_INT_VALUE(q) + '1';
                    break;
                }
            } else {
                if (!tc2) {
                    *buf++ = SCM_INT_VALUE(q) + '0';
                    break;
                } else {
                    tc3 = numcmp3(r, r, s); /* r*2 <=> s */
                    if ((round && tc3 <= 0) || (!round && tc3 < 0)) {
                        *buf++ = SCM_INT_VALUE(q) + '0';
                        break;
                    } else {
                        *buf++ = SCM_INT_VALUE(q) + '1';
                        break;
                    }
                }
            }
        }

        if (digs <= point) {
            for (;digs<point&&buflen>5;digs++) {
                *buf++ = '0', buflen--;
            }
            *buf++ = '.';
            *buf++ = '0';
        }

        /* prints exponent.  we shifted decimal point, so -1. */
        est--;
        if (est != 0) {
            *buf++ = 'e';
            sprintf(buf, "%d", (int)est);
        } else {
            *buf++ = 0;
        }
    }
}

static void number_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmObj s = Scm_NumberToString(obj, 10, FALSE);
    SCM_PUTS(SCM_STRING(s), port);
}

#define FLT_BUF 50

ScmObj Scm_NumberToString(ScmObj obj, int radix, int use_upper)
{
    ScmObj r = SCM_NIL;
    char buf[FLT_BUF];
    
    if (SCM_INTP(obj)) {
        char buf[50], *pbuf = buf;
        long value = SCM_INT_VALUE(obj);
        if (value < 0) {
            *pbuf++ = '-';
            value = -value;     /* this won't overflow */
        }
        if (radix == 10) {
            snprintf(pbuf, 49, "%ld", value);
        } else if (radix == 16) {
            snprintf(pbuf, 49, (use_upper? "%lX" : "%lx"), value);
        } else if (radix == 8) {
            snprintf(pbuf, 49, "%lo", value);
        } else {
            /* sloppy way ... */
            r = Scm_BignumToString(SCM_BIGNUM(Scm_MakeBignumFromSI(SCM_INT_VALUE(obj))),
                                   radix, use_upper);
        }
        if (r == SCM_NIL) r = SCM_MAKE_STR_COPYING(buf);
    } else if (SCM_BIGNUMP(obj)) {
        r = Scm_BignumToString(SCM_BIGNUM(obj), radix, use_upper);
    } else if (SCM_FLONUMP(obj)) {
        double_print(buf, FLT_BUF, SCM_FLONUM_VALUE(obj), FALSE);
        r = SCM_MAKE_STR_COPYING(buf);
    } else if (SCM_COMPLEXP(obj)) {
        ScmObj p = Scm_MakeOutputStringPort(TRUE);
        double_print(buf, FLT_BUF, SCM_COMPLEX_REAL(obj), FALSE);
        SCM_PUTZ(buf, -1, SCM_PORT(p));
        double_print(buf, FLT_BUF, SCM_COMPLEX_IMAG(obj), TRUE);
        SCM_PUTZ(buf, -1, SCM_PORT(p));
        SCM_PUTC('i', SCM_PORT(p));
        r = Scm_GetOutputString(SCM_PORT(p));
    } else {
        Scm_Error("number required: %S", obj);
    }
    return r;
}

/* utility to expose Burger&Dybvig algorithm.  FLAGS is not used yet,
   but reserved for future extension. */
void Scm_PrintDouble(ScmPort *port, double d, int flags)
{
    char buf[FLT_BUF];
    double_print(buf, FLT_BUF, d, FALSE);
    Scm_Putz(buf, strlen(buf), port);
}

/*
 * Number Parser
 *
 *  <number> : <prefix> <complex>
 *  <prefix> : <radix> <exactness> | <exactness> <radix>
 *  <radix>  : <empty> | '#b' | '#o' | '#d' | '#x'
 *  <exactness> : <empty> | '#e' | '#i'
 *  <complex> : <real>
 *            | <real> '@' <real>
 *            | <real> '+' <ureal> 'i'
 *            | <real> '-' <ureal> 'i'
 *            | <real> '+' 'i'
 *            | <real> '-' 'i'
 *            | '+' <ureal> 'i'
 *            | '-' <ureal> 'i'
 *            | '+' 'i'
 *            | '-' 'i'
 *  <real>   : <sign> <ureal>
 *  <sign>   : <empty> | '+' | '-'
 *  <ureal>  : <uinteger>
 *           | <uinteger> '/' <uinteger>
 *           | <decimal>
 *  <uinteger> : <digit>+ '#'*
 *  <decimal> : <digit10>+ '#'* <suffix>
 *            | '.' <digit10>+ '#'* <suffix>
 *            | <digit10>+ '.' <digit10>+ '#'* <suffix>
 *            | <digit10>+ '#'+ '.' '#'* <suffix>
 *  <suffix>  : <empty> | <exponent-marker> <sign> <digit10>+
 *  <exponent-marker> : 'e' | 's' | 'f' | 'd' | 'l'
 *
 * The parser reads characters from on-memory buffer.
 * Multibyte strings are filtered out in the early stage of
 * parsing, so the subroutines assume the buffer contains
 * only ASCII chars.
 */

struct numread_packet {
    const char *buffer;         /* original buffer */
    int buflen;                 /* original length */
    int radix;                  /* radix */
    int exactness;              /* exactness; see enum below */
    int padread;                /* '#' padding has been read */
    int strict;                 /* when true, reports an error if the
                                   input violates implementation limitation;
                                   otherwise, the routine returns #f. */
};

enum { /* used in the exactness flag */
    NOEXACT, EXACT, INEXACT
};

/* Max digits D such that all D-digit radix R integers fit in signed
   long, i.e. R^(D+1)-1 <= LONG_MAX */
static long longdigs[RADIX_MAX-RADIX_MIN+1] = { 0 };

/* Max integer I such that reading next digit (in radix R) will overflow
   long integer.   floor(LONG_MAX/R - R). */
static u_long longlimit[RADIX_MAX-RADIX_MIN+1] = { 0 };

/* An integer table of R^D, which is a "big digit" to be added
   into bignum. */
static u_long bigdig[RADIX_MAX-RADIX_MIN+1] = { 0 };

static ScmObj numread_error(const char *msg, struct numread_packet *context);

/* Returns either small integer or bignum.
   initval may be a Scheme integer that will be 'concatenated' before
   the integer to be read; it is used to read floating-point number.
   Note that value_big may keep denormalized bignum. */
static ScmObj read_uint(const char **strp, int *lenp,
                        struct numread_packet *ctx,
                        ScmObj initval)
{
    const char *str = *strp;
    int digread = FALSE;
    int len = *lenp;
    int radix = ctx->radix;
    int digits = 0, diglimit = longdigs[radix-RADIX_MIN];
    u_long limit = longlimit[radix-RADIX_MIN], bdig = bigdig[radix-RADIX_MIN];
    u_long value_int = 0;
    ScmBignum *value_big = NULL;
    char c;
    static const char tab[] = "0123456789abcdefghijklmnopqrstuvwxyz";
    const char *ptab;

    if (!SCM_FALSEP(initval)) {
        if (SCM_INTP(initval)) {
            if (SCM_INT_VALUE(initval) > limit) {
                value_big = Scm_MakeBignumWithSize(4, SCM_INT_VALUE(initval));
            } else {
                value_int = SCM_INT_VALUE(initval);
            }
        } else if (SCM_BIGNUMP(initval)) {
            value_big = SCM_BIGNUM(Scm_BignumCopy(SCM_BIGNUM(initval)));
        }
        digread = TRUE;
    } else if (*str == '0') {
        /* Ignore leading 0's, to avoid unnecessary bignum operations. */
        while (len > 0 && *str == '0') { str++; len--; }
        digread = TRUE;
    }

    while (len--) {
        int digval = -1;
        c = tolower(*str++);
        if (ctx->padread) {
            if (c == '#') digval = 0;
            else break;
        } else if (digread && c == '#') {
            digval = 0;
            ctx->padread = TRUE;
            if (ctx->exactness == NOEXACT) {
                ctx->exactness = INEXACT;
            }
        } else {
            for (ptab = tab; ptab < tab+radix; ptab++) {
                if (c == *ptab) {
                    digval = ptab-tab;
                    digread = TRUE;
                    break;
                }
            }
        }
        if (digval < 0) break;
        value_int = value_int * radix + digval;
        digits++;
        if (value_big == NULL) {
            if (value_int >= limit) {
                value_big = Scm_MakeBignumWithSize(4, value_int);
                value_int = digits = 0;
            }
        } else if (digits > diglimit) {
            value_big = Scm_BignumAccMultAddUI(value_big, bdig, value_int);
            value_int = digits = 0;
        }
    }
    *strp = str-1;
    *lenp = len+1;

    if (value_big == NULL) return Scm_MakeInteger(value_int);
    if (digits > 0) {
        value_big = Scm_BignumAccMultAddUI(value_big, 
                                           ipow(radix, digits),
                                           value_int);
    }
    return Scm_NormalizeBignum(SCM_BIGNUM(value_big));
}

/*
 * Find a double number closest to f * 10^e, using z as the starting
 * approximation.  The algorithm (and its name) is taken from Will Clinger's
 * paper "How to Read Floating Point Numbers Accurately", in the ACM
 * SIGPLAN '90, pp.92--101.
 * The algorithm is modified to take advantage of coherency between loops.
 */
static double algorithmR(ScmObj f, int e, double z)
{
    ScmObj m, x, y, abs_d, d2;
    int k, s, kprev, sign_d;
    m = Scm_DecodeFlonum(z, &k, &s);
    IEXPT10_INIT();
  retry:
    if (k >= 0) {
        if (e >= 0) {
            x = Scm_Multiply2(f, iexpt10(e));
            y = Scm_Ash(m, k);
        } else {
            x = f;
            y = Scm_Ash(Scm_Multiply2(m, iexpt10(-e)), k);
        }
    } else {
        if (e >= 0) {
            x = Scm_Ash(Scm_Multiply2(f, iexpt10(e)), -k);
            y = m;
        } else {
            x = Scm_Ash(f, -k);
            y = Scm_Multiply2(m, iexpt10(-e));
        }
    }
    kprev = k;

    /* compare  */
    for (;;) {
        /*Scm_Printf(SCM_CURERR, "z=%.20lg,\nx=%S,\ny=%S\nf=%S\nm=%S\ne=%d, k=%d\n", z, x, y, f, m, e, k);*/
        /* compare */
        sign_d = Scm_NumCmp(x, y);
        abs_d = (sign_d > 0)? Scm_Subtract2(x, y) : Scm_Subtract2(y, x);
        d2 = Scm_Ash(Scm_Multiply2(m, abs_d), 1);
        switch (Scm_NumCmp(d2, y)) {
        case -1: /* d2 < y */
            if (Scm_NumCmp(m, SCM_2_52) == 0
                && sign_d < 0
                && Scm_NumCmp(Scm_Ash(d2, 1), y) > 0) {
                goto prevfloat;
            } else {
                return ldexp(Scm_GetDouble(m), k);
            }
        case 0: /* d2 == y */
            if (!Scm_OddP(m)) {
                if (Scm_NumCmp(m, SCM_2_52) == 0
                    && sign_d < 0) {
                    goto prevfloat;
                } else {
                    return ldexp(Scm_GetDouble(m), k);
                }
            } else if (sign_d < 0) {
                goto prevfloat;
            } else {
                goto nextfloat;
            }
        default:
            if (sign_d < 0) goto prevfloat;
            else            goto nextfloat;
        }
      prevfloat:
        m = Scm_Subtract2(m, SCM_MAKE_INT(1));
        if (k > -1074 && Scm_NumCmp(m, SCM_2_52) < 0) {
            m = Scm_Ash(m, 1);
            k--;
        }
        goto next;
      nextfloat:
        m = Scm_Add2(m, SCM_MAKE_INT(1));
        if (Scm_NumCmp(m, SCM_2_53) >= 0) {
            m = Scm_Ash(m, -1);
            k++;
        }
        /*FALLTHROUGH*/
      next:
        if (kprev >= 0) {
            if (k >= 0) {
                /* k stays positive. x is invariant */
                if (e >= 0) {
                    y = Scm_Ash(m, k);
                } else {
                    y = Scm_Ash(Scm_Multiply2(m, iexpt10(-e)), k);
                }
            } else {
                /* k turned to negative */
                goto retry;
            }
        } else {
            if (k < 0) {
                /* k stays negative. */
                if (e >= 0) {
                    if (k != kprev) x = Scm_Ash(Scm_Multiply2(f, iexpt10(e)), -k);
                    y = m;
                } else {
                    if (k != kprev) x = Scm_Ash(f, -k);
                    y = Scm_Multiply2(m, iexpt10(-e));
                }
            } else {
                /* k turned to positive */
                goto retry;
            }
        }
    }
    /*NOTREACHED*/
}

static ScmObj read_real(const char **strp, int *lenp,
                        struct numread_packet *ctx)
{
    int minusp = FALSE, exp_minusp = FALSE;
    int fracdigs = 0;
    long exponent = 0;
    ScmObj intpart, fraction;

    switch (**strp) {
    case '-': minusp = TRUE;
        /* FALLTHROUGH */
    case '+':
        (*strp)++; (*lenp)--;
    }
    if ((*lenp) <= 0) return SCM_FALSE;

    /* Read integral part */
    if (**strp != '.') {
        intpart = read_uint(strp, lenp, ctx, SCM_FALSE);
        if ((*lenp) <= 0) {
            if (minusp) intpart = Scm_Negate(intpart);
            if (ctx->exactness == INEXACT) {
                return Scm_ExactToInexact(intpart);
            } else {
                return intpart;
            }
        }
        if (**strp == '/') {
            /* possibly rational */
            ScmObj denom, ratval;
            int lensave;
            
            if ((*lenp) <= 1) return SCM_FALSE;
            (*strp)++; (*lenp)--;
            lensave = *lenp;
            denom = read_uint(strp, lenp, ctx, SCM_FALSE);
            if (SCM_FALSEP(denom)) return SCM_FALSE;
            if (denom == SCM_MAKE_INT(0)) {
                if (lensave > *lenp) {
                    if (minusp) {
                        return Scm_MakeFlonum(-1.0/0.0);
                    } else {
                        return Scm_MakeFlonum(1.0/0.0);
                    }
                } else {
                    return SCM_FALSE;
                }
            }
            if (minusp) intpart = Scm_Negate(intpart);
            ratval = Scm_Divide2(intpart, denom);

            if (ctx->exactness == EXACT && !Scm_IntegerP(ratval)) {
                return numread_error("(exact non-integral rational number is not supported)",
                                     ctx);
            }
            if (ctx->exactness == INEXACT && !SCM_FLONUMP(ratval)) {
                return Scm_ExactToInexact(ratval);
            } else {
                return ratval;
            }
        }
        /* fallthrough */
    } else {
        intpart = SCM_FALSE; /* indicate there was no intpart */
    }

    /* Read fractional part.
       At this point, simple integer is already eliminated. */
    if (**strp == '.') {
        int lensave;
        if (ctx->radix != 10) {
            return numread_error("(only 10-based fraction is supported)",
                                 ctx);
        }
        (*strp)++; (*lenp)--;
        lensave = *lenp;
        fraction = read_uint(strp, lenp, ctx, intpart);
        fracdigs = lensave - *lenp;
    } else {
        fraction = intpart;
    }

    if (SCM_FALSEP(intpart)) {
        if (fracdigs == 0) return SCM_FALSE; /* input was "." */
    }

    /* Read exponent.  */
    if (*lenp > 0 && strchr("eEsSfFdDlL", (int)**strp)) {
        (*strp)++;
        if (--(*lenp) <= 0) return SCM_FALSE;
        switch (**strp) {
        case '-': exp_minusp = TRUE;
            /*FALLTHROUGH*/
        case '+':
            (*strp)++;
            if (--(*lenp) <= 0) return SCM_FALSE;
        }
        while (*lenp > 0) {
            int c = **strp;
            if (!isdigit(c)) break;
            (*strp)++, (*lenp)--;
            if (isdigit(c)) {
                exponent = exponent * 10 + (c - '0');
                /* just reject obviously wrong exponent.  more precise
                   check will be done later. */
                if (exponent >= LONG_MAX/10 - 10) {
                    return numread_error("(exponent of floating-point number out of range)", ctx);
                }
            }
        }
        if (exp_minusp) exponent = -exponent;
    }

    /*Scm_Printf(SCM_CURERR, "fraction=%S, exponent=%d\n", fraction, exponent);*/
    /* Compose flonum.*/
    {
        double realnum = Scm_GetDouble(fraction);

        realnum = raise_pow10(realnum, exponent-fracdigs);
        if (realnum > 0.0
            && (Scm_NumCmp(fraction, SCM_2_52) > 0
                || exponent-fracdigs > MAX_EXACT_10_EXP
                || exponent-fracdigs < -MAX_EXACT_10_EXP)) {
            realnum = algorithmR(fraction, exponent-fracdigs, realnum);
        }
        if (minusp) realnum = -realnum;
        /* check exactness */
        if (ctx->exactness == EXACT) {
            double integ;
            if (modf(realnum, &integ) != 0.0) {
                return numread_error("(exact non-integral number is not supported)",
                                     ctx);
            }
            return Scm_InexactToExact(Scm_MakeFlonum(realnum));
        }
        return Scm_MakeFlonum(realnum);
    }
}

/* Entry point */
static ScmObj read_number(const char *str, int len, int radix, int strict)
{
    struct numread_packet ctx;
    int radix_seen = 0, exactness_seen = 0, sign_seen = 0;
    ScmObj realpart;

    ctx.buffer = str;
    ctx.buflen = len;
    ctx.exactness = NOEXACT;
    ctx.padread = FALSE;
    ctx.strict = strict;

#define CHK_EXACT_COMPLEX()                                                 \
    do {                                                                    \
        if (ctx.exactness == EXACT) {                                       \
            return numread_error("(exact complex number is not supported)", \
                                 &ctx);                                     \
        }                                                                   \
    } while (0)

    /* suggested radix.  may be overridden by prefix. */
    if (radix <= 1 || radix > 36) return SCM_FALSE;
    ctx.radix = radix;
    
    /* start from prefix part */
    for (; len >= 0; len-=2) {
        if (*str != '#') break;
        str++;
        switch (*str++) {
        case 'x':; case 'X':;
            if (radix_seen) return SCM_FALSE;
            ctx.radix = 16; radix_seen++;
            continue;
        case 'o':; case 'O':;
            if (radix_seen) return SCM_FALSE;
            ctx.radix = 8; radix_seen++;
            continue;
        case 'b':; case 'B':;
            if (radix_seen) return SCM_FALSE;
            ctx.radix = 2; radix_seen++;
            continue;
        case 'd':; case 'D':;
            if (radix_seen) return SCM_FALSE;
            ctx.radix = 10; radix_seen++;
            continue;
        case 'e':; case 'E':;
            if (exactness_seen) return SCM_FALSE;
            ctx.exactness = EXACT; exactness_seen++;
            continue;
        case 'i':; case 'I':;
            if (exactness_seen) return SCM_FALSE;
            ctx.exactness = INEXACT; exactness_seen++;
            continue;
        }
        return SCM_FALSE;
    }
    if (len <= 0) return SCM_FALSE;

    /* number body.  need to check the special case of pure imaginary */
    if (*str == '+' || *str == '-') {
        if (len == 1) return SCM_FALSE;
        if (len == 2 && (str[1] == 'i' || str[1] == 'I')) {
            CHK_EXACT_COMPLEX();
            return Scm_MakeComplex(0.0, (*str == '+')? 1.0 : -1.0);
        }
        sign_seen = TRUE;
    }

    realpart = read_real(&str, &len, &ctx);
    if (SCM_FALSEP(realpart) || len == 0) return realpart;

    switch (*str) {
    case '@':
        /* polar representation of complex*/
        if (len <= 1) {
            return SCM_FALSE;
        } else {
            ScmObj angle;
            double dmag, dangle;
            str++; len--;
            angle = read_real(&str, &len, &ctx);
            if (SCM_FALSEP(angle) || len != 0) return SCM_FALSE;
            CHK_EXACT_COMPLEX();
            dmag = Scm_GetDouble(realpart);
            dangle = Scm_GetDouble(angle);
            return Scm_MakeComplexPolar(dmag, dangle);
        }
    case '+':;
    case '-':
        /* rectangular representation of complex */
        if (len <= 1) {
            return SCM_FALSE;
        } else if (len == 2 && str[1] == 'i') {
            return Scm_MakeComplex(Scm_GetDouble(realpart),
                                   (*str == '+' ? 1.0 : -1.0));
        } else {
            ScmObj imagpart = read_real(&str, &len, &ctx);
            if (SCM_FALSEP(imagpart) || len != 1 || *str != 'i') {
                return SCM_FALSE;
            }
            CHK_EXACT_COMPLEX();
            if (Scm_Sign(imagpart) == 0) return realpart;
            return Scm_MakeComplex(Scm_GetDouble(realpart), 
                                   Scm_GetDouble(imagpart));
        }
    case 'i':
        /* '+' <ureal> 'i'  or '-' <ureal> 'i' */
        if (!sign_seen || len != 1) return SCM_FALSE;
        CHK_EXACT_COMPLEX();
        if (Scm_Sign(realpart) == 0) return Scm_MakeFlonum(0.0);
        else return Scm_MakeComplex(0.0, Scm_GetDouble(realpart));
    default:
        return SCM_FALSE;
    }
}

static ScmObj numread_error(const char *msg, struct numread_packet *context)
{
    if (context->strict) {
        Scm_Error("bad number format %s: %A", msg,
                  Scm_MakeString(context->buffer, context->buflen,
                                 context->buflen, 0));
    }
    return SCM_FALSE;
}


ScmObj Scm_StringToNumber(ScmString *str, int radix, int strict)
{
    u_int len, size;
    const char *p = Scm_GetStringContent(str, &size, &len, NULL);
    if (size != len) {
        /* This can't be a proper number. */
        return SCM_FALSE;
    } else {
        return read_number(p, size, radix, strict);
    }
}

/*
 * Initialization
 */

ScmObj Scm__ConstObjs[SCM_NUM_CONST_OBJS] = { SCM_FALSE };

void Scm__InitNumber(void)
{
    ScmModule *mod = Scm_GaucheModule();
    int radix, i;
    u_long n;
    
    for (radix = RADIX_MIN; radix <= RADIX_MAX; radix++) {
        longlimit[radix-RADIX_MIN] =
            (u_long)floor((double)LONG_MAX/radix - radix);
        /* Find max D where R^(D+1)-1 <= LONG_MAX */
        for (i = 0, n = 1; ; i++, n *= radix) {
            if (n >= LONG_MAX/radix) {
                longdigs[radix-RADIX_MIN] = i-1;
                bigdig[radix-RADIX_MIN] = n;
                break;
            }
        }
    }
    
    SCM_2_63 = Scm_Ash(SCM_MAKE_INT(1), 63);
    SCM_2_64 = Scm_Ash(SCM_MAKE_INT(1), 64);
    SCM_2_64_MINUS_1 = Scm_Subtract2(SCM_2_64, SCM_MAKE_INT(1));
    SCM_2_52 = Scm_Ash(SCM_MAKE_INT(1), 52);
    SCM_2_53 = Scm_Ash(SCM_MAKE_INT(1), 53);
    SCM_MINUS_2_63 = Scm_Negate(SCM_2_63);
    SCM_2_32 = Scm_Ash(SCM_MAKE_INT(1), 32);
    SCM_2_31 = Scm_Ash(SCM_MAKE_INT(1), 31);
    SCM_MINUS_2_31 = Scm_Negate(SCM_2_31);
    
    dexpt2_minus_52 = ldexp(1.0, -52);
    dexpt2_minus_53 = ldexp(1.0, -53);

    Scm_InitBuiltinGeneric(&generic_add, "object-+", mod);
    Scm_InitBuiltinGeneric(&generic_sub, "object--", mod);
    Scm_InitBuiltinGeneric(&generic_mul, "object-*", mod);
    Scm_InitBuiltinGeneric(&generic_div, "object-/", mod);
}
