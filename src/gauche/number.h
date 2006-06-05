/*
 * number.h - Public API for Scheme numbers
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
 *  $Id: number.h,v 1.3 2006-06-05 05:11:25 shirok Exp $
 */

/* This file is included from gauche.h */

#ifndef GAUCHE_NUMBER_H
#define GAUCHE_NUMBER_H

/* "Normalized" numbers
 *
 * In Scheme world, numbers should be always in normalized form.
 *
 *  - Exact integers that can be representable in fixnum should be in
 *    the fixnum form, not in the bignum form.
 *  - Complex numbers whose imaginary part is 0.0 should be in the flonum
 *    form, not in the complexnum form.
 *
 * Some C API returns anormalized numbers to avoid unnecessary
 * conversion overhead.  These anormalized numbers shuold be used
 * strictly in the intermediate form within C world.  Anything that
 * is passed back to Scheme world must be normalized.
 */

#define SCM_SMALL_INT_SIZE         (SIZEOF_LONG*8-3)
#define SCM_SMALL_INT_MAX          ((1L << SCM_SMALL_INT_SIZE) - 1)
#define SCM_SMALL_INT_MIN          (-SCM_SMALL_INT_MAX-1)
#define SCM_SMALL_INT_FITS(k) \
    (((k)<=SCM_SMALL_INT_MAX)&&((k)>=SCM_SMALL_INT_MIN))

#define SCM_RADIX_MAX              36

#define SCM_INTEGERP(obj)          (SCM_INTP(obj) || SCM_BIGNUMP(obj))
#define SCM_RATIONALP(obj)         (SCM_INTEGERP(obj)||SCM_RATNUMP(obj))
#define SCM_REALP(obj)             (SCM_RATIONALP(obj)||SCM_FLONUMP(obj))
#define SCM_NUMBERP(obj)           (SCM_REALP(obj)||SCM_COMPNUMP(obj))
#define SCM_EXACTP(obj)            SCM_RATIONALP(obj)
#define SCM_INEXACTP(obj)          (SCM_FLONUMP(obj)||SCM_COMPNUMP(obj))

#define SCM_EXACT_ZERO_P(obj)      SCM_EQ(obj, SCM_MAKE_INT(0))
#define SCM_EXACT_ONE_P(obj)       SCM_EQ(obj, SCM_MAKE_INT(1))

#define SCM_UINTEGERP(obj) \
    (SCM_UINTP(obj) || (SCM_BIGNUMP(obj)&&SCM_BIGNUM_SIGN(obj)>=0))

SCM_CLASS_DECL(Scm_NumberClass);
SCM_CLASS_DECL(Scm_ComplexClass);
SCM_CLASS_DECL(Scm_RealClass);
SCM_CLASS_DECL(Scm_RationalClass);
SCM_CLASS_DECL(Scm_IntegerClass);

#define SCM_CLASS_NUMBER        (&Scm_NumberClass)
#define SCM_CLASS_COMPLEX       (&Scm_ComplexClass)
#define SCM_CLASS_REAL          (&Scm_RealClass)
#define SCM_CLASS_RATIONAL      (&Scm_RationalClass)
#define SCM_CLASS_INTEGER       (&Scm_IntegerClass)

struct ScmBignumRec {
    SCM_HEADER;
    int sign : 2;
    unsigned int size : (SIZEOF_INT*CHAR_BIT-2);
    unsigned long values[1];           /* variable length */
};

#define SCM_BIGNUM(obj)        ((ScmBignum*)(obj))
#define SCM_BIGNUMP(obj)       SCM_XTYPEP(obj, SCM_CLASS_INTEGER)
#define SCM_BIGNUM_SIZE(obj)   SCM_BIGNUM(obj)->size
#define SCM_BIGNUM_SIGN(obj)   SCM_BIGNUM(obj)->sign

#define SCM_BIGNUM_MAX_DIGITS  ((1UL<<(SIZEOF_INT*CHAR_BIT-2))-1)

struct ScmRatnumRec {
    SCM_HEADER;
    ScmObj numerator;
    ScmObj denominator;
};

#define SCM_RATNUM(obj)            ((ScmRatnum*)(obj))
#define SCM_RATNUMP(obj)           SCM_XTYPEP(obj, SCM_CLASS_RATIONAL)
#define SCM_RATNUM_NUMER(obj)      SCM_RATNUM(obj)->numerator
#define SCM_RATNUM_DENOM(obj)      SCM_RATNUM(obj)->denominator

struct ScmFlonumRec {
    SCM_HEADER;
    double value;
};

#define SCM_FLONUM(obj)            ((ScmFlonum*)(obj))
#define SCM_FLONUMP(obj)           SCM_XTYPEP(obj, SCM_CLASS_REAL)
#define SCM_FLONUM_VALUE(obj)      (SCM_FLONUM(obj)->value)

struct ScmCompnumRec {
    SCM_HEADER;
    double real;
    double imag;
};

#define SCM_COMPNUM(obj)           ((ScmCompnum*)(obj))
#define SCM_COMPNUMP(obj)          SCM_XTYPEP(obj, SCM_CLASS_COMPLEX)
#define SCM_COMPNUM_REAL(obj)      SCM_COMPNUM(obj)->real
#define SCM_COMPNUM_IMAG(obj)      SCM_COMPNUM(obj)->imag

/* Converting a Scheme number to a C number:
 *
 * It's a tricky business.  It's always possible that the Scheme number
 * you got may not fit into the desired C variable.  There are several
 * options you can choose.
 *
 *  - Error.  Throws an error.
 *  - Clamping.  If the Scheme value falls out of the supported range
 *    of C variable, use the closest representable value.
 *  - Convert only when possible.  If conversion is not possible, use
 *    the Scheme value as-is.  It is useful to provide a shortcut path
 *    to improve performance.
 *
 * Some APIs take 'clamp' argument to specify the behavior.  The value
 * can be one of the SCM_CLAMP_* enums.  If an API supports SCM_CLAMP_NONE,
 * it also takes an output argument to return a flag whether the argument
 * is out of range or not.  This output argument can be NULL if the caller 
 * doesn't specify SCM_CLAMP_NONE flag.
 */

enum ScmClampMode {
    SCM_CLAMP_ERROR = 0,       /* throws an error when out-of-range */
    SCM_CLAMP_HI = 1,
    SCM_CLAMP_LO = 2,
    SCM_CLAMP_BOTH = 3,
    SCM_CLAMP_NONE = 4         /* do not convert when out-of-range */
};

SCM_EXTERN ScmObj Scm_MakeInteger(long i);
SCM_EXTERN ScmObj Scm_MakeIntegerU(u_long i);

SCM_EXTERN long   Scm_GetIntegerClamp(ScmObj obj, int clamp, int *oor);
SCM_EXTERN u_long Scm_GetIntegerUClamp(ScmObj obj, int clamp, int *oor);
#define Scm_GetInteger(x)  Scm_GetIntegerClamp(x, SCM_CLAMP_BOTH, NULL)
#define Scm_GetIntegerU(x) Scm_GetIntegerUClamp(x, SCM_CLAMP_BOTH, NULL)

SCM_EXTERN ScmInt32  Scm_GetInteger32Clamp(ScmObj obj, int clamp, int *oor);
SCM_EXTERN ScmUInt32 Scm_GetIntegerU32Clamp(ScmObj obj, int clamp, int *oor);

/* 64bit integer stuff */
#if SIZEOF_LONG == 4
SCM_EXTERN ScmObj Scm_MakeInteger64(ScmInt64 i);
SCM_EXTERN ScmObj Scm_MakeIntegerU64(ScmUInt64 i);
SCM_EXTERN ScmInt64  Scm_GetInteger64Clamp(ScmObj obj, int clamp, int *oor);
SCM_EXTERN ScmUInt64 Scm_GetIntegerU64Clamp(ScmObj obj, int clamp, int *oor);
#else  /* SIZEOF_LONG >= 8 */
#define Scm_MakeInteger64      Scm_MakeInteger
#define Scm_MakeIntegerU64     Scm_MakeIntegerU
#define Scm_GetInteger64Clamp  Scm_GetIntegerClamp
#define Scm_GetIntegerU64Clamp Scm_GetIntegerUClamp
#endif /* SIZEOF_LONG >= 8 */
#define Scm_GetInteger64(x)    Scm_GetInteger64Clamp(x, SCM_CLAMP_BOTH, NULL)
#define Scm_GetIntegerU64(x)   Scm_GetIntegerU64Clamp(x, SCM_CLAMP_BOTH, NULL)

/* for backward compatibility -- will be gone soon */
#define Scm_MakeIntegerFromUI Scm_MakeIntegerU
#define Scm_GetUInteger       Scm_GetIntegerU

SCM_EXTERN ScmObj Scm_MakeRational(ScmObj numer, ScmObj denom);
SCM_EXTERN ScmObj Scm_MakeRatnum(ScmObj numer, ScmObj denom);
SCM_EXTERN ScmObj Scm_ReduceRational(ScmObj rational);

SCM_EXTERN ScmObj Scm_MakeFlonum(double d);
SCM_EXTERN double Scm_GetDouble(ScmObj obj);
SCM_EXTERN ScmObj Scm_DecodeFlonum(double d, int *exp, int *sign);
SCM_EXTERN ScmObj Scm_MakeFlonumToNumber(double d, int exactp);

SCM_EXTERN ScmObj Scm_MakeCompnum(double real, double imag);
SCM_EXTERN ScmObj Scm_MakeComplex(double real, double imag);
SCM_EXTERN ScmObj Scm_MakeComplexPolar(double magnitude, double angle);

SCM_EXTERN int    Scm_IntegerP(ScmObj obj);
SCM_EXTERN int    Scm_OddP(ScmObj obj);
SCM_EXTERN ScmObj Scm_Abs(ScmObj obj);
SCM_EXTERN int    Scm_Sign(ScmObj obj);
SCM_EXTERN ScmObj Scm_Negate(ScmObj obj);
SCM_EXTERN ScmObj Scm_Reciprocal(ScmObj obj);
SCM_EXTERN ScmObj Scm_ExactToInexact(ScmObj obj);
SCM_EXTERN ScmObj Scm_InexactToExact(ScmObj obj);

SCM_EXTERN ScmObj Scm_Add(ScmObj arg1, ScmObj arg2);
SCM_EXTERN ScmObj Scm_Sub(ScmObj arg1, ScmObj arg2);
SCM_EXTERN ScmObj Scm_Mul(ScmObj arg1, ScmObj arg2);
SCM_EXTERN ScmObj Scm_Div(ScmObj arg1, ScmObj arg2);

SCM_EXTERN ScmObj Scm_Quotient(ScmObj arg1, ScmObj arg2, ScmObj *rem);
SCM_EXTERN ScmObj Scm_Modulo(ScmObj arg1, ScmObj arg2, int remainder);
SCM_EXTERN ScmObj Scm_Gcd(ScmObj x, ScmObj y);

SCM_EXTERN ScmObj Scm_Expt(ScmObj x, ScmObj y);

SCM_EXTERN int    Scm_NumEq(ScmObj x, ScmObj y);
SCM_EXTERN int    Scm_NumCmp(ScmObj x, ScmObj y);
SCM_EXTERN void   Scm_MinMax(ScmObj arg0, ScmObj args, ScmObj *min, ScmObj *max);

SCM_EXTERN ScmObj Scm_LogAnd(ScmObj x, ScmObj y);
SCM_EXTERN ScmObj Scm_LogIor(ScmObj x, ScmObj y);
SCM_EXTERN ScmObj Scm_LogXor(ScmObj x, ScmObj y);
SCM_EXTERN ScmObj Scm_LogNot(ScmObj x);
SCM_EXTERN int    Scm_LogTest(ScmObj x, ScmObj y);
SCM_EXTERN int    Scm_LogBit(ScmObj x, int bit);
SCM_EXTERN ScmObj Scm_Ash(ScmObj x, int cnt);
    
enum ScmRoundMode {
    SCM_ROUND_FLOOR,
    SCM_ROUND_CEIL,
    SCM_ROUND_TRUNC,
    SCM_ROUND_ROUND
};
SCM_EXTERN ScmObj Scm_Round(ScmObj num, int mode);

SCM_EXTERN ScmObj Scm_Numerator(ScmObj n);
SCM_EXTERN ScmObj Scm_Denominator(ScmObj n);

SCM_EXTERN double Scm_Magnitude(ScmObj z);
SCM_EXTERN double Scm_Angle(ScmObj z);
SCM_EXTERN double Scm_RealPart(ScmObj z);
SCM_EXTERN double Scm_ImagPart(ScmObj z);

SCM_EXTERN ScmObj Scm_NumberToString(ScmObj num, int radix, int use_upper);
SCM_EXTERN ScmObj Scm_StringToNumber(ScmString *str, int radix, int strict);

SCM_EXTERN void   Scm_PrintDouble(ScmPort *port, double d, int flags);

#endif /* GAUCHE_NUMBER_H */

