/*
 * number.h - Public API for Scheme numbers
 *
 *   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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
#define SCM_RADIX_MIN              2

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

struct ScmCompnumRec {
    SCM_HEADER;
    double real;
    double imag;
};

#define SCM_COMPNUM(obj)           ((ScmCompnum*)(obj))
#define SCM_COMPNUMP(obj)          SCM_XTYPEP(obj, SCM_CLASS_COMPLEX)
#define SCM_COMPNUM_REAL(obj)      SCM_COMPNUM(obj)->real
#define SCM_COMPNUM_IMAG(obj)      SCM_COMPNUM(obj)->imag

/*
   Internal macros concerning FFX (fast flonum extension).  Normal
   users doesn't need to care about these.

   When GAUCHE_FFX is defined,
   new flonums generated by the system is put in the designated area
   of the VM instead of the heap, thus eliminating most of
   intermediate flonum allocations.  Flonum pointers have two flavors;
   if SCM_FLONUM_REG_P is true, it is allocated in the VM registers,
   and if SCM_FLONUM_MEM_P is true, it is allocated in heap.  An
   invariance: only ScmObjs in VM stack are allowed to be FLONUM_REGs.
   If such an ScmObj is moved to the heap, a new FLONUM_MEM is
   allocated and the pointer is fixed.

   User routines don't usually receive or return FLONUM_REGs.  If the
   routine is directly called from VM as SUBR, and it never stores
   received ScmObjs in the heap (in other words, if the routine
   immediately retrieves doubles from the received ScmObjs), the
   routine denotes the fact by marking its stub with "immediate-arg"
   so that VM can pass FLONUM_REGs.  If the routine is sure that its
   return value is used directly by the VM, it can call
   Scm_VMReturnFlonum to generate FLONUM_REGs.
 */

#define SCM_MAKE_FLONUM_REG(dptr)  SCM_OBJ(SCM_WORD(dptr)+0x02)
#define SCM_MAKE_FLONUM_MEM(dptr)  SCM_OBJ(SCM_WORD(dptr)+0x06)

#define SCM_FLONUM_REG_P(obj)      ((SCM_WORD(obj)&0x7) == 0x02)
#define SCM_FLONUM_MEM_P(obj)      ((SCM_WORD(obj)&0x7) == 0x06)

#if GAUCHE_FFX
#define SCM_FLONUM_ENSURE_MEM(var)                              \
    do {                                                        \
        if (SCM_FLONUM_REG_P(var)) {                            \
            var = Scm_MakeFlonum(SCM_FLONUM_VALUE(var));        \
        }                                                       \
    } while (0)
#else  /*!GAUCHE_FFX*/
#define SCM_FLONUM_ENSURE_MEM(var) /* empty */
#endif /*!GAUCHE_FFX*/

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

SCM_EXTERN int    Scm_ClampMode(ScmObj clamp);

SCM_EXTERN ScmObj Scm_MakeInteger(long i);
SCM_EXTERN ScmObj Scm_MakeIntegerU(u_long i);
SCM_EXTERN ScmObj Scm_MakeIntegerFromUIArray(int sign,
                                             const u_long *values,
                                             int size);

SCM_EXTERN long   Scm_GetIntegerClamp(ScmObj obj, int clamp, int *oor);
SCM_EXTERN u_long Scm_GetIntegerUClamp(ScmObj obj, int clamp, int *oor);
SCM_EXTERN int    Scm_GetInteger8Clamp(ScmObj obj, int clamp, int *oor);
SCM_EXTERN u_int  Scm_GetIntegerU8Clamp(ScmObj obj, int clamp, int *oor);
SCM_EXTERN int    Scm_GetInteger16Clamp(ScmObj obj, int clamp, int *oor);
SCM_EXTERN u_int  Scm_GetIntegerU16Clamp(ScmObj obj, int clamp, int *oor);
SCM_EXTERN int32_t  Scm_GetInteger32Clamp(ScmObj obj, int clamp, int *oor);
SCM_EXTERN uint32_t Scm_GetIntegerU32Clamp(ScmObj obj, int clamp, int *oor);

SCM_EXTERN u_long Scm_GetIntegerUMod(ScmObj obj);

/* 64bit integer stuff */
#if SIZEOF_LONG == 4
SCM_EXTERN ScmObj Scm_MakeInteger64(int64_t i);
SCM_EXTERN ScmObj Scm_MakeIntegerU64(uint64_t i);
SCM_EXTERN int64_t  Scm_GetInteger64Clamp(ScmObj obj, int clamp, int *oor);
SCM_EXTERN uint64_t Scm_GetIntegerU64Clamp(ScmObj obj, int clamp, int *oor);
#else  /* SIZEOF_LONG >= 8 */
#define Scm_MakeInteger64      Scm_MakeInteger
#define Scm_MakeIntegerU64     Scm_MakeIntegerU
#define Scm_GetInteger64Clamp  Scm_GetIntegerClamp
#define Scm_GetIntegerU64Clamp Scm_GetIntegerUClamp
#endif /* SIZEOF_LONG >= 8 */

/* Convenience macros - Scm_GetInteger() family throws an error when
   the input is out-of-range.
   NB: Before 0.9.4, we only had Scm_GetInteger, Scm_GetIntegerU,
   Scm_GetInteger64 and Scm_GetIntegerU64, and they were clamping.
   We change it in 0.9.4 to make error-signaling default, in the spirit
   of safety by default.
*/
#define Scm_GetInteger(x)    Scm_GetIntegerClamp(x, SCM_CLAMP_ERROR, NULL)
#define Scm_GetIntegerU(x)   Scm_GetIntegerUClamp(x, SCM_CLAMP_ERROR, NULL)
#define Scm_GetInteger8(x)   Scm_GetInteger8Clamp(x, SCM_CLAMP_ERROR, NULL)
#define Scm_GetIntegerU8(x)  Scm_GetIntegerU8Clamp(x, SCM_CLAMP_ERROR, NULL)
#define Scm_GetInteger16(x)  Scm_GetInteger16Clamp(x, SCM_CLAMP_ERROR, NULL)
#define Scm_GetIntegerU16(x) Scm_GetIntegerU16Clamp(x, SCM_CLAMP_ERROR, NULL)
#define Scm_GetInteger32(x)  Scm_GetInteger32Clamp(x, SCM_CLAMP_ERROR, NULL)
#define Scm_GetIntegerU32(x) Scm_GetIntegerU32Clamp(x, SCM_CLAMP_ERROR, NULL)
#define Scm_GetInteger64(x)  Scm_GetInteger64Clamp(x, SCM_CLAMP_ERROR, NULL)
#define Scm_GetIntegerU64(x) Scm_GetIntegerU64Clamp(x, SCM_CLAMP_ERROR, NULL)

/* for backward compatibility -- will be gone soon */
#define Scm_MakeIntegerFromUI(x) Scm_MakeIntegerU(x)
#define Scm_GetUInteger(x)       Scm_GetIntegerU(x)

/* System-dependent integral types */
SCM_EXTERN int       Scm_IntegerFitsSizeP(ScmObj);
SCM_EXTERN size_t    Scm_IntegerToSize(ScmObj);
SCM_EXTERN ScmObj    Scm_SizeToInteger(size_t);
SCM_EXTERN int       Scm_IntegerFitsSsizeP(ScmObj);
SCM_EXTERN ssize_t   Scm_IntegerToSsize(ScmObj);
SCM_EXTERN ScmObj    Scm_SsizeToInteger(ssize_t);
SCM_EXTERN int       Scm_IntegerFitsPtrdiffP(ScmObj);
SCM_EXTERN ptrdiff_t Scm_IntegerToPtrdiffP(ScmObj);
SCM_EXTERN ScmObj    Scm_PtrdiffToInteger(ssize_t);
SCM_EXTERN int       Scm_IntegerFitsOffsetP(ScmObj);
SCM_EXTERN off_t     Scm_IntegerToOffset(ScmObj);
SCM_EXTERN ScmObj    Scm_OffsetToInteger(off_t);
SCM_EXTERN int       Scm_IntegerFitsIntptrP(ScmObj);
SCM_EXTERN intptr_t  Scm_IntegerToIntptr(ScmObj);
SCM_EXTERN ScmObj    Scm_IntptrToInteger(intptr_t);

SCM_EXTERN ScmObj Scm_MakeRational(ScmObj numer, ScmObj denom);
SCM_EXTERN ScmObj Scm_MakeRatnum(ScmObj numer, ScmObj denom);
SCM_EXTERN ScmObj Scm_ReduceRational(ScmObj rational);

SCM_EXTERN ScmObj Scm_MakeFlonum(double d);
SCM_EXTERN double Scm_GetDouble(ScmObj obj);
SCM_EXTERN ScmObj Scm_DecodeFlonum(double d, int *exp, int *sign);
SCM_EXTERN double Scm_EncodeFlonum(ScmObj mant, int exp, int sign);
SCM_EXTERN int    Scm_FlonumSign(double d);
SCM_EXTERN ScmObj Scm_MakeFlonumToNumber(double d, int exactp);
SCM_EXTERN double       Scm_HalfToDouble(ScmHalfFloat v);
SCM_EXTERN ScmHalfFloat Scm_DoubleToHalf(double v);

SCM_EXTERN ScmObj Scm_MakeCompnum(double real, double imag);
SCM_EXTERN ScmObj Scm_MakeComplex(double real, double imag);
SCM_EXTERN ScmObj Scm_MakeComplexPolar(double magnitude, double angle);
SCM_EXTERN ScmHalfComplex Scm_GetHalfComplex(ScmObj obj);
SCM_EXTERN ScmFloatComplex  Scm_GetFloatComplex(ScmObj obj);
SCM_EXTERN ScmDoubleComplex Scm_GetDoubleComplex(ScmObj obj);
SCM_EXTERN ScmObj Scm_HalfComplexToComplex(const ScmHalfComplex c);
SCM_EXTERN ScmObj Scm_FloatComplexToComplex(const ScmFloatComplex c);
SCM_EXTERN ScmObj Scm_DoubleComplexToComplex(const ScmDoubleComplex c);

SCM_EXTERN int    Scm_IntegerP(ScmObj obj);
SCM_EXTERN int    Scm_OddP(ScmObj obj);
SCM_EXTERN int    Scm_FiniteP(ScmObj obj);
SCM_EXTERN int    Scm_InfiniteP(ScmObj obj);
SCM_EXTERN int    Scm_NanP(ScmObj obj);
SCM_EXTERN ScmObj Scm_Abs(ScmObj obj);
SCM_EXTERN int    Scm_Sign(ScmObj obj);
SCM_EXTERN ScmObj Scm_Negate(ScmObj obj);
SCM_EXTERN ScmObj Scm_Reciprocal(ScmObj obj);
SCM_EXTERN ScmObj Scm_ReciprocalInexact(ScmObj obj);

/* TODO: We'll switch to make Scm_Exact/Scm_Inexact official in 1.0
   and obsolete longer versions.  During 0.9.x we keep the old name
   for binary compatibility. */
SCM_EXTERN ScmObj Scm_InexactToExact(ScmObj obj);
SCM_EXTERN ScmObj Scm_ExactToInexact(ScmObj obj);
#define Scm_Exact    Scm_InexactToExact
#define Scm_Inexact  Scm_ExactToInexact

SCM_EXTERN ScmObj Scm_Add(ScmObj arg1, ScmObj arg2);
SCM_EXTERN ScmObj Scm_Sub(ScmObj arg1, ScmObj arg2);
SCM_EXTERN ScmObj Scm_Mul(ScmObj arg1, ScmObj arg2);
SCM_EXTERN ScmObj Scm_Div(ScmObj arg1, ScmObj arg2);
SCM_EXTERN ScmObj Scm_DivInexact(ScmObj arg1, ScmObj arg2);
SCM_EXTERN ScmObj Scm_DivCompat(ScmObj arg1, ScmObj arg2);

SCM_EXTERN ScmObj Scm_Quotient(ScmObj arg1, ScmObj arg2, ScmObj *rem);
SCM_EXTERN ScmObj Scm_Modulo(ScmObj arg1, ScmObj arg2, int remainder);
SCM_EXTERN ScmObj Scm_Gcd(ScmObj x, ScmObj y);

SCM_EXTERN ScmObj Scm_Expt(ScmObj x, ScmObj y);
SCM_EXTERN ScmObj Scm_ExactIntegerExpt(ScmObj x, ScmObj y);
SCM_EXTERN long   Scm_TwosPower(ScmObj n);
SCM_EXTERN double Scm_SinPi(double x);
SCM_EXTERN double Scm_CosPi(double x);
SCM_EXTERN double Scm_TanPi(double x);

SCM_EXTERN int    Scm_NumEq(ScmObj x, ScmObj y);
SCM_EXTERN int    Scm_NumLT(ScmObj x, ScmObj y);
SCM_EXTERN int    Scm_NumLE(ScmObj x, ScmObj y);
SCM_EXTERN int    Scm_NumGT(ScmObj x, ScmObj y);
SCM_EXTERN int    Scm_NumGE(ScmObj x, ScmObj y);
SCM_EXTERN int    Scm_NumCmp(ScmObj x, ScmObj y);
SCM_EXTERN void   Scm_MinMax(ScmObj arg0, ScmObj args, ScmObj *min, ScmObj *max);

SCM_EXTERN ScmObj Scm_LogAnd(ScmObj x, ScmObj y);
SCM_EXTERN ScmObj Scm_LogIor(ScmObj x, ScmObj y);
SCM_EXTERN ScmObj Scm_LogXor(ScmObj x, ScmObj y);
SCM_EXTERN ScmObj Scm_LogNot(ScmObj x);
SCM_EXTERN ScmObj Scm_Ash(ScmObj x, ScmSmallInt cnt);

enum ScmRoundMode {
    SCM_ROUND_FLOOR,
    SCM_ROUND_CEIL,
    SCM_ROUND_TRUNC,
    SCM_ROUND_ROUND
};
SCM_EXTERN ScmObj Scm_Round(ScmObj num, int mode);
SCM_EXTERN ScmObj Scm_RoundToExact(ScmObj num, int mode);

SCM_EXTERN ScmObj Scm_Numerator(ScmObj n);
SCM_EXTERN ScmObj Scm_Denominator(ScmObj n);

SCM_EXTERN double Scm_Magnitude(ScmObj z);
SCM_EXTERN double Scm_Angle(ScmObj z);
SCM_EXTERN double Scm_RealPart(ScmObj z);
SCM_EXTERN double Scm_ImagPart(ScmObj z);

/* Flags for ScmNumberFormat.  Scm_NumberToString and Scm_StringToNumber
   also take those flags.  The [N] and [S] mark the meaning of the flag
   in Scm_NumberToString and Scm_StringToNumber, respectively. */
enum ScmNumberFormatFlags {
    SCM_NUMBER_FORMAT_USE_UPPER = (1L<<0), /* use ABCDEF.. for base > 10
                                              [N] same.
                                              [S] ignored. */
    SCM_NUMBER_FORMAT_SHOW_PLUS = (1L<<1), /* show '+' in positive number
                                              [N] same.
                                              [S] ignored. */
    SCM_NUMBER_FORMAT_ALT_RADIX = (1L<<2), /* alternative radix prefix handling
                                              specifying non-default behavior
                                              (the actual behavior differs
                                              between N and S:
                                              [N] always add radix prefix
                                              [S] never allow radix prefix */
    SCM_NUMBER_FORMAT_ROUND_NOTATIONAL = (1L<<3),
                                           /* Using notational decimal rounding.
                                              [N] When rounding fractional part,
                                              we first generate the closest
                                              decimal notation, then round.
                                              [S] ignored.
                                            */
    SCM_NUMBER_FORMAT_STRICT_R7RS = (1L<<4),/*[N] ignored.
                                              [S] reject syntax outside of R7RS*/
    SCM_NUMBER_FORMAT_EXACT = (1L<<5),    /* [N] ignored.
                                             [S] treat as if #e is present if
                                             no exactness prefix is given */
    SCM_NUMBER_FORMAT_INEXACT = (1L<<6)   /* [N] ignored.
                                             [S] treat as if #i is present if
                                             no exactness prefix is given */
};

typedef struct ScmNumberFormatRec {
    u_long flags;
    int radix;
    int precision;    /* # of digits after decimal point, -1 for unlimited */
    int exp_lo;       /* use exp notation if exponent <= exp_lo */
    int exp_hi;       /* use exp notation if exponent >= exp_hi */
    int exp_width;    /* min # of digits used for exponent */
} ScmNumberFormat;

SCM_EXTERN void   Scm_NumberFormatInit(ScmNumberFormat*);
SCM_EXTERN size_t Scm_PrintNumber(ScmPort *port, ScmObj n, ScmNumberFormat *f);
SCM_EXTERN size_t Scm_PrintDouble(ScmPort *port, double d, ScmNumberFormat *f);

/* Higher-level convenience routines */
SCM_EXTERN ScmObj Scm_NumberToString(ScmObj num, int radix, u_long flags);
SCM_EXTERN ScmObj Scm_StringToNumber(ScmString *str, int radix, u_long flags);

/* The Scm_VM* version leaves unboxed flonum (FLONUM_REG) in VM's VAL0
   register.   They can only be called "on VM", that is, when its return
   value is used immediately by VM. */

SCM_EXTERN ScmObj Scm_VMNegate(ScmObj obj);
SCM_EXTERN ScmObj Scm_VMReciprocal(ScmObj obj);
SCM_EXTERN ScmObj Scm_VMReciprocalInexact(ScmObj obj);
SCM_EXTERN ScmObj Scm_VMExactToInexact(ScmObj obj); /* during 0.9 for backward compatibility */
#define Scm_VMInexact Scm_VMExactToInexact          /* on 1.0, shorter name will be a real name */
SCM_EXTERN ScmObj Scm_VMAbs(ScmObj obj);
SCM_EXTERN ScmObj Scm_VMAdd(ScmObj arg1, ScmObj arg2);
SCM_EXTERN ScmObj Scm_VMSub(ScmObj arg1, ScmObj arg2);
SCM_EXTERN ScmObj Scm_VMMul(ScmObj arg1, ScmObj arg2);
SCM_EXTERN ScmObj Scm_VMDiv(ScmObj arg1, ScmObj arg2);
SCM_EXTERN ScmObj Scm_VMDivInexact(ScmObj arg1, ScmObj arg2);
SCM_EXTERN ScmObj Scm_VMExpt(ScmObj x, ScmObj y);
SCM_EXTERN ScmObj Scm_VMRound(ScmObj num, int mode);

#endif /* GAUCHE_NUMBER_H */
