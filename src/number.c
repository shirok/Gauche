/*
 * number.c - numeric functions
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
 *  $Id: number.c,v 1.63 2002-02-12 07:41:54 shirok Exp $
 */

#include <math.h>
#include <limits.h>
#include <string.h>
#include <ctype.h>
#define LIBGAUCHE_BODY
#include "gauche.h"

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

/* Linux gcc have those, but the declarations aren't included unless
   __USE_ISOC9X is defined.  Just in case. */
#ifdef HAVE_TRUNC
extern double trunc(double);
#endif
#ifdef HAVE_RINT
extern double rint(double);
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
    if (exact) {
        /* see if d can be demoted to integer */
        double i, f;
        f = modf(d, &i);
        if (f == 0.0) {
            if (i > SCM_SMALL_INT_MAX || i < SCM_SMALL_INT_MIN) {
                return Scm_MakeBignumFromDouble(i);
            } else {
                return SCM_MAKE_INT((int)i);
            }
        }
    }
    return Scm_MakeFlonum(d);
}

/*=======================================================================
 *  Complex numbers
 */

ScmObj Scm_MakeComplex(double r, double i)
{
    ScmComplex *c = SCM_NEW_ATOMIC(ScmComplex);
    SCM_SET_CLASS(c, SCM_CLASS_COMPLEX);
    c->real = r;
    c->imag = i;
    return SCM_OBJ(c);
}

ScmObj Scm_Magnitude(ScmObj z)
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
    return Scm_MakeFlonum(m);
}

ScmObj Scm_Angle(ScmObj z)
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
    return Scm_MakeFlonum(a);
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

ScmObj Scm_MakeIntegerFromUI(u_long i)
{
    if (i <= (u_long)SCM_SMALL_INT_MAX) return SCM_MAKE_INT(i);
    else return Scm_MakeBignumFromUI(i);
}

/* Convert scheme integer to C integer. Overflow is neglected. */
long Scm_GetInteger(ScmObj obj)
{
    if (SCM_INTP(obj)) return SCM_INT_VALUE(obj);
    else if (SCM_BIGNUMP(obj)) return Scm_BignumToSI(SCM_BIGNUM(obj));
    else if (SCM_FLONUMP(obj)) return (long)SCM_FLONUM_VALUE(obj);
    else return 0;
}

u_long Scm_GetUInteger(ScmObj obj)
{
    if (SCM_INTP(obj)) return SCM_INT_VALUE(obj);
    else if (SCM_BIGNUMP(obj)) return Scm_BignumToUI(SCM_BIGNUM(obj));
    else if (SCM_FLONUMP(obj)) return (u_long)SCM_FLONUM_VALUE(obj);
    else return 0;
}

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
        int v = SCM_INT_VALUE(obj);
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
    int r = 0;
    
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
        int v = SCM_INT_VALUE(obj);
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
        obj = Scm_MakeComplex(-SCM_COMPLEX_REAL(obj),
                              -SCM_COMPLEX_IMAG(obj));
    } else {
        Scm_Error("number required: %S", obj);
    }
    return obj;
}

ScmObj Scm_Reciprocal(ScmObj obj)
{
    if (SCM_INTP(obj)) {
        int val = SCM_INT_VALUE(obj);
        if (val == 0) Scm_Error("divide by zero");
        obj = Scm_MakeFlonum(1.0/(double)val);
    } else if (SCM_BIGNUMP(obj)) {
        double val = Scm_BignumToDouble(SCM_BIGNUM(obj));
        if (val == 0.0) Scm_Error("divide by zero");
        obj = Scm_MakeFlonum(1.0/val);
    } else if (SCM_FLONUMP(obj)) {
        double val = SCM_FLONUM_VALUE(obj);
        if (val == 0.0) Scm_Error("divide by zero");
        obj = Scm_MakeFlonum(1.0/val);
    } else if (SCM_COMPLEXP(obj)) {
        double r = SCM_COMPLEX_REAL(obj), r1;
        double i = SCM_COMPLEX_IMAG(obj), i1;
        double d;
        if (r == 0.0 && i == 0.0) Scm_Error("divie by zero");
        d = r*r + i*i;
        r1 = r/d;
        i1 = -i/d;
        obj = Scm_MakeComplex(r1, i1);
    } else {
        Scm_Error("number required: %S", obj);
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
            obj = SCM_MAKE_INT((int)d);
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
        return Scm_MakeComplex((double)SCM_INT_VALUE(obj), 0.0);
    if (SCM_BIGNUMP(obj))
        return Scm_MakeComplex(Scm_BignumToDouble(SCM_BIGNUM(obj)), 0.0);
    if (SCM_FLONUMP(obj))
        return Scm_MakeComplex(SCM_FLONUM_VALUE(obj), 0.0);
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

/*
 * Addition and subtraction
 */

ScmObj Scm_Add(ScmObj arg0, ScmObj arg1, ScmObj args)
{
    ScmObj v = arg0;
    int result_int = 0;
    double result_real, result_imag;

    if (SCM_INTP(v)) {
        result_int = SCM_INT_VALUE(v);
        for (;;) {
            if (SCM_INTP(arg1)) {
                result_int += SCM_INT_VALUE(arg1);
                if (result_int > SCM_SMALL_INT_MAX 
                    || result_int < SCM_SMALL_INT_MIN) {
                    v = Scm_MakeBignumFromSI(result_int);
                    break;
                }
            } else if (SCM_BIGNUMP(arg1)) {
                v = Scm_BignumAdd(SCM_BIGNUM(Scm_MakeBignumFromSI(result_int)),
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
                Scm_Error("number required, but got: %S", arg1);
            }
            if (!SCM_PAIRP(args)) return Scm_MakeInteger(result_int);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
        if (!SCM_PAIRP(args)) return v;
        arg1 = SCM_CAR(args);
        args = SCM_CDR(args);
    }
    if (SCM_BIGNUMP(v)) {
        return Scm_BignumAddN(SCM_BIGNUM(v), Scm_Cons(arg1, args));
    }
    if (SCM_FLONUMP(v)) {
        result_real = SCM_FLONUM_VALUE(v);
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
                Scm_Error("number required, but got: %S", arg1);
            }
            if (!SCM_PAIRP(args)) return Scm_MakeFlonum(result_real);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    if (SCM_COMPLEXP(v)) {
        result_real = SCM_COMPLEX_REAL(v);
        result_imag = SCM_COMPLEX_IMAG(v);
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
                Scm_Error("number required, but got: %S", arg1);
            }
            if (!SCM_PAIRP(args)) {
                if (result_imag == 0.0)
                    return Scm_MakeFlonum(result_real);
                else
                    return Scm_MakeComplex(result_real, result_imag);
            }
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    Scm_Error("number required: %S", v);
    return SCM_UNDEFINED;       /* NOTREACHED */
}

ScmObj Scm_Subtract(ScmObj arg0, ScmObj arg1, ScmObj args)
{
    int result_int = 0;
    double result_real = 0.0, result_imag = 0.0;

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
                Scm_Error("number required, but got %S", arg1);
            }
            if (SCM_NULLP(args))
                return SCM_MAKE_INT(result_int);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    if (SCM_BIGNUMP(arg0)) {
        return Scm_BignumSubN(SCM_BIGNUM(arg0), Scm_Cons(arg1, args));
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
                Scm_Error("number required, but got %S", arg1);
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
                Scm_Error("number required, but got %S", arg1);
            }
            if (SCM_NULLP(args))
                return Scm_MakeComplex(result_real, result_imag);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    Scm_Error("number required: %S", arg0);
    return SCM_UNDEFINED;       /* NOTREACHED */
}

/*
 * Multiplication
 */

ScmObj Scm_Multiply(ScmObj arg0, ScmObj arg1, ScmObj args)
{
    ScmObj v = arg0;
    long result_int;
    double result_real, result_imag;

  retry:
    if (SCM_INTP(v)) {
        result_int = SCM_INT_VALUE(v);
        for (;;) {
            if (SCM_INTP(arg1)) {
                long vv = SCM_INT_VALUE(arg1);
                long k = result_int * vv;
                /* TODO: need a better way to check overflow */
                if ((vv != 0 && k/vv != result_int)
                    || k < SCM_SMALL_INT_MIN
                    || k > SCM_SMALL_INT_MAX) {
                    ScmObj big = Scm_MakeBignumFromSI(result_int);
                    v = Scm_BignumMulSI(SCM_BIGNUM(big), vv);
                    break;
                }
                result_int = k;
            } else if (SCM_BIGNUMP(arg1)) {
                v = Scm_BignumMulSI(SCM_BIGNUM(arg1), result_int);
                break;
            } else if (SCM_FLONUMP(arg1)) {
                result_real = (double)result_int;
                goto DO_FLONUM;
            } else if (SCM_COMPLEXP(arg1)) {
                result_real = (double)result_int;
                result_imag = 0.0;
                goto DO_COMPLEX;
            } else {
                Scm_Error("number required, but got: %S", arg1);
            }
            if (!SCM_PAIRP(args)) return Scm_MakeInteger(result_int);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
        if (!SCM_PAIRP(args)) return v;
        arg1 = SCM_CAR(args);
        args = SCM_CDR(args);
        goto retry;
    }
    if (SCM_BIGNUMP(v)) {
        return Scm_BignumMulN(SCM_BIGNUM(v), Scm_Cons(arg1, args));
    }
    if (SCM_FLONUMP(v)) {
        result_real = SCM_FLONUM_VALUE(v);
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
                Scm_Error("number required, but got: %S", arg1);
            }
            if (!SCM_PAIRP(args)) return Scm_MakeFlonum(result_real);
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    if (SCM_COMPLEXP(v)) {
        result_real = SCM_COMPLEX_REAL(v);
        result_imag = SCM_COMPLEX_IMAG(v);
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
                Scm_Error("number required, but got: %S", arg1);
            }
            if (!SCM_PAIRP(args)) {
                if (result_imag == 0.0)
                    return Scm_MakeFlonum(result_real);
                else
                    return Scm_MakeComplex(result_real, result_imag);
            }
            arg1 = SCM_CAR(args);
            args = SCM_CDR(args);
        }
    }
    Scm_Error("number required: %S", v);
    return SCM_UNDEFINED;       /* NOTREACHED */
}

/*
 * Division
 */

ScmObj Scm_Divide(ScmObj arg0, ScmObj arg1, ScmObj args)
{
    double result_real = 0.0, result_imag = 0.0;
    double div_real = 0.0, div_imag = 0.0;
    int exact = 1;

    if (SCM_INTP(arg0)) {
        result_real = (double)SCM_INT_VALUE(arg0);
        goto DO_FLONUM;
    }
    if (SCM_BIGNUMP(arg0)) {
        /* Try integer division first, and if remainder != 0, shift to
           inexact number */
        if (SCM_INTP(arg1)) {
            long rem;
            ScmObj div = Scm_BignumDivSI(SCM_BIGNUM(arg0),
                                         SCM_INT_VALUE(arg1),
                                         &rem);
            if (rem != 0) {
                result_real = Scm_BignumToDouble(SCM_BIGNUM(arg0));
                exact = 0;
                goto DO_FLONUM;
            }
            if (SCM_NULLP(args)) return div;
            return Scm_Divide(div, SCM_CAR(args), SCM_CDR(args));
        }
        if (SCM_BIGNUMP(arg1)) {
            ScmObj divrem = Scm_BignumDivRem(SCM_BIGNUM(arg0), SCM_BIGNUM(arg1));
            if (SCM_CDR(divrem) != SCM_MAKE_INT(0)) {
                result_real = Scm_BignumToDouble(SCM_BIGNUM(arg0));
                exact = 0;
                goto DO_FLONUM;
            }
            if (SCM_NULLP(args)) return SCM_CAR(divrem);
            return Scm_Divide(divrem, SCM_CAR(args), SCM_CDR(args));
        }
        if (SCM_FLONUMP(arg1)) {
            exact = 0;
            result_real = Scm_BignumToDouble(SCM_BIGNUM(arg0));
            goto DO_FLONUM;
        }
        if (SCM_COMPLEXP(arg1)) {
            exact = 0;
            result_real = Scm_BignumToDouble(SCM_BIGNUM(arg0));
            goto DO_COMPLEX;
        }
        Scm_Error("number required, but got %S", arg1);
    }
    if (SCM_FLONUMP(arg0)) {
        result_real = SCM_FLONUM_VALUE(arg0);
        exact = 0;
      DO_FLONUM:
        for (;;) {
            if (SCM_INTP(arg1)) {
                div_real = (double)SCM_INT_VALUE(arg1);
            } else if (SCM_BIGNUMP(arg1)) {
                div_real = Scm_BignumToDouble(SCM_BIGNUM(arg1));
            } else if (SCM_FLONUMP(arg1)) {
                div_real = SCM_FLONUM_VALUE(arg1);
                exact = 0;
            } else if (SCM_COMPLEXP(arg1)) {
                goto DO_COMPLEX;
            } else {
                Scm_Error("number required, but got %S", arg1);
            }
            if (div_real == 0) 
                Scm_Error("divide by zero");
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
        div_imag = 0.0;
      DO_COMPLEX:
        for (;;) {
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
                Scm_Error("number required, but got %S", arg1);
            }
            d = div_real*div_real + div_imag*div_imag;
            if (d == 0.0)
                Scm_Error("divide by zero");
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
    Scm_Error("number required: %S", arg0);
    return SCM_UNDEFINED;       /* NOTREACHED */
}

/*
 * Integer division
 */
ScmObj Scm_Quotient(ScmObj x, ScmObj y)
{
    double rx, ry, f, i;
    if (SCM_INTP(x)) {
        if (SCM_INTP(y)) {
            int r;
            if (SCM_INT_VALUE(y) == 0) goto DIVBYZERO;
            r = SCM_INT_VALUE(x)/SCM_INT_VALUE(y);
            return SCM_MAKE_INT(r);
        }
        if (SCM_BIGNUMP(y)) {
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
            return Scm_BignumDivSI(SCM_BIGNUM(x), SCM_INT_VALUE(y), NULL);
        } else if (SCM_BIGNUMP(y)) {
            return SCM_CAR(Scm_BignumDivRem(SCM_BIGNUM(x), SCM_BIGNUM(y)));
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
        if (ry == 0.0) goto DIVBYZERO;
        f = modf(rx/ry, &i);
        return Scm_MakeFlonum(i);
    }
  DIVBYZERO:
    Scm_Error("divide by zero");
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
            int r;
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
            int iy = SCM_INT_VALUE(y);
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
    }
  DIVBYZERO:
    Scm_Error("divide by zero");
  BADARGY:
    x = y;
  BADARG:
    Scm_Error("integer required, but got %S", x);
    return SCM_UNDEFINED;       /* dummy */
}

/*
 * Expt
 */

/* short cut for exact numbers */
static ScmObj exact_expt(ScmObj x, ScmObj y)
{
    int sign = Scm_Sign(y);
    ScmObj r = SCM_MAKE_INT(1);

    if (sign == 0) return r;
    if (x == SCM_MAKE_INT(1)) return r;
    if (x == SCM_MAKE_INT(-1)) return Scm_OddP(y)? SCM_MAKE_INT(-1) : r;
    /* TODO: optimization when x is power of two */
    if (SCM_INTP(y)) {
        int iy = SCM_INT_VALUE(y);
        if (iy < 0) iy = -iy;
        for (;;) {
            if (iy == 0) break;
            if (iy == 1) { r = Scm_Multiply(r, x, SCM_NIL); break; }
            if (iy & 0x01) r = Scm_Multiply(r, x, SCM_NIL);
            x = Scm_Multiply(x, x, SCM_NIL);
            iy >>= 1;
        }
    } else {
        /* who wants such a heavy calculation? */
        Scm_Error("exponent too big: %S", y);
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
        if (SCM_INTP(arg1))
            return (SCM_INT_VALUE(arg0) - SCM_INT_VALUE(arg1));
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

ScmObj Scm_Max(ScmObj arg0, ScmObj args)
{
    int inexact = !SCM_EXACTP(arg0);
    for (;;) {
        if (!SCM_REALP(arg0))
            Scm_Error("real number required, but got %S", arg0);
        if (SCM_NULLP(args)) {
            if (inexact && SCM_EXACTP(arg0)) {
                return Scm_ExactToInexact(arg0);
            } else {
                return arg0;
            }
        }
        if (!SCM_EXACTP(SCM_CAR(args))) inexact = TRUE;
        if (Scm_NumCmp(arg0, SCM_CAR(args)) < 0) {
            arg0 = SCM_CAR(args);
        }
        args = SCM_CDR(args);
    }
}

ScmObj Scm_Min(ScmObj arg0, ScmObj args)
{
    int inexact = !SCM_EXACTP(arg0);
    for (;;) {
        if (!SCM_REALP(arg0))
            Scm_Error("real number required, but got %S", arg0);
        if (SCM_NULLP(args)) {
            if (inexact && SCM_EXACTP(arg0)) {
                return Scm_ExactToInexact(arg0);
            } else {
                return arg0;
            }
        }
        if (!SCM_EXACTP(SCM_CAR(args))) inexact = TRUE;
        if (Scm_NumCmp(arg0, SCM_CAR(args)) > 0) {
            arg0 = SCM_CAR(args);
        }
        args = SCM_CDR(args);
    }
}

/*===============================================================
 * ROUNDING
 */

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
    /* trunc and round is neither in ANSI nor in POSIX. */
#ifdef HAVE_TRUNC
    case SCM_ROUND_TRUNC: r = trunc(v); break;
#else
    case SCM_ROUND_TRUNC: r = (v < 0.0)? ceil(v) : floor(v); break;
#endif
#ifdef HAVE_RINT
    case SCM_ROUND_ROUND: r = rint(v); break;
#else
    case SCM_ROUND_ROUND: {
        double frac = modf(v, &r);
        if (v > 0.0) {
            if (frac > 0.5) r += 1.0;
            else if (frac == 0.5) {
                if (r/2.0 != 0.0) r += 1.0;
            }
        } else {
            if (frac < -0.5) r -= 1.0;
            else if (frac == 0.5) {
                if (r/2.0 != 0.0) r -= 1.0;
            }
        }
        break;
    }
#endif
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

/*
 * Printer
 */

static void number_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmObj s = Scm_NumberToString(obj, 10, FALSE);
    SCM_PUTS(SCM_STRING(s), port);
}

#if 0
/* C's printf may not be accurate enough to guarantee read/write identity
 * of floating point numbers.
 */
static double expt10_tab[8] = { 1.0e0, 1.0e1, 1.0e2, 1.0e3, 
                                1.0e4, 1.0e5, 1.0e6, 1.0e7 };

static inline double expt10(u_int k)
{
    double v = expt10_tab[k & 0x07];
    int l = k >> 3;
    while (l-- > 0) v *= 1.0e8;
    return v;
}

/** more to come **/
#endif

static void double_print(char *buf, int buflen, double val, int plus_sign)
{
    /* TODO: Look at the algorithm of Burger & Dybvig :"Priting Floating-Point
       Numbers Quickly and Accurately", PLDI '96, pp108--116. */
    if (SCM_IS_INF(val)) {
        if (plus_sign) *buf++ = '+';
        strcpy(buf, "#<inf>");
    } else if (SCM_IS_NAN(val)) {
        if (plus_sign) *buf++ = '+';
        strcpy(buf, "#<nan>");
    } else {
        if (plus_sign && val > 0) { *buf++ = '+'; buflen--; }
        snprintf(buf, buflen - 3, "%.15g", val);
        if (strchr(buf, '.') == NULL && strchr(buf, 'e') == NULL)
            strcat(buf, ".0");
    }
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
        ScmObj p = Scm_MakeOutputStringPort();
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

/*
 * Number Parser
 */

static ScmObj read_integer(const char *str, int len, int radix)
{
    long value_int = 0;
    ScmObj value_big = SCM_FALSE;
    int minusp = 0;
    char c;
    static const char tab[] = "0123456789abcdefghijklmnopqrstuvwxyz";
    const char *ptab;

    if (*str == '+')      { minusp = 0; str++; len--; }
    else if (*str == '-') { minusp = 1; str++; len--; }
    if (len == 0) return SCM_FALSE;
    
    while (len--) {
        c = tolower(*str++);
        for (ptab = tab; ptab < tab+radix; ptab++) {
            if (c == *ptab) {
                if (SCM_FALSEP(value_big)) {
                    if (value_int < (LONG_MAX/radix - radix)) {
                        value_int = value_int * radix + (ptab-tab);
                        break;
                    } else {
                        /* will overflow */
                        value_big = Scm_MakeBignumFromSI(value_int);
                    }
                }
                value_big = Scm_BignumMulSI(SCM_BIGNUM(value_big), radix);
                value_big = Scm_BignumAddSI(SCM_BIGNUM(value_big), ptab-tab);
                break;
            }
        }
        if (ptab >= tab+radix) return SCM_FALSE;
    }
    if (SCM_FALSEP(value_big)) {
        if (minusp) return Scm_MakeInteger(-value_int);
        else        return Scm_MakeInteger(value_int);
    } else {
        if (minusp) return Scm_BignumNegate(SCM_BIGNUM(value_big));
        else        return value_big;
    }
}

static double read_real(const char *str, int len, const char **next)
{
    char c;
    ScmDString ds;
    double value;
    int point_seen = 0, exp_seen = 0, digits = 0;
    
    Scm_DStringInit(&ds);

    if (*str == '+' || *str == '-') {
        SCM_DSTRING_PUTB(&ds, *str);
        str++;
        len--;
    }
    if (len == 0) { *next = NULL; return 0.0; }

    for (; len > 0; len--, str++) {
        switch (c = *str) {
        case '0':; case '1':; case '2':; case '3':; case '4':;
        case '5':; case '6':; case '7':; case '8':; case '9':
            digits++;
            SCM_DSTRING_PUTB(&ds, c);
            continue;
        case '.':
            if (point_seen) { *next = NULL; return 0.0; }
            SCM_DSTRING_PUTB(&ds, c);
            point_seen = 1;
            continue;
        case 'e':; case 'E':;
        case 's':; case 'S':; case 'f':; case 'F':;
        case 'd':; case 'D':; case 'l':; case 'L':;
            if (digits == 0 || exp_seen) { *next = NULL; return 0.0; }
            point_seen = exp_seen = 1;
            SCM_DSTRING_PUTB(&ds, 'e');
            if (len > 0 && (str[1] == '+' || str[1] == '-')) {
                SCM_DSTRING_PUTB(&ds, *++str);
                len--;
            }
            continue;
        case '+':; case '-':; case 'i':
            break;
        default:
            *next = NULL;
            return 0.0;
        }
        break;
    }
    if (digits == 0) SCM_DSTRING_PUTB(&ds, '1');
    sscanf(Scm_DStringGetz(&ds), "%lf", &value);
    *next = str;
    return value;
}

static ScmObj read_complex(const char *str, int len)
{
    double real, imag;
    const char *next;
    char sign = 0;

    if (*str == '.' && len == 1) return SCM_FALSE;
    if (*str == '+' || *str == '-') sign = *str;

    real = read_real(str, len, &next);
    if (next == NULL) return SCM_FALSE;

    if (next == str+len) return Scm_MakeFlonum(real);

    if (*next == 'i') {
        if (sign && next == str+len-1) {
            if (real == 0.0) return Scm_MakeFlonum(0.0);
            else return Scm_MakeComplex(0, real);
        }
        else return SCM_FALSE;
    }

    if (*next == '+' || *next == '-') {
        imag = read_real(next, len-(next-str), &next);
        if (next == NULL || next != str+len-1) return SCM_FALSE;
        if (*next != 'i') return SCM_FALSE;
        if (imag == 0.0)
            return Scm_MakeFlonum(real);
        else
            return Scm_MakeComplex(real, imag);
    }
    return SCM_FALSE;
}

static ScmObj read_number(const char *str, int len, int radix)
{
    int radix_seen = 0;
    int exactness = 0, exactness_seen = 0;
    int i;

    if (radix <= 1 || radix > 36) return SCM_FALSE; /* XXX:should be error? */
    
    /* start from prefix part */
    for (; len >= 0; len-=2) {
        if (*str != '#') break;
        str++;
        switch (*str++) {
        case 'x':; case 'X':;
            if (radix_seen) return SCM_FALSE;
            radix = 16; radix_seen++;
            continue;
        case 'o':; case 'O':;
            if (radix_seen) return SCM_FALSE;
            radix = 8; radix_seen++;
            continue;
        case 'b':; case 'B':;
            if (radix_seen) return SCM_FALSE;
            radix = 2; radix_seen++;
            continue;
        case 'd':; case 'D':;
            if (radix_seen) return SCM_FALSE;
            radix = 10; radix_seen++;
            continue;
        case 'e':; case 'E':;
            if (exactness_seen) return SCM_FALSE;
            exactness = 1; exactness_seen++;
            continue;
        case 'i':; case 'I':;
            if (exactness_seen) return SCM_FALSE;
            exactness = 0; exactness_seen++;
            continue;
        }
        return SCM_FALSE;
    }
    if (len <= 0) return SCM_FALSE;

    /* number body */
    if (exactness || radix != 10)  return read_integer(str, len, radix);

    i = (*str == '+' || *str == '-')? 1 : 0;
    for (; i<len; i++) {
        if (!isdigit(str[i])) return read_complex(str, len);
    }
    return read_integer(str, len, 10);
}


ScmObj Scm_StringToNumber(ScmString *str, int radix)
{
    if (SCM_STRING_LENGTH(str) != SCM_STRING_SIZE(str)) {
        /* This can't be a proper number. */
        return SCM_FALSE;
    } else {
        return read_number(SCM_STRING_START(str), SCM_STRING_SIZE(str), radix);
    }
}
