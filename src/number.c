/*
 * number.c - numeric functions
 *
 *  Copyright(C) 2000 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, ditribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: number.c,v 1.1.1.1 2001-01-11 19:26:03 shiro Exp $
 */

#include <math.h>
#include <limits.h>
#include "gauche.h"

/*
 * Classes of Numeric Tower
 */

static ScmClass *top_cpl[] = {
    SCM_CLASS_TOP, NULL
};

static ScmClass *number_cpl[] = {
    SCM_CLASS_NUMBER, SCM_CLASS_TOP, NULL
};

static ScmClass *complex_cpl[] = {
    SCM_CLASS_COMPLEX, SCM_CLASS_NUMBER, SCM_CLASS_TOP, NULL
};

static ScmClass *real_cpl[] = {
    SCM_CLASS_REAL, SCM_CLASS_COMPLEX, SCM_CLASS_NUMBER, SCM_CLASS_TOP,
    NULL
};

static int number_print(ScmObj obj, ScmPort *port, int mode);

ScmClass Scm_NumberClass = {
    SCM_CLASS_CLASS,
    "<number>",
    number_print,
    top_cpl
};

ScmClass Scm_ComplexClass = {
    SCM_CLASS_CLASS,
    "<complex>",
    number_print,
    number_cpl
};

ScmClass Scm_RealClass = {
    SCM_CLASS_CLASS,
    "<real>",
    number_print,
    complex_cpl
};

ScmClass Scm_IntegerClass = {
    SCM_CLASS_CLASS,
    "<integer>",
    number_print,
    real_cpl
};

/*=====================================================================
 *  Flonums
 */

ScmObj Scm_MakeFlonum(double d)
{
    ScmFlonum *f = SCM_NEW(ScmFlonum);
    f->hdr.klass = SCM_CLASS_REAL;
    f->value = d;
    return SCM_OBJ(f);
}

/*======================================================================
 *  Bignums
 */

#if 0
#define SCM_ALLOC_BIGNUM(size) \
    SCM_NEW_ATOMIC2(ScmBignum*, sizeof(ScmBignum) + (size-1)*sizeof(long));

ScmObj Scm_MakeBignum(int sign, int size, long *values)
{
    int i;
    ScmBignum *b = SCM_ALLOC_BIGNUM(size);
    b->sign = sign;
    b->size = size;
    for (i=0; i<size; i++) {
        b->values[i] = values[i];
    }
    return SCM_OBJ(b);
}
#endif

/*=======================================================================
 *  Complex numbers
 */

ScmObj Scm_MakeComplex(double r, double i)
{
    ScmComplex *c = SCM_NEW_ATOMIC(ScmComplex);
    c->hdr.klass = SCM_CLASS_COMPLEX;
    c->real = r;
    c->imag = i;
    return SCM_OBJ(c);
}

/*=======================================================================
 *  Coertion
 */

ScmObj Scm_MakeInteger(long i)
{
    /* TODO: check boundary */
    return SCM_MAKE_INT(i);
}

/* Convert scheme integer to C integer. Overflow is neglected. */
long Scm_GetInteger(ScmObj obj)
{
    if (SCM_INTP(obj)) return SCM_INT_VALUE(obj);
    else if (SCM_FLONUMP(obj)) return (long)SCM_FLONUM_VALUE(obj);
    else return 0;
}

double Scm_GetDouble(ScmObj obj)
{
    if (SCM_FLONUMP(obj)) return SCM_FLONUM_VALUE(obj);
    else if (SCM_INTP(obj)) return (double)SCM_INT_VALUE(obj);
    else return 0.0;
}

/*
 *   Generic Methods
 */

/* Predicates */

ScmObj Scm_NumberP(ScmObj obj)
{
    return SCM_NUMBERP(obj)? SCM_TRUE : SCM_FALSE;
}

/* Unary Operator */

ScmObj Scm_Abs(ScmObj obj)
{
    if (SCM_INTP(obj)) {
        int v = SCM_INT_VALUE(obj);
        if (v < 0) obj = SCM_MAKE_INT(-v);
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
    } else if (SCM_FLONUMP(obj)) {
        double v = SCM_FLONUM_VALUE(obj);
        if (v != 0.0) {
            r = (v > 0.0)? 1 : -1;
        }
    } else {
        Scm_Error("real number required: %S", obj);
    }
    return r;
}

ScmObj Scm_Uminus(ScmObj obj)
{
    if (SCM_INTP(obj)) {
        /* TOOD: overflow check */
        int v = SCM_INT_VALUE(obj);
        obj = SCM_MAKE_INT(-v);
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

/*
 * Conversion operators
 */

ScmObj Scm_ExactToInexact(ScmObj obj)
{
    if (SCM_INTP(obj)) {
        obj = Scm_MakeFlonum((double)SCM_INT_VALUE(obj));
    } else if (!SCM_FLONUMP(obj) || !SCM_COMPLEXP(obj)) {
        Scm_Error("number required: %S", obj);
    }
    return obj;
}

ScmObj Scm_InexactToExact(ScmObj obj)
{
    if (SCM_FLONUMP(obj)) {
        double d = SCM_FLONUM_VALUE(obj);
        if (d < SCM_SMALL_INT_MIN || d >= SCM_SMALL_INT_MAX) {
            Scm_Error("argument out of range: %S", obj);
        }
        obj = SCM_MAKE_INT((int)d);
    } else if (SCM_COMPLEXP(obj)) {
        Scm_Error("exact complex is not supported: %S", obj);
    } if (!SCM_INTP(obj)) {
        Scm_Error("number required: %S", obj);
    }
    return obj;
}

enum NumberClass {
    FIXNUM,
    BIGNUM,
    FLONUM,
    COMPLEX,
    NONUMBER
};

#define NUMBER_CLASS(obj)                       \
    (SCM_INTP(obj)? FIXNUM :                    \
       SCM_BIGNUMP(obj)? BIGNUM :               \
         SCM_FLONUMP(obj)? FLONUM :             \
           SCM_COMPLEXP(obj)? COMPLEX: NONUMBER)

/* Type conversion:
 *   `promote' means a conversion from lower number class to higher,
 *      e.g. fixnum -> bignum -> flonum -> complex.
 *   `demote' means a conversion from higher number class to lower,
 *      e.g. complex -> flonum -> bignum -> fixnum.
 */

ScmObj Scm_PromoteToFlonum(ScmObj obj)
{
    if (SCM_INTP(obj)) return Scm_MakeFlonum(SCM_INT_VALUE(obj));
    if (SCM_FLONUMP(obj)) return obj;
    Scm_Panic("Scm_PromoteToFlonum: can't be here");
    return SCM_UNDEFINED;       /* dummy */
}

ScmObj Scm_PromoteToComplex(ScmObj obj)
{
    if (SCM_INTP(obj))
        return Scm_MakeComplex((double)SCM_INT_VALUE(obj), 0.0);
    if (SCM_FLONUMP(obj))
        return Scm_MakeComplex(SCM_FLONUM_VALUE(obj), 0.0);
    Scm_Panic("Scm_PromoteToComplex: can't be here");
    return SCM_UNDEFINED;       /* dummy */
}

/*===============================================================
 * Arithmetics
 */

/*
 * Addition and subtraction
 */

ScmObj Scm_Sum(ScmObj args)
{
    ScmObj cp, v;
    int result_int = 0;
    double result_double, result_imag;
    int nclass = FIXNUM;

    if (!SCM_PAIRP(args)) return SCM_MAKE_INT(0);

    v = SCM_CAR(args);
    args = SCM_CDR(args);
    nclass = NUMBER_CLASS(v);
    if (nclass == NONUMBER) Scm_Error("number required: %S", v);
    if (!SCM_PAIRP(args)) return v;

    while (nclass == FIXNUM) {
        /* TODO: check overflow */
        result_int += SCM_INT_VALUE(v);
        if (!SCM_PAIRP(args)) return Scm_MakeInteger(result_int);
        v = SCM_CAR(args);
        args = SCM_CDR(args);
        nclass = NUMBER_CLASS(v);
    }

    if (nclass == NONUMBER) Scm_Error("number required: %S", v);

    result_double = (double)result_int;
    while (nclass == FLONUM) {
        result_double += SCM_FLONUM_VALUE(v);
        if (!SCM_PAIRP(args)) return Scm_MakeFlonum(result_double);
        v = SCM_CAR(args);
        args = SCM_CDR(args);
        nclass = NUMBER_CLASS(v);

        if (nclass < FLONUM) {
            v = Scm_PromoteToFlonum(v);
            nclass = FLONUM;
        }
    }

    result_imag = 0.0;
    while (nclass == COMPLEX) {
        result_double += SCM_COMPLEX_REAL(v);
        result_imag += SCM_COMPLEX_IMAG(v);
        if (!SCM_PAIRP(args)) {
            if (result_imag == 0.0)
                return Scm_MakeFlonum(result_double);
            else
                return Scm_MakeComplex(result_double, result_imag);
        }
        v = SCM_CAR(args);
        args = SCM_CDR(args);
        nclass = NUMBER_CLASS(v);

        if (nclass < COMPLEX) {
            v = Scm_PromoteToComplex(v);
            nclass = COMPLEX;
        }
    }
    
    Scm_Error("number required: %S", v);
    return SCM_UNDEFINED;       /* NOTREACHED */
}

ScmObj Scm_Difference(ScmObj arg1, ScmObj args)
{
    ScmObj cp, v;
    int result_int = 0;
    double result_double = 0.0, result_imag = 0.0;
    int nclass, vnclass;

    if (!SCM_PAIRP(args)) return Scm_Uminus(arg1);
    
    if (SCM_INTP(arg1)) {
        result_int = SCM_INT_VALUE(arg1);
        nclass = FIXNUM;
    } else if (SCM_FLONUMP(arg1)) {
        result_double = SCM_FLONUM_VALUE(arg1);
        nclass = FLONUM;
    } else if (SCM_COMPLEXP(arg1)) {
        result_double = SCM_COMPLEX_REAL(arg1);
        result_imag = SCM_COMPLEX_IMAG(arg1);
        nclass = COMPLEX;
    } else {
        Scm_Error("number required: %S", arg1);
    }

    v = SCM_CAR(args);
    args = SCM_CDR(args);
    vnclass = NUMBER_CLASS(v);
    if (nclass == NONUMBER) Scm_Error("number required: %S", v);
    if (vnclass > nclass) nclass = vnclass;

    while (nclass == FIXNUM) {
        /* TODO: check overflow */
        result_int -= SCM_INT_VALUE(v);
        
        if (!SCM_PAIRP(args)) return Scm_MakeInteger(result_int);
        v = SCM_CAR(args);
        args = SCM_CDR(args);
        nclass = NUMBER_CLASS(v);
    }

    if (nclass == NONUMBER) Scm_Error("number required: %S", v);

    result_double += (double)result_int;

    while (nclass == FLONUM) {
        result_double -= SCM_FLONUM_VALUE(v);
        if (!SCM_PAIRP(args)) return Scm_MakeFlonum(result_double);
        v = SCM_CAR(args);
        args = SCM_CDR(args);
        nclass = NUMBER_CLASS(v);
        if (nclass < FLONUM) {
            v = Scm_PromoteToFlonum(v);
            nclass = FLONUM;
        }
    }

    while (nclass == COMPLEX) {
        result_double -= SCM_COMPLEX_REAL(v);
        result_imag -= SCM_COMPLEX_IMAG(v);
        if (!SCM_PAIRP(args)) {
            if (result_imag == 0.0)
                return Scm_MakeFlonum(result_double);
            else
                return Scm_MakeComplex(result_double, result_imag);
        }
        v = SCM_CAR(args);
        args = SCM_CDR(args);
        nclass = NUMBER_CLASS(v);

        if (nclass < COMPLEX) {
            v = Scm_PromoteToComplex(v);
            nclass = COMPLEX;
        }
    }
    
    Scm_Error("number required: %S", v);
    return SCM_UNDEFINED;       /* NOTREACHED */
}

/*
 * Multiplication
 */

ScmObj Scm_Product(ScmObj args)
{
    ScmObj cp, v;
    int result_int = 1;
    double result_double, result_imag;
    int nclass = FIXNUM;

    if (!SCM_PAIRP(args)) return SCM_MAKE_INT(1);

    v = SCM_CAR(args);
    args = SCM_CDR(args);
    nclass = NUMBER_CLASS(v);
    if (nclass == NONUMBER) Scm_Error("number required: %S", v);
    if (!SCM_PAIRP(args)) return v;

    while (nclass == FIXNUM) {
        /* TODO: check overflow */
        result_int *= SCM_INT_VALUE(v);
        if (!SCM_PAIRP(args)) return Scm_MakeInteger(result_int);
        v = SCM_CAR(args);
        args = SCM_CDR(args);
        nclass = NUMBER_CLASS(v);
    }

    if (nclass == NONUMBER) Scm_Error("number required: %S", v);

    result_double = (double)result_int;
    while (nclass == FLONUM) {
        result_double *= SCM_FLONUM_VALUE(v);
        if (!SCM_PAIRP(args)) return Scm_MakeFlonum(result_double);
        v = SCM_CAR(args);
        args = SCM_CDR(args);
        nclass = NUMBER_CLASS(v);

        if (nclass < FLONUM) {
            v = Scm_PromoteToFlonum(v);
            nclass = FLONUM;
        }
    }

    result_imag = 0.0;
    while (nclass == COMPLEX) {
        double r = SCM_COMPLEX_REAL(v);
        double i = SCM_COMPLEX_IMAG(v);

        double t = result_double * r - result_imag * i;
        result_imag   = result_double * i + result_imag * r;
        result_double = t;
        
        if (!SCM_PAIRP(args)) {
            if (result_imag == 0.0)
                return Scm_MakeFlonum(result_double);
            else
                return Scm_MakeComplex(result_double, result_imag);
        }
        v = SCM_CAR(args);
        args = SCM_CDR(args);
        nclass = NUMBER_CLASS(v);

        if (nclass < COMPLEX) {
            v = Scm_PromoteToComplex(v);
            nclass = COMPLEX;
        }
    }
    
    Scm_Error("number required: %S", v);
    return SCM_UNDEFINED;       /* NOTREACHED */
}

/*===============================================================
 * Comparison
 */

/* We support more than two args, but optmize for two-arg case.
 * Expecially, the third and after args are not checked until 
 * they are actually used, so (= 3 2 #f) doesn't report an error,
 * for example.
 */
ScmObj Scm_NumEq(ScmObj arg0, ScmObj arg1, ScmObj args)
{
    int nc0 = NUMBER_CLASS(arg0);
    int nc1 = NUMBER_CLASS(arg1);

    /* deal with the most common case first. */
    if (nc0 == nc1) {
        if (nc0 == FIXNUM) {
            if (arg0 != arg1)   /* we can use EQ here */
                return SCM_FALSE;
            if (SCM_PAIRP(args))
                return Scm_NumEq(arg1, SCM_CAR(args), SCM_CDR(args));
            else
                return SCM_TRUE;
        }
        if (nc0 == FLONUM) {
            if (SCM_FLONUM_VALUE(arg0) != SCM_FLONUM_VALUE(arg1))
                return SCM_FALSE;
            if (SCM_PAIRP(args))
                return Scm_NumEq(arg1, SCM_CAR(args), SCM_CDR(args));
            else
                return SCM_TRUE;
        }
        if (nc0 == COMPLEX) {
            if (SCM_COMPLEX_REAL(arg0) != SCM_COMPLEX_REAL(arg1)
                || SCM_COMPLEX_IMAG(arg0) != SCM_COMPLEX_IMAG(arg1))
                return SCM_FALSE;
            if (SCM_PAIRP(args))
                return Scm_NumEq(arg1, SCM_CAR(args), SCM_CDR(args));
            else
                return SCM_TRUE;
        }
        Scm_Error("number required: %S", arg0);
    }

    /* Need type coertion.  I assume this is less common case,
       so forgetaboutspeed. */
    if (nc0 < nc1) {            /* let arg0 be higher class */
        int tmpi; ScmObj tmps;
        tmpi = nc0;  nc0 = nc1;   nc1 = tmpi;
        tmps = arg0; arg0 = arg1; arg1 = tmps;
    }
    if (nc0 == FLONUM) {
        return Scm_NumEq(arg0, Scm_PromoteToFlonum(arg1), args);
    }
    if (nc0 == COMPLEX) {
        return Scm_NumEq(arg0, Scm_PromoteToComplex(arg1), args);
    }
    Scm_Error("number required: %S", arg0);
    return SCM_UNDEFINED;       /* NOTREACHED */
}

#define NUMCMP(FN, OP)                                                  \
ScmObj FN(ScmObj arg0, ScmObj arg1, ScmObj args)                        \
{                                                                       \
    int nc0 = NUMBER_CLASS(arg0);                                       \
    int nc1 = NUMBER_CLASS(arg1);                                       \
                                                                        \
    if (nc0 == nc1) {                                                   \
        if (nc0 == FIXNUM) {                                            \
            if (SCM_INT_VALUE(arg0) OP SCM_INT_VALUE(arg1))             \
                return SCM_FALSE;                                       \
            if (SCM_PAIRP(args))                                        \
                return FN(arg1, SCM_CAR(args), SCM_CDR(args));          \
            else return SCM_TRUE;                                       \
        }                                                               \
        if (nc0 == FLONUM) {                                            \
            if (SCM_FLONUM_VALUE(arg0) OP SCM_FLONUM_VALUE(arg1))       \
                return SCM_FALSE;                                       \
            if (SCM_PAIRP(args))                                        \
                return FN(arg1, SCM_CAR(args), SCM_CDR(args));          \
            else return SCM_TRUE;                                       \
        }                                                               \
        Scm_Error("real number required: %S", arg0);                    \
    }                                                                   \
                                                                        \
    if (nc0 < nc1) {                                                    \
        if (nc1 == FLONUM)                                              \
            return FN(Scm_PromoteToFlonum(arg0), arg1, args);           \
        else                                                            \
            Scm_Error("real number required: %S", arg1);                \
    } else {                                                            \
        if (nc0 == FLONUM)                                              \
            return FN(arg0, Scm_PromoteToFlonum(arg1), args);           \
        else                                                            \
            Scm_Error("real number required: %S", arg0);                \
    }                                                                   \
    /*NOTREACHED*/                                                      \
    return SCM_UNDEFINED;                                               \
}

NUMCMP(Scm_NumLt, >=)
NUMCMP(Scm_NumLe, >)
NUMCMP(Scm_NumGt, <=)
NUMCMP(Scm_NumGe, <)

/*===============================================================
 * Number I/O
 */

/*
 * Printer
 */

static int number_print(ScmObj obj, ScmPort *port, int mode)
{
    ScmObj s = Scm_NumberToString(obj);
    SCM_PUTS(SCM_STRING(s), port);
    return SCM_STRING_LENGTH(s);
}

/* TODO: add radix! */
ScmObj Scm_NumberToString(ScmObj obj)
{
    ScmObj r;
    
    if (SCM_INTP(obj)) {
        char buf[50];
        snprintf(buf, 50, "%d", SCM_INT_VALUE(obj));
        r = Scm_MakeString(buf, -1, -1);
    } else if (SCM_FLONUMP(obj)) {
        char buf[50];
        snprintf(buf, 50, "%#lg", SCM_FLONUM_VALUE(obj));
        r = Scm_MakeString(buf, -1, -1);
    } else if (SCM_COMPLEXP(obj)) {
        ScmObj p = Scm_MakeOutputStringPort();
        double real = SCM_COMPLEX_REAL(obj), imag = SCM_COMPLEX_IMAG(obj);
        Scm_Printf(SCM_PORT(p), "%lg%+lgi", real, imag);
        r = Scm_GetOutputString(SCM_PORT(p));
    } else {
        Scm_Error("number required: %S", obj);
    }
    return r;
}

/*
 * Number Parser
 */

static ScmObj read_real(const char *str, int len)
{
    char c;
    long value_int = 0, minusp = 0, exponent = 0, exponent_minusp = 0;
    double value_double;
    int imag_minusp, digits = 0;
    double value_imag;

    if (*str == '+')      { minusp = 0; str++; len--; }
    else if (*str == '-') { minusp = 1; str++; len--; }

    if (len == 0) return SCM_FALSE;

    /* fixnum loop */
    while (len--) {
        switch (c = *str++) {
        case '0':; case '1':; case '2':; case '3':; case '4':;
        case '5':; case '6':; case '7':; case '8':; case '9':;
            /* TODO: bignum? */
            value_int = value_int * 10 + (c - '0');
            digits++;
            continue;
        case '.':;
        case 'e':; case 'E':;
        case 's':; case 'S':; case 'f':; case 'F':;
        case 'd':; case 'D':; case 'l':; case 'L':;
        case '+':; case '-':; case 'i':;
            break;
        default:
            return SCM_FALSE;
        }
        break;
    }
    if (len < 0) return Scm_MakeInteger(minusp? -value_int : value_int);

    /* now we know the numebr is at least a flonum. */
    value_double = (double)value_int;

    if (c == '.') {
        /* fraction part */
        double divider = 10.0;
        while (len--) {
            switch (c = *str++) {
            case '0':; case '1':; case '2':; case '3':; case '4':;
            case '5':; case '6':; case '7':; case '8':; case '9':;
                value_double += ((double)(c - '0'))/divider;
                divider *= 10;
                continue;
            case 'e':; case 'E':;
            case 's':; case 'S':; case 'f':; case 'F':;
            case 'd':; case 'D':; case 'l':; case 'L':;
            case '+':; case '-':; case 'i':;
                break;
            default:
                return SCM_FALSE;
            }
            break;
        }
    }
    if (minusp) value_double = -value_double;
    if (len < 0) return Scm_MakeFlonum(value_double);

    if (c != '+' && c != '-' && c != 'i') {
        /* exponent part */
        if (*str == '+')      { exponent_minusp = 0; str++; len--; }
        else if (*str == '-') { exponent_minusp = 1; str++; len--; }
        if (len == 0) return SCM_FALSE;

        while (len--) {
            switch (c = *str++) {
            case '0':; case '1':; case '2':; case '3':; case '4':;
            case '5':; case '6':; case '7':; case '8':; case '9':;
                exponent = exponent * 10 + (c - '0');
                continue;
            case '+':; case '-':
                break;
            default:
                return SCM_FALSE;
            }
            break;
        }
    }
    if (exponent) {
        if (exponent_minusp) exponent = -exponent;
        value_double *= pow(10.0, (double)exponent);
    }
    if (len < 0) return Scm_MakeFlonum(value_double);

    if (c == 'i') {
        if (digits == 0 && value_double == 0.0)
            value_double = 1.0; /* +i or -i */
        if (len == 0)
            return Scm_MakeComplex(0.0, minusp? -value_double : value_double);
        else 
            return SCM_FALSE;
    }
    
    /* now, we got a complex number. */
    if (c == '-') imag_minusp = 1;
    else imag_minusp = 0;
    digits = 0;
    value_imag = 0.0;

    while (len--) {
        switch (c = *str++) {
        case '0':; case '1':; case '2':; case '3':; case '4':;
        case '5':; case '6':; case '7':; case '8':; case '9':;
            value_imag = value_imag * 10.0 + (c - '0');
            digits++;
            continue;
        case '.':;
        case 'e':; case 'E':;
        case 's':; case 'S':; case 'f':; case 'F':;
        case 'd':; case 'D':; case 'l':; case 'L':;
        case 'i':
            break;
        default:
            return SCM_FALSE;
        }
        break;
    }
    if (len < 0) return SCM_FALSE;
    
    if (c == '.') {
        /* fraction part */
        double divider = 10.0;
        while (len--) {
            switch (c = *str++) {
            case '0':; case '1':; case '2':; case '3':; case '4':;
            case '5':; case '6':; case '7':; case '8':; case '9':;
                value_imag += ((double)(c - '0'))/divider;
                divider *= 10;
                continue;
            case 'e':; case 'E':;
            case 's':; case 'S':; case 'f':; case 'F':;
            case 'd':; case 'D':; case 'l':; case 'L':;
            case 'i':;
                break;
            default:
                return SCM_FALSE;
            }
            break;
        }
    }
    if (digits == 0 && value_imag == 0.0) value_imag = 1.0;
    if (imag_minusp) value_imag = -value_imag;
    if (len < 0) return Scm_MakeComplex(value_double, value_imag);

    if (c != 'i') {
        /* exponent part */
        exponent_minusp = 0;
        exponent = 0;

        if (*str == '+')      { exponent_minusp = 0; str++; len--; }
        else if (*str == '-') { exponent_minusp = 1; str++; len--; }
        if (len == 0) return SCM_FALSE;

        while (len--) {
            switch (c = *str++) {
            case '0':; case '1':; case '2':; case '3':; case '4':;
            case '5':; case '6':; case '7':; case '8':; case '9':;
                exponent = exponent * 10 + (c - '0');
                continue;
            case 'i':;
                break;
            default:
                return SCM_FALSE;
            }
            break;
        }
        if (exponent) {
            if (exponent_minusp) exponent = -exponent;
            value_imag *= pow(10.0, (double)exponent);
        }
    }
    
    if (len < 0) return SCM_FALSE;
    else if (value_imag == 0.0)
        return Scm_MakeFlonum(value_double);
    else
        return Scm_MakeComplex(value_double, value_imag);
}

/*
 * Called for #b, #o, #x, or exact numbers
 */
static ScmObj read_integer(const char *str, int len, int radix)
{
    long value_int = 0; /* TODO: bignum? */
    int minusp = 0;
    char c;

    if (*str == '+')      { minusp = 0; str++; len--; }
    else if (*str == '-') { minusp = 1; str++; len--; }
    if (len == 0) return SCM_FALSE;
    
    while (len--) {
        switch (c = *str++) {
        case '0':; case '1':;
            value_int = value_int * radix + (c - '0');
            continue;
        case '2':; case '3':; case '4':; case '5':; case '6':; case '7':;
            if (radix < 8) return SCM_FALSE;
            value_int = value_int * radix + (c - '0');
            continue;
        case '8':; case '9':;
            if (radix < 10) return SCM_FALSE;
            value_int = value_int * radix + (c - '0');
            continue;
        case 'a':; case 'b':; case 'c':; case 'd':; case 'e':; case 'f':;
            if (radix < 16) return SCM_FALSE;
            value_int = value_int * radix + (c - 'a') + 10;
            continue;
        case 'A':; case 'B':; case 'C':; case 'D':; case 'E':; case 'F':;
            if (radix < 16) return SCM_FALSE;
            value_int = value_int * radix + (c - 'A') + 10;
            continue;
        default:
            return SCM_FALSE;
        }
    }
    if (minusp) value_int = -value_int;
    return Scm_MakeInteger(value_int);
}

static ScmObj read_number(const char *str, int len)
{
    int radix = 10, radix_seen = 0;
    int exactness = 0, exactness_seen = 0;

    if (len == 0) return SCM_FALSE;
    
    /* start from prefix part */
  prefix:
    {
        if (*str != '#') goto body;

        while (len--) {
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
            default:
                str--;
                goto body;
            }
        }
        return SCM_FALSE;
    }

    /* number body */
  body:
    if (exactness || radix != 10) {
        return read_integer(str, len, radix);
    } else {
        return read_real(str, len);
    }
}


ScmObj Scm_StringToNumber(ScmString *str)
{
    if (SCM_STRING_LENGTH(str) != SCM_STRING_SIZE(str)) {
        /* This can't be a proper number. */
        return SCM_FALSE;
    } else {
        return read_number(SCM_STRING_START(str), SCM_STRING_SIZE(str));
    }
}
