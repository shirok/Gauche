/*
 *	Number system
 *
 *	%Z%%M% %I% %E% (S. Kawai)
 */

/*Piccolo number objects

   Piccolo supports two number models. LIGHTNUMBER model only supports
   small integer, long integer and real; FULLNUMBER model supports
   arbitrary length integer, exact rational and complex numbers.

   SMALL INTEGER:  29-bit signed int.  Fits in immediate word.
                   Number class is 0.

   BIGINT:	   In FULLNUMBER model, epresents infinite length integer.
                   If value fits in 32 bits, it stored in quad.
                   Otherwise, it stored in cell and malloc-ed array.
		   In LIGHTNUMBER model, this object represents 32bit
		   signed integer and fits in single cell.
		   Number class is 1.

   RATIONAL:	   Rational.  Fits in a quad.
   		   Numeerator and denominator are integer.
		   Sign(denominator) = 1.
		   Only supported in FULLNUMBER model.
		   Number class is 2.

   REAL:	   Double precision flating point number. Fits in a quad.
		   Number class is 3.

   COMPLEX:	   Complex.  Fits in a quad.
                   Only supported in FULLNUMBER model.
		   Real part and imaginary part are not complex.
		   Number class is 4.

  Libpiccolo is usually configured by LIGHTNUMBER model.
 */

extern ScmObj Scm_MakeInteger(long value);

#if 0

enum l_number_classes { 
    L_SMALLINT_CLASS,
    L_BIGINT_CLASS,
    L_RATIONAL_CLASS,
    L_REAL_CLASS,
    L_COMPLEX_CLASS
};

/* Return number class.
   If argument is not a number, return -1. */
#define L_NUMBER_CLASS(obj) \
    (L_INT_P(obj)? 0 : \
     ((L_PTR_P(obj) && L_CELL_ETAG(obj) <= L_COMPLEX_TAG)? \
      L_ETAG_CARDINAL(L_CELL_ETAG(obj)) + 1 : -1))

/* Integers */

#define L_SMALL_INT_BITS  28
#define	L_SMALL_INT_MAX	(268435455L)  /* 2^28 - 1 */
#define	L_SMALL_INT_MIN	(-268435456L) /* -2^28 */

#define	L_XLONG(obj)	((SIGNED long int)L_CELL_CDR(obj))

typedef unsigned short int Ldig_t;

typedef struct {
    LispObj tag;
    Ldig_t *ptr;
    Ldig_t dig[4];
} LispBigint1;

#define	L_BIGINT_P(obj)	      (L_CELL_ETAG(obj) == L_BIGINT_TAG)
#define	L_BIGINT_VALUE(obj)   ((Ldig_t *)L_CELL_CDR(obj))
#define	L_BIGINT_LENGTH(obj)  (L_CELL_LENGTH(obj) / 2)
#define	L_BIGINT_SIGN(obj)    (L_CELL_LENGTH(obj) % 2)

#define	L_BIGINT_DIGIT	65536
#define	L_BIGINT_MASK	(L_BIGINT_DIGIT - 1)
#define L_BIGINT_BITS	16

extern LispObj l_make_bigint(/* Ldig_t*, int, int, int */);

extern LispObj l_make_integer(/* long */);
extern long l_get_integer(/* LispObj */);

/* Real */

typedef struct { 
    LispObj tag;
    long int exactness;
    double value;
} LispReal;

#define	LISPREALPTR(obj)	((LispReal *)L_XPTR(obj))
#define	L_REAL_P(obj)		(L_CELL_ETAG(obj) == L_REAL_TAG)
#define	L_REAL_VALUE(obj)	LISPREALPTR(obj)->value

#ifndef	L_REAL_EXPONENT_MAX
#define	L_REAL_EXPONENT_MAX	307
#define	L_REAL_EXPONENT_MIN	-307
#endif

extern LispObj l_make_real(/* double */);


/* Rational */

#define	L_RATIONAL_P(obj)	(L_CELL_ETAG(obj) == L_RATIONAL_TAG)
#define	L_RATIONAL_NUME(obj)	L_QUAD_SLOT1(obj)
#define	L_RATIONAL_DENOM(obj)	L_QUAD_SLOT2(obj)

extern LispObj l_make_ratnum(/* LispObj, LispObj */);
extern LispObj l_make_rational(/* LispObj, LispObj */);

/* Complex */

#define	L_COMPLEX_P(obj)	(L_CELL_ETAG(obj) == L_COMPLEX_TAG)
#define	L_COMPLEX_REAL(obj)	L_QUAD_SLOT1(obj)
#define	L_COMPLEX_IMAG(obj)	L_QUAD_SLOT2(obj)

extern LispObj l_make_cmpnum(/* LispObj, LispObj */);
extern LispObj l_make_complex(/* LispObj, LispObj */);

/*
 * Generic number macros & methods
 */

#define	L_NUM_EXACTNESS(obj)	L_QUAD_SLOT0(obj)

#define	L_NUMBER_P(obj) \
    (L_INT_P(obj) ||	\
     (L_PTR_P(obj) && (L_CELL_ETAG(obj) <= L_COMPLEX_TAG)))

#define	L_NUM_COMPLEX_P(obj) L_NUMBER_P(obj)

#define	L_NUM_REAL_P(obj) \
    (L_INT_P(obj) ||	\
     (L_PTR_P(obj) && (L_CELL_ETAG(obj) <= L_REAL_TAG)))

#define	L_NUM_RATIONAL_P(obj) \
    (L_INT_P(obj) ||	\
     (L_PTR_P(obj) && (L_CELL_ETAG(obj) <= L_RATIONAL_TAG)))

#define	L_NUM_INTEGER_P(obj) \
    (L_INT_P(obj) || (L_PTR_P(obj) && L_BIGINT_P(obj)))

#define	L_NUM_EXACT_P(obj)  \
    (L_NUM_INTEGER_P(obj) || L_NUM_EXACTNESS(obj))

#define	L_NUM_INEXACT_P(obj) \
    !(L_NUM_EXACT_P(obj))

extern LispObj l_sgn(/* LispObj */);
extern LispObj l_abs(/* LispObj */);
extern LispObj l_minus(/* LispObj */);
extern LispObj l_complement(/* LispObj */);

extern LispObj l_add(/* LispObj, LispObj */);
extern LispObj l_sub(/* LispObj, LispObj */);
extern LispObj l_mul(/* LispObj, LispObj */);
extern LispObj l_div(/* LispObj, LispObj */);

extern LispObj l_string_to_number(/* LispObj */);
extern LispObj l_number_to_string(/* LispObj, int */);

#endif
