/*
 * uvectorP.h - internal macros for the uniform vector module
 *
 *  Copyright(C) 2001-2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: uvectorP.h,v 1.7 2002-06-19 08:18:23 shirok Exp $
 */

#ifndef GAUCHE_UVECTOR_P_H
#define GAUCHE_UVECTOR_P_H

#include <float.h>

/* For each vector type VV, the following macro should be defined.
 * VVELTTYPE   - C type for elements of the vector
 * VVMAX       - Maximum value of the element, in VVELTTYPE
 * VVMIN       - Minimum value of the element, in VVELTTYPE
 * VVBOX(ScmObj obj, VVELTTYPE elt) - box given element and set it
 *               to obj.
 * VVUNBOX(VVELTTYPE elt, ScmObj obj, int clamp) - unbox the given
 *               ScmObj and set it to elt.  Check the range of obj.
 *               If it is out of range, deal with it according to clamp.
 * VVELTPRINT(ScmPort *out, VVELTTYPE elt) - print the element.
 * VVELTEQ(VVELTTYPE x, VVELTTYPE y) - compare two elements.
 */

#define BADOBJ(obj)      Scm_Error("bad type of object: %S", obj)
#define TOOSMALLOBJ(obj) Scm_Error("value too small: %S", obj)
#define TOOLARGEOBJ(obj) Scm_Error("value too large: %S", obj)

#ifdef HAVE_ISINF
#define ISINF(x)  isinf(x)
#else
#define ISINF(x)  ((x) != 0 && (x) == (x)/2.0)
#endif

#define CLAMP_HI_P(clamp)   ((clamp)&SCM_UVECTOR_CLAMP_HI)
#define CLAMP_LO_P(clamp)   ((clamp)&SCM_UVECTOR_CLAMP_LO)

/* clamp within integer range.  should be used for small numbers. */
#define CLAMP_INT(val, min, max, clamp)                 \
  do {                                                  \
    if (val < min) {                                    \
      if (CLAMP_LO_P(clamp)) val = min;                 \
      else Scm_Error("value too small: %d", val);       \
    } else if (val > max) {                             \
      if (CLAMP_HI_P(clamp)) val = min;                 \
      else Scm_Error("value too large: %d", val);       \
    }                                                   \
  } while (0)

/* val must be a bignum.  exclude it unconditionally.  should be used for
   small numbers. */
#define CLAMP_BIG(elt, val, min, max, clamp)    \
  do {                                          \
    if (SCM_BIGNUM_SIGN(val) < 0) {             \
      if (CLAMP_LO_P(clamp)) elt = min;         \
      else TOOSMALLOBJ(val);                    \
    } else if (SCM_BIGNUM_SIGN(val) > 0) {      \
      if (CLAMP_HI_P(clamp)) elt = max;         \
      else TOOLARGEOBJ(val);                    \
    }                                           \
  } while (0)

/* val must be float or double. */
#define CLAMP_FLT(val, min, max, clamp)                 \
  do {                                                  \
    if (ISINF(val)) {                                   \
      if (val < 0) {                                    \
        if (CLAMP_LO_P(clamp)) val = min;               \
        else Scm_Error("value too small: -infinity");   \
      } else {                                          \
        if (CLAMP_HI_P(clamp)) val = max;               \
        else Scm_Error("value too small: +infinity");   \
      }                                                 \
    }                                                   \
  } while (0)

/*
 * S8Vector
 */
#define S8ELTTYPE  signed char
#define S8MIN      -128
#define S8MAX       127
#define S8BOX(obj, elt)    (obj) = SCM_MAKE_INT(elt)
#define S8UNBOX(elt, obj, clamp)                                        \
  do {                                                                  \
    if (SCM_BIGNUMP(obj)) CLAMP_BIG(elt, obj, S8MIN, S8MAX, clamp);     \
    else if (!SCM_INTP(obj)) BADOBJ(obj);                               \
    else {                                                              \
      int V__ = SCM_INT_VALUE(obj);                                     \
      CLAMP_INT(V__, S8MIN, S8MAX, clamp);                              \
      elt = (S8ELTTYPE)V__;                                             \
    }                                                                   \
  } while (0)
#define S8ELTPRINT(out, elt)    Scm_Printf(out, "%d", elt)
#define S8ELTEQ(x, y)           ((x)==(y))

/*
 * U8Vector
 */
#define U8ELTTYPE unsigned char
#define U8MIN     0
#define U8MAX     255
#define U8BOX(obj, elt)    (obj) = SCM_MAKE_INT(elt)
#define U8UNBOX(elt, obj, clamp)                                        \
  do {                                                                  \
    if (SCM_BIGNUMP(obj)) CLAMP_BIG(elt, obj, U8MIN, U8MAX, clamp);     \
    else if (!SCM_INTP(obj)) BADOBJ(obj);                               \
    else {                                                              \
      int V__ = SCM_INT_VALUE(obj);                                     \
      CLAMP_INT(V__, U8MIN, U8MAX, clamp);                              \
      elt = (U8ELTTYPE)V__;                                             \
    }                                                                   \
  } while (0)
#define U8ELTPRINT(out, elt)    Scm_Printf(out, "%u", elt)
#define U8ELTEQ(x, y)           ((x)==(y))

/*
 * S16Vector
 */
#define S16ELTTYPE  signed short
#define S16MIN      -32768
#define S16MAX       32767
#define S16BOX(obj, elt)    (obj) = SCM_MAKE_INT(elt)
#define S16UNBOX(elt, obj, clamp)                                       \
  do {                                                                  \
    if (SCM_BIGNUMP(obj)) CLAMP_BIG(elt, obj, S16MIN, S16MAX, clamp);   \
    else if (!SCM_INTP(obj)) BADOBJ(obj);                               \
    else {                                                              \
      int V__ = SCM_INT_VALUE(obj);                                     \
      CLAMP_INT(V__, S16MIN, S16MAX, clamp);                            \
      elt = (S16ELTTYPE)V__;                                            \
    }                                                                   \
  } while (0)
#define S16ELTPRINT(out, elt)    Scm_Printf(out, "%d", elt)
#define S16ELTEQ(x, y)           ((x)==(y))

/*
 * U16Vector
 */
#define U16ELTTYPE unsigned short
#define U16MIN     0
#define U16MAX     65535
#define U16BOX(obj, elt)    (obj) = SCM_MAKE_INT(elt)
#define U16UNBOX(elt, obj, clamp)                                       \
  do {                                                                  \
    if (SCM_BIGNUMP(obj)) CLAMP_BIG(elt, obj, U16MIN, U16MAX, clamp);   \
    else if (!SCM_INTP(obj)) BADOBJ(obj);                               \
    else {                                                              \
      int V__ = SCM_INT_VALUE(obj);                                     \
      CLAMP_INT(V__, U16MIN, U16MAX, clamp);                            \
      elt = (U16ELTTYPE)V__;                                            \
    }                                                                   \
  } while (0)
#define U16ELTPRINT(out, elt)    Scm_Printf(out, "%u", elt)
#define U16ELTEQ(x, y)           ((x)==(y))

/*
 * S32Vector
 */
#define S32ELTTYPE SCM_UVECTOR_INT32
#define S32MAX     2147483647L
#define S32MIN     (-(S32MAX)-1)
#define S32BOX(obj, elt)    (obj) = Scm_MakeInteger(elt)
#if SIZEOF_LONG == 4
/* 32bit architecture */
#define S32UNBOX(elt, obj, clamp)                                       \
  do {                                                                  \
    if (SCM_INTP(obj)) {                                                \
      (elt) = SCM_INT_VALUE(obj);                                       \
    } else if (SCM_BIGNUMP(obj)) {                                      \
      if (Scm_NumCmp(obj, Scm_UvectorS32Min) < 0) {                     \
        if (CLAMP_LO_P(clamp)) (elt) = S32MIN;                          \
        else TOOSMALLOBJ(obj);                                          \
      } else if (Scm_NumCmp(obj, Scm_UvectorS32Max) > 0) {              \
        if (CLAMP_HI_P(clamp)) (elt) = S32MAX;                          \
        else TOOLARGEOBJ(obj);                                          \
      } else {                                                          \
        (elt) = (SCM_UVECTOR_INT32)Scm_BignumToSI(SCM_BIGNUM(obj));     \
      }                                                                 \
    } else BADOBJ(obj);                                                 \
  } while (0)
#else  /* SIZEOF_LONG >= 8 */
#define S32UNBOX(elt, obj, clamp)                                       \
  do {                                                                  \
    if (SCM_INTP(obj)) {                                                \
      long V__ = SCM_INT_VALUE(obj);                                    \
      CLAMP_INT(V__, S32MIN, S32MAX, clamp);                            \
      (elt) = (SCM_UVECTOR_INT32)V__;                                   \
    } else if (SCM_BIGNUMP(obj)) {                                      \
      if (Scm_NumCmp(obj, Scm_UvectorS32Min) < 0) {                     \
        if (CLAMP_LO_P(clamp)) (elt) = S32MIN;                          \
        else TOOSMALLOBJ(obj);                                          \
      } else if (Scm_NumCmp(obj, Scm_UvectorS32Max) > 0) {              \
        if (CLAMP_HI_P(clamp)) (elt) = S32MAX;                          \
        else TOOLARGEOBJ(obj);                                          \
      } else {                                                          \
        (elt) = (SCM_UVECTOR_INT32)Scm_BignumToSI(SCM_BIGNUM(obj));     \
      }                                                                 \
    } else BADOBJ(obj);                                                 \
  } while (0)
#endif /* SIZEOF_LONG >= 8 */
#define S32ELTPRINT(out, elt)  Scm_Printf(out, "%d", elt)
#define S32ELTEQ(x, y)         ((x)==(y))


/*
 * U32Vector
 */
#define U32ELTTYPE SCM_UVECTOR_UINT32
#define U32MIN     0
#define U32MAX     4294967295UL
#define U32BOX(obj, elt)    (obj) = Scm_MakeIntegerFromUI(elt)
#if SIZEOF_LONG == 4
/* 32bit architecture */
#define U32UNBOX(elt, obj, clamp)                                       \
  do {                                                                  \
    if (SCM_INTP(obj)) {                                                \
      (elt) = SCM_INT_VALUE(obj);                                       \
    } else if (SCM_BIGNUMP(obj)) {                                      \
      if (Scm_NumCmp(obj, Scm_UvectorU32Min) < 0) {                     \
        if (CLAMP_LO_P(clamp)) (elt) = U32MIN;                          \
        else TOOSMALLOBJ(obj);                                          \
      } else if (Scm_NumCmp(obj, Scm_UvectorU32Max) > 0) {              \
        if (CLAMP_HI_P(clamp)) (elt) = U32MAX;                          \
        else TOOLARGEOBJ(obj);                                          \
      } else {                                                          \
        (elt) = (SCM_UVECTOR_UINT32)Scm_BignumToUI(SCM_BIGNUM(obj));    \
      }                                                                 \
    } else BADOBJ(obj);                                                 \
  } while (0)
#else  /* SIZEOF_LONG >= 8 */
#define U32UNBOX(elt, obj, clamp)                                       \
  do {                                                                  \
    if (SCM_INTP(obj)) {                                                \
      long V__ = SCM_INT_VALUE(obj);                                    \
      CLAMP_INT(V__, U32MIN, U32MAX, clamp);                            \
      (elt) = (SCM_UVECTOR_UINT32)V__;                                  \
    } else if (SCM_BIGNUMP(obj)) {                                      \
      if (Scm_NumCmp(obj, Scm_UvectorU32Min) < 0) {                     \
        if (CLAMP_LO_P(clamp)) (elt) = U32MIN;                          \
        else TOOSMALLOBJ(obj);                                          \
      } else if (Scm_NumCmp(obj, Scm_UvectorU32Max) > 0) {              \
        if (CLAMP_HI_P(clamp)) (elt) = U32MAX;                          \
        else TOOLARGEOBJ(obj);                                          \
      } else {                                                          \
        (elt) = (SCM_UVECTOR_UINT32)Scm_BignumToSI(SCM_BIGNUM(obj));    \
      }                                                                 \
    } else BADOBJ(obj);                                                 \
  } while (0)
#endif /* SIZEOF_LONG >= 8 */
#define U32ELTPRINT(out, elt)  Scm_Printf(out, "%u", elt)
#define U32ELTEQ(x, y)         ((x)==(y))


/*
 * S64Vector
 */
#define S64ELTTYPE SCM_UVECTOR_INT64
#if SIZEOF_LONG == 4
#define S64MIN  Scm_UvectorS64Min
#define S64MAX  Scm_UvectorS64Max
#define S64BOX(obj, elt)  (obj) = (elt)
#define S64UNBOX(elt, obj, clamp)               \
  do {                                          \
    if (SCM_INTP(obj)) (elt) = (obj);           \
    else if (SCM_BIGNUMP(obj)) {                \
      if (Scm_NumCmp(obj, S64MIN) < 0) {        \
        if (CLAMP_LO_P(clamp)) (elt) = S64MIN;  \
        else TOOSMALLOBJ(obj);                  \
      } else if (Scm_NumCmp(obj, S64MAX) > 0) { \
        if (CLAMP_HI_P(clamp)) (elt) = S64MAX;  \
        else TOOLARGEOBJ(obj);                  \
      } else {                                  \
        (elt) = (obj);                          \
      }                                         \
    } else BADOBJ(obj);                         \
  } while (0)
#define S64ELTPRINT(out, elt)  Scm_Printf(out, "%S", elt)
#define S64ELTEQ(x, y)         Scm_NumEq(x, y)
#else /* SIZEOF_LONG >= 8 */
#define S64MIN -9223372036854775808L
#define S64MAX  9223372036854775807L
#define S64BOX(obj, elt)  (obj) = Scm_MakeInteger(elt)
#define S64UNBOX(elt, obj, clamp) 
  do {                                                                  \
    if (SCM_INTP(obj)) {                                                \
      (elt) = SCM_INT_VALUE(obj);                                       \
    } else if (SCM_BIGNUMP(obj)) {                                      \
      if (Scm_NumCmp(obj, Scm_UvectorS64Min) < 0) {                     \
        if (CLAMP_LO_P(clamp)) (elt) = S64MIN;                          \
        else TOOSMALLOBJ(obj);                                          \
      } else if (Scm_NumCmp(obj, Scm_UvectorS64Max) > 0) {              \
        if (CLAMP_HI_P(clamp)) (elt) = S64MAX;                          \
        else TOOLARGEOBJ(obj);                                          \
      } else {                                                          \
        (elt) = (SCM_UVECTOR_INT64)Scm_BignumToSI(SCM_BIGNUM(obj));     \
      }                                                                 \
    } else BADOBJ(obj);                                                 \
  } while (0)
#define S64ELTPRINT(out, elt)  Scm_Printf(out, "%ld", elt)
#define S64ELTEQ(x, y)         ((x)==(y))
#endif /* SIZEOF_LONG >= 8 */


/*
 * U64Vector
 */
#define U64ELTTYPE SCM_UVECTOR_INT64
#if SIZEOF_LONG == 4
#define U64MIN  Scm_UvectorU64Min
#define U64MAX  Scm_UvectorU64Max
#define U64BOX(obj, elt)  (obj) = (elt)
#define U64UNBOX(elt, obj, clamp)               \
  do {                                          \
    if (SCM_INTP(obj)) {                        \
      if (SCM_INT_VALUE(obj) < 0) {             \
        if (CLAMP_LO_P(clamp)) (elt) = U64MIN;  \
        else TOOSMALLOBJ(obj);                  \
      } else {                                  \
          (elt) = (obj);                        \
      }                                         \
    } else if (SCM_BIGNUMP(obj)) {              \
      if (Scm_NumCmp(obj, U64MIN) < 0) {        \
        if (CLAMP_LO_P(clamp)) (elt) = U64MIN;  \
        else TOOSMALLOBJ(obj);                  \
      } else if (Scm_NumCmp(obj, U64MAX) > 0) { \
        if (CLAMP_HI_P(clamp)) (elt) = U64MAX;  \
        else TOOLARGEOBJ(obj);                  \
      } else {                                  \
        (elt) = (obj);                          \
      }                                         \
    } else BADOBJ(obj);                         \
  } while (0)
#define U64ELTPRINT(out, elt)  Scm_Printf(out, "%S", elt)
#define U64ELTEQ(x, y)         Scm_NumEq(x, y)
#else /* SIZEOF_LONG >= 8 */
#define U64MIN  0UL
#define U64MAX  18446744073709551616UL
#define U64BOX(obj, elt)  (obj) = Scm_MakeIntegerFromUI(elt)
#define U64UNBOX(elt, obj, clamp)                                       \
  do {                                                                  \
    if (SCM_INTP(obj)) {                                                \
      long V__ = SCM_INT_VALUE(obj);                                    \
      if (V__ < 0) {                                                    \
        if (CLAMP_LO_P(clamp)) (elt) = U64MIN;                          \
        else TOOSMALLOBJ(obj);                                          \
      } else {                                                          \
        (elt) = V__;                                                    \
      }                                                                 \
    } else if (SCM_BIGNUMP(obj)) {                                      \
      if (Scm_NumCmp(obj, Scm_UvectorU64Min) < 0) {                     \
        if (CLAMP_LO_P(clamp)) (elt) = U64MIN;                          \
        else TOOSMALLOBJ(obj);                                          \
      } else if (Scm_NumCmp(obj, Scm_UvectorU64Max) > 0) {              \
        if (CLAMP_HI_P(clamp)) (elt) = U64MAX;                          \
        else TOOLARGEOBJ(obj);                                          \
      } else {                                                          \
        (elt) = (SCM_UVECTOR_INT64)Scm_BignumToUI(SCM_BIGNUM(obj));     \
      }                                                                 \
    } else BADOBJ(obj);                                                 \
  } while (0)
#define U64ELTPRINT(out, elt)  Scm_Printf(out, "%lu", elt)
#define U64ELTEQ(x, y)         ((x)==(y))
#endif /* SIZEOF_LONG >= 8 */


/*
 * F32Vector
 */
#define F32ELTTYPE  float
#define F32MIN      FLT_MIN
#define F32MAX      FLT_MAX
#define F32BOX(obj, elt)  (obj) = Scm_MakeFlonum((double)elt)
#define F32UNBOX(elt, obj, clamp)                                           \
    do {                                                                    \
        float v;                                                            \
        if (SCM_FLONUMP(obj)) v = (float)SCM_FLONUM_VALUE(obj);             \
        else if (SCM_INTP(obj)) v = (float)SCM_INT_VALUE(obj);              \
        else if (SCM_BIGNUMP(obj)) v = Scm_BignumToDouble(SCM_BIGNUM(obj)); \
        else BADOBJ(obj);                                                   \
        CLAMP_FLT(v, F32MIN, F32MAX, clamp);                                \
        elt = v;                                                            \
    } while (0)
#define F32ELTPRINT(out, elt) Scm_Printf(out, "%f", elt)
#define F32ELTEQ(x, y)        ((x)==(y))


/*
 * F64Vector
 */
#define F64ELTTYPE  double
#define F64MIN      DBL_MIN
#define F64MAX      DBL_MAX
#define F64BOX(obj, elt)  (obj) = Scm_MakeFlonum((double)elt)
#define F64UNBOX(elt, obj, clamp)                                           \
    do {                                                                    \
        double v;                                                           \
        if (SCM_FLONUMP(obj)) v = SCM_FLONUM_VALUE(obj);                    \
        else if (SCM_INTP(obj)) v = SCM_INT_VALUE(obj);                     \
        else if (SCM_BIGNUMP(obj)) v = Scm_BignumToDouble(SCM_BIGNUM(obj)); \
        else BADOBJ(obj);                                                   \
        CLAMP_FLT(v, F64MIN, F64MAX, clamp);                                \
        elt = v;                                                            \
    } while (0)
#define F64ELTPRINT(out, elt) Scm_Printf(out, "%f", elt)
#define F64ELTEQ(x, y)        ((x)==(y))

#endif /* GAUCHE_UVECTOR_P_H */
