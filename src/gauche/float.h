/*
 * float.h - auxilirary floating-point number support
 *
 *   Copyright (c) 2007-2022  Shiro Kawai  <shiro@acm.org>
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

/*
 * Defines some types and macros to support newer standard or non-standard
 * floating point numbers easily.
 */

#ifndef GAUCHE_FLOAT_H
#define GAUCHE_FLOAT_H

#include <math.h>
#include <complex.h>
#undef I                        /* avoid unintentional conflict */

#if defined(HAVE_FPU_CONTROL_H)
#include <fpu_control.h>
#endif

/* assuming gauche/config.h is read. */

/* We avoid to use C macro 'complex' for C++ extensions. */
typedef float _Complex ScmFloatComplex;
typedef double _Complex ScmDoubleComplex;

/*
 * Half float support
 */
#ifdef HAVE_UINT16_T
typedef uint16_t        ScmHalfFloat;
#else
typedef unsigned short  ScmHalfFloat;
#endif

#define SCM_HALF_FLOAT_SIGN_BIT(hf)  ((hf)&0x8000U)
#define SCM_HALF_FLOAT_EXPONENT(hf)  (((hf)&0x7c00U)>>10)
#define SCM_HALF_FLOAT_MANTISSA(hf)  ((hf)&0x03ffU)
#define SCM_HALF_FLOAT_IS_NAN(hf)                       \
    ((((~(hf))&0x7c00U) == 0) && (((hf)&0x03ffU) != 0))
#define SCM_HALF_FLOAT_CMP(op, hf1, hf2)        \
    (!SCM_HALF_FLOAT_IS_NAN(hf1)                \
     && !SCM_HALF_FLOAT_IS_NAN(hf2)             \
     && ((hf1) op (hf2)))

typedef struct {
    ScmHalfFloat r;
    ScmHalfFloat i;
} ScmHalfComplex;

#define SCM_HALF_COMPLEX_REAL(hc)  ((hc).r)
#define SCM_HALF_COMPLEX_IMAG(hc)  ((hc).i)

/*
 * Long double support
 */

#ifdef HAVE_LONG_DOUBLE
typedef long double     ScmLongDouble;
#else
typedef double          ScmLongDouble;
#endif

/* NaN and Infinities.  The following works for most Unix platforms w/gcc.
   However, Windows require a different treatment. */
#ifndef SCM_DBL_POSITIVE_INFINITY
#define SCM_DBL_POSITIVE_INFINITY  (1.0/0.0)
#endif

#ifndef SCM_DBL_NEGATIVE_INFINITY
#define SCM_DBL_NEGATIVE_INFINITY  (-1.0/0.0)
#endif

#ifndef SCM_DBL_NAN
#define SCM_DBL_NAN           (0.0/0.0)
#endif

#ifndef SCM_FLT_POSITIVE_INFINITY
#define SCM_FLT_POSITIVE_INFINITY  (1.0f/0.0f)
#endif

#ifndef SCM_FLT_NEGATIVE_INFINITY
#define SCM_FLT_NEGATIVE_INFINITY  (-1.0f/0.0f)
#endif

#ifndef SCM_FLT_NAN
#define SCM_FLT_NAN           (0.0f/0.0f)
#endif



#ifdef HAVE_ISNAN
#define SCM_IS_NAN(x)  isnan(x)
#else
#define SCM_IS_NAN(x)  (!((x)==(x)))
#endif

#ifdef HAVE_ISINF
#define SCM_IS_INF(x)  isinf(x)
#else
#define SCM_IS_INF(x)  Scm_IsInf(x)
SCM_EXTERN int Scm_IsInf(double x);
#endif

/*
 * Floating pointer control register
 *
 *  In some places we need to make sure the FP calculations are done
 *  with IEEE double precision, not with x87 extended double precision,
 *  for the latter would cause inaccuracy because of double-rounding.
 *
 *  Unfortunately setting FP control modes differ among platforms.
 */

#if defined(__MINGW32__) || defined(__MINGW64__)
#include <float.h>

/* Recent versions of Mingw GCC (4.6.1 and 4.7.0, afaik) has a bug that
   Windows float.h isn't included by mingw-gcc's float.h.  This is a kludge
   to let us use _controlfp().*/
#  ifndef _MCW_PC
extern unsigned int __cdecl _controlfp(unsigned int, unsigned int);
#  define _PC_53          0x00010000
#  define _MCW_PC         0x00030000
#  endif /*_MCW_PC*/

#define SCM_FP_ENSURE_DOUBLE_PRECISION_BEGIN()        \
    { unsigned int old_fpc_val__ = _controlfp(0, 0);  \
      _controlfp(_PC_53, _MCW_PC);

#define SCM_FP_ENSURE_DOUBLE_PRECISION_END() \
      _controlfp(old_fpc_val__, _MCW_PC); }

#elif defined(_FPU_GETCW) && defined(_FPU_EXTENDED) /* linux x86 */

#define SCM_FP_ENSURE_DOUBLE_PRECISION_BEGIN()        \
    { fpu_control_t old_fpc_val__, new_fpc_val__;     \
      _FPU_GETCW(old_fpc_val__);                      \
      new_fpc_val__ = ((old_fpc_val__ & ~_FPU_EXTENDED) | _FPU_DOUBLE); \
      _FPU_SETCW(new_fpc_val__);

#define SCM_FP_ENSURE_DOUBLE_PRECISION_END() \
      _FPU_SETCW(old_fpc_val__); }

#elif defined(__CYGWIN__)

/* Cygwin doesn't seem to have fpu_control.h
   We're too sloppy and don't bother restoring control word for now.
 */

#define SCM_FP_ENSURE_DOUBLE_PRECISION_BEGIN()  \
    do {                                        \
        static const u_short cw = 0x27f;        \
        asm volatile("fldcw %0": : "m"(cw));    \
    } while(0)

#define SCM_FP_ENSURE_DOUBLE_PRECISION_END()

#elif defined(__NetBSD__) && defined(__i386__) && defined(HAVE_FPSETPREC)
/*
 * NetBSD 6.99.26 switched to the x87 default control word (0x037f)
 * as initial value for new processes.
 */
#include <ieeefp.h>

#define SCM_FP_ENSURE_DOUBLE_PRECISION_BEGIN()        \
    { fp_prec_t old_prec__ = fpsetprec(FP_PD);
#define SCM_FP_ENSURE_DOUBLE_PRECISION_END() \
      fpsetprec(old_prec__); }

#else  /* fallback */
#define SCM_FP_ENSURE_DOUBLE_PRECISION_BEGIN() /* nothing */
#define SCM_FP_ENSURE_DOUBLE_PRECISION_END()   /* nothing */
#endif


#endif /*GAUCHE_FLOAT_H*/
