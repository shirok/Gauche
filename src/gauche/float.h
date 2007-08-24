/*
 * float.h - auxilirary floating-point number support
 *
 *   Copyright (c) 2007  Shiro Kawai  <shiro@acm.org>
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
 *  $Id: float.h,v 1.4 2007-08-24 23:55:45 shirok Exp $
 */

/*
 * Defines some types and macros to support newer standard or non-standard
 * floating point numbers easily.
 */

#ifndef GAUCHE_FLOAT_H
#define GAUCHE_FLOAT_H

/* assuming gauche/config.h is read. */

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

/*
 * Long double support
 */

#ifdef HAVE_LONG_DOUBLE
typedef long double     ScmLongDouble;
#else
typedef double          ScmLongDouble;
#endif

/* IEEE754 double flonum structure.  This info may be provided by
 * a system header (e.g. ieee754.h) but for the portability we
 * define by ourselves.
 */
typedef union {
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
} ScmIEEEDouble;

/* NaN and Infinities.  The following works for most Unix platforms w/gcc.
   However, MSVC requires a different treatment. */
#ifndef SCM_DBL_POSITIVE_INFINITY
#define SCM_DBL_POSITIVE_INFINITY  (1.0/0.0)
#endif

#ifndef SCM_DBL_NEGATIVE_INFINITY
#define SCM_DBL_NEGATIVE_INFINITY  (-1.0/0.0)
#endif

#ifndef SCM_DBL_NAN
#define SCM_DBL_NAN           (0.0/0.0)
#endif

#endif /*GAUCHE_FLOAT_H*/
