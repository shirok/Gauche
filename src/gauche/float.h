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
 *  $Id: float.h,v 1.2 2007-03-22 11:20:31 shirok Exp $
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


#endif /*GAUCHE_FLOAT_H*/
