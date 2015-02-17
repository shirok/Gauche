/*
 * int64.h - auxilirary definition for 64bit integers
 *
 *   Copyright (c) 2004-2015  Shiro Kawai  <shiro@acm.org>
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

/* Some Scheme API needs to deal with 64bit signed/unsigned integer
   (such as srfi-4 vectors and binary I/O routines.)
   This file defines some macros to help writing code without
   concerning about how the hardware/compiler supports int64.

   This header defines the following types and macros.

   [type] ScmInt64     64-bit signed integer.
   [type] ScmUInt64    64-bit unsigned integer.
   [type] ScmInt32     32-bit signed integer.
   [type] ScmUInt32    32-bit signed integer.

   Based on these types, gauche.h declares some conversion functions
   that can be used regardless of the representation of 64bit integer
   on the target machine.

   To box 64bit integer as ScmObj, use Scm_MakeInteger64 and
   Scm_MakeIntegerU64.

   To unbox 64bit integer from ScmObj, use Scm_GetInteger64 and
   Scm_GetIntegerU64.
*/

#ifndef GAUCHE_INT64_H
#define GAUCHE_INT64_H

/* assuming gauche/config.h is read. */

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

/* typedefs ScmInt64 and ScmUInt64 to appropriate type */

#if defined(HAVE_INT64_T) && defined(HAVE_UINT64_T)
/* If the system has int64_t, it is the best way to use it.
   Even some of ILP32 systems have int64_t. */
typedef int64_t  ScmInt64;
typedef uint64_t ScmUInt64;
#elif SIZEOF_LONG >= 8
/* NB: this would fail if sizeof(long) becomes 16; but such newer systems
   are likely to have int64_t defined. */
typedef long   ScmInt64;
typedef u_long ScmUInt64;
#else  /* SIZEOF_LONG == 4 */
/* This is the fallback. */
#define SCM_EMULATE_INT64 1
#ifdef WORDS_BIGENDIAN
typedef struct {
    unsigned long hi;
    unsigned long lo;
} ScmInt64, ScmUInt64;
#else  /* !WORDS_BIGENDIAN */
typedef struct {
    unsigned long lo;
    unsigned long hi;
} ScmInt64, ScmUInt64;
#endif /* !WORDS_BIGENDIAN */
#endif /* SIZEOF_LONG == 4 */

/* Used internally to set up 64bit integer values */
#if SIZEOF_LONG == 4
#if SCM_EMULATE_INT64
#define SCM_SET_INT64_MAX(v64)  \
    ((v64).hi = LONG_MAX, (v64).lo = ULONG_MAX)
#define SCM_SET_INT64_MIN(v64)  \
    ((v64).hi = (u_long)LONG_MAX + 1, (v64).lo = 0)
#define SCM_SET_UINT64_MAX(v64) \
    ((v64).hi = ULONG_MAX, (v64).lo = ULONG_MAX)
#define SCM_SET_INT64_ZERO(v64) \
    ((v64).hi = (v64).lo = 0)
#define SCM_SET_INT64_BY_LONG(v64, val) \
    ((v64).hi = 0, (v64).lo = (val))
#else  /* !SCM_EMULATE_INT64 */
#define SCM_SET_INT64_MAX(v64)  \
    ((v64) = ((((int64_t)LONG_MAX)<<32) + (int64_t)ULONG_MAX))
#define SCM_SET_INT64_MIN(v64)  \
    ((v64) = (((int64_t)LONG_MAX + 1) << 32))
#define SCM_SET_UINT64_MAX(v64) \
    ((v64) = ((((uint64_t)ULONG_MAX) << 32) + (uint64_t)ULONG_MAX))
#define SCM_SET_INT64_ZERO(v64)           ((v64) = 0)
#define SCM_SET_INT64_BY_LONG(v64, val)   ((v64) = (int64_t)(val))
#define SCM_SET_INT64_BY_DOUBLE(v64, val) ((v64) = (int64_t)(val))
#endif /* !SCM_EMULATE_INT64 */
#elif  SIZEOF_LONG == 8
#define SCM_SET_INT64_MAX(v64)    ((v64) = LONG_MAX)
#define SCM_SET_INT64_MIN(v64)    ((v64) = LONG_MIN)
#define SCM_SET_UINT64_MAX(v64)   ((v64) = ULONG_MAX)
#define SCM_SET_INT64_ZERO(v64)   ((v64) = 0)
#define SCM_SET_INT64_BY_LONG(v64, val)   ((v64) = (val))
#define SCM_SET_INT64_BY_DOUBLE(v64, val) ((v64) = (long)(val))
#endif /* SIZEOF_LONG == 4 or 8 */

/* Compare 64bit values */
#if SCM_EMULATE_INT64
#define SCM_INT64_EQV(a64, b64) ((a64).hi == (b64).hi && (a64).lo == (b64).lo)
#define SCM_INT64_CMP(op, a64, b64) \
    (((a64).hi op (b64).hi)         \
     || ((a64).hi == (b64).hi && (a64).lo op (b64).lo))
#else  /*!SCM_EMULATE_INT64*/
#define SCM_INT64_EQV(a64, b64)     ((a64) == (b64))
#define SCM_INT64_CMP(op, a64, b64) ((a64) op (b64))
#endif /*!SCM_EMULATE_INT64*/

/* ScmInt32 and ScmUInt32 can be used when one needs exactly 32bit int */

#if defined(HAVE_INT32_T) && defined(HAVE_UINT32_T)
typedef int32_t  ScmInt32;
typedef uint32_t ScmUInt32;
#elif SIZEOF_INT == 4
typedef int   ScmInt32;
typedef u_int ScmUInt32;
#elif SIZEOF_LONG == 4
typedef long   ScmInt32;
typedef u_long ScmUInt32;
#else
#error system does not have 32bit integer
#endif

/* Some macros that can be used without concerning underlying implementation
   of int64. */

/* for backward compatibility */
#define SCM_INT64_TO_DOUBLE  Scm_Int64ToDouble

double    Scm_Int64ToDouble(ScmInt64 v);
double    Scm_UInt64ToDouble(ScmUInt64 v);
ScmInt64  Scm_DoubleToInt64(double v);
ScmUInt64 Scm_DoubleToUInt64(double v);

#endif /*GAUCHE_INT64_H*/
