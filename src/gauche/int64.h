/*
 * int64.h - auxilirary definition for 64bit integers
 *
 *   Copyright (c) 2004 Shiro Kawai, All rights reserved.
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
 *  $Id: int64.h,v 1.2 2004-01-30 04:05:47 shirok Exp $
 */

/* Some Scheme API needs to deal with 64bit signed/unsigned integer
   (such as srfi-4 vectors and binary I/O routines.)
   This file defines some macros to help writing code without
   concerning about how the hardware/compiler supports int64. */

#ifndef GAUCHE_INT64_H
#define GAUCHE_INT64_H

/* assuming gauche/config.h is read. */

#ifdef HAVE_STDINT_H
#include <stdint.h>
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

/* initialize given variable v64 to max/min values.
   only used on sizeof(long)==4 systems. */
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
#else  /* !SCM_EMULATE_INT64 */
#define SCM_SET_INT64_MAX(v64)  \
   ((v64) = ((((int64_t)LONG_MAX)<<32) + (int64_t)ULONG_MAX))
#define SCM_SET_INT64_MIN(v64)  \
   ((v64) = (((int64_t)LONG_MAX + 1) << 32))
#define SCM_SET_UINT64_MAX(v64) \
   ((v64) = ((((uint64_t)ULONG_MAX) << 32) + (uint64_t)ULONG_MAX))
#define SCM_SET_INT64_ZERO(v64) \
   ((v64) = 0)
#endif /* !SCM_EMULATE_INT64 */
#endif /* SIZEOF_LONG == 4 */

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

#endif /*GAUCHE_INT64_H*/
