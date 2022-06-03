/*
 * int64.h - auxilirary definition for 64bit integers
 *
 *   Copyright (c) 2004-2022  Shiro Kawai  <shiro@acm.org>
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

/* TRANSIENT: This header is obsolete, for we now assume C99
   thus [u]int64_t is available.  We keep this until 1.0 release
   for API compatibility.  */


#ifndef GAUCHE_INT64_H
#define GAUCHE_INT64_H

#if GAUCHE_API_VERSION < 98

/* assuming gauche/config.h is read. */

typedef int32_t  ScmInt32;
typedef uint32_t ScmUInt32;
typedef int64_t  ScmInt64;
typedef uint64_t ScmUInt64;

#define SCM_SET_INT64_MAX(v64)    ((v64) = INT64_MAX)
#define SCM_SET_INT64_MIN(v64)    ((v64) = INT64_MIN)
#define SCM_SET_UINT64_MAX(v64)   ((v64) = UINT64_MAX)
#define SCM_SET_INT64_ZERO(v64)   ((v64) = 0)
#define SCM_SET_INT64_BY_LONG(v64, val)   ((v64) = (val))
#define SCM_SET_INT64_BY_DOUBLE(v64, val) ((v64) = (int64_t)(val))

#define SCM_INT64_EQV(a64, b64)     ((a64) == (b64))
#define SCM_INT64_CMP(op, a64, b64)     ((a64) op (b64))

/* for the backward compatibility */
#define SCM_INT64_TO_DOUBLE  Scm_Int64ToDouble

double    Scm_Int64ToDouble(int64_t v);
double    Scm_UInt64ToDouble(uint64_t v);
int64_t   Scm_DoubleToInt64(double v);
uint64_t  Scm_DoubleToUInt64(double v);

#endif /*GAUCHE_API_VERSION < 98*/

#endif /*GAUCHE_INT64_H*/
