/*
 * scmconst.h - frequently used static constant values
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

#ifndef GAUCHE_SCMCONST_H
#define GAUCHE_SCMCONST_H

SCM_EXTERN ScmObj Scm__ConstObjs[];   /* initialized in number.c */

#define SCM_2_64               (Scm__ConstObjs[0])    /*  2^64 */
#define SCM_2_64_MINUS_1       (Scm__ConstObjs[1])    /*  2^64-1 */
#define SCM_2_63               (Scm__ConstObjs[2])    /*  2^63 */
#define SCM_MINUS_2_63         (Scm__ConstObjs[3])    /* -2^63 */
#define SCM_2_53               (Scm__ConstObjs[4])    /*  2^53 */
#define SCM_2_52               (Scm__ConstObjs[5])    /*  2^52 */
#define SCM_2_32               (Scm__ConstObjs[6])    /*  2^32 */
#define SCM_2_31               (Scm__ConstObjs[7])    /*  2^31 */
#define SCM_MINUS_2_31         (Scm__ConstObjs[8])    /* -2^31 */
#define SCM_POSITIVE_INFINITY  (Scm__ConstObjs[9])    /* #i1/0 */
#define SCM_NEGATIVE_INFINITY  (Scm__ConstObjs[10])   /* #i-1/0 */
#define SCM_NAN                (Scm__ConstObjs[11])   /* #<nan> */

/* Minimum positive denormalized double as exact number: 1/2^1075 */
#define SCM_MIN_DENORMALIZED_FLONUM_EXACT (Scm__ConstObjs[12])

/* Maximum positive double as exact number: 2^1024 - 2^971 + 2^970 - 1 */
#define SCM_MAX_FINITE_FLONUM_EXACT (Scm__ConstObjs[13])

#define SCM_NUM_CONST_OBJS  14

#endif /*GAUCHE_SCMCONST_H*/
