/*
 * gauche/bignum.h - Internal API for bignums
 *
 *   Copyright (c) 2000-2011  Shiro Kawai  <shiro@acm.org>
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

/* APIs concerning bignums.  They are not for general public use,
   so separated to here. */

#ifndef GAUCHE_BIGNUM_H
#define GAUCHE_BIGNUM_H

SCM_EXTERN ScmObj Scm_MakeBignumFromSI(long val);
SCM_EXTERN ScmObj Scm_MakeBignumFromUI(u_long val);
SCM_EXTERN ScmObj Scm_MakeBignumFromUIArray(int sign, u_long *values, int size);
SCM_EXTERN ScmObj Scm_MakeBignumFromDouble(double val);
SCM_EXTERN ScmObj Scm_BignumCopy(ScmBignum *b);
SCM_EXTERN ScmObj Scm_BignumToString(ScmBignum *b, int radix, int use_upper);

SCM_EXTERN long   Scm_BignumToSI(ScmBignum *b, int clamp, int* oor);
SCM_EXTERN u_long Scm_BignumToUI(ScmBignum *b, int clamp, int* oor);
#if SIZEOF_LONG == 4
SCM_EXTERN ScmInt64  Scm_BignumToSI64(ScmBignum *b, int clamp, int *oor);
SCM_EXTERN ScmUInt64 Scm_BignumToUI64(ScmBignum *b, int clamp, int *oor);
#else  /* SIZEOF_LONG >= 8 */
#define Scm_BignumToSI64       Scm_BignumToSI
#define Scm_BignumToUI64       Scm_BignumToUI
#endif /* SIZEOF_LONG >= 8 */
SCM_EXTERN double Scm_BignumToDouble(ScmBignum *b);
SCM_EXTERN ScmObj Scm_NormalizeBignum(ScmBignum *b);
SCM_EXTERN ScmObj Scm_BignumNegate(ScmBignum *b);
SCM_EXTERN int    Scm_BignumCmp(ScmBignum *bx, ScmBignum *by);
SCM_EXTERN int    Scm_BignumAbsCmp(ScmBignum *bx, ScmBignum *by);
SCM_EXTERN int    Scm_BignumCmp3U(ScmBignum *bx, ScmBignum *off, ScmBignum *by);
SCM_EXTERN ScmObj Scm_BignumComplement(ScmBignum *bx);

SCM_EXTERN ScmObj Scm_BignumAdd(ScmBignum *bx, ScmBignum *by);
SCM_EXTERN ScmObj Scm_BignumAddSI(ScmBignum *bx, long y);
SCM_EXTERN ScmObj Scm_BignumSub(ScmBignum *bx, ScmBignum *by);
SCM_EXTERN ScmObj Scm_BignumSubSI(ScmBignum *bx, long y);
SCM_EXTERN ScmObj Scm_BignumMul(ScmBignum *bx, ScmBignum *by);
SCM_EXTERN ScmObj Scm_BignumMulSI(ScmBignum *bx, long y);
SCM_EXTERN ScmObj Scm_BignumDivSI(ScmBignum *bx, long y, long *r);
SCM_EXTERN ScmObj Scm_BignumDivRem(ScmBignum *bx, ScmBignum *by);

SCM_EXTERN ScmObj Scm_BignumLogAndSI(ScmBignum *bx, long y);
SCM_EXTERN ScmObj Scm_BignumLogAnd(ScmBignum *bx, ScmBignum *by);
SCM_EXTERN ScmObj Scm_BignumLogIor(ScmBignum *bx, ScmBignum *by);
SCM_EXTERN ScmObj Scm_BignumLogXor(ScmBignum *bx, ScmBignum *by);
SCM_EXTERN ScmObj Scm_BignumLogNot(ScmBignum *bx);
SCM_EXTERN ScmObj Scm_BignumLogBit(ScmBignum *bx, int bit);
SCM_EXTERN int    Scm_BignumLogCount(ScmBignum *b);
SCM_EXTERN ScmObj Scm_BignumAsh(ScmBignum *bx, int cnt);

SCM_EXTERN ScmBignum *Scm_MakeBignumWithSize(int size, u_long init);
SCM_EXTERN ScmBignum *Scm_BignumAccMultAddUI(ScmBignum *acc, 
                                             u_long coef, u_long c);

SCM_EXTERN int Scm_DumpBignum(ScmBignum *b, ScmPort *out);

#endif /* GAUCHE_BIGNUM_H */

