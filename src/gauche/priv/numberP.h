/*
 * gauche/priv/numberP.h - Internal API for numbers
 *
 *   Copyright (c) 2026  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PRIV_NUMBERP_H
#define GAUCHE_PRIV_NUMBERP_H

extern double Scm__EncodeDouble64(uint64_t, int, int);
extern ScmObj Scm__ComplexExpt(ScmObj x, ScmObj y);

/* We can't construct a complex value by r + i*_Complex_I, for the real part
   becomes NaN when i is infinite, and the sign of zero may be lost.
   C11 has CMPLX, but it's not universally available.  As a fallback,
   we use a union; C guarantees a complex type has the same representation
   as an array of two elements of the corresponding real type. */
static inline ScmFloatComplex Scm__MakeFloatComplex(float r, float i)
{
#if defined(CMPLXF)
    return CMPLXF(r, i);
#else
    union { ScmFloatComplex c; float a[2]; } u;
    u.a[0] = r;
    u.a[1] = i;
    return u.c;
#endif
}

static inline ScmDoubleComplex Scm__MakeDoubleComplex(double r, double i)
{
#if defined(CMPLX)
    return CMPLX(r, i);
#else
    union { ScmDoubleComplex c; double a[2]; } u;
    u.a[0] = r;
    u.a[1] = i;
    return u.c;
#endif
}

static inline ScmLongDoubleComplex Scm__MakeLongDoubleComplex(long double r,
                                                              long double i)
{
#if defined(CMPLXL)
    return CMPLXL(r, i);
#else
    union { ScmLongDoubleComplex c; long double a[2]; } u;
    u.a[0] = r;
    u.a[1] = i;
    return u.c;
#endif
}

#endif /* GAUCHE_PRIV_NUMBERP_H */
