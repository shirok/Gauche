/*
 * uvectorP.h - internal macros for the uniform vector module
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_UVECTOR_P_H
#define GAUCHE_UVECTOR_P_H

/*--------------------------------------------------------
 * inline functions used privately
 */

/*-----------------------------------------------------------
 * range checks
 */
static void range_error(const char *type, ScmObj obj)
{
    if (SCM_INTP(obj) && SCM_INT_VALUE(obj) == 0) {
        Scm_Error("value out of domain for %svector", type);
    } else {
        Scm_Error("value out of domain for %svector: %S", type, obj);
    }
}

static inline long range_s8hi(long val, int clamp)
{
    if (!(clamp & SCM_CLAMP_HI)) range_error("s8", Scm_MakeInteger(val));
    return 127;
}

static inline long range_s8lo(long val, int clamp)
{
    if (!(clamp & SCM_CLAMP_LO)) range_error("s8", Scm_MakeInteger(val));
    return -128;
}

static inline long clamp_s8(long val, int clamp)
{
    if (val > 127)  return range_s8hi(val, clamp);
    if (val < -128) return range_s8lo(val, clamp);
    return val;
}

static inline long range_u8hi(long val, int clamp)
{
    if (!(clamp & SCM_CLAMP_HI)) range_error("u8", Scm_MakeInteger(val));
    return 255;
}

static inline long range_u8lo(long val, int clamp)
{
    if (!(clamp & SCM_CLAMP_LO)) range_error("u8", Scm_MakeInteger(val));
    return 0;
}

static inline long clamp_u8(long val, int clamp)
{
    if (val > 255)  return range_u8hi(val, clamp);
    if (val < 0)    return range_u8lo(val, clamp);
    return val;
}

static inline long range_s16hi(long val, int clamp)
{
    if (!(clamp & SCM_CLAMP_HI)) range_error("s16", Scm_MakeInteger(val));
    return 32767;
}

static inline long range_s16lo(long val, int clamp)
{
    if (!(clamp & SCM_CLAMP_LO)) range_error("s16", Scm_MakeInteger(val));
    return -32768;
}

static inline long clamp_s16(long val, int clamp)
{
    if (val > 32767)  return range_s16hi(val, clamp);
    if (val < -32768) return range_s16lo(val, clamp);
    return val;
}

static inline long range_u16hi(long val, int clamp)
{
    if (!(clamp & SCM_CLAMP_HI)) range_error("u16", Scm_MakeInteger(val));
    return 65535;
}

static inline long range_u16lo(long val, int clamp)
{
    if (!(clamp & SCM_CLAMP_LO)) range_error("u16", Scm_MakeInteger(val));
    return 0;
}

static inline long clamp_u16(long val, int clamp)
{
    if (val > 65535)  return range_u16hi(val, clamp);
    if (val < 0)      return range_u16lo(val, clamp);
    return val;
}

static inline long range_s32hi(long val, int clamp)
{
    if (!(clamp & SCM_CLAMP_HI)) range_error("s32", Scm_MakeInteger(val));
    return 2147483647L;
}

static inline long range_s32lo(long val, int clamp)
{
    if (!(clamp & SCM_CLAMP_LO)) range_error("s32", Scm_MakeInteger(val));
    return -2147483647L-1;
}

#if SIZEOF_LONG == 4
#define clamp_s32(val, clamp)   (val)
#else
static inline long clamp_s32(long val, int clamp)
{
    if (val > 2147483647L)  return range_s32hi(val, clamp);
    if (val < -2147483648L) return range_s32lo(val, clamp);
    return val;
}
#endif

static inline long range_u32hi(u_long val, int clamp)
{
    if (!(clamp & SCM_CLAMP_HI)) range_error("u32", Scm_MakeIntegerU(val));
    return 4294967295UL;
}

static inline long range_u32lo(u_long val, int clamp)
{
    if (!(clamp & SCM_CLAMP_LO)) range_error("u32", Scm_MakeIntegerU(val));
    return 0;
}

#if SIZEOF_LONG == 4
#define clamp_u32(val, clamp)   (val)
#else
static inline long clamp_u32(u_long val, int clamp)
{
    if (val > 4294967295UL)    return range_u32hi(val, clamp);
    return val;
}
#endif

static inline ScmInt64 range_s64hi(ScmInt64 val, int clamp)
{
    if (!(clamp & SCM_CLAMP_HI)) range_error("s64", Scm_MakeInteger64(val));
    SCM_SET_INT64_MAX(val);
    return val;
}

static inline ScmInt64 range_s64lo(ScmInt64 val, int clamp)
{
    if (!(clamp & SCM_CLAMP_LO)) range_error("s64", Scm_MakeInteger64(val));
    SCM_SET_INT64_MIN(val);
    return val;
}

static inline ScmUInt64 range_u64hi(ScmUInt64 val, int clamp)
{
    if (!(clamp & SCM_CLAMP_HI)) range_error("u64", Scm_MakeIntegerU64(val));
    SCM_SET_UINT64_MAX(val);
    return val;
}

static inline ScmUInt64 range_u64lo(ScmUInt64 val, int clamp)
{
    if (!(clamp & SCM_CLAMP_LO)) range_error("u64", Scm_MakeIntegerU64(val));
    SCM_SET_INT64_ZERO(val);
    return val;
}

/*
 * 'option' argument for Scm_UVectorSwapBytes.
 */

enum {
    SWAPB_STD,                  /* simple swapping */
    SWAPB_ARM_LE,               /* arm-little-endian <-> little-endian */
    SWAPB_ARM_BE                /* arm-little-endian <-> big-endian */
};


#endif /* GAUCHE_UVECTOR_P_H */
