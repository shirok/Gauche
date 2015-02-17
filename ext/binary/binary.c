/*
 * binary.c - Binary I/O routines
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

#include <gauche.h>
#include <gauche/extend.h>
#include <gauche/bytes_inline.h>
#include "binary.h"

#define ENSURE_IPORT(var)  if (!var) var = SCM_CURIN
#define ENSURE_OPORT(var)  if (!var) var = SCM_CUROUT

#define SWAP_16(e, v) do { if (SWAP_REQUIRED(e)) SWAP_2(v); } while (0)
#define SWAP_32(e, v) do { if (SWAP_REQUIRED(e)) SWAP_4(v); } while (0)
#define SWAP_64(e, v) do { if (SWAP_REQUIRED(e)) SWAP_8(v); } while (0)

#ifdef DOUBLE_ARMENDIAN
#define SWAP_D(e, v)                            \
    do {                                        \
        if (IS_ARM_LE(Scm_NativeEndian())) {    \
            if (IS_BE(e)) SWAP_ARM2BE(v);       \
            else if (IS_LE(e)) SWAP_ARM2LE(v);  \
        } else {                                \
            if (IS_ARM_LE(e)) SWAP_ARM2LE(v);   \
            else if (IS_BE(e)) SWAP_8(v);       \
        }                                       \
    } while (0)
#elif WORDS_BIGENDIAN
#define SWAP_D(e, v)                            \
    do {                                        \
        if (IS_LE(e)) SWAP_8(v);                \
        else if (IS_ARM_LE(e)) SWAP_ARM2BE(v);  \
    } while (0)
#else /*!WORDS_BIGENDIAN*/
#define SWAP_D(e, v)                            \
    do {                                        \
        if (IS_BE(e)) SWAP_8(v);                \
        else if (IS_ARM_LE(e)) SWAP_ARM2LE(v);  \
    } while (0)
#endif /*!WORDS_BIGENDIAN*/


/*===========================================================
 * Readers
 */

/* generic routine to handle byte-stream */
static inline int getbytes(char *buf, int len, ScmPort *iport)
{
    int nread = 0;
    ENSURE_IPORT(iport);
    while (nread < len) {
        int r = Scm_Getz(buf, len-nread, iport);
        if (r <= 0) return EOF;
        nread += r;
        buf += r;
    }
    return nread;
}

ScmObj Scm_ReadBinaryU8(ScmPort *iport, ScmSymbol *endian)
{
    int b;
    ENSURE_IPORT(iport);
    CHECK_ENDIAN(endian);
    if ((b = Scm_Getb(iport)) == EOF) return SCM_EOF;
    else return SCM_MAKE_INT(b);
}

ScmObj Scm_ReadBinaryS8(ScmPort *iport, ScmSymbol *endian)
{
    int b;
    ENSURE_IPORT(iport);
    CHECK_ENDIAN(endian);
    if ((b = Scm_Getb(iport)) == EOF) return SCM_EOF;
    if (b >= 128) b -= 256;
    return SCM_MAKE_INT(b);
}

ScmObj Scm_ReadBinaryU16(ScmPort *iport, ScmSymbol *endian)
{
    swap_u16_t v;
    CHECK_ENDIAN(endian);
    if (getbytes(v.buf, 2, iport) == EOF) return SCM_EOF;
    SWAP_16(endian, v);
    return SCM_MAKE_INT(v.val);
}

ScmObj Scm_ReadBinaryS16(ScmPort *iport, ScmSymbol *endian)
{
    swap_s16_t v;
    CHECK_ENDIAN(endian);
    if (getbytes(v.buf, 2, iport) == EOF) return SCM_EOF;
    SWAP_16(endian, v);
    return SCM_MAKE_INT(v.val);
}

ScmObj Scm_ReadBinaryU32(ScmPort *iport, ScmSymbol *endian)
{
    swap_u32_t v;
    CHECK_ENDIAN(endian);
    if (getbytes(v.buf, 4, iport) == EOF) return SCM_EOF;
    SWAP_32(endian, v);
    return Scm_MakeIntegerFromUI(v.val);
}

ScmObj Scm_ReadBinaryS32(ScmPort *iport, ScmSymbol *endian)
{
    swap_s32_t v;
    CHECK_ENDIAN(endian);
    if (getbytes(v.buf, 4, iport) == EOF) return SCM_EOF;
    SWAP_32(endian, v);
    return Scm_MakeInteger(v.val);
}

ScmObj Scm_ReadBinaryU64(ScmPort *iport, ScmSymbol *endian)
{
    swap_u64_t v;
    CHECK_ENDIAN(endian);
    if (getbytes(v.buf, 8, iport) == EOF) return SCM_EOF;
    SWAP_64(endian, v);
    return Scm_MakeIntegerU64(v.val);
}

ScmObj Scm_ReadBinaryS64(ScmPort *iport, ScmSymbol *endian)
{
    swap_s64_t v;
    CHECK_ENDIAN(endian);
    if (getbytes(v.buf, 8, iport) == EOF) return SCM_EOF;
    SWAP_64(endian, v);
    return Scm_MakeInteger64(v.val);
}

ScmObj Scm_ReadBinaryF16(ScmPort *iport, ScmSymbol *endian)
{
    swap_f16_t v;
    CHECK_ENDIAN(endian);
    if (getbytes(v.buf, 2, iport) == EOF) return SCM_EOF;
    SWAP_16(endian, v);
    return Scm_MakeFlonum(Scm_HalfToDouble(v.val));
}

ScmObj Scm_ReadBinaryF32(ScmPort *iport, ScmSymbol *endian)
{
    swap_f32_t v;
    CHECK_ENDIAN(endian);
    if (getbytes(v.buf, 4, iport) == EOF) return SCM_EOF;
    SWAP_32(endian, v);
    return Scm_MakeFlonum((double)v.val);
}

ScmObj Scm_ReadBinaryF64(ScmPort *iport, ScmSymbol *endian)
{
    swap_f64_t v;
    CHECK_ENDIAN(endian);
    if (getbytes(v.buf, 8, iport) == EOF) return SCM_EOF;
    SWAP_D(endian, v);
    return Scm_MakeFlonum(v.val);
}

/*===========================================================
 * Writers
 */

void Scm_WriteBinaryU8(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    int val = Scm_GetIntegerU8Clamp(sval, SCM_CLAMP_NONE, NULL);
    CHECK_ENDIAN(endian);
    ENSURE_OPORT(oport);
    Scm_Putb(val, oport);
}

void Scm_WriteBinaryS8(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    int val = Scm_GetInteger8Clamp(sval, SCM_CLAMP_NONE, NULL);
    CHECK_ENDIAN(endian);
    ENSURE_OPORT(oport);
    Scm_Putb(val, oport);
}

void Scm_WriteBinaryU16(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    swap_u16_t v;
    ENSURE_OPORT(oport);
    CHECK_ENDIAN(endian);
    v.val = Scm_GetIntegerU16Clamp(sval, SCM_CLAMP_NONE, NULL);
    SWAP_16(endian, v);
    Scm_Putz(v.buf, 2, oport);
}

void Scm_WriteBinaryS16(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    swap_s16_t v;
    ENSURE_OPORT(oport);
    CHECK_ENDIAN(endian);
    v.val = Scm_GetInteger16Clamp(sval, SCM_CLAMP_NONE, NULL);
    SWAP_16(endian, v);
    Scm_Putz(v.buf, 2, oport);
}

void Scm_WriteBinaryU32(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    swap_u32_t v;
    ENSURE_OPORT(oport);
    CHECK_ENDIAN(endian);
    v.val = Scm_GetIntegerU32Clamp(sval, FALSE, FALSE);
    SWAP_32(endian, v);
    Scm_Putz(v.buf, 4, oport);
}

void Scm_WriteBinaryS32(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    swap_s32_t v;
    ENSURE_OPORT(oport);
    CHECK_ENDIAN(endian);
    v.val = Scm_GetInteger32Clamp(sval, FALSE, FALSE);
    SWAP_32(endian, v);
    Scm_Putz(v.buf, 4, oport);
}

void Scm_WriteBinaryU64(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    swap_u64_t v;
    ENSURE_OPORT(oport);
    CHECK_ENDIAN(endian);
    v.val = Scm_GetIntegerU64Clamp(sval, FALSE, FALSE);
    SWAP_64(endian, v);
    Scm_Putz(v.buf, 8, oport);
}

void Scm_WriteBinaryS64(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    swap_s64_t v;
    ENSURE_OPORT(oport);
    CHECK_ENDIAN(endian);
    v.val = Scm_GetInteger64Clamp(sval, FALSE, FALSE);
    SWAP_64(endian, v);
    Scm_Putz(v.buf, 8, oport);
}

void Scm_WriteBinaryF16(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    swap_f16_t v;
    ENSURE_OPORT(oport);
    CHECK_ENDIAN(endian);
    v.val = Scm_DoubleToHalf(Scm_GetDouble(sval));
    SWAP_16(endian, v);
    Scm_Putz(v.buf, 2, oport);
}

void Scm_WriteBinaryF32(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    swap_f32_t v;
    ENSURE_OPORT(oport);
    CHECK_ENDIAN(endian);
    v.val = (float)Scm_GetDouble(sval);
    SWAP_32(endian, v);
    Scm_Putz(v.buf, 4, oport);
}

void Scm_WriteBinaryF64(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    swap_f64_t v;
    ENSURE_OPORT(oport);
    CHECK_ENDIAN(endian);
    v.val = Scm_GetDouble(sval);
    SWAP_D(endian, v);
    Scm_Putz(v.buf, 8, oport);
}

/*===========================================================
 * Getters
 */

static void extract(ScmUVector *uv, char *buf, int off, int eltsize)
{
    int size = Scm_UVectorSizeInBytes(uv);
    unsigned char *b = (unsigned char*)SCM_UVECTOR_ELEMENTS(uv) + off;

    if (off < 0 || off+eltsize > size) {
        Scm_Error("offset %d is out of bound of the uvector.", off);
    }
    for (int i=0; i<eltsize; i++) {
        *buf++ = *b++;
    }
}


ScmObj Scm_GetBinaryU8(ScmUVector *uv, int off, ScmSymbol *endian)
{
    unsigned char b;
    CHECK_ENDIAN(endian);
    extract(uv, (char *)&b, off, 1);
    return SCM_MAKE_INT(b);
}

ScmObj Scm_GetBinaryS8(ScmUVector *uv, int off, ScmSymbol *endian)
{
    unsigned char b;
    CHECK_ENDIAN(endian);
    extract(uv, (char *)&b, off, 1);
    int r = b;
    if (r >= 128) r -= 256;
    return SCM_MAKE_INT(r);
}

ScmObj Scm_GetBinaryU16(ScmUVector *uv, int off, ScmSymbol *endian)
{
    swap_u16_t v;
    CHECK_ENDIAN(endian);
    extract(uv, v.buf, off, 2);
    SWAP_16(endian, v);
    return SCM_MAKE_INT(v.val);
}

ScmObj Scm_GetBinaryS16(ScmUVector *uv, int off, ScmSymbol *endian)
{
    swap_s16_t v;
    CHECK_ENDIAN(endian);
    extract(uv, v.buf, off, 2);
    SWAP_16(endian, v);
    return SCM_MAKE_INT(v.val);
}

ScmObj Scm_GetBinaryU32(ScmUVector *uv, int off, ScmSymbol *endian)
{
    swap_u32_t v;
    CHECK_ENDIAN(endian);
    extract(uv, v.buf, off, 4);
    SWAP_32(endian, v);
    return Scm_MakeIntegerFromUI(v.val);
}

ScmObj Scm_GetBinaryS32(ScmUVector *uv, int off, ScmSymbol *endian)
{
    swap_s32_t v;
    CHECK_ENDIAN(endian);
    extract(uv, v.buf, off, 4);
    SWAP_32(endian, v);
    return Scm_MakeInteger(v.val);
}

ScmObj Scm_GetBinaryU64(ScmUVector *uv, int off, ScmSymbol *endian)
{
    swap_u64_t v;
    CHECK_ENDIAN(endian);
    extract(uv, v.buf, off, 8);
    SWAP_64(endian, v);
    return Scm_MakeIntegerU64(v.val);
}

ScmObj Scm_GetBinaryS64(ScmUVector *uv, int off, ScmSymbol *endian)
{
    swap_s64_t v;
    CHECK_ENDIAN(endian);
    extract(uv, v.buf, off, 8);
    SWAP_64(endian, v);
    return Scm_MakeInteger64(v.val);
}

ScmObj Scm_GetBinaryF16(ScmUVector *uv, int off, ScmSymbol *endian)
{
    swap_f16_t v;
    CHECK_ENDIAN(endian);
    extract(uv, v.buf, off, 2);
    SWAP_16(endian, v);
    return Scm_MakeFlonum(Scm_HalfToDouble(v.val));
}

ScmObj Scm_GetBinaryF32(ScmUVector *uv, int off, ScmSymbol *endian)
{
    swap_f32_t v;
    CHECK_ENDIAN(endian);
    extract(uv, v.buf, off, 4);
    SWAP_32(endian, v);
    return Scm_MakeFlonum((double)v.val);
}

ScmObj Scm_GetBinaryF64(ScmUVector *uv, int off, ScmSymbol *endian)
{
    swap_f64_t v;
    CHECK_ENDIAN(endian);
    extract(uv, v.buf, off, 8);
    SWAP_D(endian, v);
    return Scm_MakeFlonum(v.val);
}

/*===========================================================
 * Putters
 */

static void inject(ScmUVector *uv, char *buf, int off, int eltsize)
{
    int size = Scm_UVectorSizeInBytes(uv);
    unsigned char *b = (unsigned char*)SCM_UVECTOR_ELEMENTS(uv) + off;

    SCM_UVECTOR_CHECK_MUTABLE(SCM_OBJ(uv));

    if (off < 0 || off+eltsize > size) {
        Scm_Error("offset %d is out of bound of the uvector.", off);
    }
    for (int i=0; i<eltsize; i++) {
        *b++ = *buf++;
    }
}

void Scm_PutBinaryU8(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    u_char v = (u_char)Scm_GetIntegerU8Clamp(val, SCM_CLAMP_NONE, NULL);
    CHECK_ENDIAN(e);
    inject(uv, (char *)&v, off, 1);
}

void Scm_PutBinaryS8(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    u_char v = (u_char)Scm_GetInteger8Clamp(val, SCM_CLAMP_NONE, NULL);
    CHECK_ENDIAN(e);
    inject(uv, (char *)&v, off, 1);
}

void Scm_PutBinaryU16(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    swap_u16_t v;
    CHECK_ENDIAN(e);
    v.val = Scm_GetIntegerU16Clamp(val, SCM_CLAMP_NONE, NULL);
    SWAP_16(e, v);
    inject(uv, v.buf, off, 2);
}

void Scm_PutBinaryS16(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    swap_s16_t v;
    CHECK_ENDIAN(e);
    v.val = Scm_GetInteger16Clamp(val, SCM_CLAMP_NONE, NULL);
    SWAP_16(e, v);
    inject(uv, v.buf, off, 2);
}

void Scm_PutBinaryU32(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    swap_u32_t v;
    CHECK_ENDIAN(e);
    v.val = Scm_GetIntegerU32Clamp(val, FALSE, FALSE);
    SWAP_32(e, v);
    inject(uv, v.buf, off, 4);
}

void Scm_PutBinaryS32(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    swap_s32_t v;
    CHECK_ENDIAN(e);
    v.val = Scm_GetInteger32Clamp(val, FALSE, FALSE);
    SWAP_32(e, v);
    inject(uv, v.buf, off, 4);
}

void Scm_PutBinaryU64(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    swap_u64_t v;
    CHECK_ENDIAN(e);
    v.val = Scm_GetIntegerU64Clamp(val, FALSE, FALSE);
    SWAP_64(e, v);
    inject(uv, v.buf, off, 8);
}

void Scm_PutBinaryS64(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    swap_s64_t v;
    CHECK_ENDIAN(e);
    v.val = Scm_GetInteger64Clamp(val, FALSE, FALSE);
    SWAP_64(e, v);
    inject(uv, v.buf, off, 8);
}

void Scm_PutBinaryF16(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    swap_f16_t v;
    CHECK_ENDIAN(e);
    v.val = Scm_DoubleToHalf(Scm_GetDouble(val));
    SWAP_16(e, v);
    inject(uv, v.buf, off, 2);
}

void Scm_PutBinaryF32(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    swap_f32_t v;
    CHECK_ENDIAN(e);
    v.val = (float)Scm_GetDouble(val);
    SWAP_32(e, v);
    inject(uv, v.buf, off, 4);
}

void Scm_PutBinaryF64(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    swap_f64_t v;
    CHECK_ENDIAN(e);
    v.val = Scm_GetDouble(val);
    SWAP_D(e, v);
    inject(uv, v.buf, off, 8);
}
