/*
 * binary.c - Binary I/O routines
 *
 *   Copyright (c) 2004-2009  Shiro Kawai  <shiro@acm.org>
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
 *  $Id: binary.c,v 1.15 2008-05-10 13:35:35 shirok Exp $
 */

#include <gauche.h>
#include <gauche/extend.h>
#include "binary.h"

#define ENSURE_IPORT(var)  if (!var) var = SCM_CURIN
#define ENSURE_OPORT(var)  if (!var) var = SCM_CUROUT

/*===========================================================
 * Endian handling
 */

#define IS_BE(endian)     (SCM_EQ(SCM_OBJ(endian), SCM_SYM_BIG_ENDIAN))
#define IS_LE(endian)     (SCM_EQ(SCM_OBJ(endian), SCM_SYM_LITTLE_ENDIAN))
#define IS_ARM_LE(endian) (SCM_EQ(SCM_OBJ(endian), SCM_SYM_ARM_LITTLE_ENDIAN))

#if WORDS_BIGENDIAN
#define SWAP_REQUIRED(endian)   (!IS_BE(endian))
#else  /*!WORDS_BIGENDIAN.  Covers both little-endian and arm-little-endian. */
#define SWAP_REQUIRED(endian)   IS_BE(endian)
#endif

#define ECHECK(endian) \
    do { if (endian == NULL) endian = SCM_SYMBOL(Scm_DefaultEndian()); \
    } while (0)

/*
 * Swapping macros.   They can be used both ways (native <-> external)
 */

#define CSWAP(buf, tmp, n, m) (tmp=buf[n], buf[n]=buf[m], buf[m]=tmp)

#define SWAP2(e) \
  do { char z; if (SWAP_REQUIRED(e)) CSWAP(v.buf, z, 0, 1); } while (0)

#define SWAP4(e)                                                \
    do { char z;                                                \
        if (SWAP_REQUIRED(e)) {                                 \
            CSWAP(v.buf, z, 0, 3); CSWAP(v.buf, z, 1, 2);       \
        }                                                       \
    } while (0)

#define SWAP8(e)                                                \
    do { char z;                                                \
        if (SWAP_REQUIRED(e)) {                                 \
            CSWAP(v.buf, z, 0, 7); CSWAP(v.buf, z, 1, 6);       \
            CSWAP(v.buf, z, 2, 5); CSWAP(v.buf, z, 3, 4);       \
        }                                                       \
    } while (0)

#if WORDS_BIGENDIAN
#define SWAPD_STD(e)                                            \
    do { char z;                                                \
        if (IS_BE(e)) {                                         \
            ; /*noop*/                                          \
        } else if (IS_LE(e)) {     f                            \
            CSWAP(v.buf, z, 0, 7); CSWAP(v.buf, z, 1, 6);       \
            CSWAP(v.buf, z, 2, 5); CSWAP(v.buf, z, 3, 4);       \
        } else if (IS_ARM_LE(e)) {                              \
            CSWAP(v.buf, z, 0, 3); CSWAP(v.buf, z, 1, 2);       \
            CSWAP(v.buf, z, 4, 7); CSWAP(v.buf, z, 5, 6);       \
        }                                                       \
    } while (0)
#else  /*!WORDS_BIGENDIAN*/
#define SWAPD_STD(e)                                            \
    do { char z;                                                \
        if (IS_BE(e)) {                                         \
            CSWAP(v.buf, z, 0, 7); CSWAP(v.buf, z, 1, 6);       \
            CSWAP(v.buf, z, 2, 5); CSWAP(v.buf, z, 3, 4);       \
        } else if (IS_LE(e)) {                                  \
            ; /*noop*/                                          \
        } else if (IS_ARM_LE(e)) {                              \
            CSWAP(v.buf, z, 0, 4); CSWAP(v.buf, z, 1, 5);       \
            CSWAP(v.buf, z, 2, 6); CSWAP(v.buf, z, 3, 7);       \
        }                                                       \
    } while (0)
#endif /*!WORDS_BIGENDIAN*/

/* Swapping for double float is a bit tricky for ARM_LE case:
   [01234567] -> [32107654]. */
#ifdef DOUBLE_ARMENDIAN
#define SWAPD_ARM(e)                                            \
    do { char z;                                                \
        if (IS_BE(e)) {                                         \
            CSWAP(v.buf, z, 0, 3); CSWAP(v.buf, z, 1, 2);       \
            CSWAP(v.buf, z, 4, 7); CSWAP(v.buf, z, 5, 6);       \
        } else if (IS_LE(e)) {                                  \
            CSWAP(v.buf, z, 0, 4); CSWAP(v.buf, z, 1, 5);       \
            CSWAP(v.buf, z, 2, 6); CSWAP(v.buf, z, 3, 7);       \
        } else if (IS_ARM_LE(e)) {                              \
            ; /*noop*/                                          \
        }                                                       \
    } while (0)

#define SWAPD(e)                                         \
    do {                                                 \
        if (IS_ARM_LE(Scm_NativeEndian())) SWAPD_ARM(e); \
        else SWAPD_STD(e);                               \
    } while (0)

#else  /*!DOUBLE_ARMENDIAN*/
#define SWAPD(e)  SWAPD_STD(e)
#endif /*!DOUBLE_ARMENDIAN*/

/*===========================================================
 * Readers
 */

/* generic routine to handle byte-stream */
static inline int getbytes(char *buf, int len, ScmPort *iport)
{
    int nread = 0, r;
    ENSURE_IPORT(iport);
    while (nread < len) {
        r = Scm_Getz(buf, len-nread, iport);
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
    ECHECK(endian);
    if ((b = Scm_Getb(iport)) == EOF) return SCM_EOF;
    else return SCM_MAKE_INT(b);
}

ScmObj Scm_ReadBinaryS8(ScmPort *iport, ScmSymbol *endian)
{
    int b;
    ENSURE_IPORT(iport);
    ECHECK(endian);
    if ((b = Scm_Getb(iport)) == EOF) return SCM_EOF;
    if (b >= 128) b -= 256;
    return SCM_MAKE_INT(b);
}

ScmObj Scm_ReadBinaryU16(ScmPort *iport, ScmSymbol *endian)
{
    union { char buf[2]; unsigned short val; } v;
    ECHECK(endian);
    if (getbytes(v.buf, 2, iport) == EOF) return SCM_EOF;
    SWAP2(endian);
    return SCM_MAKE_INT(v.val);
}

ScmObj Scm_ReadBinaryS16(ScmPort *iport, ScmSymbol *endian)
{
    union { char buf[2]; short val; } v;
    ECHECK(endian);
    if (getbytes(v.buf, 2, iport) == EOF) return SCM_EOF;
    SWAP2(endian);
    return SCM_MAKE_INT(v.val);
}

ScmObj Scm_ReadBinaryU32(ScmPort *iport, ScmSymbol *endian)
{
    union { char buf[4]; ScmUInt32 val; } v;
    ECHECK(endian);
    if (getbytes(v.buf, 4, iport) == EOF) return SCM_EOF;
    SWAP4(endian);
    return Scm_MakeIntegerFromUI(v.val);
}

ScmObj Scm_ReadBinaryS32(ScmPort *iport, ScmSymbol *endian)
{
    union { char buf[4]; ScmInt32 val; } v;
    ECHECK(endian);
    if (getbytes(v.buf, 4, iport) == EOF) return SCM_EOF;
    SWAP4(endian);
    return Scm_MakeInteger(v.val);
}

ScmObj Scm_ReadBinaryU64(ScmPort *iport, ScmSymbol *endian)
{
    union { char buf[8]; ScmUInt64 val; } v;
    ECHECK(endian);
    if (getbytes(v.buf, 8, iport) == EOF) return SCM_EOF;
    SWAP8(endian);
    return Scm_MakeIntegerU64(v.val);
}

ScmObj Scm_ReadBinaryS64(ScmPort *iport, ScmSymbol *endian)
{
    union { char buf[8]; ScmInt64 val; } v;
    ECHECK(endian);
    if (getbytes(v.buf, 8, iport) == EOF) return SCM_EOF;
    SWAP8(endian);
    return Scm_MakeInteger64(v.val);
}

ScmObj Scm_ReadBinaryF16(ScmPort *iport, ScmSymbol *endian)
{
    union { char buf[2]; ScmHalfFloat val; } v;
    ECHECK(endian);
    if (getbytes(v.buf, 2, iport) == EOF) return SCM_EOF;
    SWAP2(endian);
    return Scm_MakeFlonum(Scm_HalfToDouble(v.val));
}

ScmObj Scm_ReadBinaryF32(ScmPort *iport, ScmSymbol *endian)
{
    union { char buf[4]; float val; } v;
    ECHECK(endian);
    if (getbytes(v.buf, 4, iport) == EOF) return SCM_EOF;
    SWAP4(endian);
    return Scm_MakeFlonum((double)v.val);
}

ScmObj Scm_ReadBinaryF64(ScmPort *iport, ScmSymbol *endian)
{
    union { char buf[8]; double val;} v;
    ECHECK(endian);
    if (getbytes(v.buf, 8, iport) == EOF) return SCM_EOF;
    SWAPD(endian);
    return Scm_MakeFlonum(v.val);
}

/*===========================================================
 * Writers
 */

void Scm_WriteBinaryU8(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    int val = Scm_GetIntegerU8Clamp(sval, SCM_CLAMP_NONE, NULL);
    ECHECK(endian);
    ENSURE_OPORT(oport);
    Scm_Putb(val, oport);
}

void Scm_WriteBinaryS8(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    int val = Scm_GetInteger8Clamp(sval, SCM_CLAMP_NONE, NULL);
    ECHECK(endian);
    ENSURE_OPORT(oport);
    Scm_Putb(val, oport);
}

void Scm_WriteBinaryU16(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    union { char buf[2]; u_short val; } v;
    ENSURE_OPORT(oport);
    ECHECK(endian);
    v.val = Scm_GetIntegerU16Clamp(sval, SCM_CLAMP_NONE, NULL);
    SWAP2(endian);
    Scm_Putz(v.buf, 2, oport);
}

void Scm_WriteBinaryS16(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    union { char buf[2]; short val; } v;
    ENSURE_OPORT(oport);
    ECHECK(endian);
    v.val = Scm_GetInteger16Clamp(sval, SCM_CLAMP_NONE, NULL);
    SWAP2(endian);
    Scm_Putz(v.buf, 2, oport);
}

void Scm_WriteBinaryU32(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    union { char buf[4]; ScmUInt32 val; } v;
    ENSURE_OPORT(oport);
    ECHECK(endian);
    v.val = Scm_GetIntegerU32Clamp(sval, FALSE, FALSE);
    SWAP4(endian);
    Scm_Putz(v.buf, 4, oport);
}

void Scm_WriteBinaryS32(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    union { char buf[4]; ScmInt32 val; } v;
    ENSURE_OPORT(oport);
    ECHECK(endian);
    v.val = Scm_GetInteger32Clamp(sval, FALSE, FALSE);
    SWAP4(endian);
    Scm_Putz(v.buf, 4, oport);
}

void Scm_WriteBinaryU64(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    union { char buf[8]; ScmUInt64 val; } v;
    ENSURE_OPORT(oport);
    ECHECK(endian);
    v.val = Scm_GetIntegerU64Clamp(sval, FALSE, FALSE);
    SWAP8(endian);
    Scm_Putz(v.buf, 8, oport);
}

void Scm_WriteBinaryS64(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    union { char buf[8]; ScmInt64 val; } v;
    ENSURE_OPORT(oport);
    ECHECK(endian);
    v.val = Scm_GetInteger64Clamp(sval, FALSE, FALSE);
    SWAP8(endian);
    Scm_Putz(v.buf, 8, oport);
}

void Scm_WriteBinaryF16(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    union { char buf[2]; ScmHalfFloat val; } v;
    ENSURE_OPORT(oport);
    ECHECK(endian);
    v.val = Scm_DoubleToHalf(Scm_GetDouble(sval));
    SWAP2(endian);
    Scm_Putz(v.buf, 2, oport);
}

void Scm_WriteBinaryF32(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    union { char buf[4]; float val; } v;
    ENSURE_OPORT(oport);
    ECHECK(endian);
    v.val = (float)Scm_GetDouble(sval);
    SWAP4(endian);
    Scm_Putz(v.buf, 4, oport);
}

void Scm_WriteBinaryF64(ScmObj sval, ScmPort *oport, ScmSymbol *endian)
{
    union { char buf[8]; double val; } v;
    ENSURE_OPORT(oport);
    ECHECK(endian);
    v.val = Scm_GetDouble(sval);
    SWAPD(endian);
    Scm_Putz(v.buf, 8, oport);
}

/*===========================================================
 * Getters
 */

static void extract(ScmUVector *uv, unsigned char *buf, int off, int eltsize)
{
    int size = Scm_UVectorSizeInBytes(uv);
    unsigned char *b = (unsigned char*)SCM_UVECTOR_ELEMENTS(uv) + off;
    int i;
    
    if (off < 0 || off+eltsize > size) {
        Scm_Error("offset %d is out of bound of the uvector.", off);
    }
    for (i=0; i<eltsize; i++) {
        *buf++ = *b++;
    }
}


ScmObj Scm_GetBinaryU8(ScmUVector *uv, int off, ScmSymbol *endian)
{
    unsigned char b;
    ECHECK(endian);
    extract(uv, &b, off, 1);
    return SCM_MAKE_INT(b);
}

ScmObj Scm_GetBinaryS8(ScmUVector *uv, int off, ScmSymbol *endian)
{
    unsigned char b; int r;
    ECHECK(endian);
    extract(uv, &b, off, 1);
    r = b;
    if (r >= 128) r -= 256; 
    return SCM_MAKE_INT(r);
}

ScmObj Scm_GetBinaryU16(ScmUVector *uv, int off, ScmSymbol *endian)
{
    union { unsigned char buf[2]; unsigned short val; } v;
    ECHECK(endian);
    extract(uv, v.buf, off, 2);
    SWAP2(endian);
    return SCM_MAKE_INT(v.val);
}

ScmObj Scm_GetBinaryS16(ScmUVector *uv, int off, ScmSymbol *endian)
{
    union { unsigned char buf[2]; short val; } v;
    ECHECK(endian);
    extract(uv, v.buf, off, 2);
    SWAP2(endian);
    return SCM_MAKE_INT(v.val);
}

ScmObj Scm_GetBinaryU32(ScmUVector *uv, int off, ScmSymbol *endian)
{
    union { unsigned char buf[4]; ScmUInt32 val; } v;
    ECHECK(endian);
    extract(uv, v.buf, off, 4);
    SWAP4(endian);
    return Scm_MakeIntegerFromUI(v.val);
}

ScmObj Scm_GetBinaryS32(ScmUVector *uv, int off, ScmSymbol *endian)
{
    union { unsigned char buf[4]; ScmInt32 val; } v;
    ECHECK(endian);
    extract(uv, v.buf, off, 4);
    SWAP4(endian);
    return Scm_MakeInteger(v.val);
}

ScmObj Scm_GetBinaryU64(ScmUVector *uv, int off, ScmSymbol *endian)
{
    union { unsigned char buf[8]; ScmUInt64 val; } v;
    ECHECK(endian);
    extract(uv, v.buf, off, 8);
    SWAP8(endian);
    return Scm_MakeIntegerU64(v.val);
}

ScmObj Scm_GetBinaryS64(ScmUVector *uv, int off, ScmSymbol *endian)
{
    union { unsigned char buf[8]; ScmInt64 val; } v;
    ECHECK(endian);
    extract(uv, v.buf, off, 8);
    SWAP8(endian);
    return Scm_MakeInteger64(v.val);
}

ScmObj Scm_GetBinaryF16(ScmUVector *uv, int off, ScmSymbol *endian)
{
    union { unsigned char buf[2]; ScmHalfFloat val; } v;
    ECHECK(endian);
    extract(uv, v.buf, off, 2);
    SWAP2(endian);
    return Scm_MakeFlonum(Scm_HalfToDouble(v.val));
}

ScmObj Scm_GetBinaryF32(ScmUVector *uv, int off, ScmSymbol *endian)
{
    union { unsigned char buf[4]; float val; } v;
    ECHECK(endian);
    extract(uv, v.buf, off, 4);
    SWAP4(endian);
    return Scm_MakeFlonum((double)v.val);
}

ScmObj Scm_GetBinaryF64(ScmUVector *uv, int off, ScmSymbol *endian)
{
    union { unsigned char buf[8]; double val;} v;
    ECHECK(endian);
    extract(uv, v.buf, off, 8);
    SWAPD(endian);
    return Scm_MakeFlonum(v.val);
}

/*===========================================================
 * Putters
 */

static void inject(ScmUVector *uv, unsigned char *buf, int off, int eltsize)
{
    int size = Scm_UVectorSizeInBytes(uv);
    unsigned char *b = (unsigned char*)SCM_UVECTOR_ELEMENTS(uv) + off;
    int i;

    SCM_UVECTOR_CHECK_MUTABLE(SCM_OBJ(uv));
    
    if (off < 0 || off+eltsize > size) {
        Scm_Error("offset %d is out of bound of the uvector.", off);
    }
    for (i=0; i<eltsize; i++) {
        *b++ = *buf++;
    }
}

void Scm_PutBinaryU8(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    u_char v = (u_char)Scm_GetIntegerU8Clamp(val, SCM_CLAMP_NONE, NULL);
    ECHECK(e);
    inject(uv, &v, off, 1);
}

void Scm_PutBinaryS8(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    u_char v = (u_char)Scm_GetInteger8Clamp(val, SCM_CLAMP_NONE, NULL);
    ECHECK(e);
    inject(uv, &v, off, 1);
}

void Scm_PutBinaryU16(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    union { unsigned char buf[2]; u_short val; } v;
    ECHECK(e);
    v.val = Scm_GetIntegerU16Clamp(val, SCM_CLAMP_NONE, NULL);
    SWAP2(e);
    inject(uv, v.buf, off, 2);
}

void Scm_PutBinaryS16(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    union { unsigned char buf[2]; short val; } v;
    ECHECK(e);
    v.val = Scm_GetInteger16Clamp(val, SCM_CLAMP_NONE, NULL);
    SWAP2(e);
    inject(uv, v.buf, off, 2);
}

void Scm_PutBinaryU32(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    union { unsigned char buf[4]; ScmUInt32 val; } v;
    ECHECK(e);
    v.val = Scm_GetIntegerU32Clamp(val, FALSE, FALSE);
    SWAP4(e);
    inject(uv, v.buf, off, 4);
}    

void Scm_PutBinaryS32(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    union { unsigned char buf[4]; ScmInt32 val; } v;
    ECHECK(e);
    v.val = Scm_GetInteger32Clamp(val, FALSE, FALSE);
    SWAP4(e);
    inject(uv, v.buf, off, 4);
}

void Scm_PutBinaryU64(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    union { unsigned char buf[8]; ScmUInt64 val; } v;
    ECHECK(e);
    v.val = Scm_GetIntegerU64Clamp(val, FALSE, FALSE);
    SWAP8(e);
    inject(uv, v.buf, off, 8);
}

void Scm_PutBinaryS64(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    union { unsigned char buf[8]; ScmInt64 val; } v;
    ECHECK(e);
    v.val = Scm_GetInteger64Clamp(val, FALSE, FALSE);
    SWAP8(e);
    inject(uv, v.buf, off, 8);
}

void Scm_PutBinaryF16(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    union { unsigned char buf[2]; ScmHalfFloat val; } v;
    ECHECK(e);
    v.val = Scm_DoubleToHalf(Scm_GetDouble(val));
    SWAP2(e);
    inject(uv, v.buf, off, 2);
}

void Scm_PutBinaryF32(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    union { unsigned char buf[4]; float val; } v;
    ECHECK(e);
    v.val = (float)Scm_GetDouble(val);
    SWAP4(e);
    inject(uv, v.buf, off, 4);
}

void Scm_PutBinaryF64(ScmUVector *uv, int off, ScmObj val, ScmSymbol *e)
{
    union { unsigned char buf[8]; double val; } v;
    ECHECK(e);
    v.val = Scm_GetDouble(val);
    SWAPD(e);
    inject(uv, v.buf, off, 8);
}

/*
 * Init
 */
extern void Scm_Init_binarylib(ScmModule *mod);

SCM_EXTENSION_ENTRY void Scm_Init_binary__io(void)
{
    ScmModule *mod_io = SCM_FIND_MODULE("binary.io", SCM_FIND_MODULE_CREATE);
    SCM_INIT_EXTENSION(binary__io);
    Scm_Init_binarylib(mod_io);
}
