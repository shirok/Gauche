/*
 * binaryio.c - Binary I/O routines
 *
 *   Copyright (c) 2004-2007 Shiro Kawai, All rights reserved.
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
 *  $Id: binary.c,v 1.6 2007-02-17 13:03:39 shirok Exp $
 */

#include <gauche.h>
#include <gauche/extend.h>
#include "binary.h"

#define IPORT(var, x)                                                   \
  do {                                                                  \
    if (SCM_FALSEP(x)) {                                                \
      (var) = SCM_CURIN;                                                \
    } else if (SCM_IPORTP(x)) {                                         \
      (var) = SCM_PORT(x);                                              \
    } else {                                                            \
      Scm_Error("input port or #f is expected, but got: %S", x);        \
      (var) = NULL;                                                     \
    }                                                                   \
  } while (0)    

#define OPORT(var, x)                                                   \
  do {                                                                  \
    if (SCM_FALSEP(x)) {                                                \
      (var) = SCM_CUROUT;                                               \
    } else if (SCM_OPORTP(x)) {                                         \
      (var) = SCM_PORT(x);                                              \
    } else {                                                            \
      Scm_Error("output port or #f is expected, but got: %S", x);       \
      (var) = NULL;                                                     \
    }                                                                   \
  } while (0)    

/*
 * uint8 and sint8 is easy; just do it here.
 */
ScmObj Scm_ReadBinaryUint8(ScmObj sport, Endian endian)
{
    ScmPort *iport;
    int b;
    IPORT(iport, sport);
    if ((b = Scm_Getb(iport)) == EOF) return SCM_EOF;
    else return SCM_MAKE_INT(b);
}

ScmObj Scm_ReadBinarySint8(ScmObj sport, Endian endian)
{
    ScmPort *iport;
    int b;
    IPORT(iport, sport);
    if ((b = Scm_Getb(iport)) == EOF) return SCM_EOF;
    if (b >= 128) b -= 256;
    return SCM_MAKE_INT(b);
}

void Scm_WriteBinaryUint8(ScmObj sval, ScmObj sport, Endian endian)
{
    ScmPort *oport;
    int val = 0;
    OPORT(oport, sport);
    if (!SCM_INTP(sval)
        || ((val = SCM_INT_VALUE(sval)) < 0)
        || (val >= 256)) {
        Scm_Error("argument out of range (uint8): %S", sval);
    }
    Scm_Putb(val, oport);
}

void Scm_WriteBinarySint8(ScmObj sval, ScmObj sport, Endian endian)
{
    ScmPort *oport;
    int val = 0;
    OPORT(oport, sport);
    if (!SCM_INTP(sval)
        || ((val = SCM_INT_VALUE(sval)) < -128)
        || (val >= 128)) {
        Scm_Error("argument out of range (sint8): %S", sval);
    }
    Scm_Putb(val, oport);
}

/*
 * Readers
 */

/* generic routine to handle byte-stream */
static inline int getbytes(char *buf, int len, ScmObj sport)
{
    ScmPort *iport;
    int nread = 0, r;
    IPORT(iport, sport);
    while (nread < len) {
        r = Scm_Getz(buf, len-nread, iport);
        if (r <= 0) return EOF;
        nread += r;
        buf += r;
    }
    return nread;
}

#define CSWAP(buf, tmp, n, m) (tmp=buf[n], buf[n]=buf[m], buf[m]=tmp)

#define SWAP2() \
  do { char z; if (SWAP_REQUIRED(endian)) CSWAP(v.buf, z, 0, 1); } while (0)

#define SWAP4()                                 \
    do { char z;                                \
        if (SWAP_REQUIRED(endian)) {            \
            CSWAP(v.buf, z, 0, 3);              \
            CSWAP(v.buf, z, 1, 2);              \
        }                                       \
    } while (0)

#define SWAP8()                                 \
    do { char z;                                \
        if (SWAP_REQUIRED(endian)) {            \
            CSWAP(v.buf, z, 0, 7);              \
            CSWAP(v.buf, z, 1, 6);              \
            CSWAP(v.buf, z, 2, 5);              \
            CSWAP(v.buf, z, 3, 4);              \
        }                                       \
    } while (0)

/* ARM uses mixed endian for double.  [01234567] -> [32107654].
   For the time being, we won't support I/O for native ARM format.
   Bytes are swaped either for pure big-endian or pure little-endian. */
#ifdef DOUBLE_ARMENDIAN
#define SWAPD()                                 \
    do { char z;                                \
        if (endian == SCM_BE) {                 \
            CSWAP(v.buf, z, 0, 3);              \
            CSWAP(v.buf, z, 1, 2);              \
            CSWAP(v.buf, z, 4, 7);              \
            CSWAP(v.buf, z, 5, 6);              \
        } else {                                \
            CSWAP(v.buf, z, 0, 4);              \
            CSWAP(v.buf, z, 1, 5);              \
            CSWAP(v.buf, z, 2, 6);              \
            CSWAP(v.buf, z, 3, 7);              \
        }                                       \
  } while (0)
#else  /*!DOUBLE_ARMENDIAN*/
#define SWAPD()                                 \
  do { char z;                                  \
       if (SWAP_REQUIRED(endian)) {             \
         CSWAP(v.buf, z, 0, 7);                 \
         CSWAP(v.buf, z, 1, 6);                 \
         CSWAP(v.buf, z, 2, 5);                 \
         CSWAP(v.buf, z, 3, 4);                 \
       }                                        \
  } while (0)
#endif /*!DOUBLE_ARMENDIAN*/



ScmObj Scm_ReadBinaryUint16(ScmObj sport, Endian endian)
{
    union { char buf[2]; unsigned short val; } v;
    if (getbytes(v.buf, 2, sport) == EOF) return SCM_EOF;
    SWAP2();
    return SCM_MAKE_INT(v.val);
}

ScmObj Scm_ReadBinarySint16(ScmObj sport, Endian endian)
{
    union { char buf[2]; short val; } v;
    if (getbytes(v.buf, 2, sport) == EOF) return SCM_EOF;
    SWAP2();
    return SCM_MAKE_INT(v.val);
}

ScmObj Scm_ReadBinaryUint32(ScmObj sport, Endian endian)
{
    union { char buf[4]; ScmUInt32 val; } v;
    if (getbytes(v.buf, 4, sport) == EOF) return SCM_EOF;
    SWAP4();
    return Scm_MakeIntegerFromUI(v.val);
}

ScmObj Scm_ReadBinarySint32(ScmObj sport, Endian endian)
{
    union { char buf[4]; ScmInt32 val; } v;
    if (getbytes(v.buf, 4, sport) == EOF) return SCM_EOF;
    SWAP4();
    return Scm_MakeInteger(v.val);
}

ScmObj Scm_ReadBinaryUint64(ScmObj sport, Endian endian)
{
    union { char buf[8]; ScmUInt64 val; } v;
    if (getbytes(v.buf, 8, sport) == EOF) return SCM_EOF;
    SWAP8();
    return Scm_MakeIntegerU64(v.val);
}

ScmObj Scm_ReadBinarySint64(ScmObj sport, Endian endian)
{
    union { char buf[8]; ScmInt64 val; } v;
    if (getbytes(v.buf, 8, sport) == EOF) return SCM_EOF;
    SWAP8();
    return Scm_MakeInteger64(v.val);
}

ScmObj Scm_ReadBinaryHalfFloat(ScmObj sport, Endian endian)
{
    union { char buf[2]; ScmHalfFloat val; } v;
    if (getbytes(v.buf, 2, sport) == EOF) return SCM_EOF;
    SWAP2();
    return Scm_MakeFlonum(ScmHalfToDouble(v.val));
}

ScmObj Scm_ReadBinaryFloat(ScmObj sport, Endian endian)
{
    union { char buf[4]; float val; } v;
    if (getbytes(v.buf, 4, sport) == EOF) return SCM_EOF;
    SWAP4();
    return Scm_MakeFlonum((double)v.val);
}

ScmObj Scm_ReadBinaryDouble(ScmObj sport, Endian endian)
{
    union { char buf[8]; double val;} v;
    if (getbytes(v.buf, 8, sport) == EOF) return SCM_EOF;
    SWAPD();
    return Scm_MakeFlonum(v.val);
}

/*
 * Writers
 */

void Scm_WriteBinaryUint16(ScmObj sval, ScmObj sport, Endian endian)
{
    union { char buf[2]; u_short val; } v;
    long vv = 0;
    ScmPort *oport;
    OPORT(oport, sport);
    if (!SCM_INTP(sval)
        || ((vv = SCM_INT_VALUE(sval)) < 0)
        || (vv >= 65536)) {
        Scm_Error("argument out of range (uint16): %S", sval);
    }
    v.val = vv;
    SWAP2();
    Scm_Putz(v.buf, 2, oport);
}

void Scm_WriteBinarySint16(ScmObj sval, ScmObj sport, Endian endian)
{
    union { char buf[2]; short val; } v;
    long vv = 0;
    ScmPort *oport;
    OPORT(oport, sport);
    if (!SCM_INTP(sval)
        || ((vv = SCM_INT_VALUE(sval)) < -32768)
        || (vv >= 32768)) {
        Scm_Error("argument out of range (sint16): %S", sval);
    }
    v.val = vv;
    SWAP2();
    Scm_Putz(v.buf, 2, oport);
}

void Scm_WriteBinaryUint32(ScmObj sval, ScmObj sport, Endian endian)
{
    union { char buf[4]; ScmUInt32 val; } v;
    ScmPort *oport;
    OPORT(oport, sport);
    v.val = Scm_GetIntegerU32Clamp(sval, FALSE, FALSE);
    SWAP4();
    Scm_Putz(v.buf, 4, oport);
}

void Scm_WriteBinarySint32(ScmObj sval, ScmObj sport, Endian endian)
{
    union { char buf[4]; ScmInt32 val; } v;
    ScmPort *oport;
    OPORT(oport, sport);
    v.val = Scm_GetInteger32Clamp(sval, FALSE, FALSE);
    SWAP4();
    Scm_Putz(v.buf, 4, oport);
}

void Scm_WriteBinaryUint64(ScmObj sval, ScmObj sport, Endian endian)
{
    union { char buf[8]; ScmUInt64 val; } v;
    ScmPort *oport;
    OPORT(oport, sport);
    v.val = Scm_GetIntegerU64Clamp(sval, FALSE, FALSE);
    SWAP8();
    Scm_Putz(v.buf, 8, oport);
}

void Scm_WriteBinarySint64(ScmObj sval, ScmObj sport, Endian endian)
{
    union { char buf[8]; ScmInt64 val; } v;
    ScmPort *oport;
    OPORT(oport, sport);
    v.val = Scm_GetInteger64Clamp(sval, FALSE, FALSE);
    SWAP8();
    Scm_Putz(v.buf, 8, oport);
}

void Scm_WriteBinaryHalfFloat(ScmObj sval, ScmObj sport, Endian endian)
{
    union { char buf[2]; ScmHalfFloat val; } v;
    ScmPort *oport;
    OPORT(oport, sport);
    v.val = Scm_DoubleToHalf(Scm_GetDouble(sval));
    SWAP2();
    Scm_Putz(v.buf, 2, oport);
}

void Scm_WriteBinaryFloat(ScmObj sval, ScmObj sport, Endian endian)
{
    union { char buf[4]; float val; } v;
    ScmPort *oport;
    OPORT(oport, sport);
    v.val = (float)Scm_GetDouble(sval);
    SWAP4();
    Scm_Putz(v.buf, 4, oport);
}

void Scm_WriteBinaryDouble(ScmObj sval, ScmObj sport, Endian endian)
{
    union { char buf[8]; double val; } v;
    ScmPort *oport;
    OPORT(oport, sport);
    v.val = Scm_GetDouble(sval);
    SWAPD();
    Scm_Putz(v.buf, 8, oport);
}


/*
 * Init
 */


extern void Scm_Init_binarylib(ScmModule *mod);

void Scm_Init_binary(void)
{
    ScmModule *mod_io = SCM_FIND_MODULE("binary.io", SCM_FIND_MODULE_CREATE);
    SCM_INIT_EXTENSION(binary);
    Scm_Init_binarylib(mod_io);
}
