/*
 * binaryio.h - Binary I/O routines
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
 *  $Id: binary.h,v 1.3 2007-02-17 13:03:39 shirok Exp $
 */

#include <gauche.h>
#include <gauche/builtin-syms.h>

/* endian check */
typedef enum {
    SCM_BE,
    SCM_LE
} Endian;

#if WORDS_BIGENDIAN
#define SWAP_REQUIRED(endian)  ((endian)!=SCM_BE)
#else  /*!WORDS_BIGENDIAN*/
#define SWAP_REQUIRED(endian)  ((endian)!=SCM_LE)
#endif
 
extern ScmObj Scm_ReadBinaryUint8(ScmObj port, Endian endian);
extern ScmObj Scm_ReadBinaryUint16(ScmObj port, Endian endian);
extern ScmObj Scm_ReadBinaryUint32(ScmObj port, Endian endian);
extern ScmObj Scm_ReadBinaryUint64(ScmObj port, Endian endian);
extern ScmObj Scm_ReadBinarySint8(ScmObj port, Endian endian);
extern ScmObj Scm_ReadBinarySint16(ScmObj port, Endian endian);
extern ScmObj Scm_ReadBinarySint32(ScmObj port, Endian endian);
extern ScmObj Scm_ReadBinarySint64(ScmObj port, Endian endian);

extern ScmObj Scm_ReadBinaryHalfFloat(ScmObj port, Endian endian);
extern ScmObj Scm_ReadBinaryFloat(ScmObj port, Endian endian);
extern ScmObj Scm_ReadBinaryDouble(ScmObj port, Endian endian);

extern void Scm_WriteBinaryUint8(ScmObj val, ScmObj port, Endian endian);
extern void Scm_WriteBinaryUint16(ScmObj val, ScmObj port, Endian endian);
extern void Scm_WriteBinaryUint32(ScmObj val, ScmObj port, Endian endian);
extern void Scm_WriteBinaryUint64(ScmObj val, ScmObj port, Endian endian);
extern void Scm_WriteBinarySint8(ScmObj val, ScmObj port, Endian endian);
extern void Scm_WriteBinarySint16(ScmObj val, ScmObj port, Endian endian);
extern void Scm_WriteBinarySint32(ScmObj val, ScmObj port, Endian endian);
extern void Scm_WriteBinarySint64(ScmObj val, ScmObj port, Endian endian);

extern void Scm_WriteBinaryHalfFloat(ScmObj val, ScmObj port, Endian endian);
extern void Scm_WriteBinaryFloat(ScmObj val, ScmObj port, Endian endian);
extern void Scm_WriteBinaryDouble(ScmObj val, ScmObj port, Endian endian);


