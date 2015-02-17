/*
 * binary.h - Binary I/O routines
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
#include <gauche/priv/builtin-syms.h>

extern ScmObj Scm_ReadBinaryU8(ScmPort *iport, ScmSymbol *e);
extern ScmObj Scm_ReadBinaryU16(ScmPort *iport, ScmSymbol *e);
extern ScmObj Scm_ReadBinaryU32(ScmPort *iport, ScmSymbol *e);
extern ScmObj Scm_ReadBinaryU64(ScmPort *iport, ScmSymbol *e);
extern ScmObj Scm_ReadBinaryS8(ScmPort *iport, ScmSymbol *e);
extern ScmObj Scm_ReadBinaryS16(ScmPort *iport, ScmSymbol *e);
extern ScmObj Scm_ReadBinaryS32(ScmPort *iport, ScmSymbol *e);
extern ScmObj Scm_ReadBinaryS64(ScmPort *iport, ScmSymbol *e);
extern ScmObj Scm_ReadBinaryF16(ScmPort *iport, ScmSymbol *e);
extern ScmObj Scm_ReadBinaryF32(ScmPort *iport, ScmSymbol *e);
extern ScmObj Scm_ReadBinaryF64(ScmPort *iport, ScmSymbol *e);

extern void Scm_WriteBinaryU8(ScmObj val, ScmPort *oport, ScmSymbol *e);
extern void Scm_WriteBinaryU16(ScmObj val, ScmPort *oport, ScmSymbol *e);
extern void Scm_WriteBinaryU32(ScmObj val, ScmPort *oport, ScmSymbol *e);
extern void Scm_WriteBinaryU64(ScmObj val, ScmPort *oport, ScmSymbol *e);
extern void Scm_WriteBinaryS8(ScmObj val, ScmPort *oport, ScmSymbol *e);
extern void Scm_WriteBinaryS16(ScmObj val, ScmPort *oport, ScmSymbol *e);
extern void Scm_WriteBinaryS32(ScmObj val, ScmPort *oport, ScmSymbol *e);
extern void Scm_WriteBinaryS64(ScmObj val, ScmPort *oport, ScmSymbol *e);
extern void Scm_WriteBinaryF16(ScmObj val, ScmPort *oport, ScmSymbol *e);
extern void Scm_WriteBinaryF32(ScmObj val, ScmPort *oport, ScmSymbol *e);
extern void Scm_WriteBinaryF64(ScmObj val, ScmPort *oport, ScmSymbol *e);

extern ScmObj Scm_GetBinaryU8(ScmUVector *uv, int off, ScmSymbol *e);
extern ScmObj Scm_GetBinaryU16(ScmUVector *uv, int off, ScmSymbol *e);
extern ScmObj Scm_GetBinaryU32(ScmUVector *uv, int off, ScmSymbol *e);
extern ScmObj Scm_GetBinaryU64(ScmUVector *uv, int off, ScmSymbol *e);
extern ScmObj Scm_GetBinaryS8(ScmUVector *uv, int off, ScmSymbol *e);
extern ScmObj Scm_GetBinaryS16(ScmUVector *uv, int off, ScmSymbol *e);
extern ScmObj Scm_GetBinaryS32(ScmUVector *uv, int off, ScmSymbol *e);
extern ScmObj Scm_GetBinaryS64(ScmUVector *uv, int off, ScmSymbol *e);
extern ScmObj Scm_GetBinaryF16(ScmUVector *uv, int off, ScmSymbol *e);
extern ScmObj Scm_GetBinaryF32(ScmUVector *uv, int off, ScmSymbol *e);
extern ScmObj Scm_GetBinaryF64(ScmUVector *uv, int off, ScmSymbol *e);

extern void Scm_PutBinaryU8(ScmUVector *uv, int off, ScmObj v, ScmSymbol *e);
extern void Scm_PutBinaryU16(ScmUVector *uv, int off, ScmObj v, ScmSymbol *e);
extern void Scm_PutBinaryU32(ScmUVector *uv, int off, ScmObj v, ScmSymbol *e);
extern void Scm_PutBinaryU64(ScmUVector *uv, int off, ScmObj v, ScmSymbol *e);
extern void Scm_PutBinaryS8(ScmUVector *uv, int off, ScmObj v, ScmSymbol *e);
extern void Scm_PutBinaryS16(ScmUVector *uv, int off, ScmObj v, ScmSymbol *e);
extern void Scm_PutBinaryS32(ScmUVector *uv, int off, ScmObj v, ScmSymbol *e);
extern void Scm_PutBinaryS64(ScmUVector *uv, int off, ScmObj v, ScmSymbol *e);
extern void Scm_PutBinaryF16(ScmUVector *uv, int off, ScmObj v, ScmSymbol *e);
extern void Scm_PutBinaryF32(ScmUVector *uv, int off, ScmObj v, ScmSymbol *e);
extern void Scm_PutBinaryF64(ScmUVector *uv, int off, ScmObj v, ScmSymbol *e);
