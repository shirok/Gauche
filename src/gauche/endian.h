/*
 * gauche/endian.h - handling endian argument
 *
 *   Copyright (c) 2009-2022  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_ENDIAN_H
#define GAUCHE_ENDIAN_H

/* Byte swapping macros are in gauche/priv/bytesP.h.
   The actual code is current in number.c for now. */

SCM_EXTERN ScmObj Scm_NativeEndian(void);
SCM_EXTERN ScmObj Scm_DefaultEndian(void);
SCM_EXTERN void   Scm_SetDefaultEndian(ScmObj endian);

#define SCM_CHECK_ENDIAN(endian)                                       \
    do { if (endian == NULL) endian = SCM_SYMBOL(Scm_DefaultEndian()); \
    } while (0)

/* Check ENDIAN symbol */
SCM_EXTERN int  Scm_IsBE(ScmObj endian);
SCM_EXTERN int  Scm_IsLE(ScmObj endian);
SCM_EXTERN int  Scm_IsArmLE(ScmObj endian);

#endif /*GAUCHE_ENDIAN_H*/
