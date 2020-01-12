/*
 * gauche/endian.h - handling endian argument
 *
 *   Copyright (c) 2009-2019  Shiro Kawai  <shiro@acm.org>
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

/*
 * Byte swapping code pieces shared in binary.io and gauche.uvector.
 * This is not included via gauche.h.  Users should explicitly include it.
 */

/* Gauche use big-endian / little-endian.   scheme.bytevector requires
   big / little.   We recognize both.  */
#define SCM_IS_BE(endian)                               \
    (SCM_EQ(SCM_OBJ(endian), SCM_SYM_BIG_ENDIAN)        \
     || (SCM_EQ(SCM_OBJ(endian), SCM_SYM_BIG)))
#define SCM_IS_LE(endian)                               \
    (SCM_EQ(SCM_OBJ(endian), SCM_SYM_LITTLE_ENDIAN)     \
     || SCM_EQ(SCM_OBJ(endian), SCM_SYM_LITTLE))
#define SCM_IS_ARM_LE(endian)                           \
    (SCM_EQ(SCM_OBJ(endian), SCM_SYM_ARM_LITTLE_ENDIAN))

#define SCM_CHECK_ENDIAN(endian) \
    do { if (endian == NULL) endian = SCM_SYMBOL(Scm_DefaultEndian()); \
    } while (0)

#endif /*GAUCHE_BYTES_INLINE_H*/
