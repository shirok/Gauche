/*
 * gauche/bytes_inline.h - Some speed-sensitive byte-swapping routines
 *
 *   Copyright (c) 2009-2015  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_BYTES_INLINE_H
#define GAUCHE_BYTES_INLINE_H

/*
 * Byte swapping code pieces shared in binary.io and gauche.uvector.
 * This is not meant for general use.   We don't worry about name conflicts.
 */

#define IS_BE(endian)     (SCM_EQ(SCM_OBJ(endian), SCM_SYM_BIG_ENDIAN))
#define IS_LE(endian)     (SCM_EQ(SCM_OBJ(endian), SCM_SYM_LITTLE_ENDIAN))
#define IS_ARM_LE(endian) (SCM_EQ(SCM_OBJ(endian), SCM_SYM_ARM_LITTLE_ENDIAN))

#if WORDS_BIGENDIAN
#define SWAP_REQUIRED(endian)   (!IS_BE(endian))
#else  /*!WORDS_BIGENDIAN.  Covers both little-endian and arm-little-endian. */
#define SWAP_REQUIRED(endian)   IS_BE(endian)
#endif

#define CHECK_ENDIAN(endian) \
    do { if (endian == NULL) endian = SCM_SYMBOL(Scm_DefaultEndian()); \
    } while (0)

/*
 * Swapping macros.   They can be used both ways (native <-> external)
 */

typedef union { char buf[2]; short val; }        swap_s16_t;
typedef union { char buf[2]; u_short val; }      swap_u16_t;
typedef union { char buf[4]; ScmInt32 val; }     swap_s32_t;
typedef union { char buf[4]; ScmUInt32 val; }    swap_u32_t;
typedef union { char buf[8]; ScmInt64 val; }     swap_s64_t;
typedef union { char buf[8]; ScmUInt64 val; }    swap_u64_t;

typedef union { char buf[2]; ScmHalfFloat val; } swap_f16_t;
typedef union { char buf[4]; float val; }        swap_f32_t;
typedef union { char buf[8]; double val; }       swap_f64_t;

#define CSWAP(buf, tmp, n, m) (tmp=buf[n], buf[n]=buf[m], buf[m]=tmp)

/* In the following macros, 'v' is of type one of the suitable swap_*_t. */

#define SWAP_2(v) \
    do { char tmp; CSWAP(v.buf, tmp, 0, 1); } while (0)

#define SWAP_4(v)                                               \
    do { char tmp;                                              \
        CSWAP(v.buf, tmp, 0, 3); CSWAP(v.buf, tmp, 1, 2);       \
    } while (0)

#define SWAP_8(v)                                               \
    do { char tmp;                                              \
        CSWAP(v.buf, tmp, 0, 7); CSWAP(v.buf, tmp, 1, 6);       \
        CSWAP(v.buf, tmp, 2, 5); CSWAP(v.buf, tmp, 3, 4);       \
    } while (0)

/* Swapping for double float is a bit tricky for ARM_LE case:
     BE<->ARM : [01234567] -> [32107654]
     LE<->ARM : [01234567] -> [45670123]
*/
#define SWAP_ARM2BE(v)                                          \
    do { char tmp;                                              \
        CSWAP(v.buf, tmp, 0, 3); CSWAP(v.buf, tmp, 1, 2);       \
        CSWAP(v.buf, tmp, 4, 7); CSWAP(v.buf, tmp, 5, 6);       \
    } while (0)

#define SWAP_ARM2LE(v)                                          \
    do { char tmp;                                              \
        CSWAP(v.buf, tmp, 0, 4); CSWAP(v.buf, tmp, 1, 5);       \
        CSWAP(v.buf, tmp, 2, 6); CSWAP(v.buf, tmp, 3, 7);       \
    } while (0)


#endif /*GAUCHE_BYTES_INLINE_H*/
