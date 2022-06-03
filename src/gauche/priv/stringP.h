/*
 * stringP.h - String private API
 *
 *   Copyright (c) 2020-2022  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PRIV_STRINGP_H
#define GAUCHE_PRIV_STRINGP_H

/* String cursor */

/* If the offset is less than or equal to this, we use small cursor that
   fits in ScmObj.  Otherwise we allocate ScmStringCursorLarge. */
#if SIZEOF_LONG == 4
#define SCM_STRING_CURSOR_SMALL_OFFSET_MAX  ((1L<<24)-1)
#else
#define SCM_STRING_CURSOR_SMALL_OFFSET_MAX  ((1L<<56)-1)
#endif

struct ScmStringCursorLargeRec {
    SCM_HEADER;
    const char *start;
    ScmSmallInt offset;	 /* in bytes, relative to string body start
                            offset is non-negative. */
};

#define SCM_STRING_CURSOR_FITS_SMALL_P(off) \
    ((off) <= SCM_STRING_CURSOR_SMALL_OFFSET_MAX)

#define SCM_STRING_CURSOR_LARGE_P(obj) \
    SCM_XTYPEP(obj, SCM_CLASS_STRING_CURSOR_LARGE)
#define SCM_STRING_CURSOR_LARGE(obj)       ((ScmStringCursorLarge*)obj)
#define SCM_STRING_CURSOR_LARGE_OFFSET(obj)      \
    (SCM_STRING_CURSOR_LARGE(obj)->offset)
#define SCM_STRING_CURSOR_LARGE_POINTER(sb, obj) \
    (SCM_STRING_BODY_START(sb) + SCM_STRING_CURSOR_LARGE_OFFSET(obj))
#define SCM_STRING_CURSOR_LARGE_START(obj)       \
    (SCM_STRING_CURSOR_LARGE(obj)->start)

#define SCM_MAKE_STRING_CURSOR_SMALL(obj) \
    SCM_OBJ(((uintptr_t)(obj) << 8) + 0x1b)
#define SCM_STRING_CURSOR_SMALL_P(obj)        (SCM_TAG8(obj) == 0x1b)
#define SCM_STRING_CURSOR_SMALL_OFFSET(obj) \
    ((ScmSmallInt)(SCM_WORD(obj) >> 8))
#define SCM_STRING_CURSOR_SMALL_POINTER(sb, obj) \
    (SCM_STRING_BODY_START(sb) + SCM_STRING_CURSOR_SMALL_OFFSET(obj))

#define SCM_STRING_CURSOR_P(obj) \
    (SCM_STRING_CURSOR_SMALL_P(obj)||SCM_STRING_CURSOR_LARGE_P(obj))

/* String index
 *
 *  Attaching an index to a StringBody allows O(1) random access.
 *  We don't worry too much about the constant factor; space-efficiency
 *  is also a concern, for indexing is more important for long strings.
 */

typedef union ScmStringIndexRec {
    /* We use element size according to the length of string body.
       The first entry encodes the entry size and shift value.
       (We might try index24 and/or index48.)

       RRSSS000 - index8
       RRSSS001 - index16
       RRSSS011 - index32
       RRSSS101 - index64

       'SSS' encodes a value S, where an index is created for
       every 1<<(S+1) characters.

       'RR' is reserved.  Currently 00.

       This octet is repeated to fill the first entry.  So, if the index
       is index32 and S is 6, the first word is actually #x32323232.
       This allows us to read the first byte to find out the actual
       index type, regardless of endianness.

       The index array has the signature in the first element, and the total
       lenght of the index array in the second.  The actual index array
       begins from the third element.  Given the character index N,
       indexX[(N>>shift)+2] contains the byte position of the character.
     */
    const uint8_t  signature;
    const uint8_t  index8[1];
    const uint16_t index16[1];
    const uint32_t index32[1];
    const uint64_t index64[1];
} ScmStringIndex;

enum {
    STRING_INDEX8 = 0,
    STRING_INDEX16 = 1,
    STRING_INDEX32 = 3,
    STRING_INDEX64 = 5
};

#define STRING_INDEX(p)       ((ScmStringIndex*)(p))
#define STRING_INDEX_TYPE(p)  (STRING_INDEX(p)->signature & 0x07)
#define STRING_INDEX_SHIFT(p) (((STRING_INDEX(p)->signature>>3)&0x07)+1)
#define STRING_INDEX_INTERVAL(p) (1L<<STRING_INDEX_SHIFT(p))

#define STRING_INDEX_SIGNATURE(s, t)  (((((s)-1)&0x7)<<3)|((t)&0x07))

#define SCM_STRING_BODY_HAS_INDEX(sb) ((sb)->index != NULL)

SCM_EXTERN void Scm_StringBodyBuildIndex(ScmStringBody *sb);
SCM_EXTERN void Scm_StringBodyIndexDump(const ScmStringBody *sb, ScmPort *port);

#endif /*GAUCHE_PRIV_STRINGP_H*/
