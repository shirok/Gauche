/*
 * char-sjis.h
 *
 *   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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

#ifndef SCM_CHAR_ENCODING_BODY
/*===============================================================
 * Header part
 */

/* The name of the encoding.  Scheme procedure
 * gauche-character-encoding returns a symbol with this name.
 */
#define SCM_CHAR_ENCODING_NAME "sjis"

/* Given first byte of the multibyte character, returns # of
 * bytes that follows, i.e. if the byte consists a single-byte
 * character, it returns 0; if the byte is the first byte of
 * two-byte character, it returns 1.   It may return -1 if
 * the given byte can't be a valid first byte of multibyte characters.
 */
#define SCM_CHAR_NFOLLOWS(byte)                   \
    (((unsigned char)(byte)) < 0x81? 0 :          \
     (((unsigned char)(byte)) < 0xa0? 1 :         \
      (((unsigned char)(byte)) < 0xe0? 0 : 1)))

/* Given wide character CH, returns # of bytes used when CH is
 * encoded in multibyte string.
 */
#define SCM_CHAR_NBYTES(ch) (((ch) > 0x0ff) ? 2 : 1)

/* Maximun # of multibyte character */
#define SCM_CHAR_MAX_BYTES     2

/* From a multibyte string pointed by const char *cp, extract a character
 * and store it in ScmChar ch.  If cp doesn't point to valid multibyte
 * character, store SCM_CHAR_INVALID to ch.  cp is not modified.
 */
#define SCM_CHAR_GET(cp, ch)                                                \
    do {                                                                    \
        (ch) = (unsigned char)*(cp);                                        \
        if ((unsigned char)(ch) >= 0x80) {                                  \
          if ((unsigned char)(ch) < 0xa0 || (unsigned char)(ch) >= 0xe0) {  \
             (ch) = (((unsigned char)(ch)) << 8) + (unsigned char)*(cp+1);  \
          }                                                                 \
        }                                                                   \
    } while (0)

/* Convert a character CH to multibyte form and put it to the buffer
 * starting from char *cp.  You can assume the buffer has enough length
 * to contain the multibyte char.   cp is not modified.
 */
#define SCM_CHAR_PUT(cp, ch)                    \
    do {                                        \
        if ((ch) > 0xff) {                      \
            (cp)[0] = (ch >> 8) & 0xff;         \
            (cp)[1] = ch & 0xff;                \
        } else {                                \
            (cp)[0] = ch & 0xff;                \
        }                                       \
    } while (0)

/* const char *cp points to a multibyte string.  Set const char *result
 * to point to the previous character of the one cp points to.
 * const char *start points to the beginning of the buffer.
 * result is set to NULL if there's no valid multibyte char found
 * just before cp.   cp and start is not modified.
 */
#define SCM_CHAR_BACKWARD(cp, start, result)                    \
    do {                                                        \
        (result) = (cp);                                        \
        if ((result) == (start)) (result) = NULL;               \
        else if ((result) == (start) + 1) (result) = (start);   \
        else if (SCM_CHAR_NFOLLOWS(*((result)-2)) == 1) {       \
             (result) -= 2;                                     \
        } else {                                                \
             (result) -= 1;                                     \
        }                                                       \
    } while (0)

/* C is an ScmChar > 0x80.  Returns true if C is a whitespace character. */
#define SCM_CHAR_EXTRA_WHITESPACE(c) \
    (((c) == 0x8140)                       /* zenkaku space */ \
     || ((c) == 0x8541))                   /* NBSP */

/* Like SCM_CHAR_EXTRA_WHITESPACE, but excludes Zl and Zp.
   See R6RS on the intraline whitespaces. */
#define SCM_CHAR_EXTRA_WHITESPACE_INTRALINE(c) SCM_CHAR_EXTRA_WHITESPACE(c)

#else  /* !SCM_CHAR_ENCODING_BODY */
/*==================================================================
 * This part is included in char.c
 */

/* Array of character encoding names, recognizable by iconv, that are
   compatible with this native encoding. */
static const char *supportedCharacterEncodings[] = {
    "SHIFT_JIS",
    "SHIFT-JIS",
    "SHIFT_JISX0213",
    "SHIFT-JISX0213",
    "SJIS",
    NULL
};

/*
 * Lookup character category.  The tables are in char_attr.c, automatically
 * generated by gen-unicode.scm.
 */
static inline unsigned char Scm__LookupCharCategory(ScmChar ch)
{
    if (ch == SCM_CHAR_INVALID) return SCM_CHAR_CATEGORY_Cn;
    if (ch < 0x80) return sjis_general_category_00[ch];
    else if (ch < 0xa0)   return SCM_CHAR_CATEGORY_Cn;
    else if (ch < 0xe0)   return sjis_general_category_a0[ch-0xa0];
    else if (ch < 0x8040) return SCM_CHAR_CATEGORY_Cn;
    else if (ch < 0x9ffd) {
        unsigned char b0 = (ch >> 8) - 0x80;
        unsigned char b1 = (ch & 0xff) - 0x40;
        if (b0 >= (0xa0 - 0x80)) return SCM_CHAR_CATEGORY_Cn;
        if (b1 >= (0xfd - 0x40)) return SCM_CHAR_CATEGORY_Cn;
        SCM_ASSERT(0 <= b0 && b0 < (0xa0 - 0x80));
        SCM_ASSERT(0 <= b1 && b1 < (0xfd - 0x40));
        return sjis_general_category_8000[b0 * (0xfd-0x40) + b1];
    } else if (ch < 0xe000) {
        return SCM_CHAR_CATEGORY_Cn;
    } else if (ch < 0xfffd) {
        unsigned char b0 = (ch >> 8) - 0xe0;
        unsigned char b1 = (ch & 0xff) - 0x40;
        if (b0 >= (0x100 - 0xe0)) return SCM_CHAR_CATEGORY_Cn;
        if (b1 >= (0xfd - 0x40))  return SCM_CHAR_CATEGORY_Cn;
        return sjis_general_category_e000[b0 * (0xfd-0x40) + b1];
    } else return SCM_CHAR_CATEGORY_Cn;
}

/*
 * Returns true if the character isn't supported in Unicode.
 */
static int Scm__CharInUnicodeP(ScmChar ch)
{
    if (ch < 0x82f5 || ch > 0x8686) return TRUE;
    if (ch < 0x8600) {
        if (ch <= 0x82f9 || (ch >= 0x8397 && ch <= 0x839e)
            || ch == 0x83f6) return FALSE;
        else return TRUE;
    } else {
        if (ch == 0x8663 || (ch >= 0x8667 && ch <= 0x866e)
            || ch == 0x8685 || ch == 0x8686) return FALSE;
        else return TRUE;
    }
}

#endif /* !SCM_CHAR_ENCODING_BODY */
