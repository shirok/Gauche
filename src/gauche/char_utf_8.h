/*
 * char_utf8.h - UTF8 encoding interface
 *
 *   Copyright (c) 2000-2025  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_CHAR_UTF_8_H
#define GAUCHE_CHAR_UTF_8_H

/* The name of the encoding.  Scheme procedure
 * gauche-character-encoding returns a symbol with this name.
 */
#define SCM_CHAR_ENCODING_NAME "utf-8"

SCM_EXTERN char Scm_CharSizeTable[];
SCM_EXTERN ScmChar Scm_CharUtf8Getc(const unsigned char *);
SCM_EXTERN void Scm_CharUtf8Putc(unsigned char *, ScmChar);

/* Given first byte of the multibyte character, returns # of
 * bytes that follows, i.e. if the byte consists a single-byte
 * character, it returns 0; if the byte is the first byte of
 * two-byte character, it returns 1.   It may return -1 if
 * the given byte can't be a valid first byte of multibyte characters.
 */
#define SCM_CHAR_NFOLLOWS(ch) ((int)Scm_CharSizeTable[(unsigned char)(ch)])

/* Given wide character CH, returns # of bytes used when CH is
 * encoded in multibyte string.
 */
#define SCM_CHAR_NBYTES(ch)                     \
    (((ch) < 0x80) ? 1 :                        \
     (((ch) < 0x800) ? 2 :                      \
      (((ch) < 0x10000) ? 3 :                   \
       (((ch) < 0x200000) ? 4 :                 \
        (((ch) < 0x4000000) ? 5 : 6)))))

/* Maximun # of multibyte character */
#define SCM_CHAR_MAX_BYTES     6

/* From a multibyte string pointed by const char *cp, extract a character
 * and store it in ScmChar ch.  If cp doesn't point to valid multibyte
 * character, store SCM_CHAR_INVALID to ch.  cp is not modified.
 */
#define SCM_CHAR_GET(cp, ch)                                    \
    do {                                                        \
        if (((ch) = (unsigned char)*(cp)) >= 0x80) {            \
            (ch) = Scm_CharUtf8Getc((unsigned char*)cp);        \
        }                                                       \
    } while (0)

/* Convert a character CH to multibyte form and put it to the buffer
 * starting from char *cp.  You can assume the buffer has enough length
 * to contain the multibyte char.   cp is not modified.
 */
#define SCM_CHAR_PUT(cp, ch)                            \
    do {                                                \
        if (ch >= 0x80) {                               \
            Scm_CharUtf8Putc((unsigned char*)cp, ch);   \
        } else {                                        \
            *(cp) = (unsigned char)(ch);                \
        }                                               \
    } while (0)

/* const char *cp points to a multibyte string.  Set const char *result
 * to point to the previous character of the one cp points to.
 * const char *start points to the beginning of the buffer.
 * result is set to NULL if there's no valid multibyte char found
 * just before cp.   cp and start is not modified.
 */
#define SCM_CHAR_BACKWARD(cp, start, result)                    \
    do {                                                        \
        switch ((cp) - (start)) {                               \
        default:                                                \
            (result) = (cp) - 6;                                \
            if (SCM_CHAR_NFOLLOWS(*(result)) == 5) break;       \
            /* FALLTHROUGH */                                   \
        case 5:                                                 \
            (result) = (cp) - 5;                                \
            if (SCM_CHAR_NFOLLOWS(*(result)) == 4) break;       \
            /* FALLTHROUGH */                                   \
        case 4:                                                 \
            (result) = (cp) - 4;                                \
            if (SCM_CHAR_NFOLLOWS(*(result)) == 3) break;       \
            /* FALLTHROUGH */                                   \
        case 3:                                                 \
            (result) = (cp) - 3;                                \
            if (SCM_CHAR_NFOLLOWS(*(result)) == 2) break;       \
            /* FALLTHROUGH */                                   \
        case 2:                                                 \
            (result) = (cp) - 2;                                \
            if (SCM_CHAR_NFOLLOWS(*(result)) == 1) break;       \
            /* FALLTHROUGH */                                   \
        case 1:                                                 \
            (result) = (cp) - 1;                                \
            if (SCM_CHAR_NFOLLOWS(*(result)) == 0) break;       \
            (result) = NULL;                                    \
        }                                                       \
    } while (0)

/* C is an ScmChar > 0x80.  Returns true if C is a whitespace character. */
#define SCM_CHAR_EXTRA_WHITESPACE(c) \
    ((c) <= 0x3000 && Scm__CharIsExtraWhiteSpace(c, FALSE))
/* Like SCM_CHAR_EXTRA_WHITESPACE, but excludes Zl and Zp.
   See R6RS on the intraline whitespaces. */
#define SCM_CHAR_EXTRA_WHITESPACE_INTRALINE(c) \
    ((c) <= 0x3000 && Scm__CharIsExtraWhiteSpace(c, TRUE))

extern int Scm__CharIsExtraWhiteSpace(ScmChar c, int intraline);

#endif /*GAUCHE_CHAR_UTF_8_H*/
