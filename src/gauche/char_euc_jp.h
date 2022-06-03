/*
 * char-euc-jp.h
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
#define SCM_CHAR_ENCODING_NAME "euc-jp"

/* Given first byte of the multibyte character, returns # of
 * bytes that follows, i.e. if the byte consists a single-byte
 * character, it returns 0; if the byte is the first byte of
 * two-byte character, it returns 1.   It may return -1 if
 * the given byte can't be a valid first byte of multibyte characters.
 */
#define SCM_CHAR_NFOLLOWS(ch)                           \
    ((((unsigned char)(ch)) >= 0x80) ?                  \
     ((((unsigned char)(ch)) == 0x8f) ? 2 : 1) : 0)

/* Given wide character CH, returns # of bytes used when CH is
 * encoded in multibyte string.
 */
#define SCM_CHAR_NBYTES(ch) \
    (((ch) >= 0x80) ? (((ch) >= 0x10000)? 3 : 2) : 1)

/* Maximun # of multibyte character */
#define SCM_CHAR_MAX_BYTES     3

/* From a multibyte string pointed by const char *cp, extract a character
 * and store it in ScmChar ch.  If cp doesn't point to valid multibyte
 * character, store SCM_CHAR_INVALID to ch.  cp is not modified.
 */
/* The tests aren't "exact" in the sense that it accepts not-quite
   EUC-JP sequence, but I hope they at least exclude the 'harmful'
   sequences */
#define SCM_CHAR_GET(cp, ch)                                    \
    do {                                                        \
        if (((ch) = (unsigned char)*(cp)) >= 0x80) {            \
            if ((ch) == 0x8f &&                                 \
                (unsigned char)(cp)[1] >= 0xa1 &&               \
                (unsigned char)(cp)[2] >= 0xa1) {               \
                 (ch) = ((ch) << 16)                            \
                     + ((unsigned char)(cp)[1] << 8)            \
                     + (unsigned char)(cp)[2];                  \
            } else if ((unsigned char)(cp)[1] >= 0xa1) {        \
                (ch) = ((ch) << 8) + (unsigned char)(cp)[1];    \
            } else {                                            \
                (ch) = SCM_CHAR_INVALID;                        \
            }                                                   \
        }                                                       \
    } while (0)

/* Convert a character CH to multibyte form and put it to the buffer
 * starting from char *cp.  You can assume the buffer has enough length
 * to contain the multibyte char.   cp is not modified.
 */
#define SCM_CHAR_PUT(cp, ch)                    \
    do {                                        \
        if ((ch) > 0xff) {                      \
            if ((ch) > 0xffff) {                \
                (cp)[0] = ((ch) >> 16) & 0xff;  \
                (cp)[1] = ((ch) >> 8) & 0xff;   \
                (cp)[2] = (ch) & 0xff;          \
            } else {                            \
                (cp)[0] = (ch >> 8) & 0xff;     \
                (cp)[1] = ch & 0xff;            \
            }                                   \
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
    ((result) = Scm_CharBackwardEUC(cp, start))

SCM_EXTERN const char *Scm_CharBackwardEUC(const char *cp, const char *start);

/* C is an ScmChar > 0x80.  Returns true if C is a whitespace character. */
#define SCM_CHAR_EXTRA_WHITESPACE(c) \
    (((c) == 0xa1a1)                       /* zenkaku space */  \
     || ((c) == 0xa9a2))                   /* no-break space */

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
    "EUC-JP",
    "EUCJP",
    NULL
};

/* An ad-hoc algorithm to return a ptr to the previous character
   boundary.  Note that it is pretty permissive---the string
   can possibly include a illegal encoding. */

const char *Scm_CharBackwardEUC(const char *cp, const char *start)
{
    const unsigned char *t;
    /* be careful not to access beyond the beginning of the string */
    switch (cp - start) {
    default:
        t = (unsigned char*)(cp-3);
        if (t[0] == 0x8f && t[1] >= 0x80 && t[2] >= 0x80) {
            return (const char *)t;
        }
        /*FALLTHROUGH*/
    case 2:
        t = (unsigned char*)(cp-2);
        if (t[0] >= 0x80 && t[1] >= 0x80) {
            return (const char*)t;
        }
        /*FALLTHROUGH*/
    case 1:
        t = (unsigned char*)(cp-1);
        if (t[0] < 0x80) {
            return (const char*)t;
        }
    }
    return NULL;
}

/*
 * Lookup character category.  The tables are in char_attr.c, automatically
 * generated by gen-unicode.scm.
 */
static inline unsigned char Scm__LookupCharCategory(ScmChar ch)
{
    if (ch == SCM_CHAR_INVALID) return SCM_CHAR_CATEGORY_Cn;
    if (ch < 0x80) return eucjp_general_category_G0[ch];
    else if (ch < 0x8ea1) return SCM_CHAR_CATEGORY_Cn;
    else if (ch < 0x8ee0) return eucjp_general_category_G2[ch-0x8ea1];
    else if (ch < 0xa1a1) return SCM_CHAR_CATEGORY_Cn;
    else if (ch < 0xfefe) {
        unsigned char b0 = (ch >> 8) - 0xa1;
        unsigned char b1 = (ch & 0xff) - 0xa1;
        if (b0 >= (0xff-0xa1)) return SCM_CHAR_CATEGORY_Cn;
        if (b1 >= (0xff-0xa1)) return SCM_CHAR_CATEGORY_Cn;
        return eucjp_general_category_G1[b0 * (0xff-0xa1) + b1];
    } else if (ch < 0x8fa1a1) {
        unsigned char b0 = ((ch >> 8) & 0xff) - 0xa1;
        unsigned char b1 = (ch & 0xff) - 0xa1;
        if (b0 >= (0xff-0xa1)) return SCM_CHAR_CATEGORY_Cn;
        if (b1 >= (0xff-0xa1)) return SCM_CHAR_CATEGORY_Cn;
        return eucjp_general_category_G3[b0 * (0xff-0xa1) + b1];
    } else return SCM_CHAR_CATEGORY_Cn;
}

/*
 * Returns true if the character isn't supported in Unicode.
 */
static int Scm__CharInUnicodeP(ScmChar ch)
{
    if (ch < 0xa4f7 || ch > 0xabe6) return TRUE;
    if (ch < 0xa600) {
        if (ch <= 0xa4fb || (ch >= 0xa5f7 && ch <= 0xa5fe)) return FALSE;
        else return TRUE;
    } else {
        if (ch == 0xa6f8 || ch == 0xabc4 || (ch >= 0xabc8 && ch <= 0xabcf)
            || ch == 0xabe5 || ch == 0xabe6) return FALSE;
        else return TRUE;
    }
}

#endif /* !SCM_CHAR_ENCODING_BODY */
