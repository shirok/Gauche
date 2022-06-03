/*
 * char_utf8.h - UTF8 encoding interface
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

#else  /* !SCM_CHAR_ENCODING_BODY */
/*==================================================================
 * This part is included in char.c
 */

/* Array of character encoding names, recognizable by iconv, that are
   compatible with this native encoding. */
static const char *supportedCharacterEncodings[] = {
    "UTF-8",
    "ISO-10646/UTF-8",
    "UTF8",
    NULL
};

char Scm_CharSizeTable[256] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 0x */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 1x */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 2x */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 3x */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 4x */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 5x */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 6x */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 7x */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 8x */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 9x */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* ax */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* bx */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /* cx */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /* dx */
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /* ex */
    3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 0, 0  /* fx */
};

ScmChar Scm_CharUtf8Getc(const unsigned char *cp)
{
    ScmChar ch;
    unsigned char *ucp = (unsigned char *)cp;
    unsigned char first = *ucp++;
    if (first < 0x80) { return first; }
    else if (first < 0xc0) { return SCM_CHAR_INVALID; }
    else if (first < 0xe0) {
        ch = first&0x1f;
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (ch < 0x80) return SCM_CHAR_INVALID;
    }
    else if (first < 0xf0) {
        ch = first&0x0f;
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (ch < 0x800) return SCM_CHAR_INVALID;
    }
    else if (first < 0xf8) {
        ch = first&0x07;
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (ch < 0x10000) return SCM_CHAR_INVALID;
    }
    else if (first < 0xfc) {
        ch = first&0x03;
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (ch < 0x200000) return SCM_CHAR_INVALID;
    }
    else if (first < 0xfe) {
        ch = first&0x01;
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (*ucp < 0x80 || *ucp >= 0xc0) return SCM_CHAR_INVALID;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (ch < 0x4000000) return SCM_CHAR_INVALID;
    }
    else {
        return SCM_CHAR_INVALID;
    }
    return ch;
}

void Scm_CharUtf8Putc(unsigned char *cp, ScmChar ch)
{
    if (ch < 0x80) {
        *cp = (u_char)ch;
    }
    else if (ch < 0x800) {
        *cp++ = (u_char)((ch>>6)&0x1f) | 0xc0;
        *cp = (u_char)(ch&0x3f) | 0x80;
    }
    else if (ch < 0x10000) {
        *cp++ = (u_char)((ch>>12)&0x0f) | 0xe0;
        *cp++ = (u_char)((ch>>6)&0x3f) | 0x80;
        *cp = (u_char)(ch&0x3f) | 0x80;
    }
    else if (ch < 0x200000) {
        *cp++ = (u_char)((ch>>18)&0x07) | 0xf0;
        *cp++ = (u_char)((ch>>12)&0x3f) | 0x80;
        *cp++ = (u_char)((ch>>6)&0x3f) | 0x80;
        *cp = (u_char)(ch&0x3f) | 0x80;
    }
    else if (ch < 0x4000000) {
        *cp++ = (u_char)((ch>>24)&0x03) | 0xf8;
        *cp++ = (u_char)((ch>>18)&0x3f) | 0x80;
        *cp++ = (u_char)((ch>>12)&0x3f) | 0x80;
        *cp++ = (u_char)((ch>>6)&0x3f) | 0x80;
        *cp = (u_char)(ch&0x3f) | 0x80;
    } else {
        *cp++ = (u_char)((ch>>30)&0x1) | 0xfc;
        *cp++ = (u_char)((ch>>24)&0x3f) | 0x80;
        *cp++ = (u_char)((ch>>18)&0x3f) | 0x80;
        *cp++ = (u_char)((ch>>12)&0x3f) | 0x80;
        *cp++ = (u_char)((ch>>6)&0x3f) | 0x80;
        *cp++ = (u_char)(ch&0x3f) | 0x80;
    }
}

int Scm__CharIsExtraWhiteSpace(ScmChar c, int intraline)
{
    if (c < 0x2000) {
        return (c == 0x00a0         /* Zs NO-BREAK SPACE */
                || c == 0x1680);    /* Zs OGHAM SPACE MARK */
    } else if (c <= 0x200a) {
        /* 0x2000 - 0x200a are all Zs's */
        return TRUE;
    } else {
        return (c == 0x3000         /* Zs IDEOGRAPHIC SPACE */
                || c == 0x202f      /* Zs NARROW NO-BREAK SPACE */
                || c == 0x205f      /* Zs MEDIUM MATHEMATICAL SPACE */
                || (!intraline
                    && (c == 0x2029        /* Zp PARAGRAPH SEPARATOR */
                        || c == 0x2028))); /* Zl LINE SEPARATOR */
    }
}

/*
 * Lookup character category.  The tables are in char_attr.c, automatically
 * generated by gen-unicode.scm.
 */
static inline unsigned char Scm__LookupCharCategory(ScmChar ch)
{
    if (ch == SCM_CHAR_INVALID || ch >= 0x10ffff) {
        return SCM_CHAR_CATEGORY_Cn;
    } else if (ch < 0x20000) {
        return ucs_general_category_00000[ch];
    } else {
        return ucs_general_category_20000(ch);
    }
}

#endif /* !SCM_CHAR_ENCODING_BODY */
