/*
 * char_utf8.h - UTF8 encoding interface
 *
 *  Copyright(C) 2000 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, ditribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: char_utf_8.h,v 1.7 2002-02-04 09:28:40 shirok Exp $
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
SCM_EXTERN ScmChar Scm_CharUtf8Getc(const char *);
SCM_EXTERN void Scm_CharUtf8Putc(char *, ScmChar);

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
#define SCM_CHAR_GET(cp, ch)                            \
    do {                                                \
        if (((ch) = (unsigned char)*(cp)) >= 0x80) {    \
            (ch) = Scm_CharUtf8Getc(cp);                \
        }                                               \
    } while (0)

/* Convert a character CH to multibyte form and put it to the buffer
 * starting from char *cp.  You can assume the buffer has enough length
 * to contain the multibyte char.   cp is not modified.
 */
#define SCM_CHAR_PUT(cp, ch)                    \
    do {                                        \
        if (ch >= 0x80) {                       \
            Scm_CharUtf8Putc(cp, ch);           \
        } else {                                \
            *(cp) = (ch);                       \
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

ScmChar Scm_CharUtf8Getc(const char *cp)
{
    ScmChar ch;
    unsigned char *ucp = (unsigned char *)cp;
    unsigned char first = *ucp++;
    if (first < 0x80) { ch = first; }
    else if (first < 0xc0) { ch = SCM_CHAR_INVALID; }
    else if (first < 0xe0) {
        ch = first&0x1f;
        ch = (ch<<6) | (*ucp++&0x3f);
        if (ch < 0x80) ch = SCM_CHAR_INVALID;
    }
    else if (first < 0xf0) {
        ch = first&0x0f;
        ch = (ch<<6) | (*ucp++&0x3f);
        ch = (ch<<6) | (*ucp++&0x3f);
        if (ch < 0x800) ch = SCM_CHAR_INVALID;
    }
    else if (first < 0xf8) {
        ch = first&0x07;
        ch = (ch<<6) | (*ucp++&0x3f);
        ch = (ch<<6) | (*ucp++&0x3f);
        ch = (ch<<6) | (*ucp++&0x3f);
        if (ch < 0x10000) ch = SCM_CHAR_INVALID;
    }
    else if (first < 0xfc) {
        ch = first&0x03;
        ch = (ch<<6) | (*ucp++&0x3f);
        ch = (ch<<6) | (*ucp++&0x3f);
        ch = (ch<<6) | (*ucp++&0x3f);
        ch = (ch<<6) | (*ucp++&0x3f);
        if (ch < 0x200000) ch = SCM_CHAR_INVALID;
    }
    else if (first < 0xfe) {
        ch = first&0x01;
        ch = (ch<<6) | (*ucp++&0x3f);
        ch = (ch<<6) | (*ucp++&0x3f);
        ch = (ch<<6) | (*ucp++&0x3f);
        ch = (ch<<6) | (*ucp++&0x3f);
        ch = (ch<<6) | (*ucp++&0x3f);
        if (ch < 0x4000000) ch = SCM_CHAR_INVALID;
    }
    else {
        ch = SCM_CHAR_INVALID;
    }
    return ch;
}

void Scm_CharUtf8Putc(char *cp, ScmChar ch)
{
    if (ch < 0x80) {
        *cp = ch;
    }
    else if (ch < 0x800) {
        *cp++ = ((ch>>6)&0x1f) | 0xc0;
        *cp = (ch&0x3f) | 0x80;
    }
    else if (ch < 0x10000) {
        *cp++ = ((ch>>12)&0x0f) | 0xe0;
        *cp++ = ((ch>>6)&0x3f) | 0x80;
        *cp = (ch&0x3f) | 0x80;
    }
    else if (ch < 0x200000) {
        *cp++ = ((ch>>18)&0x07) | 0xf0;
        *cp++ = ((ch>>12)&0x3f) | 0x80;
        *cp++ = ((ch>>6)&0x3f) | 0x80;
        *cp = (ch&0x3f) | 0x80;
    }
    else if (ch < 0x4000000) {
        *cp++ = ((ch>>24)&0x03) | 0xf8;
        *cp++ = ((ch>>18)&0x3f) | 0x80;
        *cp++ = ((ch>>12)&0x3f) | 0x80;
        *cp++ = ((ch>>6)&0x3f) | 0x80;
        *cp = (ch&0x3f) | 0x80;
    } else {
        *cp++ = ((ch>>30)&0x1) | 0xfc;
        *cp++ = ((ch>>24)&0x3f) | 0x80;
        *cp++ = ((ch>>18)&0x3f) | 0x80;
        *cp++ = ((ch>>12)&0x3f) | 0x80;
        *cp++ = ((ch>>6)&0x3f) | 0x80;
        *cp++ = (ch&0x3f) | 0x80;
    }
}

#endif /* !SCM_CHAR_ENCODING_BODY */
