/*
 * char-euc-jp.h
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: char_euc_jp.h,v 1.11 2002-06-12 12:01:24 shirok Exp $
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
#define SCM_CHAR_GET(cp, ch)                                    \
    do {                                                        \
        if (((ch) = (unsigned char)*(cp)) >= 0x80) {            \
            if ((ch) == 0x8f) {                                 \
                 (ch) = ((ch) << 16)                            \
                     + ((unsigned char)(cp)[1] << 8)            \
                     + (unsigned char)(cp)[2];                  \
            } else {                                            \
                (ch) = ((ch) << 8) + (unsigned char)(cp)[1];    \
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
    do {                                                        \
        switch ((cp) - (start)) {                               \
        default:                                                \
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
    "EUC-JP",
    "EUCJP",
    NULL
};

#endif /* !SCM_CHAR_ENCODING_BODY */
