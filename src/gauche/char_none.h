/*
 * char-none.h
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
 *  $Id: char_none.h,v 1.4 2001-05-28 11:55:20 shirok Exp $
 */

#ifndef SCM_CHAR_ENCODING_BODY
/*===============================================================
 * Header part
 */

/* The name of the encoding.  Scheme procedure 
 * gauche-character-encoding returns a symbol with this name.
 */
#define SCM_CHAR_ENCODING_NAME "none"

/* Given first byte of the multibyte character, returns # of
 * bytes that follows, i.e. if the byte consists a single-byte
 * character, it returns 0; if the byte is the first byte of
 * two-byte character, it returns 1.   It may return -1 if
 * the given byte can't be a valid first byte of multibyte characters.
 */
#define SCM_CHAR_NFOLLOWS(ch)  0

/* Given wide character CH, returns # of bytes used when CH is
 * encoded in multibyte string.
 */
#define SCM_CHAR_NBYTES(ch)    1

/* Maximun # of multibyte character */
#define SCM_CHAR_MAX_BYTES     1

#define SCM_CHAR_GET(cp, ch) ((ch) = *(cp))
#define SCM_CHAR_PUT(cp, ch)  (*(cp) = (ch))

#define SCM_CHAR_BACKWARD(cp, start, result)    \
    do {                                        \
        if ((cp) > (start)) (result) = (cp)-1;  \
        else (result) = NULL;                   \
    } while (0)

#else  /* !SCM_CHAR_ENCODING_BODY */
/*==================================================================
 * This part is included in char.c
 */

/* Array of character encoding names, recognizable by iconv, that are
   compatible with this native encoding. */
static const char *supportedCharacterEncodings[] = {
    "ASCII",
    "US-ASCII",
    "ISO-8859-1",
    "ISO_8859-1",
    "ISO_8859-1:1987",
    "ISO-8859-2",
    "ISO_8859-2",
    "ISO_8859-2:1987",
    "ISO-8859-3",
    "ISO_8859-3",
    "ISO_8859-3:1988",
    "ISO-8859-4",
    "ISO_8859-4",
    "ISO_8859-4:1988",
    "ISO-8859-5",
    "ISO_8859-5",
    "ISO_8859-5:1988",
    "ISO-8859-6",
    "ISO_8859-6",
    "ISO_8859-7:1987",
    "ISO-8859-7",
    "ISO_8859-7",
    "ISO_8859-7:1987",
    "ISO-8859-8",
    "ISO_8859-8",
    "ISO_8859-8:1988",
    "ISO-8859-9",
    "ISO_8859-9",
    "ISO_8859-9:1989",
    "ISO-8859-10",
    "ISO_8859-10",
    "ISO_8859-10:1993",
    "ISO-8859-14",
    "ISO_8859-14",
    "ISO_8859-14:1998",
    NULL
};

#endif /* !SCM_CHAR_ENCODING_BODY */
