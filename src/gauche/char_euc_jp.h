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
 *  $Id: char_euc_jp.h,v 1.8 2001-04-26 20:07:13 shirok Exp $
 */

/*
 * Not complete, but for now...
 */
#ifndef SCM_CHAR_ENCODING_BODY

#define SCM_CHAR_ENCODING_NAME "euc-jp"

#define SCM_CHAR_NFOLLOWS(ch) \
    ((((unsigned char)(ch)) >= 0x80) ? 1 : 0)

#define SCM_CHAR_NBYTES(ch) \
    (((ch) >= 0x80) ? 2 : 1)

#define SCM_CHAR_MAX_BYTES     2

#define SCM_CHAR_GET(cp, ch)                                    \
    do {                                                        \
        if (((ch) = (unsigned char)*(cp)) >= 0x80) {            \
            (ch) = ((ch) << 8) + (unsigned char)*(cp+1);        \
        }                                                       \
    } while (0)

#define SCM_CHAR_PUT(cp, ch)                    \
    do {                                        \
        if (ch > 0xff) {                        \
            (cp)[0] = (ch >> 8) & 0xff;         \
            (cp)[1] = ch & 0xff;                \
        } else {                                \
            (cp)[0] = ch & 0xff;                \
        }                                       \
    } while (0)

#define SCM_CHAR_BACKWARD(cp, start, result)                    \
    do {                                                        \
        switch ((cp) - (start)) {                               \
        default:                                                \
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

#endif /* !SCM_CHAR_ENCODING_BODY */
