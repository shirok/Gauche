/*
 * char-euc-jp.h
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: char_euc_jp.h,v 1.2 2001-02-05 09:51:47 shiro Exp $
 */

/*
 * For now, we ignore 3-byte encodings.
 */
#define SCM_CHAR_NFOLLOWS(ch) \
    ((((unsigned char)(ch)) >= 0x80) ? 1 : 0)

#define SCM_CHAR_NBYTES(ch) \
    (((ch) >= 0x80) ? 2 : 1)

#define SCM_CHAR_MAX_BYTES     2

#define SCM_STR_GETC(cp, ch)                                    \
    do {                                                        \
        if (((ch) = (unsigned char)*(cp)) >= 0x80) {            \
            (ch) = ((ch) << 8) + (unsigned char)*(cp+1);        \
        }                                                       \
    } while (0)

#define SCM_STR_PUTC(cp, ch)                    \
    do {                                        \
        if (ch > 0xff) {                        \
            (cp)[0] = (ch >> 8) & 0xff;         \
            (cp)[1] = ch & 0xff;                \
        } else {                                \
            (cp)[0] = ch & 0xff;                \
        }                                       \
    } while (0)


