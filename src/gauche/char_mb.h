/*
 * char_mb.h - legacy multibyte char interface
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
 *  $Id: char_mb.h,v 1.1.1.1 2001-01-11 19:26:03 shiro Exp $
 */


/* Given ScmChar sc, returns # of bytes required to encode it. */
#define SCM_CHAR_NBYTES(sc)    (((sc) < 0x80)? 1 : 2)

/* Given ScmByte sb, returns # of bytes in encoded string to follow
   to compose a ScmChar.   It can return -1 to indicate the encoded
   string has illegal value. */
#define SCM_MBS_NFOLLOWS(sb)      (((sb) < 0x80)? 0 : 1)

/* Return maximun length of encoded string.  Must be a constant. */
#define SCM_MBS_MAXLEN_FOR_CHAR      2

/* Return one char from ScmByte *mbs and put it in ScmChar sc.
   mbs is updated to point to next char boundary. */
#define SCM_MBS_GETCHAR(sc, mbs)                \
    do {                                        \
        if (((sc) = *(mbs)++) >= 0x80) {        \
            (sc) = (sc) << 8 + *(mbs)++;        \
        }                                       \
    } while (0)

/* Put one char to ScmByte *mbs.  mbs is updated to point to next
   char boundary.  The buffer pointed by *mbs must have enough size. */
#define SCM_MBS_PUTCHAR(mbs, sc)                \
    do {                                        \
        if (SCM_CHAR_NBYTES(sc) == 1) {         \
            *(mbs)++ = (ScmByte)(sc);           \
        } else {                                \
            *(mbs)++ = (ScmByte)(sc >> 8);      \
            *(mbs)++ = (ScmByte)(sc & 0xff);    \
        }                                       \
    } while (0)
        


    
