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
 *  $Id: cord_mb_utf8.h,v 1.1.1.1 2001-01-11 19:26:03 shiro Exp $
 */

/* Given ScmChar sc, returns # of bytes required to encode it. */
#define SCM_CHAR_NBYTES(sc)                     \
    (((sc) < 0x7f)? 1 :                         \
     ((sc) < 0x800)? 2 :                        \
     ((sc) < 0x10000)? 3 :                      \
     ((sc) < 0x200000)? 4 :                     \
     ((sc) < 0x4000000)? 5 : 6)

/* Given ScmByte sb, returns # of bytes in encoded string to follow
   to compose a ScmChar.   It can return -1 to indicate the encoded
   string has illegal value. */
#define SCM_MBS_NFOLLOWS(sb)                    \
   (((sb) < 0x80) ? 0 :                         \
    ((sb) < 0xc0) ? -1 :                        \
    ((sb) < 0xe0) ? 1 :                         \
    ((sb) < 0xf0) ? 2 :                         \
    ((sb) < 0xf8) ? 3 :                         \
    ((sb) < 0xfc) ? 4 : 5)

/* Return maximun length of encoded string.  Must be a constant. */
#define SCM_MBS_MAXLEN_FOR_CHAR      6

/* Return one char from ScmByte *mbs and put it in ScmChar sc.
   mbs is updated to point to next char boundary. */
#define SCM_MBS_GETCHAR(sc, mbs)                                        \
    do {                                                                \
        int len_ = SCM_MBS_NFOLLOWS(*(mbs));                            \
        (sc) = *(mbs)++;                                                \
        if (len_ > 0) {                                                 \
            switch (len_) {                                             \
            case 1: (sc) &= ~0xc0; break;                               \
            case 2: (sc) &= ~0xe0; break;                               \
            case 3: (sc) &= ~0xf0; break;                               \
            case 4: (sc) &= ~0xf8; break;                               \
            case 5: (sc) &= ~0xfc; break;                               \
            case 6: (sc) &= ~0xfe; break;                               \
            }                                                           \
            while (len_--) {                                            \
                (sc) = ((sc)<<6) + ((*(mbs)++)&~0xfc);                  \
            }                                                           \
        } else if (len_ < 0) {                                          \
            Scm_Errorf("Encountered illegally encoded string.");        \
        }                                                               \
    } while (0)

/* construct UTF8 trailing byte */
#define UTF8TBYTE(byte)     (((byte)&0x3f)|0x80)

/* Put one char to ScmByte *mbs.  mbs is updated to point to next
   char boundary.  The buffer pointed by *mbs must have enough size. */
#define SCM_MBS_PUTCHAR(mbs, sc)                                \
    do {                                                        \
        if ((sc) < 0x80) {                                      \
            *(mbs)++ = (unsigned char)((sc) & 0x7f);            \
        } else if ((sc) < 0x800) {                              \
            *(mbs)++ = (unsigned char)(((sc) >> 6)   | 0xc0);   \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc));          \
        } else if ((sc) < 0x10000) {                            \
            *(mbs)++ = (unsigned char)(((sc) >> 12) | 0xe0);    \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc) >> 6);     \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc));          \
        } else if ((sc) < 0x200000) {                           \
            *(mbs)++ = (unsigned char)(((sc) >> 18) | 0xf0);    \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc) >> 12);    \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc) >> 6);     \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc));          \
        } else if ((sc) < 0x4000000) {                          \
            *(mbs)++ = (unsigned char)(((sc) >> 24) | 0xf8);    \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc) >> 18);    \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc) >> 12);    \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc) >> 6);     \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc));          \
        } else {                                                \
            *(mbs)++ = (unsigned char)(((sc) >> 30) | 0xfc);    \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc) >> 24);    \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc) >> 18);    \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc) >> 12);    \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc) >> 6);     \
            *(mbs)++ = (unsigned char)UTF8TBYTE((sc));          \
        }                                                       \
    } while (0)
        


    
