/*
 * jconv.c - alternative japanese code conversion routines
 *
 *  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: jconv.c,v 1.1 2002-05-29 11:29:17 shirok Exp $
 */

/* Some iconv() implementations don't support japanese character encodings,
 * or have problems handling them.  This code provides an alternative way
 * to convert these encodings.
 */

/* This file handles conversion among UTF8, Shift-JIS, EUC_JP, and ISO2022JP.
 * Shift-JIS and EUC_JP are based on JIS X 0213:2000.  ISO2022JP partially
 * handles ISO2022-JP-3 as well.
 *
 * EUC_JP is used as a 'pivot' encoding, for it can naturally handle
 * JISX 0201, JISX 0208, JISX 0212 and JISx 0213 characters.
 */

#include "charconv.h"

#define ILLEGAL_SEQUENCE  -1
#define INPUT_NOT_ENOUGH  -2
#define OUTPUT_NOT_ENOUGH -3

#define INCHK(n)   do{if (inroom < (n)) return INPUT_NOT_ENOUGH;}while(0)
#define OUTCHK(n)  do{if (outroom < (n)) return OUTPUT_NOT_ENOUGH;}while(0)

/* Substitution characters.
 *  Unrecognized 1-byte character is substituted by SUBST1_CHAR.
 *  It's common to all encodings.
 *  Unrecognized or uncovertable multibyte character is substituted
 *  by so-called 'Geta-sign'.
 */
#define SUBST1_CHAR   '?'
#define EUCJ_SUBST2_CHAR1  0xa2
#define EUCJ_SUBST2_CHAR2  0xae
#define JIS_SUBST2_CHAR1   0x02
#define JIS_SUBST2_CHAR2   0x0e
#define SJIS_SUBST2_CHAR1  0x81
#define SJIS_SUBST2_CHAR2  0xac

#define EUCJ_SUBST                              \
  do { OUTCHK(2);                               \
       outptr[0] = EUCJ_SUBST2_CHAR1;           \
       outptr[1] = EUCJ_SUBST2_CHAR2;           \
       *outchars = 2; } while (0)

#define SJIS_SUBST                              \
  do { OUTCHK(2);                               \
       outptr[0] = SJIS_SUBST2_CHAR1;           \
       outptr[1] = SJIS_SUBST2_CHAR2;           \
       *outchars = 2; } while (0)

/*=================================================================
 * Shift JIS
 */

/* Shift_JISX0213 -> EUC-JP
 * 
 * Mapping anormalities
 *
 *   0x5c, 0x7e : Shift_JISX0213 mapping table maps 0x5c to U+00A5
 *       (YEN SIGN) and 0x7e to U+203E (OVERLINE).  But mapping so
 *       breaks the program code written in Shift JIS.   I map them
 *       to the corresponding ASCII chars.
 *   0xfd, 0xfe, 0xff : These are reserved bytes.  Apple uses these
 *       bytes for vendor extension:
 *        0xfd - U+00A9 COPYRIGHT SIGN     |EUC A9A6  |JISX0213
 *        0xfe - U+2122 TRADE MARK SIGN    |EUC 8FA2EF|JISX0212
 *        0xff - U+2026 HORIZONTAL ELLIPSIS|EUC A1C4  |JISX0208
 *       This is a one-direction mapping.
 *   0x80, 0xa0 : These are reserved bytes.  Replaced to the
 *       one-byte substitution character of destination encoding.
 *
 * Conversion scheme
 *   0x00-0x7f : corresponding ASCII range.
 *   0x80      : substitution character
 *   0x81 -- 0x9f : first byte (s1) of double byte range for JIS X 0213 m=1
 *   0xa0      : substitution character
 *   0xa1 -- 0xdf : JISX 0201 kana = s1-0x80
 *   0xe0 -- 0xef : first byte (s1) of double byte range for JIS X 0213 m=1
 *   0xf0 -- 0xfc : first byte (s1) of double byte range for JIS X 0213 m=2
 *   0xfd : U+00A9, EUC A9A6, JISX0213 (1, 0x09, 0x06)
 *   0xfe : U+2122, EUC 8FA2EF, JISX0212
 *   0xff : U+2026, EUC A1C4, JISX0208 (1, 0x01, 0x24)
 *
 *   For double-byte character, second byte s2 must be in the range of
 *   0x40 <= s2 <= 0x7e or 0x80 <= s2 <= 0xfc.  Otherwise, double-byte
 *   substitution character is used.
 *
 *     two bytes (s1, s2) maps to JIS X 0213 (m, k, t) by
 *        m = 1 if s1 <= 0xef, 2 otherwise
 *        k = (s1-0x80)*2 - ((s2 >= 0x80)? 1 : 0)  if s1 <= 0xef
 *            (s1-0x9e)*2 - ((s2 >= 0x80)? 1 : 0)  if s1 >= 0xf5
 *            otherwise, use the following table
 *               s1   k (s2>=0x80, s2<0x80)
 *              0xf0   (0x01, 0x08)
 *              0xf1   (0x03, 0x04)
 *              0xf2   (0x05, 0x0c)
 *              0xf3   (0x0e, 0x0d)
 *              0xf4   (0x0f, 0x4e)
 *        t = s2-0x3f if s2 < 0x7f
 *            s2-0x40 if s2 < 0x9f
 *            s2-0x9e otherwise
 *
 *     JIS X 0213 to EUC-JP is a straightfoward conversion.
 */

static int sjis2eucj(ScmConvInfo *cinfo, const char *inptr, int inroom,
                     char *outptr, int outroom, int *outchars)
{
    unsigned char s1, s2;
    static unsigned char cvt[] = { 0xa1, 0xa8, 0xa3, 0xa4, 0xa5, 0xac, 0xae, 0xad, 0xaf, 0xee };

    s1 = *(unsigned char *)inptr;
    if (s1 < 0x7f) {
        *outptr = s1;
        *outchars = 1;
        return 1;
    }
    if ((s1 > 0x80 && s1 < 0xa0) || (s1 >= 0xe0 && s1 <= 0xfc)) {
        /* Double byte char */
        unsigned char m, e1, e2;
        INCHK(2);
        s2 = *(unsigned char*)(inptr+1);
        if (s2 < 0x40 || s2 > 0xfc) {
            EUCJ_SUBST;
            return 2;
        }
        
        if (s1 <= 0xef) {
            OUTCHK(2);
            m = 1;
            e1 = (s1-0x80)*2 + 0xa0 - ((s2 >= 0x80)? 1 : 0);
        } else if (s1 >= 0xf5) {
            OUTCHK(3);
            m = 2;
            e1 = (s1-0x9e)*2 + 0xa0 - ((s2 >= 0x80)? 1 : 0);
        } else {
            OUTCHK(3);
            m = 2;
            e1 = cvt[(s1-0xf0)*2+((s2 < 0x80)? 1 : 0)];
        }
        
        if (s2 < 0x7f) {
            e2 = s1 - 0x3f + 0xa0;
        } else if (s2 < 0x9f) {
            e2 = s1 - 0x40 + 0xa0;
        } else {
            e2 = s1 - 0x9e + 0xa0;
        }
        if (m == 1) {
            outptr[0] = e1;
            outptr[1] = e2;
            *outchars = 2;
        } else {
            outptr[0] = 0x8f;
            outptr[1] = e1;
            outptr[2] = e2;
            *outchars = 3;
        }
        return 2;
    }
    if (s1 >= 0xa1 && s1 <= 0xdf) {
        /* JISX0201 KANA */
        OUTCHK(2);
        outptr[0] = 0x8e;
        outptr[1] = s1;
        *outchars = 2;
        return 1;
    }
    if (s1 == 0xfd) {
        /* copyright mark */
        OUTCHK(2);
        outptr[0] = 0xa9;
        outptr[1] = 0xa6;
        *outchars = 2;
        return 1;
    }
    if (s1 == 0xfe) {
        /* trademark sign.  this is not in JISX0213, but in JISX0212. */
        OUTCHK(3);
        outptr[0] = 0x8f;
        outptr[1] = 0xa2;
        outptr[2] = 0xef;
        *outchars = 3;
        return 1;
    }
    if (s1 == 0xff) {
        /* horizontal ellipsis. */
        OUTCHK(2);
        outptr[0] = 0xa1;
        outptr[1] = 0xc4;
        *outchars = 2;
        return 1;
    }
    
    /* s1 == 0x80 or 0xa0 */
    outptr[0] = SUBST1_CHAR;
    *outchars = 1;
    return 1;
}

/* EUC_JISX0213 -> Shift_JIS
 * 
 * Mapping anormalities
 *
 *   0x80--0xa0 except 0x8e and 0x8f : C1 region.
 *          Doesn't have corresponding SJIS bytes,
 *          so mapped to substitution char.
 *   0xff : reserved byte.  mapped to substitution char.
 *
 * Conversion scheme
 *   0x00-0x7f : corresponding ASCII range.
 *   0x80--0x8d : substitution char.
 *   0x8e : leading byte of JISX 0201 kana
 *   0x8f : leading byte of JISX 0212 or JISX 0213 plane 2
 *   0x90--0xa0 : substitution char.
 *   0xa1--0xfe : first byte (e1) of JISX 0213 plane 1
 *   0xff : substitution char
 *
 *   For double or trible-byte character, subsequent byte has to be in
 *   the range between 0xa1 and 0xfe inclusive.  If not, it is replaced
 *   for the substitution character.
 *   
 *   If the first byte is in the range of 0xa1--0xfe, two bytes (e1, e2)
 *   is mapped to SJIS (s1, s2) by:
 *
 *     s1 = (e1 - 0xa0 + 0x101)/2 if 0xa1 <= e1 <= 0xde
 *          (e1 - 0xa0 + 0x181)/2 if 0xdf <= e1 <= 0xfe
 *     s2 = (e2 - 0xa0 + 0x3f) if even?(e1) && 0xa1 <= e2 <= 0xdf
 *          (e2 - 0xa0 + 0x40) if even?(e1) && 0xe0 <= e2 <= 0xfe
 *          (e2 - 0xa0 + 0x9e) if odd?(e1)
 *
 *   If the first byte is 0x8f, the second byte (e1) and the third byte
 *   (e2) is mapped to SJIS (s1, s2) by:
 *     if (0xee <= e1 <= 0xfe)  s1 = (e1 - 0xa0 + 0x19b)/2
 *     otherwise, follow the table:
 *       e1 == 0xa1 or 0xa8  => s1 = 0xf0
 *       e1 == 0xa3 or 0xa4  => s1 = 0xf1
 *       e1 == 0xa5 or 0xac  => s1 = 0xf2
 *       e1 == 0xae or 0xad  => s1 = 0xf3
 *       e1 == 0xaf          => s1 = 0xf4
 *     If e1 is other value, it is JISX0212; we use substitution char.
 *     s2 is mapped with the same rule above.
 */

static int eucj2sjis(ScmConvInfo *cinfo, const char *inptr, int inroom,
                     char *outptr, int outroom, int *outchars)
{
    unsigned char e1, e2;
    e1 = *(unsigned char*)inptr;
    if (e1 <= 0x7f) {
        *outptr = e1;
        *outchars = 1;
        return 1;
    }
    if (e1 >= 0xa1 && e1 <= 0xfe) {
        /* double byte char (JISX 0213 plane 1) */
        unsigned char s1, s2;
        INCHK(2);
        e2 = inptr[1];
        if (e2 < 0xa1 || e2 == 0xff) {
            SJIS_SUBST;
            return 2;
        }
        OUTCHK(2);
        if (e1 <= 0xde) s1 = (e1 - 0xa0 + 0x101)/2;
        else            s1 = (e1 - 0xa0 + 0x181)/2;
        if (e1%2) {
            s2 = e2 - 0xa0 + 0x9e;
        } else {
            if (e2 < 0xdf) s2 = e2 - 0xa0 + 0x3f;
            else           s2 = e2 - 0xa0 + 0x40;
        }
        outptr[0] = e1;
        outptr[1] = e2;
        *outchars = 2;
        return 2;
    }
    if (e1 == 0x8e) {
        /* JISX 0201 kana */
        INCHK(2);
        e2 = inptr[1];
        if (e2 < 0xa1 || e2 == 0xff) {
            *outptr = SUBST1_CHAR;
        } else {
            *outptr = e2;
        }
        *outchars = 1;
        return 2;
    }
    if (e1 == 0x8f) {
        /* triple byte char */
        unsigned char s1, s2;
        unsigned char cvt[] = { 0xf0, 0, 0xf1, 0xf1, 0xf2, 0, 0, 0xf0, 0, 0, 0, 0xf2, 0xf3, 0xf3, 0xf4 };
        
        INCHK(3);
        OUTCHK(2);
        e1 = inptr[1];
        e2 = inptr[2];
        if (e1 < 0xa1 || e1 == 0xff || e2 < 0xa1 || e2 == 0xff) {
            SJIS_SUBST;
            return 3;
        }
        if (e1 >= 0xee) {
            s1 = (e1 - 0xa0 + 0x19b)/2;
        } else if (e1 >= 0xb0) {
            SJIS_SUBST;
            return 3;
        } else {
            s1 = cvt[e1-0xa1];
            if (s1 == 0) {
                SJIS_SUBST;
                return 3;
            }
        }
        if (e1%2) {
            s2 = e2 - 0xa0 + 0x9e;
        } else {
            if (e2 < 0xdf) s2 = e2 - 0xa0 + 0x3f;
            else           s2 = e2 - 0xa0 + 0x40;
        }
        outptr[0] = e1;
        outptr[1] = e2;
        *outchars = 2;
        return 3;
    }
    /* no corresponding char */
    *outptr = SUBST1_CHAR;
    *outchars = 1;
    return 1;
}

/*=================================================================
 * UTF8
 */

/* Conversion between UTF8 and EUC_JP is based on the table found at
 * http://isweb11.infoseek.co.jp/computer/wakaba/table/jis-note.ja.html
 *
 * There are some characters in JISX0213 that can't be represented
 * in a single Unicode character, but can be with a combining character.
 * In such case, EUC_JP to UTF8 conversion uses combining character,
 * but UTF8 to EUC_JP conversion translates the combining character into
 * another character.  For example, a single JISX0213 katakana 'nga'
 * (hiragana "ka" with han-dakuon mark) will translates to Unicode
 * U+304B+309A (HIRAGANA LETTER KA + COMBINING KATAKANA-HIRAGANA SEMI-VOICED
 * SOUND MARK).  When this sequence is converted to EUC_JP again, it
 * becomes EUCJ 0xA4AB + 0xA1AC.  This is an implementation limitation,
 * and should be removed in later release.
 */


static int utf2eucj(ScmConvInfo *cinfo, const char *inptr, int inroom,
                    char *outptr, int outroom, int *outchars)
{
}


