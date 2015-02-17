/*
 * jconv.c - alternative japanese code conversion routines
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

#include <ctype.h>
#include "charconv.h"

#define INCHK(n)   do{if ((int)inroom < (n)) return INPUT_NOT_ENOUGH;}while(0)
#define OUTCHK(n)  do{if ((int)outroom < (n)) return OUTPUT_NOT_ENOUGH;}while(0)

#define ERRP(n)    ((n)==INPUT_NOT_ENOUGH||(n)==OUTPUT_NOT_ENOUGH||(n)==ILLEGAL_SEQUENCE)

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
#define UTF8_SUBST2_CHAR1   0xe3
#define UTF8_SUBST2_CHAR2   0x80
#define UTF8_SUBST2_CHAR3   0x93

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

#define UTF8_SUBST                              \
  do { OUTCHK(3);                               \
       outptr[0] = UTF8_SUBST2_CHAR1;           \
       outptr[1] = UTF8_SUBST2_CHAR2;           \
       outptr[2] = UTF8_SUBST2_CHAR2;           \
       *outchars = 3; } while (0)

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
 *        k = (s1-0x80)*2 - ((s2 < 0x9f)? 1 : 0)  if s1 <= 0x9f
 *            (s1-0xc0)*2 - ((s2 < 0x9f)? 1 : 0)  if 0xe0 <= s1 <= 0xef
 *            (s1-0x9e)*2 - ((s2 < 0x89)? 1 : 0)  if s1 >= 0xf5
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

static size_t sjis2eucj(ScmConvInfo *cinfo, const char *inptr, size_t inroom,
                        char *outptr, size_t outroom, size_t *outchars)
{
    static const unsigned char cvt[] = { 0xa1, 0xa8, 0xa3, 0xa4, 0xa5, 0xac, 0xae, 0xad, 0xaf, 0xee };

    unsigned char s1 = inptr[0];
    if (s1 < 0x7f) {
        *outptr = s1;
        *outchars = 1;
        return 1;
    }
    if ((s1 > 0x80 && s1 < 0xa0) || (s1 >= 0xe0 && s1 <= 0xfc)) {
        /* Double byte char */
        unsigned char m, e1, e2;
        INCHK(2);
        unsigned char s2 = inptr[1];
        if (s2 < 0x40 || s2 > 0xfc) {
            EUCJ_SUBST;
            return 2;
        }

        if (s1 <= 0x9f) {
            OUTCHK(2);
            m = 1;
            e1 = (s1-0x80)*2 + 0xa0 - ((s2 < 0x9f)? 1 : 0);
        } else if (s1 <= 0xef) {
            OUTCHK(2);
            m = 1;
            e1 = (s1-0xc0)*2 + 0xa0 - ((s2 < 0x9f)? 1 : 0);
        } else if (s1 >= 0xf5) {
            OUTCHK(3);
            m = 2;
            e1 = (s1-0xf5)*2 + 0x50 + 0xa0 - ((s2 < 0x9f)? 1 : 0);
        } else {
            OUTCHK(3);
            m = 2;
            e1 = cvt[(s1-0xf0)*2+((s2 < 0x9f)? 1 : 0)];
        }

        if (s2 < 0x7f) {
            e2 = s2 - 0x3f + 0xa0;
        } else if (s2 < 0x9f) {
            e2 = s2 - 0x40 + 0xa0;
        } else {
            e2 = s2 - 0x9e + 0xa0;
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
 *     s2 = (e2 - 0xa0 + 0x3f) if odd?(e1) && 0xa1 <= e2 <= 0xdf
 *          (e2 - 0xa0 + 0x40) if odd?(e1) && 0xe0 <= e2 <= 0xfe
 *          (e2 - 0xa0 + 0x9e) if even?(e1)
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

static size_t eucj2sjis(ScmConvInfo *cinfo, const char *inptr, size_t inroom,
                        char *outptr, size_t outroom, size_t *outchars)
{
    unsigned char e1 = inptr[0];
    if (e1 <= 0x7f) {
        outptr[0] = e1;
        *outchars = 1;
        return 1;
    }
    if (e1 >= 0xa1 && e1 <= 0xfe) {
        /* double byte char (JISX 0213 plane 1) */
        unsigned char s1, s2;
        INCHK(2);
        unsigned char e2 = inptr[1];
        if (e2 < 0xa1 || e2 == 0xff) {
            SJIS_SUBST;
            return 2;
        }
        OUTCHK(2);
        if (e1 <= 0xde) s1 = (e1 - 0xa0 + 0x101)/2;
        else            s1 = (e1 - 0xa0 + 0x181)/2;
        if (e1%2 == 0) {
            s2 = e2 - 0xa0 + 0x9e;
        } else {
            if (e2 <= 0xdf) s2 = e2 - 0xa0 + 0x3f;
            else            s2 = e2 - 0xa0 + 0x40;
        }
        outptr[0] = s1;
        outptr[1] = s2;
        *outchars = 2;
        return 2;
    }
    if (e1 == 0x8e) {
        /* JISX 0201 kana */
        INCHK(2);
        unsigned char e2 = inptr[1];
        if (e2 < 0xa1 || e2 == 0xff) {
            outptr[0] = SUBST1_CHAR;
        } else {
            outptr[0] = e2;
        }
        *outchars = 1;
        return 2;
    }
    if (e1 == 0x8f) {
        /* triple byte char */
        unsigned char s1, s2;
        static const unsigned char cvt[] = { 0xf0, 0, 0xf1, 0xf1, 0xf2, 0, 0, 0xf0, 0, 0, 0, 0xf2, 0xf3, 0xf3, 0xf4 };

        INCHK(3);
        OUTCHK(2);
        e1 = inptr[1];
        unsigned char e2 = inptr[2];
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
        if (e1%2 == 0) {
            s2 = e2 - 0xa0 + 0x9e;
        } else {
            if (e2 < 0xdf) s2 = e2 - 0xa0 + 0x3f;
            else           s2 = e2 - 0xa0 + 0x40;
        }
        outptr[0] = s1;
        outptr[1] = s2;
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

/* [UTF8 -> EUC_JP conversion]
 *
 * EUC-JP has the corresponding characters to the wide range of
 * UCS characters.
 *
 *   UCS4 character   # of EUC_JP characters
 *   ---------------------------------------
 *     U+0000+0xxx    564
 *     U+0000+1xxx      6
 *     U+0000+2xxx    321
 *     U+0000+3xxx    422
 *     U+0000+4xxx    347
 *     U+0000+5xxx   1951
 *     U+0000+6xxx   2047
 *     U+0000+7xxx   1868
 *     U+0000+8xxx   1769
 *     U+0000+9xxx   1583
 *     U+0000+fxxx    241
 *     U+0002+xxxx    302
 *
 * It is so wide and so sparse that naive lookup table implementation from
 * UCS to EUC can be space-wasting.  I use hierarchical table with some
 * ad-hoc heuristics.   Since the hierarchical table is used, I directly
 * translates UTF8 to EUC_JP, without converting it to UCS4.
 *
 * Strategy outline: say input consists of bytes named u0, u1, ....
 *
 *  u0 <= 0x7f  : ASCII range
 *  u0 in [0xc2-0xd1] : UTF8 uses 2 bytes.  Some mappings within this range
 *         is either very regular or very small, and they are
 *         hardcoded.   Other mappings uses table lookup.
 *  u0 == 0xe1  : UTF8 uses 3 bytes.  There are only 6 characters in this
 *         range, and it is hardcoded.
 *  u0 in [0xe2-0xe9, 0xef] : Large number of characters are in this range.
 *         Two-level table of 64 entries each is used to dispatch the
 *         characters.
 *  u0 == 0xf0  : UTF8 uses 4 bytes.  u1 is in [0xa0-0xaa].  u2 and u3 is
 *         used for dispatch table of 64 entries each.
 *
 * The final table entry is unsigned short.  0x0000 means no corresponding
 * character is defined in EUC_JP.  >=0x8000 is the EUC_JP character itself.
 * < 0x8000 means the character is in G3 plane; 0x8f should be preceded,
 * and 0x8000 must be added to the value.
 */

#include "ucs2eucj.c"

/* Emit given euc char */
static inline size_t utf2euc_emit_euc(unsigned short euc, size_t inchars, char *outptr, size_t outroom, size_t *outchars)
{
    if (euc == 0) {
        EUCJ_SUBST;
    } else if (euc < 0x8000) {
        OUTCHK(3);
        outptr[0] = 0x8f;
        outptr[1] = (euc >> 8) + 0x80;
        outptr[2] = euc & 0xff;
        *outchars = 3;
    } else {
        OUTCHK(2);
        outptr[0] = (euc >> 8);
        outptr[1] = euc & 0xff;
        *outchars = 2;
    }
    return inchars;
}

/* handle 2-byte UTF8 sequence.  0xc0 <= u0 <= 0xdf */
static inline size_t utf2euc_2(ScmConvInfo *cinfo, unsigned char u0,
                               const char *inptr, size_t inroom,
                               char *outptr, size_t outroom, size_t *outchars)
{
    const unsigned short *etab = NULL;

    INCHK(2);
    unsigned char u1 = (unsigned char)inptr[1];
    if (u1 < 0x80 || u1 >= 0xc0) return ILLEGAL_SEQUENCE;

    switch (u0) {
    case 0xc2: etab = utf2euc_c2; break;
    case 0xc3: etab = utf2euc_c3; break;
    case 0xc4: etab = utf2euc_c4; break;
    case 0xc5: etab = utf2euc_c5; break;
    case 0xc6:
        if (u1 == 0x93) { /* U+0193 -> euc ABA9 */
            return utf2euc_emit_euc(0xaba9, 2, outptr, outroom, outchars);
        } else break;
    case 0xc7: etab = utf2euc_c7; break;
    case 0xc9: etab = utf2euc_c9; break;
    case 0xca: etab = utf2euc_ca; break;
    case 0xcb: etab = utf2euc_cb; break;
    case 0xcc: etab = utf2euc_cc; break;
    case 0xcd:
        if (u1 == 0xa1) { /* U+0361 -> euc ABD2 */
            return utf2euc_emit_euc(0xabd2, 2, outptr, outroom, outchars);
        } else break;
    case 0xce: etab = utf2euc_ce; break;
    case 0xcf: etab = utf2euc_cf; break;
    case 0xd0: etab = utf2euc_d0; break;
    case 0xd1: etab = utf2euc_d1; break;
    default:
        break;
    }
    if (etab != NULL) {
        /* table lookup */
        return utf2euc_emit_euc(etab[u1-0x80], 2, outptr, outroom, outchars);
    }
    EUCJ_SUBST;
    return 2;
}

/* handle 3-byte UTF8 sequence.  0xe0 <= u0 <= 0xef */
static inline size_t utf2euc_3(ScmConvInfo *cinfo, unsigned char u0,
                               const char *inptr, size_t inroom,
                               char *outptr, size_t outroom, size_t *outchars)
{
    const unsigned char *tab1 = NULL;
    const unsigned short (*tab2)[64] = NULL;

    INCHK(3);
    unsigned char u1 = (unsigned char)inptr[1];
    unsigned char u2 = (unsigned char)inptr[2];

    switch (u0) {
    case 0xe1: /* special case : there's only 6 chars */
        {
            unsigned short euc = 0;
            if (u1 == 0xb8) {
                if (u2 == 0xbe)      euc = 0xa8f2;
                else if (u2 == 0xbf) euc = 0xa8f3;
            } else if (u1 == 0xbd) {
                if (u2 == 0xb0)      euc = 0xabc6;
                else if (u2 == 0xb1) euc = 0xabc7;
                else if (u2 == 0xb2) euc = 0xabd0;
                else if (u2 == 0xb3) euc = 0xabd1;
            }
            return utf2euc_emit_euc(euc, 3, outptr, outroom, outchars);
        }
    case 0xe2: tab1 = utf2euc_e2; tab2 = utf2euc_e2_xx; break;
    case 0xe3: tab1 = utf2euc_e3; tab2 = utf2euc_e3_xx; break;
    case 0xe4: tab1 = utf2euc_e4; tab2 = utf2euc_e4_xx; break;
    case 0xe5: tab1 = utf2euc_e5; tab2 = utf2euc_e5_xx; break;
    case 0xe6: tab1 = utf2euc_e6; tab2 = utf2euc_e6_xx; break;
    case 0xe7: tab1 = utf2euc_e7; tab2 = utf2euc_e7_xx; break;
    case 0xe8: tab1 = utf2euc_e8; tab2 = utf2euc_e8_xx; break;
    case 0xe9: tab1 = utf2euc_e9; tab2 = utf2euc_e9_xx; break;
    case 0xef: tab1 = utf2euc_ef; tab2 = utf2euc_ef_xx; break;
    default:
        break;
    }
    if (tab1 != NULL) {
        unsigned char ind = tab1[u1-0x80];
        if (ind != 0) {
            return utf2euc_emit_euc(tab2[ind-1][u2-0x80], 3, outptr, outroom, outchars);
        }
    }
    EUCJ_SUBST;
    return 3;
}

/* handle 4-byte UTF8 sequence.  u0 == 0xf0, 0xa0 <= u1 <= 0xaa */
static inline size_t utf2euc_4(ScmConvInfo *cinfo, unsigned char u0,
                               const char *inptr, size_t inroom,
                               char *outptr, size_t outroom, size_t *outchars)
{
    const unsigned short *tab = NULL;

    INCHK(4);
    if (u0 != 0xf0) {
        EUCJ_SUBST;
        return 4;
    }
    unsigned char u1 = (unsigned char)inptr[1];
    unsigned char u2 = (unsigned char)inptr[2];
    unsigned char u3 = (unsigned char)inptr[3];

    switch (u1) {
    case 0xa0: tab = utf2euc_f0_a0; break;
    case 0xa1: tab = utf2euc_f0_a1; break;
    case 0xa2: tab = utf2euc_f0_a2; break;
    case 0xa3: tab = utf2euc_f0_a3; break;
    case 0xa4: tab = utf2euc_f0_a4; break;
    case 0xa5: tab = utf2euc_f0_a5; break;
    case 0xa6: tab = utf2euc_f0_a6; break;
    case 0xa7: tab = utf2euc_f0_a7; break;
    case 0xa8: tab = utf2euc_f0_a8; break;
    case 0xa9: tab = utf2euc_f0_a9; break;
    case 0xaa: tab = utf2euc_f0_aa; break;
    default:
        break;
    }
    if (tab != NULL) {
        unsigned short u2u3 = u2*256 + u3;
        for (int i=0; tab[i]; i+=2) {
            if (tab[i] == u2u3) {
                return utf2euc_emit_euc(tab[i+1], 4, outptr, outroom, outchars);
            }
        }
    }
    EUCJ_SUBST;
    return 4;
}

/* Body of UTF8 -> EUC_JP conversion */
static size_t utf2eucj(ScmConvInfo *cinfo, const char *inptr, size_t inroom,
                       char *outptr, size_t outroom, size_t *outchars)
{
    unsigned char u0 = (unsigned char)inptr[0];

    if (u0 <= 0x7f) {
        *outptr = u0;
        *outchars = 1;
        return 1;
    }
    if (u0 <= 0xbf) {
        /* invalid UTF8 sequence */
        return ILLEGAL_SEQUENCE;
    }
    if (u0 <= 0xdf) {
        /* 2-byte UTF8 sequence */
        return utf2euc_2(cinfo, u0, inptr, inroom, outptr, outroom, outchars);
    }
    if (u0 <= 0xef) {
        /* 3-byte UTF8 sequence */
        return utf2euc_3(cinfo, u0, inptr, inroom, outptr, outroom, outchars);
    }
    if (u0 <= 0xf7) {
        /* 4-byte UTF8 sequence */
        return utf2euc_4(cinfo, u0, inptr, inroom, outptr, outroom, outchars);
    }
    if (u0 <= 0xfb) {
        /* 5-byte UTF8 sequence */
        INCHK(5);
        EUCJ_SUBST;
        return 5;
    }
    if (u0 <= 0xfd) {
        /* 6-byte UTF8 sequence */
        INCHK(6);
        EUCJ_SUBST;
        return 6;
    }
    return ILLEGAL_SEQUENCE;
}

/* [EUC_JP -> UTF8 conversion]
 *
 * Conversion strategy:
 *   If euc0 is in ASCII range, or C1 range except 0x8e or 0x8f, map it as is.
 *   If euc0 is 0x8e, use JISX0201-KANA table.
 *   If euc0 is 0x8f, use JISX0213 plane 2 table.
 *   If euc0 is in [0xa1-0xfe], use JISX0213 plane1 table.
 *   If euc0 is 0xa0 or 0xff, return ILLEGAL_SEQUENCE.
 *
 * JISX0213 plane2 table is consisted by a 2-level tree.  The first-level
 * returns an index to the second-level table by (euc1 - 0xa1).  Only the
 * range of JISX0213 defined region is converted; JISX0212 region will be
 * mapped to the substitution char.
 */

#include "eucj2ucs.c"

/* UTF8 utility.  Similar stuff is included in gauche/char_utf_8.h
   if the native encoding is UTF8, but not otherwise.
   So I include them here as well. */

void jconv_ucs4_to_utf8(unsigned int ucs, char *cp)
{
    if (ucs < 0x80) {
        *cp = ucs;
    }
    else if (ucs < 0x800) {
        *cp++ = ((ucs>>6)&0x1f) | 0xc0;
        *cp = (ucs&0x3f) | 0x80;
    }
    else if (ucs < 0x10000) {
        *cp++ = ((ucs>>12)&0x0f) | 0xe0;
        *cp++ = ((ucs>>6)&0x3f) | 0x80;
        *cp = (ucs&0x3f) | 0x80;
    }
    else if (ucs < 0x200000) {
        *cp++ = ((ucs>>18)&0x07) | 0xf0;
        *cp++ = ((ucs>>12)&0x3f) | 0x80;
        *cp++ = ((ucs>>6)&0x3f) | 0x80;
        *cp = (ucs&0x3f) | 0x80;
    }
    else if (ucs < 0x4000000) {
        *cp++ = ((ucs>>24)&0x03) | 0xf8;
        *cp++ = ((ucs>>18)&0x3f) | 0x80;
        *cp++ = ((ucs>>12)&0x3f) | 0x80;
        *cp++ = ((ucs>>6)&0x3f) | 0x80;
        *cp = (ucs&0x3f) | 0x80;
    } else {
        *cp++ = ((ucs>>30)&0x1) | 0xfc;
        *cp++ = ((ucs>>24)&0x3f) | 0x80;
        *cp++ = ((ucs>>18)&0x3f) | 0x80;
        *cp++ = ((ucs>>12)&0x3f) | 0x80;
        *cp++ = ((ucs>>6)&0x3f) | 0x80;
        *cp++ = (ucs&0x3f) | 0x80;
    }
}

/* Given 'encoded' ucs, emit utf8.  'Encoded' ucs is the entry of the
   conversion table.  If ucs >= 0x100000, it is composed by two UCS2
   character.  Otherwise, it is one UCS4 character. */
static inline size_t eucj2utf_emit_utf(unsigned int ucs, size_t inchars,
                                       char *outptr, size_t outroom,
                                       size_t *outchars)
{
    if (ucs == 0) {
        UTF8_SUBST;
    } else if (ucs < 0x100000) {
        int outreq = UCS2UTF_NBYTES(ucs);
        OUTCHK(outreq);
        jconv_ucs4_to_utf8(ucs, outptr);
        *outchars = outreq;
    } else {
        /* we need two UCS characters */
        unsigned int ucs0 = (ucs >> 16) & 0xffff;
        unsigned int ucs1 = ucs & 0xfff;
        int outreq0 = UCS2UTF_NBYTES(ucs0);
        int outreq1 = UCS2UTF_NBYTES(ucs1);
        OUTCHK(outreq0+outreq1);
        jconv_ucs4_to_utf8(ucs0, outptr);
        jconv_ucs4_to_utf8(ucs1, outptr+outreq0);
        *outchars = outreq0+outreq1;
    }
    return inchars;
}

static size_t eucj2utf(ScmConvInfo *cinfo, const char *inptr, size_t inroom,
                       char *outptr, size_t outroom, size_t *outchars)
{
    unsigned char e0 = (unsigned char)inptr[0];
    if (e0 < 0xa0) {
        if (e0 == 0x8e) {
            /* JIS X 0201 KANA */
            INCHK(2);
            unsigned char e1 = (unsigned char)inptr[1];
            if (e1 < 0xa1 || e1 > 0xdf) return ILLEGAL_SEQUENCE;
            unsigned int ucs = 0xff61 + (e1 - 0xa1);
            return eucj2utf_emit_utf(ucs, 2, outptr, outroom, outchars);
        }
        else if (e0 == 0x8f) {
            /* JIS X 0213 plane 2 */
            int index;

            INCHK(3);
            unsigned char e1 = (unsigned char)inptr[1];
            unsigned char e2 = (unsigned char)inptr[2];
            if (e1 < 0xa1 || e1 > 0xfe || e2 < 0xa1 || e2 > 0xfe) {
                return ILLEGAL_SEQUENCE;
            }
            index = euc_jisx0213_2_index[e1 - 0xa1];
            if (index < 0) {
                UTF8_SUBST;
                return 3;
            }
            unsigned int ucs = euc_jisx0213_2_to_ucs2[index][e2 - 0xa1];
            return eucj2utf_emit_utf(ucs, 3, outptr, outroom, outchars);
        }
        else {
            /* ASCII or C1 region */
            outptr[0] = e0;
            *outchars = 1;
            return 1;
        }
    }
    if (e0 > 0xa0 && e0 < 0xff) {
        /* JIS X 0213 plane 1 */
        INCHK(2);
        unsigned char e1 = (unsigned char)inptr[1];
        if (e1 < 0xa1 || e1 > 0xfe) return ILLEGAL_SEQUENCE;
        unsigned int ucs = euc_jisx0213_1_to_ucs2[e0 - 0xa1][e1 - 0xa1];
        return eucj2utf_emit_utf(ucs, 2, outptr, outroom, outchars);
    }
    return ILLEGAL_SEQUENCE;
}

/*=================================================================
 * ISO2022-JP
 */

/* ISO2022-JP{-1(,2),3} -> EUC_JP
 * Strategy: accepts as many possibilities as possible.
 * The following escape sequence is recognized:
 * (See Lunde, CJKV information processing, O'Reilly, pp.155--158)
 *
 *  <ESC> ( B     ASCII
 *  <ESC> ( J     JIS-Roman
 *  <ESC> ( H     JIS-Roman (for compatibility)
 *  <ESC> ( I     Half-width katakana (JIS X 0201 kana)
 *  <ESC> $ @     JIS C 6226-1978 (78JIS)
 *  <ESC> $ B     JIS X 0208-1983 (83JIS)
 *  <ESC> $ ( D   JIS X 0212-1990
 *  <ESC> $ ( O   JIS X 0213:2000 plane 1
 *  <ESC> $ ( P   JIS X 0213:2000 plane 2
 *  <ESC> & @ <ESC> $ B   JIS X 0208-1990, JIS X 0208:1997
 *  0x0e          JIS7 half-width katakana shift-out
 *  0x0f          JIS7 half-width katakana shift-in
 *
 * The state is reset to ASCII whenever newline character is read.
 *
 * The following escape sequences defined in ISO2022-JP-2 are recognized,
 * but all the characters within the sequence will be replaced by '?'.
 *
 *  <ESC> $ A     (GB2312-80) unsupported
 *  <ESC> $ ( C   (KS X 1001:1992) unsupported
 *  <ESC> . A     (ISO8859-1:1998) unsupported
 *  <ESC> . F     (ISO8859-7:1998) unsupported
 *
 * If other escape sequence is seen, the converter returns ILLEGAL_SEQUENCE.
 *
 * JIS8 kana is allowed.
 */

/* input states */
enum {
    JIS_ASCII,
    JIS_ROMAN,
    JIS_KANA,
    JIS_78,
    JIS_0212,
    JIS_0213_1,
    JIS_0213_2,
    JIS_UNKNOWN,
};

/* deal with escape sequence.  escape byte itself is already consumed.
   returns # of input bytes consumed by the escape sequence,
   or an error code.  cinfo->istate is updated accordingly. */
static size_t jis_esc(ScmConvInfo *cinfo, const char *inptr, size_t inroom)
{
    INCHK(2);
    unsigned char j1 = inptr[0];
    unsigned char j2 = inptr[1];
    switch (j1) {
    case '(':
        switch (j2) {
        case 'B': cinfo->istate = JIS_ASCII; break;
        case 'J': cinfo->istate = JIS_ROMAN; break;
        case 'H': cinfo->istate = JIS_ROMAN; break;
        case 'I': cinfo->istate = JIS_KANA;  break;
        default: return ILLEGAL_SEQUENCE;
        }
        return 2;
    case '$':
        switch (j2) {
        case '@': cinfo->istate = JIS_78; break;
        case 'B': cinfo->istate =  JIS_0213_1; break;
        case 'A': cinfo->istate =  JIS_UNKNOWN; break;
        case '(':
            {
                INCHK(3);
                switch (inptr[2]) {
                case 'D': cinfo->istate = JIS_0212; break;
                case 'O': cinfo->istate = JIS_0213_1; break;
                case 'P': cinfo->istate = JIS_0213_2; break;
                case 'C': cinfo->istate = JIS_UNKNOWN; break;
                default:  return ILLEGAL_SEQUENCE;
                }
                return 3;
                break;
            }
        default: return ILLEGAL_SEQUENCE;
        }
        return 2;
    case '&':
        {
            INCHK(6);
            if (inptr[2] == '@' && inptr[3] == 0x1b && inptr[4] == '$'
                && inptr[5] == 'B') {
                cinfo->istate = JIS_0213_1;
                return 5;
            } else {
                return ILLEGAL_SEQUENCE;
            }
        }
    case '.':
        switch (inptr[2]) {
        case 'A':/*fallthrough*/;
        case 'F':   cinfo->istate = JIS_UNKNOWN; break;
        default:    return ILLEGAL_SEQUENCE;
        }
        return 2;
    default: return ILLEGAL_SEQUENCE;
    }
}

/* main routine for iso2022-jp -> euc_jp */
static size_t jis2eucj(ScmConvInfo *cinfo, const char *inptr, size_t inroom,
                       char *outptr, size_t outroom, size_t *outchars)
{
    size_t inoffset = 0;

    unsigned char j0 = inptr[inoffset];
    /* skip escape sequence */
    while (j0 == 0x1b) {
        inoffset++;
        size_t r = jis_esc(cinfo, inptr+inoffset, inroom-inoffset);
        if (ERRP(r)) return r;
        inoffset += r;
        if (inoffset >= inroom) {
            *outchars = 0;
            return inoffset;
        }
        j0 = inptr[inoffset];
    }

    if (j0 == '\n' || j0 == '\r') {
        cinfo->istate = JIS_ASCII;
        outptr[0] = j0;
        *outchars = 1;
        return 1+inoffset;
    } else if (j0 < 0x20) {
        outptr[0] = j0;
        *outchars = 1;
        return 1+inoffset;
    } else if (j0 >= 0xa1 && j0 <= 0xdf) {
        /* JIS8 kana */
        OUTCHK(2);
        outptr[0] = 0x8e;
        outptr[1] = j0;
        *outchars = 2;
        return 1+inoffset;
    } else {
        switch (cinfo->istate) {
        case JIS_ROMAN:
            /* jis-roman and ascii differs on 0x5c and 0x7e -- for now,
               I ignore the difference. */
            /* FALLTHROUGH */
        case JIS_ASCII:
            outptr[0] = j0;
            *outchars = 1;
            return 1+inoffset;
        case JIS_KANA:
            OUTCHK(2);
            outptr[0] = 0x8e;
            outptr[1] = j0 + 0x80;
            *outchars = 2;
            return 1+inoffset;
        case JIS_78:
            /* for now, I ignore the difference between JIS78 and JIS83 */
            /* FALLTHROUGH */
        case JIS_0213_1: {
            INCHK(inoffset+2);
            OUTCHK(2);
            unsigned char j1 = inptr[inoffset+1];
            outptr[0] = j0 + 0x80;
            outptr[1] = j1 + 0x80;
            *outchars = 2;
            return 2+inoffset;
        }
        case JIS_0212:
            /* jis x 0212 and jis x 0213 plane 2 are different character sets,
               but uses the same conversion scheme. */
            /* FALLTHROUGH */
        case JIS_0213_2: {
            INCHK(inoffset+2);
            OUTCHK(3);
            unsigned char j1 = inptr[inoffset+1];
            outptr[0] = 0x8f;
            outptr[1] = j0 + 0x80;
            outptr[2] = j1 + 0x80;
            *outchars = 3;
            return 2+inoffset;
        }
        case JIS_UNKNOWN:
            outptr[0] = SUBST1_CHAR;
            *outchars = 1;
            return 1+inoffset;
        default:
            /* Can't be here */
            Scm_Panic("internal state of ISO2022-JP -> EUC_JP got messed up (%d).  Implementation error?", cinfo->istate);
        }
    }
    return ILLEGAL_SEQUENCE;
}

/* EUC_JP -> ISO2022JP(-3)
 *
 * For now, I follow the strategy of iso2022jp-3-compatible behavior.
 */

/* ensure the current state is newstate.  returns # of output chars.
   may return OUTPUT_NOT_ENOUGH. */
static size_t jis_ensure_state(ScmConvInfo *cinfo, int newstate, size_t outbytes,
                               char *outptr, size_t outroom)
{
    const char *escseq = NULL;
    size_t esclen = 0;

    if (cinfo->ostate == newstate) {
        OUTCHK(outbytes);
        return 0;
    }
    switch (newstate) {
    case JIS_ASCII:
        escseq = "\033(B";  esclen = 3; break;
    case JIS_KANA:
        escseq = "\033(I";  esclen = 3; break;
    case JIS_0213_1:
        escseq = "\033$B";  esclen = 3; break;
    case JIS_0213_2:
        escseq = "\033$(P"; esclen = 4; break;
    case JIS_0212:
        escseq = "\033$(D"; esclen = 4; break;
    default:
        /* Can't be here */
        Scm_Panic("something wrong in jis_ensure_state: implementation error?");
        return 0;               /* dummy */
    }
    OUTCHK(esclen + outbytes);
    memcpy(outptr, escseq, esclen);
    cinfo->ostate = newstate;
    return esclen;
}

static size_t eucj2jis(ScmConvInfo *cinfo, const char *inptr, size_t inroom,
                       char *outptr, size_t outroom, size_t *outchars)
{
    unsigned char e0 = inptr[0];
    if (e0 < 0x80) {
        size_t outoffset = jis_ensure_state(cinfo, JIS_ASCII, 1, outptr, outroom);
        if (ERRP(outoffset)) return outoffset;
        outptr[outoffset] = e0;
        *outchars = outoffset+1;
        return 1;
    } else if (e0 == 0x8e) {
        INCHK(2);
        unsigned char e1 = inptr[1];
        if (e1 > 0xa0 && e1 < 0xff) {
            size_t outoffset = jis_ensure_state(cinfo, JIS_KANA, 1, outptr, outroom);
            if (ERRP(outoffset)) return outoffset;
            outptr[outoffset] = e1 - 0x80;
            *outchars = outoffset+1;
            return 2;
        }
    } else if (e0 == 0x8f) {
        INCHK(3);
        e0 = inptr[1];
        unsigned char e1 = inptr[2];
        if (e0 > 0xa0 && e0 < 0xff && e1 > 0xa0 && e1 < 0xff) {
            int newstate = JIS_0212;
            switch (e0) {
            case 0xa1:; case 0xa3:; case 0xa4:; case 0xa5:;
            case 0xa8:; case 0xac:; case 0xad:; case 0xae:; case 0xaf:;
                newstate = JIS_0213_2; break;
            default:
                if (e0 >= 0xee) newstate = JIS_0213_2;
            }
            size_t outoffset = jis_ensure_state(cinfo, newstate, 2, outptr, outroom);
            outptr[outoffset] = e0 - 0x80;
            outptr[outoffset+1] = e1 - 0x80;
            *outchars = outoffset+1;
            return 3;
        }
    } else if (e0 > 0xa0 && e0 < 0xff) {
        INCHK(2);
        unsigned char e1 = inptr[1];
        if (e1 > 0xa0 && e1 < 0xff) {
            size_t outoffset = jis_ensure_state(cinfo, JIS_0213_1, 2, outptr, outroom);
            if (ERRP(outoffset)) return outoffset;
            outptr[outoffset] = e0 - 0x80;
            outptr[outoffset+1] = e1 - 0x80;
            *outchars = outoffset+2;
            return 2;
        }
    }
    return ILLEGAL_SEQUENCE;
}

/* reset proc */
static size_t jis_reset(ScmConvInfo *cinfo, char *outptr, size_t outroom)
{
    if (outptr == NULL) {
        /* just reset */
        cinfo->ostate = JIS_ASCII;
        return 0;
    } else {
        if (cinfo->ostate == JIS_ASCII) return 0;
        if (outroom < 3) return OUTPUT_NOT_ENOUGH;
        outptr[0] = 0x1b;
        outptr[1] = '(';
        outptr[2] = 'B';
        cinfo->ostate = JIS_ASCII;
        return 3;
    }
}

/*=================================================================
 * EUC_JP
 */

/* EUC_JP is a pivot code, so we don't need to convert.  This function
   is just a placeholder. */
static size_t pivot(ScmConvInfo *cinfo, const char *inptr, size_t inroom,
                    char *outptr, size_t outroom, size_t *outchars)
{
    return 0;
}

/*=================================================================
 * JCONV - the entry
 */

/* canonical code designator */
enum {
    JCODE_EUCJ,
    JCODE_SJIS,
    JCODE_UTF8,
    JCODE_ISO2022JP,
    JCODE_NONE,    /* a special entry standing for byte stream */
#if 0
    JCODE_ISO2022JP-2,
    JCODE_ISO2022JP-3
#endif
};

/* map canonical code designator to inconv and outconv.  the order of
   entry must match with the above designators. */
static struct conv_converter_rec {
    ScmConvProc inconv;
    ScmConvProc outconv;
    ScmConvReset reset;
} conv_converter[] = {
    { pivot, pivot, NULL },              /* EUCJ */
    { sjis2eucj, eucj2sjis, NULL },      /* SJIS */
    { utf2eucj,  eucj2utf,  NULL },      /* UTF8 */
    { jis2eucj,  eucj2jis,  jis_reset }, /* ISO2022JP */
    { pivot, pivot, NULL },              /* NONE */
};

/* map convesion name to the canonical code */
static struct conv_support_rec {
    const char *name;
    int code;
} conv_supports[] = {
    { "euc_jp",       JCODE_EUCJ },
    { "eucjp",        JCODE_EUCJ },
    { "eucj",         JCODE_EUCJ },
    { "euc_jisx0213", JCODE_EUCJ },
    { "shift_jis",    JCODE_SJIS },
    { "shiftjis",     JCODE_SJIS },
    { "sjis",         JCODE_SJIS },
    { "utf-8",        JCODE_UTF8 },
    { "utf8",         JCODE_UTF8 },
    { "iso2022jp",    JCODE_ISO2022JP },
    { "iso2022-jp",   JCODE_ISO2022JP },
    { "iso-2022-jp",  JCODE_ISO2022JP },
    { "csiso2022jp",  JCODE_ISO2022JP },
    { "iso2022jp-1",  JCODE_ISO2022JP },
    { "iso-2022jp-1", JCODE_ISO2022JP },
    { "iso2022jp-2",  JCODE_ISO2022JP },
    { "iso-2022jp-2", JCODE_ISO2022JP },
    { "iso2022jp-3",  JCODE_ISO2022JP },
    { "iso-2022jp-3", JCODE_ISO2022JP },
    { "none",         JCODE_NONE },
    { NULL, 0 }
};

static int conv_name_match(const char *s, const char *t)
{
    const char *p, *q;
    for (p=s, q=t; *p && *q; p++, q++) {
        if (*p == '-' || *p == '_') {
            if (*q != '-' && *q != '_') return FALSE;
        } else {
            if (tolower(*p) != tolower(*q)) return FALSE;
        }
    }
    if (*p || *q) return FALSE;
    return TRUE;
}

static int conv_name_find(const char *name)
{
    struct conv_support_rec *cvtab = conv_supports;
    for (; cvtab->name; cvtab++) {
        if (conv_name_match(name, cvtab->name)) {
            return cvtab->code;
        }
    }
    return -1;
}

/* Internal conversion handler.
   There are five cases to handle:
   (1) fromCode === toCode
     jconv just copies input to output.  I take speed than safety; input
     is not checked if it is conforming fromCode.
   (2) fromCode === pivot, toCode =/= pivot, and pivot->toCode supported.
   (3) fromCode =/= pivot, toCode === pivot, and fromCode->pivot supported.
     we just need one conversion subroutine.
   (4) fromCode =/= pivot, toCode =/= pivot, and fromCode->pivot->toCode
     supported.  we use two conversion subroutine cascaded.
   (5) other cases;
     we delegate the job to iconv.
*/

/* case (1) */
static size_t jconv_ident(ScmConvInfo *info, const char **iptr,
                          size_t *iroom, char **optr, size_t *oroom)
{
    size_t inroom = *iroom, outroom = *oroom;
#ifdef JCONV_DEBUG
    fprintf(stderr, "jconv_ident %s->%s\n", info->fromCode, info->toCode);
#endif
    if (inroom <= outroom) {
        memcpy(*optr, *iptr, inroom);
        *optr += inroom;
        *iptr += inroom;
        *iroom = 0;
        *oroom -= inroom;
        return inroom;
    } else {
        memcpy(*optr, *iptr, outroom);
        *optr += outroom;
        *iptr += outroom;
        *iroom -= outroom;
        *oroom = 0;
        return OUTPUT_NOT_ENOUGH;
    }
}

/* case (2) or (3) */
static size_t jconv_1tier(ScmConvInfo *info, const char **iptr,
                          size_t *iroom, char **optr, size_t *oroom)
{
    ScmConvProc cvt = info->convproc[0];
    const char *inp = *iptr;
    char *outp = *optr;
    int inr = (int)*iroom, outr = (int)*oroom;
    size_t converted = 0;

#ifdef JCONV_DEBUG
    fprintf(stderr, "jconv_1tier %s->%s\n", info->fromCode, info->toCode);
#endif
    SCM_ASSERT(cvt != NULL);
    while (inr > 0 && outr > 0) {
        size_t outchars;
        size_t inchars = cvt(info, inp, inr, outp, outr, &outchars);
        if (ERRP(inchars)) {
            converted = inchars;
            break;
        } else {
            converted += inchars;
            inp += inchars;
            inr -= (int)inchars;
            outp += outchars;
            outr -= (int)outchars;
        }
    }
    *iptr = inp;
    *iroom = inr;
    *optr = outp;
    *oroom = outr;
    return converted;
}

/* case (4) */
#define INTBUFSIZ 20            /* intermediate buffer size */
static size_t jconv_2tier(ScmConvInfo *info, const char **iptr, size_t *iroom,
                          char **optr, size_t *oroom)
{
    char buf[INTBUFSIZ];
    ScmConvProc icvt = info->convproc[0];
    ScmConvProc ocvt = info->convproc[1];
    const char *inp = *iptr;
    char *outp = *optr;
    int inr = (int)*iroom, outr = (int)*oroom;
    size_t converted = 0;

#ifdef JCONV_DEBUG
    fprintf(stderr, "jconv_2tier %s->%s\n", info->fromCode, info->toCode);
#endif
    while (inr > 0 && outr > 0) {
        size_t outchars, bufchars;
        size_t inchars = icvt(info, inp, inr, buf, INTBUFSIZ, &bufchars);
        if (ERRP(inchars)) {
            converted = inchars;
            break;
        }
        if (bufchars == 0) {
            outchars = 0;
        } else {
            bufchars = ocvt(info, buf, bufchars, outp, outr, &outchars);
            if (ERRP(bufchars)) {
                converted = bufchars;
                break;
            }
        }
        converted += inchars;
        inp += inchars;
        inr -= (int)inchars;
        outp += outchars;
        outr -= (int)outchars;
    }
    *iptr = inp;
    *iroom = inr;
    *optr = outp;
    *oroom = outr;
    return converted;
}

/* case (5) */
#ifdef HAVE_ICONV_H
/* NB: although iconv manages states, we need to keep track of whether
 * we're sure in default status (JIS_ASCII) or not (we use JIS_UNKNOWN for it).
 * It's because jconv_iconv_reset will be called twice if there is any
 * reset sequence; the first call should emit the sequence, but the second
 * call shouldn't.
 */
static size_t jconv_iconv(ScmConvInfo *info, const char **iptr, size_t *iroom,
                          char **optr, size_t *oroom)
{
#ifdef JCONV_DEBUG
    fprintf(stderr, "jconv_iconv %s->%s\n", info->fromCode, info->toCode);
#endif
    size_t r = iconv(info->handle, (char **)iptr, iroom, optr, oroom);
    info->ostate = JIS_UNKNOWN;
    if (r == (size_t)-1) {
        if (errno == EINVAL) return INPUT_NOT_ENOUGH;
        if (errno == E2BIG)  return OUTPUT_NOT_ENOUGH;
        return ILLEGAL_SEQUENCE;
    } else {
        return (int)r;
    }
}

/* reset routine for iconv */
static size_t jconv_iconv_reset(ScmConvInfo *info, char *optr, size_t oroom)
{
    size_t oroom_prev = oroom;
    if (info->ostate == JIS_ASCII) return 0;
    size_t r = iconv(info->handle, NULL, 0, &optr, &oroom);
    if (r == (size_t)-1) {
        if (errno == E2BIG)  return OUTPUT_NOT_ENOUGH;
        Scm_Panic("jconv_iconv_reset: unknown error number %d\n", errno);
    }
    info->ostate = JIS_ASCII;
    return oroom_prev - oroom;
}
#endif /*HAVE_ICONV_H*/

/*------------------------------------------------------------------
 * JCONV_OPEN
 *  Returns ScmConvInfo, setting up some fields.
 *  If no conversion is possible, returns NULL.
 */
ScmConvInfo *jconv_open(const char *toCode, const char *fromCode)
{
    ScmConvHandler handler = NULL;
    ScmConvProc convproc[2];
    ScmConvReset reset;
    iconv_t handle = (iconv_t)-1;

    int incode  = conv_name_find(fromCode);
    int outcode = conv_name_find(toCode);

    if (incode == JCODE_NONE || outcode == JCODE_NONE) {
        /* conversion to/from none means no conversion */
        handler = jconv_ident;
        convproc[0] = convproc[1] = NULL;
        reset = NULL;
    } else if (incode < 0 || outcode < 0) {
#ifdef HAVE_ICONV_H
        /* try iconv */
        handle = iconv_open(toCode, fromCode);
        if (handle == (iconv_t)-1) return NULL;
        handler = jconv_iconv;
        convproc[0] = convproc[1] = NULL;
        reset = jconv_iconv_reset;
#else /*!HAVE_ICONV_H*/
        return NULL;
#endif
    } else if (incode == outcode) {
        /* pattern (1) */
        handler = jconv_ident;
        convproc[0] = convproc[1] = NULL;
        reset = NULL;
    } else if (incode == JCODE_EUCJ) {
        /* pattern (2) */
        handler = jconv_1tier;
        convproc[0] = conv_converter[outcode].outconv;
        convproc[1] = NULL;
        reset = conv_converter[outcode].reset;
    } else if (outcode == JCODE_EUCJ) {
        /* pattern (3) */
        handler = jconv_1tier;
        convproc[0] = conv_converter[incode].inconv;
        convproc[1] = NULL;
        reset = NULL;
    } else {
        /* pattern (4) */
        handler = jconv_2tier;
        convproc[0] = conv_converter[incode].inconv;
        convproc[1] = conv_converter[outcode].outconv;
        reset = conv_converter[outcode].reset;
    }
    ScmConvInfo *info;
    info = SCM_NEW(ScmConvInfo);
    info->jconv = handler;
    info->convproc[0] = convproc[0];
    info->convproc[1] = convproc[1];
    info->reset = reset;
    info->handle = handle;
    info->toCode = toCode;
    info->istate = info->ostate = JIS_ASCII;
    info->fromCode = fromCode;
    return info;
}

/*------------------------------------------------------------------
 * JCONV_CLOSE
 */
int jconv_close(ScmConvInfo *info)
{
    int r = 0;
#ifdef HAVE_ICONV_H
    if (info->handle != (iconv_t)-1) {
        r = iconv_close(info->handle);
        info->handle = (iconv_t)-1;
    }
#endif /*HAVE_ICONV_H*/
    return r;
}

/*------------------------------------------------------------------
 * JCONV - main conversion routine
 */
size_t jconv(ScmConvInfo *info,
             const char **inptr, size_t *inroom,
             char **outptr, size_t *outroom)
{
    SCM_ASSERT(info->jconv != NULL);
    return info->jconv(info, inptr, inroom, outptr, outroom);
}

/*------------------------------------------------------------------
 * JCONV_RESET - reset
 */
size_t jconv_reset(ScmConvInfo *info, char *outptr, size_t outroom)
{
    if (info->reset) {
        return info->reset(info, outptr, outroom);
    } else {
        return 0;
    }
}
