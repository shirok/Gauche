/*
 * jconv.c - alternative japanese code conversion routines
 *
 *   Copyright (c) 2000-2020  Shiro Kawai  <shiro@acm.org>
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
 */

#include <ctype.h>
#include "charconv.h"
#include "jconv_tab.h"

#define INCHK(n)   do{if ((int)inroom < (n)) return INPUT_NOT_ENOUGH;}while(0)
#define OUTCHK(n)  do{if ((int)outroom < (n)) return OUTPUT_NOT_ENOUGH;}while(0)

#define ERRP(n)    ((n) < 0)

/* Fill outptr with substitution character.  Can return jconv error code. */
static inline int do_subst(ScmConvInfo *cinfo,
                           char *outptr, 
                           ScmSize outroom,
                           ScmSize *outchars)
{
    if (cinfo->replaceSize == 0) {
        return NO_OUTPUT_CHAR;
    }
    OUTCHK(cinfo->replaceSize);
    for (int i = 0; i < cinfo->replaceSize; i++) {
        outptr[i] = cinfo->replaceSeq[i];
    }
    *outchars = cinfo->replaceSize;
    return cinfo->replaceSize;
}

#define DO_SUBST                                                \
    do {                                                        \
        int i = do_subst(cinfo, outptr, outroom, outchars);     \
        if (i < 0) return i;                                    \
    } while (0)

/******************************************************************
 * 
 *  Single-unit handling routines
 *
 *  This section defines routines that converts single input unit
 *  to single output unit, optionally affecting the state.
 *  A unit is usually a character, but sometimes one input character
 *  may be mapped to more than one output characters, or a sequence of
 *  input characters is mapped to one output character.
 *
 *  The routine returns the number of input octets consumed, and
 *  sets the number of output octets emitted in *outchars.
 *  If an errornous condition occurs, it returns one of the following
 *  error code, and not update *outchars.
 *
 *  ILLEGAL_SEQUENCE  - Input contains illegal sequence.
 *  INPUT_NOT_ENOUGH  - Input sequence ends prematurely.
 *  OUTPUT_NOT_ENOUGH - Output buffer is too small.
 *  NO_OUTPUT_CHAR    - Input unit can't be represented in output CES.
 *
 *****************************************************************/

/*=================================================================
 * EUC-JP
 */

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

static ScmSize eucj_sjis(ScmConvInfo *cinfo SCM_UNUSED,
                         const char *inptr, ScmSize inroom,
                         char *outptr, ScmSize outroom,
                         ScmSize *outchars)
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
            DO_SUBST;
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
            DO_SUBST;
        } else {
            outptr[0] = e2;
            *outchars = 1;
        }
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
            DO_SUBST;
            return 3;
        }
        if (e1 >= 0xee) {
            s1 = (e1 - 0xa0 + 0x19b)/2;
        } else if (e1 >= 0xb0) {
            DO_SUBST;
            return 3;
        } else {
            s1 = cvt[e1-0xa1];
            if (s1 == 0) {
                DO_SUBST;
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
    DO_SUBST;
    return 1;
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

/* Returns # of input chars, or negative error code on error */
int jconv_utf8_to_ucs4(const char *cp, ScmSize size, ScmChar *ucs)
{
    u_char u0 = cp[0];
    if (u0 < 0x80) {
        *ucs = u0;
        return 1;
    } else if (u0 < 0xc0) {
        return ILLEGAL_SEQUENCE;
    } else if (u0 < 0xe0) {
        if (size < 2) return INPUT_NOT_ENOUGH;
        u_char u1 = cp[1];
        ScmChar ch = ((u0 & 0x1f) << 6) | (u1 & 0x3f);
        if (ch < 0x80) return ILLEGAL_SEQUENCE;
        *ucs = ch;
        return 2;
    } else if (u0 < 0xf0) {
        if (size < 3) return INPUT_NOT_ENOUGH;
        u_char u1 = cp[1], u2 = cp[2];
        ScmChar ch = ((u0 & 0x0f) << 12) | ((u1 & 0x3f) << 6) | (u2 & 0x3f);
        if (ch < 0x800) return ILLEGAL_SEQUENCE;
        *ucs = ch;
        return 3;
    } else if (u0 < 0xf8) {
        if (size < 4) return INPUT_NOT_ENOUGH;
        u_char u1 = cp[1], u2 = cp[2], u3 = cp[3];
        ScmChar ch = ((u0 & 0x07) << 18) | ((u1 & 0x3f) << 12)
            | ((u2 & 0x3f) << 6) | (u3 & 0x3f);
        if (ch < 0x10000) return ILLEGAL_SEQUENCE;
        *ucs = ch;
        return 4;
    } else if (u0 < 0xfc) {
        if (size < 5) return INPUT_NOT_ENOUGH;
        u_char u1 = cp[1], u2 = cp[2], u3 = cp[3], u4 = cp[4];
        ScmChar ch = ((u0 & 0x03) << 24) | ((u1 & 0x3f) << 18)
            | ((u2 & 0x3f) << 12) | ((u3 & 0x3f) << 6) | (u4 & 0x3f);
        if (ch < 0x8000000) return ILLEGAL_SEQUENCE;
        *ucs = ch;
        return 5;
    } else if (u0 < 0xfe) {
        if (size < 6) return INPUT_NOT_ENOUGH;
        u_char u1 = cp[1], u2 = cp[2], u3 = cp[3], u4 = cp[4], u5 = cp[5];
        ScmChar ch = ((u0 & 0x01) << 30) | ((u1 & 0x3f) << 24)
            | ((u2 & 0x3f) << 18) | ((u3 & 0x3f) << 12)
            | ((u4 & 0x3f) << 6) | (u5 & 0x3f);
        *ucs = ch;
        return 6;
    } else {
        return ILLEGAL_SEQUENCE;
    }
}

/* Given 'encoded' ucs, emit utf8.  'Encoded' ucs is the entry of the
   conversion table.  If ucs >= 0x100000, it is composed by two UCS2
   character.  Otherwise, it is one UCS4 character. */
static inline ScmSize eucj_utf8_emit_utf(unsigned int ucs, ScmSize inchars,
                                         char *outptr, ScmSize outroom,
                                         ScmSize *outchars)
{
    if (ucs < 0x100000) {
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

static ScmSize eucj_utf8(ScmConvInfo *cinfo SCM_UNUSED,
                         const char *inptr, ScmSize inroom,
                         char *outptr, ScmSize outroom, ScmSize *outchars)
{
    unsigned char e0 = (unsigned char)inptr[0];
    if (e0 < 0xa0) {
        if (e0 == 0x8e) {
            /* JIS X 0201 KANA */
            INCHK(2);
            unsigned char e1 = (unsigned char)inptr[1];
            if (e1 < 0xa1 || e1 > 0xdf) return ILLEGAL_SEQUENCE;
            unsigned int ucs = 0xff61 + (e1 - 0xa1);
            return eucj_utf8_emit_utf(ucs, 2, outptr, outroom, outchars);
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
                DO_SUBST;
                return 3;
            }
            unsigned int ucs = euc_jisx0213_2_to_ucs2[index][e2 - 0xa1];
            if (ucs != 0) {
                return eucj_utf8_emit_utf(ucs, 3, outptr, outroom, outchars);
            }
            DO_SUBST;
            return 3;
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
        if (ucs != 0) {
            return eucj_utf8_emit_utf(ucs, 2, outptr, outroom, outchars);
        }
        DO_SUBST;
        return 2;
    }
    /* e0 == 0xa0 */
    DO_SUBST;
    return 1;
}

/* EUC_JP -> ISO8859-1 */
static ScmSize eucj_lat1(ScmConvInfo *cinfo,
                         const char *inptr, ScmSize inroom,
                         char *outptr, ScmSize outroom, ScmSize *outchars)
{
    char u[6];
    ScmSize nu;
    ScmSize r = eucj_utf8(cinfo, inptr, inroom, u, 6, &nu);
    if (r < 0) return r;
    ScmChar ch;
    ScmSize r2 = jconv_utf8_to_ucs4(u, nu, &ch);
    if (r2 < 0) return r2;
    if (ch < 0x100) {
        *outptr = ch;
        *outchars = 1;
    } else {
        DO_SUBST;
    }
    return r;
}

/* EUC_JP -> ISO2022JP(-3)
 *
 * For now, I follow the strategy of iso2022jp-3-compatible behavior.
 */

/* ensure the current state is newstate.  returns # of output chars.
   may return OUTPUT_NOT_ENOUGH. */
static ScmSize jis_ensure_state(ScmConvInfo *cinfo, int newstate, 
                                ScmSize outbytes, 
                                char *outptr, ScmSize outroom)
{
    const char *escseq = NULL;
    ScmSize esclen = 0;

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

static ScmSize eucj_jis(ScmConvInfo *cinfo, const char *inptr, ScmSize inroom,
                        char *outptr, ScmSize outroom, ScmSize *outchars)
{
    unsigned char e0 = inptr[0];
    if (e0 < 0x80) {
        ScmSize outoffset = jis_ensure_state(cinfo, JIS_ASCII, 1, outptr, outroom);
        if (ERRP(outoffset)) return outoffset;
        outptr[outoffset] = e0;
        *outchars = outoffset+1;
        return 1;
    } else if (e0 == 0x8e) {
        INCHK(2);
        unsigned char e1 = inptr[1];
        if (e1 > 0xa0 && e1 < 0xff) {
            ScmSize outoffset = jis_ensure_state(cinfo, JIS_KANA, 1, outptr, outroom);
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
            ScmSize outoffset = jis_ensure_state(cinfo, newstate, 2, outptr, outroom);
            outptr[outoffset] = e0 - 0x80;
            outptr[outoffset+1] = e1 - 0x80;
            *outchars = outoffset+1;
            return 3;
        }
    } else if (e0 > 0xa0 && e0 < 0xff) {
        INCHK(2);
        unsigned char e1 = inptr[1];
        if (e1 > 0xa0 && e1 < 0xff) {
            ScmSize outoffset = jis_ensure_state(cinfo, JIS_0213_1, 2, outptr, outroom);
            if (ERRP(outoffset)) return outoffset;
            outptr[outoffset] = e0 - 0x80;
            outptr[outoffset+1] = e1 - 0x80;
            *outchars = outoffset+2;
            return 2;
        }
    }
    return ILLEGAL_SEQUENCE;
}


/* EUC-JP -> ASCII */
static ScmSize eucj_ascii(ScmConvInfo *cinfo,
                          const char *inptr, ScmSize inroom,
                          char *outptr, ScmSize outroom,
                          ScmSize *outchars)
{
    unsigned char e1 = inptr[0];
    if (e1 <= 0x7f) {
        outptr[0] = e1;
        *outchars = 1;
        return 1;
    }
    if (e1 >= 0xa1 && e1 <= 0xfe) {
        /* double byte char (JISX 0213 plane 1) */
        INCHK(2);
        DO_SUBST;
        return 2;
    }
    if (e1 == 0x8e) {
        INCHK(2);
        DO_SUBST;
        return 2;
    }
    if (e1 == 0x8f) {
        INCHK(3);
        DO_SUBST;
        return 3;
    }
    DO_SUBST;
    return 1;
}

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

static ScmSize sjis_eucj(ScmConvInfo *cinfo SCM_UNUSED,
                         const char *inptr, ScmSize inroom,
                         char *outptr, ScmSize outroom, 
                         ScmSize *outchars)
{
    static const unsigned char cvt[] = { 0xa1, 0xa8, 0xa3, 0xa4, 0xa5, 0xac, 0xae, 0xad, 0xaf, 0xee };

    unsigned char s1 = inptr[0];
    if (s1 <= 0x7f) {
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
            DO_SUBST;
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
    DO_SUBST;
    return 2;
}

/* SJIS -> ASCII */

static ScmSize sjis_ascii(ScmConvInfo *cinfo,
                          const char *inptr, ScmSize inroom,
                          char *outptr, ScmSize outroom,
                          ScmSize *outchars)
{
    unsigned char s1 = inptr[0];
    if (s1 <= 0x7f) {
        outptr[0] = s1;
        *outchars = 1;
        return 1;
    }
    if ((s1 > 0x80 && s1 < 0xa0) || (s1 >= 0xe0 && s1 < 0xfc)) {
        INCHK(2);
        DO_SUBST;
        *outchars = cinfo->replaceSize;
        return 2;
    }
    else {
        DO_SUBST;
        *outchars = cinfo->replaceSize;
        return 1;
    }
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
static inline ScmSize utf2euc_emit_euc(unsigned short euc, 
                                       ScmSize inchars,
                                       char *outptr, 
                                       ScmSize outroom,
                                       ScmSize *outchars)
{
    if (euc < 0x8000) {
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
static inline ScmSize utf2euc_2(ScmConvInfo *cinfo SCM_UNUSED, unsigned char u0,
                                const char *inptr, ScmSize inroom,
                                char *outptr, ScmSize outroom, 
                                ScmSize *outchars)
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
        unsigned short euc = etab[u1-0x80];
        if (euc != 0) {
            return utf2euc_emit_euc(euc, 2, outptr, outroom, outchars);
        }
    }
    DO_SUBST;
    return 2;
}

/* handle 3-byte UTF8 sequence.  0xe0 <= u0 <= 0xef */
static inline ScmSize utf2euc_3(ScmConvInfo *cinfo SCM_UNUSED, unsigned char u0,
                                const char *inptr, ScmSize inroom,
                                char *outptr, ScmSize outroom, 
                                ScmSize *outchars)
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
            unsigned short euc = tab2[ind-1][u2-0x80];
            if (euc != 0) {
                return utf2euc_emit_euc(euc, 3, outptr, outroom, outchars);
            }
        }
    }
    DO_SUBST;
    return 3;
}

/* handle 4-byte UTF8 sequence.  u0 == 0xf0, 0xa0 <= u1 <= 0xaa */
static inline ScmSize utf2euc_4(ScmConvInfo *cinfo SCM_UNUSED, unsigned char u0,
                                const char *inptr, ScmSize inroom,
                                char *outptr, ScmSize outroom,
                                ScmSize *outchars)
{
    const unsigned short *tab = NULL;

    INCHK(4);
    if (u0 != 0xf0) {
        DO_SUBST;
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
                unsigned short euc = tab[i+1];
                if (euc != 0) {
                    return utf2euc_emit_euc(euc, 4, outptr, outroom, outchars);
                }
            }
        }
    }
    DO_SUBST;
    return 4;
}

/* Body of UTF8 -> EUC_JP conversion */
static ScmSize utf8_eucj(ScmConvInfo *cinfo,     
                        const char *inptr, ScmSize inroom,
                        char *outptr, ScmSize outroom,
                        ScmSize *outchars)
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
        DO_SUBST;
        return 5;
    }
    if (u0 <= 0xfd) {
        /* 6-byte UTF8 sequence */
        INCHK(6);
        DO_SUBST;
        return 6;
    }
    return ILLEGAL_SEQUENCE;
}

/* [UTF8 <-> SJIS Conversion]
 * We convert a unit via eucjp.
 */

static ScmSize utf8_sjis(ScmConvInfo *cinfo,     
                         const char *inptr, ScmSize inroom,
                         char *outptr, ScmSize outroom,
                         ScmSize *outchars)
{
    char buf[3];
    ScmSize bufcount;
    ScmSize r = utf8_eucj(cinfo, inptr, inroom, buf, 3, &bufcount);
    if (r < 0) return r;
    ScmSize r2 = eucj_sjis(cinfo, buf, bufcount, outptr, outroom, outchars);
    if (r2 < 0) return r2;
    return r;
}

static ScmSize sjis_utf8(ScmConvInfo *cinfo,     
                         const char *inptr, ScmSize inroom,
                         char *outptr, ScmSize outroom,
                         ScmSize *outchars)
{
    char buf[3];
    ScmSize bufcount;
    ScmSize r = sjis_eucj(cinfo, inptr, inroom, buf, 3, &bufcount);
    if (r < 0) return r;
    ScmSize r2 = eucj_utf8(cinfo, buf, bufcount, outptr, outroom, outchars);
    if (r2 < 0) return r2;
    return r;
}

/* UTF8 -> UTF16 */
static ScmSize utf8_utf16(ScmConvInfo *cinfo,     
                          const char *inptr, ScmSize inroom,
                          char *outptr, ScmSize outroom,
                          ScmSize *outchars)
{
    ScmSize reqsize = 0;
    int ostate = cinfo->ostate;
    int need_bom = FALSE;
    ScmChar ch;
    
    if (ostate == UTF_DEFAULT) {
        reqsize += 2;
        need_bom = TRUE;
        ostate = UTF_BE;
    }
    int r = jconv_utf8_to_ucs4(inptr, inroom, &ch);
    if (r < 0) return r;
    if (ch < 0x10000) reqsize += 2;
    else reqsize += 4;
    
    OUTCHK(reqsize);
    if (need_bom) {
        if (ostate == UTF_BE) {
            outptr[0] = 0xfe;
            outptr[1] = 0xff;
        } else {
            outptr[1] = 0xfe;
            outptr[0] = 0xff;
        }
        outptr += 2;
    }
    if (ch < 0x10000) {
        char u[2];
        u[0] = (ch >> 8) & 0xff;
        u[1] = ch & 0xff;
        if (ostate == UTF_BE) {
            outptr[0] = u[0];
            outptr[1] = u[1];
        } else {
            outptr[1] = u[0];
            outptr[0] = u[1];
        }
    } else {
        ch -= 0x10000;
        char u[2];
        u[0] = 0xd8 + ((ch >> 18) & 0x03);
        u[1] = (ch >> 10) & 0xff;
        if (ostate == UTF_BE) {
            outptr[0] = u[0];
            outptr[1] = u[1];
        } else {
            outptr[1] = u[0];
            outptr[0] = u[1];
        }
        u[0] = 0xdc + ((ch >> 8) & 0x03);
        u[1] = ch & 0xff;
        if (ostate == UTF_BE) {
            outptr[2] = u[0];
            outptr[3] = u[1];
        } else {
            outptr[3] = u[0];
            outptr[2] = u[1];
        }
    }
    cinfo->ostate = ostate;
    *outchars = reqsize;
    return r;
}

/* UTF8 -> Latin1 */
static ScmSize utf8_lat1(ScmConvInfo *cinfo,     
                         const char *inptr, ScmSize inroom,
                         char *outptr, ScmSize outroom,
                         ScmSize *outchars)
{
    ScmChar ch;
    int r = jconv_utf8_to_ucs4(inptr, inroom, &ch);
    if (r < 0) return r;
    if (ch < 0x100) {
        *outptr = ch;
        *outchars = 1;
    } else {
        DO_SUBST;
    }
    return r;
}

/* UTF8 -> ASCII */
static ScmSize utf8_ascii(ScmConvInfo *cinfo,     
                          const char *inptr, ScmSize inroom,
                          char *outptr, ScmSize outroom,
                          ScmSize *outchars)
{
    ScmChar ch;
    int r = jconv_utf8_to_ucs4(inptr, inroom, &ch);
    if (r < 0) return r;
    if (ch < 0x80) {
        *outptr = ch;
        *outchars = 1;
    } else {
        DO_SUBST;
    }
    return r;
}

/*=================================================================
 * UTF16
 */

/* For now, we first convert it to utf8, for we already have the table
   directly supports utf8.  Theoretically though, having ucs4 to
   jis table would speed it up. */

static ScmSize utf16_utf8(ScmConvInfo *cinfo,     
                          const char *inptr, ScmSize inroom,
                          char *outptr, ScmSize outroom,
                          ScmSize *outchars)
{
    INCHK(2);
    int istate = cinfo->istate;
    ScmSize inread = 0;
    if (istate == UTF_DEFAULT) {
        if ((u_char)inptr[0] == 0xfe && (u_char)inptr[1] == 0xff) {
            inptr += 2;
            inroom -= 2;
            inread += 2;
            INCHK(2);
            istate = UTF_BE;
        } else if ((u_char)inptr[0] == 0xff && (u_char)inptr[1] == 0xfe) {
            inptr += 2;
            inroom -= 2;
            inread += 2;
            INCHK(2);
            istate = UTF_LE;
        } else {
            /* Arbitrary choice */
            istate = UTF_BE;
        }
    }
    
    u_char u[2];
    if (istate == UTF_BE) {
        u[0] = inptr[0];
        u[1] = inptr[1];
    } else {
        u[0] = inptr[1];
        u[1] = inptr[0];
    }

    ScmChar ch;

    if ((u[0] & 0xdc) == 0xd8) {
        /* surrogate */
        inptr += 2;
        inroom -= 2;
        INCHK(2);
        u_char v[2];
        if (istate == UTF_BE) {
            v[0] = inptr[0];
            v[1] = inptr[1];
        } else {
            v[0] = inptr[1];
            v[1] = inptr[0];
        }
        if ((v[1] & 0xdc) == 0xdc) {
            ch = (((u[0] & 0x03) << 18)
                  | (u[1] << 10)
                  | ((v[0] & 0x03) << 8)
                  | v[1])
                + 0x10000;
            inread += 4;
        } else {
            /* We only have first half of a surrogate pair.
               We leave the second character in the input, and try to
               substitute the first. */
            DO_SUBST;
            cinfo->istate = istate;
            return inread;
        }
    } else if ((u[0] & 0xdc) == 0xdc) {
        /* Stray second half of a surrogate pair. */
        DO_SUBST;
        return inread;
    } else {
        inread += 2;
        ch = (u[0] << 8) + u[1];
    }

    int outreq = UCS2UTF_NBYTES(ch);
    OUTCHK(outreq);
    jconv_ucs4_to_utf8(ch, outptr);
    cinfo->istate = istate;
    *outchars = outreq;
    return inread;
}

/* This handles BOM stuff.   It is pretty twisted, for we need to keep the
   internal state consistent even when we return an error. */
static ScmSize utf16_utf16(ScmConvInfo *cinfo,     
                           const char *inptr, ScmSize inroom,
                           char *outptr, ScmSize outroom,
                           ScmSize *outchars)
{
    ScmSize consumed = 0;
    ScmSize emitted = 0;

    if (cinfo->istate == UTF_DEFAULT || cinfo->ostate == UTF_DEFAULT) {
        /* We come here only at the beginning.  */
        int istate = 0;

        if (cinfo->istate == UTF_DEFAULT) {
            INCHK(2);
            if ((u_char)inptr[0] == 0xfe && (u_char)inptr[1] == 0xff) {
                consumed += 2;
                istate = UTF_BE;
                inptr += 2;
                inroom -= 2;
            } else if ((u_char)inptr[0] == 0xff && (u_char)inptr[1] == 0xfe) {
                consumed += 2;
                istate = UTF_LE;
                inptr += 2;
                inroom -= 2;
            } else {
                istate = UTF_BE;
            }
        }
        INCHK(2);
        if (cinfo->ostate == UTF_DEFAULT) {
            OUTCHK(4);
            outptr[0] = 0xfe;
            outptr[1] = 0xff;
            outptr += 2;
            outroom -= 2;
            emitted += 2;
            cinfo->ostate = UTF_BE;
        } else {
            OUTCHK(2);
        }
        cinfo->istate = istate;
    } else {
        INCHK(2);
        OUTCHK(2);
    }
    
    char u[2];
    if (cinfo->istate == UTF_BE) {
        u[0] = inptr[0];
        u[1] = inptr[1];
    } else {
        u[1] = inptr[0];
        u[0] = inptr[1];
    }
    if (cinfo->ostate == UTF_BE) {
        outptr[0] = u[0];
        outptr[1] = u[1];
    } else {
        outptr[1] = u[0];
        outptr[0] = u[1];
    }
    *outchars = emitted + 2;
    return consumed + 2;
}

static ScmSize utf16_eucj(ScmConvInfo *cinfo,     
                          const char *inptr, ScmSize inroom,
                          char *outptr, ScmSize outroom,
                          ScmSize *outchars)
{
    char buf[6];
    ScmSize nbuf;
    ScmSize r = utf16_utf8(cinfo, inptr, inroom, buf, 6, &nbuf);
    if (r < 0) return r;
    ScmSize r2 = utf8_eucj(cinfo, buf, nbuf, outptr, outroom, outchars);
    if (r2 < 0) return r2;
    return r;
}

static ScmSize eucj_utf16(ScmConvInfo *cinfo,     
                          const char *inptr, ScmSize inroom,
                          char *outptr, ScmSize outroom,
                          ScmSize *outchars)
{
    char buf[6];
    ScmSize nbuf;
    ScmSize r = eucj_utf8(cinfo, inptr, inroom, buf, 6, &nbuf);
    if (r < 0) return r;
    ScmSize r2 = utf8_utf16(cinfo, buf, nbuf, outptr, outroom, outchars);
    if (r2 < 0) return r2;
    return r;
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

/* deal with escape sequence.  escape byte itself is already consumed.
   returns # of input bytes consumed by the escape sequence,
   or an error code.  cinfo->istate is updated accordingly. */
static ScmSize jis_esc(ScmConvInfo *cinfo, const char *inptr, ScmSize inroom)
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
static ScmSize jis_eucj(ScmConvInfo *cinfo, const char *inptr, ScmSize inroom,
                        char *outptr, ScmSize outroom, ScmSize *outchars)
{
    ScmSize inoffset = 0;

    unsigned char j0 = inptr[inoffset];
    /* skip escape sequence */
    while (j0 == 0x1b) {
        inoffset++;
        ScmSize r = jis_esc(cinfo, inptr+inoffset, inroom-inoffset);
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
            DO_SUBST;
            return 1+inoffset;
        default:
            /* Can't be here */
            Scm_Panic("internal state of ISO2022-JP -> EUC_JP got messed up (%d).  Implementation error?", cinfo->istate);
        }
    }
    return ILLEGAL_SEQUENCE;
}

/* reset proc */
static ScmSize jis_reset(ScmConvInfo *cinfo, char *outptr, ScmSize outroom)
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
 * ISO8859-1
 */

static ScmSize lat1_utf8(ScmConvInfo *cinfo SCM_UNUSED,
                         const char *inptr,
                         ScmSize inroom SCM_UNUSED,
                         char *outptr,
                         ScmSize outroom,
                         ScmSize *outchars)
{
    unsigned char c = inptr[0];
    if (c <= 0x7f) {
        outptr[0] = c;
        *outchars = 1;
    } else {
        OUTCHK(2);
        outptr[0] = 0xc0 + (c >> 6);
        outptr[1] = 0x80 + (c & 0x3f);
        *outchars = 2;
    }
    return 1;
}

static ScmSize lat1_eucj(ScmConvInfo *cinfo,
                         const char *inptr,
                         ScmSize inroom,
                         char *outptr,
                         ScmSize outroom,
                         ScmSize *outchars)
{
    char buf[2];
    ScmSize nbuf;
    ScmSize r = lat1_utf8(cinfo, inptr, inroom, buf, 2, &nbuf);
    if (r < 0) return r; 
    r = utf8_eucj(cinfo, buf, nbuf, outptr, outroom, outchars);
    if (r < 0) return r;
    return 1;
}

static ScmSize lat1_ascii(ScmConvInfo *cinfo,
                          const char *inptr,
                          ScmSize inroom SCM_UNUSED,
                          char *outptr,
                          ScmSize outroom,
                          ScmSize *outchars)
{
    unsigned char c = inptr[0];
    if (c <= 0x7f) {
        outptr[0] = c;
        *outchars = 1;
    } else {
        DO_SUBST;
    }
    return 1;
}

/*=================================================================
 * ASCII
 */

/* ASCII -> X */

static ScmSize ascii_x(ScmConvInfo *cinfo SCM_UNUSED,
                       const char *inptr,
                       ScmSize inroom SCM_UNUSED,
                       char *outptr,
                       ScmSize outroom SCM_UNUSED,
                       ScmSize *outchars)
{
    outptr[0] = inptr[0];
    *outchars = 1;
    return 1;
}

static ScmSize ascii_utf16(ScmConvInfo *cinfo SCM_UNUSED,
                           const char *inptr,
                           ScmSize inroom SCM_UNUSED,
                           char *outptr,
                           ScmSize outroom SCM_UNUSED,
                           ScmSize *outchars)
{
    return utf8_utf16(cinfo, inptr, inroom, outptr, outroom, outchars);
}

/*=================================================================
 * Placeholder
 */

static ScmSize ident(ScmConvInfo *cinfo SCM_UNUSED,
                     const char *inptr SCM_UNUSED,
                     ScmSize inroom SCM_UNUSED,
                     char *outptr SCM_UNUSED,
                     ScmSize outroom SCM_UNUSED,
                     ScmSize *outchars SCM_UNUSED)
{
    return 0;
}

/******************************************************************
 * 
 * Actual conversion
 *
 */

/* map canonical code designator to inconv and outconv.  the order of
   entry must match with the above designators. 
   conv_converter[incode][outcode] returns the appropriate combiniation
   of routines.
   NB: It is tedious to maintain this table; we'll eventually generate
   this from some DSL.
*/
struct conv_converter_rec {
    ScmConvProc inconv;
    ScmConvProc outconv;
    ScmConvReset reset;
    int istate;                 /* initial input state */
    int ostate;                 /* initial output state */
};

/* map convesion name to the canonical code */
struct conv_support_rec {
    const char *name;
    int code;
};

#include "jconv_tab.c"

static int conv_name_match(const char *s, const char *t)
{
    const char *p, *q;
    for (p=s, q=t; *p && *q; p++) {
        if (*p == '-' || *p == '_') {
            continue;           /* ignore '-' and '_' */
        } else {
            if (tolower(*p) != tolower(*q)) return FALSE;
            q++;
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
static ScmSize jconv_ident(ScmConvInfo *cinfo SCM_UNUSED, const char **iptr,
                           ScmSize *iroom, char **optr, ScmSize *oroom)
{
    ScmSize inroom = *iroom, outroom = *oroom;
#ifdef JCONV_DEBUG
    fprintf(stderr, "jconv_ident %s->%s\n", cinfo->fromCode, cinfo->toCode);
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
static ScmSize jconv_1tier(ScmConvInfo *cinfo, const char **iptr,
                           ScmSize *iroom, char **optr, ScmSize *oroom)
{
    ScmConvProc cvt = cinfo->convproc[0];
    const char *inp = *iptr;
    char *outp = *optr;
    int inr = (int)*iroom, outr = (int)*oroom;
    ScmSize converted = 0;

#ifdef JCONV_DEBUG
    fprintf(stderr, "jconv_1tier %s->%s\n", cinfo->fromCode, cinfo->toCode);
#endif
    SCM_ASSERT(cvt != NULL);
    while (inr > 0 && outr > 0) {
        ScmSize outchars;
        ScmSize inchars = cvt(cinfo, inp, inr, outp, outr, &outchars);
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
static ScmSize jconv_2tier(ScmConvInfo *cinfo, const char **iptr, ScmSize *iroom,
                           char **optr, ScmSize *oroom)
{
    char buf[INTBUFSIZ];
    ScmConvProc icvt = cinfo->convproc[0];
    ScmConvProc ocvt = cinfo->convproc[1];
    const char *inp = *iptr;
    char *outp = *optr;
    int inr = (int)*iroom, outr = (int)*oroom;
    ScmSize converted = 0;

#ifdef JCONV_DEBUG
    fprintf(stderr, "jconv_2tier %s->%s\n", cinfo->fromCode, cinfo->toCode);
#endif
    while (inr > 0 && outr > 0) {
        ScmSize outchars, bufchars;
        ScmSize inchars = icvt(cinfo, inp, inr, buf, INTBUFSIZ, &bufchars);
        if (ERRP(inchars)) {
            converted = inchars;
            break;
        }
        if (bufchars == 0) {
            outchars = 0;
        } else {
            bufchars = ocvt(cinfo, buf, bufchars, outp, outr, &outchars);
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
static ScmSize jconv_iconv(ScmConvInfo *cinfo, const char **iptr, ScmSize *iroom,
                           char **optr, ScmSize *oroom)
{
#ifdef JCONV_DEBUG
    fprintf(stderr, "jconv_iconv %s->%s\n", cinfo->fromCode, cinfo->toCode);
#endif
    size_t ir = *iroom, or = *oroom;
    size_t r = iconv(cinfo->handle, (char **)iptr, &ir, optr, &or);
    *iroom = ir;
    *oroom = or;
    cinfo->ostate = JIS_UNKNOWN;
    if (r == (size_t)-1) {
        if (errno == EINVAL) return INPUT_NOT_ENOUGH;
        if (errno == E2BIG)  return OUTPUT_NOT_ENOUGH;
        return ILLEGAL_SEQUENCE;
    } else {
        return (ScmSize)r;
    }
}

/* reset routine for iconv */
static ScmSize jconv_iconv_reset(ScmConvInfo *cinfo, char *optr, ScmSize oroom)
{
    ScmSize oroom_prev = oroom;
    if (cinfo->ostate == JIS_ASCII) return 0;
    size_t or = oroom;
    size_t r = iconv(cinfo->handle, NULL, 0, &optr, &or);
    if (r == (size_t)-1) {
        if (errno == E2BIG)  return OUTPUT_NOT_ENOUGH;
        Scm_Panic("jconv_iconv_reset: unknown error number %d\n", errno);
    }
    cinfo->ostate = JIS_ASCII;
    return oroom_prev - (ScmSize)or;
}
#endif /*HAVE_ICONV_H*/

/*------------------------------------------------------------------
 * JCONV_OPEN
 *  Returns ScmConvInfo, setting up some fields.
 *  If no conversion is possible, returns NULL.
 */
ScmConvInfo *jconv_open(const char *toCode, const char *fromCode,
                        int useIconv)
{
    ScmConvHandler handler = NULL;
    ScmConvProc convproc[2] = {NULL, NULL};
    ScmConvReset reset = NULL;
    int istate = 0, ostate = 0;
    iconv_t handle = (iconv_t)-1;

    int incode  = conv_name_find(fromCode);
    int outcode = conv_name_find(toCode);

    if (incode >= 0 && outcode >= 0) {
        convproc[0] = conv_converter[incode][outcode].inconv;
        convproc[1] = conv_converter[incode][outcode].outconv;
        reset = conv_converter[incode][outcode].reset;
        istate = conv_converter[incode][outcode].istate;
        ostate = conv_converter[incode][outcode].ostate;
    }

    if (convproc[0] == NULL) {
        if (useIconv) {
#ifdef HAVE_ICONV_H
            /* try iconv */
            handle = iconv_open(toCode, fromCode);
            if (handle == (iconv_t)-1) return NULL;
            handler = jconv_iconv;
            reset = jconv_iconv_reset;
#else /*!HAVE_ICONV_H*/
            return NULL;
#endif
        } else {
            return NULL;
        }
    } else if (convproc[0] == ident) {
        handler = jconv_ident;
    } else if (convproc[1] == NULL) {
        handler = jconv_1tier;
    } else {
        handler = jconv_2tier;
    }

    ScmConvInfo *cinfo;
    cinfo = SCM_NEW(ScmConvInfo);
    cinfo->jconv = handler;
    cinfo->convproc[0] = convproc[0];
    cinfo->convproc[1] = convproc[1];
    cinfo->reset = reset;
    cinfo->handle = handle;
    cinfo->toCode = toCode;
    cinfo->istate = istate;
    cinfo->ostate = ostate;
    cinfo->fromCode = fromCode;
    /* The replacement settings can be modified by jconv_set_replacement */
    cinfo->replacep = FALSE;
    cinfo->replaceSize = 0;
    cinfo->replaceSeq = NULL;
    return cinfo;
}

/*------------------------------------------------------------------
 * JCONV_SET_REPLACEMENT
 *   Setting up replacement sequence according to the toCode.
 */
void jconv_set_replacement(ScmConvInfo *cinfo)
{
    static ScmObj ces_replacement_proc = SCM_UNDEFINED;
    SCM_BIND_PROC(ces_replacement_proc, "%ces-replacement",
                  Scm_FindModule(SCM_SYMBOL(SCM_INTERN("gauche.charconv")), 0));
    ScmObj replacements = Scm_ApplyRec1(ces_replacement_proc,
                                        SCM_MAKE_STR(cinfo->toCode));
    ScmSize i = Scm_Length(replacements);
    if (i > 0) {
        cinfo->replacep = TRUE;
        cinfo->replaceSize = i;
        char *replaceSeq = SCM_NEW_ATOMIC_ARRAY(char, i);
        for (int j = 0; j < i; j++) {
            SCM_ASSERT(SCM_PAIRP(replacements));
            replaceSeq[j] = SCM_INT_VALUE(SCM_CAR(replacements));
            replacements = SCM_CDR(replacements);
        }
        cinfo->replaceSeq = replaceSeq;
    }
}

/*------------------------------------------------------------------
 * JCONV_CLOSE
 */
int jconv_close(ScmConvInfo *cinfo)
{
    int r = 0;
#ifdef HAVE_ICONV_H
    if (cinfo->handle != (iconv_t)-1) {
        r = iconv_close(cinfo->handle);
        cinfo->handle = (iconv_t)-1;
    }
#endif /*HAVE_ICONV_H*/
    return r;
}

/*------------------------------------------------------------------
 * JCONV - main conversion routine
 */
ScmSize jconv(ScmConvInfo *cinfo,
              const char **inptr, ScmSize *inroom,
              char **outptr, ScmSize *outroom)
{
    SCM_ASSERT(cinfo->jconv != NULL);
    return cinfo->jconv(cinfo, inptr, inroom, outptr, outroom);
}

/*------------------------------------------------------------------
 * JCONV_RESET - reset
 */
ScmSize jconv_reset(ScmConvInfo *cinfo, char *outptr, ScmSize outroom)
{
    if (cinfo->reset) {
        return cinfo->reset(cinfo, outptr, outroom);
    } else {
        return 0;
    }
}
