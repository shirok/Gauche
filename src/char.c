/*
 * char.c - character and character set operations
 *
 *   Copyright (c) 2000-2018  Shiro Kawai  <shiro@acm.org>
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

#include <ctype.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/char_attr.h"
#include "gauche/priv/charP.h"
#include "gauche/priv/vectorP.h"

#include "char_attr.c"          /* generated tables */

/*=======================================================================
 * Character functions
 */

ScmObj Scm_CharEncodingName(void)
{
    return SCM_INTERN(SCM_CHAR_ENCODING_NAME);
}

/* includes encoding-specific auxiliary functions */
#define SCM_CHAR_ENCODING_BODY
#if   defined(GAUCHE_CHAR_ENCODING_EUC_JP)
#include "gauche/char_euc_jp.h"
#elif defined(GAUCHE_CHAR_ENCODING_UTF_8)
#include "gauche/char_utf_8.h"
#elif defined(GAUCHE_CHAR_ENCODING_SJIS)
#include "gauche/char_sjis.h"
#else
#include "gauche/char_none.h"
#endif

const char **Scm_SupportedCharacterEncodings(void)
{
    return supportedCharacterEncodings;
}

int Scm_SupportedCharacterEncodingP(const char *encoding)
{
    const char **cs = supportedCharacterEncodings;
    for (;*cs;cs++) {
        const char *p = *cs;
        const char *q = encoding;
        for (;*p && *q; p++, q++) {
            if (tolower(*p) != tolower(*q)) break;
        }
        if (*p == '\0' && *q == '\0') return TRUE;
    }
    return FALSE;
}

/* '0' -> 0, 'a' -> 10, etc.
   Radix is assumed in the range [2, 36] if non-extended,
   [2, 10] if extended.
   'Extended' means we recognize not only ASCII but all Nd characters.
*/
int Scm_DigitToInt(ScmChar ch, int radix, int extended)
{
    if (ch < '0') return -1;
    if (radix < 2) return -1;
    if (radix <= 10) {
        if (ch < '0' + radix) return (ch - '0');
    } else {
        if (ch <= '9') return (ch - '0');
        if (ch < 'A') return -1;
        if (ch < 'A' + radix - 10) return (ch - 'A' + 10);
        if (ch < 'a') return -1;
        if (ch < 'a' + radix - 10) return (ch - 'a' + 10);
    }
    if (extended && ch > 0x80 && radix <= 10) {
        ScmChar ucschar = Scm_CharToUcs(ch);
        int val = ucs_digit_value(ucschar);
        if (val < 0 || val >= radix) return -1;
        return val;
    } else {
        return -1;
    }
}

ScmChar Scm_IntToDigit(int n, int radix, int basechar1, int basechar2)
{
    if (n < 0) return SCM_CHAR_INVALID;
    if (basechar1 == 0) basechar1 = '0';
    if (basechar2 == 0) basechar2 = 'a';
    if (radix <= 10) {
        if (n < radix) return (ScmChar)(n + basechar1);
        else return SCM_CHAR_INVALID;
    } else {
        if (n < 10) return (ScmChar)(n + basechar1);
        if (n < radix) return (ScmChar)(n - 10 + basechar2);
        else return SCM_CHAR_INVALID;
    }
}

/*
 * Convert UCS4 code <-> character
 * If the native encoding is not utf-8, gauche.charconv module is loaded.
 * and these pointers are filled.
 */
static ScmChar (*ucs2char_hook)(int ucs4) = NULL;
static int     (*char2ucs_hook)(ScmChar ch) = NULL;

/* called by gauche.charconv */
void Scm__InstallCharconvHooks(ScmChar (*u2c)(int), int (*c2u)(ScmChar))
{
    ucs2char_hook = u2c;
    char2ucs_hook = c2u;
}

/* TRANSIENT: These two variables are no longer used, but kept here for the
   ABI compatibility.  Remove them on 1.0 release. */
ScmChar (*Scm_UcsToCharHook)(int ucs4) = NULL;
int (*Scm_CharToUcsHook)(ScmChar ch) = NULL;

ScmChar Scm_UcsToChar(int n)
{
    if (n < 0) Scm_Error("bad character code: %d", n);
#if defined(GAUCHE_CHAR_ENCODING_UTF_8)
    return (ScmChar)n;
#elif defined(GAUCHE_CHAR_ENCODING_EUC_JP) || defined(GAUCHE_CHAR_ENCODING_SJIS)
    if (n < 0x80) return (ScmChar)n; /*ASCII range*/
    if (ucs2char_hook == NULL) {
        /* NB: we don't need mutex here, for the loading of gauche.charconv
           is serialized in Scm_Require. */
        Scm_Require(SCM_MAKE_STR("gauche/charconv"),
                    SCM_LOAD_PROPAGATE_ERROR, NULL);
        if (ucs2char_hook == NULL) {
            Scm_Error("couldn't autoload gauche.charconv");
        }
    }
    return ucs2char_hook(n);
#else
    /* Encoding == 'none'.  It would be safer to reject anything beyond
       0xff, but it prevents 'none' gosh from reading any source files that
       have escaped characters in that range, even the section is cond-expanded.
       That's awfully incovenient, so we use a substitution character '?' here,
       relying the programmer to properly conditionalize the code.
       We plan to drop 'none' encoding support in 1.0, so this kludge is
       just a temporary measure.
    */
    if (n < 0x100) return (ScmChar)n; /* ISO8859-1 */
    else return (ScmChar)'?';
#endif
}

int Scm_CharToUcs(ScmChar ch)
{
    if (ch == SCM_CHAR_INVALID) Scm_Error("bad character");
#if defined(GAUCHE_CHAR_ENCODING_UTF_8)
    return (int)ch;
#elif defined(GAUCHE_CHAR_ENCODING_EUC_JP) || defined(GAUCHE_CHAR_ENCODING_SJIS)
    if (ch < 0x80) return (int)ch; /*ASCII range*/
    if (char2ucs_hook == NULL) {
        /* NB: we don't need mutex here, for the loading of gauche.charconv
           is serialized in Scm_Require. */
        Scm_Require(SCM_MAKE_STR("gauche/charconv"),
                    SCM_LOAD_PROPAGATE_ERROR, NULL);
        if (char2ucs_hook == NULL) {
            Scm_Error("couldn't autoload gauche.charconv");
        }
    }
    return char2ucs_hook(ch);
#else
    return (int)ch;             /* ISO8859-1 */
#endif /*!GAUCHE_CHAR_ENCODING_UTF_8*/
}

/*
 * Charcter classification for lexical parsing
 */

/* Table of initial 128 bytes of ASCII characters to dispatch for
   special meanings. */

enum {
    INITIAL          = 1<<0,      /* <initial> */
    SUBSEQUENT       = 1<<1,      /* <subsequent> */
    SIGN_SUBSEQUENT  = 1<<2,      /* <sign subsequent> */
    DELIMITER        = 1<<3,      /* <delimiter> */
    GAUCHE_DELIMITER = 1<<4,      /* Gauche-extended delimiter */
};

static const unsigned char ctypes[128] = {
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    /*SPC*/  DELIMITER,
    /* ! */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* " */  DELIMITER,
    /* # */  0,
    /* $ */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* % */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* & */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* ' */  GAUCHE_DELIMITER,
    /* ( */  DELIMITER,
    /* ) */  DELIMITER,
    /* * */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* + */  SUBSEQUENT|SIGN_SUBSEQUENT,
    /* , */  GAUCHE_DELIMITER,
    /* - */  SUBSEQUENT|SIGN_SUBSEQUENT,
    /* . */  SUBSEQUENT,
    /* / */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,

    /* 0 */  SUBSEQUENT,
    /* 1 */  SUBSEQUENT,
    /* 2 */  SUBSEQUENT,
    /* 3 */  SUBSEQUENT,
    /* 4 */  SUBSEQUENT,
    /* 5 */  SUBSEQUENT,
    /* 6 */  SUBSEQUENT,
    /* 7 */  SUBSEQUENT,
    /* 8 */  SUBSEQUENT,
    /* 9 */  SUBSEQUENT,
    /* : */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* ; */  DELIMITER,
    /* < */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* = */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* > */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* ? */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,

    /* @ */  SUBSEQUENT|SIGN_SUBSEQUENT,
    /* A */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* B */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* C */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* D */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* E */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* F */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* G */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* H */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* I */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* J */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* K */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* L */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* M */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* N */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* O */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,

    /* P */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* Q */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* R */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* S */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* T */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* U */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* V */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* W */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* X */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* Y */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* Z */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* [ */  GAUCHE_DELIMITER,
    /* \ */  0,
    /* ] */  GAUCHE_DELIMITER,
    /* ^ */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* _ */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,

    /* ` */  GAUCHE_DELIMITER,
    /* a */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* b */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* c */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* d */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* e */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* f */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* g */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* h */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* i */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* j */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* k */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* l */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* m */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* n */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* o */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,

    /* p */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* q */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* r */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* s */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* t */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* u */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* v */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* w */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* x */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* y */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* z */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /* { */  GAUCHE_DELIMITER,
    /* | */  DELIMITER,
    /* } */  GAUCHE_DELIMITER,
    /* ~ */  INITIAL|SUBSEQUENT|SIGN_SUBSEQUENT,
    /*DEL*/  0,
};

int Scm_CharLexerCategoryP(ScmChar c, ScmCharLexerCategory category)
{
    if (c < 128) {
        switch (category) {
        case SCM_CHAR_INITIAL:    
            return !!(ctypes[c] & INITIAL);
        case SCM_CHAR_SUBSEQUENT:
            return !!(ctypes[c] & SUBSEQUENT);
        case SCM_CHAR_SIGN_SUBSEQUENT:
            return !!(ctypes[c] & SIGN_SUBSEQUENT);
        }
    }
    if (c == 0x200c || c == 0x200d) {
        /* can be INITIAL, SUBSEQUENT and SIGN_SUBSEQUENT */
        return TRUE;
    }
    switch (Scm_CharGeneralCategory(c)) {
    case SCM_CHAR_CATEGORY_Lu:
    case SCM_CHAR_CATEGORY_Ll:
    case SCM_CHAR_CATEGORY_Lt:
    case SCM_CHAR_CATEGORY_Lm:
    case SCM_CHAR_CATEGORY_Lo:
    case SCM_CHAR_CATEGORY_Mn:
    case SCM_CHAR_CATEGORY_Nl:
    case SCM_CHAR_CATEGORY_No:
    case SCM_CHAR_CATEGORY_Pd:
    case SCM_CHAR_CATEGORY_Pc:
    case SCM_CHAR_CATEGORY_Po:
    case SCM_CHAR_CATEGORY_Sc:
    case SCM_CHAR_CATEGORY_Sm:
    case SCM_CHAR_CATEGORY_Sk:
    case SCM_CHAR_CATEGORY_So:
    case SCM_CHAR_CATEGORY_Co: 
        /* can be INITIAL, SUBSEQUENT and SIGN_SUBSEQUENT */
        return TRUE;
    case SCM_CHAR_CATEGORY_Nd:
    case SCM_CHAR_CATEGORY_Mc:
    case SCM_CHAR_CATEGORY_Me:
        return (category != SCM_CHAR_INITIAL);
    default: return FALSE;
    }
}


/*=======================================================================
 * Character set (cf. SRFI-14)
 */
/* NB: operations on charset are not very optimized, for I don't see
 * the immediate needs to do so, except Scm_CharSetContains.
 */

static void charset_print(ScmObj obj, ScmPort *out, ScmWriteContext*);
static int charset_compare(ScmObj x, ScmObj y, int equalp);
SCM_DEFINE_BUILTIN_CLASS(Scm_CharSetClass,
                         charset_print, charset_compare, NULL, NULL,
                         SCM_CLASS_COLLECTION_CPL);

#define MASK_ISSET(cs, ch)  SCM_BITS_TEST(cs->small, ch)
#define MASK_SET(cs, ch)    SCM_BITS_SET(cs->small, ch)
#define MASK_RESET(cs, ch)  SCM_BITS_RESET(cs->small, ch)

static inline void check_mutable(ScmCharSet *cs) 
{
    if (SCM_CHAR_SET_IMMUTABLE_P(cs))
        Scm_Error("Char set is immutable: %S", cs);
}

static inline void set_large(ScmCharSet *cs, int flag)
{
    if (flag) {
        cs->flags |= SCM_CHAR_SET_LARGE;
    } else {
        cs->flags &= ~(SCM_CHAR_SET_LARGE);
    }
}

/*----------------------------------------------------------------------
 * Printer
 */

static void charset_print_ch(ScmPort *out, ScmChar ch, int firstp)
{
    if (ch != 0 && ch < 0x80
        && (strchr("[]-\\", ch) != NULL || (ch == '^' && firstp))) {
        Scm_Printf(out, "\\%C", ch);
    } else {
        switch (Scm_CharGeneralCategory(ch)) {
        case SCM_CHAR_CATEGORY_Mn:
        case SCM_CHAR_CATEGORY_Mc:
        case SCM_CHAR_CATEGORY_Me:
        case SCM_CHAR_CATEGORY_Cc:
        case SCM_CHAR_CATEGORY_Cf:
        case SCM_CHAR_CATEGORY_Cs:
        case SCM_CHAR_CATEGORY_Co:
        case SCM_CHAR_CATEGORY_Cn:
            if (ch < 0x10000) Scm_Printf(out, "\\u%04lx", ch);
            else              Scm_Printf(out, "\\U%08lx", ch);
            break;
        default:
            Scm_Putc(ch, out);
        }
    }
}

static void charset_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    int prev, code, first = TRUE;
    ScmCharSet *cs = SCM_CHAR_SET(obj);

    Scm_Printf(out, "#[");
    for (prev = -1, code = 0; code < SCM_CHAR_SET_SMALL_CHARS; code++) {
        if (MASK_ISSET(cs, code) && prev < 0) {
            charset_print_ch(out, code, first);
            prev = code;
            first = FALSE;
        }
        else if (!MASK_ISSET(cs, code) && prev >= 0) {
            if (code - prev > 1) {
                if (code - prev > 2) Scm_Printf(out, "-");
                charset_print_ch(out, code-1, FALSE);
            }
            prev = -1;
        }
    }
    if (prev >= 0) {
        if (code - prev > 1) {
            if (prev < 0x7e) Scm_Printf(out, "-");
            charset_print_ch(out, code-1, FALSE);
        }
    }

    if (cs->flags & SCM_CHAR_SET_IMMUTABLE) {
        const uint32_t *v = cs->large.frozen.vec;
        for (int i = 0; i < cs->large.frozen.size; i += 2) {
            charset_print_ch(out, (int)v[i], FALSE);
            if (v[i] != v[i+1]) {
                if (v[i+1] - v[i] > 2) Scm_Printf(out, "-");
                charset_print_ch(out, (int)v[i+1], FALSE);
            }
        }
    } else {
        ScmTreeIter iter;
        ScmDictEntry *e;
        Scm_TreeIterInit(&iter, &cs->large.tree, NULL);
        while ((e = Scm_TreeIterNext(&iter)) != NULL) {
            charset_print_ch(out, (int)e->key, FALSE);
            if (e->value != e->key) {
                if (e->value - e->key > 2) Scm_Printf(out, "-");
                charset_print_ch(out, (int)e->value, FALSE);
            }
        }
    }
    Scm_Printf(out, "]");
}

/*-----------------------------------------------------------------
 * Iterators for large char set
 */
typedef struct cs_iter_rec {
    ScmCharSet *cs;
    int end;
    union {
        ScmTreeIter ti;
        size_t vi;
    } iter;
} cs_iter;

static void cs_iter_init(cs_iter *ci, ScmCharSet *cs)
{
    ci->cs = cs;
    if (!SCM_CHAR_SET_LARGE_P(cs)) {
        ci->end = TRUE;
    } else {
        ci->end = FALSE;
        if (SCM_CHAR_SET_IMMUTABLE_P(cs)) {
            ci->iter.vi = 0;
        } else {
            Scm_TreeIterInit(&ci->iter.ti, &cs->large.tree, NULL);
        }
    }
}

/* returns FALSE if already exhausted */
static int cs_iter_next(cs_iter *ci,
                        ScmChar *from /*out*/,
                        ScmChar *to   /*out*/)
{
    if (ci->end) return FALSE;
    if (SCM_CHAR_SET_IMMUTABLE_P(ci->cs)) {
        if (ci->iter.vi >= ci->cs->large.frozen.size) {
            ci->end = TRUE;
            return FALSE;
        } else {
            *from = (ScmChar)ci->cs->large.frozen.vec[ci->iter.vi];
            *to   = (ScmChar)ci->cs->large.frozen.vec[ci->iter.vi+1];
            ci->iter.vi += 2;
            return TRUE;
        }
    } else {
        ScmDictEntry *e = Scm_TreeIterNext(&ci->iter.ti);
        if (e == NULL) {
            ci->end = TRUE;
            return FALSE;
        } else {
            *from = (ScmChar)e->key;
            *to = (ScmChar)e->value;
            return TRUE;
        }
    }
}

/*-----------------------------------------------------------------
 * Constructors
 */
static int cmp(ScmTreeCore *tc, intptr_t a, intptr_t b)
{
    if (a > b) return 1;
    if (a < b) return -1;
    return 0;
}

static ScmCharSet *make_charset(void)
{
    ScmCharSet *cs = SCM_NEW(ScmCharSet);
    SCM_SET_CLASS(cs, SCM_CLASS_CHARSET);
    Scm_BitsFill(cs->small, 0, SCM_CHAR_SET_SMALL_CHARS, 0);
    Scm_TreeCoreInit(&cs->large.tree, cmp, NULL);
    cs->flags = 0;              /* small & mutable by default */
    return cs;
}

ScmObj Scm_MakeEmptyCharSet(void)
{
    return SCM_OBJ(make_charset());
}

/* This is mainly for precompiled module. */
ScmObj Scm_MakeImmutableCharSet(const ScmBits *small,
                                const uint32_t *vec,
                                size_t size)
{
    SCM_ASSERT(size > 0 && size % 2 == 0);
    ScmCharSet *cs = SCM_NEW(ScmCharSet);
    cs->flags |= SCM_CHAR_SET_IMMUTABLE;
    memcpy(cs->small, small, sizeof(cs->small));
    if (vec != NULL) {
        set_large(cs, TRUE);
        if ((cs->large.frozen.size = size) == 2) {
            cs->large.frozen.ivec[0] = vec[0];
            cs->large.frozen.ivec[1] = vec[1];
            cs->large.frozen.vec = cs->large.frozen.ivec;
        } else {
            cs->large.frozen.vec = vec;
        }
    }
    return SCM_OBJ(cs);
}

ScmObj Scm_CharSetCopy(ScmCharSet *src)
{
    ScmCharSet *dst = make_charset();
    Scm_BitsCopyX(dst->small, 0, src->small, 0, SCM_CHAR_SET_SMALL_CHARS);
    set_large(dst, SCM_CHAR_SET_LARGE_P(src));
    if (SCM_CHAR_SET_IMMUTABLE_P(src)) {
        /* The destination is mutable */
        const uint32_t *vec = src->large.frozen.vec;
        for (size_t k = 0; k < src->large.frozen.size; k += 2) {
            ScmDictEntry *e = Scm_TreeCoreSearch(&dst->large.tree,
                                                 vec[k], SCM_DICT_CREATE);
            e->value = vec[k+1];
        }
    } else {
        Scm_TreeCoreCopy(&dst->large.tree, &src->large.tree);
    }
    return SCM_OBJ(dst);
}

/* Creates flat searched vector to be used for immutable charset.
   SRC must be a mutable charset.
   The caller must provide uint32_t[2] buffer for ivec. */
static uint32_t *char_set_freeze_vec(ScmCharSet *src,
                                     uint32_t *ivec,
                                     size_t *size /*out*/)
{
    SCM_ASSERT(!SCM_CHAR_SET_IMMUTABLE_P(src));
    size_t s = (size_t)Scm_TreeCoreNumEntries(&src->large.tree) * 2;
    uint32_t *v = (s == 2)? ivec : SCM_NEW_ATOMIC_ARRAY(uint32_t, s);
    
    cs_iter iter;
    cs_iter_init(&iter, src);
    ScmChar lo, hi;
    for (size_t k = 0; cs_iter_next(&iter, &lo, &hi); k += 2) {
        SCM_ASSERT(k < s);
        v[k]   = (uint32_t)lo;
        v[k+1] = (uint32_t)hi;
    }
    *size = s;
    return v;
}

ScmObj Scm_CharSetFreeze(ScmCharSet *src)
{
    if (SCM_CHAR_SET_IMMUTABLE_P(src)) return SCM_OBJ(src);
    ScmCharSet *dst = make_charset();
    Scm_BitsCopyX(dst->small, 0, src->small, 0, SCM_CHAR_SET_SMALL_CHARS);

    dst->flags |= SCM_CHAR_SET_IMMUTABLE;
    if (SCM_CHAR_SET_LARGE_P(src)) {
        set_large(dst, TRUE);
        dst->large.frozen.vec = char_set_freeze_vec(src,
                                                    dst->large.frozen.ivec,
                                                    &dst->large.frozen.size);
    } else {
        dst->large.frozen.vec = NULL;
        dst->large.frozen.size = 0;
    }
    return SCM_OBJ(dst);
}

ScmObj Scm_CharSetFreezeX(ScmCharSet *src)
{
    if (SCM_CHAR_SET_IMMUTABLE_P(src)) return SCM_OBJ(src);
    if (SCM_CHAR_SET_LARGE_P(src)) {
        size_t s;
        uint32_t iv[2];
        uint32_t *v = char_set_freeze_vec(src, iv, &s);
        src->large.frozen.size = s;
        src->large.frozen.vec = (s == 2)? src->large.frozen.ivec : v;
        src->large.frozen.ivec[0] = iv[0];
        src->large.frozen.ivec[1] = iv[1];
    }
    src->flags |= SCM_CHAR_SET_IMMUTABLE;
    return SCM_OBJ(src);
}

/*-----------------------------------------------------------------
 * Comparison
 */
static int charset_compare(ScmObj x, ScmObj y, int equalp)
{
    ScmCharSet *xx = SCM_CHAR_SET(x);
    ScmCharSet *yy = SCM_CHAR_SET(y);

    if (equalp) {
        return (Scm_CharSetEq(xx, yy)? 0 : 1);
    } else {
        if (Scm_CharSetEq(xx, yy)) return 0;
        if (Scm_CharSetLE(xx, yy)) return -1;
        if (Scm_CharSetLE(yy, xx)) return 1;
        Scm_Error("cannot compare char-sets: %S vs %S", x, y);
        return 0;               /* dummy */
    }
}

int Scm_CharSetEq(ScmCharSet *x, ScmCharSet *y)
{
    if (!Scm_BitsEqual(x->small, y->small, 0, SCM_CHAR_SET_SMALL_CHARS))
        return FALSE;
    if (!SCM_CHAR_SET_IMMUTABLE_P(x) && !SCM_CHAR_SET_IMMUTABLE_P(y)) {
        /* shortcut */
        return Scm_TreeCoreEq(&x->large.tree, &y->large.tree);
    } else {
        cs_iter xi, yi;
        cs_iter_init(&xi, x);
        cs_iter_init(&yi, y);
        for (;;) {
            ScmChar xl, xh, yl, yh;
            int xr = cs_iter_next(&xi, &xl, &xh);
            int yr = cs_iter_next(&yi, &yl, &yh);
            if (xr == FALSE && yr == FALSE) return TRUE;
            if (!(xr && yr)) return FALSE;
            if (!(xl == yl && xh == yh)) return FALSE;
        }
    }
}

/* See if cs contains the range [lo,hi] in large char range. */
static int cs_contains_range(ScmCharSet *s, ScmChar lo, ScmChar hi)
{
    if (!SCM_CHAR_SET_LARGE_P(s)) return FALSE;
    /* We can have two cases.
     *
     * Case 1:
     *    lo<---------->hi
     *    ye<----------------->
     * Case 2:
     *         lo<---------->hi
     *    yl<------------------->
     */
    if (SCM_CHAR_SET_IMMUTABLE_P(s)) {
        size_t ye, yl;
        ye = Scm_BinarySearchU32(s->large.frozen.vec, s->large.frozen.size,
                                 (uint32_t)lo, 1, &yl, NULL);
        if (ye != (size_t)-1) { /* case 1 */
            if (s->large.frozen.vec[ye+1] < hi) return FALSE;
        } else if (yl != (size_t)-1) { /* case 2 */
            if (s->large.frozen.vec[yl+1] < hi) return FALSE;
        } else {
            return FALSE;
        }
    } else {
        ScmDictEntry *ye, *yl, *yh;
        ye = Scm_TreeCoreClosestEntries(&s->large.tree, lo, &yl, &yh);
        if (ye) {               /* case 1 */
            if (ye->value < hi) return FALSE;
        } else if (yl) {        /* case 2 */
            if (yl->value < hi) return FALSE;
        } else {
            return FALSE;
        }
    }
    return TRUE;
}

/* whether x <= y */
int Scm_CharSetLE(ScmCharSet *x, ScmCharSet *y)
{
    if (!Scm_BitsIncludes(y->small, x->small, 0, SCM_CHAR_SET_SMALL_CHARS))
        return FALSE;

    cs_iter xi;
    cs_iter_init(&xi, x);
    ScmChar lo, hi;
    while (cs_iter_next(&xi, &lo, &hi)) {
        if (!cs_contains_range(y, lo, hi)) return FALSE;
    }
    return TRUE;
}

/*-----------------------------------------------------------------
 * Modification
 * We reject immutable set at the top, so that we only deal with treemap.
 */

ScmObj Scm_CharSetAddRange(ScmCharSet *cs, ScmChar from, ScmChar to)
{
    check_mutable(cs);
    
    ScmDictEntry *e, *lo, *hi;

    if (to < from) return SCM_OBJ(cs);
    if (from < SCM_CHAR_SET_SMALL_CHARS) {
        if (to < SCM_CHAR_SET_SMALL_CHARS) {
            Scm_BitsFill(cs->small, (int)from, (int)to+1, TRUE);
            return SCM_OBJ(cs);
        }
        Scm_BitsFill(cs->small, (int)from, SCM_CHAR_SET_SMALL_CHARS, TRUE);
        from = SCM_CHAR_SET_SMALL_CHARS;
    }

    set_large(cs, TRUE);
    
    /* Let e have the lower bound. */
    e = Scm_TreeCoreClosestEntries(&cs->large.tree, from, &lo, &hi);
    if (!e) {
        if (!lo || lo->value < from-1) {
            e = Scm_TreeCoreSearch(&cs->large.tree, from, SCM_DICT_CREATE);
        } else {
            e = lo;
        }
    }
    /* Set up the upper bound.
       NB: if e is a new entry, e->value is 0. */
    if (e->value >= to) return SCM_OBJ(cs);

    hi = e;
    while ((hi = Scm_TreeCoreNextEntry(&cs->large.tree, hi->key)) != NULL) {
        if (hi->key > to+1) {
            e->value = to;
            return SCM_OBJ(cs);
        }
        Scm_TreeCoreSearch(&cs->large.tree, hi->key, SCM_DICT_DELETE);
        if (hi->value > to) {
            e->value = hi->value;
            return SCM_OBJ(cs);
        }
    }
    e->value = to;
    return SCM_OBJ(cs);
}

ScmObj Scm_CharSetAdd(ScmCharSet *dst, ScmCharSet *src)
{
    check_mutable(dst);

    if (dst == src) return SCM_OBJ(dst);  /* precaution */

    set_large(dst, SCM_CHAR_SET_LARGE_P(src));
    
    ScmTreeIter iter;
    ScmDictEntry *e;
    Scm_BitsOperate(dst->small, SCM_BIT_IOR, dst->small, src->small,
                    0, SCM_CHAR_SET_SMALL_CHARS);
    Scm_TreeIterInit(&iter, &src->large.tree, NULL);
    while ((e = Scm_TreeIterNext(&iter)) != NULL) {
        Scm_CharSetAddRange(dst, SCM_CHAR(e->key), SCM_CHAR(e->value));
    }
    return SCM_OBJ(dst);
}

ScmObj Scm_CharSetComplement(ScmCharSet *cs)
{
    check_mutable(cs);
    
    ScmDictEntry *e, *n;

    set_large(cs, !SCM_CHAR_SET_LARGE_P(cs));
    
    Scm_BitsOperate(cs->small, SCM_BIT_NOT1, cs->small, NULL,
                    0, SCM_CHAR_SET_SMALL_CHARS);
    int last = SCM_CHAR_SET_SMALL_CHARS-1;
    /* we can't use treeiter, since we modify the tree while traversing it. */
    while ((e = Scm_TreeCoreNextEntry(&cs->large.tree, last)) != NULL) {
        Scm_TreeCoreSearch(&cs->large.tree, e->key, SCM_DICT_DELETE);
        if (last < e->key-1) {
            n = Scm_TreeCoreSearch(&cs->large.tree, last+1, SCM_DICT_CREATE);
            n->value = e->key-1;
        }
        last = (int)e->value;
    }
    if (last < SCM_CHAR_MAX) {
        n = Scm_TreeCoreSearch(&cs->large.tree, last+1, SCM_DICT_CREATE);
        n->value = SCM_CHAR_MAX;
    }
    return SCM_OBJ(cs);
}

/* Make CS case-insensitive. */
ScmObj Scm_CharSetCaseFold(ScmCharSet *cs)
{
    check_mutable(cs);

    for (int ch='a'; ch<='z'; ch++) {
        if (MASK_ISSET(cs, ch) || MASK_ISSET(cs, (ch-('a'-'A')))) {
            MASK_SET(cs, ch);
            MASK_SET(cs, (ch-('a'-'A')));
        }
    }

    ScmTreeIter iter;
    ScmDictEntry *e;
    Scm_TreeIterInit(&iter, &cs->large.tree, NULL);
    while ((e = Scm_TreeIterNext(&iter)) != NULL) {
        for (ScmChar c = e->key; c <= e->value; c++) {
            ScmChar uch = Scm_CharUpcase(c);
            ScmChar lch = Scm_CharDowncase(c);
            Scm_CharSetAddRange(cs, uch, uch);
            Scm_CharSetAddRange(cs, lch, lch);
        }
    }
    return SCM_OBJ(cs);
}

/*-----------------------------------------------------------------
 * Query
 */

int Scm_CharSetContains(ScmCharSet *cs, ScmChar c)
{
    if (c < 0) return FALSE;
    if (c < SCM_CHAR_SET_SMALL_CHARS) return MASK_ISSET(cs, c);
    else if (SCM_CHAR_SET_IMMUTABLE_P(cs)) {
        if (cs->large.frozen.size == 2) {
            /* shortcut */
            return (c >= (ScmChar)cs->large.frozen.ivec[0]
                    && c <= (ScmChar)cs->large.frozen.ivec[1]);
        } else {
            size_t lo;
            size_t k = Scm_BinarySearchU32(cs->large.frozen.vec,
                                           cs->large.frozen.size,
                                           (uint32_t)c,
                                           1, &lo, NULL);
            if ((k != (size_t)-1)
                || (lo != (size_t)-1 && c <= cs->large.frozen.vec[lo+1]))
                return TRUE;
            else
                return FALSE;
        }
    } else {
        ScmDictEntry *e, *l, *h;
        e = Scm_TreeCoreClosestEntries(&cs->large.tree, (int)c, &l, &h);
        if (e || (l && l->value >= c)) return TRUE;
        else return FALSE;
    }
}

/*-----------------------------------------------------------------
 * Inspection
 */

/* returns a list of ranges contained in the charset */
ScmObj Scm_CharSetRanges(ScmCharSet *cs)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    int ind, begin = 0, prev = FALSE;

    for (ind = 0; ind < SCM_CHAR_SET_SMALL_CHARS; ind++) {
        int bit = MASK_ISSET(cs, ind);
        if (!prev && bit) begin = ind;
        if (prev && !bit) {
            ScmObj cell = Scm_Cons(SCM_MAKE_INT(begin), SCM_MAKE_INT(ind-1));
            SCM_APPEND1(h, t, cell);
        }
        prev = bit;
    }
    if (prev) {
        ScmObj cell = Scm_Cons(SCM_MAKE_INT(begin), SCM_MAKE_INT(ind-1));
        SCM_APPEND1(h, t, cell);
    }

    cs_iter iter;
    cs_iter_init(&iter, cs);
    ScmChar lo, hi;
    while (cs_iter_next(&iter, &lo, &hi)) {
        ScmObj cell = Scm_Cons(SCM_MAKE_INT(lo), SCM_MAKE_INT(hi));
        SCM_APPEND1(h, t, cell);
    }
    return h;
}

void Scm_CharSetDump(ScmCharSet *cs, ScmPort *port)
{
    Scm_Printf(port, "CharSet %p%s\nmask:",
               cs,
               SCM_CHAR_SET_IMMUTABLE_P(cs) ? " (frozen)" : "");
    for (int i=0; i<SCM_BITS_NUM_WORDS(SCM_CHAR_SET_SMALL_CHARS); i++) {
#if SIZEOF_LONG == 4
        Scm_Printf(port, "[%08lx]", cs->small[i]);
#else
        Scm_Printf(port, "[%016lx]", cs->small[i]);
#endif
    }
    Scm_Printf(port, "\nranges:");
    cs_iter iter;
    cs_iter_init(&iter, cs);
    ScmChar lo, hi;
    while (cs_iter_next(&iter, &lo, &hi)) {
        Scm_Printf(port, " %x-%x", lo, hi);
    }
    Scm_Printf(port, "\n");
}

/*-----------------------------------------------------------------
 * Reader
 */

/* Parse regexp-style character set specification (e.g. [a-zA-Z]).
   Assumes the opening bracket is already read.
   Always return a fresh charset, that can be modified afterwards.

   If the input syntax is invalid, either signals an error or returns
   #f, depending error_p flag.

   If bracket_syntax is TRUE, the first closing bracket ']' in the
   charset (except the complimenting caret) is taken as a literal
   character, instead of terminating the charset.  It should be TRUE
   during reading the regexp syntax for compatibility to POSIX regexp.

   If complement_p is not NULL, the location get a boolean value of
   whether complement character (caret in the beginning) appeared or not.
   In that case, the returned charset is not complemented. */

static ScmObj read_predef_charset(const char**, int);
static int read_charset_syntax(ScmPort *input, int bracket_syntax,
                               ScmDString *buf, int *complementp);

ScmObj Scm_CharSetRead(ScmPort *input, int *complement_p,
                       int error_p, int bracket_syntax)
{
    int complement = FALSE;
    ScmDString buf;

    Scm_DStringInit(&buf);
    if (read_charset_syntax(input, bracket_syntax, &buf, &complement)) {
        int lastchar = -1, inrange = FALSE, moreset_complement = FALSE;
        ScmCharSet *set = SCM_CHAR_SET(Scm_MakeEmptyCharSet());
        int size;
        const char *cp = Scm_DStringPeek(&buf, &size, NULL);
        const char *end = cp + size;

        while (cp < end) {
            ScmChar ch;
            SCM_CHAR_GET(cp, ch);
            if (ch == SCM_CHAR_INVALID) goto err;
            cp += SCM_CHAR_NBYTES(ch);

            ScmObj moreset;
            switch (ch) {
            case '-':
                if (inrange) goto ordchar;
                inrange = TRUE;
                continue;
            case '\\':
                if (cp >= end) goto err;
                SCM_CHAR_GET(cp, ch);
                if (ch == SCM_CHAR_INVALID) goto err;
                cp += SCM_CHAR_NBYTES(ch);
                switch (ch) {
                case 'a': ch = 7; goto ordchar;
                case 'b': ch = 8; goto ordchar;
                case 'n': ch = '\n'; goto ordchar;
                case 'r': ch = '\r'; goto ordchar;
                case 't': ch = '\t'; goto ordchar;
                case 'f': ch = '\f'; goto ordchar;
                case 'e': ch = 0x1b; goto ordchar;
                case 'x': case 'u': case 'U':
                    ch = Scm_ReadXdigitsFromString(cp, end-cp, ch,
                                                   Scm_GetPortReaderLexicalMode(input),
                                                   TRUE, &cp);
                    if (ch == SCM_CHAR_INVALID) goto err;
                    goto ordchar;
                case 'd':
                    moreset_complement = FALSE;
                    moreset = Scm_GetStandardCharSet(SCM_CHAR_SET_DIGIT);
                    break;
                case 'D':
                    moreset_complement = TRUE;
                    moreset = Scm_GetStandardCharSet(SCM_CHAR_SET_DIGIT);
                    break;
                case 's':
                    moreset_complement = FALSE;
                    moreset = Scm_GetStandardCharSet(SCM_CHAR_SET_SPACE);
                    break;
                case 'S':
                    moreset_complement = TRUE;
                    moreset = Scm_GetStandardCharSet(SCM_CHAR_SET_SPACE);
                    break;
                case 'w':
                    moreset_complement = FALSE;
                    moreset = Scm_GetStandardCharSet(SCM_CHAR_SET_WORD);
                    break;
                case 'W':
                    moreset_complement = TRUE;
                    moreset = Scm_GetStandardCharSet(SCM_CHAR_SET_WORD);
                    break;
                default:
                    goto ordchar;
                }
                if (moreset_complement) {
                    moreset = Scm_CharSetComplement(SCM_CHAR_SET(Scm_CharSetCopy(SCM_CHAR_SET(moreset))));
                }
                Scm_CharSetAdd(set, SCM_CHAR_SET(moreset));
                continue;
            case '[':
                moreset = read_predef_charset(&cp, error_p);
                if (!SCM_CHAR_SET_P(moreset)) goto err;
                Scm_CharSetAdd(set, SCM_CHAR_SET(moreset));
                continue;
            ordchar:
            default:
                if (inrange) {
                    if (lastchar < 0) {
                        Scm_CharSetAddRange(set, '-', '-');
                        Scm_CharSetAddRange(set, ch, ch);
                        lastchar = ch;
                    } else {
                        Scm_CharSetAddRange(set, lastchar, ch);
                        lastchar = -1;
                    }
                    inrange = FALSE;
                } else {
                    Scm_CharSetAddRange(set, ch, ch);
                    lastchar = ch;
                }
                continue;
            }
            break;
        }
        if (inrange) {
            Scm_CharSetAddRange(set, '-', '-');
            if (lastchar >= 0) Scm_CharSetAddRange(set, lastchar, lastchar);
        }
        if (complement_p) {
            *complement_p = complement;
            return SCM_OBJ(set);
        } else {
            if (complement) Scm_CharSetComplement(set);
            return SCM_OBJ(set);
        }
    }
  err:
    if (error_p) {
        /* TODO: We should deal with the case when input contains \0 */
        Scm_Error("Invalid charset syntax [%s%s...",
                  complement? "^" : "",
                  Scm_DStringGetz(&buf));
    }
    return SCM_FALSE;
}

/* Read till the end of char-set syntax and store the chars in BUF.
   The surrounding brackets are not stored.  The complementing caret
   ('^' right after opening '[') isn't stored either, but *complementp
   holds whether it appeared or not.
   Returns TRUE on success.
 */
int read_charset_syntax(ScmPort *input, int bracket_syntax, ScmDString *buf,
                        int *complementp)
{
#define REAL_BEGIN 1
#define CARET_BEGIN 2
    int begin = REAL_BEGIN, complement = FALSE, brackets = 0;

    for (;;) {
        int ch;
        SCM_GETC(ch, input);
        if (ch == EOF) return FALSE;

        if (begin == REAL_BEGIN && ch == '^') {
            complement = TRUE;
            begin = CARET_BEGIN;
            continue;
        }
        if (bracket_syntax && begin && ch == ']') {
            begin = FALSE;
            Scm_DStringPutc(buf, ch);
            continue;
        }
        if (ch == ']' && brackets <= 0) break;
        begin = FALSE;

        Scm_DStringPutc(buf, ch);

        switch (ch) {
        case ']': brackets--; break;
        case '[': brackets++; break;
        case '\\':
            SCM_GETC(ch, input);
            if (ch == EOF) return FALSE;
            Scm_DStringPutc(buf, ch);
            break;
        }
    }
    *complementp = complement;
    return TRUE;
}


/* Read posix [:alpha:] etc.  The first '[' is already read.
   Return #f on error if errorp is FALSE.
   Set reverse list of read chars in *chars */
#define MAX_CHARSET_NAME_LEN  10
ScmObj read_predef_charset(const char **cp, int error_p)
{
    int i;
    char name[MAX_CHARSET_NAME_LEN+1];
    for (i=0; i<MAX_CHARSET_NAME_LEN; i++) {
        ScmChar ch;
        SCM_CHAR_GET(*cp, ch);
        if (ch == SCM_CHAR_INVALID) return SCM_FALSE;
        *cp += SCM_CHAR_NBYTES(ch);
        if (!SCM_CHAR_ASCII_P(ch)) break;
        if (ch != ']') {
            name[i] = (char)ch;
            continue;
        }
        if (strncmp(name, ":alnum:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHAR_SET_ALNUM);
        } else if (strncmp(name, ":alpha:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHAR_SET_ALPHA);
        } else if (strncmp(name, ":blank:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHAR_SET_BLANK);
        } else if (strncmp(name, ":cntrl:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHAR_SET_CNTRL);
        } else if (strncmp(name, ":digit:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHAR_SET_DIGIT);
        } else if (strncmp(name, ":graph:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHAR_SET_GRAPH);
        } else if (strncmp(name, ":lower:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHAR_SET_LOWER);
        } else if (strncmp(name, ":print:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHAR_SET_PRINT);
        } else if (strncmp(name, ":punct:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHAR_SET_PUNCT);
        } else if (strncmp(name, ":space:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHAR_SET_SPACE);
        } else if (strncmp(name, ":upper:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHAR_SET_UPPER);
        } else if (strncmp(name, ":xdigit:", 8) == 0) {
            return Scm_GetStandardCharSet(SCM_CHAR_SET_XDIGIT);
        } else break;
    }
    /* here we got invalid charset name */
    if (error_p) {
        name[i] = '\0';
        Scm_Error("invalid or unsupported POSIX charset '[%s]'", name);
    }
    return SCM_FALSE;
}

/*-----------------------------------------------------------------
 * Character attributes
 */

int Scm_CharGeneralCategory(ScmChar ch)
{
    return (int)(Scm__LookupCharCategory(ch) & SCM_CHAR_CATEGORY_MASK);
}

int Scm_CharAlphabeticP(ScmChar ch)
{
    return (SCM_CHAR_ALPHA_MASK & Scm__LookupCharCategory(ch)) != 0;
}

int Scm_CharUppercaseP(ScmChar ch)
{
    return ((SCM_CHAR_ALPHA_MASK & Scm__LookupCharCategory(ch))
            == SCM_CHAR_UPPERCASE_BITS);
}

int Scm_CharLowercaseP(ScmChar ch)
{
    return ((SCM_CHAR_ALPHA_MASK & Scm__LookupCharCategory(ch))
            == SCM_CHAR_LOWERCASE_BITS);
}

int Scm_CharTitlecaseP(ScmChar ch)
{
    return (Scm_CharGeneralCategory(ch) == SCM_CHAR_CATEGORY_Lt);
}

int Scm_CharNumericP(ScmChar ch)
{
    return (Scm_CharGeneralCategory(ch) == SCM_CHAR_CATEGORY_Nd);
}

/* An internal entry to extract case mapping info.
 * Internal table is compressed, so the caller must provide
 * the buffer for ScmCharCaseMap.
 * The function returns either the pointer to the given buffer
 * with information filled, or a pointer to a static read-only
 * data structure in the internal table.
 */
static const ScmCharCaseMap casemap_identity = {
    0, 0, 0, {-1}, {-1}, {-1}
};

const ScmCharCaseMap *Scm__CharCaseMap(ScmChar ch,
                                       ScmCharCaseMap *buf,
                                       int full)
{
    if (ch < 0x10000) {
        int subtable = casemap_000[(ch >> 8) & 0xff];
        if (subtable == 255) return &casemap_identity;

        unsigned short cmap =
                casemap_subtable[subtable][(unsigned char)(ch & 0xff)];
        if (cmap == SCM_CHAR_NO_CASE_MAPPING) return &casemap_identity;
        if (cmap & 0x8000) {
            /* mapping is extended. */
            return &(extended_casemaps[cmap & 0x7fff]);
        } else {
            /* mapping is simple */
            int off = (cmap & 0x2000)? (signed int)(cmap|~0x1fff) : cmap&0x1fff;
            if (cmap & 0x4000) {
                buf->to_upper_simple = off;
                buf->to_lower_simple = 0;
                buf->to_title_simple = off;
            } else {
                buf->to_upper_simple = 0;
                buf->to_lower_simple = off;
                buf->to_title_simple = 0;
            }
            if (full) {
                /* indicate no special mappings */
                buf->to_upper_full[0] = -1;
                buf->to_lower_full[0] = -1;
                buf->to_title_full[0] = -1;
            }
            return buf;
        }
    } else {
        /* TODO: 104xx*/
        return &casemap_identity;
    }
}

/*
 * Case conversion API.  For the time being, CharCaseMap works on Unicode
 * codepoints, so we have to convert from/to ScmChar if the internal encoding
 * is either EUC-JP or SJIS.
 */
#define SIMPLE_CASE(code, buf, field) \
    (ScmChar)((code) + Scm__CharCaseMap((code), (buf), FALSE)->SCM_CPP_CAT3(to_, field, _simple))

#define SIMPLE_CASE_CV(code, buf, field)    \
    ((code) = (ScmChar)Scm_CharToUcs((int)(code)), \
     (code) = SIMPLE_CASE(code, buf, field),       \
     Scm_UcsToChar((int)(code)))

ScmChar Scm_CharUpcase(ScmChar ch)
{
    ScmCharCaseMap cm;
#if defined(GAUCHE_CHAR_ENCODING_EUC_JP) || defined(GAUCHE_CHAR_ENCODING_SJIS)
    if (ch < 0x80) return SIMPLE_CASE(ch, &cm, upper);
    else if (Scm__CharInUnicodeP(ch)) return SIMPLE_CASE_CV(ch, &cm, upper);
    else           return ch;
#elif defined(GAUCHE_CHAR_ENCODING_UTF_8)
    return SIMPLE_CASE(ch, &cm, upper);
#else
    /* Latin-1 mapping and Unicode mapping differ in U+00B5 (MICRO SIGN)
       and U+00FF (LATIN SMALL LETTER Y WITH DIAERESIS).  In Unicode
       they map to U+039C and U+0178, respectively.  In Latin-1 we don't
       have those characters, so we leave them alone. */
    if (ch == 0xb5 || ch == 0xff) return ch;
    else return SIMPLE_CASE(ch, &cm, upper);
#endif
}

ScmChar Scm_CharDowncase(ScmChar ch)
{
    ScmCharCaseMap cm;
#if defined(GAUCHE_CHAR_ENCODING_EUC_JP) || defined(GAUCHE_CHAR_ENCODING_SJIS)
    if (ch < 0x80) return SIMPLE_CASE(ch, &cm, lower);
    else if (Scm__CharInUnicodeP(ch)) return SIMPLE_CASE_CV(ch, &cm, lower);
    else           return ch;
#else
    return SIMPLE_CASE(ch, &cm, lower);
#endif
}

ScmChar Scm_CharTitlecase(ScmChar ch)
{
    ScmCharCaseMap cm;
#if defined(GAUCHE_CHAR_ENCODING_EUC_JP) || defined(GAUCHE_CHAR_ENCODING_SJIS)
    if (ch < 0x80) return SIMPLE_CASE(ch, &cm, title);
    else if (Scm__CharInUnicodeP(ch)) return SIMPLE_CASE_CV(ch, &cm, title);
    else           return ch;
#elif defined(GAUCHE_CHAR_ENCODING_UTF_8)
    return SIMPLE_CASE(ch, &cm, title);
#else
    /* In Latin-1, titlecase is the same as upcase. */
    return Scm_CharUpcase(ch);
#endif
}

ScmChar Scm_CharFoldcase(ScmChar ch)
{
    ScmCharCaseMap cm;
#if defined(GAUCHE_CHAR_ENCODING_EUC_JP) || defined(GAUCHE_CHAR_ENCODING_SJIS)
    if (Scm__CharInUnicodeP(ch)) {
        ScmChar ucs = (ScmChar)Scm_CharToUcs(ch);
        const ScmCharCaseMap *pcm = Scm__CharCaseMap(ucs, &cm, FALSE);
        if (pcm->to_lower_simple == 0 && pcm->to_upper_simple == 0) {
            /* we don't have case folding */
            return ch;
        }
        /* Otherwise, we do (char-downcase (char-upcase ch)) */
        if (pcm->to_upper_simple != 0) {
            ucs += pcm->to_upper_simple;
            pcm = Scm__CharCaseMap(ucs, &cm, FALSE);
        }
        return Scm_UcsToChar((int)(ucs + pcm->to_lower_simple));
    } else {
        return ch;
    }
#elif defined(GAUCHE_CHAR_ENCODING_UTF_8)
    if (ch == 0x130 || ch == 0x131) {
        /* char-foldcase is identity for
           U+0130 Turkish I (LATIN CAPITAL LETTER I WITH DOT ABOVE) and
           U+0131 Turkish i (LATIN SMALL LETTER DOTLESS I) */
        return ch;
    }
    const ScmCharCaseMap *pcm = Scm__CharCaseMap(ch, &cm, FALSE);
    if (pcm->to_lower_simple == 0 && pcm->to_upper_simple == 0) {
        /* we don't have case folding */
        return ch;
    }
    /* Otherwise, we do (char-downcase (char-upcase ch)) */
    if (pcm->to_upper_simple != 0) {
        ch += pcm->to_upper_simple;
        pcm = Scm__CharCaseMap(ch, &cm, FALSE);
    }
    return ch + pcm->to_lower_simple;
#else
    /* In Latin-1 range, foldcase is the same as donwcase. */
    return SIMPLE_CASE(ch, &cm, lower);
#endif
}

/*-----------------------------------------------------------------
 * Pre-defined charset
 */
/* TODO: We need different definitions of character classes for different
 * character sets.  For now, I prepare the predefined table only for
 * ASCII range, that all character sets agree on.
 */

static ScmCharSet *predef_charsets[SCM_CHAR_SET_NUM_PREDEFINED_SETS] = {NULL};
static ScmInternalMutex predef_charsets_mutex;

static void install_charsets(void)
{
    SCM_INTERNAL_MUTEX_LOCK(predef_charsets_mutex);

#define CS(n)  predef_charsets[n]
    for (int i = 0; i < SCM_CHAR_SET_NUM_PREDEFINED_SETS; i++) {
        CS(i) = SCM_CHAR_SET(Scm_MakeEmptyCharSet());
    }
    for (int code = 0; code < SCM_CHAR_SET_SMALL_CHARS; code++) {
        if (isalnum(code)) MASK_SET(CS(SCM_CHAR_SET_ALNUM), code);
        if (isalpha(code)) MASK_SET(CS(SCM_CHAR_SET_ALPHA), code);
        if (iscntrl(code)) MASK_SET(CS(SCM_CHAR_SET_CNTRL), code);
        if (isdigit(code)) MASK_SET(CS(SCM_CHAR_SET_DIGIT), code);
        if (isgraph(code)) MASK_SET(CS(SCM_CHAR_SET_GRAPH), code);
        if (islower(code)) MASK_SET(CS(SCM_CHAR_SET_LOWER), code);
        if (isprint(code)) MASK_SET(CS(SCM_CHAR_SET_PRINT), code);
        if (ispunct(code)) MASK_SET(CS(SCM_CHAR_SET_PUNCT), code);
        if (isspace(code)) MASK_SET(CS(SCM_CHAR_SET_SPACE), code);
        if (isupper(code)) MASK_SET(CS(SCM_CHAR_SET_UPPER), code);
        if (isxdigit(code)) MASK_SET(CS(SCM_CHAR_SET_XDIGIT), code);
        /* Default word constituent chars #[\w].  NB: in future versions,
           a parameter might be introduced to customize this set. */
        if (isalnum(code)||code=='_')
            MASK_SET(CS(SCM_CHAR_SET_WORD), code);
        /* isblank() is not in posix.  for now, I hardcode it. */
        if (code == ' ' || code == '\t')
            MASK_SET(CS(SCM_CHAR_SET_BLANK), code);
    }
#undef CS
    SCM_INTERNAL_MUTEX_UNLOCK(predef_charsets_mutex);
}

ScmObj Scm_GetStandardCharSet(int id)
{
    if (id < 0 || id >= SCM_CHAR_SET_NUM_PREDEFINED_SETS)
        Scm_Error("bad id for predefined charset index: %d", id);
    if (predef_charsets[id] == NULL) {
        install_charsets();
    }
    return SCM_OBJ(predef_charsets[id]);
}

void Scm__InitChar(void)
{
    SCM_INTERNAL_MUTEX_INIT(predef_charsets_mutex);

    /* Expose internal charset */
#if defined(GAUCHE_CHAR_ENCODING_EUC_JP)
    Scm_AddFeature("gauche.ces.eucjp", NULL);
#elif defined(GAUCHE_CHAR_ENCODING_SJIS)
    Scm_AddFeature("gauche.ces.sjis", NULL);
#elif defined(GAUCHE_CHAR_ENCODING_UTF_8)
    Scm_AddFeature("gauche.ces.utf8", NULL);
#else
    Scm_AddFeature("gauche.ces.none", NULL);
#endif
}
