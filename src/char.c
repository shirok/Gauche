/*
 * char.c - character and character set operations
 *
 *   Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
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
 *
 *  $Id: char.c,v 1.45 2007-02-13 12:08:26 shirok Exp $
 */

#include <ctype.h>
#define LIBGAUCHE_BODY
#include "gauche.h"

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
   Radix is assumed in the range [2, 36] */
int Scm_DigitToInt(ScmChar ch, int radix)
{
    if (ch < '0') return -1;
    if (radix <= 10) {
        if (ch < '0' + radix) return (ch - '0');
    } else {
        if (ch <= '9') return (ch - '0');
        if (ch < 'A') return -1;
        if (ch < 'A' + radix - 10) return (ch - 'A' + 10);
        if (ch < 'a') return -1;
        if (ch < 'a' + radix - 10) return (ch - 'a' + 10);
    }
    return -1;
}

ScmChar Scm_IntToDigit(int n, int radix)
{
    if (n < 0) return SCM_CHAR_INVALID;
    if (radix <= 10) {
        if (n < radix) return (ScmChar)(n + '0');
        else return SCM_CHAR_INVALID;
    } else {
        if (n < 10) return (ScmChar)(n + '0');
        if (n < radix) return (ScmChar)(n - 10 + 'a');
        else return SCM_CHAR_INVALID;
    }
}

/*
 * Convert UCS4 code <-> character
 * If the native encoding is not utf-8, gauche.charconv module is loaded.
 */
ScmChar (*Scm_UcsToCharHook)(int ucs4) = NULL;  /* filled by ext/charconv */
int (*Scm_CharToUcsHook)(ScmChar ch) = NULL;    /* filled by ext/charconv */

ScmChar Scm_UcsToChar(int n)
{
    if (n < 0) Scm_Error("bad character code: %d", n);
#if defined(GAUCHE_CHAR_ENCODING_UTF_8)
    return (ScmChar)n;
#elif defined(GAUCHE_CHAR_ENCODING_EUC_JP) || defined(GAUCHE_CHAR_ENCODING_SJIS)
    if (n < 0x80) return (ScmChar)n; /*ASCII range*/
    if (Scm_UcsToCharHook == NULL) {
        /* NB: we don't need mutex here, for the loading of gauche.charconv
           is serialized in Scm_Require. */
        Scm_Require(SCM_MAKE_STR("gauche/charconv"));
        if (Scm_UcsToCharHook == NULL) {
            Scm_Error("couldn't autoload gauche.charconv");
        }
    }
    return Scm_UcsToCharHook(n);
#else
    if (n < 0x100) return (ScmChar)n; /* ISO8859-1 */
    else return SCM_CHAR_INVALID;
#endif
}

int Scm_CharToUcs(ScmChar ch)
{
    if (ch == SCM_CHAR_INVALID) Scm_Error("bad character");
#if defined(GAUCHE_CHAR_ENCODING_UTF_8)
    return (int)ch;
#elif defined(GAUCHE_CHAR_ENCODING_EUC_JP) || defined(GAUCHE_CHAR_ENCODING_SJIS)
    if (ch < 0x80) return (int)ch; /*ASCII range*/
    if (Scm_CharToUcsHook == NULL) {
        /* NB: we don't need mutex here, for the loading of gauche.charconv
           is serialized in Scm_Require. */
        Scm_Require(SCM_MAKE_STR("gauche/charconv"));
        if (Scm_CharToUcsHook == NULL) {
            Scm_Error("couldn't autoload gauche.charconv");
        }
    }
    return Scm_CharToUcsHook(ch);
#else
    return (int)ch;             /* ISO8859-1 */
#endif /*!GAUCHE_CHAR_ENCODING_UTF_8*/
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
                         SCM_CLASS_DEFAULT_CPL);

/* masks */
#if SIZEOF_LONG == 4
#define MASK_BIT_SHIFT  5
#define MASK_BIT_MASK   0x1f
#elif SIZEOF_LONG == 8
#define MASK_BIT_SHIFT  6
#define MASK_BIT_MASK   0x3f
#elif SIZEOF_LONG == 16    /* maybe, in some future ... */
#define MASK_BIT_SHIFT  7
#define MASK_BIT_MASK   0x7f
#else
#error need to set SIZEOF_LONG
#endif

#define MASK_INDEX(ch)       ((ch) >> MASK_BIT_SHIFT)
#define MASK_BIT(ch)         (1L << ((ch) & MASK_BIT_MASK))
#define MASK_ISSET(cs, ch)   (!!(cs->mask[MASK_INDEX(ch)] & MASK_BIT(ch)))
#define MASK_SET(cs, ch)     (cs->mask[MASK_INDEX(ch)] |= MASK_BIT(ch))
#define MASK_RESET(cs, ch)   (cs->mask[MASK_INDEX(ch)] &= ~MASK_BIT(ch))

/*----------------------------------------------------------------------
 * Printer
 */
static void charset_print_ch(ScmPort *out, ScmChar ch)
{
    if (ch < 0x20 || ch == 0x7f) {
        Scm_Printf(out, "\\x%02x", ch);
    } else {
        Scm_Putc(ch, out);
    }
}

static void charset_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    int prev, code;
    ScmCharSet *cs = SCM_CHARSET(obj);
    struct ScmCharSetRange *r;

    Scm_Printf(out, "#[");
    for (prev = -1, code = 0; code < SCM_CHARSET_MASK_CHARS; code++) {
        if (MASK_ISSET(cs, code) && prev < 0) {
            charset_print_ch(out, code);
            prev = code;
        } 
        else if (!MASK_ISSET(cs, code) && prev >= 0) {
            if (code - prev > 1) {
                if (code - prev > 2) Scm_Printf(out, "-");
                charset_print_ch(out, code-1);
            }
            prev = -1;
        }
    }
    if (prev >= 0) {
        if (code - prev > 1) {
            if (prev < 0x7e) Scm_Printf(out, "-");
            charset_print_ch(out, code-1);
        }
    }
    for (r = cs->ranges; r; r = r->next) {
        charset_print_ch(out, r->lo);
        if (r->hi == r->lo) continue;
        if (r->hi - r->lo > 2) Scm_Printf(out, "-");
        charset_print_ch(out, r->hi);
    }
    Scm_Printf(out, "]", obj);
}

/*-----------------------------------------------------------------
 * Constructors
 */
static ScmCharSet *make_charset(void)
{
    ScmCharSet *cs = SCM_NEW(ScmCharSet);
    int i;
    SCM_SET_CLASS(cs, SCM_CLASS_CHARSET);
    for (i=0; i<SCM_CHARSET_MASK_SIZE; i++) cs->mask[i] = 0;
    cs->ranges = NULL;
    return cs;
}

ScmObj Scm_MakeEmptyCharSet(void)
{
    return SCM_OBJ(make_charset());
}

ScmObj Scm_CopyCharSet(ScmCharSet *src)
{
    ScmCharSet *dst = make_charset();
    struct ScmCharSetRange *rs, *rd = dst->ranges;
    int i;
    
    for (i=0; i<SCM_CHARSET_MASK_SIZE; i++) dst->mask[i] = src->mask[i];
    for (rs = src->ranges; rs; rs = rs->next) {
        if (rd == NULL) {
            rd = dst->ranges = SCM_NEW(struct ScmCharSetRange);
        } else {
            rd->next = SCM_NEW(struct ScmCharSetRange);
            rd = rd->next;
        }
        rd->lo = rs->lo;
        rd->hi = rs->hi;
    }
    if (rd) rd->next = NULL;
    return SCM_OBJ(dst);
}

/* Helper functions to read the escaped character code sequence, such as
   \xXX, \uXXXX, or \UXXXXXXXX.
   Scm_ReadXdigitsFromString reads from char* buffer (note that hex digits
   consist of single-byte characters in any encoding, we don't need to
   do the cumbersome multibyte handling).  Scm_ReadXdigitsFromPort reads
   from the port.  Both should be called after the prefix 'x', 'u' or 'U'
   char is read.  NDIGITS specifies either exact number of digits to be
   expected or maximum number of digits. */

/* If nextbuf == NULL, ndigits specifies exact # of digits.  Returns
   SCM_CHAR_INVALID if there are less digits.  Otherwise, ndigis specifies
   max # of digits, and the ptr to the next char is stored in nextbuf. */
ScmChar Scm_ReadXdigitsFromString(const char *buf, int ndigits,
                                  const char **nextbuf)
{
    int i, val = 0;
    for (i=0; i<ndigits; i++) {
        if (!isxdigit(buf[i])) {
            if (nextbuf == NULL) return SCM_CHAR_INVALID;
            else {
                *nextbuf = buf;
                return val;
            }
        }
        val = val * 16 + Scm_DigitToInt(buf[i], 16);
    }
    return (ScmChar)val;
}

/* ndigits specifies exact # of digits.  read chars are stored in buf
   so that they can be used in the error message.  Caller must provide
   a sufficient space for buf. */
ScmChar Scm_ReadXdigitsFromPort(ScmPort *port, int ndigits,
                                char *buf, int *nread)
{
    int i, c, val = 0, dig;
    
    for (i = 0; i < ndigits; i++) {
        SCM_GETC(c, port);
        if (c == EOF) break;
        dig = Scm_DigitToInt(c, 16);
        if (dig < 0) {
            SCM_UNGETC(c, port);
            break;
        }
        buf[i] = (char)c;       /* we know c is single byte char here. */
        val = val * 16 + dig;
    }
    *nread = i;
    if (i < ndigits) { /* error */
        return SCM_CHAR_INVALID;
    } else {
        return (ScmChar)val;
    }
}

/*-----------------------------------------------------------------
 * Comparison
 */
static int charset_compare(ScmObj x, ScmObj y, int equalp)
{
    ScmCharSet *xx = SCM_CHARSET(x);
    ScmCharSet *yy = SCM_CHARSET(y);
    
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
    int i;
    struct ScmCharSetRange *rx, *ry;
    for (i=0; i<SCM_CHARSET_MASK_SIZE; i++)
        if (x->mask[i] != y->mask[i]) return FALSE;
    for (rx=x->ranges, ry=y->ranges; rx && ry; rx=rx->next, ry=ry->next) {
        if (rx->lo != ry->lo || rx->hi != ry->hi) return FALSE;
    }
    if (rx || ry) return FALSE;
    return TRUE;
}

/* whether x <= y */
int Scm_CharSetLE(ScmCharSet *x, ScmCharSet *y)
{
    int i;
    struct ScmCharSetRange *rx, *ry;
    for (i=0; i<SCM_CHARSET_MASK_SIZE; i++)
        if ((x->mask[i] | y->mask[i]) != y->mask[i]) return FALSE;
    rx = x->ranges;
    ry = y->ranges;
    while (rx && ry) {
        if (rx->lo < ry->lo) return FALSE;
        if (rx->lo > ry->hi) { ry = ry->next; continue; }
        if (rx->hi > ry->hi) return FALSE;
        rx = rx->next;
    }
    if (rx) return FALSE;
    return TRUE;
}

/*-----------------------------------------------------------------
 * Modification
 */

static struct ScmCharSetRange *newrange(int lo, int hi,
                                        struct ScmCharSetRange *next)
{
    struct ScmCharSetRange *n = SCM_NEW(struct ScmCharSetRange);
    n->next = next;
    n->lo = lo;
    n->hi = hi;
    return n;
}

ScmObj Scm_CharSetAddRange(ScmCharSet *cs, ScmChar from, ScmChar to)
{
    int i;
    struct ScmCharSetRange *lo, *lop, *hi;
    
    if (to < from) return SCM_OBJ(cs);
    if (from < SCM_CHARSET_MASK_CHARS) {
        if (to < SCM_CHARSET_MASK_CHARS) {
            for (i=from; i<=to; i++) MASK_SET(cs, i);
            return SCM_OBJ(cs);
        }
        for (i=from; i<SCM_CHARSET_MASK_CHARS; i++)  MASK_SET(cs, i);
        from = SCM_CHARSET_MASK_CHARS;
    }
    if (cs->ranges == NULL) {
        cs->ranges = newrange(from, to, NULL);
        return SCM_OBJ(cs);
    }
    /* Add range.  Ranges are chained from lower character code to higher,
       without any overlap. */
    /* First, we scan the ranges so that we'll get...
        - if FROM is in a range, lo points to it.
        - if FROM is out of any ranges, lo points to the closest range that
          is higher than FROM.
        - if TO is in a range, hi points to the range.
        - if TO is out of any ranges, hi points to the closest range that
          is higher than TO. */
    for (lop = NULL, lo = cs->ranges; lo; lop = lo, lo = lo->next) {
        if (from <= lo->hi+1) break;
    }
    if (!lo) {
        lop->next = newrange(from, to, NULL);
        return SCM_OBJ(cs);
    }
    for (hi = lo; hi; hi = hi->next) {
        if (to <= hi->hi) break;
    }
    /* Then we insert, extend and/or merge the ranges accordingly. */
    if (from < lo->lo) { /* FROM extends the LO */
        if (lo == hi) {
            if (to < hi->lo-1) {
                if (lop == NULL) cs->ranges = newrange(from, to, lo);
                else             lop->next = newrange(from, to, lo);
            } else {
                lo->lo = from;
            }
        } else if (hi == NULL || to < hi->lo-1) {
            lo->lo = from;
            lo->hi = to;
            lo->next = hi;
        } else {
            lo->lo = from;
            lo->hi = hi->hi;
            lo->next = hi->next;
        }
    } else { /* FROM included in LO */
        if (lo != hi) {
            if (hi == NULL || to < hi->lo-1) {
                lo->hi = to;
                lo->next = hi;
            } else {
                lo->hi = hi->hi;
                lo->next = hi->next;
            }
        }
    }
    /* WRITE ME */
    return SCM_OBJ(cs);
}

ScmObj Scm_CharSetAdd(ScmCharSet *dst, ScmCharSet *src)
{
    int i;
    struct ScmCharSetRange *r;
    for (i=0; i<SCM_CHARSET_MASK_SIZE; i++)
        dst->mask[i] |= src->mask[i];
    for (r = src->ranges; r; r = r->next) {
        Scm_CharSetAddRange(dst, r->lo, r->hi);
    }
    return SCM_OBJ(dst);
}

ScmObj Scm_CharSetComplement(ScmCharSet *cs)
{
    int i, last;
    struct ScmCharSetRange *r, *p;
    for (i=0; i<SCM_CHARSET_MASK_SIZE; i++)
        cs->mask[i] = ~cs->mask[i];
    last = SCM_CHARSET_MASK_CHARS;
    for (p = NULL, r = cs->ranges; r; p = r, r = r->next) {
        int hi = r->hi+1;
        if (r->lo != SCM_CHARSET_MASK_CHARS) {
            r->hi = r->lo - 1;
            r->lo = last;
        } else {
            cs->ranges = r->next;
        }
        last = hi;
    }
    if (last < SCM_CHAR_MAX) {
        if (!p) cs->ranges = newrange(last, SCM_CHAR_MAX, NULL);
        else    p->next = newrange(last, SCM_CHAR_MAX, NULL);
    }
    return SCM_OBJ(cs);
}

/* Make charset case-insensitive.  For now, we only deal with
   ASCII range. */
ScmObj Scm_CharSetCaseFold(ScmCharSet *cs)
{
    int ch;
    for (ch='a'; ch<='z'; ch++) {
        if (MASK_ISSET(cs, ch) || MASK_ISSET(cs, (ch-('a'-'A')))) {
            MASK_SET(cs, ch);
            MASK_SET(cs, (ch-('a'-'A')));
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
    if (c < SCM_CHARSET_MASK_CHARS) return MASK_ISSET(cs, c);
    else {
        struct ScmCharSetRange *r;
        for (r = cs->ranges; r; r = r->next) {
            if (r->lo <= c && c <= r->hi) return TRUE;
        }
        return FALSE;
    }
}

/*-----------------------------------------------------------------
 * Inspection
 */

/* returns a list of ranges contained in the charset */
ScmObj Scm_CharSetRanges(ScmCharSet *cs)
{
    ScmObj h = SCM_NIL, t = SCM_NIL, cell;
    int ind, begin = 0, prev = FALSE;
    struct ScmCharSetRange *r;
    
    for (ind = 0; ind < SCM_CHARSET_MASK_CHARS; ind++) {
        int bit = MASK_ISSET(cs, ind);
        if (!prev && bit) begin = ind;
        if (prev && !bit) {
            cell = Scm_Cons(SCM_MAKE_INT(begin), SCM_MAKE_INT(ind-1));
            SCM_APPEND1(h, t, cell);
        }
        prev = bit;
    }
    if (prev) {
        if (!cs->ranges || cs->ranges->lo != SCM_CHARSET_MASK_CHARS) {
            cell = Scm_Cons(SCM_MAKE_INT(begin),
                            SCM_MAKE_INT(SCM_CHARSET_MASK_CHARS-1));
            SCM_APPEND1(h, t, cell);
            r = cs->ranges;
        } else {
            cell = Scm_Cons(SCM_MAKE_INT(begin), SCM_MAKE_INT(cs->ranges->hi));
            SCM_APPEND1(h, t, cell);
            r = cs->ranges->next;
        }
    } else {
        r = cs->ranges;
    }
    for (; r; r = r->next) {
        cell = Scm_Cons(SCM_MAKE_INT(r->lo), SCM_MAKE_INT(r->hi));
        SCM_APPEND1(h, t, cell);
    }
    return h;
}

#if SCM_DEBUG_HELPER
void Scm_CharSetDump(ScmCharSet *cs, ScmPort *port)
{
    int i;
    struct ScmCharSetRange *r;
    Scm_Printf(port, "CharSet %p\nmask:", cs);
    for (i=0; i<SCM_CHARSET_MASK_SIZE; i++)
        Scm_Printf(port, "[%08x]", cs->mask[i]);
    Scm_Printf(port, "\nranges:");
    for (r=cs->ranges; r; r=r->next)
        Scm_Printf(port, "(%d-%d)", r->lo, r->hi);
    Scm_Printf(port, "\n");
}
#endif /* SCM_DEBUG_HELPER */

/*-----------------------------------------------------------------
 * Reader
 */

/* Read \x, \u, \U escape sequence in the charset spec. */
static ScmChar read_charset_xdigits(ScmPort *port, int ndigs, int key)
{
    char buf[8];
    int nread;
    ScmChar r;
    SCM_ASSERT(ndigs <= 8);
    r = Scm_ReadXdigitsFromPort(port, ndigs, buf, &nread);
    if (r == SCM_CHAR_INVALID) {
        ScmDString ds;
        int c, i;
        /* skip chars to the end of regexp, so that the reader will read
           after the erroneous string */
        for (;;) {
            SCM_GETC(c, port);
            if (c == EOF || c == ']') break;
            if (c == '\\') SCM_GETC(c, port);
        }
        /* construct an error message */
        Scm_DStringInit(&ds);
        Scm_DStringPutc(&ds, '\\');
        Scm_DStringPutc(&ds, key);
        for (i=0; i<nread; i++) Scm_DStringPutc(&ds, (unsigned char)buf[i]);
        Scm_Error("Bad '\\%c' escape sequence in a char-set literal: %s",
                  key, Scm_DStringGetz(&ds));
    }
    return r;
}

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

static ScmObj read_predef_charset(ScmPort*, ScmObj*);

ScmObj Scm_CharSetRead(ScmPort *input, int *complement_p,
                       int error_p, int bracket_syntax)
{
#define REAL_BEGIN 1
#define CARET_BEGIN 2
    int begin = REAL_BEGIN, complement = FALSE;
    int lastchar = -1, inrange = FALSE, moreset_complement = FALSE;
    ScmCharSet *set = SCM_CHARSET(Scm_MakeEmptyCharSet());
    ScmObj moreset;
    ScmObj chars = SCM_NIL;
    ScmChar ch = 0;

    for (;;) {
        SCM_GETC(ch, input);
        if (ch == EOF) goto err;
        chars = Scm_Cons(SCM_MAKE_CHAR(ch), chars);

        if (begin == REAL_BEGIN && ch == '^') {
            complement = TRUE;
            begin = CARET_BEGIN;
            continue;
        }
        if (bracket_syntax && begin && ch == ']') {
            Scm_CharSetAddRange(set, ch, ch);
            lastchar = ch;
            begin = FALSE;
            continue;
        }
        begin = FALSE;

        switch (ch) {
        case '-':
            if (inrange) goto ordchar;
            inrange = TRUE;
            continue;
        case ']':
            if (inrange) {
                if (lastchar >= 0) {
                    Scm_CharSetAddRange(set, lastchar, lastchar);
                    Scm_CharSetAddRange(set, '-', '-');
                } else {
                    Scm_CharSetAddRange(set, '-', '-');
                }
            }
            break;
        case '\\':
            SCM_GETC(ch, input);
            if (ch == SCM_CHAR_INVALID) goto err;
            chars = Scm_Cons(SCM_MAKE_CHAR(ch), chars);
            switch (ch) {
            case 'a': ch = 7; goto ordchar;
            case 'b': ch = 8; goto ordchar;
            case 'n': ch = '\n'; goto ordchar;
            case 'r': ch = '\r'; goto ordchar;
            case 't': ch = '\t'; goto ordchar;
            case 'f': ch = '\f'; goto ordchar;
            case 'e': ch = 0x1b; goto ordchar;
            case 'x':
                ch = read_charset_xdigits(input, 2, 'x'); goto ordchar;
            case 'u':
                ch = Scm_UcsToChar(read_charset_xdigits(input, 4, 'u'));
                goto ordchar;
            case 'U':
                ch = Scm_UcsToChar(read_charset_xdigits(input, 8, 'U'));
                goto ordchar;
            case 'd':
                moreset_complement = FALSE;
                moreset = Scm_GetStandardCharSet(SCM_CHARSET_DIGIT);
                break;
            case 'D':
                moreset_complement = TRUE;
                moreset = Scm_GetStandardCharSet(SCM_CHARSET_DIGIT);
                break;
            case 's':
                moreset_complement = FALSE;
                moreset = Scm_GetStandardCharSet(SCM_CHARSET_SPACE);
                break;
            case 'S':
                moreset_complement = TRUE;
                moreset = Scm_GetStandardCharSet(SCM_CHARSET_SPACE);
                break;
            case 'w':
                moreset_complement = FALSE;
                moreset = Scm_GetStandardCharSet(SCM_CHARSET_WORD);
                break;
            case 'W':
                moreset_complement = TRUE;
                moreset = Scm_GetStandardCharSet(SCM_CHARSET_WORD);
                break;
            default:
                goto ordchar;
            }
            if (moreset_complement) {
                moreset = Scm_CharSetComplement(SCM_CHARSET(Scm_CopyCharSet(SCM_CHARSET(moreset))));
            }
            Scm_CharSetAdd(set, SCM_CHARSET(moreset));
            continue;
        case '[':
            moreset = read_predef_charset(input, &chars);
            if (!SCM_CHARSETP(moreset)) goto err;
            Scm_CharSetAdd(set, SCM_CHARSET(moreset));
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
    if (complement_p) {
        *complement_p = complement;
        return SCM_OBJ(set);
    } else {
        if (complement) Scm_CharSetComplement(set);
        return SCM_OBJ(set);
    }
  err:
    if (error_p)
        Scm_Error("Unclosed bracket in charset syntax [%A",
                  Scm_ListToString(Scm_ReverseX(chars)));
    return SCM_FALSE;
}

/* Read posix [:alpha:] etc.  The first '[' is already read.
   Return #f on error.  Set reverse list of read chars in *chars */
#define MAX_CHARSET_NAME_LEN  10
ScmObj read_predef_charset(ScmPort *input, ScmObj *chars)
{
    int i;
    char name[MAX_CHARSET_NAME_LEN];
    ScmChar ch;
    for (i=0; i<MAX_CHARSET_NAME_LEN; i++) {
        SCM_GETC(ch, input);
        if (ch == SCM_CHAR_INVALID) return SCM_FALSE;
        *chars = Scm_Cons(SCM_MAKE_CHAR(ch), *chars);
        if (!SCM_CHAR_ASCII_P(ch)) break;
        if (ch != ']') {
            name[i] = ch;
            continue;
        }
        if (strncmp(name, ":alnum:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHARSET_ALNUM);
        } else if (strncmp(name, ":alpha:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHARSET_ALPHA);
        } else if (strncmp(name, ":blank:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHARSET_BLANK);
        } else if (strncmp(name, ":cntrl:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHARSET_CNTRL);
        } else if (strncmp(name, ":digit:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHARSET_DIGIT);
        } else if (strncmp(name, ":graph:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHARSET_GRAPH);
        } else if (strncmp(name, ":lower:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHARSET_LOWER);
        } else if (strncmp(name, ":print:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHARSET_PRINT);
        } else if (strncmp(name, ":punct:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHARSET_PUNCT);
        } else if (strncmp(name, ":space:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHARSET_SPACE);
        } else if (strncmp(name, ":upper:", 7) == 0) {
            return Scm_GetStandardCharSet(SCM_CHARSET_UPPER);
        } else if (strncmp(name, ":xdigit:", 8) == 0) {
            return Scm_GetStandardCharSet(SCM_CHARSET_XDIGIT);
        } else break;
    }
    /* here we got invalid charset name */
    name[i] = '\0';
    Scm_Error("invalid or unsupported POSIX charset '[%s]'", name);
    return SCM_FALSE;
}

/*-----------------------------------------------------------------
 * Pre-defined charset
 */
/* TODO: We need different definitions of character classes for different
 * character sets.  For now, I prepare the predefined table only for
 * ASCII range, that all character sets agree on.
 */

static ScmCharSet *predef_charsets[SCM_CHARSET_NUM_PREDEFINED_SETS] = {NULL};
static ScmInternalMutex predef_charsets_mutex;

static void install_charsets(void)
{
    int i, code;

    SCM_INTERNAL_MUTEX_LOCK(predef_charsets_mutex);

#define CS(n)  predef_charsets[n]
    for (i = 0; i < SCM_CHARSET_NUM_PREDEFINED_SETS; i++) {
        CS(i) = SCM_CHARSET(Scm_MakeEmptyCharSet());
    }
    for (code = 0; code < SCM_CHARSET_MASK_CHARS; code++) {
        if (isalnum(code)) MASK_SET(CS(SCM_CHARSET_ALNUM), code);
        if (isalpha(code)) MASK_SET(CS(SCM_CHARSET_ALPHA), code);
        if (iscntrl(code)) MASK_SET(CS(SCM_CHARSET_CNTRL), code);
        if (isdigit(code)) MASK_SET(CS(SCM_CHARSET_DIGIT), code);
        if (isgraph(code)) MASK_SET(CS(SCM_CHARSET_GRAPH), code);
        if (islower(code)) MASK_SET(CS(SCM_CHARSET_LOWER), code);
        if (isprint(code)) MASK_SET(CS(SCM_CHARSET_PRINT), code);
        if (ispunct(code)) MASK_SET(CS(SCM_CHARSET_PUNCT), code);
        if (isspace(code)) MASK_SET(CS(SCM_CHARSET_SPACE), code);
        if (isupper(code)) MASK_SET(CS(SCM_CHARSET_UPPER), code);
        if (isxdigit(code)) MASK_SET(CS(SCM_CHARSET_XDIGIT), code);
        /* Default word constituent chars #[\w].  NB: in future versions,
           a parameter might be introduced to customize this set. */
        if (isalnum(code)||code=='_')
            MASK_SET(CS(SCM_CHARSET_WORD), code);
        /* isblank() is not in posix.  for now, I hardcode it. */
        if (code == ' ' || code == '\t')
            MASK_SET(CS(SCM_CHARSET_BLANK), code);
    }
#undef CS
    SCM_INTERNAL_MUTEX_UNLOCK(predef_charsets_mutex);
}

ScmObj Scm_GetStandardCharSet(int id)
{
    if (id < 0 || id >= SCM_CHARSET_NUM_PREDEFINED_SETS)
        Scm_Error("bad id for predefined charset index: %d", id);
    if (predef_charsets[id] == NULL) {
        install_charsets();
    }
    return SCM_OBJ(predef_charsets[id]);
}

void Scm__InitChar(void)
{
    SCM_INTERNAL_MUTEX_INIT(predef_charsets_mutex);
}
