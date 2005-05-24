/*
 * regexp.c - regular expression
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
 *  $Id: regexp.c,v 1.52 2005-05-24 07:46:14 shirok Exp $
 */

#include <setjmp.h>
#include <ctype.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"
#include "gauche/builtin-syms.h"

/* I don't like to reinvent wheels, so I looked for a regexp implementation
 * that can handle multibyte encodings and not bound to Unicode.
 * Without assuming Unicode it'll be difficult to define character classes
 * correctly, but there are domains that you don't want to do native
 * charset <-> UTF-8 each time for regexp match, trading correctness of
 * character classes.
 *
 * The most recent version of famous Henry Spencer's regex is found in Tcl
 * 8.3, that supports wide characters (the state machine seems to work
 * with UCS-4, but the internal tables seem to be set up for UCS-2 only).
 * Tcl does UTF-8 <-> UCS-2 conversion in order to do regexp match.
 *
 * Lots of variants of Spencer's old regex code is floating around, such
 * as http://arglist.com/regex/ and the one in BSD.   They don't support
 * multibyte strings, as far as I know.
 *
 * Another popular package is PCRE.  PCRE 3.4 has UTF-8 support, but only
 * experimentally.
 *
 * None seems to satisfy my criteria.
 *
 * So I reluctantly started to write my own.  I don't think I can beat
 * those guys, and am willing to grab someone's code anytime if it's suitable
 * for my purpose and under a license like BSD one.  
 */

/*
 * The idea here is to match string without converting mb <-> char as
 * much as possible.  Actually, the converion is done only when we see
 * large character sets.
 *
 * The engine is a sort of NFA, by keeping state information for backtrack
 * in C stack.  It'll bust the C stack if you try to match something like
 * (..)* with a long input string (there's a code to check the stack size
 * and aborts matching when the recursion goes too deep).
 * A possible fix is to check if recursion level exceeds some limit,
 * then save the C stack into heap (as in the C-stack-copying continuation
 * does) and reuse the stack area.
 */

/* Instructions */
enum {
    RE_MATCH1,                  /* followed by 1 byte to match */
    RE_MATCH,                   /* followed by length, and bytes to match */
    RE_MATCH1_CI,               /* case insenstive match */
    RE_MATCH_CI,                /* case insenstive match */
    RE_ANY,                     /* match any char */
    RE_TRY,                     /* followed by offset (2 bytes). try matching
                                   the following sequence, and if fails,
                                   jump to offset. */
    RE_SET,                     /* followed by charset #.  match any char in
                                   the charset. */
    RE_NSET,                    /* followed by charset #.  mathc any char but
                                   in the charset */
    RE_SET1,                    /* followed by charset #.  match any char in
                                   the charset.  guaranteed that the charset
                                   holds only range 0-127 */
    RE_NSET1,                   /* followed by charset #.  match any char
                                   but the ones in the charset.  guaranteed
                                   that the charset holds only range 0-127. */
    RE_JUMP,                    /* followed by offset (2 bytes).  jump to that
                                   bytecode. */
    RE_FAIL,                    /* fail */
    RE_SUCCESS,                 /* success */
    RE_BEGIN,                   /* followed by a group number.  start the
                                   group. */
    RE_END,                     /* followed by a group number.  end the
                                   group. */
    RE_BOL,                     /* beginning of line assertion */
    RE_EOL,                     /* end of line assertion */
    RE_WB,                      /* word boundary assertion */
    RE_NWB,                     /* negative word boundary assertion */
    RE_ASSERT,                  /* positive look-ahead assertion. followed by
                                   offset (2 bytes). */
    RE_NASSERT,                 /* negative look-ahead assertion. followed by
                                 * offset (2 bytes). */
    /* The following instructions are not necessary to implement the basic
       engine, but used in the optimized code */
    RE_MATCH1B,                 /* (match 1 byte or branch)
                                   followed by a byte, and offset.
                                   if the next byte matches the input,
                                   proceed.  otherwise, jump to the offset. */
    RE_SET1R,                   /* (1-byte set match repeat)
                                   followed by charset #.  Consumes all input
                                   that matches the given set. */
    RE_NSET1R,                  /* (1-byte negative set match repeat)
                                   followed by charset #.  Consumes all input
                                   that don't match the given set. */
    RE_SETR,                    /* (set match repeat)
                                   followed by charset #.  Consumes all input
                                   that matches the given set. */
    RE_NSETR,                   /* (negative set match repeat)
                                   followed by charset #.  Consumes all input
                                   that don't match the given set. */
    RE_NUM_INSN
};

/* maximum # of {n,m}-type limited repeat count */
#define MAX_LIMITED_REPEAT 255

/* internal regexp flag. */
#define SCM_REGEXP_BOL_ANCHORED   (1L<<2)

/* AST - the first pass of regexp compiler creates intermediate AST.
 * Alternatively, you can provide AST directly to the regexp compiler,
 * using Scm_RegCompFromAST().
 *
 *  <ast> : (<element> ...)
 *  
 *  <element> : <clause>   ; special clause
 *         | <item>        ; matches <item>
 *
 *  <item> : <char>       ; matches char
 *         | <char-set>   ; matches char set
 *         | (comp . <char-set>) ; matches complement of char set
 *         | any          ; matches any char
 *         | bol | eol    ; beginning/end of line assertion
 *         | wb | nwb     ; word-boundary/negative word boundary assertion
 *
 *  <clause> : (seq . <ast>)      ; sequence
 *           | (seq-uncase . <ast>) ; sequence (case insensitive match)
 *           | (seq-case . <ast>) ; sequence (case sensitive match)
 *           | (alt . <ast>)      ; alternative
 *           | (rep . <ast>)      ; 0 or more repetition of <ast> (greedy)
 *           | (rep-min . <ast>)  ; 0 or more repetition of <ast> (lazy)
 *           | (rep-bound <n> . <ast>) ; repetition up to <n> (greedy)
 *           | (rep-bound-min <n> . <ast>) ; repetition up to <n> (lazy)
 *           | (rep-while . <ast>) ; like rep, but no backtrack
 *           | (<integer> . <ast>) ; capturing group 
 *           | (assert . <ast>)   ; positive look-ahead assertion
 *           | (nassert . <ast>)  ; negative look-ahead assertion
 * 
 * For seq-uncase, items inside <ast> has to be prepared for case-insensitive
 * match, i.e. chars have to be downcased and char-sets have to be 
 * case-folded.
 */
   
static void regexp_print(ScmObj obj, ScmPort *port, ScmWriteContext *c);
static int  regexp_compare(ScmObj x, ScmObj y, int equalp);

SCM_DEFINE_BUILTIN_CLASS(Scm_RegexpClass,
                         regexp_print, regexp_compare, NULL, NULL,
                         SCM_CLASS_DEFAULT_CPL);
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_RegMatchClass, NULL);

static ScmRegexp *make_regexp(void)
{
    ScmRegexp *rx = SCM_NEW(ScmRegexp);
    SCM_SET_CLASS(rx, SCM_CLASS_REGEXP);
    rx->code = NULL;
    rx->numCodes = 0;
    rx->numGroups = 0;
    rx->numSets = 0;
    rx->sets = NULL;
    rx->mustMatch = NULL;
    rx->flags = 0;
    rx->pattern = NULL;
    return rx;
}

static void regexp_print(ScmObj rx, ScmPort *out, ScmWriteContext *ctx)
{
    if (SCM_REGEXP(rx)->pattern) {
        Scm_Printf(out, "#/%A/", SCM_REGEXP(rx)->pattern);
    } else {
        /* fail safe */
        Scm_Printf(out, "#<regexp %p>", rx);
    }
}

static int regexp_compare(ScmObj x, ScmObj y, int equalp)
{
    if (!equalp) {
        Scm_Error("cannot compare regexps: %S and %S", x, y);
    }
    return !(SCM_REGEXP(x)->pattern
             && SCM_REGEXP(y)->pattern
             && Scm_StringEqual(SCM_STRING(SCM_REGEXP(x)->pattern),
                                SCM_STRING(SCM_REGEXP(y)->pattern))
             && ((SCM_REGEXP(x)->flags&SCM_REGEXP_CASE_FOLD)
                 == (SCM_REGEXP(y)->flags&SCM_REGEXP_CASE_FOLD)));
}

#ifndef CHAR_MAX
#define CHAR_MAX 256
#endif

#define REGEXP_OFFSET_MAX 65535

/*=======================================================================
 * Compiler
 */

/* 3-pass compiler.
 *
 *  pass 1: parses the pattern and creates an AST.
 *  pass 2: optimize on AST.
 *  pass 3: byte code generation.
 */

/* compiler state information */
typedef struct regcomp_ctx_rec {
    ScmRegexp *rx;              /* the building regexp */
    ScmString *pattern;         /* original pattern */
    int casefoldp;              /* TRUE if case-folding match */
    ScmPort *ipat;              /* [pass1] string port for pattern */
    ScmObj sets;                /* [pass1] list of charsets */
    int grpcount;               /* [pass1] group count */
    unsigned char *code;        /* [pass3] code being built */
    int codep;                  /* [pass3] front of code generation */
    int emitp;                  /* [pass3] am I generating code? */
    int codemax;                /* [pass3] max codep */
} regcomp_ctx;

static void rc_ctx_init(regcomp_ctx *ctx, ScmRegexp *rx)
{
    ctx->rx = rx;
    ctx->pattern = rx->pattern;
    ctx->casefoldp = FALSE;
    if (rx->pattern) {
        ctx->ipat = SCM_PORT(Scm_MakeInputStringPort(rx->pattern, FALSE));
    } else {
        ctx->ipat = NULL;
    }
    ctx->sets = SCM_NIL;
    ctx->grpcount = 0;
    ctx->code = NULL;
    ctx->codep = 0;
    ctx->emitp = FALSE;
    ctx->codemax = 1;
}

static ScmObj rc_charset(regcomp_ctx *ctx);
static void rc_register_charset(regcomp_ctx *ctx, ScmCharSet *cs);
static ScmObj rc1_maybe_lazy(regcomp_ctx *ctx, ScmObj greedy, ScmObj lazy);
static ScmObj rc1_lex_minmax(regcomp_ctx *ctx);
static ScmObj rc1_lex_open_paren(regcomp_ctx *ctx);
static ScmChar rc1_lex_xdigits(ScmPort *port, int ndigs, int key);

/*----------------------------------------------------------------
 * pass1 - parser
 */

/* EBNF Syntax of Gauche's regexp.
 * This is a rough sketch.  The condition of BOL/EOL ("^" and "$"), for
 * example, has to be dealt with context information.
 * To follow the convention, "{" and "}" token that don't appear to
 * consist of the "limited repetition" syntax are regarded as literal
 * characters.
 *
 *  <re>   :
 *         | <re> <alt>
 *
 *  <alt>  :
 *         | <item>
 *         | <alt> "|" <item>
 *
 *  <item> : <atom> "*"
 *         | <atom> "+"
 *         | <atom> "?"
 *         | <atom> "{" <n> ("," <m>?)? "}"
 *         | <atom> "*?"
 *         | <atom> "+?"
 *         | <atom> "??"
 *         | <atom> "{" <n> ("," <m>?)? "}?"
 *         | <atom>
 *
 *  <atom> : a normal char, an escaped char, or a char-set
 *         | "(" <re> ")"       ;; grouping w/  capturing
 *         | "(?:"   <re> ")"   ;; grouping w/o capturing
 *         | "(?i:"  <re> ")"   ;; grouping w/o capturing (case insensitive)
 *         | "(?-i:" <re> ")"   ;; grouping w/o capturing (case sensitive)
 *         | "(?="   <re> ")"   ;; positive look-ahead assertion
 *         | "(?!"   <re> ")"   ;; negative look-ahead assertion
 */

/* Lexer */
static ScmObj rc1_lex(regcomp_ctx *ctx)
{
    ScmChar ch;
    ScmObj cs;

    ch = Scm_GetcUnsafe(ctx->ipat);
    if (ch == SCM_CHAR_INVALID) return SCM_EOF;
    switch (ch) {
    case '(': return rc1_lex_open_paren(ctx);
    case ')': return SCM_SYM_CLOSE_PAREN;
    case '|': return SCM_SYM_ALT;
    case '^': return SCM_SYM_BOL;
    case '.': return SCM_SYM_ANY;
    case '$': return SCM_SYM_EOL;
    case '[': return rc_charset(ctx);
    case '{': return rc1_lex_minmax(ctx);
    case '+': return rc1_maybe_lazy(ctx, SCM_SYM_PLUS, SCM_SYM_PLUSQ);
    case '*': return rc1_maybe_lazy(ctx, SCM_SYM_STAR, SCM_SYM_STARQ);
    case '?': return rc1_maybe_lazy(ctx, SCM_SYM_QUESTION, SCM_SYM_QUESTIONQ);
    case '\\':
        ch = Scm_GetcUnsafe(ctx->ipat);
        if (ch == SCM_CHAR_INVALID) {
            Scm_Error("stray backslash at the end of pattern: %S\n",
                      ctx->pattern);
        }
        switch (ch) {
        case 'a': return SCM_MAKE_CHAR(0x07);
        case 'n': return SCM_MAKE_CHAR('\n');
        case 'r': return SCM_MAKE_CHAR('\r');
        case 't': return SCM_MAKE_CHAR('\t');
        case 'f': return SCM_MAKE_CHAR('\f');
        case 'e': return SCM_MAKE_CHAR(0x1b);
        case 'b': return SCM_SYM_WB;
        case 'B': return SCM_SYM_NWB;
        case 'x':
            ch = rc1_lex_xdigits(ctx->ipat, 2, 'x');
            return SCM_MAKE_CHAR(ch);
        case 'u':
            ch = rc1_lex_xdigits(ctx->ipat, 4, 'u');
            return SCM_MAKE_CHAR(Scm_UcsToChar(ch));
        case 'U':
            ch = rc1_lex_xdigits(ctx->ipat, 8, 'U');
            return SCM_MAKE_CHAR(Scm_UcsToChar(ch));
        case 'd':
            cs = Scm_GetStandardCharSet(SCM_CHARSET_DIGIT);
            rc_register_charset(ctx, SCM_CHARSET(cs));
            return cs;
        case 'D':
            cs = Scm_GetStandardCharSet(SCM_CHARSET_DIGIT);
            rc_register_charset(ctx, SCM_CHARSET(cs));
            return Scm_Cons(SCM_SYM_COMP, cs);
        case 'w':
            cs = Scm_GetStandardCharSet(SCM_CHARSET_WORD);
            rc_register_charset(ctx, SCM_CHARSET(cs));
            return cs;
        case 'W':
            cs = Scm_GetStandardCharSet(SCM_CHARSET_WORD);
            rc_register_charset(ctx, SCM_CHARSET(cs));
            return Scm_Cons(SCM_SYM_COMP, cs);
        case 's':
            cs = Scm_GetStandardCharSet(SCM_CHARSET_SPACE);
            rc_register_charset(ctx, SCM_CHARSET(cs));
            return cs;
        case 'S':
            cs = Scm_GetStandardCharSet(SCM_CHARSET_SPACE);
            rc_register_charset(ctx, SCM_CHARSET(cs));
            return Scm_Cons(SCM_SYM_COMP, cs);
        }
        /*FALLTHROUGH*/
    default:
        if (ctx->casefoldp) ch = SCM_CHAR_DOWNCASE(ch);
        return SCM_MAKE_CHAR(ch);
    }
    /*NOTREACHED*/
}

/* Read \x, \u, \U escape sequence in the regexp spec. */
static ScmChar rc1_lex_xdigits(ScmPort *port, int ndigs, int key)
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
            if (c == EOF || c == '/') break;
            if (c == '\\') SCM_GETC(c, port);
        }
        /* construct an error message */
        Scm_DStringInit(&ds);
        Scm_DStringPutc(&ds, '\\');
        Scm_DStringPutc(&ds, key);
        for (i=0; i<nread; i++) Scm_DStringPutc(&ds, (unsigned char)buf[i]);
        Scm_Error("Bad '\\%c' escape sequence in a regexp literal: %s",
                  key, Scm_DStringGetz(&ds));
    }
    return r;
}

/* Called after '+', '*' or '?' is read, and check if there's a
   following '?' (lazy quantifier) */
static ScmObj rc1_maybe_lazy(regcomp_ctx *ctx, ScmObj greedy, ScmObj lazy)
{
    ScmChar ch = Scm_GetcUnsafe(ctx->ipat);
    if (ch == '?') return lazy;
    Scm_UngetcUnsafe(ch, ctx->ipat);
    return greedy;
}

/* Reads '('-sequence - either one of "(", "(?:", "(?i:" or "(?-i:".
   The leading "(" has already been read. */
static ScmObj rc1_lex_open_paren(regcomp_ctx *ctx)
{
    ScmObj pos;
    ScmChar ch;
    
    pos = Scm_PortSeekUnsafe(ctx->ipat, SCM_MAKE_INT(0), SEEK_CUR);
    ch = Scm_GetcUnsafe(ctx->ipat);
    if (ch != '?') {
        Scm_UngetcUnsafe(ch, ctx->ipat);
        return SCM_SYM_OPEN_PAREN;
    }
    ch = Scm_GetcUnsafe(ctx->ipat);
    if (ch == ':') return SCM_SYM_SEQ;
    if (ch == '=') return SCM_SYM_ASSERT;
    if (ch == '!') return SCM_SYM_NASSERT;
    if (ch == 'i') {
        ch = Scm_GetcUnsafe(ctx->ipat);
        if (ch == ':') return SCM_SYM_SEQ_UNCASE;
        /* fall through */
    } else if (ch == '-') {
        ch = Scm_GetcUnsafe(ctx->ipat);
        if (ch == 'i') {
            ch = Scm_GetcUnsafe(ctx->ipat);
            if (ch == ':') return SCM_SYM_SEQ_CASE;
        }
        /* fall through */
    }
    /* fail. */
    Scm_PortSeekUnsafe(ctx->ipat, pos, SEEK_SET);
    return SCM_SYM_OPEN_PAREN;
}

/* Reads {n,m}-type repeat syntax.  The leading "{" has been read.
   If the character sequence doesn't consist of valid syntax, rollback
   to the ordinary character sequence.
   If successfully parsed, returns (rep-bound <n> . <m>) where
    <m> == #f if the pattern is "{n}"   (exact count), or
    <m> == #t if the pattern is "{n,}"  (minimum count), or
    <m> == integer if the pattern is "{n,m}" (limited count).
   If the pattern is followed by '?', rep-bound-min is used instead.
 */
static ScmObj rc1_lex_minmax(regcomp_ctx *ctx)
{
    int rep_min = -1, rep_max = -1, exact = FALSE, ch;
    ScmObj pos, m;
    ScmObj type = SCM_SYM_REP_BOUND; /* default is greedy */

    pos = Scm_PortSeekUnsafe(ctx->ipat, SCM_MAKE_INT(0), SEEK_CUR);
    
    for (;;) {
        ch = Scm_GetcUnsafe(ctx->ipat);
        if (SCM_CHAR_ASCII_P(ch) && isdigit(ch)) {
            if (rep_min < 0) {
                rep_min = (ch - '0');
            } else {
                rep_min = rep_min*10 + (ch - '0');
            }
        } else if (ch == ',') {
            break;
        } else if (ch == '}') {
            exact = TRUE;
            break;
        } else {
            goto bad_min_max;
        }
    }
    if (rep_min < 0) goto bad_min_max;
    if (rep_min > MAX_LIMITED_REPEAT) goto out_of_range;
    if (!exact) {
        for (;;) {
            ch = Scm_GetcUnsafe(ctx->ipat);
            if (SCM_CHAR_ASCII_P(ch) && isdigit(ch)) {
                if (rep_max < 0) {
                    rep_max = (ch - '0');
                } else {
                    rep_max = rep_max*10 + (ch - '0');
                }
            } else if (ch == '}') {
                break;
            } else {
                goto bad_min_max;
            }
        }
        if (rep_max > MAX_LIMITED_REPEAT) goto out_of_range;
        if (rep_max >= 0 && rep_max < rep_min) {
            Scm_Error("{n,m}-syntax requires n <= m: %S", ctx->pattern);
        }
    }

    if (exact)            m = SCM_FALSE;
    else if (rep_max < 0) m = SCM_TRUE;
    else                  m = SCM_MAKE_INT(rep_max);

    ch = Scm_GetcUnsafe(ctx->ipat);
    if (ch == '?') type = SCM_SYM_REP_BOUND_MIN;
    else Scm_UngetcUnsafe(ch, ctx->ipat);
    return Scm_Cons(type, Scm_Cons(SCM_MAKE_INT(rep_min), m));

  out_of_range:
    Scm_Error("{n,m}-syntax can accept up to %d count: %S",
              MAX_LIMITED_REPEAT, ctx->pattern);
    /*NOTREACHED*/
  bad_min_max:
    /* back up */
    Scm_PortSeekUnsafe(ctx->ipat, pos, SEEK_SET);
    return SCM_MAKE_CHAR('{');
}

static ScmObj rc1_fold_alts(regcomp_ctx *ctx, ScmObj alts)
{
    ScmObj r = SCM_NIL, ap;
    SCM_FOR_EACH(ap, alts) {
        ScmObj alt = SCM_CAR(ap);
        if (SCM_PAIRP(alt) && SCM_NULLP(SCM_CDR(alt))) {
            r = Scm_Cons(SCM_CAR(alt), r);
        } else {
            r = Scm_Cons(Scm_Cons(SCM_SYM_SEQ, alt), r);
        }
    }
    return Scm_Cons(SCM_SYM_ALT, r);
}

/* Parser */
static ScmObj rc1_parse(regcomp_ctx *ctx, int bolp, int topp)
{
    ScmObj stack = SCM_NIL, alts = SCM_NIL;
    ScmObj token, item;
    int bolpsave = bolp;

#define PUSH(elt)  (stack = Scm_Cons((elt), stack))
#define PUSH1(elt) (stack = Scm_Cons((elt), SCM_CDR(stack)))

    for (;;) {
        token = rc1_lex(ctx);
        if (SCM_EOFP(token)) {
            if (!topp) {
                Scm_Error("unterminated grouping in regexp %S", ctx->pattern);
            }
            break;
        }
        if (SCM_EQ(token, SCM_SYM_CLOSE_PAREN)) {
            if (topp) {
                Scm_Error("extra close parenthesis in regexp %S", ctx->pattern);
            }
            break;
        }
        if (SCM_EQ(token, SCM_SYM_BOL)) {
            if (bolp) {
                PUSH(SCM_SYM_BOL);
                bolp = FALSE;
                continue;
            } else {
                token = SCM_MAKE_CHAR('^');
            }
            /*FALLTHROUGH*/
        }
        if (SCM_EQ(token, SCM_SYM_ALT)) {
            alts = Scm_Cons(Scm_ReverseX(stack), alts);
            stack = SCM_NIL;
            bolp = bolpsave;
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_OPEN_PAREN)) {
            int grpno = ++ctx->grpcount;
            item = rc1_parse(ctx, bolp, FALSE);
            PUSH(Scm_Cons(SCM_MAKE_INT(grpno), item));
            bolp = FALSE;
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_SEQ)) {
            item = rc1_parse(ctx, bolp, FALSE);
            PUSH(Scm_Cons(SCM_SYM_SEQ, item));
            bolp = FALSE;
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_SEQ_UNCASE) || SCM_EQ(token, SCM_SYM_SEQ_CASE)) {
            int oldflag = ctx->casefoldp;
            ctx->casefoldp = SCM_EQ(token, SCM_SYM_SEQ_UNCASE);
            item = rc1_parse(ctx, bolp, FALSE);
            PUSH(Scm_Cons(token, item));
            ctx->casefoldp = oldflag;
            bolp = FALSE;
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_ASSERT)) {
            item = rc1_parse(ctx, bolp, FALSE);
            PUSH(Scm_Cons(SCM_SYM_ASSERT, item));
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_NASSERT)) {
            item = rc1_parse(ctx, bolp, FALSE);
            PUSH(Scm_Cons(SCM_SYM_NASSERT, item));
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_STAR)) {
            /* "x*" => (rep x) */
            if (SCM_NULLP(stack)) goto synerr;
            item = SCM_LIST2(SCM_SYM_REP, SCM_CAR(stack));
            PUSH1(item);
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_STARQ)) {
            /* "x*?" => (rep-min x) */
            if (SCM_NULLP(stack)) goto synerr;
            item = SCM_LIST2(SCM_SYM_REP_MIN, SCM_CAR(stack));
            PUSH1(item);
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_PLUS)) {
            /* "x+" => (seq x (rep x)) */
            if (SCM_NULLP(stack)) goto synerr;
            item = SCM_LIST3(SCM_SYM_SEQ, SCM_CAR(stack),
                             SCM_LIST2(SCM_SYM_REP, SCM_CAR(stack)));
            PUSH1(item);
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_PLUSQ)) {
            /* "x+?" => (seq x (rep-min x)) */
            if (SCM_NULLP(stack)) goto synerr;
            item = SCM_LIST3(SCM_SYM_SEQ, SCM_CAR(stack),
                             SCM_LIST2(SCM_SYM_REP_MIN, SCM_CAR(stack)));
            PUSH1(item);
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_QUESTION)) {
            /* "x?" => (alt x ()) */
            if (SCM_NULLP(stack)) goto synerr;
            item = rc1_fold_alts(ctx,
                                 SCM_LIST2(SCM_NIL, SCM_LIST1(SCM_CAR(stack))));
            PUSH1(item);
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_QUESTIONQ)) {
            /* "x??" => (alt () x) */
            if (SCM_NULLP(stack)) goto synerr;
            item = rc1_fold_alts(ctx,
                                 SCM_LIST2(SCM_LIST1(SCM_CAR(stack)), SCM_NIL));
            PUSH1(item);
            continue;
        }
        if (SCM_PAIRP(token)&&
            (SCM_EQ(SCM_CAR(token), SCM_SYM_REP_BOUND) ||
             SCM_EQ(SCM_CAR(token), SCM_SYM_REP_BOUND_MIN))) {
            /* "x{n}"    => (seq x .... x) 
               "x{n,}"   => (seq x .... x (rep x))
               "x{n,m}"  => (seq x .... x (rep-bound m-n x))
               "x{n,}?"  => (seq x .... x (rep-min x))
               "x{n,m}?" => (seq x .... x (rep-bound-min m-n x)) */
            ScmObj n = SCM_CADR(token), m = SCM_CDDR(token);
            int greedy = SCM_EQ(SCM_CAR(token), SCM_SYM_REP_BOUND);

            if (SCM_NULLP(stack)) goto synerr;
            SCM_ASSERT(SCM_INTP(n));
            item = Scm_MakeList(SCM_INT_VALUE(n), SCM_CAR(stack));
            if (SCM_FALSEP(m)) {
                item = Scm_Cons(SCM_SYM_SEQ, item);
            } else if (SCM_TRUEP(m)) {
                item = Scm_Cons(SCM_SYM_SEQ, item);
                item = Scm_Append2X(item,
                                    SCM_LIST1(SCM_LIST2((greedy? SCM_SYM_REP : SCM_SYM_REP_MIN),
                                                        SCM_CAR(stack))));
            } else {
                int m_n;
                SCM_ASSERT(SCM_INTP(m));
                m_n = SCM_INT_VALUE(m)-SCM_INT_VALUE(n);
                SCM_ASSERT(m_n >= 0);
                item = Scm_Cons(SCM_SYM_SEQ, item);
                if (m_n > 0) {
                    item = Scm_Append2X(item,
                                        SCM_LIST1(SCM_LIST3((greedy? SCM_SYM_REP_BOUND : SCM_SYM_REP_BOUND_MIN),
                                                            SCM_MAKE_INT(m_n),
                                                            SCM_CAR(stack))));
                }
            }
            PUSH1(item);
            continue;
        }
        PUSH(token);
        bolp = FALSE;
        continue;
    }
    if (SCM_NULLP(alts)) {
        return Scm_ReverseX(stack);
    } else {
        alts = Scm_Cons(Scm_ReverseX(stack), alts);
        return SCM_LIST1(rc1_fold_alts(ctx, alts));
    }
  synerr:
    Scm_Error("bad regexp syntax in %S", ctx->pattern);
    return SCM_UNDEFINED;       /* dummy */
#undef PUSH
#undef PUSH1
}

static ScmObj rc1(regcomp_ctx *ctx)
{
    ScmObj ast = rc1_parse(ctx, TRUE, TRUE);
    if (ctx->casefoldp) {
        ast = SCM_LIST2(SCM_MAKE_INT(0), Scm_Cons(SCM_SYM_SEQ_UNCASE, ast));
    } else {
        ast = Scm_Cons(SCM_MAKE_INT(0), ast);
    }
    ctx->rx->numGroups = ctx->grpcount+1;
    return ast;
}

/* character range */
static ScmObj rc_charset(regcomp_ctx *ctx)
{
    int complement;
    ScmObj set = Scm_CharSetRead(ctx->ipat, &complement, FALSE, TRUE);
    if (!SCM_CHARSETP(set)) {
        Scm_Error("bad charset spec in pattern: %S", ctx->pattern);
    }
    if (ctx->casefoldp) {
        Scm_CharSetCaseFold(SCM_CHARSET(set));
    }
    
    rc_register_charset(ctx, SCM_CHARSET(set));
    if (complement) {
        return Scm_Cons(SCM_SYM_COMP, SCM_OBJ(set));
    } else {
        return SCM_OBJ(set);
    }
}

/* Remember charset so that we can construct charset vector later */
static void rc_register_charset(regcomp_ctx *ctx, ScmCharSet *cs)
{
    if (SCM_FALSEP(Scm_Memq(SCM_OBJ(cs), ctx->sets))) {
        ctx->sets = Scm_Cons(SCM_OBJ(cs), ctx->sets);
    }
}

/* An interlude between pass1 and pass2.  From the information of
   parser context, build a charset vector. */
static void rc_setup_charsets(ScmRegexp *rx, regcomp_ctx *ctx)
{
    ScmObj cp;
    int i = 0;
    rx->numSets = Scm_Length(ctx->sets);
    rx->sets = SCM_NEW_ARRAY(ScmCharSet*, rx->numSets);
    for (i=0, cp = Scm_Reverse(ctx->sets); !SCM_NULLP(cp); cp = SCM_CDR(cp)) {
        rx->sets[i++] = SCM_CHARSET(SCM_CAR(cp));
    }
}

/*-------------------------------------------------------------
 * pass 2: optimizer
 *
 *  - flattening nested sequences: (seq a (seq b) c) => (seq a b c)
 *  - introduces short-cut construct for certain cases.
 *       (... (rep #\a) #\b ...) => (... (rep-while #\a) #\b ...)
 */
static ScmObj rc2_optimize(ScmObj ast, ScmObj rest);
static int    is_distinct(ScmObj x, ScmObj y);

static ScmObj rc2_optimize_seq(ScmObj seq, ScmObj rest)
{
    ScmObj elt, tail, etype, opted;
    if (!SCM_PAIRP(seq)) return seq;
    elt = SCM_CAR(seq);
    tail = rc2_optimize_seq(SCM_CDR(seq), rest);
    rest = SCM_NULLP(tail)? rest : tail;
    if (!SCM_PAIRP(elt) || SCM_EQ(SCM_CAR(elt), SCM_SYM_COMP)) {
        if (SCM_EQ(tail, SCM_CDR(seq))) return seq;
        else return Scm_Cons(elt, tail);
    }
    etype = SCM_CAR(elt);
    if (SCM_EQ(etype, SCM_SYM_SEQ)) {
        return Scm_Append2(rc2_optimize_seq(SCM_CDR(elt), rest), tail);
    }
    if (SCM_EQ(etype, SCM_SYM_REP)) {
        /* If the head of repeating sequence and the beginning of the
           following sequence are distinct, like #/\s*foo/, the branch
           becomes deterministic (i.e. we don't need backtrack). */
        ScmObj repbody = rc2_optimize_seq(SCM_CDR(elt), rest);
        SCM_ASSERT(SCM_PAIRP(repbody));
        if (SCM_NULLP(rest) || is_distinct(SCM_CAR(repbody), SCM_CAR(rest))) {
            return Scm_Cons(Scm_Cons(SCM_SYM_REP_WHILE, repbody), tail);
        }
        if (SCM_EQ(repbody, SCM_CDR(elt))) opted = elt;
        else opted = Scm_Cons(SCM_SYM_REP, repbody);
    } else {
        opted = rc2_optimize(elt, rest);
    }
    if (SCM_EQ(elt, opted) && SCM_EQ(tail, SCM_CDR(seq))) return seq;
    else return Scm_Cons(opted, tail);
}

static ScmObj rc2_optimize(ScmObj ast, ScmObj rest)
{
    ScmObj type, seq, seqo;
    if (!SCM_PAIRP(ast)) return ast;
    type = SCM_CAR(ast);
    if (SCM_EQ(type, SCM_SYM_COMP)) return ast;

    if (SCM_EQ(type, SCM_SYM_ALT)) {
        ScmObj sp, sp2, e = SCM_UNBOUND, h, t;
        SCM_FOR_EACH(sp, SCM_CDR(ast)) {
            e = rc2_optimize(SCM_CAR(sp), rest);
            if (!SCM_EQ(e, SCM_CAR(sp))) break;
        }
        if (SCM_NULLP(sp)) return ast;
        /* need to copy the spine */
        h = t = SCM_NIL;
        SCM_FOR_EACH(sp2, SCM_CDR(ast)) {
            if (SCM_EQ(sp, sp2)) { SCM_APPEND1(h, t, e); break; }
            SCM_APPEND1(h, t, SCM_CAR(sp2));
        }
        SCM_FOR_EACH(sp2, SCM_CDR(sp2)) {
            SCM_APPEND1(h, t, rc2_optimize(SCM_CAR(sp2), rest));
        }
        return Scm_Cons(SCM_SYM_ALT, h);
    }
    if (SCM_EQ(type, SCM_SYM_REP_BOUND)) seq = SCM_CDDR(ast);
    else seq = SCM_CDR(ast);
    seqo = rc2_optimize_seq(seq, rest);
    if (SCM_EQ(seq, seqo)) return ast;
    else {
        if (SCM_EQ(type, SCM_SYM_REP_BOUND)) {
            return Scm_Cons(type, Scm_Cons(SCM_CADR(ast), seqo));
        } else {
            return Scm_Cons(type, seqo);
        }
    }
}

static int is_distinct(ScmObj x, ScmObj y)
{
    ScmObj carx;
    if (SCM_PAIRP(x)) {
        carx = SCM_CAR(x);
        if (SCM_EQ(carx, SCM_SYM_COMP)) {
            SCM_ASSERT(SCM_CHARSETP(SCM_CDR(x)));
            if (SCM_CHARP(y) || SCM_CHARSETP(y)) {
                return !is_distinct(SCM_CDR(x), y);
            }
            return FALSE;
        }
        if (SCM_INTP(carx)
            || SCM_EQ(carx, SCM_SYM_SEQ_UNCASE)
            || SCM_EQ(carx, SCM_SYM_SEQ_CASE)) {
            if (SCM_PAIRP(SCM_CDR(x))) {
                return is_distinct(SCM_CADR(x), y);
            }
        }
        return FALSE;
    }
    if (SCM_CHARP(x)) {
        if (SCM_CHARP(y)) return !SCM_EQ(x, y);
        return is_distinct(y, x);
    }
    if (SCM_CHARSETP(x)) {
        if (SCM_CHARP(y)) {
            return !Scm_CharSetContains(SCM_CHARSET(x), SCM_CHAR_VALUE(y));
        }
        if (SCM_CHARSETP(y)) {
            ScmObj ccs = Scm_CopyCharSet(SCM_CHARSET(y));
            ccs = Scm_CharSetComplement(SCM_CHARSET(ccs));
            return Scm_CharSetLE(SCM_CHARSET(x), SCM_CHARSET(ccs));
        }
        return is_distinct(y, x);
    }
    return FALSE;
}

ScmObj Scm_RegOptimizeAST(ScmObj ast)
{
    return rc2_optimize(ast, SCM_NIL);
}

/*-------------------------------------------------------------
 * pass 3 - code generation
 *          This pass actually called twice; the first run counts
 *          the size of the bytecode, and the second run fills
 *          the bytecode.   EMITP == FALSE for the first, EMITP == TRUE
 *          for the second.
 *          LASTP indicates this call is dealing with the last part of
 *          the compiled tree, thus need to deal with EOL marker.
 */

static void rc3_rec(regcomp_ctx *ctx, ScmObj ast, int lastp, int toplevelp);

/* Util function for pass2, to get an index of the charset vector
 * for the given charset.
 */
static int rc3_charset_index(ScmRegexp *rx, ScmObj cs)
{
    int i;
    for (i=0; i<rx->numSets; i++)
        if (cs == SCM_OBJ(rx->sets[i])) return i;
    Scm_Panic("rc3_charset_index: can't be here");
    return 0;                   /* dummy */
}

static void rc3_emit(regcomp_ctx *ctx, char code)
{
    if (ctx->emitp) {
        SCM_ASSERT(ctx->codep < ctx->codemax);
        ctx->code[ctx->codep++] = code;
    } else {
        ctx->codemax++;
    }
}

static void rc3_emit_offset(regcomp_ctx *ctx, int offset)
{
    if (offset > REGEXP_OFFSET_MAX) {
        Scm_Error("regexp too large.  consider splitting it up: %50.1S",
                  SCM_OBJ(ctx->rx));
    }
    
    if (ctx->emitp) {
        SCM_ASSERT(ctx->codep < ctx->codemax-1);
        ctx->code[ctx->codep++] = (offset>>8) & 0xff;
        ctx->code[ctx->codep++] = offset & 0xff;
    } else {
        ctx->codemax+=2;
    }
}

static void rc3_fill_offset(regcomp_ctx *ctx, int codep, int offset)
{
    if (offset > REGEXP_OFFSET_MAX) {
        Scm_Error("regexp too large.  consider splitting it up: %50.1S",
                  SCM_OBJ(ctx->rx));
    }

    if (ctx->emitp) {
        SCM_ASSERT(codep < ctx->codemax-1);
        ctx->code[codep] = (offset >> 8) & 0xff;
        ctx->code[codep+1] = offset & 0xff;
    }
}

static void rc3_seq(regcomp_ctx *ctx, ScmObj seq, int lastp, int toplevelp)
{
    ScmObj cp, item;
    
    SCM_FOR_EACH(cp, seq) {
        item = SCM_CAR(cp);

        /* concatenate literal character sequence */
        if (SCM_CHARP(item)) {
            int nrun = 0, ocodep = ctx->codep, nb, i;
            ScmChar ch;
            char chbuf[SCM_CHAR_MAX_BYTES];
            
            rc3_emit(ctx, (ctx->casefoldp? RE_MATCH_CI:RE_MATCH));
            rc3_emit(ctx, 0); /* patched later */
            do {
                ch = SCM_CHAR_VALUE(item);
                nb = SCM_CHAR_NBYTES(ch);
                SCM_CHAR_PUT(chbuf, ch);
                for (i=0; i<nb; i++) rc3_emit(ctx, chbuf[i]);
                nrun += nb;
                cp = SCM_CDR(cp);
                if (SCM_NULLP(cp)) break;
                item = SCM_CAR(cp);
            } while (SCM_CHARP(item) && nrun < CHAR_MAX);
            if (ctx->emitp) {
                /* patches the run length.  if we are matching to a
                   single byte char, use MATCH1 insn. */
                if (nrun == 1) {
                    ctx->code[ocodep] =
                        ctx->casefoldp?RE_MATCH1_CI:RE_MATCH1;
                    ctx->code[ocodep+1] = ctx->code[ocodep+2];
                    ctx->codep = ocodep+2;
                } else {
                    ctx->code[ocodep+1] = (char)nrun;
                }
            }
            if (SCM_NULLP(cp)) break;
            cp = Scm_Cons(item, cp); /* pushback */
        } else {
            rc3_rec(ctx, item, lastp&&SCM_NULLP(SCM_CDR(cp)), toplevelp);
        }
    }
}

static void rc3_rec(regcomp_ctx *ctx, ScmObj ast, int lastp, int toplevelp)
{
    ScmObj type;
    ScmRegexp *rx = ctx->rx;

    /* first, deal with atoms */
    if (!SCM_PAIRP(ast)) {
        /* a char */
        if (SCM_CHARP(ast)) {
            char chbuf[SCM_CHAR_MAX_BYTES];
            ScmChar ch = SCM_CHAR_VALUE(ast);
            int i, nb = SCM_CHAR_NBYTES(ch);
            SCM_CHAR_PUT(chbuf, ch);
            if (nb == 1) {
                rc3_emit(ctx, (ctx->casefoldp? RE_MATCH1_CI:RE_MATCH1));
                rc3_emit(ctx, chbuf[0]);
            } else {
                rc3_emit(ctx, (ctx->casefoldp? RE_MATCH_CI:RE_MATCH));
                rc3_emit(ctx, nb);
                for (i=0; i<nb; i++) rc3_emit(ctx, chbuf[i]);
            }
            return;
        }
        /* charset */
        if (SCM_CHARSETP(ast)) {
            if (SCM_CHARSET_SMALLP(ast)) {
                rc3_emit(ctx, RE_SET1);
            } else {
                rc3_emit(ctx, RE_SET);
            }
            rc3_emit(ctx, rc3_charset_index(rx, ast));
            return;
        }
        /* special stuff */
        if (SCM_SYMBOLP(ast)) {
            if (SCM_EQ(ast, SCM_SYM_ANY)) {
                rc3_emit(ctx, RE_ANY);
                return;
            }
            if (SCM_EQ(ast, SCM_SYM_BOL)) {
                rc3_emit(ctx, RE_BOL);
                return;
            }
            if (SCM_EQ(ast, SCM_SYM_EOL)) {
                if (lastp) {
                    rc3_emit(ctx, RE_EOL);
                } else {
                    rc3_emit(ctx, RE_MATCH1);
                    rc3_emit(ctx, '$');
                }
                return;
            }
            if (SCM_EQ(ast, SCM_SYM_WB)) {
                rc3_emit(ctx, RE_WB);
                return;
            }
            if (SCM_EQ(ast, SCM_SYM_NWB)) {
                rc3_emit(ctx, RE_NWB);
                return;
            }
            /* fallback */
        }
        Scm_Error("internal error in regexp compilation: unrecognized AST item: %S", ast);
    }

    /* now we have a structured node */
    type = SCM_CAR(ast);
    if (SCM_EQ(type, SCM_SYM_COMP)) {
        ScmObj cs = SCM_CDR(ast);
        SCM_ASSERT(SCM_CHARSETP(cs));
        if (SCM_CHARSET_SMALLP(cs)) {
            rc3_emit(ctx, RE_NSET1);
        } else {
            rc3_emit(ctx, RE_NSET);
        }
        rc3_emit(ctx, rc3_charset_index(rx, cs));
        return;
    }
    if (SCM_EQ(type, SCM_SYM_SEQ)) {
        rc3_seq(ctx, SCM_CDR(ast), lastp, toplevelp);
        return;
    }
    if (SCM_INTP(type)) {
        int grpno = SCM_INT_VALUE(type);
        rc3_emit(ctx, RE_BEGIN);
        rc3_emit(ctx, grpno);
        rc3_seq(ctx, SCM_CDR(ast), lastp, toplevelp);
        rc3_emit(ctx, RE_END);
        rc3_emit(ctx, grpno);
        return;
    }
    if (SCM_EQ(type, SCM_SYM_SEQ_UNCASE) || SCM_EQ(type, SCM_SYM_SEQ_CASE)) {
        int oldcase = ctx->casefoldp;
        ctx->casefoldp = SCM_EQ(type, SCM_SYM_SEQ_UNCASE);
        rc3_seq(ctx, SCM_CDR(ast), lastp, toplevelp);
        ctx->casefoldp = oldcase;
        return;
    }
    if (SCM_EQ(type, SCM_SYM_REP_WHILE)) {
        /* here we have an opportunity to generate an optimized code. */
        if (SCM_PAIRP(SCM_CDR(ast)) && SCM_NULLP(SCM_CDDR(ast))) {
            ScmObj elem = SCM_CADR(ast);
            if (SCM_CHARSETP(elem)) {
                rc3_emit(ctx, SCM_CHARSET_SMALLP(elem)?RE_SET1R:RE_SETR);
                rc3_emit(ctx, rc3_charset_index(rx, elem));
                return;
            }
            if (SCM_PAIRP(elem)&&SCM_EQ(SCM_CAR(elem), SCM_SYM_COMP)) {
                elem = SCM_CDR(elem);
                SCM_ASSERT(SCM_CHARSETP(elem));
                rc3_emit(ctx, SCM_CHARSET_SMALLP(elem)?RE_NSET1R:RE_NSETR);
                rc3_emit(ctx, rc3_charset_index(rx, elem));
                return;
            }
        }
        /* fallthrough to rep */
        type = SCM_SYM_REP;
    }
    if (SCM_EQ(type, SCM_SYM_ASSERT) || SCM_EQ(type, SCM_SYM_NASSERT)) {
        int ocodep = ctx->codep;
        rc3_emit(ctx, SCM_EQ(type, SCM_SYM_ASSERT) ? RE_ASSERT : RE_NASSERT);
        rc3_emit_offset(ctx, 0); /* will be patched */
        rc3_seq(ctx, SCM_CDR(ast), lastp, toplevelp);
        rc3_emit(ctx, RE_SUCCESS);
        rc3_fill_offset(ctx, ocodep+1, ctx->codep);
        return;
    }
    if (SCM_EQ(type, SCM_SYM_REP)) {
        /* rep: TRY next
                <seq>
                JUMP rep
           next:
        */
        int ocodep = ctx->codep;
        rc3_emit(ctx, RE_TRY);
        rc3_emit_offset(ctx, 0); /* will be patched */
        rc3_seq(ctx, SCM_CDR(ast), FALSE, FALSE);
        rc3_emit(ctx, RE_JUMP);
        rc3_emit_offset(ctx, ocodep);
        rc3_fill_offset(ctx, ocodep+1, ctx->codep);
        return;
    }
    if (SCM_EQ(type, SCM_SYM_REP_MIN)) {
        /* non-greedy repeat
           rep: TRY seq
                JUMP next
           seq: <seq>
                JUMP rep
           next:
        */
        int ocodep1 = ctx->codep, ocodep2;
        rc3_emit(ctx, RE_TRY);
        rc3_emit_offset(ctx, 0); /* will be patched */
        ocodep2 = ctx->codep;
        rc3_emit(ctx, RE_JUMP);
        rc3_emit_offset(ctx, 0); /* will be patched */
        rc3_fill_offset(ctx, ocodep1+1, ctx->codep);
        rc3_seq(ctx, SCM_CDR(ast), FALSE, FALSE);
        rc3_emit(ctx, RE_JUMP);
        rc3_emit_offset(ctx, ocodep1);
        rc3_fill_offset(ctx, ocodep2+1, ctx->codep);
        return;
    }
    if (SCM_EQ(type, SCM_SYM_ALT)) {
        /*     TRY #1
               <alt0>
               JUMP next
           #1: TRY #2
               <alt1>
               JUMP next
                :
                :
               TRY next
               <altN>
           next:
        */
        ScmObj clause;
        ScmObj jumps = SCM_NIL;
        int patchp;

        for (clause = SCM_CDR(ast);
             SCM_PAIRP(SCM_CDR(clause));
             clause = SCM_CDR(clause)) {
            rc3_emit(ctx, RE_TRY);
            patchp = ctx->codep;
            rc3_emit_offset(ctx, 0); /* will be patched */
            rc3_rec(ctx, SCM_CAR(clause), lastp, FALSE);
            rc3_emit(ctx, RE_JUMP);
            if (ctx->emitp) {
                jumps = Scm_Cons(SCM_MAKE_INT(ctx->codep), jumps);
            }
            rc3_emit_offset(ctx, 0); /* will be patched */
            rc3_fill_offset(ctx, patchp, ctx->codep);
        }
        rc3_rec(ctx, SCM_CAR(clause), lastp, FALSE);
        if (ctx->emitp) {
            SCM_FOR_EACH(jumps, jumps) {
                patchp = SCM_INT_VALUE(SCM_CAR(jumps));
                rc3_fill_offset(ctx, patchp, ctx->codep);
            }
        }
        return;
    }
    if (SCM_EQ(type, SCM_SYM_REP_BOUND) || SCM_EQ(type, SCM_SYM_REP_BOUND_MIN)) {
        /* (rep-bound <n> . <x>)

               TRY  #01
               JUMP #11
           #01:TRY  #02
               JUMP #12
                :
           #0n:JUMP #1N
           #11:<X>
           #12:<X>
                :
           #1n:<X>
           #1N:

           (rep-bound-min <n> . <x>)
           
               TRY  #01
               JUMP #1N
           #01:TRY  #02
               JUMP #1n
                :
           #0n TRY  #11
               JUMP #12
           #11:<X>
           #12:<X>
                :
           #1n:<X>
           #1N:

         */
        ScmObj item, jlist = SCM_NIL;
        int count, n, j0 = 0, jn;
        int greedy = SCM_EQ(type, SCM_SYM_REP_BOUND);

        SCM_ASSERT(Scm_Length(ast) == 3 && SCM_INTP(SCM_CADR(ast)));
        count = SCM_INT_VALUE(SCM_CADR(ast));
        SCM_ASSERT(count > 0);
        item = SCM_CDDR(ast);
        /* first part - TRYs and JUMPs
           j0 is used to patch the label #0k
           the destination of jumps to be patched are linked to jlist */
        for (n=0; n<count; n++) {
            if (n>0) rc3_fill_offset(ctx, j0, ctx->codep);
            rc3_emit(ctx, RE_TRY);
            if (ctx->emitp) j0 = ctx->codep;
            rc3_emit_offset(ctx, 0); /* to be patched */
            rc3_emit(ctx, RE_JUMP);
            if (ctx->emitp) {
                jlist = Scm_Cons(SCM_MAKE_INT(ctx->codep), jlist);
            }
            rc3_emit_offset(ctx, 0); /* to be patched */
        }
        rc3_fill_offset(ctx, j0, ctx->codep); /* patch #0n */
        /* finishing the first part.
           for non-greedy match, we need one more TRY. */
        if (greedy) {
            rc3_emit(ctx, RE_JUMP);
            jn = ctx->codep;
            rc3_emit_offset(ctx, 0); /* to be patched */
        } else {
            rc3_emit(ctx, RE_TRY);
            jn = ctx->codep;
            rc3_emit_offset(ctx, 0);  /* to be patched */
            rc3_emit(ctx, RE_JUMP);
            if (ctx->emitp) {
                jlist = Scm_Cons(SCM_MAKE_INT(ctx->codep), jlist);
            }
            rc3_emit_offset(ctx, 0);  /* to be patched */
            rc3_fill_offset(ctx, jn, ctx->codep);
        }
        if (ctx->emitp && greedy) jlist = Scm_ReverseX(jlist);
        if (!greedy) rc3_seq(ctx, item, FALSE, toplevelp);
        for (n=0; n<count; n++) {
            if (ctx->emitp) {
                rc3_fill_offset(ctx, SCM_INT_VALUE(SCM_CAR(jlist)),
                                ctx->codep);
            }
            rc3_seq(ctx, item, FALSE, toplevelp);
            if (ctx->emitp) jlist = SCM_CDR(jlist);
        }
        if (greedy) {
            /* the last JUMP to #1N */
            rc3_fill_offset(ctx, jn, ctx->codep);
        } else {
            /* the first JUMP to #1N */
            if (ctx->emitp) {
                SCM_ASSERT(SCM_PAIRP(jlist));
                rc3_fill_offset(ctx, SCM_INT_VALUE(SCM_CAR(jlist)), ctx->codep);
            }
        }
        return;
    }
    Scm_Error("internal error in regexp compilation: bad node: %S", ast);
}

static int is_bol_anchored(ScmObj ast)
{
    ScmObj type;
    if (!SCM_PAIRP(ast)) {
        if (SCM_EQ(ast, SCM_SYM_BOL)) return TRUE;
        else return FALSE;
    }
    type = SCM_CAR(ast);
    if (SCM_INTP(type) || SCM_EQ(type, SCM_SYM_SEQ)
        || SCM_EQ(type, SCM_SYM_SEQ_UNCASE) || SCM_EQ(type, SCM_SYM_SEQ_CASE)) {
        if (!SCM_PAIRP(SCM_CDR(ast))) return FALSE;
        return is_bol_anchored(SCM_CADR(ast));
    }
    if (SCM_EQ(type, SCM_SYM_ALT)) {
        ScmObj ap;
        SCM_FOR_EACH(ap, SCM_CDR(ast)) { 
            if (!is_bol_anchored(SCM_CAR(ap))) return FALSE;
        }
        return TRUE;
    }
    return FALSE;
}

/* pass 3 */
static ScmObj rc3(regcomp_ctx *ctx, ScmObj ast)
{
    /* check if ast is bol-anchored */
    if (is_bol_anchored(ast)) ctx->rx->flags |= SCM_REGEXP_BOL_ANCHORED;

    /* pass 3-1 : count # of insns */
    ctx->codemax = 1;
    ctx->emitp = FALSE;
    rc3_rec(ctx, ast, TRUE, TRUE);
    
    /* pass 3-2 : code generation */
    ctx->code = SCM_NEW_ATOMIC2(unsigned char *, ctx->codemax);
    ctx->emitp = TRUE;
    rc3_rec(ctx, ast, TRUE, TRUE);
    rc3_emit(ctx, RE_SUCCESS);
    ctx->rx->code = ctx->code;
    ctx->rx->numCodes = ctx->codep;
    return SCM_OBJ(ctx->rx);
}

/* For debug */
#if SCM_DEBUG_HELPER
void Scm_RegDump(ScmRegexp *rx)
{
    int end = rx->numCodes, codep;

    Scm_Printf(SCM_CUROUT, "Regexp %p: (flags=%08x)\n", rx, rx->flags);
    Scm_Printf(SCM_CUROUT, "  must = ");
    if (rx->mustMatch) {
        Scm_Printf(SCM_CUROUT, "%S\n", rx->mustMatch);
    } else {
        Scm_Printf(SCM_CUROUT, "(none)\n");
    }

    for (codep = 0; codep < end; codep++) {
        switch (rx->code[codep]) {
        case RE_MATCH1:;
        case RE_MATCH1_CI:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  %s  0x%02x  '%c'\n",
                       codep-1,
                       (rx->code[codep-1]==RE_MATCH1? "MATCH1":"MATCH1_CI"),
                       rx->code[codep], rx->code[codep]);
            continue;
        case RE_MATCH:;
        case RE_MATCH_CI:
            codep++;
            {
                u_int numchars = (u_int)rx->code[codep];
                int i;
                Scm_Printf(SCM_CUROUT, "%4d  %s(%3d) '",
                           codep-1,
                           (rx->code[codep-1]==RE_MATCH? "MATCH":"MATCH_CI"),
                           numchars);
                for (i=0; i< numchars; i++)
                    Scm_Printf(SCM_CUROUT, "%c", rx->code[++codep]);
                Scm_Printf(SCM_CUROUT, "'\n");
            }
            continue;
        case RE_ANY:
            Scm_Printf(SCM_CUROUT, "%4d  ANY\n", codep);
            continue;
        case RE_TRY:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  TRY  %d\n", codep-1,
                       (rx->code[codep])*256 + rx->code[codep+1]);
            codep++;
            continue;
        case RE_SET:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  SET  %d    %S\n",
                       codep-1, rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_NSET:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  NSET %d    %S\n",
                       codep-1, rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_SET1:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  SET1 %d    %S\n",
                       codep-1, rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_NSET1:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  NSET1 %d    %S\n",
                       codep-1, rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_JUMP:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  JUMP %d\n", codep-1,
                       (rx->code[codep])*256 + rx->code[codep+1]);
            codep++;
            continue;
        case RE_FAIL:
            Scm_Printf(SCM_CUROUT, "%4d  FAIL\n", codep);
            continue;
        case RE_SUCCESS:
            Scm_Printf(SCM_CUROUT, "%4d  SUCCESS\n", codep);
            continue;
        case RE_BEGIN:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  BEGIN %d\n", codep-1, rx->code[codep]);
            continue;
        case RE_END:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  END %d\n", codep-1, rx->code[codep]);
            continue;
        case RE_BOL:
            Scm_Printf(SCM_CUROUT, "%4d  BOL\n", codep);
            continue;
        case RE_EOL:
            Scm_Printf(SCM_CUROUT, "%4d  EOL\n", codep);
            continue;
        case RE_WB:
            Scm_Printf(SCM_CUROUT, "%4d  WB\n", codep);
            continue;
        case RE_NWB:
            Scm_Printf(SCM_CUROUT, "%4d  NWB\n", codep);
            continue;
        case RE_MATCH1B:
            Scm_Printf(SCM_CUROUT, "%4d  MATCH1B %02x '%c', %d\n",
                       codep, rx->code[codep+1], rx->code[codep+1],
                       rx->code[codep+2]*256+rx->code[codep+3]);
            codep += 3;
            continue;
        case RE_SET1R:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  SET1R %d   %S\n",
                       codep-1, rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_NSET1R:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  NSET1R %d  %S\n",
                       codep-1, rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_SETR:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  SETR %d    %S\n",
                       codep-1, rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_NSETR:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  NSETR %d   %S\n",
                       codep-1, rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_ASSERT:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  ASSERT %d\n", codep-1,
                       (rx->code[codep])*256 + rx->code[codep+1]);
            codep++;
            continue;            
        case RE_NASSERT:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  NASSERT %d\n", codep-1,
                       (rx->code[codep])*256 + rx->code[codep+1]);
            codep++;
            continue;            
        default:
            Scm_Error("regexp screwed up\n");
        }
    }
}
#endif /* SCM_DEBUG_HELPER */

/* Helper routine to be used for compilation from AST.
   Traverses AST to reorder groups and collect charsets.
   Note that the native regcomp path doesn't use these fns.
   Only the AST provided from outside is processed. */
static ScmObj rc_setup_context_seq(regcomp_ctx *ctx, ScmObj seq);

static ScmObj rc_setup_context(regcomp_ctx *ctx, ScmObj ast)
{
    ScmObj type, rest;
    if (!SCM_PAIRP(ast)) {
        if (SCM_CHARP(ast)) return ast;
        if (SCM_CHARSETP(ast)) {
            rc_register_charset(ctx, SCM_CHARSET(ast));
            return ast;
        }
        if (SCM_EQ(ast, SCM_SYM_BOL) || SCM_EQ(ast, SCM_SYM_EOL)
            || SCM_EQ(ast, SCM_SYM_ANY)) {
            return ast;
        }
        goto badast;
    }
    type = SCM_CAR(ast);
    if (SCM_INTP(type)) {
        int grpno = ctx->grpcount++;
        rest = rc_setup_context_seq(ctx, SCM_CDR(ast));
        if (SCM_INT_VALUE(type) == grpno && SCM_EQ(SCM_CDR(ast), rest)) {
            return ast;
        } else {
            return Scm_Cons(SCM_MAKE_INT(grpno), rest);
        }
    }
    if (SCM_EQ(type, SCM_SYM_COMP)) {
        if (!SCM_CHARSETP(SCM_CDR(ast))) goto badast;
        rc_register_charset(ctx, SCM_CHARSET(SCM_CDR(ast)));
        return ast;
    }
    if (SCM_EQ(type, SCM_SYM_SEQ) || SCM_EQ(type, SCM_SYM_ALT)
        || SCM_EQ(type, SCM_SYM_SEQ_UNCASE) || SCM_EQ(type, SCM_SYM_SEQ_CASE)
        || SCM_EQ(type, SCM_SYM_REP) || SCM_EQ(type, SCM_SYM_REP_WHILE)) {
        rest = rc_setup_context_seq(ctx, SCM_CDR(ast));
        if (SCM_EQ(SCM_CDR(ast), rest)) return ast;
        else return Scm_Cons(type, rest);
    }
    if (SCM_EQ(type, SCM_SYM_REP_BOUND)) {
        if (!SCM_PAIRP(SCM_CDR(ast)) || !SCM_INTP(SCM_CADR(ast))
            || SCM_INT_VALUE(SCM_CADR(ast)) < 0) {
            goto badast;
        }
        rest = rc_setup_context_seq(ctx, SCM_CDDR(ast));
        if (SCM_EQ(SCM_CDDR(ast), rest)) return ast;
        else return Scm_Cons(type, Scm_Cons(SCM_CADR(ast), rest));
    }
  badast:
    Scm_Error("invalid regexp AST: %S", ast);
    return SCM_UNDEFINED;       /* dummy */
}

static ScmObj rc_setup_context_seq(regcomp_ctx *ctx, ScmObj seq) 
{
    ScmObj sp, sp2, obj, head = SCM_NIL, tail = SCM_NIL;
    SCM_FOR_EACH(sp, seq) {
        obj = rc_setup_context(ctx, SCM_CAR(sp));
        if (!SCM_EQ(obj, SCM_CAR(sp))) break;
    }
    if (SCM_NULLP(sp)) return seq;
    /* we need to copy the spine */
    SCM_FOR_EACH(sp2, seq) {
        if (SCM_EQ(sp2, sp)) break;
        SCM_APPEND1(head, tail, SCM_CAR(sp2));
    }
    SCM_FOR_EACH(sp2, sp2) {
        SCM_APPEND1(head, tail, rc_setup_context(ctx, SCM_CAR(sp2)));
    }
    return head;
}

/*--------------------------------------------------------------
 * Compiler entry point
 */
ScmObj Scm_RegComp(ScmString *pattern, int flags)
{
    ScmRegexp *rx = make_regexp();
    ScmObj ast;
    regcomp_ctx cctx;
    
    if (SCM_STRING_INCOMPLETE_P(pattern)) {
        Scm_Error("incomplete string is not allowed: %S", pattern);
    }
    rx->pattern = SCM_STRING(Scm_MakeString(SCM_STRING_START(pattern),
                                            SCM_STRING_SIZE(pattern),
                                            SCM_STRING_LENGTH(pattern),
                                            SCM_MAKSTR_IMMUTABLE));
    rc_ctx_init(&cctx, rx);
    cctx.casefoldp = flags & SCM_REGEXP_CASE_FOLD;
    rx->flags |= (flags & SCM_REGEXP_CASE_FOLD);

    /* pass 1 : parse regexp spec */
    ast = rc1(&cctx);
    rc_setup_charsets(rx, &cctx);
    if (flags & SCM_REGEXP_PARSE_ONLY) return ast;

    /* pass 2 : optimization */
    ast = rc2_optimize(ast, SCM_NIL);

    /* pass 3 : generate bytecode */
    return rc3(&cctx, ast);
}

/* alternative entry that compiles from AST */
ScmObj Scm_RegCompFromAST(ScmObj ast)
{
    ScmRegexp *rx = make_regexp();
    regcomp_ctx cctx;
    rc_ctx_init(&cctx, rx);

    /* prepare some context */
    if (!SCM_PAIRP(ast) || !SCM_INTP(SCM_CAR(ast))) {
        /* ensure the entire AST is in a group #0 */
        ast = SCM_LIST2(SCM_MAKE_INT(0), ast);
    }
    ast = rc_setup_context(&cctx, ast);
    rc_setup_charsets(rx, &cctx);
    rx->numGroups = cctx.grpcount+1;
    
    /* pass 3 */
    return rc3(&cctx, ast);
}

/*=======================================================================
 * Matcher
 */

/* For now, I use C-stack directly to keep information for backtrack,
 * i.e. anytime I should try something I recursively call rex_rec().
 * It may run out the stack space if regexp requires deep recursion.
 *
 * Rex_rec doesn't return as long as match succeeds.  At the end of
 * code, it longjmp's to the start of matcher.
 *
 * My preliminary test showed that using C-stack & longjmp is faster than
 * allocating and maintaining the stack by myself.   Further test is required
 * for practical case, though.
 */

struct match_list {
    struct match_list *next;
    int grpnum;
    const char *ptr;
};

struct match_ctx {
    ScmRegexp *rx;
    const unsigned char *codehead; /* start of code */
    const char *input;          /* start of input */
    const char *stop;           /* end of input */
    const char *last;
    struct match_list *matches;
    void *begin_stack;          /* C stack pointer the match began from. */
    sigjmp_buf *cont;
};

#define MAX_STACK_USAGE   0x100000

static struct match_list *push_match(struct match_list *mlist,
                                            int grpnum, const char *ptr)
{
    struct match_list *elt = SCM_NEW(struct match_list);
    elt->next = mlist;
    elt->grpnum = grpnum;
    elt->ptr = ptr;
    return elt;
}

static int match_ci(const char **input, const unsigned char **code, int length)
{
    unsigned char inch, c;
    int csize, i;
    do {
        inch = *(*input)++;
        c = *(*code)++;
        if ((csize = SCM_CHAR_NFOLLOWS(inch)) == 0) {
            if (c != SCM_CHAR_DOWNCASE(inch)) return FALSE;
        } else {
            if (c != inch) return FALSE;
            for (i=0; i<csize; i++) {
                if ((unsigned char)*(*code)++ != (unsigned char)*(*input)++)
                    return FALSE;
            }
        }
        length -= (csize+1);
    } while (length > 0);
    return TRUE;
}

/* Check if input points to the word boundary.  For now, I consider
   all multibyte chars word-constituent. */
static int is_word_constituent(unsigned char b)
{
    if (b >= 128) return TRUE;
    if (b >= '0' && b <= '9') return TRUE;
    if (b >= 'A' && b <= 'Z') return TRUE;
    if (b >= 'a' && b <= 'z') return TRUE;
    return FALSE;
}

static int is_word_boundary(struct match_ctx *ctx, const char *input)
{
    unsigned char nextb, prevb;
    const char *prevp;
    
    if (input == ctx->input || input == ctx->stop) return TRUE;
    nextb = (unsigned char)*input;
    SCM_CHAR_BACKWARD(input, ctx->input, prevp);
    SCM_ASSERT(prevp != NULL);
    prevb = (unsigned char)*prevp;
    if ((is_word_constituent(nextb) && !is_word_constituent(prevb))
        || (!is_word_constituent(nextb) && is_word_constituent(prevb))) {
        return TRUE;
    }
    return FALSE;
}

static void rex_rec(const unsigned char *code,
                    const char *input,
                    struct match_ctx *ctx,                 
                    struct match_list *mlist)
{
    register int param;
    register ScmChar ch;
    ScmCharSet *cset;

    /* TODO: here we assume C-stack grows downward; need to check by
       configure */
    if ((char*)&cset < (char*)ctx->begin_stack - MAX_STACK_USAGE) {
        Scm_Error("stack overrun during matching regexp %S", ctx->rx);
    }
    
    for (;;) {
        switch(*code++) {
        case RE_MATCH:
            param = *code++;
            if (ctx->stop - input < param) return;
            while (param-- > 0) {
                if (*code++ != (unsigned char)*input++) return;
            }
            continue;
        case RE_MATCH1:
            if (ctx->stop == input) return;
            if (*code++ != (unsigned char)*input++) return;
            continue;
        case RE_MATCH_CI:
            param = *code++;
            if (ctx->stop - input < param) return;
            if (!match_ci(&input, &code, param)) return;
            continue;
        case RE_MATCH1_CI:
            if (ctx->stop == input) return;
            param  = (unsigned char)*input++;
            if (SCM_CHAR_NFOLLOWS(param)!=0
                || (*code++)!=SCM_CHAR_DOWNCASE(param)) {
                return;
            }
            continue;
        case RE_ANY:
            if (ctx->stop == input) return;
            input += SCM_CHAR_NFOLLOWS(*input) + 1;
            continue;
        case RE_TRY:
            rex_rec(code+2, input, ctx, mlist);
            code = ctx->codehead + code[0]*256 + code[1];
            continue;
        case RE_JUMP:
            code = ctx->codehead + code[0]*256 + code[1];
            continue;
        case RE_SET1:
            if (ctx->stop == input) return;
            if ((unsigned char)*input >= 128) return;
            if (!Scm_CharSetContains(ctx->rx->sets[*code++], *input)) return;
            input++;
            continue;
        case RE_NSET1:
            if (ctx->stop == input) return;
            if ((unsigned char)*input < 128) {
                if (Scm_CharSetContains(ctx->rx->sets[*code++], *input))
                    return;
                input++;
            } else {
                code++;
                input += SCM_CHAR_NFOLLOWS((unsigned char)*input) + 1;
            }
            continue;
        case RE_SET:
            if (ctx->stop == input) return;
            SCM_CHAR_GET(input, ch);
            cset = ctx->rx->sets[*code++];
            if (!Scm_CharSetContains(cset, ch)) return;
            input += SCM_CHAR_NBYTES(ch);
            continue;
        case RE_NSET:
            if (ctx->stop == input) return;
            SCM_CHAR_GET(input, ch);
            cset = ctx->rx->sets[*code++];
            if (Scm_CharSetContains(cset, ch)) return;
            input += SCM_CHAR_NBYTES(ch);
            continue;
        case RE_BEGIN:
            mlist = push_match(mlist, *code++, input);
            continue;
        case RE_END:
            mlist = push_match(mlist, -(*code++), input);
            continue;
        case RE_BOL:
            if (input != ctx->input) return;
            continue;
        case RE_EOL:
            if (input != ctx->stop) return;
            continue;
        case RE_WB:
            if (!is_word_boundary(ctx, input)) return;
            continue;
        case RE_NWB:
            if (is_word_boundary(ctx, input)) return;
            continue;
        case RE_SUCCESS:
            ctx->last = input;
            ctx->matches = mlist;
            siglongjmp(*ctx->cont, 1);
            /*NOTREACHED*/
        case RE_FAIL:
            return;
        case RE_SET1R:
            cset = ctx->rx->sets[*code++];
            for (;;) {
                if (ctx->stop <= input) break;
                if ((unsigned char)*input >= 128) break;
                if (!Scm_CharSetContains(cset, *input)) break;
                input++;
            }
            continue;
        case RE_NSET1R:
            cset = ctx->rx->sets[*code++];
            for (;;) {
                if (ctx->stop <= input) break;
                if ((unsigned char)*input < 128 ) {
                    if (Scm_CharSetContains(cset, *input)) break;
                    input++;
                } else {
                    input+=SCM_CHAR_NFOLLOWS(*input)+1;
                }
            }
            continue;
        case RE_SETR:
            cset = ctx->rx->sets[*code++];
            for (;;) {
                if (ctx->stop <= input) break;
                SCM_CHAR_GET(input, ch);
                if (!Scm_CharSetContains(cset, ch)) break;
                input += SCM_CHAR_NBYTES(ch);
            }
            continue;
        case RE_NSETR:
            cset = ctx->rx->sets[*code++];
            for (;;) {
                if (ctx->stop <= input) break;
                SCM_CHAR_GET(input, ch);
                if (Scm_CharSetContains(cset, ch)) break;
                input += SCM_CHAR_NBYTES(ch);
            }
            continue;
        case RE_ASSERT: {
            sigjmp_buf cont, *ocont = ctx->cont;
            ctx->cont = &cont;
            if (sigsetjmp(cont, FALSE) == 0) {
                rex_rec(code+2, input, ctx, mlist);
                ctx->cont = ocont;
                return;
            }
            code = ctx->codehead + code[0]*256 + code[1];
            ctx->cont = ocont;
            mlist = ctx->matches;
            continue;
        }
        case RE_NASSERT: {
            sigjmp_buf cont, *ocont = ctx->cont;
            ctx->cont = &cont;
            if (sigsetjmp(cont, FALSE) == 0) {
                rex_rec(code+2, input, ctx, mlist);
                code = ctx->codehead + code[0]*256 + code[1];
                ctx->cont = ocont;
                continue;
            }
            ctx->cont = ocont;
            return;
        }
        default:
            /* shouldn't be here */
            Scm_Error("regexp implementation seems broken\n");
        }
    }
}

static ScmObj make_match(ScmRegexp *rx, ScmString *orig,
                         struct match_ctx *ctx)
{
    int i;
    struct match_list *ml;
    ScmRegMatch *rm = SCM_NEW(ScmRegMatch);
    SCM_SET_CLASS(rm, SCM_CLASS_REGMATCH);
    rm->numMatches = rx->numGroups;
    rm->matches = SCM_NEW_ARRAY(struct ScmRegMatchSub, rx->numGroups);
    /* we keep information of original string separately, instead of
       keeping a pointer to orig; For orig may be destructively modified,
       but its elements are not. */
    rm->input = SCM_STRING_START(orig);
    rm->inputLen = SCM_STRING_LENGTH(orig);
    rm->inputSize = SCM_STRING_SIZE(orig);
    for (i=0; i<rx->numGroups; i++) {
        rm->matches[i].start = -1;
        rm->matches[i].length = -1;
        rm->matches[i].startp = NULL;
        rm->matches[i].endp = NULL;
    }

    rm->matches[0].endp = ctx->last;
    /* scan through match result */
    for (ml = ctx->matches; ml; ml = ml->next) {
        if (ml->grpnum >= 0) {
            rm->matches[ml->grpnum].startp = ml->ptr;
        } else {
            rm->matches[-ml->grpnum].endp = ml->ptr;
        }
    }

    /* sanity check (not necessary, but for now...) */
    for (i=0; i<rx->numGroups; i++) {
        if ((rm->matches[i].startp && !rm->matches[i].endp)
            || (!rm->matches[i].startp && rm->matches[i].endp)) {
            Scm_Panic("implementation error: discrepancy in regexp match #%d!", i);
        }
    }
    return SCM_OBJ(rm);
}

static ScmObj rex(ScmRegexp *rx, ScmString *orig,
                  const char *start, const char *end)
{
    struct match_ctx ctx;
    sigjmp_buf cont;
    ctx.rx = rx;
    ctx.codehead = rx->code;
    ctx.input = SCM_STRING_START(orig);
    ctx.stop = end;
    ctx.matches = NULL;
    ctx.begin_stack = (void*)&ctx;
    ctx.cont = &cont;

    if (sigsetjmp(cont, FALSE) == 0) {
        rex_rec(ctx.codehead, start, &ctx, NULL);
        return SCM_FALSE;
    } else {
        return make_match(rx, orig, &ctx);
    }
}

/*----------------------------------------------------------------------
 * entry point
 */
ScmObj Scm_RegExec(ScmRegexp *rx, ScmString *str)
{
    const char *start = SCM_STRING_START(str);
    const char *end = start + SCM_STRING_SIZE(str);
    int mustMatchLen = rx->mustMatch? SCM_STRING_SIZE(rx->mustMatch) : 0;

    if (SCM_STRING_INCOMPLETE_P(str)) {
        Scm_Error("incomplete string is not allowed: %S", str);
    }
#if 0
    /* Disabled for now; we need to use more heuristics to determine
       when we should apply mustMatch.  For example, if the regexp
       begins with BOL assertion and constant string, then it would be
       faster to go for rex directly. */
    if (rx->mustMatch) {
        /* Prescreening.  If the input string doesn't contain mustMatch
           string, it can't match the entire expression. */
        if (SCM_FALSEP(Scm_StringScan(str, rx->mustMatch,
                                      SCM_STRING_SCAN_INDEX))) {
            return SCM_FALSE;
        }
    }
#endif
    /* short cut : if rx matches only at the beginning of the string,
       we only run from the beginning of the string */
    if (rx->flags & SCM_REGEXP_BOL_ANCHORED) {
        return rex(rx, str, start, end);
    }
    /* normal matching */
    while (start <= end-mustMatchLen) {
        ScmObj r = rex(rx, str, start, end);
        if (!SCM_FALSEP(r)) return r;
        start += SCM_CHAR_NFOLLOWS(*start)+1;
    }
    return SCM_FALSE;
}

/*=======================================================================
 * Retrieving matches
 */

/* TODO: MT Warning: these retrival functions change match object's     
 * internal state.
 */
ScmObj Scm_RegMatchSubstr(ScmRegMatch *rm, int i)
{
    struct ScmRegMatchSub *sub;
    if (i < 0 || i >= rm->numMatches)
        Scm_Error("submatch index out of range: %d", i);
    sub = &rm->matches[i];
    if (sub->startp == NULL) {
        return SCM_FALSE;
    } else if (sub->length >= 0) {
        return Scm_MakeString(sub->startp, sub->endp - sub->startp,
                              sub->length, 0);
    } else {
        ScmObj s = Scm_MakeString(sub->startp, sub->endp - sub->startp, -1, 0);
        sub->length = SCM_STRING_LENGTH(s);
        return s;
    }
}

ScmObj Scm_RegMatchStart(ScmRegMatch *rm, int i)
{
    struct ScmRegMatchSub *sub;
    if (i < 0 || i >= rm->numMatches)
        Scm_Error("submatch index out of range: %d", i);
    sub = &rm->matches[i];
    if (sub->startp == NULL) {
        return SCM_FALSE;
    } else if (sub->start < 0) {
        sub->start = Scm_MBLen(rm->input, sub->startp);
    }
    return Scm_MakeInteger(sub->start);
}

ScmObj Scm_RegMatchEnd(ScmRegMatch *rm, int i)
{
    struct ScmRegMatchSub *sub;
    if (i < 0 || i >= rm->numMatches)
        Scm_Error("submatch index out of range: %d", i);
    sub = &rm->matches[i];
    if (sub->startp == NULL) {
        return SCM_FALSE;
    } else if (sub->start < 0) {
        sub->start = Scm_MBLen(rm->input, sub->startp);
    }
    if (sub->length < 0) {
        sub->length = Scm_MBLen(sub->startp, sub->endp);
    }
    return Scm_MakeInteger(sub->start + sub->length);
}

ScmObj Scm_RegMatchBefore(ScmRegMatch *rm, int i)
{
    struct ScmRegMatchSub *sub;
    if (i < 0 || i >= rm->numMatches)
        Scm_Error("submatch index out of range: %d", i);
    sub = &rm->matches[i];
    if (sub->startp == NULL) return SCM_FALSE;
    return Scm_MakeString(rm->input, sub->startp - rm->input, -1, 0);
}

ScmObj Scm_RegMatchAfter(ScmRegMatch *rm, int i)
{
    struct ScmRegMatchSub *sub;
    if (i < 0 || i >= rm->numMatches)
        Scm_Error("submatch index out of range: %d", i);
    sub = &rm->matches[i];
    if (sub->startp == NULL) return SCM_FALSE;
    return Scm_MakeString(sub->endp,
                          rm->input + rm->inputSize - sub->endp, -1, 0);
}

/* for debug */
#if SCM_DEBUG_HELPER
void Scm_RegMatchDump(ScmRegMatch *rm)
{
    int i;
    
    Scm_Printf(SCM_CUROUT, "RegMatch %p\n", rm);
    Scm_Printf(SCM_CUROUT, "  numMatches = %d\n", rm->numMatches);
    Scm_Printf(SCM_CUROUT, "  input = %S\n", rm->input);
    for (i=0; i<rm->numMatches; i++) {
        struct ScmRegMatchSub *sub = &rm->matches[i];
        if (sub->startp) {
            Scm_Printf(SCM_CUROUT, "[%3d-%3d]  %S\n",
                       sub->startp - rm->input,
                       sub->endp - rm->input,
                       Scm_MakeString(sub->startp, sub->endp-sub->startp,
                                      -1, 0));
        } else {
            Scm_Printf(SCM_CUROUT, "[---] #f\n");
        }
    }
}
#endif /*SCM_DEBUG_HELPER*/

/*=======================================================================
 * Initializing stuff
 */

void Scm__InitRegexp(void)
{
}
