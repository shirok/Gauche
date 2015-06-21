/*
 * regexp.c - regular expression
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
 *   Copyright (c) 2006 Rui Ueyama, All rights reserved.
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

#include <setjmp.h>
#include <ctype.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/regexp.h"
#include "gauche/class.h"
#include "gauche/priv/builtin-syms.h"

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
 * much as possible.  Actually, the conversion is done only when we see
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

/* Instructions.  `RL' suffix indicates that the instruction moves the
   current position pointer right to left.  These instructions are
   used within lookbehind assertion. */
enum {
    RE_MATCH1,                  /* followed by 1 byte to match */
    RE_MATCH1_RL,
    RE_MATCH,                   /* followed by length, and bytes to match */
    RE_MATCH_RL,
    RE_MATCH1_CI,               /* case insensitive match */
    RE_MATCH1_CI_RL,
    RE_MATCH_CI,                /* case insensitive match */
    RE_MATCH_CI_RL,
    RE_ANY,                     /* match any char */
    RE_ANY_RL,
    RE_TRY,                     /* followed by offset (2 bytes). try matching
                                   the following sequence, and if fails,
                                   jump to offset.  This handles backtracking.
                                */
    RE_SET,                     /* followed by charset #.  match any char in
                                   the charset. */
    RE_SET_RL,
    RE_NSET,                    /* followed by charset #.  match any char but
                                   in the charset */
    RE_NSET_RL,
    RE_SET1,                    /* followed by charset #.  match any char in
                                   the charset.  guaranteed that the charset
                                   holds only range 0-127 */
    RE_SET1_RL,
    RE_NSET1,                   /* followed by charset #.  match any char
                                   but the ones in the charset.  guaranteed
                                   that the charset holds only range 0-127. */
    RE_NSET1_RL,
    RE_JUMP,                    /* followed by offset (2 bytes).  jump to that
                                   bytecode. */
    RE_FAIL,                    /* fail */
    RE_SUCCESS,                 /* success */
    RE_BEGIN,                   /* followed by a group number.  start the
                                   group. */
    RE_BEGIN_RL,
    RE_END,                     /* followed by a group number.  end the
                                   group. */
    RE_END_RL,
    RE_BOL,                     /* beginning of line assertion */
    RE_EOL,                     /* end of line assertion */
    RE_WB,                      /* word boundary assertion */
    RE_NWB,                     /* negative word boundary assertion */
    RE_BACKREF,                 /* followed by group #. */
    RE_BACKREF_RL,
    RE_BACKREF_CI,              /* followed by group #. */
    RE_BACKREF_CI_RL,
    RE_CPAT,                    /* conditional pattern */
    RE_CPATA,                   /* conditional pattern */
    RE_ONCE,                    /* standalone pattern */
    RE_ASSERT,                  /* positive lookahead assertion. followed by
                                   offset (2 bytes). */
    RE_NASSERT,                 /* negative lookahead assertion. followed by
                                 * offset (2 bytes). */
    /* The following instructions are not necessary to implement the basic
       engine, but used in the optimized code */
    /* The *R instructions (and *R_RL counterparts) consumes all input that
       matches, without backtracking.  */
    RE_SET1R,                   /* (1-byte set match repeat)
                                   followed by charset #. */
    RE_SET1R_RL,
    RE_NSET1R,                  /* (1-byte negative set match repeat)
                                   followed by charset #. */
    RE_NSET1R_RL,
    RE_SETR,                    /* (set match repeat)
                                   followed by charset #. */
    RE_SETR_RL,
    RE_NSETR,                   /* (negative set match repeat)
                                   followed by charset #. */
    RE_NSETR_RL,
    RE_MATCH1R,                 /* (1-byte exact match repeat)
                                   followed by a byte */
    RE_MATCHR,                  /* (multiple byte exact match repeat)
                                   followed by length, and bytes to match. */
    RE_ANYR,                    /* (any char match repeat)  */
    RE_NUM_INSN
};

/* maximum # of {n,m}-type limited repeat count */
#define MAX_LIMITED_REPEAT 255

/* internal regexp flag. */
#define SCM_REGEXP_BOL_ANCHORED   (1L<<2) /* The regexp beginning is anchored
                                             by ^.*/
#define SCM_REGEXP_SIMPLE_PREFIX  (1L<<3) /* The regexp begins with a repeating
                                             character or charset, e.g. #/a+b/.
                                             See is_simple_prefixed() below. */

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
 *  <clause> : (seq . <ast>)       ; sequence
 *         | (seq-uncase . <ast>)  ; sequence (case insensitive match)
 *         | (seq-case . <ast>)    ; sequence (case sensitive match)
 *         | (alt . <ast>)         ; alternative
 *         | (rep <m> <n> . <ast>) ; repetition at least <m> up to <n> (greedy)
 *                                 ; <n> may be `#f'
 *         | (rep-min <m> <n> . <ast>)
 *                                 ; repetition at least <m> up to <n> (lazy)
 *                                 ; <n> may be `#f'
 *         | (rep-while <m> <n> . <ast>)
 *                                 ; like rep, but no backtrack
 *         | (<integer> <symbol> . <ast>)
 *                                 ; capturing group.  <symbol> may be #f.
 *         | (cpat <condition> <ast> <ast>)
 *                                 ; conditional expression
 *         | (backref . <integer>) ; backreference
 *         | (once . <ast>)        ; standalone pattern.  no backtrack
 *         | (assert . <asst>)     ; positive lookahead assertion
 *         | (nassert . <asst>)    ; negative lookahead assertion
 *
 *  <condition> : <integer>     ; (?(1)yes|no) style conditional expression
 *         | (assert . <asst>)  ; (?(?=condition)...) or (?(?<=condition)...)
 *         | (nassert . <asst>) ; (?(?!condition)...) or (?(?<!condition)...)
 *
 *  <asst> : <ast>
 *         | (lookbehind . <ast>)
 *
 * For seq-uncase, items inside <ast> has to be prepared for case-insensitive
 * match, i.e. chars have to be downcased and char-sets have to be
 * case-folded.
 */

/* NB: regexp printer is defined in libobj.scm */
static int  regexp_compare(ScmObj x, ScmObj y, int equalp);

SCM_DEFINE_BUILTIN_CLASS(Scm_RegexpClass,
                         NULL, regexp_compare, NULL, NULL,
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
    rx->grpNames = SCM_NIL;
    rx->mustMatch = NULL;
    rx->flags = 0;
    rx->pattern = SCM_FALSE;
    rx->ast = SCM_FALSE;
    return rx;
}

static int regexp_compare(ScmObj x, ScmObj y, int equalp)
{
    if (!equalp) {
        Scm_Error("cannot compare regexps: %S and %S", x, y);
    }
    ScmRegexp *rx = SCM_REGEXP(x);
    ScmRegexp *ry = SCM_REGEXP(y);

    if ((rx->numCodes != ry->numCodes)
        || (rx->numGroups != ry->numGroups)
        || (rx->numSets != ry->numSets)
        || !Scm_EqualP(rx->grpNames, ry->grpNames)
        || (rx->flags != ry->flags)) {
        return 1;
    } else {
        /* we compare bytecode. */
        for (int i=0; i<rx->numCodes; i++) {
            if (rx->code[i] != ry->code[i]) return 1;
        }
        for (int i=0; i<rx->numSets; i++) {
            if (rx->sets[i] == ry->sets[i]) continue;
            if (!Scm_CharSetEq(rx->sets[i], ry->sets[i])) return 1;
        }
    }
    return 0;
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
    ScmObj pattern;             /* original pattern or AST for diag msg */
    int casefoldp;              /* TRUE if case-folding match */
    int lookbehindp;            /* TRUE if lookbehind assertion match */
    ScmPort *ipat;              /* [pass1] string port for pattern */
    ScmObj sets;                /* [pass1] list of charsets */
    int grpcount;               /* [pass1] group count */
    unsigned char *code;        /* [pass3] code being built */
    int codep;                  /* [pass3] front of code generation */
    int emitp;                  /* [pass3] am I generating code? */
    int codemax;                /* [pass3] max codep */
} regcomp_ctx;

/* If we run from pass1, input string should be passed to PATTERN.
   If we run from pass2, PATTERN must be NULL and we take AST from RX. */
static void rc_ctx_init(regcomp_ctx *ctx,
                        ScmRegexp *rx,
                        ScmString *pattern)
{
    ctx->rx = rx;
    if (pattern) {
        ctx->pattern = Scm_CopyStringWithFlags(pattern,
                                               SCM_STRING_IMMUTABLE,
                                               SCM_STRING_IMMUTABLE);
        ctx->ipat = SCM_PORT(Scm_MakeInputStringPort(pattern, FALSE));
    } else {
        ctx->pattern = rx->ast;
        ctx->ipat = NULL;
    }
    ctx->casefoldp = FALSE;
    ctx->lookbehindp = FALSE;
    ctx->sets = SCM_NIL;
    ctx->grpcount = 0;
    ctx->code = NULL;
    ctx->codep = 0;
    ctx->emitp = FALSE;
    ctx->codemax = 1;
}

static ScmObj rc_charset(regcomp_ctx *ctx);
static void rc_register_charset(regcomp_ctx *ctx, ScmCharSet *cs);
static ScmObj rc1_rep(regcomp_ctx *ctx, ScmObj greedy,
                      ScmObj lazy, ScmObj atom);
static ScmObj rc1_read_integer(regcomp_ctx *ctx);
static ScmObj rc1_group_name(regcomp_ctx *ctx);
static ScmObj rc1_lex_minmax(regcomp_ctx *ctx);
static ScmObj rc1_lex_open_paren(regcomp_ctx *ctx);
static ScmObj rc1_lex_xdigits(ScmPort *port, int key);

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
 *         | <atom> "*+"
 *         | <atom> "++"
 *         | <atom> "?+"
 *         | <atom>
 *
 *  <atom> : a normal char, an escaped char, or a char-set
 *         | "\\" <integer>     ;; backreference
 *         | "\\k<" <string> ">" ;; backreference to named group
 *         | "(" <re> ")"       ;; grouping w/  capturing
 *         | "(?:"   <re> ")"   ;; grouping w/o capturing
 *         | "(?i:"  <re> ")"   ;; grouping w/o capturing (case insensitive)
 *         | "(?-i:" <re> ")"   ;; grouping w/o capturing (case sensitive)
 *         | "(?<" <string> ">" <re> ")" ;; named capturing group
 *         | "(?>"   <re> ")"   ;; standalone pattern
 *         | "(?="   <re> ")"   ;; positive lookahead assertion
 *         | "(?!"   <re> ")"   ;; negative lookahead assertion
 *         | "(?<="  <re> ")"   ;; positive lookbehind assertion
 *         | "(?<!"  <re> ")"   ;; negative lookbehind assertion
 *         | "(?("cond")"yes-pattern"|"no-pattern")"
 *         | "(?("cond")"yes-pattern")" ;; conditional pattern
 *
 */
/* TODO: It'd be nicer to have a dedicated condition to throw a parse error. */

/* Lexer */
static ScmObj rc1_lex(regcomp_ctx *ctx)
{
    ScmObj cs;

    ScmChar ch = Scm_GetcUnsafe(ctx->ipat);
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
    case '+': return rc1_rep(ctx, SCM_SYM_PLUS, SCM_SYM_PLUSQ, SCM_SYM_PLUSP);
    case '*': return rc1_rep(ctx, SCM_SYM_STAR, SCM_SYM_STARQ, SCM_SYM_STARP);
    case '?': return rc1_rep(ctx, SCM_SYM_QUESTION, SCM_SYM_QUESTIONQ, SCM_SYM_QUESTIONP);
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
        case 'x': case 'u': case 'U':
            return rc1_lex_xdigits(ctx->ipat, ch);
        case 'd':
            cs = Scm_GetStandardCharSet(SCM_CHAR_SET_DIGIT);
            rc_register_charset(ctx, SCM_CHAR_SET(cs));
            return cs;
        case 'D':
            cs = Scm_GetStandardCharSet(SCM_CHAR_SET_DIGIT);
            rc_register_charset(ctx, SCM_CHAR_SET(cs));
            return Scm_Cons(SCM_SYM_COMP, cs);
        case 'w':
            cs = Scm_GetStandardCharSet(SCM_CHAR_SET_WORD);
            rc_register_charset(ctx, SCM_CHAR_SET(cs));
            return cs;
        case 'W':
            cs = Scm_GetStandardCharSet(SCM_CHAR_SET_WORD);
            rc_register_charset(ctx, SCM_CHAR_SET(cs));
            return Scm_Cons(SCM_SYM_COMP, cs);
        case 's':
            cs = Scm_GetStandardCharSet(SCM_CHAR_SET_SPACE);
            rc_register_charset(ctx, SCM_CHAR_SET(cs));
            return cs;
        case 'S':
            cs = Scm_GetStandardCharSet(SCM_CHAR_SET_SPACE);
            rc_register_charset(ctx, SCM_CHAR_SET(cs));
            return Scm_Cons(SCM_SYM_COMP, cs);
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            Scm_UngetcUnsafe(ch, ctx->ipat);
            return Scm_Cons(SCM_SYM_BACKREF, rc1_read_integer(ctx));
        case 'k': {
            if (Scm_GetcUnsafe(ctx->ipat) != '<') {
                Scm_Error("\\k must be followed by '<': %S", ctx->pattern);
            }
            {
                ScmObj name = rc1_group_name(ctx);
                if (SCM_FALSEP(name)) {
                    Scm_Error("malformed backreference found in regexp %S", ctx->pattern);
                }
                return Scm_Cons(SCM_SYM_BACKREF, name);
            }
        }
        }
        /*FALLTHROUGH*/
    default:
        if (ctx->casefoldp) ch = SCM_CHAR_DOWNCASE(ch);
        return SCM_MAKE_CHAR(ch);
    }
    /*NOTREACHED*/
}

/* Read \x, \u, \U escape sequence in the regexp spec.
   This may read-ahead some hexdigit characters.  In such case, it
   returns SEQ node. */
static ScmObj rc1_lex_xdigits(ScmPort *port, int key)
{
    ScmDString buf;

    Scm_DStringInit(&buf);
    ScmObj bad = Scm_ReadXdigitsFromPort(port, key, 0, FALSE, &buf);
    if (SCM_STRINGP(bad)) {
        /* skip chars to the end of regexp, so that the reader will read
           after the erroneous string */
        for (;;) {
            int c;
            SCM_GETC(c, port);
            if (c == EOF || c == '/') break;
            if (c == '\\') SCM_GETC(c, port);
        }
        /* construct an error message */
        Scm_ReadError(port,
                      "Bad '\\%c' escape sequence in a regexp literal: \\%c%A",
                      key, key, bad);
        return SCM_UNDEFINED;   /* dummy */
    } else {
        int size, len;
        const char *chars = Scm_DStringPeek(&buf, &size, &len);
        if (len == 1) {
            ScmChar ch;
            SCM_CHAR_GET(chars, ch);
            return SCM_MAKE_CHAR(ch);
        } else {
            ScmObj h = SCM_NIL, t = SCM_NIL;
            SCM_APPEND1(h, t, SCM_SYM_SEQ);
            while (len-- > 0) {
                ScmChar ch;
                SCM_CHAR_GET(chars, ch);
                chars += SCM_CHAR_NBYTES(ch);
                SCM_APPEND1(h, t, SCM_MAKE_CHAR(ch));
            }
            return h;
        }
    }
}

/* Called after '+', '*' or '?' is read, and check if there's a
   following '?' (lazy quantifier) or '+' (atomic expression) */
static ScmObj rc1_rep(regcomp_ctx *ctx, ScmObj greedy,
                      ScmObj lazy, ScmObj atom)
{
    ScmChar ch = Scm_GetcUnsafe(ctx->ipat);
    if (ch == '?') return lazy;
    if (ch == '+') return atom;
    Scm_UngetcUnsafe(ch, ctx->ipat);
    return greedy;
}

/* Reads '('-sequence - either one of "(", "(?:", "(?i:", "(?-i:",
   "(?=", "(?<=", "(?<!", "(?<name>", "(?(condition)|).  The leading
   "(" has already been read. */
static ScmObj rc1_lex_open_paren(regcomp_ctx *ctx)
{
    ScmObj pos = Scm_PortSeekUnsafe(ctx->ipat, SCM_MAKE_INT(0), SEEK_CUR);
    ScmChar ch = Scm_GetcUnsafe(ctx->ipat);
    if (ch != '?') {
        Scm_UngetcUnsafe(ch, ctx->ipat);
        return SCM_SYM_OPEN_PAREN;
    }
    ch = Scm_GetcUnsafe(ctx->ipat);
    switch (ch) {
    case ':': return SCM_SYM_SEQ;
    case '>': return SCM_SYM_ONCE;
    case '=': return SCM_SYM_ASSERT;
    case '!': return SCM_SYM_NASSERT;
    case '(': return SCM_SYM_CPAT;
    case '<': {
        ch = Scm_GetcUnsafe(ctx->ipat);
        if (ch == '=') return SCM_SYM_LOOKBEHIND;
        if (ch == '!') return SCM_SYM_NLOOKBEHIND;
        Scm_UngetcUnsafe(ch, ctx->ipat);
        ScmObj name = rc1_group_name(ctx);
        if (!SCM_FALSEP(name)) return Scm_Cons(SCM_SYM_REG, name);
        break;
    }
    case 'i':
        ch = Scm_GetcUnsafe(ctx->ipat);
        if (ch == ':') return SCM_SYM_SEQ_UNCASE;
        break;
    case '-':
        ch = Scm_GetcUnsafe(ctx->ipat);
        if (ch == 'i') {
            ch = Scm_GetcUnsafe(ctx->ipat);
            if (ch == ':') return SCM_SYM_SEQ_CASE;
        }
        break;
    }
    /* fail. */
    Scm_PortSeekUnsafe(ctx->ipat, pos, SEEK_SET);
    return SCM_SYM_OPEN_PAREN;
}

static ScmObj rc1_read_integer(regcomp_ctx *ctx)
{
    ScmChar ch = Scm_GetcUnsafe(ctx->ipat);
    if (!isdigit(ch)) {
        Scm_Error("number expected, but got '%c'", ch);
    }
    ScmDString ds;
    Scm_DStringInit(&ds);
    do {
        Scm_DStringPutc(&ds, ch);
        ch = Scm_GetcUnsafe(ctx->ipat);
    } while (ch != SCM_CHAR_INVALID && isdigit(ch));
    if (ch != SCM_CHAR_INVALID) {
        Scm_UngetcUnsafe(ch, ctx->ipat);
    }
    ScmObj r = Scm_StringToNumber(SCM_STRING(Scm_DStringGet(&ds, 0)), 10, 0);
    if (SCM_BIGNUMP(r)) {
        Scm_Error("number too big: %S", r);
    }
    SCM_ASSERT(SCM_INTP(r));
    return r;
}

static ScmObj rc1_group_name(regcomp_ctx *ctx)
{
    ScmDString ds;
    Scm_DStringInit(&ds);

    for (;;) {
        ScmChar ch = Scm_GetcUnsafe(ctx->ipat);
        if (ch == SCM_CHAR_INVALID) return SCM_FALSE;
        if (ch == '>') {
            return Scm_Intern(SCM_STRING(Scm_DStringGet(&ds, 0)));
        }
        if (ch == '\\') {
            ch = Scm_GetcUnsafe(ctx->ipat);
            if (ch == SCM_CHAR_INVALID) return SCM_FALSE;
            /* fall through */
        }
        Scm_DStringPutc(&ds, ch);
    }
    return SCM_UNDEFINED;       /* dummy */
}

/* Reads {n,m}-type repeat syntax.  The leading "{" has been read.
   If the character sequence doesn't consist of valid syntax, rollback
   to the ordinary character sequence.
   If successfully parsed, returns (rep <n> . <m>) where
    <n> == <m> if the pattern is "{n}" (exact count), or
    <m> == #f if the pattern is "{n,}" (minimum count), or
    <m> == integer if the pattern is "{n,m}" (limited count).
   If the pattern is followed by '?', rep-min is used instead.
 */
static ScmObj rc1_lex_minmax(regcomp_ctx *ctx)
{
    int rep_min = -1, rep_max = -1, exact = FALSE;
    ScmObj type = SCM_SYM_REP; /* default is greedy */
    ScmObj pos = Scm_PortSeekUnsafe(ctx->ipat, SCM_MAKE_INT(0), SEEK_CUR);

    for (;;) {
        int ch = Scm_GetcUnsafe(ctx->ipat);
        if (SCM_CHAR_ASCII_P(ch) && isdigit(ch)) {
            if (rep_min < 0) {
                rep_min = (ch - '0');
            } else {
                rep_min = rep_min*10 + (ch - '0');
            }
        } else if (ch == ',') {
            /* NB: The following line makes us to treat {,m} as {0,m}.
               Oniguruma does so.  Perl doesn't.  Strictly speaking they're
               incompatibile to each other (if Perl code /a{,3}/ expects
               to match "a{,3}", it needs to be written as /a\{,3}/ in
               Oniguruma).   Let's take Oniguruma now. */
            if (rep_min < 0) rep_min = 0;
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
            int ch = Scm_GetcUnsafe(ctx->ipat);
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

    ScmObj m;
    if (exact)            m = SCM_MAKE_INT(rep_min);
    else if (rep_max < 0) m = SCM_FALSE;
    else                  m = SCM_MAKE_INT(rep_max);

    int ch = Scm_GetcUnsafe(ctx->ipat);
    if (ch == '?') type = SCM_SYM_REP_MIN;
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

static ScmObj rc1_parse(regcomp_ctx *, int, ScmObj);

static ScmObj rc1_lex_conditional_pattern(regcomp_ctx *ctx, int bolp,
                                          ScmObj grps)
{
    ScmChar ch = Scm_GetcUnsafe(ctx->ipat);
    if (ch == SCM_CHAR_INVALID)
        goto error;
    if (isdigit(ch)) {
        Scm_UngetcUnsafe(ch, ctx->ipat);
        ScmObj r = rc1_read_integer(ctx);
        if (!SCM_EQ(rc1_lex(ctx), SCM_SYM_CLOSE_PAREN))
            goto error;
        return r;
    }
    if (ch == '?') {
        ch = Scm_GetcUnsafe(ctx->ipat);
        if (ch == '=')
            return Scm_Cons(SCM_SYM_ASSERT, rc1_parse(ctx, bolp, grps));
        if (ch == '!')
            return Scm_Cons(SCM_SYM_NASSERT, rc1_parse(ctx, bolp, grps));
        if (ch == '<') {
            ScmObj type;
            ch = Scm_GetcUnsafe(ctx->ipat);
            if (ch == '=') {
                type = SCM_SYM_ASSERT;
            } else if (ch == '!') {
                type = SCM_SYM_NASSERT;
            } else {
                Scm_Error("unknown switch condition (?>%c...) in regexp %S",
                          ch, ctx->pattern);
                type = SCM_SYM_NASSERT; /* dummy */
            }
            ScmObj item = rc1_parse(ctx, bolp, grps);
            return SCM_LIST2(type, Scm_Cons(SCM_SYM_LOOKBEHIND, item));
        }
        /* fallthru */
    }
    Scm_Error("unknown switch condition (?%c...) in regexp %S", ch, ctx->pattern);

  error:
    Scm_Error("unterminated conditional pattern in regexp %S", ctx->pattern);
    return SCM_UNDEFINED;       /* dummy */
}

/* Parser */
/* Groups represents the current nestings of parentheses, including both
   capturing groups and non-capturing parens.  It works like a stack, where
   the leftmost item is for the innermost open paren.  The value is a
   group number for capturing groups, and #f for others. */
static ScmObj rc1_parse(regcomp_ctx *ctx, int bolp, ScmObj groups)
{
    ScmObj stack = SCM_NIL, alts = SCM_NIL;
    int bolpsave = bolp;

#define TOPP()     (SCM_NULLP(groups))
#define PUSH(elt)  (stack = Scm_Cons((elt), stack))
#define PUSH1(elt) (stack = Scm_Cons((elt), SCM_CDR(stack)))

    for (;;) {
        ScmObj token = rc1_lex(ctx);
        ScmObj item;
        if (SCM_EOFP(token)) {
            if (!TOPP()) {
                Scm_Error("unterminated grouping in regexp %S", ctx->pattern);
            }
            break;
        }
        if (SCM_EQ(token, SCM_SYM_CLOSE_PAREN)) {
            if (TOPP()) {
                Scm_Error("extra close parenthesis in regexp %S", ctx->pattern);
            }
            groups = SCM_CDR(groups);
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
            item = rc1_parse(ctx, bolp, Scm_Cons(SCM_MAKE_INT(grpno), groups));
            PUSH(Scm_Cons(SCM_MAKE_INT(grpno), Scm_Cons(SCM_FALSE, item)));
            bolp = FALSE;
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_SEQ)) {
            item = rc1_parse(ctx, bolp, Scm_Cons(SCM_FALSE, groups));
            PUSH(Scm_Cons(SCM_SYM_SEQ, item));
            bolp = FALSE;
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_SEQ_UNCASE) || SCM_EQ(token, SCM_SYM_SEQ_CASE)) {
            int oldflag = ctx->casefoldp;
            ctx->casefoldp = SCM_EQ(token, SCM_SYM_SEQ_UNCASE);
            item = rc1_parse(ctx, bolp, Scm_Cons(SCM_FALSE, groups));
            PUSH(Scm_Cons(token, item));
            ctx->casefoldp = oldflag;
            bolp = FALSE;
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_ONCE)) {
            /* (?>re) can have BOL/EOL.*/
            item = rc1_parse(ctx, TRUE, Scm_Cons(SCM_FALSE, groups));
            PUSH(Scm_Cons(SCM_SYM_ONCE, item));
            bolp = FALSE;
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_ASSERT) || SCM_EQ(token, SCM_SYM_NASSERT)) {
            /* (?=re) and (?!re) can have BOL/EOL.*/
            item = rc1_parse(ctx, TRUE, Scm_Cons(SCM_FALSE, groups));
            PUSH(Scm_Cons(token, item));
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_LOOKBEHIND) || SCM_EQ(token, SCM_SYM_NLOOKBEHIND)) {
            /* "(?<=a)" => (assert (lookbehind a))
               "(?<!a)" => (nassert (lookbehind a)) */
            item = rc1_parse(ctx, TRUE, Scm_Cons(SCM_FALSE, groups));
            PUSH(SCM_LIST2(SCM_EQ(token, SCM_SYM_LOOKBEHIND)? SCM_SYM_ASSERT : SCM_SYM_NASSERT,
                           Scm_Cons(SCM_SYM_LOOKBEHIND, item)));
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_STAR) || SCM_EQ(token, SCM_SYM_STARP)) {
            /* "x*"  => (rep 0 #f x)
               "x*+" => (once (rep 0 #f x)) */
            if (SCM_NULLP(stack)) goto synerr;
            item = SCM_LIST4(SCM_SYM_REP, SCM_MAKE_INT(0), SCM_FALSE, SCM_CAR(stack));
            PUSH1(SCM_EQ(token, SCM_SYM_STAR) ? item : SCM_LIST2(SCM_SYM_ONCE, item));
            bolp = FALSE;
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_STARQ)) {
            /* "x*?" => (rep-min 0 #f x) */
            if (SCM_NULLP(stack)) goto synerr;
            item = SCM_LIST4(SCM_SYM_REP_MIN, SCM_MAKE_INT(0), SCM_FALSE, SCM_CAR(stack));
            PUSH1(item);
            bolp = FALSE;
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_PLUS) || SCM_EQ(token, SCM_SYM_PLUSP)) {
            /* "x+"  => (rep 1 #f x))
               "x++" => (once (rep 1 #f x)) */
            if (SCM_NULLP(stack)) goto synerr;
            item = SCM_LIST4(SCM_SYM_REP, SCM_MAKE_INT(1), SCM_FALSE, SCM_CAR(stack));
            PUSH1(SCM_EQ(token, SCM_SYM_PLUS) ? item : SCM_LIST2(SCM_SYM_ONCE, item));
            bolp = FALSE;
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_PLUSQ)) {
            /* "x+?" => (rep-min 1 #f x) */
            if (SCM_NULLP(stack)) goto synerr;
            item = SCM_LIST4(SCM_SYM_REP_MIN, SCM_MAKE_INT(1), SCM_FALSE, SCM_CAR(stack));
            PUSH1(item);
            bolp = FALSE;
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_QUESTION) || SCM_EQ(token, SCM_SYM_QUESTIONP)) {
            /* "x?"  => (rep 0 1 x)
               "x?+" => (once (rep 0 1 x)) */
            if (SCM_NULLP(stack)) goto synerr;
            item = SCM_LIST4(SCM_SYM_REP, SCM_MAKE_INT(0), SCM_MAKE_INT(1), SCM_CAR(stack));
            PUSH1(SCM_EQ(token, SCM_SYM_QUESTION) ? item : SCM_LIST2(SCM_SYM_ONCE, item));
            bolp = FALSE;
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_QUESTIONQ)) {
            /* "x??" => (rep-min 0 1 x) */
            if (SCM_NULLP(stack)) goto synerr;
            item = SCM_LIST4(SCM_SYM_REP_MIN, SCM_MAKE_INT(0), SCM_MAKE_INT(1), SCM_CAR(stack));
            PUSH1(item);
            bolp = FALSE;
            continue;
        }
        if (SCM_EQ(token, SCM_SYM_CPAT)) {
            ScmObj cond, ypat, npat;
            ScmObj new_groups = Scm_Cons(SCM_FALSE, groups);
            cond = rc1_lex_conditional_pattern(ctx, bolp, new_groups);
            item = rc1_parse(ctx, bolp, new_groups);
            if (SCM_PAIRP(item) && SCM_PAIRP(SCM_CAR(item))
                && SCM_EQ(SCM_CAAR(item), SCM_SYM_ALT)) {
                ScmObj elt = SCM_CAR(item);
                if (Scm_Length(elt) > 3) {
                    Scm_Error("Conditional pattern contains too much branches: %S",
                              ctx->pattern);
                }
                ypat = SCM_LIST1(SCM_CADR(elt));
                npat = SCM_CDDR(elt);
            } else {
                ypat = item;
                npat = SCM_NIL;
            }
            PUSH(SCM_LIST4(SCM_SYM_CPAT, cond, ypat, npat));
            bolp = FALSE;
            continue;
        }
        if (SCM_PAIRP(token) && SCM_EQ(SCM_CAR(token), SCM_SYM_REG)) {
            /* "(?P<name>x)" => (<integer> name . <ast>)) */
            int grpno = ++ctx->grpcount;
            ScmObj name = SCM_CDR(token);
            item = rc1_parse(ctx, bolp, Scm_Cons(SCM_MAKE_INT(grpno), groups));
            PUSH(Scm_Cons(SCM_MAKE_INT(grpno), Scm_Cons(name, item)));
            ctx->rx->grpNames = Scm_Acons(name, SCM_MAKE_INT(grpno), ctx->rx->grpNames);
            bolp = FALSE;
            continue;
        }
        if (SCM_PAIRP(token) &&
            (SCM_EQ(SCM_CAR(token), SCM_SYM_REP) ||
             SCM_EQ(SCM_CAR(token), SCM_SYM_REP_MIN))) {
            /* "x{n}"    => (rep n n x)
               "x{n,}"   => (rep n #f x)
               "x{n,m}"  => (rep n m x)
               "x{n,}?"  => (rep-min n #t x)
               "x{n,m}?" => (rep-min n m x) */
            if (SCM_NULLP(stack)) goto synerr;
            item = SCM_LIST4(SCM_CAR(token), SCM_CADR(token), SCM_CDDR(token), SCM_CAR(stack));
            PUSH1(item);
            bolp = FALSE;
            continue;
        }
        if (SCM_PAIRP(token) && SCM_EQ(SCM_CAR(token), SCM_SYM_BACKREF)) {
            ScmObj h = SCM_NIL, t = SCM_NIL;
            ScmObj ref = SCM_CDR(token);
            if (SCM_INTP(ref)) {
                int grpno = SCM_INT_VALUE(ref);
                if (ctx->grpcount < grpno
                    || !SCM_FALSEP(Scm_Memv(SCM_MAKE_INT(grpno), groups))) {
                    Scm_Error("Backreference \\%d refers to an unfinished group.",
                              grpno);
                }
                PUSH(token);
                bolp = FALSE;
                continue;
            }

            SCM_ASSERT(SCM_SYMBOLP(ref));
            ScmObj ep;
            SCM_FOR_EACH(ep, ctx->rx->grpNames) {
                if (!SCM_EQ(SCM_CAAR(ep), ref)) continue;
                SCM_APPEND1(h, t, Scm_Cons(SCM_SYM_BACKREF, SCM_CDAR(ep)));
            }
            if (SCM_NULLP(h)) goto synerr;

            PUSH(SCM_NULLP(SCM_CDR(h))? SCM_CAR(h) : Scm_Cons(SCM_SYM_ALT, h));
            bolp = FALSE;
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
    ScmObj ast = rc1_parse(ctx, TRUE, SCM_NIL);
    if (ctx->casefoldp) {
        ast = SCM_LIST3(SCM_MAKE_INT(0), SCM_FALSE,
                        Scm_Cons(SCM_SYM_SEQ_UNCASE, ast));
    } else {
        ast = Scm_Cons(SCM_MAKE_INT(0), Scm_Cons(SCM_FALSE, ast));
    }
    int ngrp = ctx->grpcount + 1;
    ctx->rx->numGroups = ngrp;
    return ast;
}

/* character range */
static ScmObj rc_charset(regcomp_ctx *ctx)
{
    int complement;
    ScmObj set = Scm_CharSetRead(ctx->ipat, &complement, FALSE, TRUE);
    if (!SCM_CHAR_SET_P(set)) {
        Scm_Error("bad charset spec in pattern: %S", ctx->pattern);
    }
    if (ctx->casefoldp) {
        Scm_CharSetCaseFold(SCM_CHAR_SET(set));
    }

    rc_register_charset(ctx, SCM_CHAR_SET(set));
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
    rx->numSets = Scm_Length(ctx->sets);
    rx->sets = SCM_NEW_ARRAY(ScmCharSet*, rx->numSets);
    ScmObj cp = Scm_Reverse(ctx->sets);
    for (int i=0; !SCM_NULLP(cp); cp = SCM_CDR(cp)) {
        rx->sets[i++] = SCM_CHAR_SET(SCM_CAR(cp));
    }
}

/*-------------------------------------------------------------
 * pass 2: optimizer
 *
 *  - flattening nested sequences: (seq a (seq b) c) => (seq a b c)
 *  - introduces short-cut construct for certain cases.
 *       (... (rep <m> <n> #\a) #\b ...)
 *       => (... (rep-while <m> <n> #\a) #\b ...)
 */
static ScmObj rc2_optimize(ScmObj ast, ScmObj rest);
static int    is_distinct(ScmObj x, ScmObj y);

static ScmObj rc2_optimize_seq(ScmObj seq, ScmObj rest)
{
    if (!SCM_PAIRP(seq)) return seq;
    ScmObj opted;
    ScmObj elt = SCM_CAR(seq);
    ScmObj tail = rc2_optimize_seq(SCM_CDR(seq), rest);
    rest = SCM_NULLP(tail)? rest : tail;
    if (!SCM_PAIRP(elt) || SCM_EQ(SCM_CAR(elt), SCM_SYM_COMP)) {
        if (SCM_EQ(tail, SCM_CDR(seq))) return seq;
        else return Scm_Cons(elt, tail);
    }
    ScmObj etype = SCM_CAR(elt);
    if (SCM_EQ(etype, SCM_SYM_SEQ)) {
        return Scm_Append2(rc2_optimize_seq(SCM_CDR(elt), rest), tail);
    }
    if (SCM_EQ(etype, SCM_SYM_REP)) {
        /* If the head of repeating sequence and the beginning of the
           following sequence are distinct, like #/\s*foo/, the branch
           becomes deterministic (i.e. we don't need backtrack). */
        ScmObj repbody = rc2_optimize_seq(SCM_CDR(SCM_CDDR(elt)), rest);
        SCM_ASSERT(SCM_PAIRP(repbody));
        if (SCM_NULLP(rest) || is_distinct(SCM_CAR(repbody), SCM_CAR(rest))) {
            ScmObj elt2 = Scm_Append2(SCM_LIST3(SCM_SYM_REP_WHILE, SCM_CADR(elt),
                                                SCM_CAR(SCM_CDDR(elt))),
                                      repbody);
            return Scm_Cons(elt2, tail);
        }
        if (SCM_EQ(repbody, SCM_CDR(SCM_CDDR(elt)))) opted = elt;
        else opted = Scm_Append2(SCM_LIST3(SCM_SYM_REP, SCM_CADR(elt),
                                           SCM_CAR(SCM_CDDR(elt))),
                                 repbody);
    } else {
        opted = rc2_optimize(elt, rest);
    }
    if (SCM_EQ(elt, opted) && SCM_EQ(tail, SCM_CDR(seq))) return seq;
    else return Scm_Cons(opted, tail);
}

static ScmObj rc2_optimize(ScmObj ast, ScmObj rest)
{
    if (!SCM_PAIRP(ast)) return ast;
    ScmObj seq, seqo;
    ScmObj type = SCM_CAR(ast);
    if (SCM_EQ(type, SCM_SYM_COMP)) return ast;
    if (SCM_EQ(type, SCM_SYM_LOOKBEHIND)) return ast;

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
    if (SCM_EQ(type, SCM_SYM_REP) || SCM_EQ(type, SCM_SYM_REP_MIN)
        || SCM_EQ(type, SCM_SYM_REP_WHILE)) {
        seq = SCM_CADR(SCM_CDDR(ast));
        seqo = rc2_optimize_seq(seq, rest);
        if (SCM_EQ(seq, seqo)) return ast;
        return SCM_LIST4(type, SCM_CADR(ast), SCM_CAR(SCM_CDDR(ast)), seqo);
    }
    seq = SCM_CDR(ast);
    seqo = rc2_optimize_seq(seq, rest);
    if (SCM_EQ(seq, seqo)) return ast;
    return Scm_Cons(type, seqo);
}

static int is_distinct(ScmObj x, ScmObj y)
{
    if (SCM_PAIRP(x)) {
        ScmObj carx = SCM_CAR(x);
        if (SCM_EQ(carx, SCM_SYM_COMP)) {
            SCM_ASSERT(SCM_CHAR_SET_P(SCM_CDR(x)));
            if (SCM_CHARP(y) || SCM_CHAR_SET_P(y)) {
                return !is_distinct(SCM_CDR(x), y);
            }
            return FALSE;
        }
        if (SCM_INTP(carx)) {
            if (SCM_PAIRP(SCM_CDDR(x))) {
                return is_distinct(SCM_CAR(SCM_CDDR(x)), y);
            }
        }
        if (SCM_EQ(carx, SCM_SYM_SEQ_UNCASE)
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
    if (SCM_CHAR_SET_P(x)) {
        if (SCM_CHARP(y)) {
            return !Scm_CharSetContains(SCM_CHAR_SET(x), SCM_CHAR_VALUE(y));
        }
        if (SCM_CHAR_SET_P(y)) {
            ScmObj ccs = Scm_CharSetCopy(SCM_CHAR_SET(y));
            ccs = Scm_CharSetComplement(SCM_CHAR_SET(ccs));
            return Scm_CharSetLE(SCM_CHAR_SET(x), SCM_CHAR_SET(ccs));
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

static void rc3_rec(regcomp_ctx *ctx, ScmObj ast, int lastp);

/* Util function for pass3, to get an index of the charset vector
 * for the given charset.
 */
static int rc3_charset_index(ScmRegexp *rx, ScmObj cs)
{
    for (int i=0; i<rx->numSets; i++)
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

#define EMIT4(cond, insn1, insn2, insn3, insn4)                     \
    rc3_emit(ctx, (cond)? (!ctx->lookbehindp)? insn1 : insn2     \
                        : (!ctx->lookbehindp)? insn3 : insn4)

static void rc3_seq(regcomp_ctx *ctx, ScmObj seq, int lastp)
{
    ScmObj cp;

    if (ctx->lookbehindp) seq = Scm_Reverse(seq);

    SCM_FOR_EACH(cp, seq) {
        ScmObj item = SCM_CAR(cp);

        /* concatenate literal character sequence */
        if (SCM_CHARP(item)) {
            ScmObj h = SCM_NIL, t = SCM_NIL;
            int nrun = 0;
            char chbuf[SCM_CHAR_MAX_BYTES];

            do {
                ScmChar ch = SCM_CHAR_VALUE(item);
                nrun += SCM_CHAR_NBYTES(ch);
                SCM_APPEND1(h, t, item);
                cp = SCM_CDR(cp);
                if (SCM_NULLP(cp)) break;
                item = SCM_CAR(cp);
            } while (SCM_CHARP(item) && nrun < CHAR_MAX);
            if (ctx->lookbehindp) h = Scm_ReverseX(h);
            if (nrun == 1) {
                EMIT4(!ctx->casefoldp, RE_MATCH1, RE_MATCH1_RL, RE_MATCH1_CI, RE_MATCH1_CI_RL);
                rc3_emit(ctx, (char)SCM_CHAR_VALUE(SCM_CAR(h)));
            } else {
                EMIT4(!ctx->casefoldp, RE_MATCH, RE_MATCH_RL, RE_MATCH_CI, RE_MATCH_CI_RL);
                rc3_emit(ctx, (char)nrun);
                ScmObj ht;
                SCM_FOR_EACH(ht, h) {
                    ScmChar ch = SCM_CHAR_VALUE(SCM_CAR(ht));
                    int nb = SCM_CHAR_NBYTES(ch);
                    SCM_CHAR_PUT(chbuf, ch);
                    for (int i = 0; i < nb; i++) rc3_emit(ctx, chbuf[i]);
                }
            }
            if (SCM_NULLP(cp)) break;
            cp = Scm_Cons(item, cp); /* pushback */
        } else {
            int p;
            if (ctx->lookbehindp) p = lastp && SCM_EQ(cp, seq);
            else p = lastp && SCM_NULLP(SCM_CDR(cp));
            rc3_rec(ctx, item, p);
        }
    }
}

static void rc3_seq_rep(regcomp_ctx *ctx, ScmObj seq, int count, int lastp)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    if (count <= 0) return;
    while (count-- > 0) {
        SCM_APPEND(h, t, Scm_CopyList(seq));
    }
    rc3_seq(ctx, h, lastp);
}

static void rc3_minmax(regcomp_ctx *ctx, ScmObj type, int count,
                       ScmObj item, int lastp)
{
    /* (rep <n> . <x>)
                 TRY  #01
                 JUMP #11
           #01:  TRY  #02
                 JUMP #12
                  :
           #0<n>:JUMP #1<N>
           #11:  <X>
           #12:  <X>
                  :
           #1<n>:<X>
           #1<N>:

       (rep-min <n> . <x>)
                 TRY  #01
                 JUMP #1N
           #01:  TRY  #02
                 JUMP #1n
                  :
           #0<n>:TRY  #11
                 JUMP #12
           #11:  <X>
           #12:  <X>
                  :
           #1<n>:<X>
           #1<N>:
    */
    ScmObj jlist = SCM_NIL;
    int j0 = 0, jn;
    int greedy = SCM_EQ(type, SCM_SYM_REP);

    /* first part - TRYs and JUMPs
       j0 is used to patch the label #0k
       the destination of jumps to be patched are linked to jlist */
    for (int n=0; n<count; n++) {
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
    for (int n=0; n<count; n++) {
        if (ctx->emitp) {
            rc3_fill_offset(ctx, SCM_INT_VALUE(SCM_CAR(jlist)),
                            ctx->codep);
        }
        rc3_seq(ctx, item, FALSE);
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
}

static void rc3_rec(regcomp_ctx *ctx, ScmObj ast, int lastp)
{
    ScmRegexp *rx = ctx->rx;

    /* first, deal with atoms */
    if (!SCM_PAIRP(ast)) {
        /* a char */
        if (SCM_CHARP(ast)) {
            char chbuf[SCM_CHAR_MAX_BYTES];
            ScmChar ch = SCM_CHAR_VALUE(ast);
            int nb = SCM_CHAR_NBYTES(ch);
            SCM_CHAR_PUT(chbuf, ch);
            if (nb == 1) {
                EMIT4(!ctx->casefoldp, RE_MATCH1, RE_MATCH1_RL, RE_MATCH1_CI, RE_MATCH1_CI_RL);
                rc3_emit(ctx, chbuf[0]);
            } else {
                EMIT4(!ctx->casefoldp, RE_MATCH, RE_MATCH_RL, RE_MATCH_CI, RE_MATCH_CI_RL);
                rc3_emit(ctx, nb);
                for (int i=0; i<nb; i++) rc3_emit(ctx, chbuf[i]);
            }
            return;
        }
        /* charset */
        if (SCM_CHAR_SET_P(ast)) {
            EMIT4(!SCM_CHAR_SET_SMALLP(ast), RE_SET, RE_SET_RL, RE_SET1, RE_SET1_RL);
            rc3_emit(ctx, rc3_charset_index(rx, ast));
            return;
        }
        /* special stuff */
        if (SCM_SYMBOLP(ast)) {
            if (SCM_EQ(ast, SCM_SYM_ANY)) {
                rc3_emit(ctx, ctx->lookbehindp?RE_ANY_RL:RE_ANY);
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
                    rc3_emit(ctx, ctx->lookbehindp? RE_MATCH1_RL:RE_MATCH1);
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
    ScmObj type = SCM_CAR(ast);
    if (SCM_EQ(type, SCM_SYM_COMP)) {
        ScmObj cs = SCM_CDR(ast);
        SCM_ASSERT(SCM_CHAR_SET_P(cs));
        EMIT4(!SCM_CHAR_SET_SMALLP(cs), RE_NSET, RE_NSET_RL, RE_NSET1, RE_NSET1_RL);
        rc3_emit(ctx, rc3_charset_index(rx, cs));
        return;
    }
    if (SCM_EQ(type, SCM_SYM_SEQ)) {
        rc3_seq(ctx, SCM_CDR(ast), lastp);
        return;
    }
    if (SCM_INTP(type)) {
        /* (<integer> <name> . <ast>) */
        int grpno = SCM_INT_VALUE(SCM_CAR(ast));
        rc3_emit(ctx, ctx->lookbehindp?RE_BEGIN_RL:RE_BEGIN);
        rc3_emit(ctx, grpno);
        rc3_seq(ctx, SCM_CDDR(ast), lastp);
        rc3_emit(ctx, ctx->lookbehindp?RE_END_RL:RE_END);
        rc3_emit(ctx, grpno);
        return;
    }
    if (SCM_EQ(type, SCM_SYM_SEQ_UNCASE) || SCM_EQ(type, SCM_SYM_SEQ_CASE)) {
        int oldcase = ctx->casefoldp;
        ctx->casefoldp = SCM_EQ(type, SCM_SYM_SEQ_UNCASE);
        rc3_seq(ctx, SCM_CDR(ast), lastp);
        ctx->casefoldp = oldcase;
        return;
    }
    if (SCM_EQ(type, SCM_SYM_REP_WHILE)) {
        /* here we have an opportunity to generate an optimized code.
           for now, we only check elem is a single item case, but we can
           do better. */
        /* (rep-while m n . elem) */
        ScmObj m = SCM_CADR(ast), n = SCM_CAR(SCM_CDDR(ast));
        ScmObj elem = SCM_CDR(SCM_CDDR(ast));
        if (SCM_FALSEP(n) && SCM_PAIRP(elem) && SCM_NULLP(SCM_CDR(elem))) {
            /* (rep-while m #f elem1) */
            ScmObj elem1 = SCM_CAR(elem);
            if (SCM_EQ(elem1, SCM_SYM_ANY) && !ctx->lookbehindp) {
                rc3_seq_rep(ctx, elem, SCM_INT_VALUE(m), FALSE);
                rc3_emit(ctx, RE_ANYR);
                return;
            }
            if (SCM_CHARP(elem1) && !ctx->lookbehindp) {
                ScmChar ch = SCM_CHAR_VALUE(elem1);
                rc3_seq_rep(ctx, elem, SCM_INT_VALUE(m), FALSE);
                int n = SCM_CHAR_NBYTES(ch);
                if (n == 1) {
                    rc3_emit(ctx, RE_MATCH1R);
                    rc3_emit(ctx, (char)ch);
                } else {
                    char chbuf[SCM_CHAR_MAX_BYTES];
                    SCM_CHAR_PUT(chbuf, ch);
                    rc3_emit(ctx, RE_MATCHR);
                    rc3_emit(ctx, (char)n);  /* we know it's never overflow */
                    for (int i=0; i < n; i++) rc3_emit(ctx, chbuf[i]);
                }
                return;
            }
            if (SCM_CHAR_SET_P(elem1)) {
                rc3_seq_rep(ctx, elem, SCM_INT_VALUE(m), FALSE);
                EMIT4(!SCM_CHAR_SET_SMALLP(elem1), RE_SETR, RE_SETR_RL, RE_SET1R, RE_SET1R_RL);
                rc3_emit(ctx, rc3_charset_index(rx, elem1));
                return;
            }
            if (SCM_PAIRP(elem1)&&SCM_EQ(SCM_CAR(elem1), SCM_SYM_COMP)) {
                rc3_seq_rep(ctx, elem, SCM_INT_VALUE(m), FALSE);
                ScmObj cs = SCM_CDR(elem1);
                SCM_ASSERT(SCM_CHAR_SET_P(cs));
                EMIT4(!ctx->lookbehindp, RE_NSETR, RE_NSETR_RL, RE_NSET1R, RE_NSET1R_RL);
                rc3_emit(ctx, rc3_charset_index(rx, cs));
                return;
            }
        }
        /* fallthrough to rep */
        type = SCM_SYM_REP;
    }
    if (SCM_EQ(type, SCM_SYM_ONCE) && ctx->lookbehindp) {
        /* [Rui] I couldn't make a decision about the behavior of standalone
           pattern (?>re) within a lookbehind assertion ((?<=re) or
           (?<!re)).  It raises an error for now. */
        Scm_Error("standalone pattern in lookbehind assertion is not supported: %S",
                  ctx->pattern);
    }
    if (SCM_EQ(type, SCM_SYM_ASSERT) || SCM_EQ(type, SCM_SYM_NASSERT)
        || SCM_EQ(type, SCM_SYM_ONCE)) {
        int ocodep = ctx->codep;
        int op = SCM_EQ(type, SCM_SYM_ASSERT) ? RE_ASSERT :
                 SCM_EQ(type, SCM_SYM_NASSERT) ? RE_NASSERT : RE_ONCE;
        rc3_emit(ctx, op);
        rc3_emit_offset(ctx, 0); /* will be patched */
        /* Assertions can check EOF even other regexps follow, so '$'
           in the last pos of this group should be treated as EOL.
           (?>$) as well.  It is consistent with Perl and Oniguruma. */
        rc3_seq(ctx, SCM_CDR(ast), TRUE);
        rc3_emit(ctx, RE_SUCCESS);
        rc3_fill_offset(ctx, ocodep+1, ctx->codep);
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

        if (SCM_PAIRP(SCM_CDR(ast))) {
            for (clause = SCM_CDR(ast);
                 SCM_PAIRP(SCM_CDR(clause));
                 clause = SCM_CDR(clause)) {
                rc3_emit(ctx, RE_TRY);
                patchp = ctx->codep;
                rc3_emit_offset(ctx, 0); /* will be patched */
                rc3_rec(ctx, SCM_CAR(clause), lastp);
                rc3_emit(ctx, RE_JUMP);
                if (ctx->emitp) {
                    jumps = Scm_Cons(SCM_MAKE_INT(ctx->codep), jumps);
                }
                rc3_emit_offset(ctx, 0); /* will be patched */
                rc3_fill_offset(ctx, patchp, ctx->codep);
            }
            rc3_rec(ctx, SCM_CAR(clause), lastp);
            if (ctx->emitp) {
                SCM_FOR_EACH(jumps, jumps) {
                    patchp = SCM_INT_VALUE(SCM_CAR(jumps));
                    rc3_fill_offset(ctx, patchp, ctx->codep);
                }
            }
        } else {
            /* NB: alternation without any choices won't appear from the
               parsed AST, but the caller can pass in a programatically
               constructed AST.  It fails unconditionally, since we have
               no possible choice. */
            rc3_emit(ctx, RE_FAIL);
        }
        return;
    }
    if (SCM_EQ(type, SCM_SYM_REP) || SCM_EQ(type, SCM_SYM_REP_MIN)) {
        ScmObj min = SCM_CADR(ast), max = SCM_CAR(SCM_CDDR(ast));
        ScmObj item = SCM_CDR(SCM_CDDR(ast));
        int multip = 0;

        if (SCM_FALSEP(max) || SCM_INT_VALUE(max) > 1)
            multip = TRUE;
        rc3_seq_rep(ctx, item, SCM_INT_VALUE(min), multip);

        if (SCM_EQ(min, max)) {
            /* (rep <m> <m> <x>)
                    <x>
                     : (m-1 times)
            */
            return;
        }
        if (!SCM_FALSEP(max)) {
            int count = SCM_INT_VALUE(max) - SCM_INT_VALUE(min);
            rc3_minmax(ctx, type, count, item, lastp);
            return;
        }
        if (SCM_EQ(type, SCM_SYM_REP)) {
            /* (rep <m> #f <x>)
                    <x>
                     : (m-1 times)
               rep: TRY next
                    <x>
                    JUMP rep
               next:
            */
            int ocodep = ctx->codep;
            rc3_emit(ctx, RE_TRY);
            rc3_emit_offset(ctx, 0); /* will be patched */
            rc3_seq(ctx, item, FALSE);
            rc3_emit(ctx, RE_JUMP);
            rc3_emit_offset(ctx, ocodep);
            rc3_fill_offset(ctx, ocodep+1, ctx->codep);
            return;
        }
        if (SCM_EQ(type, SCM_SYM_REP_MIN)) {
            /* (rep-min <m> #f <x>)
                    <x>
                     : (m-1 times)
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
            rc3_seq(ctx, item, FALSE);
            rc3_emit(ctx, RE_JUMP);
            rc3_emit_offset(ctx, ocodep1);
            rc3_fill_offset(ctx, ocodep2+1, ctx->codep);
            return;
        }
    }
    if (SCM_EQ(type, SCM_SYM_LOOKBEHIND)) {
        int oldval = ctx->lookbehindp;
        ctx->lookbehindp = TRUE;
        rc3_seq(ctx, SCM_CDR(ast), lastp);
        ctx->lookbehindp = oldval;
        return;
    }
    if (SCM_EQ(type, SCM_SYM_BACKREF)) {
        SCM_ASSERT(SCM_INTP(SCM_CDR(ast)));
        EMIT4(!ctx->casefoldp, RE_BACKREF, RE_BACKREF_RL, RE_BACKREF_CI, RE_BACKREF_CI_RL);
        rc3_emit(ctx, (char)SCM_INT_VALUE(SCM_CDR(ast)));
        return;
    }
    if (SCM_EQ(type, SCM_SYM_CPAT)) {
        /* (cpat <n> <yes-pattern> <no-pattern>)
                 CPAT <n> #1
                 <yes-pattern>
                 JUMP #2
           #1:   <no-pattern>
           #2:

           (cpat <assert> <yes-pattern> <no-pattern>)
                 CPATA #1 #2
                 <assert>
                 SUCCESS
           #1:   <yes-pattern>
                 JUMP #3
           #2:   <no-pattern>
           #3:
        */
        ScmObj cond = SCM_CADR(ast);
        ScmObj ypat = SCM_CAR(SCM_CDDR(ast));
        ScmObj npat = SCM_CADR(SCM_CDDR(ast));
        if (SCM_INTP(cond)) {
            rc3_emit(ctx, RE_CPAT);
            rc3_emit(ctx, (char)SCM_INT_VALUE(cond));
            int ocodep1 = ctx->codep;
            rc3_emit_offset(ctx, 0); /* will be patched */
            rc3_seq(ctx, ypat, lastp);
            rc3_emit(ctx, RE_JUMP);
            int ocodep2 = ctx->codep;
            rc3_emit_offset(ctx, 0); /* will be patched */
            rc3_fill_offset(ctx, ocodep1, ctx->codep);
            rc3_seq(ctx, npat, lastp);
            rc3_fill_offset(ctx, ocodep2, ctx->codep);
        } else {
            SCM_ASSERT(SCM_EQ(SCM_CAR(cond), SCM_SYM_ASSERT)
                       || SCM_EQ(SCM_CAR(cond), SCM_SYM_NASSERT));
            rc3_emit(ctx, RE_CPATA);
            int ocodep1 = ctx->codep;
            rc3_emit_offset(ctx, 0); /* will be patched */
            int ocodep2 = ctx->codep;
            rc3_emit_offset(ctx, 0); /* will be patched */
            rc3_rec(ctx, cond, lastp);
            rc3_emit(ctx, RE_SUCCESS);
            rc3_fill_offset(ctx, ocodep1, ctx->codep);
            rc3_seq(ctx, ypat, lastp);
            rc3_emit(ctx, RE_JUMP);
            int ocodep3 = ctx->codep;
            rc3_emit_offset(ctx, 0); /* will be patched */
            rc3_fill_offset(ctx, ocodep2, ctx->codep);
            rc3_seq(ctx, npat, lastp);
            rc3_fill_offset(ctx, ocodep3, ctx->codep);
        }
        return;
    }
    Scm_Error("internal error in regexp compilation: bad node: %S", ast);
}

static int is_bol_anchored(ScmObj ast)
{
    if (!SCM_PAIRP(ast)) {
        if (SCM_EQ(ast, SCM_SYM_BOL)) return TRUE;
        else return FALSE;
    }
    ScmObj type = SCM_CAR(ast);
    if (SCM_INTP(type)) {
        if (!SCM_PAIRP(SCM_CDDR(ast))) return FALSE;
        return is_bol_anchored(SCM_CAR(SCM_CDDR(ast)));
    } else if (SCM_EQ(type, SCM_SYM_SEQ)
               || SCM_EQ(type, SCM_SYM_SEQ_UNCASE)
               || SCM_EQ(type, SCM_SYM_SEQ_CASE)) {
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

/* Aux function for is_simple_prefixed.
   Returns TRUE if AST is <char>, <char-set>, or (comp . <char-set>)*/
static int is_char_or_charset(ScmObj ast)
{
    if (SCM_CHARP(ast) || SCM_CHAR_SET_P(ast)
        || (SCM_PAIRP(ast)
            && SCM_EQ(SCM_CAR(ast), SCM_SYM_COMP)
            && SCM_CHAR_SET_P(SCM_CDR(ast)))) {
        return TRUE;
    } else {
        return FALSE;
    }
}

/* Returns TRUE iff ast has a form #/A+B/ where A is a char or charset,
   and B begins with distinct charset from A (B may be empty).
   After optimization, the AST begins with (rep-while 1 #f A).
   If so, we can greatly optimize the failure case.
   Suppose if we try input s against #/A+B/ and find it fail.  Then
   we can skip prefix of s as far as it matches #/A/. */
static int is_simple_prefixed(ScmObj ast)
{
    if (!SCM_PAIRP(ast)) return FALSE;
    ScmObj car = SCM_CAR(ast);
    if (SCM_EQ(car, SCM_SYM_REP_WHILE)) {
        if (SCM_EQ(SCM_CADR(ast), SCM_MAKE_INT(1))
            && SCM_FALSEP(SCM_CAR(SCM_CDDR(ast)))) {
            ScmObj body = SCM_CDR(SCM_CDDR(ast));
            if (SCM_PAIRP(body) && SCM_NULLP(SCM_CDR(body))) {
                return is_char_or_charset(SCM_CAR(body));
            }
        }
        return FALSE;
    } else if (SCM_EQ(car, SCM_SYM_SEQ)) { /* TODO: handle uncase */
        if (SCM_PAIRP(SCM_CDR(ast))) {
            return is_simple_prefixed(SCM_CADR(ast));
        }
        return FALSE;
    } else if (SCM_INTP(car)) {
        ScmObj s = SCM_CDDR(ast);
        if (SCM_PAIRP(s)) return is_simple_prefixed(SCM_CAR(s));
    }
    return FALSE;
}


/* returns lookahead set.  modifies the first arg.  */
static ScmObj merge_laset(ScmObj la1, ScmObj la2)
{
    if (SCM_CHAR_SET_P(la1) && SCM_CHAR_SET_P(la2)) {
        return Scm_CharSetAdd(SCM_CHAR_SET(la1),
                              SCM_CHAR_SET(la2));
    } else {
        return SCM_FALSE;
    }
}

static ScmObj calculate_lasetn(ScmObj ast);

/* returns lookahead set.  returned charset is fresh.
   TODO: We can also take advantage of wb and nwb condition to
   skip the input. */
static ScmObj calculate_laset(ScmObj head, ScmObj rest)
{
    if (!SCM_PAIRP(head)) {
        if (SCM_CHARP(head)) {
            return Scm_CharSetAddRange(SCM_CHAR_SET(Scm_MakeEmptyCharSet()),
                                       SCM_CHAR_VALUE(head),
                                       SCM_CHAR_VALUE(head));
        } else if (SCM_CHAR_SET_P(head)) {
            return Scm_CharSetCopy(SCM_CHAR_SET(head));
        }
        return SCM_FALSE;
    }
    ScmObj head_car = SCM_CAR(head);

    if (SCM_EQ(head_car, SCM_SYM_COMP)) {
        SCM_ASSERT(SCM_CHAR_SET_P(SCM_CDR(head)));
        ScmObj cs = Scm_CharSetCopy(SCM_CHAR_SET(SCM_CDR(head)));
        return Scm_CharSetComplement(SCM_CHAR_SET(cs));
    } else if (SCM_EQ(head_car, SCM_SYM_SEQ)||SCM_EQ(head_car, SCM_SYM_ONCE)) {
        return calculate_lasetn(SCM_CDR(head));
    } else if (SCM_EQ(head_car, SCM_SYM_ALT)) {
        ScmObj choices = SCM_CDR(head);
        if (!SCM_PAIRP(choices)) return SCM_FALSE;
        ScmObj r = calculate_laset(SCM_CAR(choices), SCM_NIL);
        choices = SCM_CDR(choices);
        while (!SCM_FALSEP(r) && SCM_PAIRP(choices)) {
            r = merge_laset(r, calculate_laset(SCM_CAR(choices), SCM_NIL));
            choices = SCM_CDR(choices);
        }
        return r;
    } else if (SCM_EQ(head_car, SCM_SYM_REP)
               || SCM_EQ(head_car, SCM_SYM_REP_WHILE)
               || SCM_EQ(head_car, SCM_SYM_REP_MIN)) {
        SCM_ASSERT(SCM_PAIRP(SCM_CDR(head)) && SCM_PAIRP(SCM_CDDR(head)));
        if (SCM_EQ(SCM_CADR(head), SCM_MAKE_INT(0))) {
            return merge_laset(calculate_lasetn(SCM_CDR(SCM_CDDR(head))),
                               calculate_lasetn(rest));
        } else {
            return calculate_lasetn(SCM_CDR(SCM_CDDR(head)));
        }
    } else if (SCM_INTP(head_car)) {
        SCM_ASSERT(SCM_PAIRP(SCM_CDR(head)));
        return calculate_lasetn(SCM_CDDR(head));
    } else {
        return SCM_FALSE;
    }
}

static ScmObj calculate_lasetn(ScmObj ast)
{
    if (!SCM_PAIRP(ast)) return SCM_FALSE;
    else return calculate_laset(SCM_CAR(ast), SCM_CDR(ast));
}

/* pass 3 */
static ScmObj rc3(regcomp_ctx *ctx, ScmObj ast)
{
    /* set flags and laset */
    if (is_bol_anchored(ast)) ctx->rx->flags |= SCM_REGEXP_BOL_ANCHORED;
    else if (is_simple_prefixed(ast)) ctx->rx->flags |= SCM_REGEXP_SIMPLE_PREFIX;
    ctx->rx->laset = calculate_laset(ast, SCM_NIL);

    /* pass 3-1 : count # of insns */
    ctx->codemax = 1;
    ctx->emitp = FALSE;
    rc3_rec(ctx, ast, TRUE);

    /* pass 3-2 : code generation */
    ctx->code = SCM_NEW_ATOMIC2(unsigned char *, ctx->codemax);
    ctx->emitp = TRUE;
    rc3_rec(ctx, ast, TRUE);
    rc3_emit(ctx, RE_SUCCESS);
    ctx->rx->code = ctx->code;
    ctx->rx->numCodes = ctx->codep;

    ctx->rx->ast = ast;
    return SCM_OBJ(ctx->rx);
}

/* For debug */
void Scm_RegDump(ScmRegexp *rx)
{
    Scm_Printf(SCM_CUROUT, "Regexp %p: (flags=%08x", rx, rx->flags);
    if (rx->flags&SCM_REGEXP_BOL_ANCHORED)
        Scm_Printf(SCM_CUROUT, ",BOL_ANCHORED");
    if (rx->flags&SCM_REGEXP_SIMPLE_PREFIX)
        Scm_Printf(SCM_CUROUT, ",SIMPLE_PREFIX");
    Scm_Printf(SCM_CUROUT, ")\n");
    Scm_Printf(SCM_CUROUT, " laset = %S\n", rx->laset);
    Scm_Printf(SCM_CUROUT, "  must = ");
    if (rx->mustMatch) {
        Scm_Printf(SCM_CUROUT, "%S\n", rx->mustMatch);
    } else {
        Scm_Printf(SCM_CUROUT, "(none)\n");
    }

    int end = rx->numCodes;
    for (int codep = 0; codep < end; codep++) {
        int code = rx->code[codep];
        switch (code) {
        case RE_MATCH1:    case RE_MATCH1_CI:
        case RE_MATCH1_RL: case RE_MATCH1_CI_RL:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  %s  0x%02x  '%c'\n",
                       codep-1,
                       (code==RE_MATCH1? "MATCH1": code == RE_MATCH1_CI? "MATCH1_CI":
                        code==RE_MATCH1_RL? "MATCH1_RL" : "MATCH1_CI_RL"),
                       rx->code[codep], rx->code[codep]);
            continue;
        case RE_MATCH:    case RE_MATCH_CI:
        case RE_MATCH_RL: case RE_MATCH_CI_RL:
            codep++;
            {
                u_int numchars = (u_int)rx->code[codep];
                u_int i;
                Scm_Printf(SCM_CUROUT, "%4d  %s(%3d) '",
                           codep-1,
                           (code==RE_MATCH? "MATCH": code == RE_MATCH_CI? "MATCH_CI":
                            code==RE_MATCH_RL? "MATCH_RL" : "MATCH_CI_RL"),
                           numchars);
                for (i=0; i< numchars; i++)
                    Scm_Printf(SCM_CUROUT, "%c", rx->code[++codep]);
                Scm_Printf(SCM_CUROUT, "'\n");
            }
            continue;
        case RE_ANY: case RE_ANY_RL:
            Scm_Printf(SCM_CUROUT, "%4d  %s\n",
                       codep, (code==RE_ANY? "ANY":"ANY_RL"));
            continue;
        case RE_TRY:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  TRY  %d\n", codep-1,
                       (rx->code[codep])*256 + rx->code[codep+1]);
            codep++;
            continue;
        case RE_SET: case RE_SET_RL:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  %s  %d    %S\n",
                       codep-1, (code==RE_SET? "SET":"SET_RL"),
                       rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_NSET: case RE_NSET_RL:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  %s %d    %S\n",
                       codep-1, (code==RE_NSET? "NSET":"NSET_RL"),
                       rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_SET1: case RE_SET1_RL:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  %s %d    %S\n",
                       codep-1, (code==RE_SET1? "SET1":"SET1_RL"),
                       rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_NSET1: case RE_NSET1_RL:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  %s %d    %S\n",
                       codep-1, (code==RE_NSET1? "NSET1":"NSET1_RL"),
                       rx->code[codep],
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
        case RE_BEGIN: case RE_BEGIN_RL:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  %s %d\n", codep-1,
                       code==RE_BEGIN?"BEGIN":"BEGIN_RL",
                       rx->code[codep]);
            continue;
        case RE_END: case RE_END_RL:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  %s %d\n", codep-1,
                       code==RE_END?"END":"END_RL",
                       rx->code[codep]);
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
        case RE_SET1R: case RE_SET1R_RL:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  %s %d   %S\n",
                       codep-1, (code==RE_SET1R? "SET1R":"SET1R_RL"),
                       rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_NSET1R: case RE_NSET1R_RL:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  %s %d  %S\n",
                       codep-1, (code==RE_NSET1R? "NSET1R":"NSET1R_RL"),
                       rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_SETR: case RE_SETR_RL:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  %s %d    %S\n",
                       codep-1, (code==RE_SETR? "SETR":"SETR_RL"),
                       rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_NSETR: case RE_NSETR_RL:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  %s %d   %S\n",
                       codep-1, (code==RE_NSETR? "NSETR":"NSETR_RL"),
                       rx->code[codep],
                       rx->sets[rx->code[codep]]);
            continue;
        case RE_MATCH1R:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  %s  0x%02x  '%c'\n",
                       codep-1, "MATCH1R",
                       rx->code[codep], rx->code[codep]);
            continue;
        case RE_MATCHR:
            codep++;
            {
                u_int numchars = (u_int)rx->code[codep];
                u_int i;
                Scm_Printf(SCM_CUROUT, "%4d  %s(%3d) '",
                           codep-1, "MATCHR",
                           numchars);
                for (i=0; i< numchars; i++)
                    Scm_Printf(SCM_CUROUT, "%c", rx->code[++codep]);
                Scm_Printf(SCM_CUROUT, "'\n");
            }
            continue;
        case RE_ANYR:
            Scm_Printf(SCM_CUROUT, "%4d  %s\n",
                       codep, "ANYR");
            continue;
        case RE_CPAT:
            Scm_Printf(SCM_CUROUT, "%4d  CPAT %d %d\n",
                       codep, rx->code[codep+1],
                       rx->code[codep+2]*256 + rx->code[codep+3]);
            codep += 3;
            continue;
        case RE_CPATA:
            Scm_Printf(SCM_CUROUT, "%4d  CPATA %d %d\n",
                       codep, rx->code[codep+1]*256+rx->code[codep+2],
                       rx->code[codep+3]*256 + rx->code[codep+4]);
            codep += 4;
            continue;
        case RE_BACKREF: case RE_BACKREF_RL:
        case RE_BACKREF_CI: case RE_BACKREF_CI_RL:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  %s %d\n",
                       codep-1,
                       (code==RE_BACKREF? "BACKREF" :
                        code==RE_BACKREF_RL? "BACKREF_RL" :
                        code==RE_BACKREF_CI? "BACKREF_CI" : "BACKREF_CI_RL"),
                       rx->code[codep]);
            continue;
        case RE_ONCE:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  ONCE %d\n", codep-1,
                       (rx->code[codep])*256 + rx->code[codep+1]);
            codep++;
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

/* Helper routine to be used for compilation from AST.
   Traverses AST to reorder groups and collect charsets.
   Note that the native regcomp path doesn't use these fns.
   Only the AST provided from outside is processed. */
static ScmObj rc_setup_context_seq(regcomp_ctx *ctx, ScmObj seq);

static ScmObj rc_setup_context(regcomp_ctx *ctx, ScmObj ast)
{
    if (!SCM_PAIRP(ast)) {
        if (SCM_CHARP(ast)) return ast;
        if (SCM_CHAR_SET_P(ast)) {
            rc_register_charset(ctx, SCM_CHAR_SET(ast));
            return ast;
        }
        if (SCM_EQ(ast, SCM_SYM_BOL) || SCM_EQ(ast, SCM_SYM_EOL)
            || SCM_EQ(ast, SCM_SYM_WB) || SCM_EQ(ast, SCM_SYM_NWB)
            || SCM_EQ(ast, SCM_SYM_ANY)) {
            return ast;
        }
        goto badast;
    }
    ScmObj type = SCM_CAR(ast);
    if (SCM_INTP(type)) {
        int grpno = ctx->grpcount++;
        ScmObj prevno = type, name = SCM_CADR(ast), body = SCM_CDDR(ast);
        ScmObj rest = rc_setup_context_seq(ctx, body);
        if (SCM_SYMBOLP(name)) {
            ctx->rx->grpNames = Scm_Acons(name, SCM_MAKE_INT(grpno),
                                          ctx->rx->grpNames);
        }
        if (SCM_INT_VALUE(prevno) == grpno && SCM_EQ(body, rest)) {
            return ast;
        } else {
            return Scm_Cons(SCM_MAKE_INT(grpno), Scm_Cons(name, rest));
        }
    }
    if (SCM_EQ(type, SCM_SYM_COMP)) {
        if (!SCM_CHAR_SET_P(SCM_CDR(ast))) goto badast;
        rc_register_charset(ctx, SCM_CHAR_SET(SCM_CDR(ast)));
        return ast;
    }
    if (SCM_EQ(type, SCM_SYM_BACKREF)) {
       if (!SCM_INTP(SCM_CDR(ast))) goto badast;
       return ast;
    }
    if (SCM_EQ(type, SCM_SYM_CPAT)) {
       if (!SCM_PAIRP(SCM_CDR(ast))
           || !SCM_PAIRP(SCM_CDDR(ast))
           || !SCM_PAIRP(SCM_CDR(SCM_CDDR(ast)))
           || !SCM_NULLP(SCM_CDDR(SCM_CDDR(ast))))
           goto badast;
       ScmObj cond = SCM_CADR(ast);
       ScmObj then = SCM_CAR(SCM_CDDR(ast));
       ScmObj alt = SCM_CADR(SCM_CDDR(ast));
       if (SCM_PAIRP(cond)) {
           if (!SCM_EQ(SCM_CAR(cond), SCM_SYM_ASSERT)
               && !SCM_EQ(SCM_CAR(cond), SCM_SYM_NASSERT)) goto badast;
           cond = rc_setup_context(ctx, cond);
       } else if (!SCM_INTP(cond)) {
           goto badast;
       }
       then = rc_setup_context_seq(ctx, then);
       if (!SCM_FALSEP(alt))
           alt = rc_setup_context_seq(ctx, alt);
       if (SCM_EQ(cond, SCM_CADR(ast))
           && SCM_EQ(then, SCM_CAR(SCM_CDDR(ast)))
           && SCM_EQ(alt, SCM_CADR(SCM_CDDR(ast)))) return ast;
       else return SCM_LIST4(type, cond, then, alt);
    }
    if (SCM_EQ(type, SCM_SYM_SEQ) || SCM_EQ(type, SCM_SYM_ALT)
        || SCM_EQ(type, SCM_SYM_SEQ_UNCASE) || SCM_EQ(type, SCM_SYM_SEQ_CASE)
        || SCM_EQ(type, SCM_SYM_ONCE) || SCM_EQ(type, SCM_SYM_LOOKBEHIND)
        || SCM_EQ(type, SCM_SYM_ASSERT) || SCM_EQ(type, SCM_SYM_NASSERT)) {
        ScmObj rest = rc_setup_context_seq(ctx, SCM_CDR(ast));
        if (SCM_EQ(SCM_CDR(ast), rest)) return ast;
        else return Scm_Cons(type, rest);
    }
    if (SCM_EQ(type, SCM_SYM_REP_WHILE) || SCM_EQ(type, SCM_SYM_REP)
        || SCM_EQ(type, SCM_SYM_REP_MIN)) {
        if (!SCM_PAIRP(SCM_CDR(ast)) || !SCM_PAIRP(SCM_CDDR(ast)))
            goto badast;
        ScmObj m = SCM_CADR(ast);
        ScmObj n = SCM_CAR(SCM_CDDR(ast));
        ScmObj item = SCM_CDR(SCM_CDDR(ast));
        if (!SCM_INTP(m) || SCM_INT_VALUE(m) < 0) goto badast;
        if (!SCM_FALSEP(n) && (!SCM_INTP(n) || SCM_INT_VALUE(m) < 0))
            goto badast;
        ScmObj rest = rc_setup_context_seq(ctx, item);
        if (SCM_EQ(item, rest)) return ast;
        else return SCM_LIST4(type, m, n, rest);
    }
  badast:
    Scm_Error("invalid regexp AST: %S", ast);
    return SCM_UNDEFINED;       /* dummy */
}

static ScmObj rc_setup_context_seq(regcomp_ctx *ctx, ScmObj seq)
{
    ScmObj sp, sp2, obj = SCM_NIL, head = SCM_NIL, tail = SCM_NIL;
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
    SCM_APPEND1(head, tail, obj);
    SCM_FOR_EACH(sp2, SCM_CDR(sp2)) {
        SCM_APPEND1(head, tail, rc_setup_context(ctx, SCM_CAR(sp2)));
    }
    return head;
}

/*--------------------------------------------------------------
 * Compiler entry point
 */
ScmObj Scm_RegComp(ScmString *pattern, int flags)
{
    if (SCM_STRING_INCOMPLETE_P(pattern)) {
        Scm_Error("incomplete string is not allowed: %S", pattern);
    }

    ScmRegexp *rx = make_regexp();
    regcomp_ctx cctx;
    rc_ctx_init(&cctx, rx, pattern);
    cctx.casefoldp = flags & SCM_REGEXP_CASE_FOLD;
    rx->flags |= (flags & SCM_REGEXP_CASE_FOLD);

    /* pass 1 : parse regexp spec */
    ScmObj ast = rc1(&cctx);
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
    rc_ctx_init(&cctx, rx, NULL);

    /* prepare some context */
    if (!SCM_PAIRP(ast) || !SCM_INTP(SCM_CAR(ast))) {
        /* ensure the entire AST is in a group #0 */
        ast = SCM_LIST3(SCM_MAKE_INT(0), SCM_FALSE, ast);
    }
    ast = rc_setup_context(&cctx, ast);
    rc_setup_charsets(rx, &cctx);
    rx->numGroups = cctx.grpcount;

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

struct match_ctx {
    ScmRegexp *rx;
    const unsigned char *codehead; /* start of code */
    const char *input;          /* start of input */
    const char *stop;           /* end of input */
    const char *last;
    struct ScmRegMatchSub **matches;
    void *begin_stack;          /* C stack pointer the match began from. */
    sigjmp_buf *cont;
};

#define MAX_STACK_USAGE   0x100000

static int match_ci(const char **input, const unsigned char **code, int length)
{
    do {
        ScmChar inch, c;
        SCM_CHAR_GET(*input, inch);
        int csize = SCM_CHAR_NBYTES(inch);
        *input += csize;
        SCM_CHAR_GET(*code, c);
        *code += SCM_CHAR_NBYTES(c);
        if (Scm_CharDowncase(inch) != c)
            return FALSE;
        length -= csize;
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
    const char *prevp;

    if (input == ctx->input || input == ctx->stop) return TRUE;
    unsigned char nextb = (unsigned char)*input;
    SCM_CHAR_BACKWARD(input, ctx->input, prevp);
    SCM_ASSERT(prevp != NULL);
    unsigned char prevb = (unsigned char)*prevp;
    if ((is_word_constituent(nextb) && !is_word_constituent(prevb))
        || (!is_word_constituent(nextb) && is_word_constituent(prevb))) {
        return TRUE;
    }
    return FALSE;
}

static void rex_rec(const unsigned char *code,
                    const char *input,
                    struct match_ctx *ctx)
{
    register int param;
    register ScmChar ch;
    ScmCharSet *cset;
    const char *bpos;

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
        case RE_MATCH_RL:
            param = *code++;
            if (input - param < ctx->input) return;
            bpos = input = input - param;
            while (param-- > 0) {
                if (*code++ != (unsigned char)*bpos++) return;
            }
            continue;
        case RE_MATCH1:
            if (ctx->stop == input) return;
            if (*code++ != (unsigned char)*input++) return;
            continue;
        case RE_MATCH1_RL:
            if (ctx->input == input) return;
            if (*code++ != (unsigned char)*--input) return;
            continue;
        case RE_MATCH_CI:
            param = *code++;
            if (ctx->stop - input < param) return;
            if (!match_ci(&input, &code, param)) return;
            continue;
        case RE_MATCH_CI_RL:
            param = *code++;
            if (input - param < ctx->input) return;
            bpos = input = input - param;
            if (!match_ci(&bpos, &code, param)) return;
            continue;
        case RE_MATCH1_CI:
            if (ctx->stop == input) return;
            param  = (unsigned char)*input++;
            if (SCM_CHAR_NFOLLOWS(param)!=0
                || (*code++)!=SCM_CHAR_DOWNCASE(param)) {
                return;
            }
            continue;
        case RE_MATCH1_CI_RL:
            if (ctx->input == input) return;
            param = (unsigned char)*--input;
            if (SCM_CHAR_NFOLLOWS(param)!=0
                || (*code++)!=SCM_CHAR_DOWNCASE(param)) {
                return;
            }
            continue;
        case RE_ANY:
            if (ctx->stop == input) return;
            input += SCM_CHAR_NFOLLOWS(*input) + 1;
            continue;
        case RE_ANY_RL:
            if (ctx->input == input) return;
            SCM_CHAR_BACKWARD(input, ctx->input, bpos);
            input = bpos;
            continue;
        case RE_TRY:
            rex_rec(code+2, input, ctx);
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
        case RE_SET1_RL:
            if (ctx->input == input) return;
            SCM_CHAR_BACKWARD(input, ctx->input, bpos);
            if ((unsigned char)*bpos >= 128) return;
            if (!Scm_CharSetContains(ctx->rx->sets[*code++], *bpos)) return;
            input = bpos;
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
        case RE_NSET1_RL:
            if (ctx->input == input) return;
            SCM_CHAR_BACKWARD(input, ctx->input, bpos);
            if ((unsigned char)*bpos < 128) {
                if (Scm_CharSetContains(ctx->rx->sets[*code++], *bpos))
                    return;
            }
            input = bpos;
            continue;
        case RE_SET:
            if (ctx->stop == input) return;
            SCM_CHAR_GET(input, ch);
            cset = ctx->rx->sets[*code++];
            if (!Scm_CharSetContains(cset, ch)) return;
            input += SCM_CHAR_NBYTES(ch);
            continue;
        case RE_SET_RL:
            if (ctx->input == input) return;
            SCM_CHAR_BACKWARD(input, ctx->input, bpos);
            SCM_CHAR_GET(bpos, ch);
            cset = ctx->rx->sets[*code++];
            if (!Scm_CharSetContains(cset, ch)) return;
            input = bpos;
            continue;
        case RE_NSET:
            if (ctx->stop == input) return;
            SCM_CHAR_GET(input, ch);
            cset = ctx->rx->sets[*code++];
            if (Scm_CharSetContains(cset, ch)) return;
            input += SCM_CHAR_NBYTES(ch);
            continue;
        case RE_NSET_RL:
            if (ctx->input == input) return;
            SCM_CHAR_BACKWARD(input, ctx->input, bpos);
            SCM_CHAR_GET(bpos, ch);
            cset = ctx->rx->sets[*code++];
            if (Scm_CharSetContains(cset, ch)) return;
            input = bpos;
            continue;
        case RE_BEGIN: {
            int grpno = *code++;
            const char *opos = ctx->matches[grpno]->startp;
            const char *oend = ctx->matches[grpno]->endp;
            ctx->matches[grpno]->startp = input;
            rex_rec(code, input, ctx);
            ctx->matches[grpno]->startp = opos;
            ctx->matches[grpno]->endp = oend;
            return;
        }
        case RE_BEGIN_RL: {
            int grpno = *code++;
            const char *opos = ctx->matches[grpno]->endp;
            ctx->matches[grpno]->endp = input;
            rex_rec(code, input, ctx);
            ctx->matches[grpno]->endp = opos;
            return;
        }
        case RE_END: {
            int grpno = *code++;
            ctx->matches[grpno]->endp = input;
            continue;
        }
        case RE_END_RL: {
            int grpno = *code++;
            ctx->matches[grpno]->startp = input;
            continue;
        }
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
        case RE_SET1R_RL:
            cset = ctx->rx->sets[*code++];
            for (;;) {
                if (input == ctx->input) break;
                SCM_CHAR_BACKWARD(input, ctx->input, bpos);
                if ((unsigned char)*bpos >= 128) break;
                if (!Scm_CharSetContains(cset, *bpos)) break;
                input = bpos;
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
        case RE_NSET1R_RL:
            cset = ctx->rx->sets[*code++];
            for (;;) {
                if (ctx->input == input) break;
                SCM_CHAR_BACKWARD(input, ctx->input, bpos);
                if ((unsigned char)*bpos < 128) {
                    if (Scm_CharSetContains(cset, *bpos)) break;
                }
                input = bpos;
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
        case RE_SETR_RL:
            cset = ctx->rx->sets[*code++];
            for (;;) {
                if (ctx->input == input) break;
                SCM_CHAR_BACKWARD(input, ctx->input, bpos);
                SCM_CHAR_GET(bpos, ch);
                if (!Scm_CharSetContains(cset, ch)) break;
                input = bpos;
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
        case RE_NSETR_RL:
            cset = ctx->rx->sets[*code++];
            for (;;) {
                if (ctx->input == input) break;
                SCM_CHAR_BACKWARD(input, ctx->input, bpos);
                SCM_CHAR_GET(bpos, ch);
                if (Scm_CharSetContains(cset, ch)) break;
                input = bpos;
            }
            continue;
        case RE_MATCH1R:
            for (;;) {
                if (ctx->stop <= input) break;
                if ((unsigned char)*input >= 128) break;
                if (*code != (unsigned char)*input) break;
                input++;
            }
            code++;
            continue;
        case RE_MATCHR:
            param = *code++;
            for (;;) {
                if (ctx->stop <= input) break;
                const unsigned char *str = code;
                const unsigned char *ip = input;
                for (unsigned int i = 0; i < param; i++) {
                    if (*str++ != (unsigned char)*ip++) break;
                }
                input = ip;
            }
            code += param;
            continue;
        case RE_ANYR:
            for (;;) {
                if (ctx->stop <= input) break;
                input += SCM_CHAR_NFOLLOWS(*input) + 1;
            }
            continue;
        case RE_CPAT: {
            int grpno = *code++;
            if (ctx->matches[grpno]->startp) code += 2;
            else code = ctx->codehead + code[0]*256 + code[1];
            continue;
        }
        case RE_CPATA: {
            sigjmp_buf cont, *ocont = ctx->cont;
            ctx->cont = &cont;
            if (sigsetjmp(cont, FALSE) == 0) {
                rex_rec(code+4, input, ctx);
                ctx->cont = ocont;
                code = ctx->codehead + code[2]*256 + code[3];
                continue;
            }
            code = ctx->codehead + code[0]*256 + code[1];
            ctx->cont = ocont;
            continue;
        }
        case RE_BACKREF: {
            int grpno = *code++;
            const char *match = ctx->matches[grpno]->startp;
            const char *end = ctx->matches[grpno]->endp;
            if (!match || !end) return;
            while (match < end) {
                if (*input++ != *match++) return;
            }
            continue;
        }
        case RE_BACKREF_RL: {
            int grpno = *code++, len;
            const char *match = ctx->matches[grpno]->startp;
            const char *end = ctx->matches[grpno]->endp;
            if (!match || !end) return;
            len = (int)(end - match);
            if (input - len < ctx->input) return;
            bpos = input = input - len;
            while (len-- > 0) {
                if (*match++ != (unsigned char)*bpos++) return;
            }
            continue;
        }
        case RE_BACKREF_CI: {
            int grpno = *code++;
            const char *match = ctx->matches[grpno]->startp;
            const char *end = ctx->matches[grpno]->endp;
            int i = 0;
            ScmChar cx, cy;
            if (!match || !end) return;
            while (match+i < end) {
                if (input == ctx->stop) return;
                SCM_CHAR_GET(input+i, cx);
                SCM_CHAR_GET(match+i, cy);
                if (SCM_CHAR_UPCASE(cx) != SCM_CHAR_UPCASE(cy))
                    return;
                i += SCM_CHAR_NBYTES(cx);
            }
            input += i;
            continue;
        }
        case RE_BACKREF_CI_RL: {
            int grpno = *code++, i = 0, len;
            const char *match = ctx->matches[grpno]->startp;
            const char *end = ctx->matches[grpno]->endp;
            ScmChar cx, cy;
            if (!match || !end) return;

            len = (int)(end - match);
            if (input - len < ctx->input) return;
            bpos = input = input - len;
            while (match+i < end) {
                if (bpos == ctx->stop) return;
                SCM_CHAR_GET(bpos+i, cx);
                SCM_CHAR_GET(match+i, cy);
                if (SCM_CHAR_UPCASE(cx) != SCM_CHAR_UPCASE(cy))
                    return;
                i += SCM_CHAR_NBYTES(cx);
            }
            continue;
        }
        case RE_ONCE: case RE_ASSERT: {
            sigjmp_buf cont, *ocont = ctx->cont;
            ctx->cont = &cont;
            if (sigsetjmp(cont, FALSE) == 0) {
                rex_rec(code+2, input, ctx);
                ctx->cont = ocont;
                return;
            }
            if (code[-1] == RE_ONCE) input = ctx->last;
            code = ctx->codehead + code[0]*256 + code[1];
            ctx->cont = ocont;
            continue;
        }
        case RE_NASSERT: {
            sigjmp_buf cont, *ocont = ctx->cont;
            ctx->cont = &cont;
            if (sigsetjmp(cont, FALSE) == 0) {
                rex_rec(code+2, input, ctx);
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
    ScmRegMatch *rm = SCM_NEW(ScmRegMatch);
    SCM_SET_CLASS(rm, SCM_CLASS_REGMATCH);
    rm->numMatches = rx->numGroups;
    rm->grpNames = rx->grpNames;
    /* we keep information of original string separately, instead of
       keeping a pointer to orig; For orig may be destructively modified,
       but its elements are not. */
    const ScmStringBody *origb = SCM_STRING_BODY(orig);
    rm->input = SCM_STRING_BODY_START(origb);
    rm->inputLen = SCM_STRING_BODY_LENGTH(origb);
    rm->inputSize = SCM_STRING_BODY_SIZE(origb);
    rm->matches = ctx->matches;
    return SCM_OBJ(rm);
}

static ScmObj rex(ScmRegexp *rx, ScmString *orig,
                  const char *start, const char *end)
{
    struct match_ctx ctx;
    sigjmp_buf cont;

    ctx.rx = rx;
    ctx.codehead = rx->code;
    ctx.input = SCM_STRING_BODY_START(SCM_STRING_BODY(orig));
    ctx.stop = end;
    ctx.begin_stack = (void*)&ctx;
    ctx.cont = &cont;
    ctx.matches = SCM_NEW_ARRAY(struct ScmRegMatchSub *, rx->numGroups);

    for (int i = 0; i < rx->numGroups; i++) {
        ctx.matches[i] = SCM_NEW(struct ScmRegMatchSub);
        ctx.matches[i]->start = -1;
        ctx.matches[i]->length = -1;
        ctx.matches[i]->after = -1;
        ctx.matches[i]->startp = NULL;
        ctx.matches[i]->endp = NULL;
    }

    if (sigsetjmp(cont, FALSE) == 0) {
        rex_rec(ctx.codehead, start, &ctx);
        return SCM_FALSE;
    }
    return make_match(rx, orig, &ctx);
}

/* advance start pointer while the character matches (skip_match=TRUE) or does
   not match (skip_match=FALSE), until start pointer hits limit. */
static inline const char *skip_input(const char *start, const char *limit,
                                     ScmObj laset, int skip_match)
{
    while (start <= limit) {
        ScmChar ch;
        SCM_CHAR_GET(start, ch);
        if (Scm_CharSetContains(SCM_CHAR_SET(laset), ch)) {
            if (!skip_match) return start;
        } else {
            if (skip_match) return start;
        }
        start += SCM_CHAR_NFOLLOWS(*start)+1;
    }
    return limit;
}

/*----------------------------------------------------------------------
 * entry point
 */
ScmObj Scm_RegExec(ScmRegexp *rx, ScmString *str)
{
    const ScmStringBody *b = SCM_STRING_BODY(str);
    const char *start = SCM_STRING_BODY_START(b);
    const char *end = start + SCM_STRING_BODY_SIZE(b);
    const ScmStringBody *mb = rx->mustMatch? SCM_STRING_BODY(rx->mustMatch) : NULL;
    int mustMatchLen = mb? SCM_STRING_BODY_SIZE(mb) : 0;
    const char *start_limit = end - mustMatchLen;

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

    /* if we have lookahead-set, we may be able to skip input efficiently. */
    if (!SCM_FALSEP(rx->laset)) {
        if (rx->flags & SCM_REGEXP_SIMPLE_PREFIX) {
            while (start <= start_limit) {
                ScmObj r = rex(rx, str, start, end);
                if (!SCM_FALSEP(r)) return r;
                const char *next = skip_input(start, start_limit, rx->laset,
                                              TRUE);
                if (start != next) start = next;
                else start = next + SCM_CHAR_NFOLLOWS(*start) + 1;
            }
        } else {
            while (start <= start_limit) {
                start = skip_input(start, start_limit, rx->laset, FALSE);
                ScmObj r = rex(rx, str, start, end);
                if (!SCM_FALSEP(r)) return r;
                start += SCM_CHAR_NFOLLOWS(*start)+1;
            }
        }
        return SCM_FALSE;
    }

    /* normal matching */
    while (start <= start_limit) {
        ScmObj r = rex(rx, str, start, end);
        if (!SCM_FALSEP(r)) return r;
        start += SCM_CHAR_NFOLLOWS(*start)+1;
    }
    return SCM_FALSE;
}

/*=======================================================================
 * Retrieving matches
 */

/* We calculate string length and position (in characters) lazily.
 * The match routine only sets ScmRegMatchSub's startp and endp
 * fields, and leaves start, length, and after fields -1.
 * When the submatch is retrieved we calculate values of those fields.
 *
 * Note that, even such retrieval functions mutate the state of
 * submatch objects, we don't need mutex to avoid race condition
 * in MT environment.  The state transition is one way (-1 to a
 * fixed value) and idempotent, so there's no problem if more than
 * one thread try to change the fields.
 *
 * The three parameters, start, length, and after, indicates the
 * # of characters.  Character counting is expensive, so we try
 * to avoid calling Scm_MBLen as much as possible.   If other two
 * values are known, we just subtract them from the inputLen.
 *
 * |<-------original string------------>|
 * |      |<---matched substr -->|      |
 * |      |                      |      |
 * |<---->|<-------------------->|<---->|
 * |start          length          after|
 * |<---------------------------------->|
 *               inputLen
 */

/* We want to avoid unnecessary character counting as much as
   possible. */

#define MSUB_BEFORE_SIZE(rm, sub) ((int)((sub)->startp - (rm)->input))
#define MSUB_SIZE(rm, sub)        ((int)((sub)->endp - (sub)->startp))
#define MSUB_AFTER_SIZE(rm, sub)  ((int)((rm)->input + (rm)->inputSize - (sub)->endp))

#define MSUB_BEFORE_LENGTH(rm, sub) \
    Scm_MBLen((rm)->input, (sub)->startp)
#define MSUB_LENGTH(rm, sub) \
    Scm_MBLen((sub)->startp, (sub)->endp)
#define MSUB_AFTER_LENGTH(rm, sub) \
    Scm_MBLen((sub)->endp, (rm)->input + (rm)->inputSize)

#define UNCOUNTED(rm, sub)                                      \
    (((sub)->start    >= 0 ? 0 : MSUB_BEFORE_SIZE(rm, sub))     \
     + ((sub)->length >= 0 ? 0 : MSUB_SIZE(rm, sub))            \
     + ((sub)->after  >= 0 ? 0 : MSUB_AFTER_SIZE(rm, sub)))

static void regmatch_count_start(ScmRegMatch *rm,
                                 struct ScmRegMatchSub *sub)
{
    if (SCM_REG_MATCH_SINGLE_BYTE_P(rm)) {
        sub->start = MSUB_BEFORE_SIZE(rm, sub);
    } else if (UNCOUNTED(rm, sub) / 2 > MSUB_BEFORE_SIZE(rm, sub)) {
        sub->start = MSUB_BEFORE_LENGTH(rm, sub);
    } else {
        if (sub->length < 0) sub->length = MSUB_LENGTH(rm, sub);
        if (sub->after < 0)  sub->after  = MSUB_AFTER_LENGTH(rm, sub);
        sub->start = rm->inputLen - sub->after - sub->length;
    }
}

static void regmatch_count_length(ScmRegMatch *rm,
                                  struct ScmRegMatchSub *sub)
{
    if (SCM_REG_MATCH_SINGLE_BYTE_P(rm)) {
        sub->length = MSUB_SIZE(rm, sub);
    } else if (UNCOUNTED(rm, sub) / 2 > MSUB_SIZE(rm, sub)) {
        sub->length = MSUB_LENGTH(rm, sub);
    } else {
        if (sub->start < 0) sub->start = MSUB_BEFORE_LENGTH(rm, sub);
        if (sub->after < 0) sub->after = MSUB_AFTER_LENGTH(rm, sub);
        sub->length = rm->inputLen - sub->start - sub->after;
    }
}

static void regmatch_count_after(ScmRegMatch *rm,
                                 struct ScmRegMatchSub *sub)
{
    if (SCM_REG_MATCH_SINGLE_BYTE_P(rm)) {
        sub->after = MSUB_AFTER_SIZE(rm, sub);
    } else if (UNCOUNTED(rm, sub) / 2 > MSUB_AFTER_SIZE(rm, sub)) {
        sub->after = MSUB_AFTER_LENGTH(rm, sub);
    } else {
        if (sub->start < 0)  sub->start  = MSUB_BEFORE_LENGTH(rm, sub);
        if (sub->length < 0) sub->length = MSUB_LENGTH(rm, sub);
        sub->after = rm->inputLen - sub->start - sub->length;
    }
}

static struct ScmRegMatchSub *regmatch_ref(ScmRegMatch *rm, ScmObj obj)
{
    struct ScmRegMatchSub *sub = NULL;
    if (SCM_INTP(obj)) {
        int i = SCM_INT_VALUE(obj);
        if (i < 0 || i >= rm->numMatches)
            Scm_Error("submatch index out of range: %d", i);
        sub = rm->matches[i];
        if (!sub->startp || !sub->endp) return NULL;
        return sub;
    }
    if (SCM_SYMBOLP(obj)) {
        ScmObj ep;
        SCM_FOR_EACH(ep, rm->grpNames) {
            if (!SCM_EQ(obj, SCM_CAAR(ep))) continue;
            sub = rm->matches[SCM_INT_VALUE(SCM_CDAR(ep))];
            if (!sub->startp || !sub->endp) continue;
            return sub;
        }
        if (sub != NULL) {
            if (sub->startp && sub->endp) return sub;
            else return NULL;
        }
        Scm_Error("named submatch not found: %S", obj);
    }
    Scm_Error("integer or symbol expected, but got %S", obj);
    return NULL;       /* dummy */
}

ScmObj Scm_RegMatchStart(ScmRegMatch *rm, ScmObj obj)
{
    struct ScmRegMatchSub *sub = regmatch_ref(rm, obj);
    if (sub == NULL) return SCM_FALSE;
    if (sub->start < 0) regmatch_count_start(rm, sub);
    return Scm_MakeInteger(sub->start);
}

ScmObj Scm_RegMatchEnd(ScmRegMatch *rm, ScmObj obj)
{
    struct ScmRegMatchSub *sub = regmatch_ref(rm, obj);
    if (sub == NULL) return SCM_FALSE;
    if (sub->after < 0) regmatch_count_after(rm, sub);
    return Scm_MakeInteger(rm->inputLen - sub->after);
}

ScmObj Scm_RegMatchBefore(ScmRegMatch *rm, ScmObj obj)
{
    struct ScmRegMatchSub *sub = regmatch_ref(rm, obj);
    if (sub == NULL) return SCM_FALSE;
    if (sub->start < 0) regmatch_count_start(rm, sub);
    return Scm_MakeString(rm->input, MSUB_BEFORE_SIZE(rm, sub),
                          sub->start, 0);
}

ScmObj Scm_RegMatchSubstr(ScmRegMatch *rm, ScmObj obj)
{
    struct ScmRegMatchSub *sub = regmatch_ref(rm, obj);
    if (sub == NULL) return SCM_FALSE;
    if (sub->length < 0) regmatch_count_length(rm, sub);
    return Scm_MakeString(sub->startp, MSUB_SIZE(rm, sub),
                          sub->length, 0);
}

ScmObj Scm_RegMatchAfter(ScmRegMatch *rm, ScmObj obj)
{
    struct ScmRegMatchSub *sub = regmatch_ref(rm, obj);
    if (sub == NULL) return SCM_FALSE;
    if (sub->after < 0) regmatch_count_after(rm, sub);
    return Scm_MakeString(sub->endp, MSUB_AFTER_SIZE(rm, sub),
                          sub->after, 0);
}

/* for debug */
void Scm_RegMatchDump(ScmRegMatch *rm)
{
    Scm_Printf(SCM_CUROUT, "RegMatch %p\n", rm);
    Scm_Printf(SCM_CUROUT, "  numMatches = %d\n", rm->numMatches);
    Scm_Printf(SCM_CUROUT, "  input = %S\n", rm->input);
    for (int i=0; i<rm->numMatches; i++) {
        struct ScmRegMatchSub *sub = rm->matches[i];
        if (sub->startp) {
            Scm_Printf(SCM_CUROUT, "[%3d-%3d]  %S\n",
                       sub->startp - rm->input,
                       sub->endp - rm->input,
                       Scm_MakeString(sub->startp,
                                      (int)(sub->endp-sub->startp),
                                      -1, 0));
        } else {
            Scm_Printf(SCM_CUROUT, "[---] #f\n");
        }
    }
}

/*=======================================================================
 * Initializing stuff
 */

void Scm__InitRegexp(void)
{
}
