/*
 * regexp.c - regular expression
 *
 *  Copyright(C) 2000-2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: regexp.c,v 1.38 2003-02-28 01:07:07 shirok Exp $
 */

#include <setjmp.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"

static void regexp_print(ScmObj obj, ScmPort *port, ScmWriteContext *c);

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_RegexpClass, regexp_print);
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_RegMatchClass, NULL);

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
    RE_NUM_INSN
};

/* symbols used internally */
ScmObj sym_alt;                 /* alt */
ScmObj sym_rep;                 /* rep */
ScmObj sym_any;                 /* any */
ScmObj sym_bol;                 /* bol */
ScmObj sym_eol;                 /* eol */
ScmObj sym_comp;                /* complement charset */

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

#define CASEFOLDP(rx)   ((rx)->flags&SCM_REGEXP_CASE_FOLD)

#ifndef CHAR_MAX
#define CHAR_MAX 256
#endif

/*=======================================================================
 * Compiler
 */

/* Two-and-half-pass compiler.
 *
 *  pass 1: parse pattern, generating parse tree and also counting sizes
 *          of necessary storage.
 *  pass 2: byte code generation.
 */

/* compiler state information */
struct comp_ctx {
    ScmString *pattern;         /* original pattern */
    ScmPort *ipat;              /* [pass1] string port for pattern */
    ScmObj sets;                /* [pass1] list of charsets */
    char *code;                 /* [pass2] code being built */
    int codep;                  /* [pass2] front of code generation */
    int codemax;                /* [pass2] max codep */
};

static ScmObj re_compile_charset(ScmRegexp *rx, struct comp_ctx *ctx);
static void re_compile_register_charset(struct comp_ctx *ctx, ScmCharSet *cs);

/* Util function in pass1.  look back the parser tree to find out
   the last branch of the parse tree. */
static ScmObj last_item(struct comp_ctx *ctx, ScmObj head, ScmObj tail,
                        ScmObj gstack, ScmChar ch)
{
    ScmObj last = SCM_CAR(tail);
    if (SCM_INTP(last)) {
        int gnum = SCM_INT_VALUE(last);
        if (gnum < 0) {
            /* just after close parenthesis.  There may be the case that
               we're also at the end of implicit grouping (by '|') so
               let's check it first. */
            if (ch == '|') {
                return SCM_CDAR(gstack);
            } else {
                /* find the begining of the group. */
                ScmObj cp, gstart = SCM_MAKE_INT(-gnum);
                SCM_FOR_EACH(cp, head) {
                    if (SCM_CAR(cp) == gstart) return cp;
                }
                Scm_Error("something broken internally.");
            }
        } else {
            /* just after open parenthesis or beginning. */
            if (ch == '|') {
                /* insert a placeholder */
                SCM_SET_CDR(tail, Scm_Cons(SCM_FALSE, SCM_NIL));
                return SCM_CDR(tail);
            } else {
                Scm_Error("bad regexp pattern: %S", ctx->pattern);
            }
        }
    } else {
        if (ch == '|') {
            /* '(foo|' : returns the beginning of the current group */
            return SCM_CDAR(gstack);
        } else {
            /* '(foo*' : returns the last char */
            return tail;
        }
    }
    return SCM_NIL; /* dummy */
}

/* Util function in pass1.  Fold the last branch into alternative subtree
   if necessary. */
static ScmObj fold_alternatives(ScmObj head, ScmObj tail, ScmObj grpnum)
{
    ScmObj cp;
    SCM_FOR_EACH(cp, head) {
        if (SCM_CAR(cp) == grpnum) {
            if (SCM_PAIRP(SCM_CDR(cp)) && SCM_PAIRP(SCM_CADR(cp))) {
                ScmObj clause = SCM_CADR(cp);
                if (SCM_CAR(clause) == sym_alt) {
                    SCM_SET_CDR(Scm_LastPair(clause),
                                SCM_LIST1(SCM_CDDR(cp)));
                    SCM_SET_CDR(SCM_CDR(cp), SCM_NIL);
                    return SCM_CDR(cp);
                }
            }
            break;
        }
    }
    return tail;
}

/* Are we at the place where '^' can be a BOL assertion? */
static int can_be_bol(ScmObj head)
{
    ScmObj cp;
    if (SCM_NULLP(head)) return TRUE;
    SCM_FOR_EACH(cp, head) {
        if (SCM_INTP(SCM_CAR(cp))) continue; /* group */
        if (SCM_PAIRP(SCM_CAR(cp)) && SCM_CAAR(cp) == sym_alt) continue;
        return FALSE;
    }
    return TRUE;
}

/* Read \x, \u, \U escape sequence in the regexp spec. */
static ScmChar read_regexp_xdigits(ScmPort *port, int ndigs, int key)
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

/*----------------------------------------------------------------
 * pass1 - parser
 */

ScmObj re_compile_pass1(ScmRegexp *rx, struct comp_ctx *ctx)
{
    ScmObj head = SCM_NIL, tail = SCM_NIL, elt, cell, cs;
    ScmObj grpstack;            /* group stack. */
    ScmChar ch = 0;
    int grpcount = 0;
    int open_alternative = FALSE;

    /* default group == entire match*/
    SCM_APPEND1(head, tail, SCM_MAKE_INT(0));
    grpstack = Scm_Cons(tail, SCM_NIL);
    
    for (;;) {
        ch = Scm_GetcUnsafe(ctx->ipat);
        if (ch == SCM_CHAR_INVALID) break;

        switch (ch) {
        case '(':
            grpcount++;
            SCM_APPEND1(head, tail, SCM_MAKE_INT(grpcount));
            grpstack = Scm_Cons(tail, grpstack);
            break;
        case ')':
            if (SCM_NULLP(SCM_CDR(grpstack))) {
                Scm_Error("extra close parenthesis in regexp: %S",
                          ctx->pattern);
            } else {
                ScmObj gnum = SCM_CAAR(grpstack);
                int g = SCM_INT_VALUE(gnum);
                tail = fold_alternatives(head, tail, gnum);
                SCM_APPEND1(head, tail, SCM_MAKE_INT(-g));
                grpstack = SCM_CDR(grpstack);
            }
            break;
        case '|':
            elt = last_item(ctx, head, tail, grpstack, ch);
            if (SCM_PAIRP(SCM_CAR(elt))
                && SCM_CAAR(elt) == sym_alt) {
                tail = fold_alternatives(head, tail, SCM_CAAR(grpstack));
            } else {
                cell = Scm_Cons(SCM_CAR(elt), SCM_CDR(elt));
                SCM_SET_CAR(elt, SCM_LIST2(sym_alt, cell));
                SCM_SET_CDR(elt, SCM_NIL);
                tail = elt;
                if (SCM_CAAR(grpstack) == SCM_MAKE_INT(0)) {
                    open_alternative = TRUE;
                }
            }
            break;
        case '+':  /* x+ === xx* */
            elt = Scm_CopyList(last_item(ctx, head, tail, grpstack, ch));
            SCM_APPEND1(head, tail, Scm_Cons(sym_rep, elt));
            break;
        case '?':  /* x? === (x|) */
            elt = last_item(ctx, head, tail, grpstack, ch);
            cell = Scm_Cons(SCM_CAR(elt), SCM_CDR(elt));
            SCM_SET_CAR(elt, SCM_LIST3(sym_alt, cell, SCM_NIL));
            SCM_SET_CDR(elt, SCM_NIL);
            tail = elt;
            break;
        case '*':
            elt = last_item(ctx, head, tail, grpstack, ch);
            cell = Scm_Cons(SCM_CAR(elt), SCM_CDR(elt));
            SCM_SET_CAR(elt, Scm_Cons(sym_rep, cell));
            SCM_SET_CDR(elt, SCM_NIL);
            tail = elt;
            break;
        case '.':
            SCM_APPEND1(head, tail, sym_any);
            break;
        case '[':
            SCM_APPEND1(head, tail, re_compile_charset(rx, ctx));
            break;
        case '^':
            if (can_be_bol(head)) {
                SCM_APPEND1(head, tail, sym_bol);
                break;
            } else {
                goto ordchar;
            }
        case '$':
            /* This can potentially be an EOL (we don't know for sure
               until the entire expression is parsed).  For now, we mark
               it as sym_eol.  Pass 2 determines it is actually an EOL
               marker or just a '$' char. */
            SCM_APPEND1(head, tail, sym_eol);
            break;
        case '\\':
            /* TODO: handle special escape sequences */
            ch = Scm_GetcUnsafe(ctx->ipat);
            if (ch == SCM_CHAR_INVALID) {
                Scm_Error("stray backslash at the end of pattern: %S\n",
                          ctx->pattern);
            }
            switch (ch) {
            case 'a': SCM_APPEND1(head, tail, SCM_MAKE_CHAR(0x07)); break;
            case 'n': SCM_APPEND1(head, tail, SCM_MAKE_CHAR('\n')); break;
            case 'r': SCM_APPEND1(head, tail, SCM_MAKE_CHAR('\r')); break;
            case 't': SCM_APPEND1(head, tail, SCM_MAKE_CHAR('\t')); break;
            case 'f': SCM_APPEND1(head, tail, SCM_MAKE_CHAR('\f')); break;
            case 'e': SCM_APPEND1(head, tail, SCM_MAKE_CHAR(0x1b)); break;
            case 'x':
                ch = read_regexp_xdigits(ctx->ipat, 2, 'x');
                SCM_APPEND1(head, tail, SCM_MAKE_CHAR(ch));
                break;
            case 'u':
                ch = read_regexp_xdigits(ctx->ipat, 4, 'u');
                SCM_APPEND1(head, tail, SCM_MAKE_CHAR(Scm_UcsToChar(ch)));
                break;
            case 'U':
                ch = read_regexp_xdigits(ctx->ipat, 8, 'U');
                SCM_APPEND1(head, tail, SCM_MAKE_CHAR(Scm_UcsToChar(ch)));
                break;
            case 'd':
                cs = Scm_GetStandardCharSet(SCM_CHARSET_DIGIT);
                SCM_APPEND1(head, tail, cs);
                re_compile_register_charset(ctx, SCM_CHARSET(cs));
                break;
            case 'D':
                cs = Scm_GetStandardCharSet(SCM_CHARSET_DIGIT);
                SCM_APPEND1(head, tail, Scm_Cons(sym_comp, cs));
                re_compile_register_charset(ctx, SCM_CHARSET(cs));
                break;
            case 'w':
                cs = Scm_GetStandardCharSet(SCM_CHARSET_ALNUM);
                SCM_APPEND1(head, tail, cs);
                re_compile_register_charset(ctx, SCM_CHARSET(cs));
                break;
            case 'W':
                cs = Scm_GetStandardCharSet(SCM_CHARSET_ALNUM);
                SCM_APPEND1(head, tail, Scm_Cons(sym_comp, cs));
                re_compile_register_charset(ctx, SCM_CHARSET(cs));
                break;
            case 's':
                cs = Scm_GetStandardCharSet(SCM_CHARSET_SPACE);
                SCM_APPEND1(head, tail, cs);
                re_compile_register_charset(ctx, SCM_CHARSET(cs));
                break;
            case 'S':
                cs = Scm_GetStandardCharSet(SCM_CHARSET_SPACE);
                SCM_APPEND1(head, tail, Scm_Cons(sym_comp, cs));
                re_compile_register_charset(ctx, SCM_CHARSET(cs));
                break;
            default:
                goto ordchar;
            }
            break;
        default:
        ordchar:
            if (CASEFOLDP(rx)) ch = SCM_CHAR_DOWNCASE(ch);
            SCM_APPEND1(head, tail, SCM_MAKE_CHAR(ch));
            continue;
        }
    }

    if (open_alternative) tail = fold_alternatives(head, tail, SCM_MAKE_INT(0));
    SCM_ASSERT(SCM_PAIRP(grpstack));
    if (!SCM_NULLP(SCM_CDR(grpstack)))
        Scm_Error("extra open parenthesis in regexp: %S", ctx->pattern);

    rx->numGroups = grpcount+1;
    rx->numCodes = 0;
    return head;
}

/* character range */
static ScmObj re_compile_charset(ScmRegexp *rx, struct comp_ctx *ctx)
{
    int complement;
    ScmObj set = Scm_CharSetRead(ctx->ipat, &complement, FALSE, TRUE);
    if (!SCM_CHARSETP(set)) {
        Scm_Error("bad charset spec in pattern: %S", ctx->pattern);
    }
    if (CASEFOLDP(rx)) {
        Scm_CharSetCaseFold(SCM_CHARSET(set));
    }
    
    re_compile_register_charset(ctx, SCM_CHARSET(set));
    if (complement) {
        return Scm_Cons(sym_comp, SCM_OBJ(set));
    } else {
        return SCM_OBJ(set);
    }
}

/* An interlude between pass1 and pass2.  From the information of
 * parser context, build a charset vector.
 */
static void re_compile_register_charset(struct comp_ctx *ctx, ScmCharSet *cs)
{
    if (SCM_FALSEP(Scm_Memq(SCM_OBJ(cs), ctx->sets))) {
        ctx->sets = Scm_Cons(SCM_OBJ(cs), ctx->sets);
    }
}

static void re_compile_setup_charsets(ScmRegexp *rx, struct comp_ctx *ctx)
{
    ScmObj cp;
    int i = 0;
    rx->numSets = Scm_Length(ctx->sets);
    rx->sets = SCM_NEW2(ScmCharSet**, sizeof(ScmCharSet*)*rx->numSets);
    for (i=0, cp = Scm_Reverse(ctx->sets); !SCM_NULLP(cp); cp = SCM_CDR(cp)) {
        rx->sets[i++] = SCM_CHARSET(SCM_CAR(cp));
    }
}

/* Util function for pass2, to get an index of the charset vector
 * for the given charset.
 */
static int re_compile_charset_index(ScmRegexp *rx, ScmObj cs)
{
    int i;
    for (i=0; i<rx->numSets; i++)
        if (cs == SCM_OBJ(rx->sets[i])) return i;
    Scm_Panic("re_compile_charset_index: can't be here");
    return 0;                   /* dummy */
}

static inline void re_compile_emit(struct comp_ctx *ctx, char code, int emitp)
{
    if (emitp) {
        SCM_ASSERT(ctx->codep < ctx->codemax);
        ctx->code[ctx->codep++] = code;
    } else {
        ctx->codemax++;
    }
}

/* check to see if we're at the tail of the tree */
static int can_be_eol(ScmObj cp)
{
    SCM_FOR_EACH(cp, cp) {
        /* only "end group" can appear */
        if (!SCM_INTP(SCM_CAR(cp))) return FALSE;
    }
    return TRUE;
}

/*-------------------------------------------------------------
 * pass 2 - code generation
 *          This pass actually called twice; the first run counts
 *          the size of the bytecode, and the second run fills
 *          the bytecode.   EMITP == FALSE for the first, EMITP == TRUE
 *          for the second.
 *          LASTP indicates this call is dealing with the last part of
 *          the compiled tree, thus need to deal with EOL marker.
 */
static void re_compile_pass2(ScmObj compiled, ScmRegexp *rx,
                             struct comp_ctx *ctx,
                             int lastp, int emitp, int toplevelp)
{
    ScmObj cp, item;
    ScmChar ch;
    char chbuf[SCM_CHAR_MAX_BYTES];
    int longest_size = 0, longest_idx = 0;

    SCM_FOR_EACH(cp, compiled) {
        item = SCM_CAR(cp);

        /* literal characters */
        if (SCM_CHARP(item)) {
            int nrun = 0, ocodep = ctx->codep, nb, i;

            /* find out the longest run of bytes */
            re_compile_emit(ctx, (CASEFOLDP(rx)? RE_MATCH_CI:RE_MATCH), emitp);
            re_compile_emit(ctx, 0, emitp); /* patched later */
            do {
                ch = SCM_CHAR_VALUE(item);
                nb = SCM_CHAR_NBYTES(ch);
                SCM_CHAR_PUT(chbuf, SCM_CHAR_VALUE(item));
                for (i=0; i<nb; i++) re_compile_emit(ctx, chbuf[i], emitp);
                nrun += nb;
                cp = SCM_CDR(cp);
                if (SCM_NULLP(cp)) break;
                item = SCM_CAR(cp);
            } while (SCM_CHARP(item) && nrun < CHAR_MAX);
            if (emitp) {
                /* patches the jump offset.  if we are matching to a
                   single byte char, use MATCH1 insn. */
                if (nrun == 1) {
                    ctx->code[ocodep] = CASEFOLDP(rx)?RE_MATCH1_CI:RE_MATCH1;
                    ctx->code[ocodep+1] = ctx->code[ocodep+2];
                    ctx->codep = ocodep+2;
                } else {
                    ctx->code[ocodep+1] = (char)nrun;
                    if (toplevelp && nrun > longest_size) {
                        longest_idx = ocodep+2;
                        longest_size = nrun;
                    }
                }
            }
            if (SCM_NULLP(cp)) break;
            cp = Scm_Cons(item, cp); /* pushback */
            continue;
        }

        /* group start/end */
        if (SCM_INTP(item)) {
            int grpnum = SCM_INT_VALUE(item);
            if (grpnum < 0) {
                re_compile_emit(ctx, RE_END, emitp);
                re_compile_emit(ctx, -grpnum, emitp);
            } else {
                re_compile_emit(ctx, RE_BEGIN, emitp);
                re_compile_emit(ctx, grpnum, emitp);
            }
            continue;
        }

        /* charset */
        if (SCM_CHARSETP(item)) {
            if (SCM_CHARSET_SMALLP(item)) {
                re_compile_emit(ctx, RE_SET1, emitp);
            } else {
                re_compile_emit(ctx, RE_SET, emitp);
            }
            re_compile_emit(ctx, re_compile_charset_index(rx, item), emitp);
            continue;
        }

        /* special stuff */
        if (SCM_SYMBOLP(item)) {
            if (item == sym_any) {
                re_compile_emit(ctx, RE_ANY, emitp);
                continue;
            }
            if (item == sym_bol) {
                re_compile_emit(ctx, RE_BOL, emitp);
                continue;
            }
            if (item == sym_eol) {
                if (lastp && can_be_eol(SCM_CDR(cp))) {
                    re_compile_emit(ctx, RE_EOL, emitp);
                } else {
                    re_compile_emit(ctx, RE_MATCH1, emitp);
                    re_compile_emit(ctx, '$', emitp);
                }
                continue;
            }
            /* fallback to error */
        }

        if (SCM_PAIRP(item)) {
            ScmObj car = SCM_CAR(item);
            int ocodep;

            if (car == sym_rep) {
                ocodep = ctx->codep;
                re_compile_emit(ctx, RE_TRY, emitp);
                re_compile_emit(ctx, 0, emitp); /* will be patched */
                re_compile_emit(ctx, 0, emitp); /* will be patched */
                re_compile_pass2(SCM_CDR(item), rx, ctx, FALSE, emitp, FALSE);
                re_compile_emit(ctx, RE_JUMP, emitp);
                re_compile_emit(ctx, (ocodep>>8), emitp);
                re_compile_emit(ctx, (ocodep&0xff), emitp);
                if (emitp) {
                    ctx->code[ocodep+1] = (ctx->codep>>8);
                    ctx->code[ocodep+2] = (ctx->codep&0xff);
                }
                continue;
            }
            if (car == sym_alt) {
                ScmObj clause;
                ScmObj jumps = SCM_NIL;
                int patchp;

                for (clause = SCM_CDR(item);
                     SCM_PAIRP(SCM_CDR(clause));
                     clause = SCM_CDR(clause)) {
                    re_compile_emit(ctx, RE_TRY, emitp);
                    patchp = ctx->codep;
                    re_compile_emit(ctx, 0, emitp); /* will be patched */
                    re_compile_emit(ctx, 0, emitp); /* will be patched */
                    re_compile_pass2(SCM_CAR(clause), rx, ctx,
                                     can_be_eol(SCM_CDR(cp)), emitp, FALSE);
                    re_compile_emit(ctx, RE_JUMP, emitp);
                    if (emitp) {
                        jumps = Scm_Cons(SCM_MAKE_INT(ctx->codep), jumps);
                    }
                    re_compile_emit(ctx, 0, emitp); /* will be patched */
                    re_compile_emit(ctx, 0, emitp); /* will be patched */
                    if (emitp) {
                        ctx->code[patchp] = (ctx->codep>>8);
                        ctx->code[patchp+1] = (ctx->codep&0xff);
                    }
                }
                re_compile_pass2(SCM_CAR(clause), rx, ctx,
                                 can_be_eol(SCM_CDR(cp)), emitp, FALSE);
                if (emitp) {
                    SCM_FOR_EACH(jumps, jumps) {
                        patchp = SCM_INT_VALUE(SCM_CAR(jumps));
                        ctx->code[patchp] = ctx->codep >> 8;
                        ctx->code[patchp+1] = ctx->codep & 0xff;
                    }
                }
                continue;
            }
            if (car == sym_comp) {
                ScmObj cs = SCM_CDR(item);
                SCM_ASSERT(SCM_CHARSETP(cs));
                if (SCM_CHARSET_SMALLP(cs)) {
                    re_compile_emit(ctx, RE_NSET1, emitp);
                } else {
                    re_compile_emit(ctx, RE_NSET, emitp);
                }
                re_compile_emit(ctx, re_compile_charset_index(rx, cs), emitp);
                continue;
            }
            /* fallback to error */
        }

        if (SCM_FALSEP(item)) {
            /* this is a placeholder.  do nothing. */
            continue;
        }
        
        Scm_Error("internal error while rexexp compilation: item %S\n", item);
    }
    /* see if we have the literal sequence */
    if (toplevelp && longest_size > 0) {
        rx->mustMatch = SCM_STRING(Scm_MakeString(&(ctx->code[longest_idx]),
                                                  longest_size, -1,
                                                  SCM_MAKSTR_IMMUTABLE));
    }
}

/* For debug */
#if SCM_DEBUG_HELPER
void Scm_RegDump(ScmRegexp *rx)
{
    int end = rx->numCodes, codep;

    Scm_Printf(SCM_CUROUT, "Regexp %p:\n", rx);
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
                       rx->code[codep]*256 + rx->code[codep+1]);
            codep++;
            continue;
        case RE_SET:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  SET  %d    %S\n",
                       codep-1, rx->code[codep],
                       rx->sets[(unsigned int)rx->code[codep]]);
            continue;
        case RE_NSET:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  NSET  %d    %S\n",
                       codep-1, rx->code[codep],
                       rx->sets[(unsigned int)rx->code[codep]]);
            continue;
        case RE_SET1:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  SET1 %d    %S\n",
                       codep-1, rx->code[codep],
                       rx->sets[(unsigned int)rx->code[codep]]);
            continue;
        case RE_NSET1:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  NSET1 %d    %S\n",
                       codep-1, rx->code[codep],
                       rx->sets[(unsigned int)rx->code[codep]]);
            continue;
        case RE_JUMP:
            codep++;
            Scm_Printf(SCM_CUROUT, "%4d  JUMP %d\n", codep-1,
                       rx->code[codep]*256+rx->code[codep+1]);
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
        default:
            Scm_Error("regexp screwed up\n");
        }
    }
}
#endif /* SCM_DEBUG_HELPER */

/*--------------------------------------------------------------
 * Compiler entry point
 */
ScmObj Scm_RegComp(ScmString *pattern, int flags)
{
    ScmRegexp *rx = make_regexp();
    ScmObj compiled;
    struct comp_ctx cctx;

    rx->flags = flags;
    
    if (SCM_STRING_INCOMPLETE_P(pattern)) {
        Scm_Error("incomplete string is not allowed: %S", pattern);
    }

    rx->pattern = SCM_STRING(Scm_MakeString(SCM_STRING_START(pattern),
                                            SCM_STRING_SIZE(pattern),
                                            SCM_STRING_LENGTH(pattern),
                                            SCM_MAKSTR_IMMUTABLE));
    cctx.pattern = rx->pattern;
    cctx.ipat = SCM_PORT(Scm_MakeInputStringPort(pattern, FALSE));
    cctx.sets = SCM_NIL;
    cctx.codep = 0;

    /* pass 1 : parse regexp spec */
    compiled = re_compile_pass1(rx, &cctx);
    re_compile_setup_charsets(rx, &cctx);

    /* pass 2.1 : count required bytecode size */
    cctx.codemax = 1;
    re_compile_pass2(compiled, rx, &cctx, TRUE, FALSE, TRUE);
    
    /* pass 2.2 : generate bytecodes */
    cctx.code = SCM_NEW_ATOMIC2(char *, cctx.codemax);
    rx->numCodes = cctx.codemax;
    re_compile_pass2(compiled, rx, &cctx, TRUE, TRUE, TRUE);
    re_compile_emit(&cctx, RE_SUCCESS, TRUE);
    rx->code = cctx.code;
    rx->numCodes = cctx.codep;
    return SCM_OBJ(rx);
}

/*=======================================================================
 * Matcher
 */

/* For now, I use C-stack directly to keep information for backtrack,
 * i.e. anytime I should try something I recursively call re_exec_rec().
 * It may run out the stack space if regexp requires deep recursion.
 * Possible optimization is to treat trivial cases (like '\S*') specially
 * to avoid recursion.
 *
 * Re_exec_rec doesn't return as long as match succeeds.  At the end of
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
    const char *codehead;
    const char *input;          /* start of input */
    const char *stop;           /* end of input */
    const char *last;
    struct match_list *matches;
    void *begin_stack;          /* C stack pointer the match began from. */
    sigjmp_buf cont;
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

static int match_ci(const char **input, const char **code, int length)
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
                if (*(*code)++ != *(*input)++) return FALSE;
            }
        }
        length -= (csize+1);
    } while (length > 0);
    return TRUE;
}

void re_exec_rec(const char *code,
                 const char *input,
                 struct match_ctx *ctx,                 
                 struct match_list *mlist)
{
    register int param, param2;
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
            param = (unsigned char)*code++;
            if (ctx->stop - input < param) return;
            while (param-- > 0) {
                if (*code++ != *input++) return;
            }
            continue;
        case RE_MATCH1:
            if (ctx->stop == input) return;
            if (*code++ != *input++) return;
            continue;
        case RE_MATCH_CI:
            param = (unsigned char)*code++;
            if (ctx->stop - input < param) return;
            if (!match_ci(&input, &code, param)) return;
            continue;
        case RE_MATCH1_CI:
            if (ctx->stop == input) return;
            param  = (unsigned char)*input++;
            param2 = (unsigned char)*code++;
            if (SCM_CHAR_NFOLLOWS(param)!=0 || param2!=SCM_CHAR_DOWNCASE(param))
                return;
            continue;
        case RE_ANY:
            if (ctx->stop == input) return;
            input += SCM_CHAR_NFOLLOWS(*input) + 1;
            continue;
        case RE_TRY:
            param = (unsigned char)*code++;
            param2 = (unsigned char)*code++;
            re_exec_rec(code, input, ctx, mlist);
            code = ctx->codehead + param*256 + param2;
            continue;
        case RE_JUMP:
            param = (unsigned char)*code++;
            param2 = (unsigned char)*code++;
            code = ctx->codehead + param*256 + param2;
            continue;
        case RE_SET1:
            if (ctx->stop == input) return;
            if ((unsigned char)*input >= 128) return;
            param = (unsigned char)*code++;
            if (!Scm_CharSetContains(ctx->rx->sets[param], *input)) return;
            input++;
            continue;
        case RE_NSET1:
            if (ctx->stop == input) return;
            if ((unsigned char)*input < 128) {
                param = (unsigned char)*code++;
                if (Scm_CharSetContains(ctx->rx->sets[param], *input)) return;
                input++;
            } else {
                code++;
                input += SCM_CHAR_NFOLLOWS((unsigned char)*input) + 1;
            }
            continue;
        case RE_SET:
            if (ctx->stop == input) return;
            param = (unsigned char)*code++;
            SCM_CHAR_GET(input, ch);
            cset = ctx->rx->sets[param];
            if (!Scm_CharSetContains(cset, ch)) return;
            input += SCM_CHAR_NBYTES(ch);
            continue;
        case RE_NSET:
            if (ctx->stop == input) return;
            param = (unsigned char)*code++;
            SCM_CHAR_GET(input, ch);
            cset = ctx->rx->sets[param];
            if (Scm_CharSetContains(cset, ch)) return;
            input += SCM_CHAR_NBYTES(ch);
            continue;
        case RE_BEGIN:
            param = (unsigned char)*code++;
            mlist = push_match(mlist, param, input);
            continue;
        case RE_END:
            param = (unsigned char)*code++;
            mlist = push_match(mlist, -param, input);
            continue;
        case RE_BOL:
            if (input != ctx->input) return;
            continue;
        case RE_EOL:
            if (input != ctx->stop) return;
            continue;
        case RE_SUCCESS:
            ctx->last = input;
            ctx->matches = mlist;
            siglongjmp(ctx->cont, 1);
            /*NOTREACHED*/
        case RE_FAIL:
            return;
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
    rm->matches = SCM_NEW2(struct ScmRegMatchSub*,
                           sizeof(struct ScmRegMatchSub)*rx->numGroups);
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

static ScmObj re_exec(ScmRegexp *rx, ScmString *orig,
                      const char *start, const char *end)
{
    struct match_ctx ctx;
    ctx.rx = rx;
    ctx.codehead = rx->code;
    ctx.input = SCM_STRING_START(orig);
    ctx.stop = end;
    ctx.matches = NULL;
    ctx.begin_stack = (void*)&ctx;

    if (sigsetjmp(ctx.cont, TRUE) == 0) {
        re_exec_rec(ctx.codehead, start, &ctx, NULL);
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
       faster to go for re_exec directly. */
    if (rx->mustMatch) {
        /* Prescreening.  If the input string doesn't contain mustMatch
           string, it can't match the entire expression. */
        if (SCM_FALSEP(Scm_StringScan(str, rx->mustMatch,
                                      SCM_STRING_SCAN_INDEX))) {
            return SCM_FALSE;
        }
    }
#endif
    while (start <= end-mustMatchLen) {
        ScmObj r = re_exec(rx, str, start, end);
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
    sym_alt = SCM_INTERN("alt");
    sym_rep = SCM_INTERN("rep");
    sym_any = SCM_INTERN("any");
    sym_bol = SCM_INTERN("bol");
    sym_eol = SCM_INTERN("eol");
    sym_comp = SCM_INTERN("comp");
}
