/*
 * regexp.c - regular expression
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: regexp.c,v 1.5 2001-04-13 10:36:50 shiro Exp $
 */

#include "gauche.h"

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_RegexpClass, NULL);
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_RegMatchClass, NULL);

#ifndef CHAR_MAX
#define CHAR_MAX 256
#endif

/* THIS CODE IS EXPERIMENTAL, AND NOT WORKING YET. */

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
 * much as possible.  The conversion is inevitable anytime we have to
 * match charsets, but there are number of cases we can avoid it.
 * The engine itself will be implemented in NFA-based scheme.
 */

/* Instructions */
enum {
    RE_MATCH1,                  /* followed by 1 byte to match */
    RE_MATCH,                   /* followed by length, and bytes to match */
    RE_ANY,                     /* match any char (maybe but newline) */
    RE_TRY,                     /* followed by offset.  try matching
                                   the following sequence, and if fails,
                                   jump to offset. */
    RE_REP,                     /* followed by offset.  try matching the
                                   following sequence repeatedly, and if
                                   it fails, jump to offset. */
    RE_SET,                     /* followed by a number of charset */
    RE_JUMP,                    /* followed by offset.  jump to that
                                   bytecode. */
    RE_BEGIN,                   /* followed by a group number.  start the
                                   group. */
    RE_END                      /* followed by a group number.  end the
                                   group. */
};

/* symbols used internally */
ScmObj sym_alt;                 /* alt */
ScmObj sym_rep;                 /* rep */
ScmObj sym_any;                 /* any */

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
    rx->mustMatchLen = 0;
    return rx;
}

/*=======================================================================
 * Compiler
 */

/* Two-pass compiler.
 *
 *  pass 1: parse pattern, generating parse tree and also counting sizes
 *          of necessary storage.
 *  pass 2: byte code generation.
 */

/* compiler state information */
struct comp_ctx {
    ScmString *pattern;         /* original pattern */
    const char *rxstr;          /* current point being parsed */
    int rxlen;                  /* current length */
    ScmObj sets;                /* list of charsets */
};

static ScmObj re_compile_charset(ScmRegexp *rx, struct comp_ctx *ctx);

static inline ScmChar fetch_pattern(struct comp_ctx *ctx)
{
    ScmChar ch;
    SCM_STR_GETC(ctx->rxstr, ch);
    ctx->rxstr += SCM_CHAR_NBYTES(ch);
    ctx->rxlen--;
    return ch;
}

static ScmObj last_item(struct comp_ctx *ctx, ScmObj head, ScmObj tail,
                        ScmObj gstack, ScmChar ch)
{
    ScmObj last = SCM_CAR(tail);
    if (SCM_INTP(last)) {
        int gnum = SCM_INT_VALUE(last);
        if (gnum < 0) {
            /* just after close parenthesis.
               find the begining of the group. */
            ScmObj cp, gstart = SCM_MAKE_INT(-gnum);
            SCM_FOR_EACH(cp, head) {
                if (SCM_CAR(cp) == gstart) return cp;
            }
            Scm_Error("something broken internally.");
        } else {
            /* just after open parenthesis or beginning. */
            if (ch == '|') {
                /* notimpl */
                Scm_Error("not implemented");
            } else {
                Scm_Error("bad regexp pattern: %S", ctx->pattern);
            }
        }
    } else {
        if (ch == '|') {
            return SCM_CDAR(gstack);
        } else {
            return tail;
        }
    }
}

/* pass1 - parser */
ScmObj re_compile_pass1(ScmRegexp *rx, struct comp_ctx *ctx)
{
    ScmObj head = SCM_NIL, tail = SCM_NIL, elt, cell, sym;
    ScmObj grpstack;            /* group stack. */
    ScmChar ch;
    int grpcount = 0;
    int insncount = 0;

    /* default group == entire match*/
    SCM_APPEND1(head, tail, SCM_MAKE_INT(0));
    grpstack = Scm_Cons(tail, SCM_NIL);
    
    while (ctx->rxlen > 0) {
        ch = fetch_pattern(ctx);

        switch (ch) {
        case '(':
            grpcount++;
            insncount++;
            SCM_APPEND1(head, tail, SCM_MAKE_INT(grpcount));
            grpstack = Scm_Cons(tail, grpstack);
            continue;
        case ')':
            if (SCM_NULLP(SCM_CDR(grpstack))) {
                Scm_Error("extra close parenthesis in regexp: %S",
                          ctx->pattern);
            } else {
                ScmObj gnum = SCM_CAAR(grpstack);
                int g = SCM_INT_VALUE(gnum);
                SCM_APPEND1(head, tail, SCM_MAKE_INT(-g));
                grpstack = SCM_CDR(grpstack);
                insncount++;
            }
            continue;
        case '|':
            elt = last_item(ctx, head, tail, grpstack, ch);
            elt = last_item(ctx, head, tail, grpstack, ch);
            cell = Scm_Cons(SCM_CAR(elt), SCM_CDR(elt));
            SCM_SET_CAR(elt, Scm_Cons(sym_alt, cell));
            SCM_SET_CDR(elt, SCM_NIL);
            tail = elt;
            insncount++;
            continue;
        case '+':  /* x+ === xx* */
            elt = Scm_CopyList(last_item(ctx, head, tail, grpstack, ch));
            SCM_APPEND1(head, tail, Scm_Cons(sym_rep, elt));
            insncount++;
            continue;
        case '?':  /* x? === (x|) */
            /* not yet */
            continue;
        case '*':
            elt = last_item(ctx, head, tail, grpstack, ch);
            cell = Scm_Cons(SCM_CAR(elt), SCM_CDR(elt));
            SCM_SET_CAR(elt, Scm_Cons(sym_rep, cell));
            SCM_SET_CDR(elt, SCM_NIL);
            tail = elt;
            insncount++;
            continue;
        case '.':
            SCM_APPEND1(head, tail, sym_any);
            insncount++;
            continue;
        case '[':
            SCM_APPEND1(head, tail, re_compile_charset(rx, ctx));
            insncount++;
            continue;
        default:
            insncount += SCM_CHAR_NBYTES(ch);
            SCM_APPEND1(head, tail, SCM_MAKE_CHAR(ch));
        }
    }

    SCM_ASSERT(SCM_PAIRP(grpstack));
    if (!SCM_NULLP(SCM_CDR(grpstack)))
        Scm_Error("extra open parenthesis in regexp: %S", ctx->pattern);

    rx->numGroups = grpcount+1;
    rx->numCodes = insncount;
    return head;
}

/* character range */
/* TODO:  [:class:] and other posix weird stuff. */
static ScmObj re_compile_charset(ScmRegexp *rx, struct comp_ctx *ctx)
{
    int begin = TRUE, complement = FALSE;
    int lastchar = -1, inrange = FALSE;
    ScmCharSet *set = SCM_CHARSET(Scm_MakeEmptyCharSet());
    ScmChar ch;

    for (;;) {
        if (ctx->rxlen <= 0) Scm_Error("Unclosed bracket: %S", ctx->pattern);
        ch = fetch_pattern(ctx);

        if (begin) {
            if (ch == '^') complement = TRUE;
            else {
                Scm_CharSetAddRange(set, ch, ch);
                lastchar = ch;
            }
            begin = FALSE;
            continue;
        }
        switch (ch) {
        case '-':
            if (lastchar < 0) {
                Scm_Error("bad character range spec: %S", ctx->pattern);
            }
            inrange = TRUE;
            continue;
        case ']':
            if (inrange) {
                SCM_ASSERT(lastchar >= 0);
                Scm_CharSetAddRange(set, lastchar, lastchar);
                Scm_CharSetAddRange(set, '-', '-');
            }
            break;
        default:
            if (inrange) {
                SCM_ASSERT(lastchar >= 0);
                Scm_CharSetAddRange(set, lastchar, ch);
                lastchar = -1;
                inrange = FALSE;
            } else {
                Scm_CharSetAddRange(set, ch, ch);
                lastchar = ch;
            }
            continue;
        }
        break;
    }
    if (complement) Scm_CharSetComplement(set);
    ctx->sets = Scm_Cons(SCM_OBJ(set), ctx->sets);
    return SCM_OBJ(set);
}

static void re_compile_setup_charsets(ScmRegexp *rx, struct comp_ctx *ctx)
{
    ScmObj cp;
    rx->numSets = 0;
    SCM_FOR_EACH(cp, Scm_Reverse(ctx->sets)) {
        rx->sets[rx->numSets++] = SCM_CHARSET(SCM_CAR(cp));
    }
}

static int re_compile_charset_index(ScmRegexp *rx, ScmObj cs)
{
    int i;
    for (i=0; i<rx->numSets; i++)
        if (cs == SCM_OBJ(rx->sets[i])) return i;
    Scm_Panic("re_compile_charset_index: can't be here");
    return 0;                   /* dummy */
}

/* pass 2 - code generation */
ScmObj re_compile_pass2(ScmObj compiled, ScmRegexp *rx)
{
    ScmObj cp = compiled, stack = SCM_NIL, item;
    ScmObj forward = SCM_NIL;   /* ((cell . patch_addr) ...) */
    char *code = SCM_NEW_ATOMIC2(char *, rx->numCodes*2);
    int codep = 0, codemax = 100 /*rx->numCodes*2*/, setcount = 0, nbytes;
    ScmChar ch;
    char chbuf[SCM_CHAR_MAX_BYTES];

    if (rx->numSets > 0) {
        rx->sets = SCM_NEW2(ScmCharSet **, rx->numSets * sizeof(ScmCharSet*));
    }
    
#define EMIT(code_)                             \
    do { SCM_ASSERT(codep < codemax); code[codep++] = (code_); } while(0)
#define NEXT()   cp = SCM_CDR(cp); continue
    
    for (;;) {
        if (SCM_NULLP(cp)) {
            ScmObj fp, patchp;
            
            if (SCM_NULLP(stack)) break;
            cp = SCM_CAR(stack);
            stack = SCM_CDR(stack);

            /* look for jump addresses */
            SCM_FOR_EACH(fp, forward) {
                if (SCM_CAAR(fp) == cp) {
                    patchp = SCM_CDAR(fp);
                    code[SCM_INT_VALUE(patchp)] = codep;
                }
            }
            continue;
        }
        item = SCM_CAR(cp);

        if (SCM_CHARP(item)) {
            /* find out the longest run of bytes */
            int nrun = 0, patchp = codep+1, nb, i;
            EMIT(RE_MATCH);
            EMIT(0);            /* patched later */
            do {
                ch = SCM_CHAR_VALUE(item);
                nb = SCM_CHAR_NBYTES(ch);
                SCM_STR_PUTC(chbuf, SCM_CHAR_VALUE(item));
                for (i=0; i<nb; i++) EMIT(chbuf[i]);
                nrun += nb;
                if (SCM_NULLP(SCM_CDR(cp))) break;
                cp = SCM_CDR(cp);
                item = SCM_CAR(cp);
            } while (SCM_CHARP(item) && nrun < CHAR_MAX);
            code[patchp] = (char)nrun;
            if (!SCM_CHARP(item)) cp = Scm_Cons(item, cp); /* pushback */
            NEXT();
        }
        
        if (SCM_INTP(item)) {
            int grpnum = SCM_INT_VALUE(item);
            if (grpnum < 0) {
                EMIT(RE_END);
                EMIT(-grpnum);
            } else {
                EMIT(RE_BEGIN);
                EMIT(grpnum);
            }
            NEXT();
        }

        if (SCM_CHARSETP(item)) {
            EMIT(RE_SET);
            EMIT(re_compile_charset_index(rx, item));
            setcount++;
            NEXT();
        }

        if (SCM_SYMBOLP(item)) {
            if (item == sym_any) {
                EMIT(RE_ANY);
                NEXT();
            } else if (item == sym_rep) {
                EMIT(RE_REP);
                forward = Scm_Acons(SCM_CAR(stack), SCM_MAKE_INT(codep),
                                    forward);
                EMIT(0);        /* will be patched */
                NEXT();
            } else if (item == sym_alt) {
                /* needs to find the merge point */
                EMIT(RE_TRY);
                EMIT(0);
                NEXT();
            }
            /* fallback to error */
        }

        if (SCM_PAIRP(item)) {
            stack = Scm_Cons(SCM_CDR(cp), stack);
            cp = item;
            continue;
        }

        Scm_Error("internal error while rexexp compilation: item %S\n", item);
    }
    
    rx->code = code;
    rx->numCodes = codep;
    return SCM_OBJ(rx);
}

/* For debug */
void re_dump(ScmRegexp *rx)
{
    int end = rx->numCodes, codep;
    for (codep = 0; codep < end; codep++) {
        switch (rx->code[codep]) {
        case RE_MATCH1:
            codep++;
            printf("%4d  MATCH1  0x%02x  '%c'\n",
                   codep-1, rx->code[codep], rx->code[codep]);
            continue;
        case RE_MATCH:
            codep++;
            {
                u_int numchars = (u_int)rx->code[codep];
                int i;
                printf("%4d  MATCH(%3d) '", codep-1, numchars);
                for (i=0; i< numchars; i++)
                    printf("%c", rx->code[++codep]);
                printf("'\n");
            }
            continue;
        case RE_ANY:
            printf("%4d  ANY\n", codep);
            continue;
        case RE_TRY:
            codep++;
            printf("%4d  TRY  %d\n", codep-1, rx->code[codep]);
            continue;
        case RE_REP:
            codep++;
            printf("%4d  REP  %d\n", codep-1, rx->code[codep]);
            continue;
        case RE_SET:
            codep++;
            printf("%4d  SET  %d\n", codep-1, rx->code[codep]);
            continue;
        case RE_JUMP:
            codep++;
            printf("%4d  JUMP %d\n", codep-1, rx->code[codep]);
            continue;
        case RE_BEGIN:
            codep++;
            printf("%4d  BEGIN %d\n", codep-1, rx->code[codep]);
            continue;
        case RE_END:
            codep++;
            printf("%4d  END %d\n", codep-1, rx->code[codep]);
            continue;
        default:
            Scm_Error("regexp screwed up\n");
        }
    }
}

ScmObj re_compile(ScmString *pattern)
{
    ScmRegexp *rx = make_regexp();
    ScmObj compiled;
    struct comp_ctx cctx;
    cctx.pattern = pattern;
    cctx.rxstr = SCM_STRING_START(pattern);
    cctx.rxlen = SCM_STRING_LENGTH(pattern);
    cctx.sets = SCM_NIL;
    
    compiled = re_compile_pass1(rx, &cctx);
    re_compile_setup_charsets(rx, &cctx);
    re_compile_pass2(compiled, rx);
    re_dump(rx);
    return compiled;
}

/*=======================================================================
 * Matcher
 */

/* to be written ... */

/*=======================================================================
 * Initializing stuff
 */

void Scm__InitRegexp(void)
{
    Scm_InitBuiltinClass(SCM_CLASS_REGEXP, "<regexp>", Scm_GaucheModule());
    Scm_InitBuiltinClass(SCM_CLASS_REGMATCH, "<regmatch>", Scm_GaucheModule());

    sym_alt = SCM_INTERN("alt");
    sym_rep = SCM_INTERN("rep");
    sym_any = SCM_INTERN("any");
}
