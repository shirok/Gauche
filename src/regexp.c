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
 *  $Id: regexp.c,v 1.3 2001-04-13 06:18:06 shiro Exp $
 */

#include "gauche.h"

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_RegexpClass, NULL);

#ifndef CHAR_MAX
#define CHAR_MAX 256
#endif

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

/* symbols used internally */
ScmObj sym_alt;                 /* alt */
ScmObj sym_rep;                 /* rep */
ScmObj sym_any;                 /* any */

static ScmRegexp *make_regexp(void)
{
    ScmRegexp *re = SCM_NEW(ScmRegexp);
    SCM_SET_CLASS(re, SCM_CLASS_REGEXP);
    re->code = NULL;
    re->numGroups = 0;
    return re;
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
ScmObj re_compile_pass1(ScmRegexp *re, struct comp_ctx *ctx)
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
            sym = sym_alt;
            elt = last_item(ctx, head, tail, grpstack, ch);
            cell = Scm_Cons(SCM_CAR(elt), SCM_CDR(elt));
            SCM_SET_CAR(elt, Scm_Cons(sym, cell));
            SCM_SET_CDR(elt, SCM_NIL);
            tail = elt;
            insncount++;
            continue;
        case '+':  /* x+ === xx* */
            elt = Scm_CopyList(last_item(ctx, head, tail, grpstack, ch));
            SCM_APPEND1(head, tail, Scm_Cons(sym_rep, elt));
            insncount++;
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
            SCM_APPEND1(head, tail, re_compile_charset(re, ctx));
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

    re->numGroups = grpcount+1;
    re->numCodes = insncount;
    return head;
}

/* character range */
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
    ScmRegexp *re = make_regexp();
    ScmObj compiled;
    struct comp_ctx cctx;
    cctx.pattern = pattern;
    cctx.rxstr = SCM_STRING_START(pattern);
    cctx.rxlen = SCM_STRING_LENGTH(pattern);
    cctx.sets = SCM_NIL;
    
    compiled = re_compile_pass1(re, &cctx);
    re_compile_setup_charsets(re, &cctx);
    re_compile_pass2(compiled, re);
    re_dump(re);
    return compiled;
}

void Scm__InitRegexp(void)
{
    Scm_InitBuiltinClass(SCM_CLASS_REGEXP, "<regexp>", Scm_GaucheModule());

    sym_alt = SCM_INTERN("alt");
    sym_rep = SCM_INTERN("rep");
    sym_any = SCM_INTERN("any");
}
