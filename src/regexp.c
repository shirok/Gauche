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
 *  $Id: regexp.c,v 1.1 2001-04-08 20:14:55 shiro Exp $
 */

#include "gauche.h"

/*
 * Compiled regexp is a list of codes.
 *
 *  <codelist> := <code> ...
 *  <code> := <char>                 ; matches <char>
 *            <charset>              ; matches <charset>
 *            (group <num> . <codelist>) ; if <codelist> matches, it'll be
 *                                   ;   <num>'th group.
 *            (* . <codelist>)       ; matches zero or more <codelist>
 *            (or <codelist> ...)    ; alternatives.
 *            (rep <m> . <codelist>) ; repeat at most <m> times.
 *
 * Backtrack may happen under '*', 'or' or 'rep' case.  I use recursive
 * call to process those structures, that keeps intermediate result
 * in the C stack.  Once succeeded, it longjmp()'s to the toplevel
 * (thus, if the intermediate routine returns it meands it failed)
 */

static ScmObj re_compile_group(const char *, const char *, int, ScmObj);
static ScmObj re_compile_range(const char *, const char *, int, ScmObj);

static ScmObj re_compile(const char *orig, const char *ptr, int len, ScmObj r)
{
    ScmChar ch;
    while (len > 0) {
        SCM_STR_GETC(ch, ptr);
        ptr += SCM_CHAR_NBYTES(ch);
        len--;

        switch (ch) {
        case '[':
            return re_compile_range(orig, ptr, len, r);
        case '(':
            return re_compile_group(orig, ptr, len, r);
        default:
            r = Scm_Cons(SCM_MAKE_CHAR(ch), r);
        }
    }
    return Scm_ReverseX(r);
}

static ScmObj re_compile_group(const char *orig, const char *ptr,
                               int len, ScmObj r)
{
    ScmChar ch;
    if (len == 0) Scm_Error("unterminated group in the regexp: %s", orig);
    
}
