/*
 * predef-syms.h - list of pre-defined symbols
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, ditribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: predef-syms.h,v 1.1 2001-02-05 09:58:53 shiro Exp $
 */

/* DEFSYM(c-name, scheme-name) */

DEFSYM(ScmQquote,           "quote");
DEFSYM(ScmQquasiquote,      "quasiquote");
DEFSYM(ScmQunquote,         "unquote");
DEFSYM(ScmQunquoteSplicing, "unquote-splicing");
DEFSYM(ScmQdefine,          "define");
DEFSYM(ScmQlambda,          "lambda");
DEFSYM(ScmQif,              "if");
DEFSYM(ScmQset,             "set!");
DEFSYM(ScmQlet,             "let");
DEFSYM(ScmQletStar,         "let*");
DEFSYM(ScmQletrec,          "letrec");
DEFSYM(ScmQbegin,           "begin");
DEFSYM(ScmQwhen,            "when");
DEFSYM(ScmQunless,          "unless");
DEFSYM(ScmQand,             "and");
DEFSYM(ScmQor,              "or");
DEFSYM(ScmQcond,            "cond");
DEFSYM(ScmQcase,            "case");
DEFSYM(ScmQelse,            "else");
DEFSYM(ScmQyields,          "=>");
DEFSYM(ScmQdo,              "do");
DEFSYM(ScmQdelay,           "delay");
DEFSYM(ScmQmacroExpand,     "%macro-expand");

#if 0
DEFSYM(ScmQcons,   "cons");
DEFSYM(ScmQcar,    "car");
DEFSYM(ScmQcdr,    "cdr");
DEFSYM(ScmQlist,   "list");
DEFSYM(ScmQeq,     "eq?");
DEFSYM(ScmQeqv,    "eqv?");
DEFSYM(ScmQequal,  "equal");
DEFSYM(ScmQmemv,   "memv");
#endif


