/*
 * predef-syms.h - list of pre-defined symbols
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
 *  $Id: predef-syms.h,v 1.7 2001-02-20 12:02:26 shiro Exp $
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
DEFSYM(ScmQreceive,         "receive");
DEFSYM(ScmQdefineSyntax,    "define-syntax");
DEFSYM(ScmQletSyntax,       "let-syntax");
DEFSYM(ScmQletrecSyntax,    "letrec-syntax");
DEFSYM(ScmQsyntaxRules,     "syntax-rules");
DEFSYM(ScmQellipsis,        "...");

DEFSYM(ScmQmacroExpand,     "%macro-expand");
DEFSYM(ScmQloadPath,        "*load-path*");
DEFSYM(ScmQloadNext,        "*load-next*");
DEFSYM(ScmQloadHistory,     "*load-history*");
DEFSYM(ScmQloadFilename,    "*load-filename*");

/* cpp can't generate #define lines automatically... */
#ifdef DEFSYM_DEFINES
#define SCM_SYM_QUOTE            SCM_OBJ(&ScmQquote)
#define SCM_SYM_QUASIQUOTE       SCM_OBJ(&ScmQquasiquote)
#define SCM_SYM_UNQUOTE          SCM_OBJ(&ScmQunquote)
#define SCM_SYM_UNQUOTE_SPLICING SCM_OBJ(&ScmQunquoteSplicing)
#define SCM_SYM_DEFINE           SCM_OBJ(&ScmQdefine)
#define SCM_SYM_LAMBDA           SCM_OBJ(&ScmQlambda)
#define SCM_SYM_IF               SCM_OBJ(&ScmQif)
#define SCM_SYM_SET              SCM_OBJ(&ScmQset)
#define SCM_SYM_LET              SCM_OBJ(&ScmQlet)
#define SCM_SYM_LET_STAR         SCM_OBJ(&ScmQletStar)
#define SCM_SYM_LETREC           SCM_OBJ(&ScmQletrec)
#define SCM_SYM_BEGIN            SCM_OBJ(&ScmQbegin)
#define SCM_SYM_WHEN             SCM_OBJ(&ScmQwhen)
#define SCM_SYM_UNLESS           SCM_OBJ(&ScmQunless)
#define SCM_SYM_AND              SCM_OBJ(&ScmQand)
#define SCM_SYM_OR               SCM_OBJ(&ScmQor)
#define SCM_SYM_COND             SCM_OBJ(&ScmQcond)
#define SCM_SYM_CASE             SCM_OBJ(&ScmQcase)
#define SCM_SYM_ELSE             SCM_OBJ(&ScmQelse)
#define SCM_SYM_YIELDS           SCM_OBJ(&ScmQyields) /* => */
#define SCM_SYM_DO               SCM_OBJ(&ScmQdo)
#define SCM_SYM_DELAY            SCM_OBJ(&ScmQdelay)
#define SCM_SYM_RECEIVE          SCM_OBJ(&ScmQreceive)
#define SCM_SYM_DEFINE_SYNTAX    SCM_OBJ(&ScmQdefineSyntax)
#define SCM_SYM_LET_SYNTAX       SCM_OBJ(&ScmQletSyntax)
#define SCM_SYM_LETREC_SYNTAX    SCM_OBJ(&ScmQletrecSyntax)
#define SCM_SYM_SYNTAX_RULES     SCM_OBJ(&ScmQsyntaxRules)
#define SCM_SYM_ELLIPSIS         SCM_OBJ(&ScmQellipsis) /* ... */
#define SCM_SYM_MACRO_EXPAND     SCM_OBJ(&ScmQmacroExpand)
#define SCM_SYM_LOAD_PATH        SCM_OBJ(&ScmQloadPath)
#define SCM_SYM_LOAD_NEXT        SCM_OBJ(&ScmQloadNext)
#define SCM_SYM_LOAD_HISTORY     SCM_OBJ(&ScmQloadHistory)
#define SCM_SYM_LOAD_FILENAME    SCM_OBJ(&ScmQloadFilename)
#endif /*DEFSYM_DEFINES*/
