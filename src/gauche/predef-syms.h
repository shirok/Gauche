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
 *  $Id: predef-syms.h,v 1.16 2001-03-07 06:58:54 shiro Exp $
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
DEFSYM(ScmQdefineModule,    "define-module");
DEFSYM(ScmQwithModule,      "with-module");
DEFSYM(ScmQselectModule,    "select-module");
DEFSYM(ScmQcurrentModule,   "current-module");
DEFSYM(ScmQimport,          "import");
DEFSYM(ScmQexport,          "export");
DEFSYM(ScmQdefineMacro,     "define-macro");
DEFSYM(ScmQdefineSyntax,    "define-syntax");
DEFSYM(ScmQletSyntax,       "let-syntax");
DEFSYM(ScmQletrecSyntax,    "letrec-syntax");
DEFSYM(ScmQsyntaxRulesInt,  "%syntax-rules");
DEFSYM(ScmQsyntaxRules,     "syntax-rules");
DEFSYM(ScmQellipsis,        "...");

DEFSYM(ScmQnull,            "null");
DEFSYM(ScmQscheme,          "scheme");
DEFSYM(ScmQgauche,          "gauche");
DEFSYM(ScmQuser,            "user");

DEFSYM(ScmQmacroExpand,     "%macro-expand");
DEFSYM(ScmQmacroExpand1,    "%macro-expand-1");
DEFSYM(ScmQloadPath,        "*load-path*");
DEFSYM(ScmQloadNext,        "*load-next*");
DEFSYM(ScmQloadHistory,     "*load-history*");
DEFSYM(ScmQloadFilename,    "*load-filename*");
DEFSYM(ScmQdynamicLoadPath, "*dynamic-load-path*");

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
#define SCM_SYM_DEFINE_MODULE    SCM_OBJ(&ScmQdefineModule)
#define SCM_SYM_WITH_MODULE      SCM_OBJ(&ScmQwithModule)
#define SCM_SYM_SELECT_MODULE    SCM_OBJ(&ScmQselectModule)
#define SCM_SYM_CURRENT_MODULE   SCM_OBJ(&ScmQcurrentModule)
#define SCM_SYM_IMPORT           SCM_OBJ(&ScmQimport)
#define SCM_SYM_EXPORT           SCM_OBJ(&ScmQexport)

#define SCM_SYM_DEFINE_MACRO     SCM_OBJ(&ScmQdefineMacro)
#define SCM_SYM_DEFINE_SYNTAX    SCM_OBJ(&ScmQdefineSyntax)
#define SCM_SYM_LET_SYNTAX       SCM_OBJ(&ScmQletSyntax)
#define SCM_SYM_LETREC_SYNTAX    SCM_OBJ(&ScmQletrecSyntax)
#define SCM_SYM_SYNTAX_RULES_INT SCM_OBJ(&ScmQsyntaxRulesInt)
#define SCM_SYM_SYNTAX_RULES     SCM_OBJ(&ScmQsyntaxRules)
#define SCM_SYM_ELLIPSIS         SCM_OBJ(&ScmQellipsis) /* ... */

#define SCM_SYM_NULL             SCM_OBJ(&ScmQnull)
#define SCM_SYM_SCHEME           SCM_OBJ(&ScmQscheme)
#define SCM_SYM_GAUCHE           SCM_OBJ(&ScmQgauche)
#define SCM_SYM_USER             SCM_OBJ(&ScmQuser)

#define SCM_SYM_MACRO_EXPAND     SCM_OBJ(&ScmQmacroExpand)
#define SCM_SYM_MACRO_EXPAND_1   SCM_OBJ(&ScmQmacroExpand1)
#define SCM_SYM_LOAD_PATH        SCM_OBJ(&ScmQloadPath)
#define SCM_SYM_LOAD_NEXT        SCM_OBJ(&ScmQloadNext)
#define SCM_SYM_LOAD_HISTORY     SCM_OBJ(&ScmQloadHistory)
#define SCM_SYM_LOAD_FILENAME    SCM_OBJ(&ScmQloadFilename)
#define SCM_SYM_DYNAMIC_LOAD_PATH SCM_OBJ(&ScmQdynamicLoadPath)
#endif /*DEFSYM_DEFINES*/
