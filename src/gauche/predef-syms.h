/*
 * predef-syms.h - list of pre-defined symbols
 *
 *   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
 *  $Id: predef-syms.h,v 1.26 2003-07-05 03:29:13 shirok Exp $
 */

/* DEFSYM(c-name, scheme-name) */

DEFSYM(ScmQquote,           "quote");
DEFSYM(ScmQquasiquote,      "quasiquote");
DEFSYM(ScmQunquote,         "unquote");
DEFSYM(ScmQunquoteSplicing, "unquote-splicing");
DEFSYM(ScmQdefine,          "define");
DEFSYM(ScmQdefineConstant,  "define-constant");
DEFSYM(ScmQdefineInModule,  "define-in-module");
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
DEFSYM(ScmQmacroExpand,     "%macroexpand");
DEFSYM(ScmQmacroExpand1,    "%macroexpand-1");

DEFSYM(ScmQnull,            "null");
DEFSYM(ScmQscheme,          "scheme");
DEFSYM(ScmQgauche,          "gauche");
DEFSYM(ScmQgaucheGf,        "gauche.gf");
DEFSYM(ScmQuser,            "user");

DEFSYM(ScmQloadPath,        "*load-path*");
DEFSYM(ScmQloadNext,        "*load-next*");
DEFSYM(ScmQloadHistory,     "*load-history*");
DEFSYM(ScmQloadPort,        "*load-port*");
DEFSYM(ScmQloadSuffixes,    "*load-suffixes*");
DEFSYM(ScmQdynamicLoadPath, "*dynamic-load-path*");

DEFSYM(ScmQsourceInfo,      "source-info");
DEFSYM(ScmQbindInfo,        "bind-info");

/* cpp can't generate #define lines automatically... */
#ifdef DEFSYM_DEFINES
#define SCM_SYM_QUOTE            SCM_OBJ(&ScmQquote)
#define SCM_SYM_QUASIQUOTE       SCM_OBJ(&ScmQquasiquote)
#define SCM_SYM_UNQUOTE          SCM_OBJ(&ScmQunquote)
#define SCM_SYM_UNQUOTE_SPLICING SCM_OBJ(&ScmQunquoteSplicing)
#define SCM_SYM_DEFINE           SCM_OBJ(&ScmQdefine)
#define SCM_SYM_DEFINE_CONSTANT  SCM_OBJ(&ScmQdefineConstant)
#define SCM_SYM_DEFINE_IN_MODULE SCM_OBJ(&ScmQdefineInModule)
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
#define SCM_SYM_MACRO_EXPAND     SCM_OBJ(&ScmQmacroExpand)
#define SCM_SYM_MACRO_EXPAND_1   SCM_OBJ(&ScmQmacroExpand1)

#define SCM_SYM_NULL             SCM_OBJ(&ScmQnull)
#define SCM_SYM_SCHEME           SCM_OBJ(&ScmQscheme)
#define SCM_SYM_GAUCHE           SCM_OBJ(&ScmQgauche)
#define SCM_SYM_GAUCHE_GF        SCM_OBJ(&ScmQgaucheGf)
#define SCM_SYM_USER             SCM_OBJ(&ScmQuser)

#define SCM_SYM_LOAD_PATH        SCM_OBJ(&ScmQloadPath)
#define SCM_SYM_LOAD_NEXT        SCM_OBJ(&ScmQloadNext)
#define SCM_SYM_LOAD_HISTORY     SCM_OBJ(&ScmQloadHistory)
#define SCM_SYM_LOAD_PORT        SCM_OBJ(&ScmQloadPort)
#define SCM_SYM_LOAD_SUFFIXES    SCM_OBJ(&ScmQloadSuffixes)
#define SCM_SYM_DYNAMIC_LOAD_PATH SCM_OBJ(&ScmQdynamicLoadPath)

#define SCM_SYM_SOURCE_INFO      SCM_OBJ(&ScmQsourceInfo)
#define SCM_SYM_BIND_INFO        SCM_OBJ(&ScmQbindInfo)
#endif /*DEFSYM_DEFINES*/
