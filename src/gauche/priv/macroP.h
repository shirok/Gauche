/*
 * macro.h - structures used internally in macro expander
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_MACRO_H
#define GAUCHE_MACRO_H


SCM_DECL_BEGIN

/* Syntax is a built-in procedure to compile given form. */
struct ScmSyntaxRec {
    SCM_HEADER;
    ScmSymbol *name;            /* for debugging.  can be NULL */
    ScmObj     handler;         /* syntax handler.  (Sexpr, Env) -> IForm */
};

/* Macro */
struct ScmMacroRec {
    SCM_HEADER;
    ScmSymbol *name;            /* for debugging.  can be NULL */
    ScmObj transformer;         /* (Sexpr, CEnv) -> Sexpr */
};

/*
 * SyntaxRules keeps a compiled rules of macro transformation.
 */

typedef struct ScmSyntaxRuleBranchRec {
    ScmObj pattern;             /* pattern to match */
    ScmObj templat;             /* template to be expanded */
    int numPvars;               /* # of pattern variables */
    int maxLevel;               /* maximum # of nested subpatterns */
} ScmSyntaxRuleBranch;

typedef struct ScmSyntaxRules {
    SCM_HEADER;
    ScmObj name;                  /* name of the macro (for debug) */
    int numRules;                 /* # of rules */
    int maxNumPvars;              /* max # of pattern variables */
    ScmSyntaxRuleBranch rules[1]; /* variable length */
} ScmSyntaxRules;

SCM_CLASS_DECL(Scm_SyntaxRulesClass);
#define SCM_CLASS_SYNTAX_RULES   (&Scm_SyntaxRulesClass)

#define SCM_SYNTAX_RULES(obj)    ((ScmSyntaxRules*)(obj))
#define SCM_SYNTAX_RULES_P(obj)  SCM_XTYPEP(obj, SCM_CLASS_SYNTAX_RULES)

SCM_EXTERN ScmObj Scm_CompileSyntaxRules(ScmObj name, ScmObj ellipsis,
                                         ScmObj lietrals,
                                         ScmObj rules, ScmObj mod, ScmObj env);

/*
 * Pattern variable reference object
 */
#define SCM_PVREF_TAG              0x13
#define SCM_PVREF_P(obj)           ((SCM_WORD(obj)&0xff) == SCM_PVREF_TAG)
#define SCM_PVREF_LEVEL(obj)       ((SCM_WORD(obj)>>24) & 0xff)
#define SCM_PVREF_COUNT(obj)       ((SCM_WORD(obj)>>16) & 0xff)

#define SCM_MAKE_PVREF(level, count)  \
    SCM_OBJ((SCM_WORD(level)<<24) | (SCM_WORD(count)<<16) | SCM_PVREF_TAG)

/* Temporary */
SCM_EXTERN ScmObj Scm_MakeMacroTransformerOld(ScmSymbol *name,
                                              ScmProcedure *proc);

SCM_DECL_END

#endif /* GAUCHE_MACRO_H */
