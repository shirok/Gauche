/*
 * macro.h - structures using internally in macro expander
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
 *  $Id: macro.h,v 1.2 2001-02-23 12:59:38 shiro Exp $
 */

#ifndef GAUCHE_MACRO_H
#define GAUCHE_MACRO_H

/*
 * SyntaxPattern object is an internal object used to expand r5rs macro.
 */

typedef struct ScmSyntaxPatternRec {
    SCM_HEADER;
    ScmObj pattern;             /* subpattern */
    ScmObj vars;                /* pattern variables in this subpattern */
    short level;                /* level of this subpattern */
    short repeat;               /* does this subpattern repeat? */
} ScmSyntaxPattern;

extern ScmClass Scm_SyntaxPatternClass;
#define SCM_CLASS_SYNTAX_PATTERN   (&Scm_SyntaxPatternClass)

#define SCM_SYNTAX_PATTERN(obj)   ((ScmSyntaxPattern*)(obj))
#define SCM_SYNTAX_PATTERN_P(obj) SCM_XTYPEP(obj, SCM_CLASS_SYNTAX_PATTERN)

/*
 * SyntaxRules keeps a compiled rules of macro transformation.
 */

typedef struct ScmSyntaxRuleBranchRec {
    ScmObj pattern;             /* pattern to match */
    ScmObj template;            /* template to be expanded */
    int numPvars;               /* # of pattern variables */
} ScmSyntaxRuleBranch;

typedef struct ScmSyntaxRules {
    SCM_HEADER;
    ScmObj name;                  /* name of the macro (for debug) */
    int numRules;                 /* # of rules */
    int maxNumPvars;              /* max # of pattern variables */
    ScmSyntaxRuleBranch rules[1]; /* variable length */
} ScmSyntaxRules;

extern ScmClass Scm_SyntaxRulesClass;
#define SCM_CLASS_SYNTAX_RULES   (&Scm_SyntaxRulesClass)

#define SCM_SYNTAX_RULES(obj)    ((ScmSyntaxRules*)(obj))
#define SCM_SYNTAX_RULES_P(obj)  SCM_XTYPEP(obj, SCM_CLASS_SYNTAX_RULES)

#endif /* GAUCHE_MACRO_H */
