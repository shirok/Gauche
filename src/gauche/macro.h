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
 *  $Id: macro.h,v 1.1 2001-02-22 19:41:29 shiro Exp $
 */

#ifndef GAUCHE_SYNTAX_PATTERN_H
#define GAUCHE_SYNTAX_PATTERN_H

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

#endif /* GAUCHE_SYNTAX_PATTERN_H */
