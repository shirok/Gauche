/*
 * boolean.c
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
 *  $Id: boolean.c,v 1.14 2002-02-07 10:33:51 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"

int Scm_EqP(ScmObj x, ScmObj y)
{
    return (x == y);
}

int Scm_EqvP(ScmObj x, ScmObj y)
{
    /* for our implementation, only the number matters. */
    if (SCM_NUMBERP(x)) {
        if (SCM_NUMBERP(y)) return Scm_NumEq(x, y);
        else return FALSE;
    }
    return (x == y);
}

int Scm_EqualP(ScmObj x, ScmObj y)
{
    ScmClass *cx, *cy;
    if (SCM_PAIRP(x)) {
        if (SCM_PAIRP(y)) {
            if (Scm_EqualP(SCM_CAR(x), SCM_CAR(y))
                && Scm_EqualP(SCM_CDR(x), SCM_CDR(y)))
                return TRUE;
        }
        return FALSE;
    }
    if (SCM_STRINGP(x)) {
        if (SCM_STRINGP(y)) {
            return (Scm_StringCmp(SCM_STRING(x), SCM_STRING(y)) == 0);
        }
        return FALSE;
    }
    if (SCM_NUMBERP(x)) {
        if (SCM_NUMBERP(y)) return Scm_NumEq(x, y);
        else return FALSE;
    }
    if (SCM_VECTORP(x)) {
        if (SCM_VECTORP(y)) {
            int sizx = SCM_VECTOR_SIZE(x);
            int sizy = SCM_VECTOR_SIZE(y);
            if (sizx == sizy) {
                while (sizx--) {
                    if (!Scm_EqualP(SCM_VECTOR_ELEMENT(x, sizx),
                                    SCM_VECTOR_ELEMENT(y, sizx)))
                        break;
                }
                if (sizx < 0) return TRUE;
            }
        }
        return FALSE;
    }
    if (!SCM_PTRP(x)) return (x == y);
    cx = Scm_ClassOf(x);
    cy = Scm_ClassOf(y);
    if (cx == cy && cx->compare) {
        return (cx->compare(x, y) == 0);
    }
    return (x == y);
}

int Scm_EqualM(ScmObj x, ScmObj y, int mode)
{
    switch (mode) {
    case SCM_CMP_EQ:
        return x == y;
    case SCM_CMP_EQV:
        return Scm_EqvP(x, y);
    case SCM_CMP_EQUAL:
        return Scm_EqualP(x, y);
    }
    return FALSE;
}
