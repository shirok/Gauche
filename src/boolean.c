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
 *  $Id: boolean.c,v 1.6 2001-03-05 00:54:30 shiro Exp $
 */

#include "gauche.h"

int Scm_EqP(ScmObj x, ScmObj y)
{
    return (x == y);
}

int Scm_EqvP(ScmObj x, ScmObj y)
{
    /* for our implementation, only the number matters. */
    if (SCM_NUMBERP(x)) {
        if (SCM_NUMBERP(y)) return !SCM_FALSEP(Scm_NumEq(x, y, SCM_NIL));
        else return FALSE;
    }
    return (x == y);
}

int Scm_EqualP(ScmObj x, ScmObj y)
{
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
            return Scm_StringEqual(SCM_STRING(x), SCM_STRING(y));
        }
        return FALSE;
    }
    if (SCM_NUMBERP(x)) {
        if (SCM_NUMBERP(y)) return SCM_TRUEP(Scm_NumEq(x, y, SCM_NIL));
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
    return (x == y);
}



