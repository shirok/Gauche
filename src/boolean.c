/*
 * boolean.c
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
 *  $Id: boolean.c,v 1.3 2001-02-05 10:04:17 shiro Exp $
 */

#include "gauche.h"

ScmObj Scm_EqP(ScmObj x, ScmObj y)
{
    return SCM_EQ(x, y)? SCM_TRUE : SCM_FALSE;
}

ScmObj Scm_EqvP(ScmObj x, ScmObj y)
{
    /* for our implementation, only the number matters. */
    if (SCM_NUMBERP(x)) {
        if (SCM_NUMBERP(y)) return Scm_NumEq(x, y, SCM_NIL);
        else return SCM_FALSE;
    }
    return SCM_EQ(x, y)? SCM_TRUE : SCM_FALSE;
}

ScmObj Scm_EqualP(ScmObj x, ScmObj y)
{
    if (SCM_PAIRP(x)) {
        if (SCM_PAIRP(y)) {
            if (Scm_EqualP(SCM_CAR(x), SCM_CAR(y))
                && Scm_EqualP(SCM_CDR(x), SCM_CDR(y)))
                return SCM_TRUE;
        }
        return SCM_FALSE;
    }
    if (SCM_STRINGP(x)) {
        if (SCM_STRINGP(y)) {
            return Scm_StringEqual(SCM_STRING(x), SCM_STRING(y));
        }
        return SCM_FALSE;
    }
    if (SCM_NUMBERP(x)) {
        if (SCM_NUMBERP(y)) return Scm_NumEq(x, y, SCM_NIL);
        else return SCM_FALSE;
    }
    if (SCM_VECTORP(x)) {
        if (SCM_VECTORP(y)) {
            int sizx = SCM_VECTOR_SIZE(x);
            int sizy = SCM_VECTOR_SIZE(y);
            if (sizx == sizy) {
                while (sizx--) {
                    if (Scm_EqualP(SCM_VECTOR_ELEMENT(x, sizx),
                                   SCM_VECTOR_ELEMENT(y, sizx))
                        == SCM_FALSE){
                        break;
                    }
                }
                if (sizx < 0) return SCM_TRUE;
            }
        }
        return SCM_FALSE;
    }
    return SCM_EQ(x, y)? SCM_TRUE : SCM_FALSE;
}



