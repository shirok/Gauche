/*
 * boolean.c
 *
 *   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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

#define LIBGAUCHE_BODY
#include "gauche.h"

int Scm_EqP(ScmObj x, ScmObj y)
{
    return SCM_EQ(x, y);
}

int Scm_EqvP(ScmObj x, ScmObj y)
{
    /* For our implementation, only numbers need different treatment
       than SCM_EQ.  We first check flonums, or we'd have to FLONUM_ENSURE_MEM
       before we pass them to Scm_NumEq.
    */
    if (SCM_NUMBERP(x)) {
        if (SCM_NUMBERP(y)) {
            /* Since flonums are the only "inexact real" type in Gauche,
               we can safely reject the cases where either one is flonum and
               another is not. */
            if (SCM_FLONUMP(x)) {
                if (SCM_FLONUMP(y)) {
                    return (SCM_FLONUM_VALUE(x) == SCM_FLONUM_VALUE(y));
                } else {
                    return FALSE;
                }
            } else if (SCM_FLONUMP(y)) {
                return FALSE;
            }
            /* More generic case. */
            if ((SCM_EXACTP(x) && SCM_EXACTP(y))
                || (SCM_INEXACTP(x) && SCM_INEXACTP(y))) {
                return Scm_NumEq(x, y);
            }
        }
        return FALSE;
    }
    return SCM_EQ(x, y);
}

int Scm_EqualP(ScmObj x, ScmObj y)
{
    ScmClass *cx, *cy;

    if (SCM_EQ(x, y)) return TRUE;
    if (SCM_PAIRP(x)) {
        if (!SCM_PAIRP(y)) return FALSE;
        do {
            if (!Scm_EqualP(SCM_CAR(x), SCM_CAR(y))) return FALSE;
            x = SCM_CDR(x);
            y = SCM_CDR(y);
        } while (SCM_PAIRP(x)&&SCM_PAIRP(y));
        return Scm_EqualP(x, y);
    }
    if (SCM_STRINGP(x)) {
        if (SCM_STRINGP(y)) {
            return Scm_StringEqual(SCM_STRING(x), SCM_STRING(y));
        }
        return FALSE;
    }
    if (SCM_NUMBERP(x)) {
        if (SCM_NUMBERP(y)) {
            if ((SCM_EXACTP(x) && SCM_EXACTP(y))
                || (SCM_INEXACTP(x) && SCM_INEXACTP(y))) {
                return Scm_NumEq(x, y);
            }
        }
        return FALSE;
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
    /* EXPERIMENTAL: when identifier is compared by equal?,
       we use its symbolic name to compare.  This allows
       comparing macro output with equal?, and also less confusing
       when R5RS macro and legacy macro are mixed.
       For "proper" comparison of identifiers keeping their semantics,
       we need such procedures as free-identifier=? and bound-identifier=?
       anyway, so this change of equal? won't have a negative impact, I hope.

       NB: this operation come here instead of the beginning of this
       procedure, since comparing identifiers are relatively rare so
       we don't want to check idnetifier-ness every time.
    */
    if (SCM_IDENTIFIERP(x) || SCM_IDENTIFIERP(y)) {
        if (SCM_IDENTIFIERP(x)) x = SCM_OBJ(SCM_IDENTIFIER(x)->name);
        if (SCM_IDENTIFIERP(y)) y = SCM_OBJ(SCM_IDENTIFIER(y)->name);
        return SCM_EQ(x, y);
    }
    /* End of EXPERIMENTAL code */

    if (!SCM_HPTRP(x)) return (x == y);
    cx = Scm_ClassOf(x);
    cy = Scm_ClassOf(y);
    if (cx == cy && cx->compare) {
        return (cx->compare(x, y, TRUE) == 0);
    }
    return FALSE;
}

int Scm_EqualM(ScmObj x, ScmObj y, int mode)
{
    switch (mode) {
    case SCM_CMP_EQ:
        return SCM_EQ(x, y);
    case SCM_CMP_EQV:
        return Scm_EqvP(x, y);
    case SCM_CMP_EQUAL:
        return Scm_EqualP(x, y);
    }
    return FALSE;
}
