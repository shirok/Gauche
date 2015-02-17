/*
 * boolean.c
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

/* Equal? needs to deal with circuler structures.
   We adopt the algorithm in Adams&Dybvig's "Efficient Nondestructive
   Equality Checking for Trees and Graphs",
   Proceedings of ICFP 08, pp. 179-188.

   It is much easier to write the algorithm in Scheme, but we don't want
   the overhead of crossing C-Scheme boundary for trivial cases.
   So we cover a simple cases (non-aggregates, user-defined objects and
   flat lists/vectors) in C, and fall back to Scheme routine if we encounter
   more complex structures.

   Caveat: The cycle may involve user-defined objects.  To detect such
   cycle, we need to pass down the context info to ScmClass.compare
   procedure and object-equal? method.  This change would break
   the backward compatibility, so we'll consider it in future versions.
   For now, let such cyclic structures explode.
*/

int Scm_EqualP(ScmObj x, ScmObj y)
{
#define CHECK_AGGREGATE(a, b)                   \
    do {                                        \
        if (SCM_PAIRP(a)) {                     \
            if (SCM_PAIRP(b)) goto fallback;    \
            return FALSE;                       \
        }                                       \
        if (SCM_VECTORP(a)) {                   \
            if (SCM_VECTORP(b)) goto fallback;  \
            return FALSE;                       \
        }                                       \
        if (!Scm_EqualP(a, b)) return FALSE;    \
    } while (0)                                 \

    if (SCM_EQ(x, y)) return TRUE;

    if (SCM_NUMBERP(x)) {
        if (!SCM_NUMBERP(y)) return FALSE;
        return Scm_EqvP(x, y);
    }
    if (SCM_PAIRP(x)) {
        if (!SCM_PAIRP(y)) return FALSE;
        /* We loop on "spine" of lists, so that the typical long flat list
           can be compared quickly.  If we find nested lists/vectors, we
           jump to Scheme routine.  We adopt hare and tortoise to detect
           loop in the CDR side. */
        ScmObj xslow = x; ScmObj yslow = y;
        int xcirc = FALSE; int ycirc = FALSE;
        for (;;) {
            ScmObj carx = SCM_CAR(x);
            ScmObj cary = SCM_CAR(y);
            CHECK_AGGREGATE(carx, cary);

            x = SCM_CDR(x); y = SCM_CDR(y);
            if (!SCM_PAIRP(x) || !SCM_PAIRP(y)) return Scm_EqualP(x, y);
            carx = SCM_CAR(x); cary = SCM_CAR(y);

            CHECK_AGGREGATE(carx, cary);

            if (xslow == x) {
                if (ycirc) return TRUE;
                xcirc = TRUE;
            }
            if (yslow == y) {
                if (xcirc) return TRUE;
                ycirc = TRUE;
            }

            x = SCM_CDR(x); y = SCM_CDR(y);
            if (!SCM_PAIRP(x) || !SCM_PAIRP(y)) return Scm_EqualP(x, y);
            xslow = SCM_CDR(xslow); yslow = SCM_CDR(yslow);
        }
    }
    if (SCM_VECTORP(x)) {
        if (!SCM_VECTORP(y)) return FALSE;
        ScmWord i = 0, len = SCM_VECTOR_SIZE(x);
        if (SCM_VECTOR_SIZE(y) != len) return FALSE;
        for (; i < len; i++) {
            ScmObj xx = SCM_VECTOR_ELEMENT(x, i);
            ScmObj yy = SCM_VECTOR_ELEMENT(y, i);
            /* NB: If we detect nested structure in middle of vectors,
               we run Scheme routine for the entire vector; so we'll test
               equality of elements before the current one again.  If
               they are simple objects that's negligible, but there may
               be objects of user-defined datatypes, for which object-equal?
               could be expensive; we'll fix it in future. */
            CHECK_AGGREGATE(xx, yy);
        }
        return TRUE;
    }
    if (SCM_STRINGP(x)) {
        if (!SCM_STRINGP(y)) return FALSE;
        return Scm_StringEqual(SCM_STRING(x), SCM_STRING(y));
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
    ScmClass *cx = Scm_ClassOf(x);
    ScmClass *cy = Scm_ClassOf(y);
    if (cx == cy && cx->compare) return (cx->compare(x, y, TRUE) == 0);
    else                         return FALSE;

 fallback: 
    {
        /* Fall back to Scheme version. */
        static ScmObj equal_interleave_proc = SCM_UNDEFINED;
        SCM_BIND_PROC(equal_interleave_proc, "%interleave-equal?",
                      Scm_GaucheInternalModule());
        return !SCM_FALSEP(Scm_ApplyRec2(equal_interleave_proc, x, y));
    }
#undef CHECK_AGGREGATE
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
