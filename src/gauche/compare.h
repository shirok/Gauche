/*
 * compare.h - comparison & sort
 *
 *   Copyright (c) 2014-2022  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_COMPARE_H
#define GAUCHE_COMPARE_H


SCM_DECL_BEGIN

/* srfi-114/srfi-128 comparator */
struct ScmComparatorRec {
    SCM_HEADER;
    ScmObj name;                /* debugging aid */
    ScmObj typeFn;              /* proc */
    ScmObj eqFn;                /* proc */
    ScmObj compareFn;           /* proc */
    ScmObj hashFn;              /* proc */
    ScmObj orderFn;             /* proc */
    u_long flags;
};

/* Difference between srfi-114 and srfi-128:
   srfi-114 uses (compare a b) => -1 (a<b), 0 (a=b) or 1 (a>b)
   srfi-128 uses (order a b)   => #t (a<b) or #f (a>=b)
   We can emulate one by the other, but emulation has some overhead.
   So we allow both, and try to use whichever provided to the
   constructor.  (We do emulate if comparator-comparison-procedure
   is called on srfi-128-constructed comparator, etc.)

   Flags:

   SCM_COMPARATOR_NO_ORDER - The comparator can't order.
     (neither compare fn nor order fn is given to the constructor;
      note that compareFn and orderFn members have dummy procedure
      in that case).

   SCM_COMPARATOR_NO_HASH - The comparator can't hash.
     (hash fn is not given to the constructor; note that hashFn member
      has dummy procedure)

   SCM_COMPARATOR_ANY_TYPE - The comparator can accept any Scheme object.
     A small optimization to skip type tests.

   SCM_COMPARATOR_USE_COMPARISON - Records the fact that #t is passed
     to the equality-test in srfi-114 style constructor.  We use compareFn
     for the equality check.

   SCM_COMPARATOR_SRFI_128 - Indicates this is srfi-128-style comparator,
     so using orderFn is preferred to compareFn.
*/
enum ScmComparatorFlags {
    SCM_COMPARATOR_NO_ORDER = (1L<<0), /* 'compare' proc unavailable */
    SCM_COMPARATOR_NO_HASH  = (1L<<1), /* 'hash' proc unavailable */
    SCM_COMPARATOR_ANY_TYPE = (1L<<2), /* type-test always returns #t */
    SCM_COMPARATOR_USE_COMPARISON = (1L<<3), /* equality use comarison */
    SCM_COMPARATOR_SRFI_128 = (1L<<4)  /* srfi-128 style comparator */
};

SCM_CLASS_DECL(Scm_ComparatorClass);
#define SCM_CLASS_COMPARATOR   (&Scm_ComparatorClass)
#define SCM_COMPARATOR(obj)    ((ScmComparator*)(obj))
#define SCM_COMPARATORP(obj)   SCM_XTYPEP(obj, SCM_CLASS_COMPARATOR)

SCM_EXTERN ScmObj Scm_MakeComparator(ScmObj type, ScmObj eq,
                                     ScmObj compare_or_order, ScmObj hash,
                                     ScmObj name, u_long flags);
SCM_EXTERN ScmObj Scm_ComparatorComparisonProcedure(ScmComparator *);
SCM_EXTERN ScmObj Scm_ComparatorOrderingPredicate(ScmComparator *);
SCM_EXTERN ScmObj Scm_ComparatorHashFunction(ScmComparator *);

/* Other genreic utilities */
SCM_EXTERN int    Scm_Compare(ScmObj x, ScmObj y);
SCM_EXTERN void   Scm_SortArray(ScmObj *elts, int nelts, ScmObj cmpfn);
SCM_EXTERN ScmObj Scm_SortList(ScmObj objs, ScmObj fn);
SCM_EXTERN ScmObj Scm_SortListX(ScmObj objs, ScmObj fn);


SCM_DECL_END

#endif /*GAUCHE_COMPARE_H*/
