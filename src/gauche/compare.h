/*
 * compare.h - comparison & sort
 *
 *   Copyright (c) 2014-2015  Shiro Kawai  <shiro@acm.org>
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

/* srfi-114 comparator */
struct ScmComparatorRec {
    SCM_HEADER;
    ScmObj name;                /* debugging aid */
    ScmObj typeFn;              /* proc */
    ScmObj eqFn;                /* proc */
    ScmObj compareFn;           /* proc */
    ScmObj hashFn;              /* proc */
    u_long flags;
};

/* comparators can be constructed with #f for comareFn and/or hashFn.
   srfi-114 requires accessor functions to those slots returns dummy
   procedures that raises an error.  We still need to be able to tell
   these slots have valid procedures.  Instead of storing #f and tweak
   accessors to return a dummy procedure, we keep the info in flags.

   SCM_COMPARATOR_ANY_TYPE is a small optimization to bypass type test
   if possible.

   SCM_COMPARATOR_USE_COMPARISON is to record the fact that #t is passed
   to the equality-test.  The fact is only used to compare comparators.
*/
enum ScmComparatorFlags {
    SCM_COMPARATOR_NO_ORDER = (1L<<0), /* 'compare' proc unavailable */
    SCM_COMPARATOR_NO_HASH  = (1L<<1), /* 'hash' proc unavailable */
    SCM_COMPARATOR_ANY_TYPE = (1L<<2), /* type-test always returns #t */
    SCM_COMPARATOR_USE_COMPARISON = (1L<<3)  /* equality use comarison */
};

SCM_CLASS_DECL(Scm_ComparatorClass);
#define SCM_CLASS_COMPARATOR   (&Scm_ComparatorClass)
#define SCM_COMPARATOR(obj)    ((ScmComparator*)(obj))
#define SCM_COMPARATORP(obj)   SCM_XTYPEP(obj, SCM_CLASS_COMPARATOR)

SCM_EXTERN ScmObj  Scm_MakeComparator(ScmObj type, ScmObj eq,
                                      ScmObj compare, ScmObj hash,
                                      ScmObj name, u_long flags);

/* Other genreic utilities */
SCM_EXTERN int    Scm_Compare(ScmObj x, ScmObj y);
SCM_EXTERN void   Scm_SortArray(ScmObj *elts, int nelts, ScmObj cmpfn);
SCM_EXTERN ScmObj Scm_SortList(ScmObj objs, ScmObj fn);
SCM_EXTERN ScmObj Scm_SortListX(ScmObj objs, ScmObj fn);


SCM_DECL_END

#endif /*GAUCHE_COMPARE_H*/
