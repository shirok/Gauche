/*
 * compare.c - comparison & sort
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

#include <stdlib.h>
#define LIBGAUCHE_BODY
#include "gauche.h"

/*
 * Compare.
 */

int Scm_Compare(ScmObj x, ScmObj y)
{
    ScmClass *cx, *cy;

    /* Shortcut for typical case */
    if (SCM_NUMBERP(x) && SCM_NUMBERP(y))
        return Scm_NumCmp(x, y);
    if (SCM_STRINGP(x) && SCM_STRINGP(y))
        return Scm_StringCmp(SCM_STRING(x), SCM_STRING(y));
    if (SCM_CHARP(x) && SCM_CHARP(y))
        return SCM_CHAR_VALUE(x) == SCM_CHAR_VALUE(y)? 0 :
            SCM_CHAR_VALUE(x) < SCM_CHAR_VALUE(y)? -1 : 1;

    cx = Scm_ClassOf(x);
    cy = Scm_ClassOf(y);
    if (Scm_SubtypeP(cx, cy)) {
        if (cy->compare) return cy->compare(x, y, FALSE);
    } else {
        if (cx->compare) return cx->compare(x, y, FALSE);
    }
    Scm_Error("can't compare %S and %S", x, y);
    return 0; /* dummy */
}

/* NB: It turns out that calling back Scheme funtion from sort routine
   is very inefficient and runs much slower than Scheme version, if
   a Scheme comarison function is given.
   So, as of 0.7.2, the C function is only used when a comparison
   function is omitted. */

/*
 * Basic function for sort family.  An array pointed by elts will be
 * destructively sorted.  Cmpfn can be either an applicable Scheme
 * object or #f.  If it's an applicable object, two arguments x and y
 * will be passed to it, and it must return an integer or a boolean
 * value, such that:
 *
 *  if (x < y), it may return a negative integer or #t.
 *  if (x == y), it may return 0 or #f.
 *  if (x > y), it may return a positive integer or #f.
 *
 * If cmpfn is #f, the first object's default compare method is used.
 *
 * Some notes:
 *  - We can't use libc's qsort, since it doesn't pass closure to cmpfn.
 *  - The naive Quicksort behaves too badly in the worst case.
 *  - The comparison operation is far more costly than exchange.
 *
 * The current implementation is hybrid of Quicksort and Heapsort.  First
 * the algorithm proceeds by Quicksort, but when it detects the recursion
 * is too deep, it switches to Heapsort.  See Knuth, The Art of Computer
 * Programming Second Edition, Section 5.2.2, p.122.
 */

/* Heap sort */
static inline void shift_up(ScmObj *elts, int root, int nelts,
                            int (*cmp)(ScmObj, ScmObj, ScmObj), ScmObj data)
{
    int l = root+1, maxchild;
    while (l*2 <= nelts) {
        if (l*2 == nelts) {
            maxchild = nelts-1;
        } else if (cmp(elts[l*2-1], elts[l*2], data) < 0) {
            maxchild = l*2;
        } else {
            maxchild = l*2-1;
        }
        if (cmp(elts[l-1], elts[maxchild], data) < 0) {
            ScmObj tmp = elts[maxchild];
            elts[maxchild] = elts[l-1];
            elts[l-1] = tmp;
            l = maxchild+1;
        } else {
            break;
        }
    }
}

static void sort_h(ScmObj *elts, int nelts,
                   int (*cmp)(ScmObj, ScmObj, ScmObj), ScmObj data)
{
    int l, r;
    for (l=nelts/2-1; l>=0; l--) {
        shift_up(elts, l, nelts, cmp, data);
    }
    for (r=nelts-1; r>=1; r--) {
        ScmObj tmp = elts[r];
        elts[r] = elts[0];
        elts[0] = tmp;
        shift_up(elts, 0, r, cmp, data);
    }
}

/* Quick sort */
static void sort_q(ScmObj *elts, int lo, int hi, int depth, int limit,
                   int (*cmp)(ScmObj, ScmObj, ScmObj), ScmObj data)
{
    while (lo < hi) {
        if (depth >= limit) {
            sort_h(elts+lo, (hi-lo+1), cmp, data);
            break;
        } else {
            int l = lo, r = hi;
            ScmObj pivot = elts[lo], tmp;
            while (l <= r) {
                while (l <= r && cmp(elts[l], pivot, data) < 0) l++;
                while (l <= r && cmp(pivot, elts[r], data) < 0) r--;
                if (l > r) break;
                tmp = elts[l]; elts[l] = elts[r]; elts[r] = tmp;
                l++;
                r--;
            }
            if (lo < r) sort_q(elts, lo, r, depth+1, limit, cmp, data);
            /* tail call to
               sort_q(elts, l, hi, depth+1, limit, cmp, data); */
            lo = l;
            depth++;
        }
    }
}

static int cmp_scm(ScmObj x, ScmObj y, ScmObj fn)
{
    ScmObj r = Scm_ApplyRec(fn, SCM_LIST2(x, y));
    if (SCM_TRUEP(r) || (SCM_INTP(r) && SCM_INT_VALUE(r) < 0))
        return -1;
    else
        return 1;
}

static int cmp_int(ScmObj x, ScmObj y, ScmObj dummy)
{
    return Scm_Compare(x, y);
}

void Scm_SortArray(ScmObj *elts, int nelts, ScmObj cmpfn)
{
    int limit, i;
    if (nelts <= 1) return;
    /* approximate 2*log2(nelts) */
    for (i=nelts,limit=1; i > 0; limit++) {i>>=1;}
    if (SCM_PROCEDUREP(cmpfn)) {
        sort_q(elts, 0, nelts-1, 0, limit, cmp_scm, cmpfn);
    } else {
        sort_q(elts, 0, nelts-1, 0, limit, cmp_int, NULL);
    }
}

/*
 * higher-level fns
 */

#define STATIC_SIZE 32

static ScmObj sort_list_int(ScmObj objs, ScmObj fn, int destructive)
{
    ScmObj cp;
    ScmObj starray[STATIC_SIZE], *array;
    int len = STATIC_SIZE, i;
    array = Scm_ListToArray(objs, &len, starray, TRUE);
    Scm_SortArray(array, len, fn);
    if (destructive) {
        for (i=0, cp=objs; i<len; i++, cp = SCM_CDR(cp)) {
            SCM_SET_CAR(cp, array[i]);
        }
        return objs;
    } else {
        return Scm_ArrayToList(array, len);
    }
}

ScmObj Scm_SortList(ScmObj objs, ScmObj fn)
{
    return sort_list_int(objs, fn, FALSE);
}

ScmObj Scm_SortListX(ScmObj objs, ScmObj fn)
{
    return sort_list_int(objs, fn, TRUE);
}
