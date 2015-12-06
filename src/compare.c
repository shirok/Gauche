/*
 * compare.c - comparison & sort
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

#include <stdlib.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"

/*
 * Comparator
 */

/* Unlike Scheme's make-comparator, TYPE, EQ, COMPARE and HASH arguments
   all must be a procedure.  */
ScmObj Scm_MakeComparator(ScmObj type, ScmObj eq,
                          ScmObj compare, ScmObj hash,
                          ScmObj name, u_long flags)
{
    ScmComparator *c = SCM_NEW(ScmComparator);
    SCM_SET_CLASS(c, &Scm_ComparatorClass);
    c->name = name;
    c->typeFn = type;
    c->eqFn = eq;
    c->compareFn = compare;
    c->hashFn = hash;
    c->flags = flags;
    return SCM_OBJ(c);
}

/*
 * Generic compare.
 */

int Scm_Compare(ScmObj x, ScmObj y)
{
    /* Shortcut for typical case */
    if (SCM_NUMBERP(x) && SCM_NUMBERP(y)) {
        if (SCM_COMPNUMP(x) || SCM_COMPNUMP(y)) {
            /* Scm_NumCmp can't compare complex numbers---it doesn't make
               mathematical sense.  But Scm_Compare is used just to order
               items, it doesn't need to carry meaning.  So here it goes.
               We follow srfi-114 spec. */
            /* TODO: If we ever introduce exact compnums, we should use
               exact number first to compare, for Scm_GetDouble may lose
               precision. */
            /* TODO: Handle NaN. */
            double xr = Scm_RealPart(x);
            double yr = Scm_RealPart(y);
            if (xr < yr) return -1;
            if (xr > yr) return 1;
            double xi = Scm_ImagPart(x);
            double yi = Scm_ImagPart(y);
            if (xi < yi) return -1;
            if (xi > yi) return 1;
            return 0;
        } else {
            return Scm_NumCmp(x, y);
        }
    }
    if (SCM_STRINGP(x) && SCM_STRINGP(y))
        return Scm_StringCmp(SCM_STRING(x), SCM_STRING(y));
    if (SCM_CHARP(x) && SCM_CHARP(y))
        return SCM_CHAR_VALUE(x) == SCM_CHAR_VALUE(y)? 0 :
            SCM_CHAR_VALUE(x) < SCM_CHAR_VALUE(y)? -1 : 1;

    /* Set cx, cy here, for we may jump to distinct_types later. */
    ScmClass *cx = Scm_ClassOf(x);
    ScmClass *cy = Scm_ClassOf(y);

    /* srfi-114 default comparator behaviors*/
    /* () is the smallest of all */
    if (SCM_NULLP(x)) return (SCM_NULLP(y)? 0 : -1);
    if (SCM_NULLP(y)) return (SCM_NULLP(x)? 0 : 1);
    if (SCM_PAIRP(x)) {
        if (SCM_PAIRP(y)) {
            ScmObj px = x;
            ScmObj py = y;
            while (SCM_PAIRP(px) && SCM_PAIRP(py)) {
                int r = Scm_Compare(SCM_CAR(px), SCM_CAR(py));
                if (r != 0) return r;
                px = SCM_CDR(px);
                py = SCM_CDR(py);
            }
            return Scm_Compare(px, py);
        }
        goto distinct_types;
    }
    if (SCM_FALSEP(x)) {
        if (SCM_FALSEP(y)) return  0;
        if (SCM_TRUEP(y)) return  -1;
        goto distinct_types;
    }
    if (SCM_TRUEP(x)) {
        if (SCM_FALSEP(y)) return  1;
        if (SCM_TRUEP(y)) return  0;
        goto distinct_types;
    }
    
    if (Scm_SubtypeP(cx, cy)) {
        if (cy->compare) return cy->compare(x, y, FALSE);
    } else if (Scm_SubtypeP(cy, cx)) {
        if (cx->compare) return cx->compare(x, y, FALSE);
    }
    if (cx == cy) {
        /* x and y are of the same type, and they can't be ordered. */
        Scm_Error("can't compare %S and %S", x, y);
    }
    
 distinct_types:
    /* x and y are of distinct types.  Follow the srfi-114 rule:
       () < pairs < booleans < chars < strings < symbols < numbers
          < vectors < bytevectors < others
       Note that we already eliminated NULL.
    */
#define ELIMINATE(pred) \
    do { \
        if pred(x) return -1;                   \
        if pred(y) return 1;                    \
    } while (0)

    ELIMINATE(SCM_PAIRP);
    ELIMINATE(SCM_BOOLP);
    ELIMINATE(SCM_CHARP);
    ELIMINATE(SCM_STRINGP);
    ELIMINATE(SCM_SYMBOLP);
    ELIMINATE(SCM_NUMBERP);
    ELIMINATE(SCM_VECTORP);

    /* To conform srfi-114, we must order u8vector first.  For the
       consistency, we use this order:
       u8 < s8 < u16 < s16 < u32 < s32 < u64 < s64 < f16 < f32 < f64
       Unfortunately this doesn't match the order of ScmUVectorType,
       so we need some tweak.
    */
    if (SCM_UVECTORP(x)) {
        if (SCM_UVECTORP(y)) {
            int tx = Scm_UVectorType(Scm_ClassOf(x));
            int ty = Scm_UVectorType(Scm_ClassOf(y));
            if (tx/2 < ty/2) return -1;
            if (tx/2 > ty/2) return 1;
            if (tx < SCM_UVECTOR_F16) {
                /* x and y are either sNvector and uNvector with the same N.
                   The odd one is uNvector.
                 */
                return (tx%2)? -1:1;
            } else {
                return (tx<ty)? -1:1;
            }
        }
        return -1;              /* y is other, so x comes first. */
    } else if (SCM_UVECTORP(y)) {
        return 1;               /* x is other, so y comes first. */
    }

    /* Now we have two objects of different types, both are not the
       types defined the order in srfi-114.
       To achieve better stability, we first compare the name of the
       classes and the names of their defining modules; if they are still
       the same, we fall back to compare addresses.
       Note: Addresses and defining modules may be changed when
       the class is redefined.
    */
    ScmObj nx = cx->name;
    ScmObj ny = cy->name;
    int nr = Scm_Compare(nx, ny);
    if (nr != 0) return nr;

    ScmObj mx = cx->modules;
    ScmObj my = cy->modules;
    while (SCM_PAIRP(mx) && SCM_PAIRP(my)) {
        SCM_ASSERT(SCM_MODULEP(SCM_CAR(mx)) && SCM_MODULEP(SCM_CAR(my)));
        int r = Scm_Compare(SCM_MODULE(SCM_CAR(mx))->name,
                            SCM_MODULE(SCM_CAR(my))->name);
        if (r != 0) return r;
        mx = SCM_CDR(mx);
        my = SCM_CDR(my);
    }
    if (SCM_PAIRP(mx)) return -1;
    if (SCM_PAIRP(my)) return 1;

    if (cx < cy) return -1;
    else return 1;
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
    for (int l=nelts/2-1; l>=0; l--) {
        shift_up(elts, l, nelts, cmp, data);
    }
    for (int r=nelts-1; r>=1; r--) {
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
            ScmObj pivot = elts[lo];
            while (l <= r) {
                while (l <= r && cmp(elts[l], pivot, data) < 0) l++;
                while (l <= r && cmp(pivot, elts[r], data) < 0) r--;
                if (l > r) break;
                ScmObj tmp = elts[l]; elts[l] = elts[r]; elts[r] = tmp;
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
    ScmObj starray[STATIC_SIZE];
    int len = STATIC_SIZE;
    ScmObj *array = Scm_ListToArray(objs, &len, starray, TRUE);
    Scm_SortArray(array, len, fn);
    if (destructive) {
        ScmObj cp = objs;
        for (int i=0; i<len; i++, cp = SCM_CDR(cp)) {
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

/*
 * Initialization
 */

void Scm__InitComparator()
{
    /* code that requires initialization has been moved to
       libcmp.scm and libomega.scm */
}
