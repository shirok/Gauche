/*
 * compare.c - comparison & sort
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
 *  $Id: compare.c,v 1.2 2001-04-01 20:18:25 shiro Exp $
 */

#include <stdlib.h>
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

    cx = Scm_ClassOf(x);
    cy = Scm_ClassOf(y);
    if (Scm_SubtypeP(cx, cy)) {
        if (cy->compare) return cy->compare(x, y);
    } else {
        if (cx->compare) return cx->compare(x, y);
    }
    Scm_Error("can't compare %S and %S", x, y);
    return 0; /* dummy */
}

/*
 * Basic function for sort family.  An array pointed by elts will be
 * destructively sorted.  Cmpfn can be either an applicable Scheme
 * object or #f.  If it's an applicable object, two arguments x and y
 * will be passed to it, and the it must return an integer or a boolean
 * value, such that:
 *
 *  if (x < y), it may return a negative integer or #t.
 *  if (x == y), it may return 0 or #f.
 *  if (x > y), it may return a positive integer or #f.
 *
 * If cmpfn is #f, the first object's default compare method is used.
 */

static void sort_rec(ScmObj *elts, int lo, int hi,
                     int (*cmp)(ScmObj, ScmObj, ScmObj), ScmObj data)
{
    if (lo < hi) {
        int l = lo, r = hi;
        ScmObj pivot = elts[lo], tmp;
        while (l <= r) {
            while (cmp(elts[l], pivot, data) < 0) l++;
            while (cmp(pivot, elts[r], data) < 0) r--;
            if (l > r) break;
            tmp = elts[l]; elts[l] = elts[r]; elts[r] = tmp;
            l++;
            r--;
        }
        sort_rec(elts, lo, r, cmp, data);
        sort_rec(elts, l, hi, cmp, data);
    }
}

static int cmp_scm(ScmObj x, ScmObj y, ScmObj fn)
{
    ScmObj r = Scm_Apply(fn, SCM_LIST2(x, y));
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
    if (nelts <= 1) return;
    if (SCM_PROCEDUREP(cmpfn)) {
        sort_rec(elts, 0, nelts-1, cmp_scm, cmpfn);
    } else {
        sort_rec(elts, 0, nelts-1, cmp_int, NULL);
    }
}

/*
 * higher-level fns
 */

#define STATIC_SIZE 32

static ScmObj sort_list_int(ScmObj objs, ScmObj fn, int destructive)
{
    ScmObj cp;
    ScmObj starray[STATIC_SIZE], *array = starray;
    int len = Scm_Length(objs), i;
    if (len < 0) Scm_Error("improper list not allowed: %S", objs);
    if (len >= STATIC_SIZE)
        array = SCM_NEW2(ScmObj *, sizeof(ScmObj)*len);
    for (i=0, cp=objs; !SCM_NULLP(cp); i++, cp = SCM_CDR(cp)) {
        array[i] = SCM_CAR(cp);
    }
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
