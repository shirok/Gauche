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
 *  $Id: compare.c,v 1.1 2001-04-01 10:21:04 shiro Exp $
 */

#include <stdlib.h>
#include "gauche.h"

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

void Scm_SortArray(ScmObj *elts, int nelts, ScmObj cmpfn)
{
    /*write me*/
}

