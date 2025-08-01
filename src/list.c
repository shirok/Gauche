/*
 * list.c - List related functions
 *
 *   Copyright (c) 2000-2025  Shiro Kawai  <shiro@acm.org>
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
#include "gauche/priv/pairP.h"

/*
 * Classes
 */

static ScmClass *list_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_ListClass),
    SCM_CLASS_STATIC_PTR(Scm_SequenceClass),
    SCM_CLASS_STATIC_PTR(Scm_CollectionClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

SCM_DEFINE_BUILTIN_CLASS(Scm_ListClass, NULL, NULL, NULL, NULL, list_cpl+1);
SCM_DEFINE_BUILTIN_CLASS(Scm_PairClass, NULL, NULL, NULL, NULL, list_cpl);
SCM_DEFINE_BUILTIN_CLASS(Scm_NullClass, NULL, NULL, NULL, NULL, list_cpl);

/*
 * CONSTRUCTOR
 */

ScmObj Scm_Cons(ScmObj car, ScmObj cdr)
{
    ScmPair *z = SCM_NEW(ScmPair);
    /* NB: these ENSURE_MEMs are moved here from vm loop to reduce
       the register pressure there.  In most cases these increases
       just a couple of mask-and-test instructions on the data on
       the register. */
    SCM_FLONUM_ENSURE_MEM(car);
    SCM_FLONUM_ENSURE_MEM(cdr);
    SCM_SET_CAR_UNCHECKED(z, car);
    SCM_SET_CDR_UNCHECKED(z, cdr);
    return SCM_OBJ(z);
}

ScmObj Scm_Acons(ScmObj caar, ScmObj cdar, ScmObj cdr)
{
    ScmPair *y = SCM_NEW(ScmPair);
    ScmPair *z = SCM_NEW(ScmPair);
    SCM_SET_CAR_UNCHECKED(y, caar);
    SCM_SET_CDR_UNCHECKED(y, cdar);
    SCM_SET_CAR_UNCHECKED(z, SCM_OBJ(y));
    SCM_SET_CDR_UNCHECKED(z, cdr);
    return SCM_OBJ(z);
}

ScmObj Scm_List(ScmObj elt, ...)
{
    if (elt == NULL) return SCM_NIL;

    va_list pvar;
    va_start(pvar, elt);
    ScmObj cdr = Scm_VaList(pvar);
    va_end(pvar);
    return Scm_Cons(elt, cdr);
}


ScmObj Scm_Conses(ScmObj elt, ...)
{
    if (elt == NULL) return SCM_NIL;

    va_list pvar;
    va_start(pvar, elt);
    ScmObj cdr = Scm_VaCons(pvar);
    va_end(pvar);
    if (cdr == NULL) return elt;
    else             return Scm_Cons(elt, cdr);
}


ScmObj Scm_VaList(va_list pvar)
{
    ScmObj start = SCM_NIL, cp = SCM_NIL, obj;

    for (obj = va_arg(pvar, ScmObj);
         obj != NULL;
         obj = va_arg(pvar, ScmObj))
    {
        if (SCM_NULLP(start)) {
            start = SCM_OBJ(SCM_NEW(ScmPair));
            SCM_SET_CAR_UNCHECKED(start, obj);
            SCM_SET_CDR_UNCHECKED(start, SCM_NIL);
            cp = start;
        } else {
            ScmObj item;
            item = SCM_OBJ(SCM_NEW(ScmPair));
            SCM_SET_CDR_UNCHECKED(cp, item);
            SCM_SET_CAR_UNCHECKED(item, obj);
            SCM_SET_CDR_UNCHECKED(item, SCM_NIL);
            cp = item;
        }
    }
    return start;
}


ScmObj Scm_VaCons(va_list pvar SCM_UNUSED)
{
    Scm_Panic("Scm_VaCons: not implemented");
    return SCM_UNDEFINED;
}

ScmObj Scm_ArrayToList(ScmObj *elts, ScmSize nelts)
{
    return Scm_ArrayToListWithTail(elts, nelts, SCM_NIL);
}

ScmObj Scm_ArrayToListWithTail(ScmObj *elts, ScmSize nelts, ScmObj tail)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    if (elts) {
        for (ScmSize i=0; i<nelts; i++) SCM_APPEND1(h, t, *elts++);
    }
    if (!SCM_NULLP(tail)) SCM_APPEND(h, t, tail);
    return h;
}

ScmObj *Scm_ListToArray(ScmObj list, ScmSize *nelts, ScmObj *store, int alloc)
{
    ScmSize len = Scm_Length(list);
    if (len < 0) Scm_Error("proper list required, but got %S", list);

    ScmObj *array;
    if (store == NULL) {
        array = SCM_NEW_ARRAY(ScmObj, len);
    } else {
        if (*nelts < len) {
            if (!alloc)
                Scm_Error("ListToArray: storage too small");
            array = SCM_NEW_ARRAY(ScmObj, len);
        } else {
            array = store;
        }
    }
    ScmSize i = 0;
    for (ScmObj lp=list; i<len; i++, lp=SCM_CDR(lp)) {
        array[i] = SCM_CAR(lp);
    }
    *nelts = len;
    return array;
}

/* cXr stuff */

#define CXR(cname, sname, body)                 \
ScmObj cname (ScmObj obj)                       \
{                                               \
   ScmObj obj2 = obj;                           \
   body                                         \
   return obj2;                                 \
}

#define A                                                       \
   if (!SCM_PAIRP(obj2)) Scm_Error("bad object: %S", obj);      \
   obj2 = SCM_CAR(obj2);

#define D                                                       \
   if (!SCM_PAIRP(obj2)) Scm_Error("bad object: %S", obj);      \
   obj2 = SCM_CDR(obj2);

CXR(Scm_Car, "car", A)
CXR(Scm_Cdr, "cdr", D)
CXR(Scm_Caar, "caar", A A)
CXR(Scm_Cadr, "cadr", D A)
CXR(Scm_Cdar, "cdar", A D)
CXR(Scm_Cddr, "cddr", D D)

void Scm_SetCar(ScmObj pair, ScmObj obj)
{
    if (!SCM_PAIRP(pair)) {
        Scm_Error("set-car!: Pair required, but got: %S", pair);
    }
    ScmExtendedPairDescriptor *d = Scm__GetExtendedPairDescriptor(pair);
    if (d) {
        if (d->flags & SCM_PAIR_IMMUTABLE) {
            Scm_Error("attempt to mutate car of an immutable pair %S with %S",
                      pair, obj);
        }
        if (d->setCar) {
            d->setCar(pair, obj);
            return;
        }
    }
    SCM_CAR(pair) = obj;
}

void Scm_SetCdr(ScmObj pair, ScmObj obj)
{
    if (!SCM_PAIRP(pair)) {
        Scm_Error("set-cdr!: Pair required, but got: %S", pair);
    }
    ScmExtendedPairDescriptor *d = Scm__GetExtendedPairDescriptor(pair);
    if (d) {
        if (d->flags & SCM_PAIR_IMMUTABLE) {
            Scm_Error("attempt to mutate cdr of an immutable pair %S with %S",
                      pair, obj);
        }
        if (d->setCdr) {
            d->setCdr(pair, obj);
            return;
        }
    }
    SCM_CDR(pair) = obj;
}

/*
 * List manipulate routines:
 */

/* Scm_Length
   return length of list in C integer.
   If the argument is a dotted list, return -1.
   If the argument is a circular list, return -2. */

ScmSize Scm_Length2(ScmObj obj, u_long flags)
{
    ScmObj slow = obj;
    ScmSize len = 0;
    _Bool leave_lazy = flags & SCM_LENGTH_LAZY;

    for (;;) {
        if (SCM_NULLP(obj)) break;
        if (leave_lazy && SCM_LAZY_PAIR_P(obj)) return SCM_LIST_LAZY;
        if (!SCM_PAIRP(obj)) return SCM_LIST_DOTTED;

        obj = SCM_CDR(obj);
        len++;
        if (SCM_NULLP(obj)) break;
        if (leave_lazy && SCM_LAZY_PAIR_P(obj)) return SCM_LIST_LAZY;
        if (!SCM_PAIRP(obj)) return SCM_LIST_DOTTED;

        obj = SCM_CDR(obj);
        slow = SCM_CDR(slow);
        if (obj == slow) return SCM_LIST_CIRCULAR;
        len++;
    }
    return len;
}

/* for the compatibility */
ScmSize Scm_Length(ScmObj obj)
{
    return Scm_Length2(obj, 0);
}

/* Scm_CopyList(list)
 *   Copy toplevel list LIST.  LIST can be improper.
 *   If LIST is not a pair, return LIST itself.
 *   Note: For the naming consistency, this should be Scm_ListCopy().
 */

ScmObj Scm_CopyList(ScmObj list)
{
    if (!SCM_PAIRP(list)) return list;
    ScmObj tortoise = list, hare = list;

    ScmObj start = SCM_NIL, last = SCM_NIL;
    for (;;) {
        if (!SCM_PAIRP(hare)) break;
        SCM_APPEND1(start, last, SCM_CAR(hare));
        hare = SCM_CDR(hare);

        if (!SCM_PAIRP(hare)) break;
        SCM_APPEND1(start, last, SCM_CAR(hare));
        hare = SCM_CDR(hare);
        tortoise = SCM_CDR(tortoise);
        if (hare == tortoise) {
            Scm_Error("Attempt to copy a circular list: %S", list);
        }
    }
    if (!SCM_NULLP(hare)) SCM_SET_CDR_UNCHECKED(last, hare);
    return start;
}

/* Scm_AlistCopy(alist)
 *   Copy the spine and each pair of alist.  If there's non-pair element
 *   it's included as is.  We detect a cycle.
 *   NB: We won't allow 'dotted' alist.
 */
ScmObj Scm_AlistCopy(ScmObj alist)
{
    if (!SCM_PAIRP(alist)) return alist;
    ScmObj tortoise = alist, hare = alist;

    ScmObj start = SCM_NIL, last = SCM_NIL;
    for (;;) {
        if (!SCM_PAIRP(hare)) break;
        if (SCM_PAIRP(SCM_CAR(hare))) {
            ScmObj p = Scm_Cons(SCM_CAAR(hare), SCM_CDAR(hare));
            SCM_APPEND1(start, last, p);
        } else {
            SCM_APPEND1(start, last, SCM_CAR(hare));
        }
        hare = SCM_CDR(hare);

        if (!SCM_PAIRP(hare)) break;
        if (SCM_PAIRP(SCM_CAR(hare))) {
            ScmObj p = Scm_Cons(SCM_CAAR(hare), SCM_CDAR(hare));
            SCM_APPEND1(start, last, p);
        } else {
            SCM_APPEND1(start, last, SCM_CAR(hare));
        }
        hare = SCM_CDR(hare);

        tortoise = SCM_CDR(tortoise);
        if (hare == tortoise) {
            Scm_Error("Attempt to copy a circular alist: %S", alist);
        }
    }
    if (!SCM_NULLP(hare)) Scm_Error("Dotted alist isn't allowed: %S", alist);
    return start;
}


/* Scm_MakeList(len, fill)
 *    Make a list of specified length.
 *    Note that <len> is C-integer.
 */

ScmObj Scm_MakeList(ScmSmallInt len, ScmObj fill)
{
    if (len < 0) {
        Scm_Error("make-list: negative length given: %ld", len);
    }
    ScmObj start = SCM_NIL, last = SCM_NIL;
    while (len--) {
        SCM_APPEND1(start, last, fill);
    }
    return start;
}


/* Scm_Append2X(list, obj)
 *    Replace cdr of last pair of LIST for OBJ.
 *    If LIST is not a pair, return OBJ.
 */

ScmObj Scm_Append2X(ScmObj list, ScmObj obj)
{
    ScmObj cp;
    SCM_FOR_EACH(cp, list) {
        if (SCM_NULLP(SCM_CDR(cp))) {
            Scm_SetCdr(cp, obj);
            return list;
        }
    }
    return obj;
}

/* Scm_Append2(list, obj)
 *   Copy LIST and append OBJ to it.
 *   If LIST is not a pair, return OBJ.
 */

ScmObj Scm_Append2(ScmObj list, ScmObj obj)
{
    if (SCM_NULLP(list)) return obj;

    ScmObj cp, start = SCM_NIL, last = SCM_NIL;
    SCM_FOR_EACH(cp, list) {
        SCM_APPEND1(start, last, SCM_CAR(cp));
    }
    if (!SCM_NULLP(cp)) {
        Scm_Error("proper list required, but got %S", list);
    }
    SCM_SET_CDR_UNCHECKED(last, obj);

    return start;
}

ScmObj Scm_Append(ScmObj args)
{
    ScmObj start = SCM_NIL, last = SCM_NIL, cp;
    SCM_FOR_EACH(cp, args) {
        if (!SCM_PAIRP(SCM_CDR(cp))) {
            if (SCM_NULLP(start)) return SCM_CAR(cp);
            SCM_SET_CDR_UNCHECKED(last, SCM_CAR(cp));
            break;
        } else if (SCM_NULLP(SCM_CAR(cp))) {
            continue;
        } else if (!SCM_PAIRP(SCM_CAR(cp))) {
            Scm_Error("pair required, but got %S", SCM_CAR(cp));
        } else {
            SCM_APPEND(start, last, Scm_CopyList(SCM_CAR(cp)));
            if (SCM_PAIRP(last) && !SCM_NULLP(SCM_CDR(last))) {
                Scm_Error("proper list required, but got %S", SCM_CAR(cp));
            }
        }
    }
    return start;
}

/* Scm_Reverse2(list, tail)
 *    Reverse LIST, and append TAIL to the result.
 *    If LIST is an improper list, cdr of the last pair is ignored.
 *    If LIST is not a pair, TAIL is returned.
 * Scm_Reverse(list)
 *    Scm_Reverse2(list, SCM_NIL).  Just for the backward compatibility.
 */

ScmObj Scm_Reverse2(ScmObj list, ScmObj tail)
{
    if (!SCM_PAIRP(list)) return tail;

    ScmPair *p = SCM_NEW(ScmPair);
    SCM_SET_CAR_UNCHECKED(p, SCM_NIL);
    SCM_SET_CDR_UNCHECKED(p, tail);
    ScmObj result = SCM_OBJ(p);
    ScmObj cp;
    SCM_FOR_EACH(cp, list) {
        SCM_SET_CAR_UNCHECKED(result, SCM_CAR(cp));
        p = SCM_NEW(ScmPair);
        SCM_SET_CAR_UNCHECKED(p, SCM_NIL);
        SCM_SET_CDR_UNCHECKED(p, result);
        result = SCM_OBJ(p);
    }
    return SCM_CDR(result);
}

ScmObj Scm_Reverse(ScmObj list)
{
    return Scm_Reverse2(list, SCM_NIL);
}


/* Scm_Reverse2X(list, tail)
 *   Return reversed list of LIST.  Pairs in previous LIST is used to
 *   create new list.  TAIL is appended to the result.
 *   If LIST is not a pair, returns TAIL.
 *   If LIST is an improper list, cdr of the last cell is ignored.
 */

ScmObj Scm_Reverse2X(ScmObj list, ScmObj tail)
{
    if (!SCM_PAIRP(list)) return tail;
    ScmObj first, next, result = tail;
    for (first = list; SCM_PAIRP(first); first = next) {
        next = SCM_CDR(first);
        Scm_SetCdr(first, result);
        result = first;
    }
    return result;
}

ScmObj Scm_ReverseX(ScmObj list)
{
    return Scm_Reverse2X(list, SCM_NIL);
}

/* Scm_ListTail(list, i, fallback)
 * Scm_ListRef(list, i, fallback)
 *    Note that i is C-INTEGER.  If i is out of bound, signal error.
 */

ScmObj Scm_ListTail(ScmObj list, ScmSmallInt i, ScmObj fallback)
{
    if (i < 0) goto err;
    ScmSmallInt cnt = i;
    while (cnt-- > 0) {
        if (!SCM_PAIRP(list)) goto err;
        list = SCM_CDR(list);
    }
    return list;
  err:
    if (SCM_UNBOUNDP(fallback)) Scm_Error("argument out of range: %ld", i);
    return fallback;
}

ScmObj Scm_ListRef(ScmObj list, ScmSmallInt i, ScmObj fallback)
{
    if (i < 0) goto err;
    for (ScmSmallInt k=0; k<i; k++) {
        if (!SCM_PAIRP(list)) goto err;
        list = SCM_CDR(list);
    }
    if (!SCM_PAIRP(list)) goto err;
    return SCM_CAR(list);
  err:
    if (SCM_UNBOUNDP(fallback)) {
        Scm_Error("argument out of range: %ld", i);
    }
    return fallback;
}

/* Scm_LastPair(l)
 *   Return last pair of (maybe improper) list L.
 *   If L is not a pair, signal error.
 */

ScmObj Scm_LastPair(ScmObj l)
{
    if (!SCM_PAIRP(l)) Scm_Error("pair required: %S", l);

    ScmObj slow = l;
    for (;;) {
        if (!SCM_PAIRP(SCM_CDR(l))) return l;
        l = SCM_CDR(l);
        if (!SCM_PAIRP(SCM_CDR(l))) return l;
        l = SCM_CDR(l);
        slow = SCM_CDR(slow);
        if (l == slow) Scm_Error("list is circular: %S", l);
    }
    return SCM_UNDEFINED;       /* NOTREACHED */
}

/* Scm_Memq(obj, list)
 * Scm_Memv(obj, list)
 * Scm_Member(obj, list)
 *    LIST must be a list.  Return the first sublist whose car is obj.
 *    If obj doesn't occur in LIST, or LIST is not a pair, #f is returned.
 */

ScmObj Scm_Memq(ScmObj obj, ScmObj list)
{
    SCM_FOR_EACH(list, list) if (obj == SCM_CAR(list)) return list;
    return SCM_FALSE;
}

ScmObj Scm_Memv(ScmObj obj, ScmObj list)
{
    SCM_FOR_EACH(list, list) {
        if (Scm_EqvP(obj, SCM_CAR(list))) return list;
    }
    return SCM_FALSE;
}

ScmObj Scm_Member(ScmObj obj, ScmObj list, int cmpmode)
{
    SCM_FOR_EACH(list, list) {
        if (Scm_EqualM(obj, SCM_CAR(list), cmpmode)) return list;
    }
    return SCM_FALSE;
}

/* delete. */
ScmObj Scm_Delete(ScmObj obj, ScmObj list, int cmpmode)
{
    if (SCM_NULLP(list)) return SCM_NIL;

    ScmObj start = SCM_NIL, last = SCM_NIL, cp, prev = list;
    SCM_FOR_EACH(cp, list) {
        if (Scm_EqualM(obj, SCM_CAR(cp), cmpmode)) {
            for (; prev != cp; prev = SCM_CDR(prev))
                SCM_APPEND1(start, last, SCM_CAR(prev));
            prev = SCM_CDR(cp);
        }
    }
    if (list == prev) return list;
    if (SCM_NULLP(start)) return prev;
    if (SCM_PAIRP(prev)) SCM_SET_CDR(last, prev);
    return start;
}

ScmObj Scm_DeleteX(ScmObj obj, ScmObj list, int cmpmode)
{
    ScmObj cp, prev = SCM_NIL;
    SCM_FOR_EACH(cp, list) {
        if (Scm_EqualM(obj, SCM_CAR(cp), cmpmode)) {
            if (SCM_NULLP(prev)) {
                list = SCM_CDR(cp);
            } else {
                Scm_SetCdr(prev, SCM_CDR(cp));
            }
        } else {
            prev = cp;
        }
    }
    return list;
}


/*
 * assq, assv, assoc
 *    ALIST must be a list of pairs.  Return the first pair whose car
 *    is obj.  If ALIST contains non pair, it's silently ignored.
 */

ScmObj Scm_Assq(ScmObj obj, ScmObj alist)
{
    if (!SCM_LISTP(alist)) Scm_Error("assq: list required, but got %S", alist);
    ScmObj cp;
    SCM_FOR_EACH(cp,alist) {
        ScmObj entry = SCM_CAR(cp);
        if (!SCM_PAIRP(entry)) continue;
        if (obj == SCM_CAR(entry)) return entry;
    }
    return SCM_FALSE;
}

ScmObj Scm_Assv(ScmObj obj, ScmObj alist)
{
    if (!SCM_LISTP(alist)) Scm_Error("assv: list required, but got %S", alist);
    ScmObj cp;
    SCM_FOR_EACH(cp,alist) {
        ScmObj entry = SCM_CAR(cp);
        if (!SCM_PAIRP(entry)) continue;
        if (Scm_EqvP(obj, SCM_CAR(entry))) return entry;
    }
    return SCM_FALSE;
}

ScmObj Scm_Assoc(ScmObj obj, ScmObj alist, int cmpmode)
{
    if (!SCM_LISTP(alist)) Scm_Error("assoc: list required, but got %S", alist);
    ScmObj cp;
    SCM_FOR_EACH(cp,alist) {
        ScmObj entry = SCM_CAR(cp);
        if (!SCM_PAIRP(entry)) continue;
        if (Scm_EqualM(obj, SCM_CAR(entry), cmpmode)) return entry;
    }
    return SCM_FALSE;
}

/* Assoc-delete */
ScmObj Scm_AssocDelete(ScmObj elt, ScmObj alist, int cmpmode)
{
    if (!SCM_LISTP(alist)) {
        Scm_Error("assoc-delete: list required, but got %S", alist);
    }
    if (SCM_NULLP(alist)) return SCM_NIL;

    ScmObj start = SCM_NIL, last = SCM_NIL, cp, p, prev = alist;
    SCM_FOR_EACH(cp, alist) {
        p = SCM_CAR(cp);
        if (SCM_PAIRP(p)) {
            if (Scm_EqualM(elt, SCM_CAR(p), cmpmode)) {
                for (; prev != cp; prev = SCM_CDR(prev))
                    SCM_APPEND1(start, last, SCM_CAR(prev));
                prev = SCM_CDR(cp);
            }
        }
    }
    if (alist == prev) return alist;
    if (SCM_NULLP(start)) return prev;
    if (SCM_PAIRP(prev)) SCM_SET_CDR(last, prev);
    return start;
}

ScmObj Scm_AssocDeleteX(ScmObj elt, ScmObj alist, int cmpmode)
{
    if (!SCM_LISTP(alist)) {
        Scm_Error("assoc-delete!: list required, but got %S", alist);
    }
    ScmObj cp, prev = SCM_NIL;
    SCM_FOR_EACH(cp, alist) {
        ScmObj e = SCM_CAR(cp);
        if (SCM_PAIRP(e)) {
            if (Scm_EqualM(elt, SCM_CAR(e), cmpmode)) {
                if (SCM_NULLP(prev)) {
                    alist = SCM_CDR(cp);
                    continue;
                } else {
                    Scm_SetCdr(prev, SCM_CDR(cp));
                }
            }
        }
        prev = cp;
    }
    return alist;
}

/* DeleteDuplicates.  preserve the order of original list.   N^2 algorithm */

ScmObj Scm_DeleteDuplicates(ScmObj list, int cmpmode)
{
    ScmObj result = SCM_NIL, tail = SCM_NIL, lp;
    SCM_FOR_EACH(lp, list) {
        if (SCM_FALSEP(Scm_Member(SCM_CAR(lp), result, cmpmode))) {
            SCM_APPEND1(result, tail, SCM_CAR(lp));
        }
    }
    if (!SCM_NULLP(lp) && !SCM_NULLP(tail)) SCM_SET_CDR(tail, lp);
    return result;
}

ScmObj Scm_DeleteDuplicatesX(ScmObj list, int cmpmode)
{
    ScmObj lp;

    SCM_FOR_EACH(lp, list) {
        ScmObj obj = SCM_CAR(lp);
        ScmObj tail = Scm_DeleteX(obj, SCM_CDR(lp), cmpmode);
        if (SCM_CDR(lp) != tail) Scm_SetCdr(lp, tail);
    }
    return list;
}

/*
 * Monotonic Merge
 *
 *  Merge lists, keeping the order of elements (left to right) in each
 *  list.   If there's more than one way to order an element, choose the
 *  first one appears in the given list of lists.
 *  Returns SCM_FALSE if the lists are inconsistent to be ordered
 *  in the way.
 *
 *  START is an item of the starting point.  It is inserted into the result
 *  first.  SEQUENCES is a list of lists describing the order of preference.
 *
 *  The algorithm is used in C3 linearization of class precedence
 *  calculation, described in the paper
 *    http://www.webcom.com/~haahr/dylan/linearization-oopsla96.html.
 *  Since the algorithm is generally useful, I implement the core routine
 *  of the algorithm here.
 */

ScmObj Scm_MonotonicMerge(ScmObj sequences)
{
    ScmObj result = SCM_NIL;
    int nseqs = Scm_Length(sequences);
    if (nseqs < 0) Scm_Error("bad list of sequences: %S", sequences);
    ScmObj *seqv = SCM_NEW_ARRAY(ScmObj, nseqs);
    for (ScmObj *sp=seqv;
         SCM_PAIRP(sequences);
         sp++, sequences=SCM_CDR(sequences)) {
        *sp = SCM_CAR(sequences);
    }

    for (;;) {
        /* have we consumed all the inputs? */
        ScmObj *sp;
        for (sp=seqv; sp<seqv+nseqs; sp++) {
            if (!SCM_NULLP(*sp)) break;
        }
        if (sp == seqv+nseqs) return Scm_ReverseX(result);

        /* select candidate */
        ScmObj next = SCM_FALSE;
        for (sp = seqv; sp < seqv+nseqs; sp++) {
            if (!SCM_PAIRP(*sp)) continue;
            ScmObj h = SCM_CAR(*sp);
            ScmObj *tp;
            for (tp = seqv; tp < seqv+nseqs; tp++) {
                if (!SCM_PAIRP(*tp)) continue;
                if (!SCM_FALSEP(Scm_Memq(h, SCM_CDR(*tp)))) {
                    break;
                }
            }
            if (tp != seqv+nseqs) continue;
            next = h;
            break;
        }

        if (SCM_FALSEP(next)) return SCM_FALSE; /* inconsistent */

        /* move the candidate to the result */
        result = Scm_Cons(next, result);
        for (sp = seqv; sp < seqv+nseqs; sp++) {
            if (SCM_PAIRP(*sp) && SCM_EQ(next, SCM_CAR(*sp))) {
                *sp = SCM_CDR(*sp);
            }
        }
    }
    /* NOTREACHED */
}

/*
 * Extended pairs
 */

static ScmObj make_extended_pair(ScmExtendedPairDescriptor *desc,
                                 ScmObj car, ScmObj cdr, ScmObj attrs)
{
    ScmRealExtendedPair *xp = SCM_NEW(ScmRealExtendedPair);
    /* ScmRealExtendedPair is not an ScmObj, and
       ScmExtendedPairDescriptor is not an ScmClass.   To avoid confusion,
       we manually tweak tag bits.
    */
    xp->hiddenTag = SCM_WORD((ScmByte*)desc + 7);
    xp->data.car = car;
    xp->data.cdr = cdr;
    xp->data.attributes = attrs;
    return SCM_OBJ(&xp->data);  /* hide the first word  */
}

/* "vanilla" extended pair.  used mainly to hold extra attributes, but
   otherwise behaves like normal pairs.
   NB: Static initialization with ScmClass* requires extra care on
   Windows.  To avoid complication, we initialize the klass field in
   _Init() routine.
*/
static ScmExtendedPairDescriptor mpair_desc SCM_ALIGN8 = {
    NULL,                       /* will be SCM_CLASS_PAIR */
    0,
    NULL,
    NULL
};

ScmObj Scm_MakeExtendedPair(ScmObj car, ScmObj cdr, ScmObj attrs)
{
    return make_extended_pair(&mpair_desc, car, cdr, attrs);
}

/* Returns NULL if p isn't an extended pair. */
ScmExtendedPairDescriptor *Scm__GetExtendedPairDescriptor(ScmObj p)
{
    if (!SCM_EXTENDED_PAIR_P(p)) return NULL;
    ScmRealExtendedPair *z = (ScmRealExtendedPair*)(((ScmObj*)p) - 1);
    SCM_ASSERT((z->hiddenTag&0x7) == 0x7);
    return (ScmExtendedPairDescriptor *)(z->hiddenTag-7);
}

/* Returns vanilla extended pair descriptor */
ScmExtendedPairDescriptor *Scm__GetDefaultExtendedPairDesctiptor()
{
    return &mpair_desc;
}

ScmObj Scm_PairAttr(ScmPair *pair)
{
    if (SCM_EXTENDED_PAIR_P(pair)) {
        return SCM_EXTENDED_PAIR(pair)->attributes;
    } else {
        return SCM_NIL;
    }
}

/* The common scenario is to use ExtendedCons in place of Cons,
   then add attributes later.  So we provide this.  */
ScmObj Scm_ExtendedCons(ScmObj car, ScmObj cdr)
{
    return Scm_MakeExtendedPair(car, cdr, SCM_NIL);
}

ScmObj Scm_PairAttrGet(ScmPair *pair, ScmObj key, ScmObj fallback)
{
    if (!SCM_EXTENDED_PAIR_P(pair)) {
        goto fallback;
    }

    ScmObj p = Scm_Assq(key, SCM_EXTENDED_PAIR(pair)->attributes);
    if (SCM_PAIRP(p)) return SCM_CDR(p);
  fallback:
    if (fallback == SCM_UNBOUND)
        Scm_Error("No value associated with key %S in pair attributes of %S",
                  key, SCM_OBJ(pair));
    return fallback;
}

ScmObj Scm_PairAttrSet(ScmPair *pair, ScmObj key, ScmObj value)
{
    if (!SCM_EXTENDED_PAIR_P(pair)) {
        Scm_Error("Cannot set pair attribute (%S) to non-extended pair: %S",
                  key, SCM_OBJ(pair));
    }

    /* We update the attribute list non-destructively.
       In case if two threads update the list simultaneously, one change
       can be lost, but no invalid access can occur.  Guaranteeing mutex
       is the caller's responsibility, though.
    */
    ScmObj xs, attrs = SCM_EXTENDED_PAIR(pair)->attributes;
    SCM_FOR_EACH(xs, attrs) {
        SCM_ASSERT(SCM_PAIRP(SCM_CAR(xs)));
        if (SCM_EQ(SCM_CAAR(xs), key)) {
            /* need to copy up to here */
            ScmObj h = SCM_NIL, t = SCM_NIL, ys;
            SCM_FOR_EACH(ys, attrs) {
                if (SCM_EQ(ys, xs)) {
                    SCM_APPEND1(h, t, Scm_Cons(key, value));
                    SCM_APPEND(h, t, SCM_CDR(xs));
                    SCM_EXTENDED_PAIR(pair)->attributes = h;
                    return SCM_UNDEFINED;
                }
                SCM_APPEND1(h, t, SCM_CAR(ys));
            }
            SCM_ASSERT(FALSE);  /* Shouldn't be here */
        }
    }
    SCM_EXTENDED_PAIR(pair)->attributes = Scm_Acons(key, value, attrs);
    return SCM_UNDEFINED;
}

/*
 * Immutable pairs
 */

/* not cheap.  usually you don't want to check this and let it catched
   in set-car!/set-cdr!. */
int Scm_ImmutablePairP(ScmObj obj)
{
    if (!SCM_EXTENDED_PAIR_P(obj)) return FALSE;
    ScmExtendedPairDescriptor *d = Scm__GetExtendedPairDescriptor(obj);
    return d->flags & SCM_PAIR_IMMUTABLE;
}

static ScmExtendedPairDescriptor ipair_desc SCM_ALIGN8 = {
    NULL,                       /* will be SCM_CLASS_PAIR.  see above. */
    SCM_PAIR_IMMUTABLE,
    NULL,
    NULL
};

ScmObj Scm_MakeImmutablePair(ScmObj car, ScmObj cdr, ScmObj attrs)
{
    return make_extended_pair(&ipair_desc, car, cdr, attrs);
}

void Scm__InitList()
{
    mpair_desc.klass = SCM_CLASS_PAIR;
    ipair_desc.klass = SCM_CLASS_PAIR;
}

/* Temporary - Check if normal pairs are all aligned with 2-word boundary */
#if GAUCHE_CHECK_PAIR_ALIGNMENT
int Scm_CheckingPairP(ScmObj obj)
{
    if (SCM_HPTRP(obj)&&(SCM_HTAG(obj)!=7||Scm_PairP(SCM_OBJ(obj)))) {
        if (SCM_WORD(obj) & SIZEOF_LONG) {
            if ((SCM_WORD((ScmObj*)obj - 1) & 7) != 7) {
                fprintf(stderr, "Unaligned pair %p\n", obj);
            }
        }
        return TRUE;
    }
    return FALSE;
}
#endif /*GAUCHE_CHECK_PAIR_ALIGNMENT*/
