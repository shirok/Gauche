/*
 * list.c - List related functions
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
 *  $Id: list.c,v 1.7 2001-02-05 09:46:26 shiro Exp $
 */

#include "gauche.h"
#include "gauche/memory.h"

/*
 * Classes
 */

static ScmClass *list_cpl[] = {
    SCM_CLASS_LIST, SCM_CLASS_SEQUENCE, SCM_CLASS_COLLECTION, SCM_CLASS_TOP,
    NULL
};

SCM_DEFCLASS(Scm_ListClass, "<list>", NULL, SCM_CLASS_SEQUENCE_CPL);
SCM_DEFCLASS(Scm_PairClass, "<pair>", NULL, list_cpl);
SCM_DEFCLASS(Scm_NullClass, "<null>", NULL, list_cpl);

/*
 * CONSTRUCTOR
 */

ScmObj Scm_Cons(ScmObj car, ScmObj cdr)
{
    ScmPair *z;
    SCM_MALLOC_WORDS(z, sizeof(ScmPair)/sizeof(GC_word), ScmPair*);
    SCM_SET_CLASS(z, SCM_CLASS_PAIR);
    SCM_SET_CAR(z, car);
    SCM_SET_CDR(z, cdr);
    z->attributes = SCM_NIL;
    return SCM_OBJ(z);
}

ScmObj Scm_List(ScmObj elt, ...)
{
    va_list pvar;
    ScmObj cdr;

    if (elt == NULL) return SCM_NIL;
        
    va_start(pvar, elt);
    cdr = Scm_VaList(pvar);
    va_end(pvar);
    return Scm_Cons(elt, cdr);
}


ScmObj Scm_Conses(ScmObj elt, ...)
{
    va_list pvar;
    ScmObj cdr;

    if (elt == NULL) return SCM_NIL;
    
    va_start(pvar, elt);
    cdr = Scm_VaCons(pvar);
    va_end(pvar);
    if (cdr == NULL) return elt;
    else             return Scm_Cons(elt, cdr);
}


ScmObj Scm_VaList(va_list pvar)
{
    ScmObj start = SCM_NIL, cp, obj;
    
    for (obj = va_arg(pvar, ScmObj);
	 obj != NULL;
	 obj = va_arg(pvar, ScmObj))
    {
	if (SCM_NULLP(start)) {
            start = SCM_OBJ(SCM_NEW(ScmPair));
            SCM_SET_CAR(start, obj);
            SCM_SET_CDR(start, SCM_NIL);
            cp = start;
        } else {
            ScmObj item;
            item = SCM_OBJ(SCM_NEW(ScmPair));
            SCM_SET_CDR(cp, item);
            SCM_SET_CAR(item, obj);
            SCM_SET_CDR(item, SCM_NIL);
            cp = item;
	}
    }
    return start;
}


ScmObj Scm_VaCons(va_list pvar)
{
    Scm_Panic("Scm_VaCons: not implemented");
    return SCM_UNDEFINED;
}

/* Procedures intended to be used from Scheme */

ScmObj Scm_PairP(ScmObj obj)
{
    return SCM_PAIRP(obj)? SCM_TRUE : SCM_FALSE;
}

/* cXr stuff */

#define	CXR(cname, sname, body)                 \
ScmObj cname (ScmObj obj)                       \
{                                               \
   ScmObj obj2 = obj;                           \
   body                                         \
   return obj2;                                 \
}

#define	A                                                       \
   if (!SCM_PAIRP(obj2)) Scm_Error("bad object: %S", obj);      \
   obj2 = SCM_CAR(obj2);

#define	D                                                       \
   if (!SCM_PAIRP(obj2)) Scm_Error("bad object: %S", obj);      \
   obj2 = SCM_CDR(obj2);


CXR(Scm_Car, "car", A)
CXR(Scm_Cdr, "cdr", D)
CXR(Scm_Caar, "caar", A A)
CXR(Scm_Cadr, "cadr", D A)
CXR(Scm_Cdar, "cdar", A D)
CXR(Scm_Cddr, "cddr", D D)
CXR(Scm_Caaar, "caaar", A A A)
CXR(Scm_Caadr, "caadr", D A A)
CXR(Scm_Cadar, "cadar", A D A)
CXR(Scm_Caddr, "caddr", D D A)
CXR(Scm_Cdaar, "cdaar", A A D)
CXR(Scm_Cdadr, "cdadr", D A D)
CXR(Scm_Cddar, "cddar", A D D)
CXR(Scm_Cdddr, "cdddr", D D D)
CXR(Scm_Caaaar, "caaaar", A A A A)
CXR(Scm_Caaadr, "caaadr", D A A A)
CXR(Scm_Caadar, "caadar", A D A A)
CXR(Scm_Caaddr, "caaddr", D D A A)
CXR(Scm_Cadaar, "cadaar", A A D A)
CXR(Scm_Cadadr, "cadadr", D A D A)
CXR(Scm_Caddar, "caddar", A D D A)
CXR(Scm_Cadddr, "cadddr", D D D A)
CXR(Scm_Cdaaar, "cdaaar", A A A D)
CXR(Scm_Cdaadr, "cdaadr", D A A D)
CXR(Scm_Cdadar, "cdadar", A D A D)
CXR(Scm_Cdaddr, "cdaddr", D D A D)
CXR(Scm_Cddaar, "cddaar", A A D D)
CXR(Scm_Cddadr, "cddadr", D A D D)
CXR(Scm_Cdddar, "cdddar", A D D D)
CXR(Scm_Cddddr, "cddddr", D D D D)

/*
 * List manipulate routines:
 */

/* Scm_Length
   return length of list in C integer.  If argment is not a proper
   or circular list, return -1. */

int Scm_Length(ScmObj obj)
{
    ScmObj slow = obj;
    int len = 0;

    if (SCM_NULLP(obj)) return 0;
    for (;;) {
        if (SCM_NULLP(obj)) break;
        if (!SCM_PAIRP(obj)) return -1;
	if (len != 0 && obj == slow) return -1; /* circular */
	
	obj = SCM_CDR(obj);
	len++;
        if (SCM_NULLP(obj)) break;
        if (!SCM_PAIRP(obj)) return -1;
	if (obj == slow) return -1; /* circular */

	obj = SCM_CDR(obj);
	slow = SCM_CDR(slow);
	len++;
    }
    return len;
}

/* Scm_CopyList(list)
 *   Copy toplevel list LIST.  LIST can be improper.
 *   If LIST is not a pair, return LIST itself.
 */

ScmObj Scm_CopyList(ScmObj list)
{
    ScmObj start = SCM_NIL, last;

    if (!SCM_PAIRP(list)) return list;
    
    SCM_FOR_EACH(list, list) {
        SCM_APPEND1(start, last, SCM_CAR(list));
    }
    if (!SCM_NULLP(list)) {
        if (start == SCM_NIL) start = list;
        else SCM_SET_CDR(last, list);
    }
    return start;
}

/* Scm_MakeList(len, fill)
 *    Make a list of specified length.
 *    Note that <len> is C-integer.
 */

ScmObj Scm_MakeList(int len, ScmObj fill)
{
    ScmObj start = SCM_NIL, last;
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
            SCM_SET_CDR(cp, obj);
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
    ScmObj start = SCM_NIL, last;

    if (!SCM_PAIRP(list)) return obj;

    SCM_FOR_EACH(list, list) {
	SCM_APPEND1(start, last, SCM_CAR(list));
    }
    SCM_SET_CDR(last, obj);

    return start;
}

ScmObj Scm_Append(ScmObj args)
{
    ScmObj start = SCM_NIL, last, cp;
    SCM_FOR_EACH(cp, args) {
        if (!SCM_PAIRP(SCM_CDR(cp))) {
            if (SCM_NULLP(start)) return SCM_CAR(cp);
            SCM_SET_CDR(last, SCM_CAR(cp));
            break;
        } else if (SCM_NULLP(SCM_CAR(cp))) {
            continue;
        } else if (!SCM_PAIRP(SCM_CAR(cp))) {
            Scm_Error("pair required, but got %S", SCM_CAR(cp));
        } else {
            SCM_APPEND(start, last, Scm_CopyList(SCM_CAR(cp)));
        }
    }
    return start;
}

/* Scm_Reverse(list)
 *    Reverse LIST.  If LIST is not a pair, return LIST itself.
 *    If LIST is improper list, cdr of the last pair is ignored.
 */

ScmObj Scm_Reverse(ScmObj list)
{
    ScmObj cp, result;
    ScmPair *p;

    if (!SCM_PAIRP(list)) return list;

    SCM_NEW_PAIR(p, SCM_NIL, SCM_NIL);
    result = SCM_OBJ(p);
    SCM_FOR_EACH(cp, list) {
	SCM_SET_CAR(result, SCM_CAR(cp));
        SCM_NEW_PAIR(p, SCM_NIL, result);
        result = SCM_OBJ(p);
    }
    return SCM_CDR(result);
}

    
/* Scm_ReverseX(list)
 *   Return reversed list of LIST.  Pairs in previous LIST is used to
 *   create new list.  If LIST is not a pair, return LIST itself.
 *   If LIST is an improper list, cdr of the last cell is ignored.
 */

ScmObj Scm_ReverseX(ScmObj list)
{
    ScmObj first, next, result = SCM_NIL;
    if (!SCM_PAIRP(list)) return list;
    for (first = list; SCM_PAIRP(first); first = next) {
        next = SCM_CDR(first);
        SCM_SET_CDR(first, result);
        result = first;
    }
    return result;
}


/* Scm_ListTail(list, i)
 * Scm_ListRef(list, i)
 *    Note that i is C-INTEGER.  If i is out of bound, signal error.
 */

ScmObj Scm_ListTail(ScmObj list, int i)
{
    if (i < 0) Scm_Error("argument out of range: %d", i);
    while (i-- > 0) {
        if (!SCM_PAIRP(list))
            Scm_Error("argument out of range: %d", i);
        list = SCM_CDR(list);
    }
    return list;
}

ScmObj Scm_ListRef(ScmObj list, int i)
{
    if (i < 0) Scm_Error("argument out of range: %d", i);
    while (i-- > 0) {
        if (!SCM_PAIRP(list))
            Scm_Error("argument out of range: %d", i);
        list = SCM_CDR(list);
    }
    return SCM_CAR(list);
}

/* Scm_LastPair(l)
 *   Return last pair of (maybe improper) list L.
 *   If L is not a pair, signal error.
 */

ScmObj Scm_LastPair(ScmObj l)
{
    ScmObj cp;

    if (!SCM_PAIRP(l)) Scm_Error("pair required: %S", l);
    SCM_FOR_EACH(cp, l) {
	ScmObj cdr = SCM_CDR(cp);
	if (!SCM_PAIRP(cdr)) return cp;
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
        if (Scm_EqvP(obj, SCM_CAR(list)) != SCM_FALSE) return list;
    }
    return SCM_FALSE;
}

ScmObj Scm_Member(ScmObj obj, ScmObj list)
{
    SCM_FOR_EACH(list, list) {
        if (Scm_EqualP(obj, SCM_CAR(list)) != SCM_FALSE) return list;
    }
    return SCM_FALSE;
}

/*
 * assq, assv, assoc
 *    ALIST must be a list of pairs.  Return the first pair whose car
 *    is obj.  If ALIST contains non pair, it's silently ignored.
 */

ScmObj Scm_Assq(ScmObj obj, ScmObj alist)
{
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
    ScmObj cp;
    SCM_FOR_EACH(cp,alist) {
	ScmObj entry = SCM_CAR(cp);
	if (!SCM_PAIRP(entry)) continue;
	if (Scm_EqvP(obj, SCM_CAR(entry)) != SCM_FALSE) return entry;
    }
    return SCM_FALSE;
}

ScmObj Scm_Assoc(ScmObj obj, ScmObj alist)
{
    ScmObj cp;
    SCM_FOR_EACH(cp,alist) {
        ScmObj entry = SCM_CAR(cp);
        if (!SCM_PAIRP(entry)) continue;
        if (Scm_EqualP(obj, SCM_CAR(entry)) != SCM_FALSE) return entry;
    }
    return SCM_FALSE;
}

/* Return union of two lists.
   Comparison is done by `eq?'.
 */

ScmObj Scm_Union(ScmObj list1, ScmObj list2)
{
    int len1 = Scm_Length(list1);
    int len2 = Scm_Length(list2);
    
    if (len1 < 0 || len2 < 0) return SCM_NIL;
    if (len1 == 0) return list2;
    if (len2 == 0) return list1;

    SCM_FOR_EACH(list1, list1) {
        if (SCM_FALSEP(Scm_Memq(SCM_CAR(list1), list2))) {
            list2 = Scm_Cons(SCM_CAR(list1), list2);
        }
    }
    return list2;
}

/* Return intersection of two lists. */

/*
 * Pair attributes
 */

ScmObj Scm_PairAttrGet(ScmPair *pair, ScmObj key, ScmObj fallback)
{
    ScmObj p = Scm_Assq(key, SCM_PAIR_ATTR(pair));
    if (SCM_PAIRP(p)) return SCM_CDR(p);
    if (fallback == SCM_UNBOUND)
        Scm_Error("No value associated with key %S in pair attributes of %S",
                  key, SCM_OBJ(pair));
    return fallback;
}

ScmObj Scm_PairAttrSet(ScmPair *pair, ScmObj key, ScmObj value)
{
    ScmObj p = Scm_Assq(key, SCM_PAIR_ATTR(pair));
    if (SCM_PAIRP(p)) SCM_SET_CDR(p, value);
    else SCM_PAIR_ATTR(pair) = Scm_Cons(Scm_Cons(key, value),
                                        SCM_PAIR_ATTR(pair));
    return SCM_UNDEFINED;
}

/*
 * Other utilities.
 */

ScmObj Scm_NullP(ScmObj obj)
{
    return SCM_NULLP(obj)? SCM_TRUE : SCM_FALSE;
}

ScmObj Scm_ListP(ScmObj obj)
{
    return (Scm_Length(obj) >= 0) ? SCM_TRUE : SCM_FALSE;
}

