/*
 * char.c - character and character set operations
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
 *  $Id: char.c,v 1.12 2001-04-07 06:39:24 shiro Exp $
 */

#include "gauche.h"

/*
 * Character functions
 */

/* not much here... most are in stdlib.stub */
ScmObj Scm_CharEncodingName(void)
{
    return SCM_INTERN(SCM_CHAR_ENCODING_NAME);
}

/*
 * Character set (cf. SRFI-14)
 */
/* NB: operations on charset are not very optimized, for I don't see
 * the immediate needs to do so, except Scm_CharSetContains (this one
 * will be a macro in the future).
 */

static void charset_print(ScmObj obj, ScmPort *out, ScmWriteContext*);
static int charset_compare(ScmObj x, ScmObj y);
SCM_DEFINE_BUILTIN_CLASS(Scm_CharSetClass,
                         charset_print, charset_compare, NULL,
                         SCM_CLASS_DEFAULT_CPL);

/* masks */
#if SIZEOF_LONG == 4
#define MASK_BIT_SHIFT  5
#define MASK_BIT_MASK   0x1f
#elif SIZEOF_LONG == 8
#define MASK_BIT_SHIFT  6
#define MASK_BIT_MASK   0x3f
#elif SIZEOF_LONG == 16    /* maybe, in some future ... */
#define MASK_BIT_SHIFT  7
#define MASK_BIT_MASK   0x7f
#else
#error need to set SIZEOF_LONG
#endif

#define MASK_INDEX(ch)       ((ch) >> MASK_BIT_SHIFT)
#define MASK_BIT(ch)         (1L << ((ch) & MASK_BIT_MASK))
#define MASK_ISSET(cs, ch)   (cs->mask[MASK_INDEX(ch)] & MASK_BIT(ch))
#define MASK_SET(cs, ch)     (cs->mask[MASK_INDEX(ch)] |= MASK_BIT(ch))
#define MASK_RESET(cs, ch)   (cs->mask[MASK_INDEX(ch)] &= ~MASK_BIT(ch))

/* printer */
static void charset_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    Scm_Printf(out, "#<char-set %p>", obj);
}

/* comparer */
static int charset_compare(ScmObj x, ScmObj y)
{
    return 1;                   /* for now */
}

/* constructors */
static ScmCharSet *make_charset(void)
{
    ScmCharSet *cs = SCM_NEW(ScmCharSet);
    int i;
    SCM_SET_CLASS(cs, SCM_CLASS_CHARSET);
    for (i=0; i<SCM_CHARSET_MASK_SIZE; i++) cs->mask[i] = 0;
    cs->ranges = NULL;
    return cs;
}

ScmObj Scm_MakeEmptyCharSet(void)
{
    return SCM_OBJ(make_charset());
}

ScmObj Scm_CopyCharSet(ScmCharSet *src)
{
    ScmCharSet *dst = make_charset();
    struct ScmCharSetRange *rs, *rd = dst->ranges;
    int i;
    
    for (i=0; i<SCM_CHARSET_MASK_SIZE; i++) dst->mask[i] = src->mask[i];
    for (rs = src->ranges; rs; rs = rs->next) {
        if (rd == NULL) {
            rd = dst->ranges = SCM_NEW(struct ScmCharSetRange);
            rd->lo = rs->lo;
            rd->hi = rs->hi;
        } else {
            rd->next = SCM_NEW(struct ScmCharSetRange);
            rd = rd->next;
            rd->lo = rs->lo;
            rd->hi = rs->hi;
        }
    }
    if (rd) rd->next = NULL;
    return SCM_OBJ(dst);
}

/* comparison */

int Scm_CharSetEq(ScmCharSet *x, ScmCharSet *y)
{
    int i;
    struct ScmCharSetRange *rx, *ry;
    for (i=0; i<SCM_CHARSET_MASK_SIZE; i++)
        if (x->mask[i] != y->mask[i]) return FALSE;
    for (rx=x->ranges, ry=y->ranges; rx && ry; rx=rx->next, ry=ry->next) {
        if (rx->lo != ry->lo || rx->hi != ry->hi) return FALSE;
    }
    if (rx || ry) return FALSE;
    return TRUE;
}

/* modification */

static struct ScmCharSetRange *newrange(int lo, int hi,
                                        struct ScmCharSetRange *next)
{
    struct ScmCharSetRange *n = SCM_NEW(struct ScmCharSetRange);
    n->next = next;
    n->lo = lo;
    n->hi = hi;
    return n;
}

ScmObj Scm_CharSetAddRange(ScmCharSet *cs, ScmChar from, ScmChar to)
{
    int i;
    struct ScmCharSetRange *lo, *lop, *hi, *hip, *n;
    
    if (to < from) return;
    if (from < SCM_CHARSET_MASK_CHARS) {
        if (to < SCM_CHARSET_MASK_CHARS) {
            for (i=from; i<=to; i++) MASK_SET(cs, i);
            return SCM_OBJ(cs);
        }
        for (i=from; i<SCM_CHARSET_MASK_CHARS; i++)  MASK_SET(cs, i);
        from = SCM_CHARSET_MASK_CHARS;
    }
    if (cs->ranges == NULL) {
        cs->ranges = newrange(from, to, NULL);
        return SCM_OBJ(cs);
    }
    /* Add range.  Ranges are chained from lower character code to higher,
       without any overlap. */
    /* First, we scan the ranges so that we'll get...
        - if FROM is in a range, lo points to it.
        - if FROM is out of any ranges, lo points to the closest range that
          is higher than FROM.
        - if TO is in a range, hi points to the range.
        - if TO is out of any ranges, hi points to the closest range that
          is higher than TO. */
    for (lop = NULL, lo = cs->ranges; lo; lop = lo, lo = lo->next) {
        if (from <= lo->hi+1) break;
    }
    if (!lo) {
        lop->next = newrange(from, to, NULL);
        return SCM_OBJ(cs);
    }
    for (hip = lop, hi = lo; hi; hip = hi, hi = hi->next) {
        if (to <= hi->hi) break;
    }
    /* Then we insert, extend and/or merge the ranges accordingly. */
    if (from < lo->lo) { /* FROM extends the LO */
        if (lo == hi) {
            if (to < hi->lo-1) {
                if (lop == NULL) cs->ranges = newrange(from, to, lo);
                else             lop->next = newrange(from, to, lo);
            } else {
                lo->lo = from;
                lo->hi = hi->hi;
                lo->next = hi->next;
            }
        } else if (hi == NULL || to < hi->lo-1) {
            lo->lo = from;
            lo->hi = to;
            lo->next = hi;
        } else {
            lo->lo = from;
            lo->hi = hi->hi;
            lo->next = hi->next;
        }
    } else { /* FROM included in LO */
        if (lo != hi) {
            if (hi == NULL || to < hi->lo-1) {
                lo->hi = to;
                lo->next = hi;
            } else {
                lo->hi = hi->hi;
                lo->next = hi->next;
            }
        }
    }
    /* WRITE ME */
    return SCM_OBJ(cs);
}

ScmObj Scm_CharSetAdd(ScmCharSet *dst, ScmCharSet *src)
{
    int i;
    struct ScmCharSetRange *r;
    for (i=0; i<SCM_CHARSET_MASK_SIZE; i++)
        dst->mask[i] |= src->mask[i];
    for (r = src->ranges; r; r = r->next) {
        Scm_CharSetAddRange(dst, r->lo, r->hi);
    }
    return SCM_OBJ(dst);
}

ScmObj Scm_CharSetComplement(ScmCharSet *cs)
{
    int i, last;
    struct ScmCharSetRange *r, *p;
    for (i=0; i<SCM_CHARSET_MASK_SIZE; i++)
        cs->mask[i] = ~cs->mask[i];
    last = SCM_CHARSET_MASK_CHARS;
    for (p = NULL, r = cs->ranges; r; p = r, r = r->next) {
        int hi = r->hi+1;
        if (r->lo != SCM_CHARSET_MASK_CHARS) {
            r->hi = r->lo - 1;
            r->lo = last;
        } else {
            cs->ranges = r->next;
        }
        last = hi;
    }
    if (last < SCM_CHAR_MAX) {
        if (!p) cs->ranges = newrange(last, SCM_CHAR_MAX, NULL);
        else    p->next = newrange(last, SCM_CHAR_MAX, NULL);
    }
    return SCM_OBJ(cs);
}

/* query */

int Scm_CharSetContains(ScmCharSet *cs, ScmChar c)
{
    if (c < 0) return FALSE;
    if (c < SCM_CHARSET_MASK_CHARS) return MASK_ISSET(cs, c);
    else {
        struct ScmCharSetRange *r;
        for (r = cs->ranges; r; r = r->next) {
            if (r->lo <= c && c <= r->hi) return TRUE;
        }
        return FALSE;
    }
}

/* inspection */

/* returns a list of ranges contained in the charset */
ScmObj Scm_CharSetRanges(ScmCharSet *cs)
{
    ScmObj h = SCM_NIL, t = SCM_NIL, cell;
    int ind, begin = 0, prev = FALSE;
    struct ScmCharSetRange *r;
    
    for (ind = 0; ind < SCM_CHARSET_MASK_CHARS; ind++) {
        int bit = MASK_ISSET(cs, ind);
        if (!prev && bit) begin = ind;
        if (prev && !bit) {
            cell = Scm_Cons(SCM_MAKE_INT(begin), SCM_MAKE_INT(ind-1));
            SCM_APPEND1(h, t, cell);
        }
        prev = bit;
    }
    if (prev) {
        if (!cs->ranges || cs->ranges->lo != SCM_CHARSET_MASK_CHARS) {
            cell = Scm_Cons(SCM_MAKE_INT(begin),
                            SCM_MAKE_INT(SCM_CHARSET_MASK_CHARS-1));
            SCM_APPEND1(h, t, cell);
            r = cs->ranges;
        } else {
            cell = Scm_Cons(SCM_MAKE_INT(begin), SCM_MAKE_INT(cs->ranges->hi));
            SCM_APPEND1(h, t, cell);
            r = cs->ranges->next;
        }
    } else {
        r = cs->ranges;
    }
    for (; r; r = r->next) {
        cell = Scm_Cons(SCM_MAKE_INT(r->lo), SCM_MAKE_INT(r->hi));
        SCM_APPEND1(h, t, cell);
    }
    return h;
}

void Scm_CharSetDump(ScmCharSet *cs, ScmPort *port)
{
    int i;
    struct ScmCharSetRange *r;
    Scm_Printf(port, "CharSet %p\nmask:", cs);
    for (i=0; i<SCM_CHARSET_MASK_SIZE; i++)
        Scm_Printf(port, "[%08x]", cs->mask[i]);
    Scm_Printf(port, "\nranges:");
    for (r=cs->ranges; r; r=r->next)
        Scm_Printf(port, "(%d-%d)", r->lo, r->hi);
    Scm_Printf(port, "\n");
}

/*
 * Initialization
 */
void Scm__InitChar(void)
{
    Scm_InitBuiltinClass(SCM_CLASS_CHARSET, "<char-set>", Scm_GaucheModule());
}

