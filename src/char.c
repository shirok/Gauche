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
 *  $Id: char.c,v 1.7 2001-04-01 23:18:03 shiro Exp $
 */

#include "gauche.h"

/* Most character stuffs are defined as macros. */

/*
 * Character set (cf. SRFI-14)
 */

static int charset_print(ScmObj obj, ScmPort *out, int mode);
static int charset_compare(ScmObj x, ScmObj y);
SCM_DEFINE_BUILTIN_CLASS(Scm_CharSetClass,
                         charset_print, charset_compare, NULL,
                         SCM_CLASS_DEFAULT_CPL);

/* printer */
static int charset_print(ScmObj obj, ScmPort *out, int mode)
{
    return Scm_Printf(out, "#<char-set %p>", obj);
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

/*
 * Initialization
 */
void Scm__InitChar(void)
{
    Scm_InitBuiltinClass(SCM_CLASS_CHARSET, "<char-set>", Scm_GaucheModule());
}

