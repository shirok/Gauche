/*
 * keyword.c - keyword implementation
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
 *  $Id: keyword.c,v 1.12 2003-04-30 20:24:09 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"

/*
 * Keywords
 */

static void keyword_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    if (SCM_WRITE_MODE(ctx) != SCM_WRITE_DISPLAY) {
        SCM_PUTC(':', port);
    }
    SCM_PUTS(SCM_KEYWORD(obj)->name, port);
    return;
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_KeywordClass, keyword_print);

/* Global keyword table. */
static struct {
    ScmHashTable *table;
    ScmInternalMutex mutex;
} keywords = { NULL };

/* Returns a keyword whose name is NAME.  Note that preceding ':' is not
 * a part of the keyword name.
 */
ScmObj Scm_MakeKeyword(ScmString *name)
{
    ScmHashEntry *e;
    ScmObj r;

    (void)SCM_INTERNAL_MUTEX_LOCK(keywords.mutex);
    e = Scm_HashTableGet(keywords.table, SCM_OBJ(name));
    if (e) r = e->value;
    else {
        ScmKeyword *k = SCM_NEW(ScmKeyword);
        SCM_SET_CLASS(k, SCM_CLASS_KEYWORD);
        k->name = SCM_STRING(Scm_CopyString(name));
        Scm_HashTablePut(keywords.table, SCM_OBJ(name), SCM_OBJ(k));
        r = SCM_OBJ(k);
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(keywords.mutex);
    return r;
}

ScmObj Scm_GetKeyword(ScmObj key, ScmObj list, ScmObj fallback)
{
    ScmObj cp;
    SCM_FOR_EACH(cp, list) {
        if (!SCM_PAIRP(SCM_CDR(cp))) Scm_Error("incomplete key list: %S", list);
        if (key == SCM_CAR(cp)) return SCM_CADR(cp);
        cp = SCM_CDR(cp);
    }
    if (SCM_UNBOUNDP(fallback)) {
        Scm_Error("value for key %S is not provided: %S", key, list);
    }
    return fallback;
}

void Scm__InitKeyword(void)
{
    (void)SCM_INTERNAL_MUTEX_INIT(keywords.mutex);
    keywords.table = SCM_HASHTABLE(Scm_MakeHashTable((ScmHashProc)SCM_HASH_STRING, NULL, 256));
}
