/*
 * keyword.c - keyword implementation
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
 *  $Id: keyword.c,v 1.1 2001-02-06 06:57:55 shiro Exp $
 */

#include "gauche.h"

/*
 * Keywords
 */

static int keyword_print(ScmObj obj, ScmPort *port, int mode)
{
    int nc = 0;
    if (mode != SCM_PRINT_DISPLAY) {
        SCM_PUTC(':', port);
        nc++;
    }
    SCM_PUTS(SCM_KEYWORD(obj)->name, port);
    nc += SCM_STRING_LENGTH(SCM_KEYWORD(obj)->name);
    return nc;
}

SCM_DEFCLASS(Scm_KeywordClass, "<keyword>", keyword_print,
             SCM_CLASS_DEFAULT_CPL);

/* Global keyword table.  Must be protected in MT environment */
static ScmHashTable *keywordTable;

/* Returns a keyword whose name is NAME.  Note that preceding ':' is not
 * a part of the keyword name.
 */
ScmObj Scm_MakeKeyword(ScmString *name)
{
    ScmHashEntry *e = Scm_HashTableGet(keywordTable, SCM_OBJ(name));
    if (e) return e->value;
    else {
        ScmKeyword *k = SCM_NEW(ScmKeyword);
        SCM_SET_CLASS(k, SCM_CLASS_KEYWORD);
        k->name = SCM_STRING(Scm_CopyString(name));
        Scm_HashTablePut(keywordTable, SCM_OBJ(name), SCM_OBJ(k));
        return SCM_OBJ(k);
    }
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
    keywordTable = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_STRING, NULL, 256));
}
