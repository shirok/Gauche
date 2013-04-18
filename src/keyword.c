/*
 * keyword.c - keyword implementation
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

#define LIBGAUCHE_BODY
#include "gauche.h"

/*
 * Keywords
 */

static void keyword_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    if (SCM_WRITE_MODE(ctx) == SCM_WRITE_DISPLAY) {
        SCM_PUTS(SCM_KEYWORD(obj)->name, port);
    } else {
        SCM_PUTC(':', port);
        /* We basically print keyword names in the same way as symbols
           (i.e. using |-escape if necessary).  However, as a convention,
           two things are different from the default symbol writer.
           (1) We don't check the noninitials; :1 is unambiguously a
           keyword, so we don't need to print :|1|.
           (2) A keyword with an empty name can be printed just as :,
           instead of :||.
           These conventions are useful if we pass the S-expression with
           these keywords to other Scheme implementations that don't support
           CL-style keywords; they would just read those ones as symbols.
        */
        Scm_WriteSymbolName(SCM_KEYWORD(obj)->name, port, ctx,
                            (SCM_SYMBOL_WRITER_NOESCAPE_INITIAL
                             |SCM_SYMBOL_WRITER_NOESCAPE_EMPTY));
    }
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
    ScmObj r;
    ScmKeyword *k;

    (void)SCM_INTERNAL_MUTEX_LOCK(keywords.mutex);
    r = Scm_HashTableRef(keywords.table, SCM_OBJ(name), SCM_FALSE);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(keywords.mutex);

    if (SCM_KEYWORDP(r)) return r;

    k = SCM_NEW(ScmKeyword);
    SCM_SET_CLASS(k, SCM_CLASS_KEYWORD);
    k->name = SCM_STRING(Scm_CopyString(name));
    (void)SCM_INTERNAL_MUTEX_LOCK(keywords.mutex);
    r = Scm_HashTableSet(keywords.table, SCM_OBJ(name), SCM_OBJ(k),
                         SCM_DICT_NO_OVERWRITE);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(keywords.mutex);
    return r;
}

ScmObj Scm_GetKeyword(ScmObj key, ScmObj list, ScmObj fallback)
{
    ScmObj cp;
    SCM_FOR_EACH(cp, list) {
        if (!SCM_PAIRP(SCM_CDR(cp))) {
            Scm_Error("incomplete key list: %S", list);
        }
        if (key == SCM_CAR(cp)) return SCM_CADR(cp);
        cp = SCM_CDR(cp);
    }
    if (SCM_UNBOUNDP(fallback)) {
        Scm_Error("value for key %S is not provided: %S", key, list);
    }
    return fallback;
}

ScmObj Scm_DeleteKeyword(ScmObj key, ScmObj list)
{
    ScmObj cp;
    SCM_FOR_EACH(cp, list) {
        if (!SCM_PAIRP(SCM_CDR(cp))) {
            Scm_Error("incomplete key list: %S", list);
        }
        if (key == SCM_CAR(cp)) {
            /* found */
            ScmObj h = SCM_NIL, t = SCM_NIL;
            ScmObj tail = Scm_DeleteKeyword(key, SCM_CDR(SCM_CDR(cp)));
            ScmObj cp2;
            SCM_FOR_EACH(cp2, list) {
                if (cp2 == cp) {
                    SCM_APPEND(h, t, tail);
                    return h;
                } else {
                    SCM_APPEND1(h, t, SCM_CAR(cp2));
                }
            }
        }
        cp = SCM_CDR(cp);
    }
    return list;
}

ScmObj Scm_DeleteKeywordX(ScmObj key, ScmObj list)
{
    ScmObj cp, prev = SCM_FALSE;
    SCM_FOR_EACH(cp, list) {
        if (!SCM_PAIRP(SCM_CDR(cp))) {
            Scm_Error("incomplete key list: %S", list);
        }
        if (key == SCM_CAR(cp)) {
            /* found */
            if (SCM_FALSEP(prev)) {
                /* we're at the head of list */
                return Scm_DeleteKeywordX(key, SCM_CDR(SCM_CDR(cp)));
            } else {
                ScmObj tail = Scm_DeleteKeywordX(key, SCM_CDR(SCM_CDR(cp)));
                SCM_SET_CDR(prev, tail);
                return list;
            }
        }
        cp = SCM_CDR(cp);
        prev = cp;
    }
    return list;
}

void Scm__InitKeyword(void)
{
    (void)SCM_INTERNAL_MUTEX_INIT(keywords.mutex);
    keywords.table = SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_STRING, 256));
}
