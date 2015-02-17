/*
 * sptab.h - Sparse hashtable
 *
 *   Copyright (c) 2009-2015  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_SPTAB_H
#define GAUCHE_SPTAB_H

#include <gauche.h>
#include <gauche/extend.h>

#if defined(EXTSPARSE_EXPORTS)
#define LIBGAUCHE_EXT_BODY
#endif
#include <gauche/extern.h>      /* redefine SCM_EXTERN */

#include "ctrie.h"

typedef struct SparseTableRec {
    SCM_HEADER;
    CompactTrie trie;
    u_long      numEntries;
    u_long      (*hashfn)(ScmObj key);
    int         (*cmpfn)(ScmObj a, ScmObj b);
    ScmComparator *comparator;  /* only used for generic table */
} SparseTable;

SCM_CLASS_DECL(Scm_SparseTableClass);
#define SCM_CLASS_SPARSE_TABLE  (&Scm_SparseTableClass)
#define SPARSE_TABLE(obj)       ((SparseTable*)(obj))
#define SPARSE_TABLE_P(obj)     SCM_XTYPEP(obj, SCM_CLASS_SPARSE_TABLE)

extern ScmObj MakeSparseTable(ScmHashType type, ScmComparator *data,
                              u_long flags);
extern ScmObj SparseTableRef(SparseTable *st, ScmObj key, ScmObj fallback);
extern ScmObj SparseTableSet(SparseTable *st, ScmObj key,
                             ScmObj value, int flags);
extern ScmObj SparseTableDelete(SparseTable *st, ScmObj key);
extern void   SparseTableClear(SparseTable *st);
extern ScmObj SparseTableCopy(const SparseTable *st);

extern void   SparseTableDump(SparseTable *sv);
extern void   SparseTableCheck(SparseTable *sv);

/* Iterator */
typedef struct SparseTableIterRec {
    SparseTable *st;
    CompactTrieIter ctit;
    ScmObj chain;
    int end;
} SparseTableIter;

extern void   SparseTableIterInit(SparseTableIter *it, SparseTable *st);
extern ScmObj SparseTableIterNext(SparseTableIter *it);

extern void   Scm_Init_sptab(ScmModule *mod);

#endif /*GAUCHE_SPTAB_H*/
