/*
 * treemap.h - general library of balanced trees
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

/* This file is included from gauche.h */

/*
 * Provides ScmTreeCore, a raw red-black tree implementation,
 * and ScmTreeMap, ScmObj wrapper of ScmTreeCore.
 */

#ifndef GAUCHE_TREEMAP_H
#define GAUCHE_TREEMAP_H

/*================================================================
 * ScmTreeCore
 */

typedef struct ScmTreeCoreRec ScmTreeCore;

typedef int ScmTreeCoreCompareProc(ScmTreeCore*, intptr_t, intptr_t);

/* A general tree map for internal use.  This is NOT a Scheme object. */

struct ScmTreeCoreRec {
    ScmDictEntry *root;
    ScmTreeCoreCompareProc *cmp;
    int   num_entries;
    void  *data;
};

#define SCM_TREE_CORE_DATA(core)  ((core)->data)

typedef struct ScmTreeIterRec {
    ScmTreeCore  *t;
    ScmDictEntry *e;
    int at_end;
} ScmTreeIter;

/*
 * Initializers
 */
SCM_EXTERN void Scm_TreeCoreInit(ScmTreeCore *tc,
                                 ScmTreeCoreCompareProc *cmp,
                                 void *data);
SCM_EXTERN void Scm_TreeCoreCopy(ScmTreeCore *dst,
                                 const ScmTreeCore *src);
SCM_EXTERN void Scm_TreeCoreClear(ScmTreeCore *tc);

/*
 * Accessors
 */
SCM_EXTERN ScmDictEntry *Scm_TreeCoreSearch(ScmTreeCore *tc,
                                            intptr_t key,
                                            ScmDictOp op);

SCM_EXTERN ScmDictEntry *Scm_TreeCoreClosestEntries(ScmTreeCore *tc,
                                                    intptr_t key,
                                                    ScmDictEntry **lo,
                                                    ScmDictEntry **hi);

SCM_EXTERN ScmDictEntry *Scm_TreeCoreNextEntry(ScmTreeCore *tc, intptr_t key);
SCM_EXTERN ScmDictEntry *Scm_TreeCorePrevEntry(ScmTreeCore *tc, intptr_t key);

typedef enum ScmTreeCoreBoundOp {
    SCM_TREE_CORE_MIN,
    SCM_TREE_CORE_MAX
} ScmTreeCoreBoundOp;

SCM_EXTERN ScmDictEntry *Scm_TreeCoreGetBound(ScmTreeCore *tc,
                                              ScmTreeCoreBoundOp op);
SCM_EXTERN ScmDictEntry *Scm_TreeCorePopBound(ScmTreeCore *tc,
                                              ScmTreeCoreBoundOp op);

SCM_EXTERN int           Scm_TreeCoreNumEntries(ScmTreeCore *tc);

SCM_EXTERN int           Scm_TreeCoreEq(ScmTreeCore *a, ScmTreeCore *b);

/*
 * Iterators
 */
SCM_EXTERN void          Scm_TreeIterInit(ScmTreeIter *iter,
                                          ScmTreeCore *tc,
                                          ScmDictEntry *start);
SCM_EXTERN ScmDictEntry *Scm_TreeIterNext(ScmTreeIter *iter);
SCM_EXTERN ScmDictEntry *Scm_TreeIterPrev(ScmTreeIter *iter);
SCM_EXTERN ScmDictEntry *Scm_TreeIterCurrent(ScmTreeIter *iter);
SCM_EXTERN int           Scm_TreeIterAtEnd(ScmTreeIter *iter);

/* for debug */
SCM_EXTERN void          Scm_TreeCoreCheckConsistency(ScmTreeCore *tc);
SCM_EXTERN void          Scm_TreeCoreDump(ScmTreeCore *tc, ScmPort *out);

/*================================================================
 * ScmTreeMap
 */

/* We store ScmComparator in core.data if the treemap is created
   in the Scheme world.  See make-tree-map in lib/gauche/treeutil.scm */
struct ScmTreeMapRec {
    SCM_HEADER;
    ScmTreeCore core;
};

SCM_CLASS_DECL(Scm_TreeMapClass);
#define SCM_CLASS_TREE_MAP       (&Scm_TreeMapClass)

#define SCM_TREE_MAP(obj)        ((ScmTreeMap*)(obj))
#define SCM_TREE_MAP_P(obj)      SCM_XTYPEP(obj, SCM_CLASS_TREE_MAP)

#define SCM_TREE_MAP_CORE(obj)   (&SCM_TREE_MAP(obj)->core)
#define SCM_TREE_MAP_DATA(obj)   SCM_TREE_CORE_DATA(SCM_TRE_MAP_CORE(obj))

SCM_EXTERN ScmObj    Scm_MakeTreeMap(ScmTreeCoreCompareProc *cmp,
                                     void *data);
SCM_EXTERN ScmObj    Scm_TreeMapCopy(const ScmTreeMap *src);

SCM_EXTERN ScmObj    Scm_TreeMapRef(ScmTreeMap *tm, ScmObj key,
                                    ScmObj fallback);
SCM_EXTERN ScmObj    Scm_TreeMapSet(ScmTreeMap *tm, ScmObj key, ScmObj value,
                                    int flags);
SCM_EXTERN ScmObj    Scm_TreeMapDelete(ScmTreeMap *tm, ScmObj key);

/* For debug */
SCM_EXTERN void      Scm_TreeMapDump(ScmTreeMap *tm, ScmPort *out);

#endif /* GAUCHE_TREEMAP_H */

