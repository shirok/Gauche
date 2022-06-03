/*
 * weak.h - Public API for weak pointers
 *
 *   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_WEAK_H
#define GAUCHE_WEAK_H

/*================================================================
 * Weak box
 */

typedef struct ScmWeakBoxRec ScmWeakBox; /* opaque */

SCM_EXTERN ScmWeakBox *Scm_MakeWeakBox(void *value);
SCM_EXTERN int         Scm_WeakBoxEmptyP(ScmWeakBox *wbox);
SCM_EXTERN void        Scm_WeakBoxSet(ScmWeakBox *wbox, void *value);
SCM_EXTERN void       *Scm_WeakBoxRef(ScmWeakBox *wbox);

/*================================================================
 * Weak vector
 */

typedef struct ScmWeakVectorRec {
    SCM_HEADER;
    ScmSmallInt size;
    void *pointers;  /* opaque */
} ScmWeakVector;

#define SCM_WEAK_VECTOR(obj)   ((ScmWeakVector*)(obj))
#define SCM_WEAK_VECTOR_P(obj)  SCM_XTYPEP(obj, SCM_CLASS_WEAK_VECTOR)
SCM_CLASS_DECL(Scm_WeakVectorClass);
#define SCM_CLASS_WEAK_VECTOR  (&Scm_WeakVectorClass)

SCM_EXTERN ScmObj Scm_MakeWeakVector(ScmSmallInt size);
SCM_EXTERN ScmObj Scm_WeakVectorRef(ScmWeakVector *v,
                                    ScmSmallInt index, ScmObj fallback);
SCM_EXTERN ScmObj Scm_WeakVectorSet(ScmWeakVector *v,
                                    ScmSmallInt index, ScmObj val);

/*================================================================
 * Weak hash tables
 */

typedef enum {
    SCM_WEAK_KEY   = (1L<<0),
    SCM_WEAK_VALUE = (1L<<1),
    SCM_WEAK_BOTH  = (SCM_WEAK_KEY|SCM_WEAK_VALUE)
} ScmWeakness;


typedef struct ScmWeakHashTableRec {
    SCM_HEADER;
    ScmWeakness weakness;
    ScmHashType type;
    ScmHashCore core;
    ScmObj      defaultValue;
    ScmHashProc        *hashfn;
    ScmHashCompareProc *cmpfn;
    u_int       goneEntries;
} ScmWeakHashTable;

typedef struct ScmWeakHashIterRec {
    ScmWeakHashTable *table;
    ScmHashIter iter;
} ScmWeakHashIter;


SCM_CLASS_DECL(Scm_WeakHashTableClass);
#define SCM_CLASS_WEAK_HASH_TABLE     (&Scm_WeakHashTableClass)
#define SCM_WEAK_HASH_TABLE(obj)      ((ScmWeakHashTable*)(obj))
#define SCM_WEAK_HASH_TABLE_P(obj)    SCM_ISA(obj, SCM_CLASS_WEAK_HASH_TABLE)
#define SCM_WEAK_HASH_TABLE_CORE(obj) (&SCM_WEAK_HASH_TABLE(obj)->core)

SCM_EXTERN ScmObj Scm_MakeWeakHashTableSimple(ScmHashType type,
                                              ScmWeakness weakness,
                                              int initSize,
                                              ScmObj defaultValue);

SCM_EXTERN ScmObj Scm_WeakHashTableCopy(ScmWeakHashTable *tab);
SCM_EXTERN ScmObj Scm_WeakHashTableRef(ScmWeakHashTable *ht,
                                       ScmObj key, ScmObj fallback);
SCM_EXTERN ScmObj Scm_WeakHashTableSet(ScmWeakHashTable *ht,
                                       ScmObj key, ScmObj value, int flags);
SCM_EXTERN ScmObj Scm_WeakHashTableDelete(ScmWeakHashTable *ht, ScmObj key);
SCM_EXTERN ScmObj Scm_WeakHashTableKeys(ScmWeakHashTable *ht);
SCM_EXTERN ScmObj Scm_WeakHashTableValues(ScmWeakHashTable *ht);

SCM_EXTERN void   Scm_WeakHashIterInit(ScmWeakHashIter *iter,
                                       ScmWeakHashTable *ht);
SCM_EXTERN int    Scm_WeakHashIterNext(ScmWeakHashIter *iter,
                                       ScmObj *key, ScmObj *value);

#endif /* GAUCHE_WEAK_H */
