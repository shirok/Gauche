/*
 * hash.h - Public API for hashtables
 *
 *   Copyright (c) 2000-2007 Shiro Kawai <shiro@acm.org>
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
 *
 *  $Id: hash.h,v 1.3 2007-04-13 11:15:37 shirok Exp $
 */

/* This file is included from gauche.h */

#ifndef GAUCHE_HASH_H
#define GAUCHE_HASH_H

/*================================================================
 * ScmHashCore
 */

/* Hash types */
typedef enum {
    SCM_HASH_EQ,
    SCM_HASH_EQV,
    SCM_HASH_EQUAL,
    SCM_HASH_STRING,
    SCM_HASH_GENERAL,

    SCM_HASH_WORD
} ScmHashType;

typedef struct ScmHashCoreRec ScmHashCore;
typedef struct ScmHashIterRec ScmHashIter;

typedef unsigned long ScmHashProc(ScmHashCore *hc, intptr_t key);
typedef int ScmHashCompareProc(ScmHashCore *hc, intptr_t x, intptr_t y);

struct ScmHashCoreRec {
    void **buckets;
    int numBuckets;
    int numEntries;
    int numBucketsLog2;
    void               *accessfn; /* actual type hidden */
    ScmHashProc        *hashfn;
    ScmHashCompareProc *cmpfn;
    void *data;
};

SCM_EXTERN void Scm_HashCoreInitSimple(ScmHashCore *core,
                                       ScmHashType type,
                                       unsigned int initSize,
                                       void *data);

SCM_EXTERN void Scm_HashCoreCopy(ScmHashCore *dst, const ScmHashCore *src);

SCM_EXTERN ScmDictEntry *Scm_HashCoreSearch(ScmHashCore *core,
                                            intptr_t key,
                                            ScmDictOp op);

SCM_EXTERN int  Scm_HashCoreNumEntries(ScmHashCore *core);

struct ScmHashIterRec {
    ScmHashCore *core;
    int   bucket;
    void *entry;
};

#if defined(GAUCHE_API_0_9) || defined(LIBGAUCHE_BODY)
SCM_EXTERN void Scm_HashIterInit(ScmHashIter *iter, ScmHashCore *core);
SCM_EXTERN ScmDictEntry *Scm_HashIterNext(ScmHashIter *iter);
#endif /* See the compatibility section below for old APIs */

/*
 * Hash functions
 */
SCM_EXTERN unsigned long Scm_EqHash(ScmObj obj);
SCM_EXTERN unsigned long Scm_EqvHash(ScmObj obj);
SCM_EXTERN unsigned long Scm_Hash(ScmObj obj);
SCM_EXTERN unsigned long Scm_HashString(ScmString *str, unsigned long bound);

/*================================================================
 * ScmHashTable
 */

struct ScmHashTableRec {
    SCM_HEADER;
    ScmHashType type;
    ScmHashCore core;
};

SCM_CLASS_DECL(Scm_HashTableClass);
#define SCM_CLASS_HASH_TABLE  (&Scm_HashTableClass)
#define SCM_HASH_TABLE(obj)   ((ScmHashTable*)(obj))
#define SCM_HASH_TABLE_P(obj)  SCM_ISA(obj, SCM_CLASS_HASH_TABLE)

#define SCM_HASH_TABLE_CORE(obj) (&SCM_HASH_TABLE(obj)->core)

SCM_EXTERN ScmObj Scm_MakeHashTableSimple(ScmHashType type, int initSize);

SCM_EXTERN ScmObj Scm_HashTableCopy(ScmHashTable *tab);

SCM_EXTERN ScmObj Scm_HashTableRef(ScmHashTable *ht,
                                   ScmObj key, ScmObj fallback);
SCM_EXTERN ScmObj Scm_HashTableSet(ScmHashTable *ht,
                                   ScmObj key, ScmObj value, int flags);
SCM_EXTERN ScmObj Scm_HashTableDelete(ScmHashTable *ht, ScmObj key);


SCM_EXTERN ScmObj Scm_HashTableKeys(ScmHashTable *table);
SCM_EXTERN ScmObj Scm_HashTableValues(ScmHashTable *table);

SCM_EXTERN ScmObj Scm_HashTableStat(ScmHashTable *table);


/*====================================================================
 * For backward compatibility.  DEPRECATED.
 */

#define SCM_HASHTABLE       SCM_HASH_TABLE
#define SCM_HASHTABLEP      SCM_HASH_TABLE_P
#define SCM_CLASS_HASHTABLE SCM_CLASS_HASH_TABLE
#define SCM_HASH_ADDRESS    SCM_HASH_EQ

typedef struct ScmHashEntryRec {
    void *key;
    void *value;
} ScmHashEntry;

SCM_EXTERN ScmHashEntry *Scm_HashTableGet(ScmHashTable *hash, ScmObj key);
SCM_EXTERN ScmHashEntry *Scm_HashTableAdd(ScmHashTable *hash,
					  ScmObj key, ScmObj value);
SCM_EXTERN ScmHashEntry *Scm_HashTablePut(ScmHashTable *hash,
					  ScmObj key, ScmObj value);

SCM_EXTERN ScmObj Scm_MakeHashTable(ScmHashProc *hashfn,
				    ScmHashCompareProc *cmpfn,
				    unsigned int initSize);

#if !defined(GAUCHE_API_0_9) && !defined(LIBGAUCHE_BODY)
#define Scm_HashIterInit(table, iter) Scm__HashIterInitCompat(table, iter)
#define Scm_HashIterNext(iter)        Scm__HashIterNextCompat(iter)

SCM_EXTERN void          Scm__HashIterInitCompat(ScmHashTable *table,
                                                 ScmHashIter *iter);
SCM_EXTERN ScmHashEntry *Scm__HashIterNextCompat(ScmHashIter *iter);
#endif

#endif /* GAUCHE_HASH_H */

