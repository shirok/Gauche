/*
 * weak.c - weak vectors and tables
 *
 *   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
 *  $Id: weak.c,v 1.5.2.1 2005-01-10 00:22:39 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"

/*=============================================================
 * Weak vector
 *
 *  A weak vector is like a vector of Scheme objects, except
 *  it doesn't prevent the referenced objects to be garbage-collected.
 *  Internally, it is implemented using "disappearing link" feature
 *  of Boehm GC; when the referenced object is collected, the pointer
 *  in the vector is set to NULL.
 *  It is important to keep track of whether the entry of the vector
 *  is registered as a disappearing link or not, for you can't register
 *  the same location more than once.
 */

static void weakvector_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    int i;
    ScmWeakVector *v = SCM_WEAKVECTOR(obj);
    ScmObj *ptrs = (ScmObj*)v->pointers;
    Scm_Printf(port, "#,(<weak-vector> %d", v->size);
    for (i=0; i<v->size; i++) {
        SCM_PUTC(' ', port);
        if (ptrs[i]) Scm_Write(ptrs[i], SCM_OBJ(port), ctx->mode);
        else         Scm_Write(SCM_FALSE, SCM_OBJ(port), ctx->mode);
    }
    SCM_PUTC(')', port);
}

static void weakvector_finalize(ScmObj obj, void *data)
{
    int i;
    ScmWeakVector *v = SCM_WEAKVECTOR(obj);
    ScmObj *p = (ScmObj*)v->pointers;
    for (i=0; i<v->size; i++) {
        if (p[i]==NULL || SCM_PTRP(p[i])) {
            GC_unregister_disappearing_link((GC_PTR*)&p[i]);
        }
        p[i] = SCM_FALSE;       /* safety */
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_WeakVectorClass, weakvector_print,
                         NULL, NULL, NULL,
                         SCM_CLASS_SEQUENCE_CPL);

ScmObj Scm_MakeWeakVector(int size)
{
    int i;
    ScmObj *p;
    ScmWeakVector *v = SCM_NEW(ScmWeakVector);
    
    SCM_SET_CLASS(v, SCM_CLASS_WEAKVECTOR);
    v->size = size;
    /* Allocate pointer array by ATOMIC, so that GC won't trace the
       pointers in it.  */
    p = SCM_NEW_ATOMIC2(ScmObj*, size * sizeof(ScmObj));
    for (i=0; i<size; i++) p[i] = SCM_FALSE;
    v->pointers = (void*)p;
    Scm_RegisterFinalizer(SCM_OBJ(v), weakvector_finalize, NULL);
    return SCM_OBJ(v);
}

ScmObj Scm_WeakVectorRef(ScmWeakVector *v, int index, ScmObj fallback)
{
    ScmObj *p;
    if (index < 0 || index >= v->size) {
        if (SCM_UNBOUNDP(fallback)) {
            Scm_Error("argument out of range: %d", index);
        } else {
            return fallback;
        }
    }
    p = (ScmObj*)v->pointers;
    if (p[index] == NULL) {
        if (SCM_UNBOUNDP(fallback)) return SCM_FALSE;
        else return fallback;
    } else {
        return p[index];
    }
}

ScmObj Scm_WeakVectorSet(ScmWeakVector *v, int index, ScmObj value)
{
    ScmObj *p;
    if (index < 0 || index >= v->size) {
        Scm_Error("argument out of range: %d", index);
    }
    p = (ScmObj*)v->pointers;

    /* unregister the location if it was registered before */
    if (p[index] == NULL || SCM_PTRP(p[index])) {
        GC_unregister_disappearing_link((GC_PTR*)&p[index]);
    }

    p[index] = value;
    /* register the location if the value is a heap object */
    if (SCM_PTRP(value)) {
        GC_general_register_disappearing_link((GC_PTR*)&p[index], (GC_PTR)value);
    }
    return SCM_UNDEFINED;
}

/*=============================================================
 * Weak hash table
 *
 *  A weak hash table is realized by combination of a normal hash 
 *  table and a weak vector.  The normal hash table keeps an index
 *  to the weak vector, and the weak vector keeps weak pointer to
 *  the actual object.
 *
 *  This two-level structure is chosen so that the weak pointer
 *  logic won't mess up the existing hash-table API.  Merging
 *  weak hash to the existing hash table would have required client
 *  code to treat ScmHashEntry differently whether the hash table
 *  is weak or not, so integration wouldn't make things cleaner.
 *
 *  Unused entries of the backing storage weak vector is chained to
 *  a free list, which one entry contains Scheme integer of the index
 *  of the next entry.  The last entry contains Scheme integer -1.
 *
 *
 *                             backing storage
 *                                +----------+
 *   freeList : 1              ->0|    ---------> value of entry #1
 *                            /   +----------+
 *                           /   1|    3     |
 *   hash entry#0 : 2 -\    /     +----------+
 *                      \--/---->2|    ---------> value of entry #2
 *                        /       +----------+
 *   hash entry#1 : 0 ---/       3|    4     |
 *                                +----------+
 *                               4|   -1     |  : end of freeList
 *   hash entry#3 : 5 -\          +----------+
 *                      \------->5|    ---------> value of entry #3
 *                                +----------+
 *                                :          :
 *
 *  The backing storage entry will be replaced by NULL if the object
 *  pointed from it is GC-ed.  We don't use Scm_WeakVectorRef and
 *  Scm_WeakVectorSet to access the backing storage, since those APIs
 *  hide the nullified pointer.  However we do rely on weak vector's
 *  constructor and finalizer.
 */
#if 0
SCM_DEFINE_BUILTIN_CLASS(Scm_WeakHashTableClass, NULL,
                         NULL, NULL, NULL,
                         SCM_CLASS_COLLECTION_CPL);

#define BACKING_STORAGE_UNIT  256

ScmObj Scm_MakeWeakHashTable(ScmHashProc hashfn,
                             ScmHashCmpProc cmpfn,
                             unsigned int initSize)
{
    ScmHashTable *ht;
    ScmWeakVector *wv;
    ScmWeakHashTable *w;
    int vsize, i;
    
    ht = SCM_HASHTABLE(Scm_MakeHashTable(hashfn, cmpfn, initSize));
    vsize = (initSize+BACKING_STORAGE_UNIT-1)&(~(BACKING_STORAGE_UNIT-1));
    wv = SCM_WEAKVECTOR(Scm_MakeWeakVector(vsize));
    w  = SCM_NEW(ScmWeakHashTable);
    
    SCM_SET_CLASS(w, SCM_CLASS_WEAK_HASH_TABLE);
    w->hashTable = ht;
    w->backingStorage = wv;
    w->freeList = 0;
    w->bsSize = vsize;

    return SCM_OBJ(w);
}

/* Push index-th slot into the free list */
static void wh_free_entry(ScmWeakHashTable *wh, int index)
{
    if (wh->wv->pointers[index] == NULL
        || SCM_PTRP(wh->wv->pointers[index])) {
        GC_unregister_disappearing_link((GC_PTR*)&wh->wv->pointers[index]);
    }
    wh->wv->pointers[index] = SCM_MAKE_INT(wh->freeList);
    wh->freeList = index;
}

/* Scan backingStorage */
/* NB: gc can run on another thread during this operation.  It should
   be OK, since it would replace the entry to NULL atomically; we'll miss
   the newly nullified entry in the already-scanned area, but that will
   be catched by the next call of wh_collect_freed().
   We don't cache wh->wv, however, since the other thread may extend
   the hash table and change the entry.  It is a program error, but we
   don't want to crash the system. */
static void wh_collect_freed(ScmWeakHashTable wh)
{
    int i;
    for (i=0; i<wh->bsSize; i++) {
        if (wh->wv->pointers[i] == NULL) {
            wh->wv->pointers[i] = SCM_MAKE_INT(wh->freeList);
            wh->freeList = i;
        }
    }
}

ScmObj Scm_WeakHashTableGet(ScmWeakHashTable *wh, ScmObj key)
{
    ScmHashEntry *e;
    ScmWeakVector *wv;
    int index;
    ScmObj val;
    ScmObj *p;

    SCM_ASSERT(wh->hashTable != NULL);
    SCM_ASSERT(wh->backingStorage != NULL);
    wv = wh->backingStorage;

    e = Scm_HashTableGet(wh->hashTable, key);
    if (e == NULL || !SCM_INTP(e->value)) return SCM_UNBOUND;

    index = SCM_INT_VALUE(e->value);
    SCM_ASSERT(index >= 0 && index < wv->size);
    p = (ScmObj*)wv->pointers;
    if (p[index] == NULL) {
        Scm_HashTableDelete(wv->hashTable, key);
        
        return SCM_UNBOUND;
    } else {
        return p[index];
    }
}

#endif

