/*
 * weak.c - weak vectors and tables
 *
 *   Copyright (c) 2000-2005 Shiro Kawai, All rights reserved.
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
 *  $Id: weak.c,v 1.10 2005-07-28 22:46:43 shirok Exp $
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
    ScmWeakVector *v = SCM_WEAK_VECTOR(obj);
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
    ScmWeakVector *v = SCM_WEAK_VECTOR(obj);
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
    
    SCM_SET_CLASS(v, SCM_CLASS_WEAK_VECTOR);
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

SCM_DEFINE_BUILTIN_CLASS(Scm_WeakHashTableClass, NULL,
                         NULL, NULL, NULL,
                         SCM_CLASS_COLLECTION_CPL);

#if 0                           /* not working yet */

#define BACKING_STORAGE_UNIT  256

ScmObj Scm_MakeWeakHashTable(ScmHashProc hashfn,
                             ScmHashCmpProc cmpfn,
                             unsigned int initSize)
{
    ScmHashTable *ht;
    ScmWeakVector *wv;
    ScmWeakHashTable *wh;
    ScmObj *ptrs;
    int vsize, i;

    if (initSize == 0) initSize = BACKING_STORAGE_UNIT;
    ht = SCM_HASH_TABLE(Scm_MakeHashTable(hashfn, cmpfn, initSize));
    vsize = (initSize+BACKING_STORAGE_UNIT-1)&(~(BACKING_STORAGE_UNIT-1));
    wv = SCM_WEAK_VECTOR(Scm_MakeWeakVector(vsize));
    wh = SCM_NEW(ScmWeakHashTable);
    
    SCM_SET_CLASS(wh, SCM_CLASS_WEAK_HASH_TABLE);
    wh->hashTable = ht;
    wh->backingStorage = wv;
    wh->freeList = 0;
    wh->bsSize = vsize;

    ptrs = (ScmObj*)wv->pointers;
    for (i=0; i<vsize-1; i++) {
        ptrs[i] = SCM_MAKE_INT(i+1);
    }
    ptrs[i] = SCM_MAKE_INT(-1);
    
    Scm_Printf(SCM_CURERR, "bssize=%d\n", wh->backingStorage->size);
    return SCM_OBJ(wh);
}

/* Register/unregister weak ptr */
static void wh_register_weak(ScmWeakHashTable *wh, int index, ScmObj value)
{
    ScmObj *ptrs = (ScmObj*)wh->backingStorage->pointers;
    GC_general_register_disappearing_link((GC_PTR*)&ptrs[index],
                                          (GC_PTR)value);
}

static void wh_unregister_weak(ScmWeakHashTable *wh, int index)
{
    ScmObj *ptrs = (ScmObj*)wh->backingStorage->pointers;
    GC_unregister_disappearing_link((GC_PTR*)&ptrs[index]);
}

/* Push index-th slot into the free list */
static void wh_free_entry(ScmWeakHashTable *wh, int index)
{
    ScmObj *ptrs = (ScmObj*)wh->backingStorage->pointers;
    if (ptrs[index] == NULL || SCM_PTRP(ptrs[index])) {
        wh_unregister_weak(wh, index);
    }
    ptrs[index] = SCM_MAKE_INT(wh->freeList);
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
static void wh_collect_freed(ScmWeakHashTable *wh)
{
    int i;
    ScmObj *ptrs = (ScmObj*)wh->backingStorage->pointers;
    for (i=0; i<wh->bsSize; i++) {
        if (ptrs[i] == NULL) {
            wh_unregister_weak(wh, i);
            ptrs[i] = SCM_MAKE_INT(wh->freeList);
            wh->freeList = i;
        }
    }
}

/* Returns the index into the backing storage that can be used
   to contain a new entry.  If the vector is full, we reallocate
   the new vector. */
static int wh_get_free_slot(ScmWeakHashTable *wh)
{
    int index, i, newsize;
    ScmWeakVector *newwv;
    ScmObj *dst;
    ScmObj *ptr = (ScmObj*)wh->backingStorage->pointers;
    
    if (wh->freeList >= 0) {
        index = wh->freeList;
        SCM_ASSERT(SCM_INTP(ptr[index]));
        wh->freeList = SCM_INT_VALUE(ptr[index]);
        SCM_ASSERT(wh->freeList>=-1 && wh->freeList<wh->backingStorage->size);
        return index;
    }
    /* collect NULLified entry and try again */
    wh_collect_freed(wh);
    if (wh->freeList >= 0) {
        index = wh->freeList;
        SCM_ASSERT(SCM_INTP(ptr[index]));
        wh->freeList = SCM_INT_VALUE(index);
        SCM_ASSERT(wh->freeList>=-1 && wh->freeList<wh->backingStorage->size);
        return index;
    }
    /* the backing storage is full.  realloc the vector. */
    newsize = wh->bsSize + BACKING_STORAGE_UNIT;
    newwv = SCM_WEAK_VECTOR(Scm_MakeWeakVector(newsize));
    
    index = wh->bsSize;
    dst = (ScmObj*)newwv->pointers;
    for (i=0; i<index; i++) {
        dst[i] = ((ScmObj*)wh->backingStorage->pointers)[i];
    }
    for (i=index+1; i<newsize-1; i++) {
        dst[i] = SCM_MAKE_INT(i+1);
    }
    dst[i] = SCM_MAKE_INT(-1);
    wh->backingStorage = newwv;
    wh->bsSize = newsize;
    wh->freeList = index+1;
    return index;
}


/* Retrieve the value weakly associated to the key.
   Returns SCM_UNBOUND if the key is not associated, or the value
   has been GCed. */
ScmObj Scm_WeakHashTableGet(ScmWeakHashTable *wh, ScmObj key)
{
    ScmHashEntry *e;
    ScmWeakVector *wv;
    int index;
    ScmObj *ptrs;

    e = Scm_HashTableGet(wh->hashTable, key);
    if (e == NULL || !SCM_INTP(e->value)) return SCM_UNBOUND;

    index = SCM_INT_VALUE(e->value);
    Scm_Printf(SCM_CURERR, "bssize=%d\n", wh->backingStorage->size);
    Scm_Printf(SCM_CURERR, "index=%d\n", index);
    SCM_ASSERT(index >= 0 && index < wh->backingStorage->size);
    ptrs = (ScmObj*)wh->backingStorage->pointers;
    if (ptrs[index] == NULL) {
        /* The value has been GCed. */
        Scm_Printf(SCM_CURERR, "zit");
        Scm_HashTableDelete(wh->hashTable, key);
        Scm_Printf(SCM_CURERR, "zut");
        wh_free_entry(wh, index);
        Scm_Printf(SCM_CURERR, "zyt");
        return SCM_UNBOUND;
    } else {
        return ptrs[index];
    }
}

ScmObj Scm_WeakHashTablePut(ScmWeakHashTable *wh, ScmObj key, ScmObj value)
{
    ScmHashEntry *e;
    int index, new_entry;
    ScmObj *ptrs;

    /* try to add a dummy index value, to see if the key is already
       registered. */
    e = Scm_HashTableAdd(wh->hashTable, key, SCM_MAKE_INT(-1));
    if (e->value == SCM_MAKE_INT(-1)) {
        /* new entry. */
        index = wh_get_free_slot(wh);
        e->value = SCM_MAKE_INT(index);
        new_entry = TRUE;
    } else {
        index = SCM_INT_VALUE(e->value);
        new_entry = FALSE;
    }
    Scm_Printf(SCM_CURERR, "allocated index=%d\n", index);

    ptrs = (ScmObj*)wh->backingStorage->pointers;

    if (!new_entry && (ptrs[index] == NULL || SCM_PTRP(ptrs[index]))) {
        wh_unregister_weak(wh, index);
    }

    ptrs[index] = value;

    if (SCM_PTRP(value)) {
        wh_register_weak(wh, index, value);
    }
    return value;
}

int Scm_WeakHashTableDelete(ScmWeakHashTable *wh, ScmObj key)
{
    ScmHashEntry *e;
    int index;

    e = Scm_HashTableDelete(wh->hashTable, key);
    if (e) {
        index = SCM_INT_VALUE(e->value);
        wh_free_entry(wh, index);
        return TRUE;
    } else {
        return FALSE;
    }
}

#endif

