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
 *  $Id: weak.c,v 1.11 2005-07-30 06:10:02 shirok Exp $
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
 */

static ScmClass *weakhash_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_HashTableClass),
    SCM_CLASS_STATIC_PTR(Scm_CollectionClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

SCM_DEFINE_BUILTIN_CLASS(Scm_WeakHashTableClass, NULL,
                         NULL, NULL, NULL,
                         weakhash_cpl);

#if 0
struct weakhash_rec {
    int weakness;
};

#define WEAK_CHECK(wh)                                                  \
    do {                                                                \
        if (!SCM_ISA(wh, SCM_CLASS_WEAK_HASH_TABLE)) {                  \
            Scm_Error("weak hash table required, but got %S", wh);      \
        }                                                               \
    } while (0)

static int get_weakness(ScmHashTable *wh)
{
    WEAK_CHECK(wh);
    return ((struct weakhash_rec*)wh->data)->weakness;
}

static unsigned long weak_hash(ScmHashTable *wh, void *key)
{
    if (((struct weakhash_rec*)wh->data)->weakness & SCM_HASH_WEAK_KEY) {
        return Scm_EqHash(SCM_OBJ(((void**)key)[0]));
    } else {
        return Scm_EqHash(SCM_OBJ(key));
    }
}

static int weak_hash_cmp(ScmHashTable *wh, void *key, ScmHashEntry *e)
{
    if (((struct weakhash_rec*)wh->data)->weakness & SCM_HASH_WEAK_KEY) {
        return (key == ((void**)(e->key))[0]);
    } else {
        return (key == e->key);
    }
}

ScmObj Scm_MakeWeakHashTable(int hashtype, int weakness, unsigned int initSize)
{
    ScmHashTable *ht;
    struct weakhash_rec *data = SCM_NEW(struct weakhash_rec);
    data->weakness = weakness;
    ht = Scm_MakeHashTableFull(SCM_CLASS_WEAK_HASH_TABLE, SCM_HASH_RAW,
                               weak_hash, weak_hash_cmp, initSize, data);
    return SCM_OBJ(ht);
}

void Scm_WeakHashTablePutRaw(ScmHashTable *wh, void *key, void *value)
{
    int weakness = get_weakness(wh);
    ScmHashEntry *e;

    if (weakness & SCM_HASH_WEAK_KEY) {
        void *dummy[1];
        dummy[0] = key;
        e = Scm_HashTableGetRaw(wh, (void*)dummy, NULL);
    } else {
        e = Scm_HashTableGetRaw(wh, key, NULL);
    }
    
    if (weakness & SCM_HASH_WEAK_VALUE) {
        void **cell = SCM_NEW_ATOMIC(void **);
        cell[0] = value;
        GC_general_register_disappearing_link((GC_PTR*)cell, (GC_PTR)value);
        vv = (void *)cell;
    } else {
        vv = value;
    }

    if (e->value) {
        /* We already had an entry */
        if (weakness & SCM_HASH_WEAK_VALUE) {
            GC_unregister_disapperaing_link((GC_PTR*)e->value);
            e->value = value;
            GC_general_register_disappearing_link((GC_PTR*)cell,
                                                  (GC_PTR)value);
        } else {
            e->value = value;
        }
    } else {
        void *kk, *vv;
        if (weakness && SCM_HASH_WEAK_KEY) {
            void **cell = SCM_NEW_ATOMIC(void **);
            cell[0] = key;
            
        }
    }
}
#endif
