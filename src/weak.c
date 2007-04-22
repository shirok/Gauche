/*
 * weak.c - weak vectors and tables
 *
 *   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
 *  $Id: weak.c,v 1.14 2007-04-22 09:19:19 shirok Exp $
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
 * Weak box
 */

/* Weak box is not an ScmObj.  It provides a packaged 'weak pointer'
   feature to C. */

/* ptr points to the target object weakly.
   Registered flag becomes TRUE whenever ptr points to a GC_malloced object,
   thus &wbox->ptr is registered as a disappearing link.
   Note that we can distinguish a box that contaning NULL pointer, and
   a box whose target has been GCed and hence ptr is cleared---in the
   former case registered is FALSE, while in the latter case it is TRUE. */
struct ScmWeakBoxRec {
    void *ptr;
    int registered;
};

static void wbox_setvalue(ScmWeakBox *wbox, void *value)
{
    GC_PTR base = GC_base((GC_PTR)value);
    wbox->ptr = value;
    if (base != NULL) {
        GC_general_register_disappearing_link((GC_PTR)&wbox->ptr, base);
        wbox->registered = TRUE;
    } else {
        wbox->registered = FALSE;
    }
}


ScmWeakBox *Scm_MakeWeakBox(void *value)
{
    ScmWeakBox *wbox = SCM_NEW_ATOMIC(ScmWeakBox);
    wbox_setvalue(wbox, value);
    return wbox;
}

int Scm_WeakBoxEmptyP(ScmWeakBox *wbox)
{
    return (wbox->registered && wbox->ptr == NULL);
}

void Scm_WeakBoxSet(ScmWeakBox *wbox, void *newvalue)
{
    if (wbox->registered) {
        GC_unregister_disappearing_link((GC_PTR)&wbox->ptr);
        wbox->registered = FALSE;
    }
    wbox_setvalue(wbox, newvalue);
}

void *Scm_WeakBoxRef(ScmWeakBox *wbox)
{
    return wbox->ptr;           /* NB: if NULL is retured, you can't know
                                   whether box has been containing NULL or
                                   the target is GCed.  You have to call
                                   Scm_WeakBoxEmptyP to check that. */
}

/*=============================================================
 * Weak Hash Table
 */

static void weakhash_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmWeakHashTable *ht = SCM_WEAK_HASH_TABLE(obj);
    char *type = "";

    switch (ht->type) {
    case SCM_HASH_EQ:      type = "eq?"; break;
    case SCM_HASH_EQV:     type = "eqv?"; break;
    case SCM_HASH_EQUAL:   type = "equal?"; break;
    case SCM_HASH_STRING:  type = "string=?"; break;
    case SCM_HASH_GENERAL: type = "general"; break;
    default: Scm_Panic("something wrong with a hash table");
    }

    /* NB: should we also print weakness info? */
    Scm_Printf(port, "#<weak-hash-table %s %p>", type, ht);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_WeakHashTableClass, weakhash_print,
                         NULL, NULL, NULL,
                         SCM_CLASS_DICTIONARY_CPL);

static int weak_key_compare(const ScmHashCore *hc, intptr_t key,
                            intptr_t entrykey)
{
    ScmWeakHashTable *wh = SCM_WEAK_HASH_TABLE(hc->data);
    ScmWeakBox *box = (ScmWeakBox *)entrykey;
    intptr_t realkey = (intptr_t)Scm_WeakBoxRef(box);
    if (Scm_WeakBoxEmptyP(box)) {
        return FALSE;
    } else {
        return wh->realcmpfn(hc, key, realkey);
    }
}

ScmObj Scm_MakeWeakHashTableSimple(ScmHashType type,
                                   ScmWeakness weakness,
                                   int initSize,
                                   ScmObj defaultValue)
{
    ScmWeakHashTable *wh = SCM_NEW(ScmWeakHashTable);
    SCM_SET_CLASS(wh, SCM_CLASS_WEAK_HASH_TABLE);
    wh->weakness = weakness;
    wh->type = type;
    wh->defaultValue = defaultValue;
    Scm_HashCoreInitSimple(&wh->core, type, initSize, wh);
    /* NB: UGLY.  Nees cleaner way to put custom cmp fn. */
    if (weakness & SCM_WEAK_KEY) {
        wh->realcmpfn = wh->core.cmpfn;
        wh->core.cmpfn = weak_key_compare;
    }
    return SCM_OBJ(wh);
}

ScmObj Scm_WeakHashTableCopy(ScmWeakHashTable *src)
{
    ScmWeakHashTable *wh = SCM_NEW(ScmWeakHashTable);
    SCM_SET_CLASS(wh, SCM_CLASS_WEAK_HASH_TABLE);

    wh->weakness = src->weakness;
    wh->type = src->type;
    wh->defaultValue = src->defaultValue;
    wh->realcmpfn = src->realcmpfn;
    Scm_HashCoreCopy(&wh->core, &src->core);
    return SCM_OBJ(wh);
}

ScmObj Scm_WeakHashTableRef(ScmWeakHashTable *ht, ScmObj key, ScmObj fallback)
{
    ScmDictEntry *e = Scm_HashCoreSearch(SCM_WEAK_HASH_TABLE_CORE(ht),
                                         (intptr_t)key, SCM_DICT_GET);
    if (!e) return fallback;
    if (ht->weakness & SCM_WEAK_VALUE) {
        void *val = Scm_WeakBoxRef((ScmWeakBox*)e->value);
        if (Scm_WeakBoxEmptyP((ScmWeakBox*)e->value)) return fallback;
        SCM_ASSERT(val != NULL);
        return SCM_OBJ(val);
    } else {
        return SCM_DICT_VALUE(e);
    }
}

ScmObj Scm_WeakHashTableSet(ScmWeakHashTable *ht, ScmObj key, ScmObj value,
                            int flags)
{
    ScmDictEntry *e =
        Scm_HashCoreSearch(SCM_WEAK_HASH_TABLE_CORE(ht),
                           (intptr_t)key,
                           (flags&SCM_DICT_NO_CREATE)?SCM_DICT_GET:SCM_DICT_CREATE);
    if (!e) return SCM_UNBOUND;
    if (ht->weakness&SCM_WEAK_VALUE) {
        if (flags&SCM_DICT_NO_OVERWRITE) {
            if (e->value) {
                void *val = Scm_WeakBoxRef((ScmWeakBox*)e->value);
                if (!Scm_WeakBoxEmptyP((ScmWeakBox*)e->value))
                    return SCM_OBJ(val);
            }
        }
        e->value = (intptr_t)Scm_MakeWeakBox(value);
        return value;
    } else {
        if (flags&SCM_DICT_NO_OVERWRITE && e->value) {
            return SCM_DICT_VALUE(e);
        }
        SCM_DICT_SET_VALUE(e, value);
        return value;
    }
}

ScmObj Scm_WeakHashTableDelete(ScmWeakHashTable *ht, ScmObj key)
{
    ScmDictEntry *e = Scm_HashCoreSearch(SCM_WEAK_HASH_TABLE_CORE(ht),
                                         (intptr_t)key, SCM_DICT_DELETE);
    if (e && e->value) {
        if (ht->weakness&SCM_WEAK_VALUE) {
            void *val = Scm_WeakBoxRef((ScmWeakBox*)e->value);
            if (!Scm_WeakBoxEmptyP((ScmWeakBox*)e->value))
                return SCM_OBJ(val);
            else
                return SCM_UNBOUND;
        } else {
            return SCM_DICT_VALUE(e);
        }
    } else {
        return SCM_UNBOUND;
    }
}

ScmObj Scm_WeakHashTableKeys(ScmWeakHashTable *table)
{
    ScmHashIter iter;
    ScmDictEntry *e;
    ScmObj h = SCM_NIL, t = SCM_NIL;
    Scm_HashIterInit(&iter, SCM_WEAK_HASH_TABLE_CORE(table));
    while ((e = Scm_HashIterNext(&iter)) != NULL) {
        if (table->weakness & SCM_WEAK_KEY) {
            ScmWeakBox *box = (ScmWeakBox*)e->key;
            ScmObj realkey = SCM_OBJ(Scm_WeakBoxRef(box));
            if (Scm_WeakBoxEmptyP(box)) continue;
            SCM_ASSERT(realkey != NULL);
            SCM_APPEND1(h, t, realkey);
        } else {
            SCM_APPEND1(h, t, SCM_DICT_KEY(e));
        }
    }
    return h;
}

ScmObj Scm_WeakHashTableValues(ScmWeakHashTable *table)
{
    ScmHashIter iter;
    ScmDictEntry *e;
    ScmObj h = SCM_NIL, t = SCM_NIL;
    Scm_HashIterInit(&iter, SCM_WEAK_HASH_TABLE_CORE(table));
    while ((e = Scm_HashIterNext(&iter)) != NULL) {
        if (table->weakness & SCM_WEAK_VALUE) {
            ScmWeakBox *box = (ScmWeakBox*)e->value;
            ScmObj realval = SCM_OBJ(Scm_WeakBoxRef(box));
            if (Scm_WeakBoxEmptyP(box)) continue;
            SCM_ASSERT(realval != NULL);
            SCM_APPEND1(h, t, realval);
        } else {
            SCM_APPEND1(h, t, SCM_DICT_VALUE(e));
        }
    }
    return h;
}

