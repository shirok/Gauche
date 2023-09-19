/*
 * threadlocal.c - thread locals
 *
 *   Copyright (c) 2022-2023  Shiro Kawai  <shiro@acm.org>
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
#include "gauche/priv/vmP.h"

/* Thread local storage are kept in each VM, and each thread local object
 * has index of it.   Thread locals are used in the core so we define
 * it in src/ instead of ext/threads/.
 *
 * We have inheritable and non-inheritable thread locals.  Inheritable
 * thread locals are initialized with the value of the thread that calls
 * make-thread.  Non-inheritable ones are initialized by the init-value
 * given to the thread local constructor.
 *
 * TODO: We now need to allocate tl slots to every thread.
 * We may be able to use a tree instead of a flat vector so that we
 * can avoid allocation of leaf nodes until they are accessed.
 */

#define THREAD_LOCAL_INIT_SIZE 64
#define THREAD_LOCAL_GROW      16

/* Global table to track free slots of the thread local table.
 * tl_freelists[].freelist array has a list of free slots, terminated by -1.
 */
struct tl_freelist {
    ScmSize size;
    ScmSize head;
    ScmSize *freelist;
} tl_freelists[2];

static ScmInternalMutex tl_mutex = SCM_INTERNAL_MUTEX_INITIALIZER;

static void tl_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx);

SCM_DEFINE_BUILTIN_CLASS(Scm_ThreadLocalClass,
                         tl_print, NULL, NULL, NULL,
                         SCM_CLASS_OBJECT_CPL);

static void tl_print(ScmObj obj,
                     ScmPort *out,
                     ScmWriteContext *ctx SCM_UNUSED)
{
    Scm_Printf(out, "#<%A %S @%p>",
               Scm_ShortClassName(Scm_ClassOf(obj)),
               SCM_THREAD_LOCAL(obj)->name,
               obj);
}

static void init_tl_freelist(void)
{
    for (int i=0; i<2; i++) {
        tl_freelists[i].freelist
            = SCM_NEW_ATOMIC_ARRAY(ScmSize, THREAD_LOCAL_INIT_SIZE);
        for (ScmSize j=0; j<THREAD_LOCAL_INIT_SIZE-1; j++) {
            tl_freelists[i].freelist[j] = j+1;
        }
        tl_freelists[i].size = THREAD_LOCAL_INIT_SIZE;
        tl_freelists[i].head = 0;
        tl_freelists[i].freelist[THREAD_LOCAL_INIT_SIZE-1] = -1;
    }
}

static ScmSize get_tl_slot(int kind)
{
    ScmSize r = -1;
    SCM_INTERNAL_MUTEX_LOCK(tl_mutex);
    if (tl_freelists[kind].head >= 0) {
        r = tl_freelists[kind].head;
        tl_freelists[kind].head = tl_freelists[kind].freelist[r];
        tl_freelists[kind].freelist[r] = -1;
    } else {
        ScmSize oldsize = tl_freelists[kind].size;
        ScmSize newsize = oldsize + THREAD_LOCAL_GROW;
        ScmSize *newlist = SCM_NEW_ATOMIC_ARRAY(ScmSize, newsize);
        memcpy(tl_freelists[kind].freelist, newlist, oldsize);
        for (ScmSize i=oldsize+1; i < newsize-1; i++) {
            newlist[i] = i+1;
        }
        newlist[oldsize] = -1;
        newlist[newsize-1] = -1;
        tl_freelists[kind].freelist = newlist;
        tl_freelists[kind].size = newsize;
        tl_freelists[kind].head = oldsize+1;
        r = oldsize;
    }
    SCM_INTERNAL_MUTEX_UNLOCK(tl_mutex);
    return r;
}

#if 0
static void return_tl_slot(int kind, ScmSize slot)
{
    SCM_INTERNAL_MUTEX_LOCK(tl_mutex);
    tl_freelists[kind].freelist[slot] = tl_freelists[kind].head;
    tl_freelists[kind].head = slot;
    SCM_INTERNAL_MUTEX_UNLOCK(tl_mutex);
}
#endif

/* Init table.  For primordial thread, base == NULL.  For non-primordial
 * thread, base is the current thread (this must be called from the
 * creator thread).
 */
static void fill_tl_vector(ScmVMThreadLocalVector *dst,
                           const ScmVMThreadLocalVector *base)
{
    if (base) {
        /* NB: In this case, the caller is the owner thread of BASE,
           so we don't need to worry about base->parameters being
           modified during copying. */
        dst->vector = SCM_NEW_ARRAY(ScmObj, base->size);
        dst->size = base->size;
        for (ScmSize i=0; i<dst->size; i++) {
            dst->vector[i] = base->vector[i];
        }
    } else {
        dst->vector = SCM_NEW_ARRAY(ScmObj, THREAD_LOCAL_INIT_SIZE);
        dst->size = THREAD_LOCAL_INIT_SIZE;
        for (ScmSize i=0; i<dst->size; i++) {
            dst->vector[i] = SCM_UNBOUND;
        }
    }
}

ScmVMThreadLocalTable *Scm__MakeVMThreadLocalTable(ScmVM *base)
{
    ScmVMThreadLocalTable *t = SCM_NEW(ScmVMThreadLocalTable);
    if (base) {
        fill_tl_vector(&t->vs[SCM_THREAD_LOCAL_VECTOR_INHERITABLE],
                       &base->threadLocals->vs[SCM_THREAD_LOCAL_VECTOR_INHERITABLE]);
        fill_tl_vector(&t->vs[SCM_THREAD_LOCAL_VECTOR_NONINHERITABLE], NULL);
    } else {
        fill_tl_vector(&t->vs[SCM_THREAD_LOCAL_VECTOR_INHERITABLE], NULL);
        fill_tl_vector(&t->vs[SCM_THREAD_LOCAL_VECTOR_NONINHERITABLE], NULL);
    }
    return t;
}

static int tl_kind(const ScmThreadLocal *tl)
{
    if (tl->flags & SCM_THREAD_LOCAL_INHERITABLE)
        return SCM_THREAD_LOCAL_VECTOR_INHERITABLE;
    else
        return SCM_THREAD_LOCAL_VECTOR_NONINHERITABLE;
}

static void ensure_tl_slot(ScmVMThreadLocalTable *t, ScmSize index, int kind)
{
    ScmVMThreadLocalVector *p = &t->vs[kind];
    if (index >= p->size) {
        ScmSize newsiz =
            ((index+THREAD_LOCAL_GROW)/THREAD_LOCAL_GROW)*THREAD_LOCAL_GROW;
        ScmObj *newvec = SCM_NEW_ARRAY(ScmObj, newsiz);

        ScmSize i;
        for (i=0; i < p->size; i++) {
            newvec[i] = p->vector[i];
            p->vector[i] = SCM_FALSE; /*be friendly to GC*/
        }
        for (; i < newsiz; i++) {
            newvec[i] = SCM_UNBOUND;
        }
        p->vector = newvec;
        p->size = newsiz;
    }
}

/*
 * Create a thread local
 */
static void tl_finalize(ScmObj obj, void *data SCM_UNUSED)
{
    ScmThreadLocal *tl = SCM_THREAD_LOCAL(obj);
    if (tl->index >= 0) {
#if 0
        int kind = ((tl->flags & SCM_THREAD_LOCAL_INHERITABLE)
                    ? SCM_THREAD_LOCAL_VECTOR_INHERITABLE
                    : SCM_THREAD_LOCAL_VECTOR_NONINHERITABLE);
        return_tl_slot(kind, tl->index);
#endif
        tl->index = -1;
    }
}

ScmThreadLocal *Scm_MakeThreadLocal(ScmObj name,
                                    ScmObj initval,
                                    u_long flags)
{
    int kind = ((flags & SCM_THREAD_LOCAL_INHERITABLE)
                ? SCM_THREAD_LOCAL_VECTOR_INHERITABLE
                : SCM_THREAD_LOCAL_VECTOR_NONINHERITABLE);
    ScmSize index = get_tl_slot(kind);
    ensure_tl_slot(Scm_VM()->threadLocals, index, kind);

    ScmThreadLocal *tl = SCM_NEW(ScmThreadLocal);
    SCM_SET_CLASS(tl, SCM_CLASS_THREAD_LOCAL);
    tl->name = name;
    tl->index = index;
    tl->initialValue = initval;
    tl->flags = flags;
    Scm_RegisterFinalizer(SCM_OBJ(tl), tl_finalize, NULL);
    return tl;
}

/*
 * Accessor & modifier
 */

ScmObj Scm_ThreadLocalRef(ScmVM *vm, const ScmThreadLocal *tl)
{
    ScmVMThreadLocalVector *t = &vm->threadLocals->vs[tl_kind(tl)];

    ScmObj result;
    if (tl->index >= t->size) {
        result = tl->initialValue;
    } else {
        result = t->vector[tl->index];
        if (SCM_UNBOUNDP(result)) {
            result = t->vector[tl->index] = tl->initialValue;
        }
    }
    return result;
}

ScmObj Scm_ThreadLocalSet(ScmVM *vm, const ScmThreadLocal *tl,
                          ScmObj val)
{
    ScmObj oldval = SCM_UNBOUND;
    ScmVMThreadLocalVector *t = &vm->threadLocals->vs[tl_kind(tl)];

    if (tl->index >= t->size) {
        ensure_tl_slot(vm->threadLocals, tl->index, tl_kind(tl));
    } else {
        oldval = t->vector[tl->index];
    }
    if (SCM_UNBOUNDP(oldval)) {
        oldval = tl->initialValue;
    }

    t->vector[tl->index] = val;

    return oldval;
}

void Scm__InitThreadLocal(void)
{
    init_tl_freelist();
    SCM_INTERNAL_MUTEX_INIT(tl_mutex);
    /* We don't initialize Scm_ThraedLocalClass yet, since class
       stuff is not initialized yet.  The class is initialized in
       class.c. */
}
