/*
 * threadlocal.c - thread locals
 *
 *   Copyright (c) 2022  Shiro Kawai  <shiro@acm.org>
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
 * given to the thread local consturctor.
 *
 * TODO: We now need to allocate tl slots to every thread.
 * We may be able to use a tree instead of a flat vector so that we
 * can avoid allocation of leaf nodes until they are accessed.
 */

#define THREAD_LOCAL_INIT_SIZE 64
#define THREAD_LOCAL_GROW      16

/* Every time a new thread local is created (in any thread), it is
 * given a unique index in the process.  Negative index is for
 * inheritable thread locals (-index-1 gives the index to the vector).
 */
static ScmSize next_tl_noninheritable_index = 0;
static ScmSize next_tl_inheritable_index = -1;
static ScmInternalMutex tl_mutex = SCM_INTERNAL_MUTEX_INITIALIZER;

static void tl_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx);

SCM_DEFINE_BASE_CLASS(Scm_ThreadLocalClass, ScmThreadLocal,
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

static ScmVMThreadLocalVector *get_tl_vector(ScmVMThreadLocalTable *t,
                                             ScmSize index,
                                             ScmSize *vindex)
{
    int kind = (index < 0
                ? SCM_THREAD_LOCAL_VECTOR_INHERITABLE
                : SCM_THREAD_LOCAL_VECTOR_NONINHERITABLE);
    *vindex = index < 0 ? (-index-1) : index;
    return &t->vs[kind];
}

static void ensure_tl_slot(ScmVMThreadLocalTable *t, ScmSize index)
{
    ScmSize vindex;
    ScmVMThreadLocalVector *p = get_tl_vector(t, index, &vindex);

    if (vindex >= p->size) {
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
ScmThreadLocal *Scm_MakeThreadLocal(ScmClass *klass,
                                    ScmObj name,
                                    ScmObj initval,
                                    u_long flags)
{
    ScmSize index;

    SCM_INTERNAL_MUTEX_LOCK(tl_mutex);
    if (flags & SCM_THREAD_LOCAL_INHERITABLE) {
        index = next_tl_inheritable_index--;
    } else {
        index = next_tl_noninheritable_index++;
    }
    SCM_INTERNAL_MUTEX_UNLOCK(tl_mutex);
    ensure_tl_slot(Scm_VM()->threadLocals, index);

    ScmThreadLocal *tl = SCM_NEW(ScmThreadLocal);
    SCM_SET_CLASS(tl, SCM_CLASS_THREAD_LOCAL);
    tl->name = name;
    tl->index = index;
    tl->initialValue = initval;
    tl->flags = flags;
    return tl;
}

/*
 * Accessor & modifier
 */

ScmObj Scm_ThreadLocalRef(ScmVM *vm, const ScmThreadLocal *tl)
{
    ScmSize vindex;
    ScmVMThreadLocalVector *t = get_tl_vector(vm->threadLocals,
                                              tl->index, &vindex);
    ScmObj result;
    if (vindex >= t->size) {
        result = tl->initialValue;
    } else {
        result = t->vector[vindex];
        if (SCM_UNBOUNDP(result)) {
            result = t->vector[vindex] = tl->initialValue;
        }
    }
    if (tl->flags & SCM_PARAMETER_LAZY) return Scm_Force(result);
    else return result;
}

ScmObj Scm_ThreadLocalSet(ScmVM *vm, const ScmThreadLocal *tl,
                          ScmObj val)
{
    ScmObj oldval = SCM_UNBOUND;
    ScmSize vindex;
    ScmVMThreadLocalVector *t = get_tl_vector(vm->threadLocals,
                                              tl->index, &vindex);
    if (vindex >= t->size) {
        ensure_tl_slot(vm->threadLocals, tl->index);
    } else {
        oldval = t->vector[vindex];
    }
    if (SCM_UNBOUNDP(oldval)) {
        oldval = tl->initialValue;
    }

    t->vector[vindex] = val;

    if (tl->flags & SCM_PARAMETER_LAZY) return Scm_Force(oldval);
    else return oldval;
}

void Scm__InitThreadLocal(void)
{
    SCM_INTERNAL_MUTEX_INIT(tl_mutex);
    /* We don't initialize Scm_ThraedLocalClass yet, since class
       stuff is not initialized yet.  The class is initialized in
       class.c. */
}
