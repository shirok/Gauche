/*
 * dispatch.c - method dispatch accelerator
 *
 *   Copyright (c) 2017-2018  Shiro Kawai  <shiro@acm.org>
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
#include "gauche/priv/atomicP.h"
#include "gauche/priv/dispatchP.h"

/*
 * Method dispatch acceleration
 *
 *  gf->dispatcher may contain a structure that accelerates method dispatch.
 *  The structure must be treated as opaque from other parts.
 *
 *  In the current implementation, we use special open-addressing hash table.
 *  We don't use ScmHashCore for a few reasons:
 *  
 *   - We mutex modification of the table by gf->mutex, but we don't want
 *     to lock for searching it.  ScmHashCore doesn't guarantee consistency
 *     of reading while modifying.
 *   - It is in performance critical path, and we can take advantage of
 *     domain knowledge to make it faster than generic implementation.
 *
 *  At this moment, the dispatch accelerator isn't built automatically.
 *  You need to call gauche.object#generic-build-dispatcher! explicitly
 *  on a generic function.  The effect varies depending on the methods
 *  the GF has, so we selectively turn on this feature for speed-sensitive
 *  GFs and see how it goes.  If it is proven to be effective enough,
 *  we might implement an automated mechanism when GF meets certain
 *  criteria.
 *
 *  We take advantage of the following facts:
 *
 *   - Once we hit the leaf method we don't need to find less-specific
 *     methods at all.
 *   - Some generic functions has methods most of which are leaf methods,
 *     and can be mostly specialized by a single argument.  The typical
 *     example is `ref'.
 *
 *  When methods are mostly specilized by N-th argument, we call the N
 *  as AXIS (zero based).  Typically, methods tend to be specialized by
 *  the first argument, in which case axis is 0.
 *
 *  The method dispatcher contains a hash table, where each key is
 *  a tuple (<class>, <number-of-args>), and its value is a list of
 *  methods, whose axis specializer is <class> and which can take
 *  <number-of-args>.
 *
 *  When a GF with dispatch accelerator is called, we retrieve the class
 *  of the axis of the actual argument, and lookup the hash table with
 *  that class and the number of actual arguments.  If we have a hit,
 *  and the entry solely consists of leaf methods,
 *  the list of methods are the list of applicable methods---since there
 *  are no more specific methods, and we don't need to look for less specific
 *  methods.  This means...
 *
 *   - We don't need to go through the entire method list to find
 *     all applicable methods.
 *   - We don't need to allocate to construct the list of applicable
 *     methods.
 *
 *  Note that if the entry has at least one non-leaf methods, we have to
 *  fall back to the normal path, since there may be other applicable
 *  methods that might be called via next-method.  To cut the overhead
 *  of checking, we keep the list of leaf methods and the one of non-leaf
 *  methods separately.
 */

/* On concurrent access:
 *
 *   We don't want to lock the method hash for every invocation of GF.
 *   So we employ some atomic pointer operations so that readers can
 *   see the consistent state even another thread is modifying it.
 *
 *   We still aquire GF lock whenever we modify the mhash, so there's
 *   at most one thread that's modifying it.  Modification is only required
 *   when a new method is added to the GF or any one of classes that
 *   specializes one of the methods is redefined.
 *
 *   The modification is done by making a modified copy of (sub)structure
 *   and then swap the pointer atomically, thus the concurrent reader
 *   won't see inconsistent state.
 *
 *   The locking on GF is done in class.c.
 */

struct ScmMethodDispatcherRec {
    int axis;                   /* Which argument we look at?
                                   This is immutable. */
    AO_t methodHash;            /* mhash.  In case mhash is extended,
                                   we atomically swap reference. */
};

typedef struct mhash_entry_rec {
    ScmClass *klass;
    int nargs;
    ScmObj leaves;             /* list of matching leaf methods */
    ScmObj nonleaves;          /* list of matching non-leaf methods */
} mhash_entry;

typedef struct mhash_rec {
    int size;                   /* # of bins.  power of 2. */
    int num_entries;            /* # of active entries.  */
    AO_t bins[1];               /* Table.  Each entry may have one of:
                                     0 - free
                                     1 - deleted
                                     mhash_entry
                                   We might atomically change the bin value,
                                   which shouldn't affect concurrently reading
                                   threads.
                                */
} mhash;


static inline u_long mhashfn(ScmClass *k, int nargs)
{
    return (((SCM_WORD(k) >> 3) + nargs) * 2654435761UL) >> 20;
}

static mhash *make_mhash(int size)
{
    mhash *mh = SCM_NEW2(mhash*, sizeof(mhash)+(sizeof(AO_t)*(size-1)));
    mh->size = size;
    mh->num_entries = 0;
    for (int i=0; i<size; i++) mh->bins[i] = (AO_t)0;
    return mh;
}

static ScmObj mhash_probe(const mhash *h, ScmClass *k, int nargs)
{
    /* Quadratic probing
       j = H_i(k,nargs) + (i + i^2)/2
       H_{i+1} - H_i = ((i+1) + (i+1)^2 - (i + i^2))/2 = i + 1
     */
    u_long j = mhashfn(k, nargs) & (h->size - 1);
    int i = 0;
    for (; i < h->size; i++) {
        ScmWord w = SCM_WORD(AO_load(&h->bins[j]));
        if (w == 0) break;
        if (w != 1) {
            mhash_entry *e = (mhash_entry*)w;
            if (e->klass == k && e->nargs == nargs) {
                if (SCM_NULLP(e->nonleaves)) return e->leaves;
                else return SCM_FALSE;
            }
        }
        j = (j + i + 1) & (h->size - 1);
    }
    return SCM_FALSE;
}

static mhash *mhash_insert_1(mhash *h, ScmClass *k, int nargs, ScmMethod *m)
{
    u_long j = mhashfn(k, nargs) & (h->size - 1);
    long free_slot = -1;
    ScmObj ltail = SCM_NIL, ntail = SCM_NIL;
    int i = 0;
    for (; i < h->size; i++) {
        ScmWord w = SCM_WORD(AO_load(&h->bins[j]));
        if (w == 0) {           /* end of chain */
            if (free_slot < 0) free_slot = j;
            break;
        }
        if (w == 1) {
            if (free_slot < 0) free_slot = j;
            continue;
        }
        mhash_entry *e = (mhash_entry*)w;
        if (e->klass == k && e->nargs == nargs) {
            free_slot = j;
            ltail = e->leaves;
            ntail = e->nonleaves;
            h->num_entries--;
            break;
        }
        j = (j + i + 1) & (h->size - 1);
        
    }
    SCM_ASSERT(free_slot >= 0);
    mhash_entry *e = SCM_NEW(mhash_entry);
    e->klass = k;
    e->nargs = nargs;
    e->leaves = SCM_METHOD_LEAF_P(m)? Scm_Cons(SCM_OBJ(m), ltail) : ltail;
    e->nonleaves = SCM_METHOD_LEAF_P(m) ? ntail : Scm_Cons(SCM_OBJ(m), ntail);
    AO_store_full(&h->bins[free_slot], (AO_t)e);
    h->num_entries++;
    return h;
}

static mhash *mhash_insert(mhash *h, ScmClass *k, int nargs, ScmMethod *m)
{
    if (h->size <= h->num_entries*2) {
        /* extend */
        mhash *nh = make_mhash(h->size*2);
        nh->num_entries = h->num_entries;
        for (int i = 0; i < h->size; i++) {
            ScmWord w = h->bins[i];
            if (w == 0 || w == 1) continue;
            mhash_entry *e = (mhash_entry*)w;
            u_long j = mhashfn(e->klass, e->nargs) & (nh->size - 1);
            int k = 0;
            for (; k < nh->size; k++) {
                if (SCM_WORD(nh->bins[j]) == 0) {
                    nh->bins[j] = w;
                    break;
                }
                j = (j + k + 1) & (nh->size - 1);
            }
            SCM_ASSERT(k < nh->size);
        }
        h = nh;
    }
    return mhash_insert_1(h, k, nargs, m);
}

static mhash *mhash_delete(mhash *h, ScmClass *k, int nargs, ScmMethod *m)
{
    u_long j = mhashfn(k, nargs) & (h->size - 1);
    long free_slot = -1;
    ScmObj tail = SCM_NIL;
    
    int i = 0;
    for (; i < h->size; i++) {
        ScmWord w = SCM_WORD(AO_load(&h->bins[j]));
        if (w == 0) break;
        if (w == 1) continue;
        mhash_entry *e = (mhash_entry*)w;
        if (e->klass == k && e->nargs == nargs) {
            ScmObj ml = e->leaves;
            ScmObj mn = e->nonleaves;
            if (SCM_PAIRP(ml) && SCM_EQ(SCM_CAR(ml), SCM_OBJ(m))) {
                ml = SCM_CDR(ml); /* fast path */
            } else {
                ml = Scm_Delete(SCM_OBJ(m), ml, SCM_CMP_EQ);
            }
            if (SCM_PAIRP(mn) && SCM_EQ(SCM_CAR(mn), SCM_OBJ(m))) {
                mn = SCM_CDR(mn); /* fast path */
            } else {
                mn = Scm_Delete(SCM_OBJ(m), mn, SCM_CMP_EQ);
            }

            if (SCM_NULLP(ml) && SCM_NULLP(ml)) {
                h->num_entries--;
                AO_store(&h->bins[j], 1); /* mark as deleted */
            } else {
                mhash_entry *e = SCM_NEW(mhash_entry);
                e->klass = k;
                e->nargs = nargs;
                e->leaves = ml;
                e->nonleaves = mn;
                AO_store_full(&h->bins[j], (AO_t)e);
            }
            break;
        }
        j = (j + i + 1) & (h->size - 1);
    }
    return h;
}

static void mhash_print(mhash *h, ScmPort *out)
{
    Scm_Printf(out, "mhash size=%d num_entries=%d\n", h->size, h->num_entries);
    for (int i=0; i<h->size; i++) {
        ScmWord w = SCM_WORD(h->bins[i]);
        if (w == 0) {
            Scm_Printf(out, "[%3d] empty\n\n\n", i);
        } else if (w == 1) {
            Scm_Printf(out, "[%3d] deleted\n\n\n", i);
        } else {
            mhash_entry *e = (mhash_entry*)w;
            Scm_Printf(out, "[%3d] %lu %S(%d)\n", i, 
                       (mhashfn(e->klass, e->nargs) % h->size),
                       e->klass, e->nargs);
            Scm_Printf(out, "  Leaves:   %S\n", e->leaves);
            Scm_Printf(out, "  NonLeaves:%S\n", e->nonleaves);
        }
    }
}

static mhash *add_method_to_dispatcher(mhash *h, int axis, ScmMethod *m)
{
    int req = SCM_PROCEDURE_REQUIRED(m);
    if (req >= axis) {
        ScmClass *klass = m->specializers[axis];
        if (SCM_PROCEDURE_OPTIONAL(m)) {
            for (int k = req; k < SCM_DISPATCHER_MAX_NARGS; k++)
                h = mhash_insert(h, klass, k, m);
        } else {
            h = mhash_insert(h, klass, req, m);
        }
    }
    return h;
}

static mhash *delete_method_from_dispatcher(mhash *h, int axis, ScmMethod *m)
{
    int req = SCM_PROCEDURE_REQUIRED(m);
    if (req >= axis) {
        ScmClass *klass = m->specializers[axis];
        if (SCM_PROCEDURE_OPTIONAL(m)) {
            for (int k = req; k < SCM_DISPATCHER_MAX_NARGS; k++)
                h = mhash_delete(h, klass, k, m);
        } else {
            h = mhash_delete(h, klass, req, m);
        }
    }
    return h;
}

/*
    NB: We run through the method list twice, first process the 
    leaf methods, and then process non-leaf methods.  Non-leaf methods
    cancels the dispatcher entry and forces to go through normal route.
 */
ScmMethodDispatcher *Scm__BuildMethodDispatcher(ScmObj methods, int axis)
{
    mhash *mh = make_mhash(32);
    ScmObj mm;
    for (int i = 0; i < 2; i++) {
        SCM_FOR_EACH(mm, methods) {
            ScmMethod *m = SCM_METHOD(SCM_CAR(mm));
            if ((i == 0 && SCM_METHOD_LEAF_P(m))
                || (i == 1 && !SCM_METHOD_LEAF_P(m))) {
                mh = add_method_to_dispatcher(mh, axis, m);
            }
        }
    }
    ScmMethodDispatcher *dis = SCM_NEW(ScmMethodDispatcher);
    dis->axis = axis;
    dis->methodHash = (AO_t)mh;
    return dis;
}

void Scm__MethodDispatcherAdd(ScmMethodDispatcher *dis, ScmMethod *m)
{
    mhash *h = (mhash*)AO_load(&dis->methodHash);
    mhash *h2 = add_method_to_dispatcher(h, dis->axis, m);
    if (h != h2) AO_store(&dis->methodHash, (AO_t)h2);
}

void Scm__MethodDispatcherDelete(ScmMethodDispatcher *dis, ScmMethod *m)
{
    mhash *h = (mhash*)AO_load(&dis->methodHash);
    mhash *h2 = delete_method_from_dispatcher(h, dis->axis, m);
    if (h != h2) AO_store(&dis->methodHash, (AO_t)h2);
}

ScmObj Scm__MethodDispatcherLookup(ScmMethodDispatcher *dis,
                                   ScmClass **typev, int argc)
{
    if (dis->axis <= argc) {
        ScmClass *selector = typev[dis->axis];
        mhash *h = (mhash*)AO_load(&dis->methodHash);
        return mhash_probe(h, selector, argc);
    } else {
        return SCM_FALSE;
    }
}

void Scm__MethodDispatcherDump(ScmMethodDispatcher *dis, ScmPort *port)
{
    Scm_Printf(port, "MethodDispatcher axis=%d\n", dis->axis);
    mhash_print((mhash*)dis->methodHash, port);
}

