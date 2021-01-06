/*
 * lazy.c - lazy evaluation constructs
 *
 *   Copyright (c) 2000-2020  Shiro Kawai  <shiro@acm.org>
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
#include "gauche/priv/pairP.h"

/*==================================================================
 * Promise
 */

/* NB: We adopted the semantics described in srfi-45.
 *     http://srfi.schemers.org/srfi-45/srfi-45.html
 *
 * The 'forced' flag indicates one of two state of a promise.
 *
 *  forced == TRUE:  the promise is in 'eager' state.  code has a value.
 *  forced == FALSE: the promise is in 'lazy' state.  code has a thunk.
 *
 * [syntax]     lazy expr   : Promise a -> Promise a
 *    Creates a lazy promise, delaying evaluation of expr.
 * [procedure]  eager expr  : a -> Promise a
 *    Creates a eager promise, encapsulating the result of evaluation of expr.
 * [syntax]     delay expr  : a -> Promise a
 *    (lazy (eager expr))
 * [procedure]  force expr  : Promise a -> a
 *
 * One might want to create a subtype of promise; for example, srfi-40
 * requires the stream type to be distinct from other types, although
 * it is essentially a promise with a specific usage pattern.  To realize
 * that portably, one need effectively reimplement force/delay mechanism
 * (since 'eager' operation is required to return Stream instread of Promise),
 * which is kind of shame.
 *
 * Gauche experimentally tries to address this problem by allowing the
 * program to add a specific KIND object to a promise instance.
 *
 * Thread safety: It is safe that more than one thread force a promise
 * simultaneously.  Only one thread does calculation.
 */

/*
 * The body of promise
 */
typedef struct ScmPromiseContentRec {
    int forced;                 /* TRUE if code has a thunk */
    ScmObj code;                /* thunk or value */
    ScmInternalMutex mutex;
    ScmVM *owner;               /* who is working on this? */
    int count;                  /* count for recursive lock */
} ScmPromiseContent;

/*
 * class stuff
 */

static void promise_print(ScmObj obj, ScmPort *port,
                          ScmWriteContext *ctx SCM_UNUSED)
{
    ScmPromise *p = (ScmPromise*)obj;
    const char *forced = p->content->forced? " (forced)" : "";
    if (SCM_FALSEP(p->kind)) {
        Scm_Printf(port, "#<promise %p%s>", p, forced);
    } else {
        Scm_Printf(port, "#<promise(%S) %p%s>", p->kind, p, forced);
    }
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_PromiseClass, promise_print);

/*
 * promise object
 */

ScmObj Scm_MakePromise(int forced, ScmObj code)
{
    ScmPromise *p = SCM_NEW(ScmPromise);
    ScmPromiseContent *c = SCM_NEW(ScmPromiseContent);
    SCM_SET_CLASS(p, SCM_CLASS_PROMISE);
    SCM_INTERNAL_MUTEX_INIT(c->mutex);
    c->owner = NULL;
    c->count = 0;
    c->forced = forced;
    c->code = code;
    p->content = c;
    p->kind = SCM_FALSE;
    return SCM_OBJ(p);
}

/*
 * force
 */

static ScmObj release_promise(ScmObj *args SCM_UNUSED,
                              int nargs SCM_UNUSED,
                              void *data)
{
    ScmPromise *p = SCM_PROMISE(data);
    p->content->owner = NULL;
    SCM_INTERNAL_MUTEX_UNLOCK(p->content->mutex);
    return SCM_UNDEFINED;
}

static void install_release_thunk(ScmVM *vm, ScmObj promise)
{
    /* TODO: the before thunk must be something that
       prevents restarting the execution process. */
    vm->handlers = Scm_Acons(Scm_NullProc(),
                             Scm_MakeSubr(release_promise,
                                          (void*)promise, 0, 0,
                                          SCM_MAKE_STR("promise_release")),
                             vm->handlers);
}

static ScmObj force_cc(ScmObj result, void **data)
{
    ScmPromise *p = (ScmPromise*)data[0];
    ScmObj handlers = (ScmObj)data[1];

    /* Check if the original promise is forced by evaluating
       the delayed expr to detect recursive force situation */
    if (!p->content->forced) {
        if (SCM_PROMISEP(result)) {
            /* Deal with a recursive promise introduced by lazy operation.
               See srfi-45 for the details. */
            p->content->forced = SCM_PROMISE(result)->content->forced;
            p->content->code   = SCM_PROMISE(result)->content->code;
            SCM_PROMISE(result)->content = p->content;
        } else {
            /* This isn't supposed to happen if 'lazy' is used properly
               on the promise-yielding procedure, but we can't prevent
               one from writing (lazy 3).  So play safe. */
            p->content->forced = TRUE;
            p->content->code = result;
        }
    }
    if (--p->content->count == 0) {
        p->content->owner = NULL;
        SCM_INTERNAL_MUTEX_UNLOCK(p->content->mutex);
    }
    Scm_VM()->handlers = handlers;
    SCM_RETURN(Scm_VMForce(SCM_OBJ(p)));
}

ScmObj Scm_VMForce(ScmObj obj)
{
    if (!SCM_PROMISEP(obj)) {
        SCM_RETURN(obj);
    } else {
        ScmPromiseContent *c = SCM_PROMISE(obj)->content;

        if (c->forced) SCM_RETURN(c->code);
        else {
            ScmVM *vm = Scm_VM();
            void *data[2];
            data[0] = obj;
            data[1] = vm->handlers;

            if (c->owner == vm) {
                /* we already have the lock and evaluating this promise. */
                c->count++;
                Scm_VMPushCC(force_cc, data, 2);
                SCM_RETURN(Scm_VMApply0(c->code));
            } else {
                /* TODO: check if the executing thread terminates
                   prematurely */
                SCM_INTERNAL_MUTEX_LOCK(c->mutex);
                if (c->forced) {
                    SCM_INTERNAL_MUTEX_UNLOCK(c->mutex);
                    SCM_RETURN(c->code);
                }
                SCM_ASSERT(c->owner == NULL);
                c->owner = vm;
                install_release_thunk(vm, obj);
                c->count++;
                /* mutex is unlocked by force_cc. */
                Scm_VMPushCC(force_cc, data, 2);
                SCM_RETURN(Scm_VMApply0(c->code));
            }
        }
    }
}

ScmObj Scm_Force(ScmObj obj)
{
    if (!SCM_PROMISEP(obj)) {
        SCM_RETURN(obj);
    } else {
        ScmPromiseContent *c = SCM_PROMISE(obj)->content;
        if (c->forced) SCM_RETURN(c->code);

        static ScmObj force = SCM_UNDEFINED;
        SCM_BIND_PROC(force, "force", Scm_SchemeModule());
        return Scm_ApplyRec1(force, obj);
    }
}

/*=================================================================
 * Lazy pairs
 *
 *  Lazy pair is a lazy structure that can turn into a normal pair.
 *  If you check whether the object is pair or not by SCM_PAIRP,
 *  it is 'forced' to become a pair.  The forcing is
 *  identity-preserving; that is, once a lazy pair is forced, the pointer
 *  now becomes a pair (actually, an extended pair).  It is a critical 
 *  attribute to make the forcing implicit---we can't do it for general
 *  values.   Since the forcing is implicit, majority of the code won't see
 *  ScmLazyPair.
 *
 *  The identity-preserving property requires us to generate
 *  one item ahead from the generator, for we can't replace lazypair
 *  to (), which is an immediate value.
 */

/*

  (0) Initial state
    A lazypair is allocated on 4-word boundary, but we return a pointer to
    the second word.  That makes the object look like odd-word boundary 
    aligned, a marker of Extended Pair (see priv/pairP.h).   The 'hidden'
    first word will be used for synchronization.
    NB: An extended pair must have preceding hidden word with 111 in the lower
    bits for extra check (when !SCM_PAIR_ALWAYS_ALIGNED_EVEN_WORDS).  That
    check is done after SCM_PAIRP, however, so it won't see (AO_t)0.

            +---------------+
            |   (AO_t)0     |
   ScmObj > +---------------+
            |  LazyPair tag |
            +---------------+
            |  ScmObj item  |
            +---------------+
            |    ScmObj     | ----> generator
            +---------------+

  (1) The first thread (owner) grabs the packet, then evaluates the generator.
  The grabbing is done by CAS to make it atomic.

            +---------------+
            |  (AO_t)owner  |
   ScmObj > +---------------+
            |  LazyPair tag |
            +---------------+
            |  ScmObj item  |
            +---------------+
            |    ScmObj     | ----> generator
            +---------------+


  (2) If generator yields a non-EOF value, owner first creates a
      new LazyPair...

            +---------------+
            |  (AO_t)owner  |
   ScmObj > +---------------+
            |  LazyPair tag |
            +---------------+
            |  ScmObj item  |
            +---------------+
            |    ScmObj     | --------------+-> generator
            +---------------+               |
                                            |
                                            |
                                            |
                         +---------------+  |
                         |   (AO_t)0     |  |
                         +---------------+  |
                         |  LazyPair tag |  |
                         +---------------+  |
                         | ScmObj newitem|  |
                         +---------------+  |
                         |    ScmObj     | -/
                         +---------------+

  (3) then it replaces the cdr pointer with the new LazyPair, and clear
      the last slot.

            +---------------+
            |  (AO_t)owner  |
   ScmObj > +---------------+
            |  LazyPair tag |
            +---------------+
            |     ScmObj    | -\
            +---------------+  |
            |      NIL      |  |
            +---------------+  |
                               |
                               |   +---------------+
                               |   |    (AO_t)0    |
                               |   +---------------+
                               \-> |  LazyPair tag |
                                   +---------------+
                                   | ScmObj newitem|
                                   +---------------+
                                   |    ScmObj     | ---> generator
                                   +---------------+

  (4) replaces the hidden word with the extended pair descriptor.
      At this moment, it still look like a lazy pair and someone working on
      it, from the thread other than the owner.

            +---------------+
            |    desc + 7   | ---> ScmExtendedPairDescriptor
   ScmObj > +---------------+
            |  LazyPair tag |
            +---------------+
            |     ScmObj    | -\
            +---------------+  |
            |      NIL      |  |
            +---------------+  |
                               |
                               |   +---------------+
                               |   |    (AO_t)0    |
                               |   +---------------+
                               \-> |  LazyPair tag |
                                   +---------------+
                                   | ScmObj newitem|
                                   +---------------+
                                   |    ScmObj     | ---> generator
                                   +---------------+

  (5) finally we set the car field with the precomputed item value.
      Now it looks like just an extended pair.

            +---------------+
            |    desc + 7   | ---> ScmExtendedPairDescriptor
   ScmObj > +---------------+
            |  ScmObj item  |
            +---------------+
            |     ScmObj    | -\
            +---------------+  |
            |      NIL      |  |
            +---------------+  |
                               |
                               |   +---------------+
                               |   |    (AO_t)0    |
                               |   +---------------+
                               \-> |  LazyPair tag |
                                   +---------------+
                                   | ScmObj newitem|
                                   +---------------+
                                   |    ScmObj     | ---> generator
                                   +---------------+


  (2') If generator yields EOF, we don't create a new lazy pair.
  We first replace the second and third slot by NIL,

            +---------------+
            |  (AO_t)owner  |
   ScmObj > +---------------+
            | LazyPair tag  |
            +---------------+
            |      NIL      |
            +---------------+
            |      NIL      |
            +---------------+

  (3') then replace the car part by the cached value, which turns
  the object to an (extended) pair,

            +---------------+
            |   desc + 7    | ---> ScmExtendedPairDescriptor
   ScmObj > +---------------+
            | LazyPair tag  |
            +---------------+
            |      NIL      |
            +---------------+
            |      NIL      |
            +---------------+


  (4') then set the car field with the cached value, which turns
  the object to an (extended) pair, with its cdr being NIL.

            +---------------+
            |   desc + 7    | ---> ScmExtendedPairDescriptor
   ScmObj > +---------------+
            |     item      |
            +---------------+
            |      NIL      |
            +---------------+
            |      NIL      |
            +---------------+

  Each step of the state transitions (0)->(1)->(2)->(3)->(4)->(5) and
  (0)->(1)->(2')->(3')->(4') are atomic, so the observer see either
  one of those states.

  The generator can actually return multiple values, to tailor the
  resulting list.

    primary value   - the next element
    secondary value - if returned and not #f, this becomes the next generator

  This is useful to implement lcons, but we keep it a 'hidden' feature
  for now, to see if it is general enough.
 */


SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_LazyPairClass, NULL);

/* This is what everyone see */
struct ScmLazyPairRec {
    SCM_HEADER;
    ScmObj item;
    ScmObj generator;
};

/* And this is the real thing */
typedef struct ScmRealLazyPairRec {
    ScmAtomicVar owner;
    ScmLazyPair data;
} ScmRealLazyPair;

ScmObj Scm_MakeLazyPair(ScmObj item, ScmObj generator)
{
    ScmRealLazyPair *z = SCM_NEW(ScmRealLazyPair);
    z->owner = (ScmAtomicWord)0;
    ScmLazyPair *r = &z->data;
    SCM_SET_CLASS(r, SCM_CLASS_LAZY_PAIR);
    r->generator = generator;
    r->item = item;
    return SCM_OBJ(r);
}

#define REAL_LAZY_PAIR(obj) \
    (ScmRealLazyPair*)((char*)(obj) - offsetof(ScmRealLazyPair, data))
#define XPAIR_DESC() \
    (ScmAtomicWord)SCM_CLASS2TAG(Scm__GetDefaultExtendedPairDesctiptor())

/* Force a lazy pair.
   NB: When an error occurs during forcing, we release the lock of the
   pair, so that the pair can be forced again.  However, the generator
   has already caused some side-effect before the error, so the next
   forcing may not yield a correct next value.  Another plausible option
   is to mark the pair 'unforcible' permanently, by lp->owner == (AO_t)2,
   and let subsequent attempt of forcing the pair fail.
 */
ScmObj Scm_ForceLazyPair(volatile ScmLazyPair *obj)
{
    volatile ScmRealLazyPair *lp = REAL_LAZY_PAIR(obj);
    static const ScmTimeSpec req = {0, 1000000};
    ScmTimeSpec rem;
    ScmVM *vm = Scm_VM();
    ScmAtomicWord zero = 0;	/* Need to use C11 intrinsic */

    do {
        if (AO_compare_and_swap_full(&lp->owner, zero, SCM_WORD(vm))) {
            /* Here we own the lazy pair. */
            volatile ScmObj item = lp->data.item;
            /* Calling generator might change VM state, so we protect
               incomplete stack frame if there's any. */
            int extra_frame_pushed = Scm__VMProtectStack(vm);
            SCM_UNWIND_PROTECT {
                ScmObj val = Scm_ApplyRec0(lp->data.generator);
                ScmObj newgen = ((vm->numVals >= 2 && !SCM_FALSEP(vm->vals[0]))
                                 ? vm->vals[0]
                                 : lp->data.generator);
                vm->numVals = 1; /* make sure the extra val won't leak out */

                if (SCM_EOFP(val)) {
                    lp->data.item = SCM_NIL;
                    lp->data.generator = SCM_NIL;
                } else {
                    ScmObj newlp = Scm_MakeLazyPair(val, newgen);
                    lp->data.item = newlp;
                    lp->data.generator = SCM_NIL;
                }
                AO_nop_full();
                lp->owner = XPAIR_DESC();
                AO_nop_full();
                SCM_SET_CAR_UNCHECKED(obj, item); /* Overwrite LazyPair tag */
            } SCM_WHEN_ERROR {
                lp->owner = (ScmAtomicWord)0; /*NB: See above about error handling*/
                SCM_NEXT_HANDLER;
            } SCM_END_PROTECT;
            if (extra_frame_pushed) {
                Scm__VMUnprotectStack(vm);
            }
            return SCM_OBJ(&lp->data); /* lp is now an (extended) pair */
        }
        /* Check if we're already working on forcing this pair.  Unlike
           force/delay, We don't allow recursive forcing of lazy pair.
           Since generators are supposed to be called every time to yield
           a new value, so it is ambiguous what value should be returned
           if a generator calls itself recursively. */
        if (SCM_WORD(lp->owner) == SCM_WORD(vm)) {
            /* NB: lp->owner will be reset by the original caller of
               the generator. */
            Scm_Error("Attempt to recursively force a lazy pair.");
        }
        /* Somebody's already working on forcing.  Let's wait for it
           to finish, or to abort. */
        while (SCM_HTAG(obj) == 7 && lp->owner != 0) {
            Scm_NanoSleep(&req, &rem);
        }
    } while (lp->owner == 0); /* we retry if the previous owner abandoned. */
    return SCM_OBJ(lp);
}

/* Extract item and generator from lazy pair OBJ, without forcing it.
   If OBJ is a lazy pair, item and generator is filled and TRUE is returned.
   If OBJ is an ordinary pair (including the case that it was a lazy pair
   but forced during execution of Scm_DecomposeLazyPair), returns its CAR
   and a generator that returns its CDR.
   Otherwise, returns FALSE.  */
static ScmObj dummy_gen(ScmObj *args SCM_UNUSED,
                        int nargs SCM_UNUSED,
                        void *data)
{
    ScmObj item;
    ScmObj generator;
    if (Scm_DecomposeLazyPair(SCM_OBJ(data), &item, &generator)) {
        return Scm_Values2(item, generator);
    } else {
        return Scm_Values2(SCM_EOF, SCM_FALSE);
    }
}

int Scm_DecomposeLazyPair(ScmObj obj, ScmObj *item, ScmObj *generator)
{
 retry:
    if (SCM_LAZY_PAIR_P(obj)) {
        volatile ScmRealLazyPair *lp = REAL_LAZY_PAIR(obj);
        static const ScmTimeSpec req = {0, 1000000};
        ScmTimeSpec rem;
        ScmVM *vm = Scm_VM();
        ScmAtomicWord zero = 0;	/* Need to use C11 intrinsic */

        if (AO_compare_and_swap_full(&lp->owner, zero, SCM_WORD(vm))) {
            *item = lp->data.item;
            *generator = lp->data.generator;
            AO_nop_full();
            lp->owner = XPAIR_DESC();
            return TRUE;
        }

        /* Somebody else is working on OBJ.  Retry. */
        Scm_NanoSleep(&req, &rem);
        goto retry;
    }
    if (SCM_PAIRP(obj)) {
        ScmObj next;
        *item = SCM_CAR(obj);
        next = SCM_NULLP(SCM_CDR(obj)) ? SCM_EOF : SCM_CDR(obj);
        *generator = Scm_MakeSubr(dummy_gen, (void*)next, 0, 0, SCM_FALSE);
        return TRUE;
    } else {
        return FALSE;
    }
}

int Scm_PairP(ScmObj x)
{
    if (SCM_LAZY_PAIR_P(x)) {
        Scm_ForceLazyPair(SCM_LAZY_PAIR(x));
        return TRUE;
    } else {
        return FALSE;
    }
}

