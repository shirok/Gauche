/*
 * lazy.c - lazy evaluation constructs
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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
/* Workaround for sh4 */
/*
 * ABI wise, I'd say SuperH is difficult to support, and libatomic_ops
 * is not well supported.
 *
 * I believe that using gUSA, we could improve libatomic_ops
 * implementation for SH-4 (compare_and_swap, etc.).  But, those
 * are imcompatible to SH-4A, SMP machines.
 *
 * I believe that SH-4A, architecture wise, breaks SH-4 ABI already.
 * 
 * If SH-4A machine insists as if it were SH4 (ABI), we can't use
 * gUSA, nor ll/sc equivalents (movli.l/movco.l, IIRC), either.
 * That's totally a mess.
 *
 * Only workaround for both of SH-4 and SH-4A is to downgrade to
 * pthread implementation, so that it will work reliably.
 * 
 * -- gniibe  2012-11-27
 *
 */
#if defined(__SH4__)
#define AO_USE_PTHREAD_DEFS 1
#endif
/* Workaround for armel */
/*
 * It is unfortunate that libatomic_ops is not well supported
 * for ARM architectures.  It could be understandable as there
 * are so many variants in "ARM".
 *
 * For __ARMEL__ (which means ARM_ARCH_4T, in Debian), there is no 
 * hardware support for atomic operations, unfortunatelly.
 *
 * NOTE:
 * It is ARMv6 which introduced LDREX/STREX (exclusives).
 * It is ARMv7 which introduced DMB/DSB instructions (memory barrier).
 *
 *      -- gniibe  2012-11-27
 */
#if defined(__ARMEL__)
#define AO_USE_PTHREAD_DEFS 1
#endif
#include "atomic_ops.h"

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

static void promise_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
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

static ScmObj release_promise(ScmObj *args, int nargs, void *data)
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
    SCM_RETURN(Scm_Force(SCM_OBJ(p)));
}

ScmObj Scm_Force(ScmObj obj)
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

#if GAUCHE_LAZY_PAIR
/*=================================================================
 * Lazy pairs
 *
 *  Lazy pair is a lazy structure that can turn into a normal pair.
 *  If you check whether the object is pair or not by SCM_PAIRP,
 *  it is 'forced' to become a pair.  The forcing is
 *  identity-preserving; that is, once a lazy pair is forced, the pointer
 *  now becomes a pair.  It is a critical attribute to make the
 *  forcing implicit---we can't do it for general values.
 *  Since the forcing is implicit, majority of the code won't see
 *  ScmLazyPair.
 *
 *  The identity-preserving property requires us to generate
 *  one item ahead from the generator, for we can't replace lazypair
 *  to (), which is an immediate value.
 */

/*

  (0) Initial state

  +---------------+
  |  LazyPair tag |
  +---------------+
  |  ScmObj item  |
  +---------------+
  |    ScmObj     | ----> generator
  +---------------+
  |   (AO_t)0     |
  +---------------+


  (1) The first one (owner) grabs the packet, then evaluates the generator.
  The grabbing is done by CAS to make it atomic.

  +---------------+
  |  LazyPair tag |
  +---------------+
  |  ScmObj item  |
  +---------------+
  |    ScmObj     | ----> generator
  +---------------+
  |  (AO_t)owner  |
  +---------------+


  (2) If generator yields a non-nil value, owner first creates a
      new LazyPair...

  +---------------+
  |  LazyPair tag |
  +---------------+
  |  ScmObj item  |
  +---------------+
  |    ScmObj     | ------------------------+-> generator
  +---------------+                         |
  |  (AO_t)owner  |                         |
  +---------------+                         |
                                            |
                         +---------------+  |
                         |  LazyPair tag |  |
                         +---------------+  |
                         | ScmObj newitem|  |
                         +---------------+  |
                         |    ScmObj     | -/
                         +---------------+
                         |    (AO_t)0    |
                         +---------------+

  (3) then it replaces the cdr pointer with the new LazyPair, and clear
      the third slot.

  +---------------+
  |  LazyPair tag |
  +---------------+
  |     ScmObj    | -\
  +---------------+  |
  |      NIL      |  |
  +---------------+  |
  |  (AO_t)owner  |  |
  +---------------+  |
                     |
                     |   +---------------+
                     \-> |  LazyPair tag |
                         +---------------+
                         | ScmObj newitem|
                         +---------------+
                         |    ScmObj     | ---> generator
                         +---------------+
                         |    (AO_t)0    |
                         +---------------+


  (4) and replaces the car pointer with the lookahead value, which makes
      the original object an (extended) pair,

  +---------------+
  |  ScmObj item  |
  +---------------+
  |     ScmObj    | -\
  +---------------+  |
  |      NIL      |  |
  +---------------+  |
  |  (AO_t)owner  |  |
  +---------------+  |
                     |
                     |   +---------------+
                     \-> |  LazyPair tag |
                         +---------------+
                         | ScmObj newitem|
                         +---------------+
                         |    ScmObj     | ---> generator
                         +---------------+
                         |    (AO_t)0    |
                         +---------------+

  (5) finally set the fourth slot with 1, just not to grab the pointer
      to the owner thread so that the owner thread won't be retained
      unnecessarily.

  +---------------+
  |  ScmObj item  |
  +---------------+
  |     ScmObj    | -\
  +---------------+  |
  |      NIL      |  |
  +---------------+  |
  |    (AO_t)1    |  |
  +---------------+  |
                     |
                     |   +---------------+
                     \-> |  LazyPair tag |
                         +---------------+
                         | ScmObj newitem|
                         +---------------+
                         |    ScmObj     | ---> generator
                         +---------------+
                         |    (AO_t)0    |
                         +---------------+


  (2') If generator yields EOF, we don't create a new lazy pair.
  We first replace the second and third slot by NIL,

  +---------------+
  | LazyPair tag  |
  +---------------+
  |      NIL      |
  +---------------+
  |      NIL      |
  +---------------+
  |  (AO_t)owner  |
  +---------------+

  (3') then replace the car part by the cached value, which turns
  the object to an (extended) pair,

  +---------------+
  |  ScmObj item  |
  +---------------+
  |      NIL      |
  +---------------+
  |      NIL      |
  +---------------+
  |  (AO_t)owner  |
  +---------------+


  (4') then clear the fourth slot to be GC-friendly.

  +---------------+
  |  ScmObj item  |
  +---------------+
  |      NIL      |
  +---------------+
  |      NIL      |
  +---------------+
  |    (AO_t)1    |
  +---------------+


  Each step of the state transitions (0)->(1)->(2)->(3)->(4)->(5) and
  (0)->(1)->(2')->(3')->(4') are atomic, so the observer see either
  one of those states.
 */

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_LazyPairClass, NULL);

/* The order is important - must correspond to ScmExtendedPair. */
struct ScmLazyPairRec {
    SCM_HEADER;
    ScmObj item;
    ScmObj generator;
    AO_t owner;
};

ScmObj Scm_MakeLazyPair(ScmObj item, ScmObj generator)
{
    ScmLazyPair *z = SCM_NEW(ScmLazyPair);
    z->owner = (AO_t)0;
    SCM_SET_CLASS(z, SCM_CLASS_LAZY_PAIR);
    z->generator = generator;
    z->item = item;
    return SCM_OBJ(z);
}

/* Force a lazy pair.
   NB: When an error occurs during forcing, we release the lock of the
   pair, so that the pair can be forced again.  However, the generator
   has already caused some side-effect before the error, so the next
   forcing may not yield a correct next value.  Another plausible option
   is to mark the pair 'unforcible' permanently, by lp->owner == (AO_t)2,
   and let subsequent attempt of forcing the pair fail.
 */
ScmObj Scm_ForceLazyPair(volatile ScmLazyPair *lp)
{
    static const ScmTimeSpec req = {0, 1000000};
    ScmTimeSpec rem;
    ScmVM *vm = Scm_VM();

    do {
        if (AO_compare_and_swap_full(&lp->owner, 0, SCM_WORD(vm))) {
            /* Here we own the lazy pair. */
            ScmObj item = lp->item;
            /* Calling generator might change VM state, so we protect
               incomplete stack frame if there's any. */
            int extra_frame_pushed = Scm__VMProtectStack(vm);
            SCM_UNWIND_PROTECT {
                ScmObj val = Scm_ApplyRec0(lp->generator);
                ScmObj newgen = (vm->numVals == 1)? lp->generator : vm->vals[0];
                vm->numVals = 1; /* make sure the extra val won't leak out */

                if (SCM_EOFP(val)) {
                    lp->item = SCM_NIL;
                    lp->generator = SCM_NIL;
                } else {
                    ScmObj newlp = Scm_MakeLazyPair(val, newgen);
                    lp->item = newlp;
                    lp->generator = SCM_NIL;
                }
                AO_nop_full();
                SCM_SET_CAR(lp, item);
                /* We don't need barrier here. */
                lp->owner = (AO_t)1;
            } SCM_WHEN_ERROR {
                lp->owner = (AO_t)0; /*NB: See above about error handling*/
                SCM_NEXT_HANDLER;
            } SCM_END_PROTECT;
            if (extra_frame_pushed) {
                Scm__VMUnprotectStack(vm);
            }
            return SCM_OBJ(lp); /* lp is now an (extended) pair */
        }
        /* Check if we're already working on forcing this pair.  Unlike
           force/delay, We don't allow recursive forcing of lazy pair.
           Since generators are supposed to be called every time to yield
           a new value, so it is ambiguous what value should be returned
           if a generator calls itself recursively. */
        if (lp->owner == SCM_WORD(vm)) {
            /* NB: lp->owner will be reset by the original caller of
               the generator. */
            Scm_Error("Attempt to recursively force a lazy pair.");
        }
        /* Somebody's already working on forcing.  Let's wait for it
           to finish, or to abort. */
        while (SCM_HTAG(lp) == 7 && lp->owner != 0) {
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
static ScmObj dummy_gen(ScmObj *args, int nargs, void *data)
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
    if (SCM_LAZY_PAIR_P(obj)) {
        volatile ScmLazyPair *lp = SCM_LAZY_PAIR(obj);
        static const ScmTimeSpec req = {0, 1000000};
        ScmTimeSpec rem;
        ScmVM *vm = Scm_VM();

        for (;;) {
            if (AO_compare_and_swap_full(&lp->owner, 0, SCM_WORD(vm))) {
                *item = lp->item;
                *generator = lp->generator;
                AO_nop_full();
                lp->owner = 0;
                return TRUE;
            }
            if (lp->owner == (AO_t)1) {
                /* Somebody else has forced OBJ.  In the typical cases
                   where we call this funtion for co-recursive lazy
                   algorithms, this situation rarely happens.   We fallthrough
                   to the SCM_PAIRP check below to return appropriate
                   values. */
                SCM_ASSERT(SCM_HTAG(lp) != 7);
                break;
            }
            Scm_NanoSleep(&req, &rem);
        }
        /*FALLTHROUGH*/
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

#endif /* GAUCHE_LAZY_PAIR */
