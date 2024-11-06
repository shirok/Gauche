/*
 * lazy.c - lazy evaluation constructs
 *
 *   Copyright (c) 2000-2024  Shiro Kawai  <shiro@acm.org>
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
#include "gauche/priv/configP.h"
#include "gauche/priv/atomicP.h"
#include "gauche/priv/pairP.h"
#include "gauche/priv/parameterP.h"
#include "gauche/priv/promiseP.h"

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
    ScmPromiseState state;
    ScmObj code;                /* thunk or value */
    ScmObj parameterization;    /* parameterization of delay form */
    ScmInternalMutex mutex;
} ScmPromiseContent;

/*
 * class stuff
 */

static void promise_print(ScmObj obj, ScmPort *port,
                          ScmWriteContext *ctx SCM_UNUSED)
{
    ScmPromise *p = (ScmPromise*)obj;
    const char *forced = "";
    switch (p->content->state) {
    case SCM_PROMISE_FORCED:
        forced = " (forced)";
        break;
    case SCM_PROMISE_EXCEPTION:
        forced = " (exception)";
        break;
    case SCM_PROMISE_UNFORCED:
        break;
    }
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

/* Promise can be initialized in one of UNFORCED, FORCED or EXCEPTION state.
 * UNFORCED can change to FORCED or EXCEPTION.  Once it becomes FORCED
 * or EXCEPTION, the state never changes.
 * The value of ScmPromise->content->code depends on the state.
 */

ScmObj Scm_MakePromise(ScmPromiseState state, ScmObj obj)
{
    ScmPromise *p = SCM_NEW(ScmPromise);
    ScmPromiseContent *c = SCM_NEW(ScmPromiseContent);
    SCM_SET_CLASS(p, SCM_CLASS_PROMISE);
    SCM_INTERNAL_MUTEX_INIT(c->mutex);
    c->parameterization = SCM_FALSE;
    c->state = state;

    switch (state) {
    case SCM_PROMISE_UNFORCED:
        c->code = obj;          /* obj is a thunk */
        c->parameterization = Scm_CurrentParameterization();
        break;
    case SCM_PROMISE_FORCED:
        if (!SCM_LISTP(obj)) SCM_TYPE_ERROR(obj, "list");
        c->code = obj;          /* obj is a list of values */
        break;
    case SCM_PROMISE_EXCEPTION:
        c->code = obj;          /* obj is a condition */
        break;
    }
    p->content = c;
    p->kind = SCM_FALSE;
    return SCM_OBJ(p);
}

/*
 * force
 */

static ScmObj force_exc_handler(ScmObj *argv, int argc, void *data)
{
    SCM_ASSERT(argc == 1);
    SCM_ASSERT(SCM_PAIRP(SCM_OBJ(data)));
    ScmPromise *p = (ScmPromise*)SCM_CAR(SCM_OBJ(data));
    ScmObj prev_handler = SCM_CDR(SCM_OBJ(data));

    ScmPromiseContent *c = p->content;

    SCM_INTERNAL_MUTEX_LOCK(c->mutex);
    if (c->state == SCM_PROMISE_UNFORCED) {
        c->code = argv[0];      /* condition object */
        Scm_AtomicThreadFence();
        c->state = SCM_PROMISE_EXCEPTION;
    }
    SCM_INTERNAL_MUTEX_UNLOCK(c->mutex);

    Scm_VMPushExceptionHandler(prev_handler);
    SCM_RETURN(Scm_Raise(c->code, SCM_RAISE_NON_CONTINUABLE));
}

static ScmObj force_cc(ScmObj result, void **data)
{
    ScmPromise *p = (ScmPromise*)data[0];
    ScmPromiseContent *c = p->content;
    ScmObj handlers = (ScmObj)data[1];

    SCM_INTERNAL_MUTEX_LOCK(c->mutex);
    if (c->state == SCM_PROMISE_UNFORCED) {
        /* It is critical to set code first, then state.  The reader will
           check state first; if it's eitehr FORCED or EXCEPTION, it's sure
           that c->code holds results/exception.
         */
        if (SCM_PROMISEP(result)) {
            /* Deal with a recursive promise introduced by lazy operation.
               See srfi-45 for the details. */
            ScmPromiseContent *cc = SCM_PROMISE(result)->content;
            c->code  = cc->code;
            Scm_AtomicThreadFence();
            c->state = cc->state;
            SCM_PROMISE(result)->content = c;
        } else {
            /* This isn't supposed to happen if 'lazy' is used properly
               on the promise-yielding procedure, but we can't prevent
               one from writing (lazy 3).  So play safe. */
            SCM_ASSERT(SCM_LISTP(result));
            c->code = result;
            Scm_AtomicThreadFence();
            c->state = SCM_PROMISE_FORCED;
        }
    }
    SCM_INTERNAL_MUTEX_UNLOCK(c->mutex);
    Scm_VMSetDynamicHandlers(handlers);
    /* We recursively call force to deliver the result. */
    SCM_RETURN(Scm_VMForce(SCM_OBJ(p)));
}

ScmObj Scm_VMForce(ScmObj obj)
{
    if (!SCM_PROMISEP(obj)) SCM_RETURN(obj);

    ScmPromiseContent *c = SCM_PROMISE(obj)->content;
    ScmVM *vm = Scm_VM();

 retry:
    switch (c->state) {
    case SCM_PROMISE_FORCED:
        /* already forced. c->code has values. */
        return Scm_Values(c->code);
    case SCM_PROMISE_EXCEPTION:
        /* evaluation of promise ended up exception. */
        return Scm_VMThrowException(vm, c->code, 0);
    case SCM_PROMISE_UNFORCED:
        {
            /* State and code may be changed by another thread, so we
               take the snapshot. */
            ScmPromiseState state;
            ScmObj code;
            SCM_INTERNAL_MUTEX_LOCK(c->mutex);
            state = c->state;
            code = c->code;
            SCM_INTERNAL_MUTEX_UNLOCK(c->mutex);
            if (state != SCM_PROMISE_UNFORCED) {
                /* another thread already forced. */
                goto retry;
            }
            /* From now on, it is ok that another thread changes the promise's
               state.  We go on to evaluate thunk anyway, and we'll check
               the promise's state at the moment we try to set the result.
             */
            void *data[2];
            data[0] = obj;
            data[1] = Scm_VMGetDynamicHandlers();
            Scm_VMPushCC(force_cc, data, 2);

            if (SCM_PARAMETERIZATIONP(c->parameterization)) {
                Scm_InstallParameterization(SCM_PARAMETERIZATION(c->parameterization));
            }
            ScmObj exc_handler =
                Scm_MakeSubr(force_exc_handler,
                             Scm_Cons(obj, Scm_VMCurrentExceptionHandler()),
                             1, 0,
                             SCM_INTERN("force-exc-handler"));
            Scm_VMPushExceptionHandler(exc_handler);
            SCM_RETURN(Scm_VMApply0(code));
        }
    }
    return SCM_UNDEFINED;       /* dummy */
}

/* TODO: The caller assumes only one result, but we extended force to
 * return multiple values.  Need the way to update API.
 */

ScmObj Scm_Force(ScmObj obj)
{
    if (!SCM_PROMISEP(obj)) {
        SCM_RETURN(obj);
    } else {
        ScmPromiseContent *c = SCM_PROMISE(obj)->content;
        if (c->state == SCM_PROMISE_FORCED) {
            SCM_ASSERT(SCM_PAIRP(c->code));
            return SCM_CAR(c->code);
        }

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

  [Extended protocol]

  Experimentally, the generator can return more than just the next item.

  - It can return the second value as pair attribute alist, to be attached
    to the pair containing the generated item.

  - It can return a (lazy) pair or (), instead of just an item.  This is
    useful to implement lcons.  We need a special marker to distinguish
    such generator from the ordinary one that returns a pair as an element.
    So such lazy pair should be constructed with Scm_LazyCons, instead of
    Scm_MakeLazyPair.
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

/* Clean up pair attribute list given from outside.
   (1) We need to copy it so that future mutation won't interfere
   (2) If the given obj contains bad items, we simply ignore it,
       for raising error lazily isn't useful.
*/
static ScmObj copy_attrs(ScmObj attrs)
{
    ScmObj h = SCM_NIL, t = SCM_NIL, cp;
    SCM_FOR_EACH(cp, attrs) {
        if (SCM_PAIRP(SCM_CAR(cp))) {
            SCM_APPEND1(h, t, Scm_Cons(SCM_CAAR(cp), SCM_CDAR(cp)));
        }
    }
    return h;
}

static ScmObj make_lazy_pair(ScmObj item, ScmObj generator, ScmObj attrs)
{
    ScmRealLazyPair *z = SCM_NEW(ScmRealLazyPair);
    z->owner = (ScmAtomicWord)0;
    ScmLazyPair *r = &z->data;
    SCM_SET_CLASS(r, SCM_CLASS_LAZY_PAIR);
    r->generator = generator;
    if (!SCM_NULLP(attrs) || SCM_MVBOXP(item)) {
        ScmMVBox *b = Scm_MakeMVBox(2, SCM_UNDEFINED);
        SCM_MVBOX_SET(b, 0, item);
        SCM_MVBOX_SET(b, 1, copy_attrs(attrs));
        r->item = SCM_OBJ(b);
    } else {
        r->item = item;
    }
    return SCM_OBJ(r);
}

ScmObj Scm_MakeLazyPair(ScmObj item, ScmObj generator, ScmObj attrs)
{
    /* A check to eliminate the caller accidentally create a 'marked' lazy
       pair.  Using cons as a generator to mark it is just a provisional
       solution, so do not count on it. */
    if (SCM_PAIRP(generator)) {
        Scm_Error("generator must be a procedure, but got: %S", generator);
    }
    return make_lazy_pair(item, generator, attrs);
}

ScmObj Scm_LazyCons(ScmObj item, ScmObj thunk, ScmObj attrs)
{
    /* A 'marked' lazy pair, where thunk returns the next pair instead of
       next item.   Provisionally, we insert extra indirection in generator
       slot to mark it.  It may change in future, so external code shouldn't
       count on it. */
    return make_lazy_pair(item, SCM_LIST1(thunk), attrs);
}

/* This can return () */
static ScmObj generator_to_lazy_pair(ScmObj generator)
{
    ScmVM *vm = Scm_VM();
    int lazy_cons = FALSE;

    if (SCM_PAIRP(generator)) {
        /* 'marked' */
        generator = SCM_CAR(generator);
        lazy_cons = TRUE;
    }

    /* This may be called with incomplete VM stack, so we protect it. */
    int extra_frame_pushed = Scm__VMProtectStack(vm);

    ScmObj val = Scm_ApplyRec0(generator);
    ScmObj r = SCM_NIL;

    if (!lazy_cons) {
        /* standard lazy pair.  val is the next item. */
        if (SCM_EOFP(val)) {
            r = SCM_NIL;
        } else {
            ScmObj attrs = ((vm->numVals > 1)
                            ? vm->vals[0] /* second value */
                            : SCM_NIL);
            r = Scm_MakeLazyPair(val, generator, attrs);
        }
    } else {
        /* Generator returns lazy pair diretly.
           Check LAZY_PAIR_P first to avoid forcing it */
        if (SCM_LAZY_PAIR_P(val) || SCM_LISTP(val)) {
            r = val;
        }
        /* If lcons's second argument doesn't yield a () or a pair,
           we just treat it as if () is returned.  Since it is delayed
           evaluation, raising error won't help. */
    }
    vm->numVals = 1; /* make sure the extra vals won't leak out */
    if (extra_frame_pushed) Scm__VMUnprotectStack(vm);
    return r;
}

ScmObj Scm_GeneratorToLazyPair(ScmObj generator)
{
    /* A check to eliminate the caller accidentally create a 'marked' lazy
       pair.  Using cons as a generator to mark it is just a provisional
       solution, so do not count on it. */
    if (SCM_PAIRP(generator)) {
        Scm_Error("generator must be a procedure, but got: %S", generator);
    }
    return generator_to_lazy_pair(generator);
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
    volatile ScmRealLazyPair * volatile lp = REAL_LAZY_PAIR(obj);
    static const ScmTimeSpec req = {0, 1000000};
    ScmTimeSpec rem;
    ScmVM *vm = Scm_VM();

    do {
        ScmAtomicWord zero = 0;
        if (Scm_AtomicCompareExchange(&lp->owner, &zero, SCM_WORD(vm))) {
            /* Here we own the lazy pair. */
            volatile ScmObj item = lp->data.item;
            volatile ScmObj attrs = SCM_NIL;
            if (SCM_MVBOXP(item)) {
                attrs = SCM_MVBOX_VALUES(item)[1];
                item = SCM_MVBOX_VALUES(item)[0];
            }

            SCM_UNWIND_PROTECT {
                lp->data.item = generator_to_lazy_pair(lp->data.generator);
                lp->data.generator = attrs;
                lp->owner = XPAIR_DESC();
                Scm_AtomicThreadFence();
                SCM_SET_CAR_UNCHECKED(obj, item); /* Overwrite LazyPair tag */
            } SCM_WHEN_ERROR {
                lp->owner = (ScmAtomicWord)0; /*NB: See above about error handling*/
                SCM_NEXT_HANDLER;
            } SCM_END_PROTECT;
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

int Scm_PairP(ScmObj x)
{
    if (SCM_LAZY_PAIR_P(x)) {
        Scm_ForceLazyPair(SCM_LAZY_PAIR(x));
        return TRUE;
    } else {
        return FALSE;
    }
}
