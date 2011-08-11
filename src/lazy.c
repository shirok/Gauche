/*
 * promise.c - promise object
 *
 *   Copyright (c) 2000-2011  Shiro Kawai  <shiro@acm.org>
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
            void *data[1];
            data[0] = obj;

            if (c->owner == vm) {
                /* we already have the lock and evaluating this promise. */
                c->count++;
                Scm_VMPushCC(force_cc, data, 1);
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
                Scm_VMPushCC(force_cc, data, 1);
                SCM_RETURN(Scm_VMApply0(c->code));
            }
        }
    }
}
