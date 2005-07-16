/*
 * promise.c - promise object
 *
 *   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
 *  $Id: promise.c,v 1.13 2005-07-16 01:47:40 shirok Exp $
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
 */

/*
 * The body of promise
 */
typedef struct ScmPromiseContentRec {
    int forced;                 /* TRUE if code has a thunk */
    ScmObj code;
} ScmPromiseContent;

/*
 * class stuff
 */

static void promise_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmPromise *p = (ScmPromise*)obj;
    Scm_Printf(port, "#<promise %p%s>",
               p,
               p->content->forced? " (forced)" : "");
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
    c->forced = forced;
    c->code = code;
    p->content = c;
    return SCM_OBJ(p);
}

/*
 * force
 */

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
    SCM_RETURN(Scm_Force(SCM_OBJ(p)));
}

ScmObj Scm_Force(ScmObj obj)
{
    if (!SCM_PROMISEP(obj)) {
        SCM_RETURN(obj);
    } else {
        ScmPromise *p = (ScmPromise*)obj;
        if (p->content->forced) SCM_RETURN(p->content->code);
        else {
            Scm_VMPushCC(force_cc, (void**)&p, 1);
            SCM_RETURN(Scm_VMApply0(p->content->code));
        }
    }
}


