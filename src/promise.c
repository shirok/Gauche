/*
 * promise.c - promise object
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: promise.c,v 1.6 2001-02-19 14:48:49 shiro Exp $
 */

#include "gauche.h"

/*
 * class stuff
 */

static int promise_print(ScmObj obj, ScmPort *port, int mode)
{
    ScmPromise *p = (ScmPromise*)obj;
    return Scm_Printf(port, "#<promise %p%s>", p, 
                      p->forced? " (forced)" : "");
}

SCM_DEFCLASS(Scm_PromiseClass, "<promise>", promise_print,
             SCM_CLASS_DEFAULT_CPL);

/*
 * promise object
 */

ScmObj Scm_MakePromise(ScmObj code)
{
    ScmPromise *p = SCM_NEW(ScmPromise);
    SCM_SET_CLASS(p, SCM_CLASS_PROMISE);
    p->forced = 0;
    p->code = code;
    return SCM_OBJ(p);
}

/*
 * force
 */

static ScmObj force_cc(ScmObj result, void **data)
{
    ScmPromise *p = (ScmPromise*)data[0];
    ScmObj r;
    
    if (p->forced) {            /* the promise has been already forced
                                   by the recursive force. */
        r = p->code;
    } else {
        p->forced++;
        p->code = result;
        r = result;
    }
    SCM_RETURN(r);
}

ScmObj Scm_Force(ScmObj obj)
{
    if (!SCM_XTYPEP(obj, SCM_CLASS_PROMISE)) {
        SCM_RETURN(obj);
    } else {
        ScmPromise *p = (ScmPromise*)obj;
        if (p->forced) SCM_RETURN(p->code);
        else {
            Scm_VMPushCC(force_cc, (void**)&p, 1);
            Scm_VMApply0(p->code);
            SCM_RETURN(SCM_UNDEFINED);
        }
    }
}


