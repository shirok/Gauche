/*
 * proc.c - Procedures
 *
 *  Copyright(C) 2000 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, ditribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: proc.c,v 1.3 2001-01-17 08:22:37 shiro Exp $
 */

#include "gauche.h"

/*
 * Classes
 */

static ScmClass *top_cpl[] = { SCM_CLASS_TOP, NULL };
static ScmClass *proc_cpl[] = { SCM_CLASS_PROCEDURE, SCM_CLASS_TOP, NULL };

static int proc_print(ScmObj obj, ScmPort *port, int mode);
static int closure_print(ScmObj obj, ScmPort *port, int mode);
static int subr_print(ScmObj obj, ScmPort *port, int mode);

ScmClass Scm_ProcedureClass = {
    SCM_CLASS_CLASS,
    "<procedure>",
    proc_print,
    top_cpl
};

ScmClass Scm_ClosureClass = {
    SCM_CLASS_CLASS,
    "<closure>",
    closure_print,
    proc_cpl
};

ScmClass Scm_SubrClass = {
    SCM_CLASS_CLASS,
    "<subr>",
    subr_print,
    proc_cpl
};

static int proc_print(ScmObj obj, ScmPort *port, int mode)
{
    return Scm_Printf(port, "#<procedure %p>", obj);
}

/*
 * Closure
 */

ScmObj Scm_MakeClosure(int required, int optional,
                       ScmObj code, ScmEnvFrame *env, ScmObj info)
{
    ScmClosure *c = SCM_NEW(ScmClosure);
    c->common.hdr.klass = SCM_CLASS_CLOSURE;
    c->common.required = required;
    c->common.optional = optional;
    c->common.info = info;
    c->code = code;
    c->env = env;
    return SCM_OBJ(c);
}

static int closure_print(ScmObj obj, ScmPort *port, int mode)
{
    return Scm_Printf(port, "#<closure %p>", obj);
}

/*
 * Subr
 */

ScmObj Scm_MakeSubr(void (*func)(ScmObj*, int, void*),
                    void *data,
                    int required, int optional,
                    ScmObj info)
{
    ScmSubr *s = SCM_NEW(ScmSubr);
    s->common.hdr.klass = SCM_CLASS_SUBR;
    s->common.required = required;
    s->common.optional = optional;
    s->common.info = info;
    s->func = func;
    s->inliner = NULL;
    s->data = data;
    return SCM_OBJ(s);
}

static int subr_print(ScmObj obj, ScmPort *port, int mode)
{
    ScmSubr *subr = SCM_SUBR(obj);
    int nc = 0;

    SCM_PUTCSTR("#<subr", port); nc += 6;
    
    if (SCM_PROCEDURE_INFO(subr)) {
        nc += Scm_Printf(port, ":%S", SCM_PROCEDURE_INFO(subr));
    }
    if (mode == SCM_PRINT_DEBUG) {
        nc += Scm_Printf(port, " %p", subr);
    }
    SCM_PUTC('>', port); nc++;
    return nc;
}

/*
 * Mapper family
 */

/*
 * One argument version of for-each, map and fold.
 */
static void foreach1_cc(ScmObj result, void **data)
{
    ScmObj args = SCM_OBJ(data[1]);
    if (SCM_PAIRP(args)) {
        ScmObj proc = SCM_OBJ(data[0]);
        void *data[2];
        data[0] = proc;
        data[1] = SCM_CDR(args);
        Scm_VMPushCC(foreach1_cc, data, 2);
        Scm_Apply1(proc, SCM_CAR(args));
    } else {
        SCM_RETURN(SCM_UNDEFINED);
    }
}

void Scm_ForEach1(ScmProcedure *proc, ScmObj args)
{
    if (!SCM_NULLP(args)) {
        void *data[2];
        data[0] = proc;
        data[1] = SCM_CDR(args);
        Scm_VMPushCC(foreach1_cc, data, 2);
        Scm_Apply1(SCM_OBJ(proc), SCM_CAR(args));
    }
}

static void map1_cc(ScmObj result, void **data)
{
    ScmObj args = SCM_OBJ(data[1]);
    ScmObj head = SCM_OBJ(data[2]);
    ScmObj tail = SCM_OBJ(data[3]);

    SCM_APPEND1(head, tail, result);
    
    if (SCM_PAIRP(args)) {
        ScmObj proc  = SCM_OBJ(data[0]);
        void *data[4];
        data[0] = proc;
        data[1] = SCM_CDR(args);
        data[2] = head;
        data[3] = tail;
        Scm_VMPushCC(map1_cc, data, 4);
        Scm_Apply1(proc, SCM_CAR(args));
    } else {
        SCM_RETURN(head);
    }
}

void Scm_Map1(ScmProcedure *proc, ScmObj args)
{
    if (!SCM_NULLP(args)) {
        void *data[4];
        data[0] = proc;
        data[1] = SCM_CDR(args);
        data[2] = SCM_NIL;
        data[3] = SCM_NIL;
        Scm_VMPushCC(map1_cc, data, 4);
        Scm_Apply1(SCM_OBJ(proc), SCM_CAR(args));
    } else {
        SCM_RETURN(SCM_NIL);
    }
}

/*
 * General case
 */

/* gather CAR's and CDR's of given arglist.  returns 1 if at least
   one of the arglist reaches the end. */
static int mapper_collect_args(ScmObj argslist,
                               ScmObj *thisargs, ScmObj *moreargs)
{
    ScmObj arg = SCM_NIL, argtail;
    ScmObj more = SCM_NIL, moretail;
    ScmObj cp;
    
    SCM_FOR_EACH(cp, argslist) {
        ScmObj argsN = SCM_CAR(cp);
        if (!SCM_PAIRP(argsN)) {
            /* ran out the argument. */
            return 1;
        }
        SCM_APPEND1(arg, argtail, SCM_CAR(argsN));
        SCM_APPEND1(more, moretail, SCM_CDR(argsN));
    }
    *thisargs = arg;
    *moreargs = more;
    return 0;
}


static void foreachN_cc(ScmObj result, void **data)
{
    ScmObj proc;
    ScmObj args_list = SCM_OBJ(data[1]), cp;
    ScmObj args, moreargs;
    void *d[2];

    if (mapper_collect_args(args_list, &args, &moreargs)) {
        SCM_RETURN(SCM_UNDEFINED);
        return;
    }
    
    proc = SCM_OBJ(data[0]);
    d[0] = proc;
    d[1] = moreargs;
    Scm_VMPushCC(foreachN_cc, d, 2);
    Scm_Apply(proc, args);
}

void Scm_ForEach(ScmProcedure *proc, ScmObj arg1, ScmObj args)
{
    if (SCM_NULLP(args)) Scm_ForEach1(proc, arg1); /* shortcut */
    else {
        void *data[2];
        data[0] = proc;
        data[1] = Scm_Cons(arg1, args);
        Scm_VMPushCC(foreachN_cc, data, 2);
    }
}

static void mapN_cc(ScmObj result, void **data)
{
    ScmObj proc;
    ScmObj args_list = SCM_OBJ(data[1]), cp;
    ScmObj head = SCM_OBJ(data[2]);
    ScmObj tail = SCM_OBJ(data[3]);
    ScmObj args, moreargs;
    void *d[4];

    SCM_APPEND1(head, tail, result);

    if (mapper_collect_args(args_list, &args, &moreargs)) {
        SCM_RETURN(head);
        return;
    }

    proc = SCM_OBJ(data[0]);
    d[0] = proc;
    d[1] = moreargs;
    d[2] = head;
    d[3] = tail;
    Scm_VMPushCC(mapN_cc, d, 4);
    Scm_Apply(proc, args);
}

void Scm_Map(ScmProcedure *proc, ScmObj arg1, ScmObj args)
{
    if (SCM_NULLP(args)) Scm_Map1(proc, arg1); /* shortcut */
    else {
        ScmObj thisargs, moreargs;
        void *data[4];

        if (mapper_collect_args(Scm_Cons(arg1, args),
                                &thisargs, &moreargs)) {
            /* one of the arglist is already nil. */
            SCM_RETURN(SCM_NIL);
            return;
        }
        
        data[0] = proc;
        data[1] = moreargs;
        data[2] = SCM_NIL;
        data[3] = SCM_NIL;
        Scm_VMPushCC(mapN_cc, data, 4);
        Scm_Apply(SCM_OBJ(proc), thisargs);
    }
}

