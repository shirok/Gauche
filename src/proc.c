/*
 * proc.c - Procedures
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
 *  $Id: proc.c,v 1.13 2001-03-20 07:10:42 shiro Exp $
 */

#include "gauche.h"

/*
 * Classes
 */

static ScmClass *proc_cpl[] = { SCM_CLASS_PROCEDURE, SCM_CLASS_TOP, NULL };

static int proc_print(ScmObj obj, ScmPort *port, int mode);
static int closure_print(ScmObj obj, ScmPort *port, int mode);
static int subr_print(ScmObj obj, ScmPort *port, int mode);

SCM_DEFINE_BUILTIN_CLASS(Scm_ProcedureClass,
                         proc_print, NULL, NULL, NULL,
                         SCM_CLASS_DEFAULT_CPL);
SCM_DEFINE_BUILTIN_CLASS(Scm_ClosureClass,
                         closure_print, NULL, NULL, NULL,
                         proc_cpl);
SCM_DEFINE_BUILTIN_CLASS(Scm_SubrClass,
                         subr_print, NULL, NULL, NULL,
                         proc_cpl);

static int proc_print(ScmObj obj, ScmPort *port, int mode)
{
    return Scm_Printf(port, "#<procedure %p>", obj);
}

/*
 * Closure
 */

/* NOTE: env shouldn't point to the VM stack. If you want to capture
   the current VM env, you should call Scm_VMSaveCurrentEnv() to move
   frames to the heap, then pass its result to Scm_MakeClosure. */
ScmObj Scm_MakeClosure(int required, int optional,
                       ScmObj code, ScmEnvFrame *env, ScmObj info)
{
    ScmClosure *c = SCM_NEW(ScmClosure);
    SCM_SET_CLASS(c, SCM_CLASS_CLOSURE);
    c->common.required = required;
    c->common.optional = optional;
    c->common.generic = FALSE;
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

ScmObj Scm_MakeSubr(ScmObj (*func)(ScmObj*, int, void*),
                    void *data,
                    int required, int optional,
                    ScmObj info)
{
    ScmSubr *s = SCM_NEW(ScmSubr);
    SCM_SET_CLASS(s, SCM_CLASS_SUBR);
    s->common.required = required;
    s->common.optional = optional;
    s->common.generic = FALSE;
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
 * A dummy function which does nothing.   Convenient to pass to other
 * fhunctions which requires a thunk.
 */
static ScmObj theNullProc = SCM_NIL;

static ScmObj null_proc(ScmObj *args, int nargs, void *data)
{
    return SCM_UNDEFINED;
}

ScmObj Scm_NullProc(void)
{
    if (SCM_NULLP(theNullProc)) {
        theNullProc = Scm_MakeSubr(null_proc, NULL, 0, 1,
                                   SCM_MAKE_STR("nullproc"));
    }
    return SCM_OBJ(theNullProc);
}

/*
 * Mapper family
 */

/*
 * One argument version of for-each, map and fold.
 */
static ScmObj foreach1_cc(ScmObj result, void **data)
{
    ScmObj args = SCM_OBJ(data[1]);
    if (SCM_PAIRP(args)) {
        ScmObj proc = SCM_OBJ(data[0]);
        void *data[2];
        data[0] = proc;
        data[1] = SCM_CDR(args);
        Scm_VMPushCC(foreach1_cc, data, 2);
        SCM_RETURN(Scm_VMApply1(proc, SCM_CAR(args)));
    } else {
        SCM_RETURN(SCM_UNDEFINED);
    }
}

ScmObj Scm_ForEach1(ScmProcedure *proc, ScmObj args)
{
    if (!SCM_NULLP(args)) {
        void *data[2];
        data[0] = proc;
        data[1] = SCM_CDR(args);
        Scm_VMPushCC(foreach1_cc, data, 2);
        SCM_RETURN(Scm_VMApply1(SCM_OBJ(proc), SCM_CAR(args)));
    } else {
        SCM_RETURN(SCM_UNDEFINED);
    }
}

static ScmObj map1_cc(ScmObj result, void **data)
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
        SCM_RETURN(Scm_VMApply1(proc, SCM_CAR(args)));
    } else {
        SCM_RETURN(head);
    }
}

ScmObj Scm_Map1(ScmProcedure *proc, ScmObj args)
{
    if (!SCM_NULLP(args)) {
        void *data[4];
        data[0] = proc;
        data[1] = SCM_CDR(args);
        data[2] = SCM_NIL;
        data[3] = SCM_NIL;
        Scm_VMPushCC(map1_cc, data, 4);
        SCM_RETURN(Scm_VMApply1(SCM_OBJ(proc), SCM_CAR(args)));
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


static ScmObj foreachN_cc(ScmObj result, void **data)
{
    ScmObj proc;
    ScmObj args_list = SCM_OBJ(data[1]), cp;
    ScmObj args, moreargs;
    void *d[2];

    if (mapper_collect_args(args_list, &args, &moreargs)) {
        SCM_RETURN(SCM_UNDEFINED);
    }
    
    proc = SCM_OBJ(data[0]);
    d[0] = proc;
    d[1] = moreargs;
    Scm_VMPushCC(foreachN_cc, d, 2);
    SCM_RETURN(Scm_VMApply(proc, args));
}

ScmObj Scm_ForEach(ScmProcedure *proc, ScmObj arg1, ScmObj args)
{
    if (SCM_NULLP(args)) {
        SCM_RETURN(Scm_ForEach1(proc, arg1)); /* shortcut */
    } else {
        void *data[2];
        data[0] = proc;
        data[1] = Scm_Cons(arg1, args);
        SCM_RETURN(foreachN_cc(SCM_UNDEFINED, data));
    }
}

static ScmObj mapN_cc(ScmObj result, void **data)
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
    }

    proc = SCM_OBJ(data[0]);
    d[0] = proc;
    d[1] = moreargs;
    d[2] = head;
    d[3] = tail;
    Scm_VMPushCC(mapN_cc, d, 4);
    SCM_RETURN(Scm_VMApply(proc, args));
}

ScmObj Scm_Map(ScmProcedure *proc, ScmObj arg1, ScmObj args)
{
    if (SCM_NULLP(args)) {
        SCM_RETURN(Scm_Map1(proc, arg1)); /* shortcut */
    } else {
        ScmObj thisargs, moreargs;
        void *data[4];

        if (mapper_collect_args(Scm_Cons(arg1, args),
                                &thisargs, &moreargs)) {
            /* one of the arglist is already nil. */
            SCM_RETURN(SCM_NIL);
        }
        
        data[0] = proc;
        data[1] = moreargs;
        data[2] = SCM_NIL;
        data[3] = SCM_NIL;
        Scm_VMPushCC(mapN_cc, data, 4);
        SCM_RETURN(Scm_VMApply(SCM_OBJ(proc), thisargs));
    }
}

/*
 * Initialization
 */
void Scm__InitProc(void)
{
    Scm_ClosureClass.flags |= SCM_CLASS_APPLICABLE;
    Scm_SubrClass.flags |= SCM_CLASS_APPLICABLE;
}
