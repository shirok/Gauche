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
 *  $Id: proc.c,v 1.31 2002-09-11 03:19:40 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"

/*=================================================================
 * Classes
 */

static void proc_print(ScmObj obj, ScmPort *port, ScmWriteContext *);

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_ProcedureClass, proc_print);

static void proc_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    if (SCM_PROCEDURE_TYPE(obj) == SCM_PROC_SUBR) {
        ScmSubr *subr = SCM_SUBR(obj);
        SCM_PUTZ("#<subr", -1, port);
        if (SCM_PROCEDURE_INFO(subr)) {
            Scm_Printf(port, " %A", SCM_PROCEDURE_INFO(subr));
        }
        if (ctx->mode == SCM_WRITE_DEBUG) {
            Scm_Printf(port, " %p", subr);
        }
        SCM_PUTC('>', port);
    } else {
        Scm_Printf(port, "#<closure %p%S>", obj, SCM_PROCEDURE_INFO(obj));
    }
}

/*=================================================================
 * Closure
 */

ScmObj Scm_MakeClosure(int required, int optional,
                       ScmObj code, ScmObj info)
{
    ScmClosure *c = SCM_NEW(ScmClosure);
    
    SCM_SET_CLASS(c, SCM_CLASS_PROCEDURE);
    SCM_PROCEDURE_INIT(c, required, optional, SCM_PROC_CLOSURE, info);
    c->code = code;
    c->env = Scm_GetCurrentEnv();
    return SCM_OBJ(c);
}

/*=================================================================
 * Subr
 */

ScmObj Scm_MakeSubr(ScmObj (*func)(ScmObj*, int, void*),
                    void *data,
                    int required, int optional,
                    ScmObj info)
{
    ScmSubr *s = SCM_NEW(ScmSubr);
    SCM_SET_CLASS(s, SCM_CLASS_PROCEDURE);
    SCM_PROCEDURE_INIT(s, required, optional, SCM_PROC_SUBR, info);
    s->func = func;
    s->inliner = NULL;
    s->data = data;
    return SCM_OBJ(s);
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

/*=================================================================
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

ScmObj Scm_ForEach1(ScmObj proc, ScmObj args)
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

ScmObj Scm_Map1(ScmObj proc, ScmObj args)
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
    ScmObj arg = SCM_NIL, argtail = SCM_NIL;
    ScmObj more = SCM_NIL, moretail = SCM_NIL;
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
    ScmObj args_list = SCM_OBJ(data[1]);
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

ScmObj Scm_ForEach(ScmObj proc, ScmObj arg1, ScmObj args)
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
    ScmObj args_list = SCM_OBJ(data[1]);
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

ScmObj Scm_Map(ScmObj proc, ScmObj arg1, ScmObj args)
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

/*=================================================================
 * Generic setter
 */

ScmObj Scm_SetterSet(ScmProcedure *proc, ScmProcedure *setter, int lock)
{
    if (proc->locked) {
        Scm_Error("can't change the locked setter of procedure %S", proc);
    }
#if 0 /* this doesn't work weel for generic functions */
    /* check setter can get at least one more argument than proc */
    if (!SCM_PROCEDURE_OPTIONAL(setter)
        && (SCM_PROCEDURE_REQUIRED(proc) >= SCM_PROCEDURE_REQUIRED(setter))) {
        Scm_Error("procedure %S takes too few arguments to be a setter of procedure %S",
                  setter, proc);
    }
#endif
    proc->setter = SCM_OBJ(setter);
    proc->locked = lock;
    return SCM_OBJ(proc);
}

ScmObj Scm_Setter(ScmProcedure *proc)
{
    if (SCM_FALSEP(proc->setter)) {
        Scm_Error("no setter defined for procedure %S", proc);
    }
    return SCM_OBJ(proc->setter);
}

/*=================================================================
 * Scheme-level accessors
 */
static ScmObj proc_required(ScmProcedure *p)
{
    return SCM_MAKE_INT(p->required);
}

static ScmObj proc_optional(ScmProcedure *p)
{
    return SCM_MAKE_BOOL(p->optional);
}

static ScmObj proc_locked(ScmProcedure *p)
{
    return SCM_MAKE_BOOL(p->locked);
}

static ScmObj proc_info(ScmProcedure *p)
{
    return p->info;
}

static ScmObj proc_setter(ScmProcedure *p)
{
    return p->setter;
}

static ScmClassStaticSlotSpec proc_slots[] = {
    SCM_CLASS_SLOT_SPEC("required", proc_required, NULL),
    SCM_CLASS_SLOT_SPEC("optional", proc_optional, NULL),
    SCM_CLASS_SLOT_SPEC("locked", proc_locked, NULL),
    SCM_CLASS_SLOT_SPEC("info", proc_info, NULL),
    SCM_CLASS_SLOT_SPEC("setter", proc_setter, NULL),
    {NULL},
};


/*=================================================================
 * Initialization
 */
void Scm__InitProc(void)
{
    Scm_InitBuiltinClass(&Scm_ProcedureClass, "<procedure>",
                         proc_slots, 0, Scm_GaucheModule());
    Scm_ProcedureClass.flags |= SCM_CLASS_APPLICABLE;
}
