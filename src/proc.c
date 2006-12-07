/*
 * proc.c - Procedures
 *
 *   Copyright (c) 2000-2005 Shiro Kawai, All rights reserved.
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
 *  $Id: proc.c,v 1.44 2006-12-07 04:58:48 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"
#include "gauche/code.h"

/*=================================================================
 * Classes
 */

static void proc_print(ScmObj obj, ScmPort *port, ScmWriteContext *);

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_ProcedureClass, proc_print);

static void proc_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmObj info = SCM_PROCEDURE_INFO(obj);
    if (SCM_PROCEDURE_TYPE(obj) == SCM_PROC_SUBR) {
        SCM_PUTZ("#<subr", -1, port);
        if (!SCM_FALSEP(info)) {
            Scm_Printf(port, " %A", info);
        }
        SCM_PUTC('>', port);
    } else {
        Scm_Printf(port, "#<closure %S>", info);
    }
}

/*=================================================================
 * Closure
 */

ScmObj Scm_MakeClosure(ScmObj code, ScmEnvFrame *env)
{
    ScmClosure *c = SCM_NEW(ScmClosure);
    int req, opt;
    ScmObj info;

    SCM_ASSERT(SCM_COMPILED_CODE(code));
    info = Scm_CompiledCodeFullName(SCM_COMPILED_CODE(code));
    req  = SCM_COMPILED_CODE_REQUIRED_ARGS(code);
    opt  = SCM_COMPILED_CODE_OPTIONAL_ARGS(code);

    SCM_SET_CLASS(c, SCM_CLASS_PROCEDURE);
    SCM_PROCEDURE_INIT(c, req, opt, SCM_PROC_CLOSURE, info);
    c->code = code;
    c->env = env;
    SCM_PROCEDURE(c)->inliner = SCM_COMPILED_CODE(code)->intermediateForm;
    
    return SCM_OBJ(c);
}

/*=================================================================
 * Subr
 */

ScmObj Scm_MakeSubr(ScmSubrProc *func,
                    void *data,
                    int required, int optional,
                    ScmObj info)
{
    ScmSubr *s = SCM_NEW(ScmSubr);
    SCM_SET_CLASS(s, SCM_CLASS_PROCEDURE);
    SCM_PROCEDURE_INIT(s, required, optional, SCM_PROC_SUBR, info);
    s->func = func;
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
        return Scm_VMApply1(proc, SCM_CAR(args));
    } else {
        return SCM_UNDEFINED;
    }
}

ScmObj Scm_ForEach1(ScmObj proc, ScmObj args)
{
    if (!SCM_NULLP(args)) {
        void *data[2];
        data[0] = proc;
        data[1] = SCM_CDR(args);
        Scm_VMPushCC(foreach1_cc, data, 2);
        return Scm_VMApply1(SCM_OBJ(proc), SCM_CAR(args));
    } else {
        return SCM_UNDEFINED;
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
        return Scm_VMApply1(proc, SCM_CAR(args));
    } else {
        return head;
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
        return Scm_VMApply1(SCM_OBJ(proc), SCM_CAR(args));
    } else {
        return SCM_NIL;
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
    return Scm_VMApply(proc, args);
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
    return Scm_VMApply(proc, args);
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
    proc->setter = SCM_OBJ(setter);
    proc->locked = lock;
    return SCM_OBJ(proc);
}

static ScmObj object_setter(ScmObj *args, int nargs, void *data)
{
    SCM_ASSERT(nargs == 1);
    return Scm_VMApply(SCM_OBJ(&Scm_GenericObjectSetter),
                       Scm_Cons(SCM_OBJ(data), args[0]));
}

static SCM_DEFINE_STRING_CONST(object_setter__NAME, "object-setter", 13, 13);

ScmObj Scm_Setter(ScmObj proc)
{
    if (SCM_PROCEDUREP(proc)) {
        /* NB: This used to signal an error if no setter procedure is associated
           to proc; now it returns #f in such case */
        return SCM_PROCEDURE(proc)->setter;
    } else {
        /* fallback to (setter object-apply) */
        return Scm_MakeSubr(object_setter, (void*)proc, 0, 1,
                            SCM_OBJ(&object_setter__NAME));
    }
}

int Scm_HasSetter(ScmObj proc)
{
    if (SCM_PROCEDUREP(proc)) {
        return !SCM_FALSEP(SCM_PROCEDURE(proc)->setter);
    } else {
        /* setter of object-apply is used. */
        return TRUE;
    }
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
    Scm_InitStaticClass(&Scm_ProcedureClass, "<procedure>",
                        Scm_GaucheModule(), proc_slots, 0);
    Scm_ProcedureClass.flags |= SCM_CLASS_APPLICABLE;
}
