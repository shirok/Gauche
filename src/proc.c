/*
 * proc.c - Procedures
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
#include "gauche/class.h"
#include "gauche/code.h"
#include "gauche/priv/builtin-syms.h"

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
            Scm_Printf(port, " %S", info);
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

    SCM_ASSERT(SCM_COMPILED_CODE(code));
    ScmObj info = Scm_Cons(Scm_CompiledCodeFullName(SCM_COMPILED_CODE(code)),
                           SCM_COMPILED_CODE(code)->argInfo);
    int req = SCM_COMPILED_CODE_REQUIRED_ARGS(code);
    int opt = SCM_COMPILED_CODE_OPTIONAL_ARGS(code);

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
 * Currying
 */

/* NB: This code doesn't work yet if the original procedure takes
   variable length arguments.  We disable this feature for now. */

#if 0

/* When a procedure is marked autocurrying and it is given with args less
 * than reqargs, Scm_CurryProcedure is called.  It saves the given args
 * into the curried_packet and creates a subr of curried procedure.
 *
 * The argument GIVEN points the beginning of argument array.  It actually
 * points into the VM stack, so we should copy the information before
 * calling anything that might change the VM state.
 * The # of given argument is indicated by NGIVEN.
 *
 * Note that some of the given arguments are already folded into a list
 * (in case we are called via APPLY).
 *
 *   when FOLDLEN < 0            when FOLDLEN >= 0
 *    N = NGIVEN-1                 N = NGIVEN-1
 *                                 K = NGIVEN-1-FOLDLEN
 *
 *          | argN |                 |   ----> (argK argK+1 ... argN)
 *          :      :                 :      :
 *          | arg1 |                 | arg1 |
 *   given >| arg0 |          given >| arg0 |
 *
 *  We assume at least one arg should be given in order to curry, and
 *  ngiven should always be smaller than the # of required args (otherwise
 *  we don't need to curry at all!).  Thus 0 < NGIVEN < REQUIRED.
 */
typedef struct curried_packet_rec {
    ScmObj proc;
    int ngiven;
    ScmObj argv[4];          /* keep first 4 args unfolded */
    ScmObj more;             /* the rest of args */
} curried_packet;

static ScmObj kick_curried(ScmObj *args, int nargs, void *data)
{
    curried_packet *p = (curried_packet*)data;
    ScmObj proc = p->proc;
    ScmObj *av = p->argv;
    SCM_ASSERT(p->ngiven + nargs >= 2);

    /* TODO: if p->proc takes variable length arguments, we shouldn't use
       Scm_VMApply* family below, since the last word of args already
       contains folded arguments, and Scm_VMApply causes to fold the
       arguments again.
    */

    switch (p->ngiven + nargs) {
    case 2:
        return Scm_VMApply2(proc, p->argv[0], args[0]);
    case 3:
        switch (nargs) {
        case 1: return Scm_VMApply3(proc, av[0], av[1], args[0]);
        case 2: return Scm_VMApply3(proc, av[0], args[0], args[1]);
        default: break;         /*NOTREACHED*/
        }
    case 4:
        switch (nargs) {
        case 1: return Scm_VMApply4(proc, av[0], av[1], av[2], args[0]);
        case 2: return Scm_VMApply4(proc, av[0], av[1], args[0], args[1]);
        case 3: return Scm_VMApply4(proc, av[0], args[0], args[1], args[2]);
        default: break;         /*NOTREACHED*/
        }
    default:
        {
            ScmObj h = SCM_NIL, t = SCM_NIL;
            for (int i = 0; i < p->ngiven; i++) SCM_APPEND1(h, t, av[i]);
            if (SCM_PAIRP(p->more)) SCM_APPEND(h, t, Scm_CopyList(p->more));
            for (int i = 0; i < nargs; i++) SCM_APPEND1(h, t, args[i]);
            return Scm_VMApply(proc, h);
        }
    }
    return SCM_UNDEFINED;       /* dummy */
}

ScmObj Scm_CurryProcedure(ScmObj proc, ScmObj *given, int ngiven, int foldlen)
{
    int required = SCM_PROCEDURE_REQUIRED(proc);
    int n = ngiven - foldlen;
    ScmObj h = SCM_NIL, t = SCM_NIL;
    ScmObj restarg = (foldlen > 0)? given[n] : SCM_NIL;

    SCM_ASSERT(SCM_PROCEDUREP(proc));
    SCM_ASSERT(ngiven < required && ngiven > 0);
    curried_packet *packet = SCM_NEW(curried_packet);
    packet->proc = proc;
    packet->ngiven = ngiven;

    /* pack the given args into the packet */
    switch (n) {
    default: packet->argv[3] = given[3]; /*FALLTHROUGH*/
    case 3: packet->argv[2] = given[2]; /*FALLTHROUGH*/
    case 2: packet->argv[1] = given[1]; /*FALLTHROUGH*/
    case 1: packet->argv[0] = given[0]; /*FALLTHROUGH*/
    }
    if (foldlen > 0) {
        for (int i=n; i<4 && SCM_PAIRP(restarg); i++, restarg = SCM_CDR(restarg)) {
            packet->argv[i] = SCM_CAR(restarg);
        }
    }
    for (int i=4; i<n; i++) {
        SCM_APPEND1(h, t, given[i]);
    }
    if (SCM_PAIRP(restarg)) {
        SCM_APPEND(h, t, Scm_CopyList(restarg));
    }
    packet->more = h;

    ScmObj subr = Scm_MakeSubr(kick_curried, (void*)packet,
                               required - ngiven, SCM_PROCEDURE_OPTIONAL(proc),
                               Scm_Cons(SCM_SYM_CURRIED, SCM_PROCEDURE_INFO(proc)));
    SCM_PROCEDURE_CURRYING(subr) = TRUE;
    return subr;
}

#endif /* 0 : disabling currying feature for now */

/*=================================================================
 * Mapper family
 * OBSOLETED - map and for-each is defiend in Scheme now (liblist.scm)
 * These are just kept for the backward compatibility.  Will be gone
 * in 1.0.
 */

ScmObj Scm_ForEach1(ScmObj proc, ScmObj args)
{
    return Scm_ForEach(proc, args, SCM_NIL);
}

ScmObj Scm_Map1(ScmObj proc, ScmObj args)
{
    return Scm_Map(proc, args, SCM_NIL);
}

ScmObj Scm_ForEach(ScmObj proc, ScmObj arg1, ScmObj args)
{
    static ScmObj stub = SCM_UNDEFINED;
    SCM_BIND_PROC(stub, "for-each", Scm_SchemeModule());
    return Scm_VMApply(stub, Scm_Cons(proc, Scm_Cons(arg1, args)));
}

ScmObj Scm_Map(ScmObj proc, ScmObj arg1, ScmObj args)
{
    static ScmObj stub = SCM_UNDEFINED;
    SCM_BIND_PROC(stub, "map", Scm_SchemeModule());
    return Scm_VMApply(stub, Scm_Cons(proc, Scm_Cons(arg1, args)));
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
    return SCM_MAKE_BOOL(p->optional); /* for backward compatibility */
}

static ScmObj proc_optcount(ScmProcedure *p)
{
    return SCM_MAKE_INT(p->optional);
}

static ScmObj proc_locked(ScmProcedure *p)
{
    return SCM_MAKE_BOOL(p->locked);
}

static ScmObj proc_currying(ScmProcedure *p)
{
    return SCM_MAKE_BOOL(p->currying);
}

static ScmObj proc_constant(ScmProcedure *p)
{
    return SCM_MAKE_BOOL(p->constant);
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
    SCM_CLASS_SLOT_SPEC("optcount", proc_optcount, NULL),
    SCM_CLASS_SLOT_SPEC("locked", proc_locked, NULL),
    SCM_CLASS_SLOT_SPEC("currying", proc_currying, NULL),
    SCM_CLASS_SLOT_SPEC("constant", proc_constant, NULL),
    SCM_CLASS_SLOT_SPEC("info", proc_info, NULL),
    SCM_CLASS_SLOT_SPEC("setter", proc_setter, NULL),
    SCM_CLASS_SLOT_SPEC_END()
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
