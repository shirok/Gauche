/*
 * signal.c - signal handling
 *
 *  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, disribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: signal.c,v 1.1 2002-01-04 16:08:55 shirok Exp $
 */

#include "gauche.h"
#include "gauche/class.h"

/*=======================================================
 * Signal class
 */

static void signal_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmSignal *s = (ScmSignal*)obj;
    Scm_Printf(port, "#<signal %d>", s->sig);
}

static ScmObj signal_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_SignalClass,
                         signal_print, NULL, NULL,
                         signal_allocate,
                         SCM_CLASS_DEFAULT_CPL);

static ScmObj signal_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmSignal *s = SCM_ALLOCATE(ScmSignal, klass);
    SCM_SET_CLASS(s, klass);
    s->sig = 0;
    return SCM_OBJ(s);
}

static ScmObj signal_sig_get(ScmSignal *sig)
{
    return SCM_MAKE_INT(sig->sig);
}

static void signal_sig_set(ScmSignal *sig, ScmObj code)
{
    if (!SCM_INTP(code)) Scm_Error("integer required, but got %S", code);
    sig->sig = SCM_INT_VALUE(code);
}

static ScmClassStaticSlotSpec signal_slots[] = {
    SCM_CLASS_SLOT_SPEC("signal",
                        signal_sig_get,
                        signal_sig_set),
    { NULL }
};

/*=======================================================
 * Initialization
 */

void Scm__InitSignal()
{
    ScmModule *mod = Scm_GaucheModule();
    Scm_InitBuiltinClass(&Scm_SignalClass, "<signal>",
                         signal_slots, sizeof(ScmSignal), mod);
}

