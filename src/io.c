/*
 * io.c - higher-level input/output routines
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
 *  $Id: io.c,v 1.4 2001-01-24 11:30:39 shiro Exp $
 */

#include "gauche.h"

/*
 * This file defines routines related to handling ports, requiring
 * running VM.
 */

/* call-with-*-file */


static ScmObj null_thunk(ScmObj *args, int nargs, void *data)
{
    SCM_RETURN(SCM_UNDEFINED);
}

static ScmObj wrapper1(ScmObj *args, int nargs, void *data)
{
    ScmObj p = SCM_OBJ(data);
    Scm_VMApply1(SCM_CAR(p), SCM_CDR(p));
    return SCM_FALSE;
}

static ScmObj port_closer(ScmObj *args, int nargs, void *data)
{
    Scm_ClosePort(SCM_PORT(data));
    SCM_RETURN(SCM_UNDEFINED);
}

void Scm_CallWithFile(ScmString *path, ScmProcedure *proc, int inputp)
{
    ScmObj port, finalizer, nullproc, bodyproc;

    if (!(SCM_PROCEDURE_REQUIRED(proc) == 1
          || (SCM_PROCEDURE_REQUIRED(proc)==0
              && SCM_PROCEDURE_OPTIONAL(proc)))) {
        Scm_Error("procedure must take 1 argument: %S", proc);
    }
    port = Scm_OpenFilePort(Scm_GetStringConst(path),
                            inputp? "r" : "w");
    if (port == SCM_FALSE)
        Scm_Error("can't open %s file: %S",
                  inputp? "input" : "output", path);
    finalizer = Scm_MakeSubr(port_closer, (void*)port, 0, 0, SCM_FALSE);
    nullproc = Scm_MakeSubr(null_thunk, NULL, 0, 0, SCM_FALSE);
    bodyproc = Scm_MakeSubr(wrapper1,
                            (void*)Scm_Cons(SCM_OBJ(proc), port),
                            0, 0, SCM_FALSE);
    Scm_VMDynamicWind(nullproc, bodyproc, finalizer);
}

static ScmObj port_restorer(ScmObj *args, int nargs, void *data)
{
    ScmObj origport = SCM_CAR(SCM_OBJ(data));
    ScmObj type = SCM_CDR(SCM_OBJ(data));
    if (type == SCM_MAKE_INT(0)) {
        Scm_ClosePort(SCM_VM_CURRENT_INPUT_PORT(Scm_VM()));
        SCM_VM_CURRENT_INPUT_PORT(Scm_VM()) = SCM_PORT(origport);
    } else if (type == SCM_MAKE_INT(1)) {
        Scm_ClosePort(SCM_VM_CURRENT_OUTPUT_PORT(Scm_VM()));
        SCM_VM_CURRENT_OUTPUT_PORT(Scm_VM()) = SCM_PORT(origport);
    } else {
        Scm_ClosePort(SCM_VM_CURRENT_ERROR_PORT(Scm_VM()));
        SCM_VM_CURRENT_ERROR_PORT(Scm_VM()) = SCM_PORT(origport);
    }
    return SCM_UNDEFINED;
}
   
void Scm_WithFile(ScmString *path, ScmProcedure *thunk, int type)
{
    ScmObj port, origport, finalizer, nullproc;
    SCM_ASSERT(type >= 0 && type <= 2);
    if (SCM_PROCEDURE_REQUIRED(thunk) != 0) {
        Scm_Error("thunk required: %S", thunk);
    }
    port = Scm_OpenFilePort(Scm_GetStringConst(path),
                            (type == 0)? "r" : "w");
    if (port == SCM_FALSE)
        Scm_Error("can't open %s file: %S",
                  (type == 0)? "input" : "output", path);
    switch (type) {
    case 0:
        origport = SCM_OBJ(SCM_VM_CURRENT_INPUT_PORT(Scm_VM()));
        SCM_VM_CURRENT_INPUT_PORT(Scm_VM()) = SCM_PORT(port);
        break;
    case 1:
        origport = SCM_OBJ(SCM_VM_CURRENT_OUTPUT_PORT(Scm_VM()));
        SCM_VM_CURRENT_OUTPUT_PORT(Scm_VM()) = SCM_PORT(port);
        break;
    case 2:
        origport = SCM_OBJ(SCM_VM_CURRENT_ERROR_PORT(Scm_VM()));
        SCM_VM_CURRENT_ERROR_PORT(Scm_VM()) = SCM_PORT(port);
        break;
    }
    
    finalizer = Scm_MakeSubr(port_restorer,
                             Scm_Cons(origport, SCM_MAKE_INT(type)),
                             0, 0, SCM_FALSE);
    nullproc = Scm_MakeSubr(null_thunk, NULL, 0, 0, SCM_FALSE);
    Scm_VMDynamicWind(nullproc, SCM_OBJ(thunk), finalizer);
}
