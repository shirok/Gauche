/*
 * io.c - higher-level input/output routines
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
 *  $Id: io.c,v 1.8 2001-03-11 09:42:23 shiro Exp $
 */

#include "gauche.h"

/*
 * This file defines routines related to handling ports, requiring
 * running VM.
 */

/* call-with-*-file */

static ScmObj wrapper1(ScmObj *args, int nargs, void *data)
{
    ScmObj p = SCM_OBJ(data);
    return Scm_VMApply1(SCM_CAR(p), SCM_CDR(p));
}

static ScmObj port_closer(ScmObj *args, int nargs, void *data)
{
    Scm_ClosePort(SCM_PORT(data));
    SCM_RETURN(SCM_UNDEFINED);
}

ScmObj Scm_CallWithFile(ScmString *path, ScmProcedure *proc, int inputp)
{
    ScmObj port, finalizer, bodyproc;

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
    bodyproc = Scm_MakeSubr(wrapper1,
                            (void*)Scm_Cons(SCM_OBJ(proc), port),
                            0, 0, SCM_FALSE);
    return Scm_VMDynamicWind(Scm_NullProc(), bodyproc, finalizer);
}


/* with-port */

struct with_port_packet {
    ScmObj origport;
    int type;
    int closep;
};

static ScmObj port_restorer(ScmObj *args, int nargs, void *data)
{
    struct with_port_packet *p = (struct with_port_packet*)data;
    ScmObj origport = p->origport;
    ScmPort *curport;
    
    if (p->type == 0) {
        curport = SCM_VM_CURRENT_INPUT_PORT(Scm_VM());
        SCM_VM_CURRENT_INPUT_PORT(Scm_VM()) = SCM_PORT(origport);
    } else if (p->type == 1) {
        curport = SCM_VM_CURRENT_OUTPUT_PORT(Scm_VM());
        SCM_VM_CURRENT_OUTPUT_PORT(Scm_VM()) = SCM_PORT(origport);
    } else {
        curport = SCM_VM_CURRENT_ERROR_PORT(Scm_VM());
        SCM_VM_CURRENT_ERROR_PORT(Scm_VM()) = SCM_PORT(origport);
    }
    if (p->closep) Scm_ClosePort(curport);
    return SCM_UNDEFINED;
}

ScmObj Scm_WithPort(ScmPort *port, ScmProcedure *thunk, int type, int closep)
{
    ScmObj origport, finalizer;
    struct with_port_packet *packet;
    
    SCM_ASSERT(type >= 0 && type <= 2);
    if (SCM_PROCEDURE_REQUIRED(thunk) != 0) {
        Scm_Error("thunk required: %S", thunk);
    }
    switch (type) {
    case 0:
        origport = SCM_OBJ(SCM_VM_CURRENT_INPUT_PORT(Scm_VM()));
        SCM_VM_CURRENT_INPUT_PORT(Scm_VM()) = port;
        break;
    case 1:
        origport = SCM_OBJ(SCM_VM_CURRENT_OUTPUT_PORT(Scm_VM()));
        SCM_VM_CURRENT_OUTPUT_PORT(Scm_VM()) = port;
        break;
    case 2:
        origport = SCM_OBJ(SCM_VM_CURRENT_ERROR_PORT(Scm_VM()));
        SCM_VM_CURRENT_ERROR_PORT(Scm_VM()) = port;
        break;
    }
    
    packet = SCM_NEW(struct with_port_packet);
    packet->origport = origport;
    packet->type = type;
    packet->closep = closep;
    finalizer = Scm_MakeSubr(port_restorer, (void*)packet,
                             0, 0, SCM_FALSE);
    return Scm_VMDynamicWind(Scm_NullProc(), SCM_OBJ(thunk), finalizer);
}

ScmObj Scm_WithFile(ScmString *path, ScmProcedure *thunk, int type)
{
    ScmObj port;
    port = Scm_OpenFilePort(Scm_GetStringConst(path),
                            (type == 0)? "r" : "w");
    if (port == SCM_FALSE)
        Scm_Error("can't open %s file: %S",
                  (type == 0)? "input" : "output", path);
    return Scm_WithPort(SCM_PORT(port), thunk, type, TRUE);
}
