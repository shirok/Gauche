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
 *  $Id: io.c,v 1.2 2001-01-16 06:41:46 shiro Exp $
 */

#include "gauche.h"

/*
 * This file defines routines related to handling ports, requiring
 * running VM.
 */

/* call-with-*-file */


static void null_thunk(ScmObj *args, int nargs, void *data)
{
    SCM_RETURN(SCM_UNDEFINED);
}

static void wrapper1(ScmObj *args, int nargs, void *data)
{
    ScmObj p = SCM_OBJ(data);
    Scm_Apply1(SCM_CAR(p), SCM_CDR(p));
}

static void port_closer(ScmObj *args, int nargs, void *data)
{
    Scm_ClosePort(SCM_PORT(data));
    SCM_RETURN(SCM_UNDEFINED);
}

void Scm_CallWithFile(ScmString *path, ScmProcedure *proc, int inputp)
{
    ScmObj port, initializer, finalizer, nullproc, bodyproc;

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
    Scm_DynamicWind(nullproc, bodyproc, finalizer);
}
   
