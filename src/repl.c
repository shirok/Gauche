/*
 * repl.c - repl
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
 *  $Id: repl.c,v 1.19 2002-02-07 10:33:51 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/vm.h"

void Scm_Repl(ScmObj prompt, ScmPort *in, ScmPort *out)
{
    ScmObj c, v;
    volatile int eofread = FALSE;
    
    for (;;) {
        ScmObj p = Scm_Eval(prompt, SCM_UNBOUND);
        SCM_UNWIND_PROTECT {
            Scm_Write(p, SCM_OBJ(out), SCM_WRITE_DISPLAY);
            SCM_FLUSH(out);
            v = Scm_Read(SCM_OBJ(in));
            if (SCM_EOFP(v)) { eofread = TRUE; break; }
            Scm_Eval(v, SCM_UNBOUND);
            SCM_FOR_EACH(c, Scm_VMGetResult(Scm_VM())) {
                Scm_Printf(out, "%S\n", SCM_CAR(c));
            }
        }
        SCM_WHEN_ERROR {
            switch (Scm_VM()->escapeReason) {
            case SCM_VM_ESCAPE_CONT:
                SCM_NEXT_HANDLER;
            default:
                Scm_Panic("unknown escape");
            case SCM_VM_ESCAPE_ERROR:
                break;
            }
        }
        SCM_END_PROTECT;
        if (eofread) return;
    }
}

