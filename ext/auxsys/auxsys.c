/*
 * auxsys.c - Auxiliary system functions
 *
 *  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: auxsys.c,v 1.1 2002-02-21 08:55:52 shirok Exp $
 */

#include <gauche.h>
#include <gauche/extend.h>

extern void Scm_Init_auxsyslib(ScmModule *mod);

void Scm_Init_auxsys(void)
{
    ScmModule *mod;

    SCM_INIT_EXTENSION(auxsys);
    mod = SCM_MODULE(SCM_FIND_MODULE("gauche.auxsys", TRUE));
    Scm_Init_auxsyslib(mod);
}
