/*
 * load.c - load a program
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: load.c,v 1.10 2001-02-06 08:04:19 shiro Exp $
 */

#include "gauche.h"

/*
 * Load file.
 */

/* To peek *load-path* variable from C */
static ScmGloc *load_path_rec;

/*
 * Scm_LoadFromPort
 * 
 *   The most basic function in the load()-family.  Read an expression
 *   from the given port and evaluates it repeatedly, until it reaches
 *   EOF.  Then the port is closed.
 *
 *   The result of the last evaluation remains on VM.
 */

/* C-continuation of the loading */
static ScmObj load_cc(ScmObj result, void **data)
{
    ScmObj port = SCM_OBJ(data[0]);
    ScmObj expr = Scm_Read(port);


    if (!SCM_EOFP(expr)) {
        Scm_VMPushCC(load_cc, (void **)&port, 1);
        Scm_VMEval(expr, SCM_UNBOUND);
    } else {
        Scm_ClosePort(SCM_PORT(port));
    }
    SCM_RETURN(result);
}

ScmObj Scm_VMLoadFromPort(ScmPort *port)
{
    if (!SCM_IPORTP(port))
        Scm_Error("input port required, but got: %S", port);
    if (SCM_PORT_CLOSED_P(port))
        Scm_Error("port already closed: %S", port);
    return load_cc(SCM_NIL, (void **)&port);
}

/*
 * Load
 */

/* TODO: expand ~user in the pathname */

ScmObj Scm_VMTryLoad(const char *cpath)
{
    ScmObj p = Scm_OpenFilePort(cpath, "r");
    if (SCM_FALSEP(p)) return FALSE;
    return Scm_VMLoadFromPort(SCM_PORT(p));
}

ScmObj Scm_VMLoad(const char *cpath)
{
    ScmObj p, lpath, spath, fpath;

    p = Scm_OpenFilePort(cpath, "r");
    if (SCM_FALSEP(p)) {
        if (cpath[0] == '/') Scm_Error("cannot open file: %s", cpath);
        /* TODO: more efficient pathname handling */
        spath = Scm_MakeString(cpath, -1, -1);
        SCM_FOR_EACH(lpath, load_path_rec->value) {
            if (!SCM_STRINGP(SCM_CAR(lpath))) {
                /* TODO: should be warning? */
                Scm_Error("*load-path* contains invalid element: %S",
                          load_path_rec->value);
            }
            fpath = Scm_StringAppendC(SCM_STRING(SCM_CAR(lpath)), "/", 1, 1);
            fpath = Scm_StringAppend2(SCM_STRING(fpath), SCM_STRING(spath));
            p = Scm_OpenFilePort(Scm_GetStringConst(SCM_STRING(fpath)), "r");
            if (!SCM_FALSEP(p)) break;
        }
        if (SCM_FALSEP(p)) {
            Scm_Error("cannot find file %s in *load-path* %S",
                      cpath, load_path_rec->value);
        }
    }
    return Scm_VMLoadFromPort(SCM_PORT(p));
}

void Scm_Load(const char *cpath)
{
    ScmObj f = Scm_MakeString(cpath, -1, -1);
    ScmObj l = SCM_INTERN("load");
    Scm_Eval(SCM_LIST2(l, f), SCM_NIL);
}

/*
 * Initialization
 */

void Scm__InitLoad(void)
{
    ScmObj instdir = SCM_MAKE_STR(SCM_INSTALL_DIR);
    ScmObj curdir = SCM_MAKE_STR(".");

    Scm_Define(Scm_SchemeModule(), SCM_SYMBOL(SCM_SYM_LOAD_PATH),
               SCM_LIST2(instdir, curdir));
    load_path_rec = Scm_FindBinding(Scm_SchemeModule(),
                                    SCM_SYMBOL(SCM_SYM_LOAD_PATH),
                                    TRUE);
}
