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
 *  $Id: load.c,v 1.15 2001-02-17 10:23:26 shiro Exp $
 */

#include <unistd.h>
#include "gauche.h"

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

/*
 * Load file.
 */

/* To peek Scheme variable from C */
static ScmGloc *load_path_rec;       /* *load-path*       */
static ScmGloc *load_next_rec;       /* *load-next*       */
static ScmGloc *load_history_rec;    /* *load-history*    */
static ScmGloc *load_filename_rec;   /* *load-filename*   */

/*--------------------------------------------------------------------
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

/*---------------------------------------------------------------------
 * Scm_FindFile
 *
 *   Core function to search specified file from the search path *PATH.
 *   Search rules are:
 *   
 *    (1) If given filename begins with "/", "./" or "../", the file is
 *        searched.
 *    (2) If gievn filename begins with "~", unix-style username
 *        expansion is done, then the resulting file is searched.
 *    (3) Otherwise, the file is searched for each directory in
 *        *load-path*.
 *
 *   If a file is found, it's pathname is returned.  *PATH is modified
 *   to contain the remains of *load-path*, which can be used again to
 *   find next matching filename.
 */

ScmObj Scm_FindFile(ScmString *filename, ScmObj *paths, int error_if_not_found)
{
    int size = SCM_STRING_LENGTH(filename);
    const char *ptr = SCM_STRING_START(filename);
    int use_load_paths = TRUE;
    ScmObj file = SCM_OBJ(filename);
    
    if (size == 0) Scm_Error("bad filename to load: \"\"");
    if (*ptr == '~') {
        file = Scm_NormalizePathname(filename, SCM_PATH_EXPAND);
        use_load_paths = FALSE;
    } else if (*ptr == '/'
               || (*ptr == '.' && *(ptr+1) == '/')
               || (*ptr == '.' && *(ptr+1) == '.' && *(ptr+2) == '/')) {
        use_load_paths = FALSE;
    }

    if (use_load_paths) {
        ScmObj lpath, fpath;
        SCM_FOR_EACH(lpath, *paths) {
            if (!SCM_STRINGP(SCM_CAR(lpath))) {
                /* TODO: should be warning? */
                Scm_Error("*load-path* contains invalid element: %S", *paths);
            }
            fpath = Scm_StringAppendC(SCM_STRING(SCM_CAR(lpath)), "/", 1, 1);
            fpath = Scm_StringAppend2(SCM_STRING(fpath), SCM_STRING(file));
            if (access(Scm_GetStringConst(SCM_STRING(fpath)), F_OK) == 0)
                break;
        }
        if (SCM_PAIRP(lpath)) {
            *paths = SCM_CDR(lpath);
            return SCM_OBJ(fpath);
        } else if (error_if_not_found) {
            Scm_Error("cannot find file %S in *load-path* %S", file, *paths);
        } else {
            *paths = SCM_NIL;
        }
    } else {
        *paths = SCM_NIL;
        if (access(Scm_GetStringConst(SCM_STRING(file)), F_OK) == 0) {
            return SCM_OBJ(file);
        } else if (error_if_not_found) {
            Scm_Error("cannot find file %S to load", file);
        }
    }
    return SCM_FALSE;
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

ScmObj Scm_VMLoad(ScmString *filename)
{
    ScmObj port, truename, load_paths = Scm_GetLoadPath();

    truename = Scm_FindFile(filename, &load_paths, TRUE);
    load_next_rec->value = load_paths;
    port = Scm_OpenFilePort(Scm_GetStringConst(SCM_STRING(truename)), "r");
    if (SCM_FALSEP(port)) {
        Scm_Error("file %S exists, but couldn't open.", truename);
    }
    return Scm_VMLoadFromPort(SCM_PORT(port));
}

void Scm_Load(const char *cpath)
{
    ScmObj f = Scm_MakeString(cpath, -1, -1);
    ScmObj l = SCM_INTERN("load");
    Scm_Eval(SCM_LIST2(l, f), SCM_NIL);
}

/*
 * Utilities
 */

ScmObj Scm_GetLoadPath(void)
{
    return load_path_rec->value;
}

ScmObj Scm_AddLoadPath(const char *cpath, int afterp)
{
    ScmObj spath = Scm_MakeString(cpath, -1, -1);
    /* for safety */
    if (!SCM_PAIRP(load_path_rec->value)) {
        load_path_rec->value = SCM_LIST1(spath);
    } else if (afterp) {
        load_path_rec->value =
            Scm_Append2(load_path_rec->value, SCM_LIST1(spath));
    } else {
        load_path_rec->value = Scm_Cons(spath, load_path_rec->value);
    }
    return load_path_rec->value;
}

/*
 * Dynamic link
 */

SCM_DEFCLASS(Scm_DLObjClass, "<dynamic-linked-object>", NULL, SCM_CLASS_DEFAULT_CPL);

ScmObj Scm_DynLink(ScmString *filename)
{
#ifdef HAVE_DLFCN_H
    ScmDLObj *dlobj;
    ScmObj truename, load_paths = Scm_GetLoadPath();
    void *handle;

    truename = Scm_FindFile(filename, &load_paths, FALSE);
    handle = dlopen(Scm_GetStringConst(SCM_STRING(truename)), RTLD_LAZY);
    if (handle == NULL) return SCM_FALSE;

    dlobj = SCM_NEW(ScmDLObj);
    SCM_SET_CLASS(dlobj, SCM_CLASS_DLOBJ);
    dlobj->handle = handle;
    dlobj->initialized = FALSE;
    return SCM_OBJ(dlobj);
#else
    Scm_Error("dynamic linking is not supported on this architecture");
    return SCM_FALSE;           /* dummy */
#endif
}

int Scm_DynInit(ScmDLObj *dlobj, ScmString *initfn)
{
    void (*func)(void);

    if (!dlobj->initialized) {
        func = (void(*)(void))dlsym(dlobj->handle, Scm_GetStringConst(initfn));
        if (func == NULL) return FALSE;
        func();
        dlobj->initialized = TRUE;
    }
    return TRUE;
}

/*
 * Initialization
 */

void Scm__InitLoad(void)
{
    ScmObj instdir = SCM_MAKE_STR(SCM_INSTALL_DIR);
    ScmObj curdir = SCM_MAKE_STR(".");
    ScmModule *m = Scm_SchemeModule();

#define DEF(rec, sym, val) \
    rec = SCM_GLOC(Scm_Define(m, SCM_SYMBOL(sym), val))

    DEF(load_path_rec,    SCM_SYM_LOAD_PATH, SCM_LIST2(curdir, instdir));
    DEF(load_history_rec, SCM_SYM_LOAD_HISTORY, SCM_NIL);
    DEF(load_next_rec,    SCM_SYM_LOAD_NEXT, SCM_NIL);
    DEF(load_filename_rec,SCM_SYM_LOAD_FILENAME, SCM_FALSE);
}
