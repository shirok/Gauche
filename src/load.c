/*
 * load.c - load a program
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
 *  $Id: load.c,v 1.21 2001-03-06 08:55:03 shiro Exp $
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
 *
 *   No matter how the load terminates, either normal or abnormal,
 *   the port is closed, and the current module is restored to the
 *   one when load is called.
 */

struct load_packet {
    ScmPort *port;
    ScmModule *prev_module;
};

/* Clean up */
static ScmObj load_after(ScmObj *args, int nargs, void *data)
{
    struct load_packet *p = (struct load_packet *)data;
    Scm_ClosePort(p->port);
    Scm_SelectModule(p->prev_module);
    return SCM_UNDEFINED;
}

/* C-continuation of the loading */
static ScmObj load_cc(ScmObj result, void **data)
{
    ScmObj port = SCM_OBJ(data[0]);
    ScmObj expr = Scm_Read(port);

    if (!SCM_EOFP(expr)) {
        Scm_VMPushCC(load_cc, (void **)&port, 1);
        Scm_VMEval(expr, SCM_UNBOUND);
    }
    SCM_RETURN(result);
}

static ScmObj load_body(ScmObj *args, int nargs, void *data)
{
    struct load_packet *p = (struct load_packet *)data;
    return load_cc(SCM_NIL, (void **)&p->port);
}

ScmObj Scm_VMLoadFromPort(ScmPort *port)
{
    struct load_packet *p;
    
    if (!SCM_IPORTP(port))
        Scm_Error("input port required, but got: %S", port);
    if (SCM_PORT_CLOSED_P(port))
        Scm_Error("port already closed: %S", port);

    p = SCM_NEW(struct load_packet);
    p->port = port;
    p->prev_module = Scm_CurrentModule();
    return Scm_VMDynamicWindC(NULL, load_body, load_after, p);
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

/*------------------------------------------------------------------
 * Dynamic link
 */

/* TODO: keep catalog of loaded object to avoid loading the same object
   more than once */
ScmObj Scm_DynLoad(ScmString *filename, ScmObj initfn)
{
#ifdef HAVE_DLFCN_H
    ScmObj truename, load_paths = Scm_GetLoadPath();
    void *handle;
    void (*func)(void);
    const char *initname;

    if (SCM_STRINGP(initfn)) {
        initname = Scm_GetStringConst(SCM_STRING(initfn));
    } else {
        initname = "Scm_DLInit";
    }
    
    truename = Scm_FindFile(filename, &load_paths, TRUE);
    handle = dlopen(Scm_GetStringConst(SCM_STRING(truename)), RTLD_LAZY);
    if (handle == NULL) {
        /* TODO: check if dlerror() is available on all platforms */
        const char *err = dlerror();
        if (err == NULL) {
            Scm_Error("failed to link %S dynamically", truename);
        } else {
            Scm_Error("failed to link %S dynamically: %s", truename, err);
        }
    }
    func = (void(*)(void))dlsym(handle, initname);
    if (func == NULL) {
        dlclose(handle);
        Scm_Error("dynamic linking of %S failed: couldn't find initialization function", truename);
    }
    func();
    return SCM_TRUE;
#else
    Scm_Error("dynamic linking is not supported on this architecture");
    return SCM_FALSE;           /* dummy */
#endif
}

/*------------------------------------------------------------------
 * Autoload
 */

struct autoload_data {
    ScmSymbol *name;
    ScmModule *module;
    ScmString *path;
    ScmObj args;
    int loaded;
};

static ScmObj autoload_cc(ScmObj result, void *data[])
{
    struct autoload_data *a = (struct autoload_data *)data[0];
    ScmGloc *g = Scm_FindBinding(a->module, a->name, FALSE);
    SCM_ASSERT(g != NULL && !SCM_UNBOUNDP(g->value));
    return Scm_VMApply(g->value, a->args);
}

static ScmObj autoload_sub(ScmObj *argv, int nargs, void *data)
{
    struct autoload_data *adata = (struct autoload_data *)data;
    if (adata->loaded) {
        /* If we're here, the autoloader procedure hasn't properly
           overwritten by loading. */
        Scm_Error("symbol %S is not redefined by autoloading %S",
                  adata->name, adata->path);
    }
    adata->loaded = TRUE;
    SCM_ASSERT(nargs == 1);
    adata->args = argv[0];
    Scm_VMPushCC(autoload_cc, (void **)&adata, 1);
    return Scm_VMLoad(adata->path);
}

ScmObj Scm_MakeAutoload(ScmSymbol *name, ScmString *path)
{
    struct autoload_data *adata = SCM_NEW(struct autoload_data);
    ScmObj p;
    adata->name = name;
    adata->module = SCM_CURRENT_MODULE();
    adata->path = path;
    adata->args = SCM_NIL;
    adata->loaded = FALSE;

    p = Scm_MakeOutputStringPort();
    Scm_Printf(SCM_PORT(p), "autoload %A::%A (%A)",
               adata->module->name, adata->name, adata->path);
    return Scm_MakeSubr(autoload_sub, (void*)adata, 0, 1,
                        Scm_GetOutputString(SCM_PORT(p)));
}

/*------------------------------------------------------------------
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
