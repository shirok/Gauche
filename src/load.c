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
 *  $Id: load.c,v 1.37 2001-05-19 10:56:28 shirok Exp $
 */

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <ctype.h>
#include "gauche.h"
#include "gauche/arch.h"

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#define LOAD_SUFFIX ".scm"

/*
 * Load file.
 */

/* To peek Scheme variable from C. */
/* TODO: need to lock in MT */
static ScmGloc *load_path_rec;       /* *load-path*         */
static ScmGloc *dynload_path_rec;    /* *dynamic-load-path* */

/* TODO: the followings should be in VM prvate in MT env */
static ScmGloc *load_next_rec;       /* *load-next*         */
static ScmGloc *load_history_rec;    /* *load-history*      */
static ScmGloc *load_port_rec;       /* *load-port*         */

/* List of provided features.  Must be protected in MT. */
static ScmObj provided;

/* List of dynamically loaded objects.  Must be protected in MT. */
static ScmObj dso_list;

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
    ScmObj prev_port;
    ScmObj prev_history;
};

/* Clean up */
static ScmObj load_after(ScmObj *args, int nargs, void *data)
{
    struct load_packet *p = (struct load_packet *)data;
    Scm_ClosePort(p->port);
    Scm_SelectModule(p->prev_module);
    load_port_rec->value = p->prev_port;
    load_history_rec->value = p->prev_history;
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
    ScmObj port_info;
    
    if (!SCM_IPORTP(port))
        Scm_Error("input port required, but got: %S", port);
    if (SCM_PORT_CLOSED_P(port))
        Scm_Error("port already closed: %S", port);

    p = SCM_NEW(struct load_packet);
    p->port = port;
    p->prev_module = Scm_CurrentModule();
    p->prev_port = load_port_rec->value;
    p->prev_history = load_history_rec->value;

    load_port_rec->value = SCM_OBJ(port);
    if (SCM_PORTP(p->prev_port)) {
        port_info = SCM_LIST2(p->prev_port,
                              Scm_MakeInteger(Scm_PortLine(SCM_PORT(p->prev_port))));
    } else {
        port_info = SCM_LIST1(SCM_FALSE);
    }
    load_history_rec->value = Scm_Cons(port_info, load_history_rec->value);
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
 *    (2) If given filename begins with "~", unix-style username
 *        expansion is done, then the resulting file is searched.
 *    (3) Otherwise, the file is searched for each directory in
 *        *load-path*.
 *
 *   If a file is found, it's pathname is returned.  *PATH is modified
 *   to contain the remains of *load-path*, which can be used again to
 *   find next matching filename.
 */

static int regfilep(ScmObj path)
{
    struct stat statbuf;
    int r = stat(Scm_GetStringConst(SCM_STRING(path)), &statbuf);
    if (r < 0) return FALSE;
    return S_ISREG(statbuf.st_mode);
}

ScmObj Scm_FindFile(ScmString *filename, ScmObj *paths, int error_if_not_found)
{
    int size = SCM_STRING_LENGTH(filename);
    const char *ptr = SCM_STRING_START(filename);
    int use_load_paths = TRUE;
    ScmObj file = SCM_OBJ(filename), fpath = SCM_NIL;
    
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
        ScmObj lpath;
        SCM_FOR_EACH(lpath, *paths) {
            if (!SCM_STRINGP(SCM_CAR(lpath))) {
                /* TODO: should be warning? */
                Scm_Error("*load-path* contains invalid element: %S", *paths);
            }
            fpath = Scm_StringAppendC(SCM_STRING(SCM_CAR(lpath)), "/", 1, 1);
            fpath = Scm_StringAppend2(SCM_STRING(fpath), SCM_STRING(file));
            if (regfilep(fpath)) break;
            fpath = Scm_StringAppendC(SCM_STRING(fpath), LOAD_SUFFIX, -1, -1);
            if (regfilep(fpath)) break;
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
        if (regfilep(file)) return SCM_OBJ(file);
        fpath = Scm_StringAppendC(SCM_STRING(file), LOAD_SUFFIX, -1, -1);
        if (regfilep(fpath)) return fpath;
        if (error_if_not_found) {
            Scm_Error("cannot find file %S to load", file);
        }
    }
    return SCM_FALSE;
}

/*
 * Load
 */

ScmObj Scm_VMLoad(ScmString *filename, int errorp)
{
    ScmObj port, truename, load_paths = Scm_GetLoadPath();

    truename = Scm_FindFile(filename, &load_paths, errorp);
    if (SCM_FALSEP(truename)) return SCM_FALSE;
    load_next_rec->value = load_paths;
    port = Scm_OpenFilePort(Scm_GetStringConst(SCM_STRING(truename)), "r");
    if (SCM_FALSEP(port)) {
        if (errorp) 
            Scm_Error("file %S exists, but couldn't open.", truename);
        else
            return SCM_FALSE;
    }
    return Scm_VMLoadFromPort(SCM_PORT(port));
}

void Scm_Load(const char *cpath, int errorp)
{
    ScmObj f = SCM_MAKE_STR_COPYING(cpath);
    ScmObj l = SCM_INTERN("load");
    if (errorp) {
        Scm_Eval(SCM_LIST2(l, f), SCM_NIL);
    } else {
        ScmObj k = SCM_MAKE_KEYWORD("error-if-not-found");
        Scm_Eval(SCM_LIST4(l, f, k, SCM_FALSE), SCM_NIL);
    }
}

/*
 * Utilities
 */

ScmObj Scm_GetLoadPath(void)
{
    return load_path_rec->value;
}

ScmObj Scm_GetDynLoadPath(void)
{
    return dynload_path_rec->value;
}

static ScmObj break_env_paths(const char *envname)
{
    const char *e = getenv(envname);
    if (geteuid() == 0) return SCM_NIL; /* don't trust env when run by root */
    if (e == NULL) return SCM_NIL;
    else return Scm_StringSplitByChar(SCM_STRING(SCM_MAKE_STR_COPYING(envname)), ':');
}

/* Add CPATH to the current list of load path.  The path is
 * added before the current list, unless AFTERP is true.
 * The existence of CPATH is not checked.
 *
 * Besides load paths, existence of directories CPATH/$ARCH and
 * CPATH/../$ARCH is checked, where $ARCH is the system architecture
 * signature, and if found, it is added to the dynload_path.  If
 * no such directory is found, CPATH itself is added to the dynload_path.
 */
ScmObj Scm_AddLoadPath(const char *cpath, int afterp)
{
    ScmObj spath = SCM_MAKE_STR_COPYING(cpath);
    ScmObj dpath;
    struct stat statbuf;

    if (!SCM_PAIRP(load_path_rec->value)) {
        load_path_rec->value = SCM_LIST1(spath);
    } else if (afterp) {
        load_path_rec->value =
            Scm_Append2(load_path_rec->value, SCM_LIST1(spath));
    } else {
        load_path_rec->value = Scm_Cons(spath, load_path_rec->value);
    }

    /* check dynload path */
    dpath = Scm_StringAppendC(SCM_STRING(spath), "/", 1, 1);
    dpath = Scm_StringAppendC(SCM_STRING(dpath), Scm_HostArchitecture(),-1,-1);
    if (stat(Scm_GetStringConst(SCM_STRING(dpath)), &statbuf) < 0
        || !S_ISDIR(statbuf.st_mode)) {
        dpath = Scm_StringAppendC(SCM_STRING(spath), "/../", 4, 4);
        dpath = Scm_StringAppendC(SCM_STRING(dpath), Scm_HostArchitecture(),-1,-1);
        if (stat(Scm_GetStringConst(SCM_STRING(dpath)), &statbuf) < 0
            || !S_ISDIR(statbuf.st_mode)) {
            dpath = spath;
        }
    }

    if (!SCM_PAIRP(dynload_path_rec->value)) {
        dynload_path_rec->value = SCM_LIST1(dpath);
    } else if (afterp) {
        dynload_path_rec->value =
            Scm_Append2(dynload_path_rec->value, SCM_LIST1(dpath));
    } else {
        dynload_path_rec->value = Scm_Cons(dpath, dynload_path_rec->value);
    }
    
    return load_path_rec->value;
}

/*------------------------------------------------------------------
 * Dynamic link
 */

/* Problem of transitive dynamic linking:
 *
 * Suppose you wrote a useful extention library for Gauche, named useful.so.
 * It can be loaded into Gauche by dynamic linking feature.   Then, the 
 * other person wrote another extention library, another.so, from which
 * he wants to call a C function defined in useful.so.  What shall he do?
 * Since generally, symbols in dlopen-ed useful.so is not visible from
 * other dlopen-ed libraries.  Using RTLD_GLOBAL in Scm_DynLoad allows
 * another.so to see global symbols in useful.so, but do we really want
 * to open all of the global symbols of dynloaded libraries?
 * Furthermore, how can another.so ensure useful.so has been loaded before it?
 * 
 * The other option is to name an extention library lib*.so, if it exposes
 * C-level functions meant to be called by the others.  Using the above
 * example, useful.so should be libuseful.so, and when the other person
 * compiles another.so, he/she specify "`gauche-config -L` -luseful".
 * Then another.so can see libuseful.so, and the latter is loaded
 * automatically (as far as LD_LIBRARY_PATH contains the location of
 * libuseful.so) if it has not already been loaded.
 *
 * In initialize routine of another.so, he/she must call Scm_Init_libuseful()
 * as well, to ensure the initialization of the useful module.
 * Consequently, Scm_Init_* function should be prepared to be called more than
 * one time.
 */

#ifdef HAVE_DLOPEN
static const char *get_dynload_initfn(const char *filename)
{
    const char *head, *tail, *s;
    char *name, *d;
    const char prefix[] = "Scm_Init_";
    
    head = strrchr(filename, '/');
    if (head == NULL) head = filename;
    else head++;
    tail = strchr(head, '.');
    if (tail == NULL) tail = filename + strlen(filename);

    name = SCM_NEW_ATOMIC2(char *, sizeof(prefix) + tail - head);
    strcpy(name, prefix);
    for (s = head, d = name + sizeof(prefix) - 1; s < tail; s++, d++) {
        if (isalnum(*s)) *d = tolower(*s);
        else *d = '_';
    }
    *d = '\0';
    return name;
}
#endif /* HAVE_DLOPEN */

/* Dynamically load the specified object by FILENAME.
   FILENAME must not contain the system's suffix (.so, for example).
*/
ScmObj Scm_DynLoad(ScmString *filename, ScmObj initfn)
{
#ifdef HAVE_DLOPEN
    ScmObj truename, load_paths = Scm_GetDynLoadPath();
    void *handle;
    void (*func)(void);
    const char *cpath, *initname;

    filename = SCM_STRING(Scm_StringAppendC(filename, "." SHLIB_SO_SUFFIX, -1, -1));
    truename = Scm_FindFile(filename, &load_paths, TRUE);

    if (!SCM_FALSEP(Scm_Member(truename, dso_list, SCM_CMP_EQUAL)))
        return SCM_FALSE;
    
    cpath = Scm_GetStringConst(SCM_STRING(truename));
    handle = dlopen(cpath, RTLD_LAZY);
    if (handle == NULL) {
        /* TODO: check if dlerror() is available on all platforms */
        const char *err = dlerror();
        if (err == NULL) {
            Scm_Error("failed to link %S dynamically", truename);
        } else {
            Scm_Error("failed to link %S dynamically: %s", truename, err);
        }
    }
    if (SCM_STRINGP(initfn)) {
        initname = Scm_GetStringConst(SCM_STRING(initfn));
    } else {
        initname = get_dynload_initfn(cpath);
    }
    
    func = (void(*)(void))dlsym(handle, initname);
    if (func == NULL) {
        dlclose(handle);
        Scm_Error("dynamic linking of %S failed: couldn't find initialization function %s", truename, initname);
    }
    func();
    dso_list = Scm_Cons(truename, dso_list);
    return SCM_TRUE;
#else
    Scm_Error("dynamic linking is not supported on this architecture");
    return SCM_FALSE;           /* dummy */
#endif
}

/*------------------------------------------------------------------
 * Require and provide
 */

/* STk's require takes a string.  SLIB's require takes a symbol.
   For now, I allow only a string. */
/* Note that require and provide is recognized at compile time. */
/* TODO: in MT env, we should prevent the race condition that
   more than one thread requires the same (unprovided) feature
   simultaneously.  Some kind of "providing" flag, maybe. */
/* IDEA: allow require and provide an optional :version argument.
   For example, (require "foo" :version 1.2) looks for the module
   with (provide "foo" :version 1.2).  Allows mutiple versions of
   files coexist in a path.   In order to do that, the "providing"
   module needs to be compiled in a kind of sandbox environment, until
   the system confirms it has a proper version of 'provide'. */
/* TODO: related to above issue; what if an error occurs during loading
   a require file?  what we should really do is to rollback the interpreter
   state before loading that module; including cancelling any (provide)-ed
   feature in the module.  The sandbox environment mentioned above will
   help such unrolling, but it's not perfect if the toplevel expression
   in the module changes the global state.  */
ScmObj Scm_Require(ScmObj feature)
{
    if (!SCM_STRINGP(feature))
        Scm_Error("require: string expected, but got %S\n", feature);
    if (SCM_FALSEP(Scm_Member(feature, provided, SCM_CMP_EQUAL))) {
        ScmObj filename = Scm_StringAppendC(SCM_STRING(feature), ".scm", 4, 4);
        Scm_Load(Scm_GetStringConst(SCM_STRING(filename)), TRUE);
    }
    return SCM_TRUE;
}

ScmObj Scm_Provide(ScmObj feature)
{
    if (!SCM_STRINGP(feature))
        Scm_Error("provide: string expected, but got %S\n", feature);
    if (SCM_FALSEP(Scm_Member(feature, provided, SCM_CMP_EQUAL))) {
        provided = Scm_Cons(feature, provided);
    }
    return feature;
}

ScmObj Scm_ProvidedP(ScmObj feature)
{
    if (SCM_FALSEP(Scm_Member(feature, provided, SCM_CMP_EQUAL)))
        return SCM_FALSE;
    else
        return SCM_TRUE;
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
    return Scm_VMLoad(adata->path, TRUE);
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
    ScmModule *m = Scm_SchemeModule();
    ScmObj init_load_path, init_dynload_path, t;

    init_load_path = t = SCM_NIL;
    SCM_APPEND(init_load_path, t, break_env_paths("GAUCHE_LOAD_PATH"));
    SCM_APPEND1(init_load_path, t, SCM_MAKE_STR(GAUCHE_SITE_LIB_DIR));
    SCM_APPEND1(init_load_path, t, SCM_MAKE_STR(GAUCHE_LIB_DIR));

    init_dynload_path = t = SCM_NIL;
    SCM_APPEND(init_dynload_path, t, break_env_paths("GAUCHE_DYNLOAD_PATH"));
    SCM_APPEND1(init_dynload_path, t, SCM_MAKE_STR(GAUCHE_SITE_ARCH_DIR));
    SCM_APPEND1(init_dynload_path, t, SCM_MAKE_STR(GAUCHE_ARCH_DIR));
    
#define DEF(rec, sym, val) \
    rec = SCM_GLOC(Scm_Define(m, SCM_SYMBOL(sym), val))

    DEF(load_path_rec,    SCM_SYM_LOAD_PATH, init_load_path);
    DEF(dynload_path_rec, SCM_SYM_DYNAMIC_LOAD_PATH, init_dynload_path);
    DEF(load_history_rec, SCM_SYM_LOAD_HISTORY, SCM_NIL);
    DEF(load_next_rec,    SCM_SYM_LOAD_NEXT, SCM_NIL);
    DEF(load_port_rec,    SCM_SYM_LOAD_PORT, SCM_FALSE);

    provided = SCM_LIST2(SCM_MAKE_STR("srfi-6"), /* string ports (builtin) */
                         SCM_MAKE_STR("srfi-8")  /* receive (builtin) */
                         );
    dso_list = SCM_NIL;
}
