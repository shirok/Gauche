/*
 * load.c - load a program
 *
 *  Copyright(C) 2000-2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: load.c,v 1.66 2002-12-30 07:44:12 shirok Exp $
 */

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/arch.h"
#include "gauche/port.h"

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#define LOAD_SUFFIX ".scm"

/*
 * Load file.
 */

/* Static parameters */
static struct {
    /* Load path list */
    ScmGloc *load_path_rec;       /* *load-path*         */
    ScmGloc *dynload_path_rec;    /* *dynamic-load-path* */
    ScmInternalMutex path_mutex;

    /* Provided features */
    ScmObj provided;            /* List of provided features. */
    ScmObj providing;           /* Alist of features that is being loaded,
                                   and the thread that is loading it. */
    ScmObj waiting;             /* Alist of threads that is waiting for
                                   a feature to being provided, and the
                                   feature that is waited. */
    ScmInternalMutex prov_mutex;
    ScmInternalCond  prov_cv;

    /* Dynamic linking */
    ScmObj dso_list;              /* List of dynamically loaded objects. */
    ScmObj dso_loading;           /* DSO file being loaded. */
    ScmInternalMutex dso_mutex;
    ScmInternalCond dso_cv;
} ldinfo;

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
    ScmReadContext ctx;
    ScmObj prev_port;
    ScmObj prev_history;
    ScmObj prev_next;
};

/* Clean up */
static ScmObj load_after(ScmObj *args, int nargs, void *data)
{
    struct load_packet *p = (struct load_packet *)data;
    ScmVM *vm = Scm_VM();
    Scm_ClosePort(p->port);
    Scm_SelectModule(p->prev_module);
    vm->load_port = p->prev_port;
    vm->load_history = p->prev_history;
    vm->load_next = p->prev_next;
    return SCM_UNDEFINED;
}

/* C-continuation of the loading */
static ScmObj load_cc(ScmObj result, void **data)
{
    struct load_packet *p = (struct load_packet*)(data[0]);
    ScmObj expr = Scm_ReadWithContext(SCM_OBJ(p->port), &(p->ctx));

    if (!SCM_EOFP(expr)) {
        Scm_VMPushCC(load_cc, data, 1);
        return Scm_VMEval(expr, SCM_UNBOUND);
    } else {
        return SCM_TRUE;
    }
}

static ScmObj load_body(ScmObj *args, int nargs, void *data)
{
    return load_cc(SCM_NIL, &data);
}

ScmObj Scm_VMLoadFromPort(ScmPort *port, ScmObj next_paths)
{
    struct load_packet *p;
    ScmObj port_info;
    ScmVM *vm = Scm_VM();
    
    if (!SCM_IPORTP(port))
        Scm_Error("input port required, but got: %S", port);
    if (SCM_PORT_CLOSED_P(port))
        Scm_Error("port already closed: %S", port);

    p = SCM_NEW(struct load_packet);
    p->port = port;
    p->prev_module = Scm_CurrentModule();
    p->prev_port = vm->load_port;
    p->prev_history = vm->load_history;
    p->prev_next = vm->load_next;

    SCM_READ_CONTEXT_INIT(&(p->ctx));
    p->ctx.flags = SCM_READ_LITERAL_IMMUTABLE | SCM_READ_SOURCE_INFO;
    if (SCM_VM_RUNTIME_FLAG_IS_SET(vm, SCM_CASE_FOLD)) {
        p->ctx.flags |= SCM_READ_CASE_FOLD;
    }
    
    vm->load_next = next_paths;
    vm->load_port = SCM_OBJ(port);
    if (SCM_PORTP(p->prev_port)) {
        port_info = SCM_LIST2(p->prev_port,
                              Scm_MakeInteger(Scm_PortLine(SCM_PORT(p->prev_port))));
    } else {
        port_info = SCM_LIST1(SCM_FALSE);
    }
    vm->load_history = Scm_Cons(port_info, vm->load_history);
    return Scm_VMDynamicWindC(NULL, load_body, load_after, p);
}

/* Scheme subr (load-from-port subr paths) */
static ScmObj load_from_port(ScmObj *args, int argc, void *data)
{
    ScmPort *port;
    ScmObj paths = SCM_NIL, rest = args[1];
    if (!SCM_IPORTP(args[0])) {
        Scm_Error("input port required, but got %S", args[0]);
    }
    port = SCM_PORT(args[0]);
    if (SCM_PAIRP(rest)) paths = SCM_CAR(rest);
    return Scm_VMLoadFromPort(port, paths);
}

static SCM_DEFINE_STRING_CONST(load_from_port_NAME, "load-from-port", 14, 14);
static SCM_DEFINE_SUBR(load_from_port_STUB, 1, 1,
                       SCM_OBJ(&load_from_port_NAME), load_from_port,
                       NULL, NULL);

void Scm_LoadFromPort(ScmPort *port)
{
    Scm_Apply(SCM_OBJ(&load_from_port_STUB), SCM_LIST1(SCM_OBJ(port)));
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
                Scm_Warn("*load-path* contains invalid element: %S", *paths);
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

ScmObj Scm_VMLoad(ScmString *filename, ScmObj load_paths, int errorp)
{
    ScmObj port, truename;
    ScmVM *vm = Scm_VM();

    if (!SCM_PAIRP(load_paths)) load_paths = Scm_GetLoadPath();
    truename = Scm_FindFile(filename, &load_paths, errorp);
    if (SCM_FALSEP(truename)) return SCM_FALSE;
    if (SCM_VM_RUNTIME_FLAG_IS_SET(vm, SCM_LOAD_VERBOSE)) {
        int len = Scm_Length(vm->load_history);
        SCM_PUTZ(";;", 2, SCM_CURERR);
        while (len-- > 0) SCM_PUTC(' ', SCM_CURERR);
        Scm_Printf(SCM_CURERR, "Loading %A...\n", truename);
    }
    port = Scm_OpenFilePort(Scm_GetStringConst(SCM_STRING(truename)),
                            O_RDONLY, SCM_PORT_BUFFER_FULL, 0);
    if (SCM_FALSEP(port)) {
        if (errorp) Scm_Error("file %S exists, but couldn't open.", truename);
        else        return SCM_FALSE;
    }
    return Scm_VMLoadFromPort(SCM_PORT(port), load_paths);
}

/* Scheme subr (%load filename &keyword paths error-if-not-found) */
static ScmObj key_paths;
static ScmObj key_error_if_not_found;

static ScmObj load(ScmObj *args, int argc, void *data)
{
    ScmString *file;
    ScmObj paths;
    int errorp;
    if (!SCM_STRINGP(args[0])) {
        Scm_Error("string required, but got %S", args[0]);
    }
    file = SCM_STRING(args[0]);
    paths  = Scm_GetKeyword(key_paths, args[1], SCM_FALSE);
    errorp = !SCM_FALSEP(Scm_GetKeyword(key_error_if_not_found, args[1], SCM_TRUE));
    return Scm_VMLoad(file, paths, errorp);
}

static SCM_DEFINE_STRING_CONST(load_NAME, "load", 4, 4);
static SCM_DEFINE_SUBR(load_STUB, 1, 1, SCM_OBJ(&load_NAME), load, NULL, NULL);


int Scm_Load(const char *cpath, int errorp)
{
    ScmObj r, f = SCM_MAKE_STR_COPYING(cpath);
    if (errorp) {
        r = Scm_Apply(SCM_OBJ(&load_STUB), SCM_LIST1(f));
    } else {
        r = Scm_Apply(SCM_OBJ(&load_STUB),
                      SCM_LIST3(f, key_error_if_not_found, SCM_FALSE));
    }
    return !SCM_FALSEP(r);
}

/*
 * Utilities
 */

ScmObj Scm_GetLoadPath(void)
{
    ScmObj paths;
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.path_mutex);
    paths = Scm_CopyList(ldinfo.load_path_rec->value);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.path_mutex);
    return paths;
}

ScmObj Scm_GetDynLoadPath(void)
{
    ScmObj paths;
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.path_mutex);
    paths = Scm_CopyList(ldinfo.dynload_path_rec->value);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.path_mutex);
    return paths;
}

static ScmObj break_env_paths(const char *envname)
{
    const char *e = getenv(envname);
    if (geteuid() == 0) return SCM_NIL; /* don't trust env when run by root */
    if (e == NULL) return SCM_NIL;
    else return Scm_StringSplitByChar(SCM_STRING(SCM_MAKE_STR_COPYING(e)), ':');
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
    ScmObj r;
    struct stat statbuf;

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

    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.path_mutex);
    if (!SCM_PAIRP(ldinfo.load_path_rec->value)) {
        ldinfo.load_path_rec->value = SCM_LIST1(spath);
    } else if (afterp) {
        ldinfo.load_path_rec->value =
            Scm_Append2(ldinfo.load_path_rec->value, SCM_LIST1(spath));
    } else {
        ldinfo.load_path_rec->value = Scm_Cons(spath, ldinfo.load_path_rec->value);
    }
    r = ldinfo.load_path_rec->value;

    if (!SCM_PAIRP(ldinfo.dynload_path_rec->value)) {
        ldinfo.dynload_path_rec->value = SCM_LIST1(dpath);
    } else if (afterp) {
        ldinfo.dynload_path_rec->value =
            Scm_Append2(ldinfo.dynload_path_rec->value, SCM_LIST1(dpath));
    } else {
        ldinfo.dynload_path_rec->value =
            Scm_Cons(dpath, ldinfo.dynload_path_rec->value);
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.path_mutex);
    
    return r;
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
#if defined(__NetBSD__)
#define DYNLOAD_PREFIX   ___STRING(_C_LABEL(Scm_Init_))
#elif defined(__ppc__) && defined(__APPLE__) && defined(__MACH__)
/* Darwin/MacOSX */
#define DYNLOAD_PREFIX   "_Scm_Init_"
#elif defined(__FreeBSD__) && __FreeBSD__ < 3
/* FreeBSD 2.x   (patch from Abe Hiroshi) */
#define RTLD_NOW         1
#define RTLD_GLOBAL      4
#define DYNLOAD_PREFIX   "_Scm_Init_"
#else
/* We might have some other ifdefs... */
#define DYNLOAD_PREFIX   "Scm_Init_"
#endif
static const char *get_dynload_initfn(const char *filename)
{
    const char *head, *tail, *s;
    char *name, *d;
    const char prefix[] = DYNLOAD_PREFIX;
    
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
ScmObj Scm_DynLoad(ScmString *filename, ScmObj initfn, int export)
{
#ifdef HAVE_DLOPEN
    ScmObj truename, load_paths = Scm_GetDynLoadPath();
    void *handle;
    void (*func)(void);
    const char *cpath, *initname;
    int flags = RTLD_NOW;

    filename = SCM_STRING(Scm_StringAppendC(filename, "." SHLIB_SO_SUFFIX, -1, -1));
    truename = Scm_FindFile(filename, &load_paths, TRUE);

    if (!SCM_FALSEP(Scm_Member(truename, ldinfo.dso_list, SCM_CMP_EQUAL)))
        return SCM_FALSE;

    if (export) flags |= RTLD_GLOBAL;
    cpath = Scm_GetStringConst(SCM_STRING(truename));
    handle = dlopen(cpath, flags);
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
    ldinfo.dso_list = Scm_Cons(truename, ldinfo.dso_list);
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

/* [Preventing Race Condition]
 *
 *   Besides the list of provided features (ldinfo.provided), the
 *   system keeps two kind of global assoc list for transient information.
 *
 *   ldinfo.providing keeps a list of (<feature> . <thread>), where
 *   <thread> is currently loading a file for <feature>.
 *   ldinfo.waiting keeps a list of (<thread> . <feature>), where
 *   <thread> is waiting for <feature> to be provided.
 *
 *   Scm_Require first checks ldinfo.provided list; if the feature is
 *   already provided, no problem; just return.
 *   If not, ldinfo.providing is searched.  If the feature is being provided
 *   by some other thread, the calling thread pushes itself onto
 *   ldinfo.waiting list and waits for the feature to be provided.
 *
 *   There may be a case that the feature dependency forms a loop because
 *   of bug.  An error should be signaled in such a case, rather than going
 *   to deadlock.   So, when the calling thread finds the required feature
 *   is in the ldinfo.providing alist, it checks the waiting chaing of
 *   features, and no threads are waiting for a feature being provided by
 *   the calling thread.
 */

ScmObj Scm_Require(ScmObj feature)
{
    ScmObj filename;
    ScmVM *vm = Scm_VM();
    ScmObj provided, providing, p, q;
    int loop = FALSE;
    
    if (!SCM_STRINGP(feature)) {
        Scm_Error("require: string expected, but got %S\n", feature);
    }

    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.prov_mutex);
    do {
        provided = Scm_Member(feature, ldinfo.provided, SCM_CMP_EQUAL);
        if (!SCM_FALSEP(provided)) break;
        providing = Scm_Assoc(feature, ldinfo.providing, SCM_CMP_EQUAL);
        if (SCM_FALSEP(providing)) break;

        /* Checks for dependency loop */
        p = providing;
        SCM_ASSERT(SCM_PAIRP(p));
        if (SCM_CDR(p) == SCM_OBJ(vm)) {
            loop = TRUE;
            break;
        }
        
        for (;;) {
            q = Scm_Assoc(SCM_CDR(p), ldinfo.waiting, SCM_CMP_EQ);
            if (SCM_FALSEP(q)) break;
            SCM_ASSERT(SCM_PAIRP(q));
            p = Scm_Assoc(SCM_CDR(q), ldinfo.providing, SCM_CMP_EQUAL);
            SCM_ASSERT(SCM_PAIRP(p));
            if (SCM_CDR(p) == SCM_OBJ(vm)) {
                loop = TRUE;
                break;
            }
        }
        if (loop) break;
        ldinfo.waiting = Scm_Acons(SCM_OBJ(vm), feature, ldinfo.waiting);
        (void)SCM_INTERNAL_COND_WAIT(ldinfo.prov_cv, ldinfo.prov_mutex);
        ldinfo.waiting = Scm_AssocDeleteX(SCM_OBJ(vm), ldinfo.waiting, SCM_CMP_EQ);
        continue;
    } while (0);
    if (!loop && SCM_FALSEP(provided)) {
        ldinfo.providing = Scm_Acons(feature, SCM_OBJ(vm), ldinfo.providing);
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.prov_mutex);

    if (loop) Scm_Error("a loop is detected in the require dependency involving feature %S", feature);
    if (!SCM_FALSEP(provided)) return SCM_TRUE;
    SCM_UNWIND_PROTECT {
        filename = Scm_StringAppendC(SCM_STRING(feature), ".scm", 4, 4);
        Scm_Load(Scm_GetStringConst(SCM_STRING(filename)), TRUE);
    } SCM_WHEN_ERROR {
        (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.prov_mutex);
        ldinfo.providing = Scm_AssocDeleteX(feature, ldinfo.providing, SCM_CMP_EQUAL);
        (void)SCM_INTERNAL_COND_SIGNAL(ldinfo.prov_cv);
        (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.prov_mutex);
        SCM_NEXT_HANDLER;
    } SCM_END_PROTECT;
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.prov_mutex);
    ldinfo.providing = Scm_AssocDeleteX(feature, ldinfo.providing, SCM_CMP_EQUAL);
    (void)SCM_INTERNAL_COND_SIGNAL(ldinfo.prov_cv);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.prov_mutex);
    return SCM_TRUE;
}

ScmObj Scm_Provide(ScmObj feature)
{
    if (!SCM_STRINGP(feature))
        Scm_Error("provide: string expected, but got %S\n", feature);
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.prov_mutex);
    if (SCM_FALSEP(Scm_Member(feature, ldinfo.provided, SCM_CMP_EQUAL))) {
        ldinfo.provided = Scm_Cons(feature, ldinfo.provided);
    }
    if (!SCM_FALSEP(Scm_Member(feature, ldinfo.providing, SCM_CMP_EQUAL))) {
        ldinfo.providing = Scm_DeleteX(feature, ldinfo.providing, SCM_CMP_EQUAL);
    }
    (void)SCM_INTERNAL_COND_SIGNAL(ldinfo.prov_cv);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.prov_mutex);
    return feature;
}

int Scm_ProvidedP(ScmObj feature)
{
    int r;
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.prov_mutex);
    r = !SCM_FALSEP(Scm_Member(feature, ldinfo.provided, SCM_CMP_EQUAL));
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.prov_mutex);
    return r;
}

/*------------------------------------------------------------------
 * Autoload
 */

static void autoload_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    Scm_Printf(out, "#<autoload %A::%A (%A)>",
               SCM_AUTOLOAD(obj)->module->name,
               SCM_AUTOLOAD(obj)->name, SCM_AUTOLOAD(obj)->path);
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_AutoloadClass, autoload_print);

ScmObj Scm_MakeAutoload(ScmSymbol *name,
                        ScmString *path,
                        ScmSymbol *import_from)
{
    ScmAutoload *adata = SCM_NEW(ScmAutoload);
    SCM_SET_CLASS(adata, SCM_CLASS_AUTOLOAD);
    adata->name = name;
    adata->module = SCM_CURRENT_MODULE();
    adata->path = path;
    adata->import_from = import_from;
    adata->loaded = FALSE;
    return SCM_OBJ(adata);
}

ScmObj Scm_LoadAutoload(ScmAutoload *adata)
{
    if (adata->loaded) Scm_Error("Autoload is not working? %S", adata);
    adata->loaded = TRUE;
    Scm_Require(SCM_OBJ(adata->path));
    if (adata->import_from) {
        /* autoloaded file defines import_from module.  we need to
           import the binding individually. */
        ScmObj m = Scm_FindModule(adata->import_from, FALSE), fv;
        ScmGloc *f, *g;
        if (!SCM_MODULEP(m)) {
            Scm_Error("Trying to autoload module %S from file %S, but the file doesn't define such a module",
                      adata->import_from, adata->path);
        }
        f = Scm_FindBinding(SCM_MODULE(m), adata->name, FALSE);
        g = Scm_FindBinding(adata->module, adata->name, FALSE);
        SCM_ASSERT(f != NULL);
        SCM_ASSERT(g != NULL);
        fv = SCM_GLOC_GET(f);
        if (SCM_UNBOUNDP(fv) || SCM_AUTOLOADP(fv)) {
            Scm_Error("Autoloaded symbol %S is not defined in the module %S",
                      adata->name, adata->import_from);
        }
        SCM_GLOC_SET(g, fv);
        return fv;
    } else {
        /* Normal import.  The binding must have been inserted to
           adata->module */
        ScmObj gv;
        ScmGloc *g = Scm_FindBinding(adata->module, adata->name, FALSE);
        SCM_ASSERT(g != NULL);
        gv = SCM_GLOC_GET(g);
        if (SCM_UNBOUNDP(gv) || SCM_AUTOLOADP(gv)) {
            Scm_Error("Autoloaded symbol %S is not defined in the file %S",
                      adata->name, adata->path);
        }
        return gv;
    }
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

    (void)SCM_INTERNAL_MUTEX_INIT(ldinfo.path_mutex);
    (void)SCM_INTERNAL_MUTEX_INIT(ldinfo.prov_mutex);
    (void)SCM_INTERNAL_COND_INIT(ldinfo.prov_cv);
    (void)SCM_INTERNAL_MUTEX_INIT(ldinfo.dso_mutex);
    (void)SCM_INTERNAL_COND_INIT(ldinfo.dso_cv);

    key_paths = SCM_MAKE_KEYWORD("paths");
    key_error_if_not_found = SCM_MAKE_KEYWORD("error-if-not-found");
    
    SCM_DEFINE(m, "load-from-port", SCM_OBJ(&load_from_port_STUB));
    SCM_DEFINE(m, "load", SCM_OBJ(&load_STUB));

#define DEF(rec, sym, val) \
    rec = SCM_GLOC(Scm_Define(m, SCM_SYMBOL(sym), val))

    DEF(ldinfo.load_path_rec,    SCM_SYM_LOAD_PATH, init_load_path);
    DEF(ldinfo.dynload_path_rec, SCM_SYM_DYNAMIC_LOAD_PATH, init_dynload_path);

    ldinfo.provided = SCM_LIST4(SCM_MAKE_STR("srfi-6"), /* string ports (builtin) */
                                SCM_MAKE_STR("srfi-8"), /* receive (builtin) */
                                SCM_MAKE_STR("srfi-10"), /* #, (builtin) */
                                SCM_MAKE_STR("srfi-17")  /* set! (builtin) */
        );
    ldinfo.providing = SCM_NIL;
    ldinfo.waiting = SCM_NIL;
    ldinfo.dso_list = SCM_NIL;
    ldinfo.dso_loading = SCM_FALSE;
}
