/*
 * load.c - load a program
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"
#include "gauche/port.h"
#include "gauche/priv/builtin-syms.h"
#include "gauche/priv/readerP.h"
#include "gauche/priv/portP.h"

#include <ctype.h>
#include <fcntl.h>

/*
 * Load file.
 */

/* Static parameters */
static struct {
    /* Load path list */
    ScmGloc *load_path_rec;      /* *load-path*         */
    ScmGloc *dynload_path_rec;   /* *dynamic-load-path* */
    ScmGloc *load_suffixes_rec;  /* *load-suffixes*     */
    ScmGloc *load_path_hooks_rec; /* *load-path-hooks*   */
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

    /* Dynamic enviornments kept during specific `load'.  They are
       thread-specific, and we use ScmParameter mechanism. */
    ScmParameterLoc load_history;      /* history of the nested load */
    ScmParameterLoc load_next;         /* list of the directories to be
                                          searched. */
    ScmParameterLoc load_port;         /* current port from which we are
                                          loading */
    
    /* Dynamic linking */
    ScmObj dso_suffixes;
    ScmObj dso_list;              /* List of dynamically loaded objects. */
    ScmObj dso_prelinked;         /* List of 'prelinked' DSOs, that is, they
                                     are already linked but pretened to be
                                     DSOs.  dynamic-load won't do anything.
                                     NB: We assume initfns of prelinked DSOs
                                     are already called by the application,
                                     but we may change this design in future.
                                  */
    ScmInternalMutex dso_mutex;
} ldinfo = { (ScmGloc*)&ldinfo, };  /* trick to put ldinfo in .data section */

/* keywords used for load and load-from-port surbs */
static ScmObj key_error_if_not_found = SCM_UNBOUND;
static ScmObj key_macro              = SCM_UNBOUND;
static ScmObj key_ignore_coding      = SCM_UNBOUND;
static ScmObj key_paths              = SCM_UNBOUND;
static ScmObj key_environment        = SCM_UNBOUND;

#define PARAM_REF(vm, loc)      Scm_ParameterRef(vm, &ldinfo.loc)

/*
 * ScmLoadPacket is the way to communicate to Scm_Load facility.
 */

/* small utility.  initializes OUT fields of the load packet. */
static void load_packet_prepare(ScmLoadPacket *packet)
{
    if (packet) {
        packet->exception = SCM_FALSE;
        packet->loaded = FALSE;
    }
}

/* for applications to initialize ScmLoadPacket before passing it to
   Scm_Load or Scm_LoadFromPort.   As of 0.9, ScmLoadPacket only has
   fields to be filled by those APIs, so applications don't need to
   initialize it explicitly.  However, it is possible in future that
   we add some fields to pass info from applications to APIs, in which
   case it is necessary for this function to set appropriate initial
   values for such fields. */
void Scm_LoadPacketInit(ScmLoadPacket *p)
{
    load_packet_prepare(p);
}

/*--------------------------------------------------------------------
 * Scm_LoadFromPort
 *
 *   The most basic function in the load()-family.  Read an expression
 *   from the given port and evaluates it repeatedly, until it reaches
 *   EOF.  Then the port is closed.   The port is locked by the calling
 *   thread until the operation terminates.
 *
 *   The result of the last evaluation remains on VM.
 *
 *   No matter how the load terminates, either normal or abnormal,
 *   the port is closed, and the current module is restored to the
 *   one when load is called.
 *
 *   FLAGS argument is ignored for now, but reserved for future
 *   extension.  SCM_LOAD_QUIET_NOFILE and SCM_LOAD_IGNORE_CODING
 *   won't have any effect for LoadFromPort; see Scm_Load below.
 *
 *   TODO: if we're using coding-aware port, how should we propagate
 *   locking into the wrapped (original) port?
 */

int Scm_LoadFromPort(ScmPort *port, u_long flags, ScmLoadPacket *packet)
{
    static ScmObj load_from_port = SCM_UNDEFINED;
    ScmObj args = SCM_NIL;
    SCM_BIND_PROC(load_from_port, "load-from-port", Scm_GaucheModule());
    load_packet_prepare(packet);

    args = Scm_Cons(SCM_OBJ(port), args);

    if (flags&SCM_LOAD_PROPAGATE_ERROR) {
        Scm_ApplyRec(load_from_port, args);
        if (packet) packet->loaded = TRUE;
        return 0;
    } else {
        ScmEvalPacket eresult;
        int r = Scm_Apply(load_from_port, args, &eresult);
        if (packet) {
            packet->exception = eresult.exception;
            packet->loaded = (r >= 0);
        }
        return (r < 0)? -1 : 0;
    }
}

/*---------------------------------------------------------------------
 * Scm_Load
 * Scm_VMLoad
 *
 *  Scheme's load().
 *
 *  filename   - name of the file.  can be sans suffix.
 *  load_paths - list of pathnames or #f.   If #f, system's load path
 *               is used.
 *  env        - a module where the forms are evaluated, or #f.
 *               If #f, the current module is used.
 *  flags      - combination of ScmLoadFlags.
 */

/* The real `load' function is moved to Scheme.  This is a C stub to
   call it. */
ScmObj Scm_VMLoad(ScmString *filename, ScmObj paths, ScmObj env, int flags)
{
    ScmObj opts = SCM_NIL;
    static ScmObj load_proc = SCM_UNDEFINED;
    SCM_BIND_PROC(load_proc, "load", Scm_SchemeModule());

    if (flags&SCM_LOAD_QUIET_NOFILE) {
        opts = Scm_Cons(key_error_if_not_found, Scm_Cons(SCM_FALSE, opts));
    }
    if (flags&SCM_LOAD_IGNORE_CODING) {
        opts = Scm_Cons(key_ignore_coding, Scm_Cons(SCM_TRUE, opts));
    }
    if (SCM_NULLP(paths) || SCM_PAIRP(paths)) {
        opts = Scm_Cons(key_paths, Scm_Cons(paths, opts));
    }
    if (!SCM_FALSEP(env)) {
        opts = Scm_Cons(key_environment, Scm_Cons(env, opts));
    }
    return Scm_VMApply(load_proc, Scm_Cons(SCM_OBJ(filename), opts));
}

int Scm_Load(const char *cpath, u_long flags, ScmLoadPacket *packet)
{
    static ScmObj load_proc = SCM_UNDEFINED;
    ScmObj f = SCM_MAKE_STR_COPYING(cpath);
    ScmObj opts = SCM_NIL;
    SCM_BIND_PROC(load_proc, "load", Scm_SchemeModule());

    if (flags&SCM_LOAD_QUIET_NOFILE) {
        opts = Scm_Cons(key_error_if_not_found, Scm_Cons(SCM_FALSE, opts));
    }
    if (flags&SCM_LOAD_IGNORE_CODING) {
        opts = Scm_Cons(key_ignore_coding, Scm_Cons(SCM_TRUE, opts));
    }

    load_packet_prepare(packet);
    if (flags&SCM_LOAD_PROPAGATE_ERROR) {
        ScmObj r = Scm_ApplyRec(load_proc, Scm_Cons(f, opts));
        if (packet) {
            packet->loaded = !SCM_FALSEP(r);
        }
        return 0;
    } else {
        ScmEvalPacket eresult;
        int r = Scm_Apply(load_proc, Scm_Cons(f, opts), &eresult);
        if (packet) {
            packet->exception = eresult.exception;
            packet->loaded = (r > 0 && !SCM_FALSEP(eresult.results[0]));
        }
        return (r >= 0)? 0 : -1;
    }
}

/* A convenience routine */
int Scm_LoadFromCString(const char *program, u_long flags, ScmLoadPacket *p)
{
    ScmObj ip = Scm_MakeInputStringPort(SCM_STRING(SCM_MAKE_STR(program)), TRUE);
    return Scm_LoadFromPort(SCM_PORT(ip), flags, p);
}


/*
 * Utilities
 */

ScmObj Scm_GetLoadPath(void)
{
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.path_mutex);
    ScmObj paths = Scm_CopyList(ldinfo.load_path_rec->value);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.path_mutex);
    return paths;
}

ScmObj Scm_GetDynLoadPath(void)
{
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.path_mutex);
    ScmObj paths = Scm_CopyList(ldinfo.dynload_path_rec->value);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.path_mutex);
    return paths;
}

static ScmObj break_env_paths(const char *envname)
{
    const char *e = Scm_GetEnv(envname);
#ifndef GAUCHE_WINDOWS
    char delim = ':';
#else  /*GAUCHE_WINDOWS*/
    char delim = ';';
#endif /*GAUCHE_WINDOWS*/

    if (e == NULL || strlen(e) == 0) {
        return SCM_NIL;
    } else if (Scm_IsSugid()) {
        /* don't trust env when setugid'd */
        return SCM_NIL;
    } else {
        return Scm_StringSplitByChar(SCM_STRING(SCM_MAKE_STR_COPYING(e)),
                                     delim);
    }
}

static ScmObj add_list_item(ScmObj orig, ScmObj item, int afterp)
{
    if (afterp) {
        return Scm_Append2(orig, SCM_LIST1(item));
    } else {
        return Scm_Cons(item, orig);
    }
}
#define ADD_LIST_ITEM(list, item, afterp) \
    list = add_list_item(list, item, afterp)

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
    struct stat statbuf;

    /* check dynload path */
    ScmObj dpath = Scm_StringAppendC(SCM_STRING(spath), "/", 1, 1);
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
    ADD_LIST_ITEM(ldinfo.load_path_rec->value, spath, afterp);
    ADD_LIST_ITEM(ldinfo.dynload_path_rec->value, dpath, afterp);
    ScmObj r = ldinfo.load_path_rec->value;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.path_mutex);

    return r;
}

void Scm_AddLoadPathHook(ScmObj proc, int afterp)
{
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.path_mutex);
    ADD_LIST_ITEM(ldinfo.load_path_hooks_rec->value, proc, afterp);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.path_mutex);
}

void Scm_DeleteLoadPathHook(ScmObj proc)
{
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.path_mutex);
    /* we should use Scm_Delete, instead of Scm_DeleteX,
       to avoid race with reader of the list */
    ldinfo.load_path_hooks_rec->value
        = Scm_Delete(proc, ldinfo.load_path_hooks_rec->value, SCM_CMP_EQ);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.path_mutex);
}

/*------------------------------------------------------------------
 * Dynamic linking
 */

/* The API to load object file dynamically differ among platforms.
 * We include the platform-dependent implementations (dl_*.c) that
 * provides a common API:
 *
 *   void *dl_open(const char *pathname)
 *     Dynamically loads the object file specified by PATHNAME,
 *     and returns its handle.   On failure, returns NULL.
 *
 *     PATHNAME is guaranteed to contain directory names, so this function
 *     doesn't need to look it up in the search paths.
 *     The caller also checks whether pathname is already loaded or not,
 *     so this function doesn't need to worry about duplicate loads.
 *     This function should have the semantics equivalent to the
 *     RTLD_NOW|RTLD_GLOBAL of dlopen().
 *
 *     We don't call with NULL as PATHNAME; dlopen() returns the handle
 *     of the calling program itself in such a case, but we never need that
 *     behavior.
 *
 *   ScmDynloadInitFn dl_sym(void *handle, const char *symbol)
 *     Finds the address of SYMBOL in the dl_openModule()-ed module
 *     HANDLE.
 *
 *   void dl_close(void *handle)
 *     Closes the opened module.  This can only be called when we couldn't
 *     find the initialization function in the module; once the initialization
 *     function is called, we don't have a safe way to remove the module.
 *
 *   const char *dl_error(void)
 *     Returns the last error occurred on HANDLE in the dl_* function.
 *
 * Notes:
 *   - The caller must take care of mutex so that dl_ won't be called from
 *     more than one thread at a time, and no other thread calls
 *     dl_* functions between dl_open and dl_error (so that dl_open
 *     can store the error info in global variable).
 *
 * Since this API assumes the caller does a lot of work, the implementation
 * should be much simpler than implementing fully dlopen()-compatible
 * functions.
 */

/* The implementation of dynamic loader is a bit complicated in the presense
   of multiple threads and multiple initialization routines.

   We keep struct dlobj_rec record for each DYNAMIC-LOADed files (keyed
   by pathname including suffix) to track the state of loading.  The thread
   must lock the structure first to operate on the particluar DSO.

   By default, a DSO has one initialization function (initfn) whose name
   can be derived from DSO's basename (if DSO is /foo/bar/baz.so, the
   initfn is Scm_Init_baz).  DSO may have more than one initfn, if it is
   made from multiple Scheme files via precompiler; in which case, each
   initfn initializes a part of DSO corresponding to a Scheme module.
   Each *.sci file contains dynamic-load form of the DSO with :init-function
   keyword arguments.
 */

typedef void (*ScmDynLoadInitFn)(void);

typedef struct dlobj_initfn_rec {
    struct dlobj_initfn_rec *next; /* chain */
    const char *name;           /* name of initfn (always w/ leading '_') */
    ScmDynLoadInitFn fn;        /* function ptr */
    int initialized;            /* TRUE once fn returns */
} dlobj_initfn;

struct ScmDLObjRec {
    SCM_HEADER;
    const char *path;           /* pathname for DSO, including suffix */
    int loaded;                 /* TRUE if this DSO is already loaded.
                                   It may need to be initialized, though.
                                   Check initfns.  */
    void *handle;               /* whatever dl_open returned */
    ScmVM *loader;              /* The VM that's holding the lock to operate
                                   on this DLO. */
    dlobj_initfn *initfns;      /* list of initializer functions */
    ScmInternalMutex mutex;
    ScmInternalCond  cv;
};

static void dlobj_print(ScmObj obj, ScmPort *sink, ScmWriteContext *mode)
{
    Scm_Printf(sink, "#<dlobj \"%s\">", SCM_DLOBJ(obj)->path);
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_DLObjClass, dlobj_print);

static ScmDLObj *make_dlobj(const char *path)
{
    ScmDLObj *z = SCM_NEW(ScmDLObj);
    SCM_SET_CLASS(z, &Scm_DLObjClass);
    z->path = path;
    z->loader = NULL;
    z->loaded = FALSE;
    z->initfns = NULL;
    (void)SCM_INTERNAL_MUTEX_INIT(z->mutex);
    (void)SCM_INTERNAL_COND_INIT(z->cv);
    return z;
}

/* NB: we rely on dlcompat library for dlopen instead of using dl_darwin.c
   for now; Boehm GC requires dlopen when compiled with pthread, so there's
   not much point to avoid dlopen here. */
#if defined(HAVE_DLOPEN)
#include "dl_dlopen.c"
#elif defined(GAUCHE_WINDOWS)
#include "dl_win.c"
#else
#include "dl_dummy.c"
#endif

/* Find dlobj with path, creating one if there aren't, and returns it. */
static ScmDLObj *find_dlobj(const char *path)
{
    ScmDLObj *z = NULL;
    ScmObj p;
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.dso_mutex);
    SCM_FOR_EACH(p, ldinfo.dso_list) {
        ScmDLObj *d = SCM_DLOBJ(SCM_CAR(p));
        SCM_ASSERT(SCM_DLOBJP(d));
        if (strcmp(d->path, path) == 0) {
            z = d;
            break;
        }
    }
    if (z == NULL) {
        z = make_dlobj(path);
        ldinfo.dso_list = Scm_Cons(SCM_OBJ(z), ldinfo.dso_list);
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.dso_mutex);
    return z;
}

static void lock_dlobj(ScmDLObj *dlo)
{
    ScmVM *vm = Scm_VM();
    (void)SCM_INTERNAL_MUTEX_LOCK(dlo->mutex);
    while (dlo->loader != vm) {
        if (dlo->loader == NULL) break;
        (void)SCM_INTERNAL_COND_WAIT(dlo->cv, dlo->mutex);
    }
    dlo->loader = vm;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(dlo->mutex);
}

static void unlock_dlobj(ScmDLObj *dlo)
{
    (void)SCM_INTERNAL_MUTEX_LOCK(dlo->mutex);
    dlo->loader = NULL;
    (void)SCM_INTERNAL_COND_BROADCAST(dlo->cv);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(dlo->mutex);
}

/* Derives initialization function name from the module file name.
   This function _always_ appends underscore before the symbol.
   The dynamic loader first tries the symbol without underscore,
   then tries with underscore. */
#define DYNLOAD_PREFIX   "_Scm_Init_"

static const char *derive_dynload_initfn(const char *filename)
{
    const char *head = strrchr(filename, '/');
    if (head == NULL) head = filename;
    else head++;
    const char *tail = strchr(head, '.');
    if (tail == NULL) tail = filename + strlen(filename);

    char *name = SCM_NEW_ATOMIC2(char *, sizeof(DYNLOAD_PREFIX) + tail - head);
    strcpy(name, DYNLOAD_PREFIX);
    char *d = name + sizeof(DYNLOAD_PREFIX) - 1;
    for (const char *s = head; s < tail; s++, d++) {
        if (isalnum(*s)) *d = tolower(*s);
        else *d = '_';
    }
    *d = '\0';
    return name;
}

/* find dlobj_initfn from the given dlobj with name.
   Assuming the caller holding the lock of OBJ. */
static dlobj_initfn *find_initfn(ScmDLObj *dlo, const char *name)
{
    dlobj_initfn *fns = dlo->initfns;
    for (; fns != NULL; fns = fns->next) {
        if (strcmp(name, fns->name) == 0) return fns;
    }
    fns = SCM_NEW(dlobj_initfn);
    fns->name = name;
    fns->fn = NULL;
    fns->initialized = FALSE;
    fns->next = dlo->initfns;
    dlo->initfns = fns;
    return fns;
}

/* From the given DSO name, find out the path of the actual DSO */
static const char *find_dso_path(ScmString *dsoname)
{
    static ScmObj find_file = SCM_UNDEFINED;
    SCM_BIND_PROC(find_file, "find-load-file", Scm_GaucheInternalModule());

    ScmObj spath = Scm_ApplyRec5(find_file, SCM_OBJ(dsoname), Scm_GetDynLoadPath(),
                                 ldinfo.dso_suffixes, SCM_FALSE, SCM_FALSE);
    if (SCM_FALSEP(spath)) {
        Scm_Error("can't find dlopen-able module %S", dsoname);
    }
    SCM_ASSERT(SCM_STRINGP(SCM_CAR(spath)));
    return Scm_GetStringConst(SCM_STRING(SCM_CAR(spath)));
}

/* Obtain the initializer function name (with '_' prepended) */
const char *get_initfn_name(ScmObj initfn, const char *dsopath)
{
    if (SCM_STRINGP(initfn)) {
        ScmObj _initfn =
            Scm_StringAppend2(SCM_STRING(Scm_MakeString("_", 1, 1, 0)),
                              SCM_STRING(initfn));
        return Scm_GetStringConst(SCM_STRING(_initfn));
    } else {
        return derive_dynload_initfn(dsopath);
    }
}

/* Load the DSO.  The caller holds the lock of dlobj.  May throw an error;
   the caller makes sure it releases the lock even in that case. */
static void load_dlo(ScmDLObj *dlo)
{
    ScmVM *vm = Scm_VM();
    if (SCM_VM_RUNTIME_FLAG_IS_SET(vm, SCM_LOAD_VERBOSE)) {
        int len = Scm_Length(PARAM_REF(vm, load_history));
        SCM_PUTZ(";;", 2, SCM_CURERR);
        while (len-- > 0) Scm_Putz("  ", 2, SCM_CURERR);
        Scm_Printf(SCM_CURERR, "Dynamically Loading %s...\n", dlo->path);
    }
    dlo->handle = dl_open(dlo->path);
    if (dlo->handle == NULL) {
        const char *err = dl_error();
        if (err == NULL) {
            Scm_Error("failed to link %s dynamically", dlo->path);
        } else {
            Scm_Error("failed to link %s dynamically: %s", dlo->path, err);
        }
        /*NOTREACHED*/
    }
    dlo->loaded = TRUE;
}

/* Call the DSO's initfn.  The caller holds the lock of dlobj, and responsible
   to release the lock even when this fn throws an error. */
static void call_initfn(ScmDLObj *dlo, const char *name)
{
    dlobj_initfn *ifn = find_initfn(dlo, name);

    if (ifn->initialized) return;

    if (!ifn->fn) {
        /* locate initfn.  Name always has '_'.  Whether the actual
           symbol dl_sym returns has '_' or not depends on the platform,
           so we first try without '_', then '_'. */
        ifn->fn = dl_sym(dlo->handle, name+1);
        if (ifn->fn == NULL) {
            ifn->fn = (void(*)(void))dl_sym(dlo->handle, name);
            if (ifn->fn == NULL) {
                dl_close(dlo->handle);
                dlo->handle = NULL;
                Scm_Error("dynamic linking of %s failed: "
                          "couldn't find initialization function %s",
                          dlo->path, name);
                /*NOTREACHED*/
            }
        }
    }

    /* Call initialization function.  note that there can be arbitrary
       complex stuff done within func(), including evaluation of
       Scheme procedures and/or calling dynamic-load for other
       object.  There's a chance that, with some contrived case,
       func() can trigger the dynamic loading of the same file we're
       loading right now.  However, if the code follows the Gauche's
       standard module structure, such circular dependency is detected
       by Scm_Load, so we don't worry about it here. */
    ifn->fn();
    ifn->initialized = TRUE;
}

/* Experimental: Prelink feature---we allow the extension module to be
   statically linked, and (dynamic-load DSONAME) merely calls initfn.
   The application needs to call Scm_RegisterPrelinked to tell the system
   which DSO is statically linked.  We pretend that the named DSO is
   already loaded from a pseudo pathname "@/DSONAME" (e.g. for
   "gauche--collection", we use "@/gauche--collection".) */

static const char *pseudo_pathname_for_prelinked(ScmString *dsoname)
{
    ScmDString ds;
    Scm_DStringInit(&ds);
    Scm_DStringPutz(&ds, "@/", 2);
    Scm_DStringAdd(&ds, dsoname);
    return Scm_DStringGetz(&ds);
}

/* Register DSONAME as prelinked.  DSONAME shoudn't have system's suffix.
   INITFNS is an array of function pointers, NULL terminated.
   INITFN_NAMES should have prefixed with '_', for call_initfn() searches
   names with '_' first. */
void Scm_RegisterPrelinked(ScmString *dsoname,
                           const char *initfn_names[],
                           ScmDynLoadInitFn initfns[])
{
    const char *path = pseudo_pathname_for_prelinked(dsoname);
    ScmDLObj *dlo = find_dlobj(path);
    dlo->loaded = TRUE;

    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.dso_mutex);
    for (int i=0; initfns[i] && initfn_names[i]; i++) {
        dlobj_initfn *ifn = find_initfn(dlo, initfn_names[i]);
        SCM_ASSERT(ifn->fn == NULL);
        ifn->fn = initfns[i];
    }
    ldinfo.dso_prelinked = Scm_Cons(SCM_OBJ(dsoname), ldinfo.dso_prelinked);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.dso_mutex);
}

static const char *find_prelinked(ScmString *dsoname)
{
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.dso_mutex);
    /* in general it is dangerous to invoke equal?-comparison during lock,
       but in this case we know they're string comparison and won't raise
       an error. */
    ScmObj z = Scm_Member(SCM_OBJ(dsoname), ldinfo.dso_prelinked, SCM_CMP_EQUAL);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.dso_mutex);
    return SCM_FALSEP(z) ? NULL : pseudo_pathname_for_prelinked(dsoname);
}

/* Dynamically load the specified object by DSONAME.
   DSONAME must not contain the system's suffix (.so, for example).
   The same name of DSO can be only loaded once.
   A DSO may contain multiple initialization functions (initfns), in
   which case each initfn is called at most once.
*/
ScmObj Scm_DynLoad(ScmString *dsoname, ScmObj initfn, u_long flags/*reserved*/)
{
    const char *dsopath = find_prelinked(dsoname);
    if (!dsopath) dsopath = find_dso_path(dsoname);
    const char *initname = get_initfn_name(initfn, dsopath);
    ScmDLObj *dlo = find_dlobj(dsopath);

    /* Load the dlobj if necessary. */
    lock_dlobj(dlo);
    if (!dlo->loaded) {
        SCM_UNWIND_PROTECT { load_dlo(dlo); }
        SCM_WHEN_ERROR { unlock_dlobj(dlo); SCM_NEXT_HANDLER; }
        SCM_END_PROTECT;
    }

    /* Now the dlo is loaded.  We need to call initializer. */
    SCM_ASSERT(dlo->loaded);

    SCM_UNWIND_PROTECT { call_initfn(dlo, initname); }
    SCM_WHEN_ERROR { unlock_dlobj(dlo);  SCM_NEXT_HANDLER; }
    SCM_END_PROTECT;

    unlock_dlobj(dlo);
    return SCM_TRUE;
}

/* Expose dlobj to Scheme world */

static ScmObj dlobj_path_get(ScmObj obj)
{
    return SCM_MAKE_STR_IMMUTABLE(SCM_DLOBJ(obj)->path);
}

static ScmObj dlobj_loaded_get(ScmObj obj)
{
    return SCM_MAKE_BOOL(SCM_DLOBJ(obj)->loaded);
}

static ScmObj dlobj_initfns_get(ScmObj obj)
{
    ScmObj h = SCM_NIL;
    ScmObj t = SCM_NIL;
    lock_dlobj(SCM_DLOBJ(obj));
    dlobj_initfn *ifn = SCM_DLOBJ(obj)->initfns;
    for (;ifn != NULL; ifn = ifn->next) {
        ScmObj p = Scm_Cons(SCM_MAKE_STR_IMMUTABLE(ifn->name),
                            SCM_MAKE_BOOL(ifn->initialized));
        SCM_APPEND1(h, t, p);
    }
    unlock_dlobj(SCM_DLOBJ(obj));
    return h;
}

static ScmClassStaticSlotSpec dlobj_slots[] = {
    SCM_CLASS_SLOT_SPEC("path", dlobj_path_get, NULL),
    SCM_CLASS_SLOT_SPEC("loaded?", dlobj_loaded_get, NULL),
    SCM_CLASS_SLOT_SPEC("init-functions", dlobj_initfns_get, NULL),
    SCM_CLASS_SLOT_SPEC_END()
};

ScmObj Scm_DLObjs()
{
    ScmObj z = SCM_NIL;
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.dso_mutex);
    z = Scm_CopyList(ldinfo.dso_list);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.dso_mutex);
    return z;
}

/*------------------------------------------------------------------
 * Require and provide
 */

/* STk's require takes a string.  SLIB's require takes a symbol.
   For now, I allow only a string. */
/* Note that require and provide is recognized at compile time. */

static int do_require(ScmObj, int, ScmModule *, ScmLoadPacket *);

/* [Preventing Race Condition]
 *
 *   Besides the list of provided features (ldinfo.provided), the
 *   system keeps two kind of global assoc list for transient information.
 *
 *   ldinfo.providing keeps a list of (<feature> <thread> <provided> ...),
 *   where <thread> is currently loading a file for <feature>.
 *   ldinfo.waiting keeps a list of (<thread> . <feature>), where
 *   <thread> is waiting for <feature> to be provided.
 *   (The <provided> list is pushed by 'provide' while loading <feature>.
 *   It is used for autprovide feature.  See below).
 *
 *   Scm_Require first checks ldinfo.provided list; if the feature is
 *   already provided, no problem; just return.
 *   If not, ldinfo.providing is searched.  If the feature is being provided
 *   by some other thread, the calling thread pushes itself onto
 *   ldinfo.waiting list and waits for the feature to be provided.
 *
 *   There may be a case that the feature dependency forms a loop because
 *   of a bug.  An error should be signaled in such a case, rather than going
 *   to deadlock.   So, when the calling thread finds the required feature
 *   is in the ldinfo.providing alist, it checks the waiting chain of
 *   features, and no threads are waiting for a feature being provided by
 *   the calling thread.
 *
 *   When the above checks are all false, the calling thread is responsible
 *   to load the required feature.  It pushes the feature and itself
 *   onto the providing list and start loading the file.
 *
 * [Autoprovide Feature]
 *
 *   When a file is loaded via 'require', it almost always provides the
 *   required feature.  Thus we allow the file to omit the 'provide' form.
 *   That is, if a file X.scm is loaded because of (require "X"), and
 *   there's no 'provide' form in X.scm, the feature "X" is automatically
 *   provided upon a successful loading of X.scm.
 *
 *   If a 'provide' form appears in X.scm, the autoprovide feature is
 *   turned off.  It is allowed that X.scm provides features other than
 *   "X".   As a special case, (provide #f) causes the autoprovide feature
 *   to be turned of without providing any feature.
 *
 *   To track what is provided, the 'provide' form pushes its argument
 *   to the entry of 'providing' list whose thread matches the calling
 *   thread.  (It is possible that there's more than one entry in the
 *   'providing' list, for a required file may call another require form.
 *   The entry is always pushed at the beginning of the providing list,
 *   we know that the first matching entry is the current one.)
 */

/* NB: It has never been explicit, but 'require' and 'extend' are expected to
   work as if we load the module into #<module gauche>.  Those forms only loads
   the file once, so it doesn't make much sense to allow it to load into
   different modules for each time, since you never know whether the file
   is loaded at this time or it has already been loaded.  With the same
   reason, it doesn't make much sense to use the current module.

   Until 0.9.4 we didn't actually force 'require' to use #<module gauche>
   as the base module.  Most of the time, 'require' is called in a module
   that inherits gauche, so the problem didn't appear.  However, if one
   requires a file from a module that doesn't inherit gauche, most likely
   accidentally, he sees confusing errors---because "define-module" form
   isn't recognized!

   As of 0.9.4 we fix the base module to #<module gauche> when require is
   called.   Note that autoload needs different requirements, so we have
   a subroutine do_require that takes the desired base module.
 */
int Scm_Require(ScmObj feature, int flags, ScmLoadPacket *packet)
{
    return do_require(feature, flags, Scm_GaucheModule(), packet);
}

int do_require(ScmObj feature, int flags, ScmModule *base_mod,
               ScmLoadPacket *packet)
{
    ScmVM *vm = Scm_VM();
    ScmObj provided;
    int loop = FALSE;

    load_packet_prepare(packet);
    if (!SCM_STRINGP(feature)) {
        ScmObj e = Scm_MakeError(Scm_Sprintf("require: string expected, but got %S\n", feature));
        if (flags&SCM_LOAD_PROPAGATE_ERROR) Scm_Raise(e);
        else {
            if (packet) packet->exception = e;
            return -1;
        }
    }

    /* Check provided, providing and waiting list.  See the comment above. */
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.prov_mutex);
    for (;;) {
        provided = Scm_Member(feature, ldinfo.provided, SCM_CMP_EQUAL);
        if (!SCM_FALSEP(provided)) break;
        ScmObj providing = Scm_Assoc(feature, ldinfo.providing, SCM_CMP_EQUAL);
        if (SCM_FALSEP(providing)) break;

        /* Checks for dependencies */
        ScmObj p = providing;
        SCM_ASSERT(SCM_PAIRP(p) && SCM_PAIRP(SCM_CDR(p)));
        if (SCM_CADR(p) == SCM_OBJ(vm)) { loop = TRUE; break; }

        for (;;) {
            ScmObj q = Scm_Assq(SCM_CDR(p), ldinfo.waiting);
            if (SCM_FALSEP(q)) break;
            SCM_ASSERT(SCM_PAIRP(q));
            p = Scm_Assoc(SCM_CDR(q), ldinfo.providing, SCM_CMP_EQUAL);
            SCM_ASSERT(SCM_PAIRP(p) && SCM_PAIRP(SCM_CDR(p)));
            if (SCM_CADR(p) == SCM_OBJ(vm)) { loop = TRUE; break; }
        }
        if (loop) break;
        ldinfo.waiting = Scm_Acons(SCM_OBJ(vm), feature, ldinfo.waiting);
        (void)SCM_INTERNAL_COND_WAIT(ldinfo.prov_cv, ldinfo.prov_mutex);
        ldinfo.waiting = Scm_AssocDeleteX(SCM_OBJ(vm), ldinfo.waiting, SCM_CMP_EQ);
    }
    if (!loop && SCM_FALSEP(provided)) {
        ldinfo.providing =
            Scm_Acons(feature, SCM_LIST1(SCM_OBJ(vm)), ldinfo.providing);
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.prov_mutex);

    if (loop) {
        ScmObj e = Scm_MakeError(Scm_Sprintf("a loop is detected in the require dependency involving feature %S", feature));
        if (flags&SCM_LOAD_PROPAGATE_ERROR) Scm_Raise(e);
        else {
            if (packet) packet->exception = e;
            return -1;
        }
    }

    if (!SCM_FALSEP(provided)) return 0; /* no work to do */
    /* Make sure to load the file into base_mod.   We don't need UNWIND_PROTECT
       here, since errors are caught in Scm_Load. */
    ScmLoadPacket xresult;
    ScmModule *prev_mod = vm->module;
    vm->module = base_mod;
    int r = Scm_Load(Scm_GetStringConst(SCM_STRING(feature)), 0, &xresult);
    vm->module = prev_mod;
    if (packet) packet->exception = xresult.exception;

    if (r < 0) {
        /* Load failed */
        (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.prov_mutex);
        ldinfo.providing = Scm_AssocDeleteX(feature, ldinfo.providing, SCM_CMP_EQUAL);
        (void)SCM_INTERNAL_COND_BROADCAST(ldinfo.prov_cv);
        (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.prov_mutex);
        if (flags&SCM_LOAD_PROPAGATE_ERROR) Scm_Raise(xresult.exception);
        else return -1;
    }

    /* Success */
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.prov_mutex);
    ScmObj p = Scm_Assoc(feature, ldinfo.providing, SCM_CMP_EQUAL);
    ldinfo.providing = Scm_AssocDeleteX(feature, ldinfo.providing, SCM_CMP_EQUAL);
    /* `Autoprovide' feature */
    if (SCM_NULLP(SCM_CDDR(p))
        && SCM_FALSEP(Scm_Member(feature, ldinfo.provided, SCM_CMP_EQUAL))) {
        ldinfo.provided = Scm_Cons(feature, ldinfo.provided);
    }
    (void)SCM_INTERNAL_COND_BROADCAST(ldinfo.prov_cv);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.prov_mutex);
    if (packet) packet->loaded = TRUE;
    return 0;
}

ScmObj Scm_Provide(ScmObj feature)
{
    ScmVM *self = Scm_VM();

    if (!SCM_STRINGP(feature)&&!SCM_FALSEP(feature)) {
        SCM_TYPE_ERROR(feature, "string");
    }
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.prov_mutex);
    if (SCM_STRINGP(feature)
        && SCM_FALSEP(Scm_Member(feature, ldinfo.provided, SCM_CMP_EQUAL))) {
        ldinfo.provided = Scm_Cons(feature, ldinfo.provided);
    }
    ScmObj cp;
    SCM_FOR_EACH(cp, ldinfo.providing) {
        if (SCM_CADR(SCM_CAR(cp)) == SCM_OBJ(self)) {
            SCM_SET_CDR(SCM_CDR(SCM_CAR(cp)), SCM_LIST1(feature));
            break;
        }
    }
    (void)SCM_INTERNAL_COND_SIGNAL(ldinfo.prov_cv);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ldinfo.prov_mutex);
    return feature;
}

int Scm_ProvidedP(ScmObj feature)
{
    (void)SCM_INTERNAL_MUTEX_LOCK(ldinfo.prov_mutex);
    int r = !SCM_FALSEP(Scm_Member(feature, ldinfo.provided, SCM_CMP_EQUAL));
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

ScmObj Scm_MakeAutoload(ScmModule *where,
                        ScmSymbol *name,
                        ScmString *path,
                        ScmSymbol *import_from)
{
    ScmAutoload *adata = SCM_NEW(ScmAutoload);
    SCM_SET_CLASS(adata, SCM_CLASS_AUTOLOAD);
    adata->name = name;
    adata->module = where;
    adata->path = path;
    adata->import_from = import_from;
    adata->loaded = FALSE;
    adata->value = SCM_UNBOUND;
    (void)SCM_INTERNAL_MUTEX_INIT(adata->mutex);
    (void)SCM_INTERNAL_COND_INIT(adata->cv);
    adata->locker = NULL;
    return SCM_OBJ(adata);
}

void Scm_DefineAutoload(ScmModule *where,
                        ScmObj file_or_module,
                        ScmObj list)
{
    ScmString *path = NULL;
    ScmSymbol *import_from = NULL;

    if (SCM_STRINGP(file_or_module)) {
        path = SCM_STRING(file_or_module);
    } else if (SCM_SYMBOLP(file_or_module)) {
        import_from = SCM_SYMBOL(file_or_module);
        path = SCM_STRING(Scm_ModuleNameToPath(import_from));
    } else {
        Scm_Error("autoload: string or symbol required, but got %S",
                  file_or_module);
    }
    ScmObj ep;
    SCM_FOR_EACH(ep, list) {
        ScmObj entry = SCM_CAR(ep);
        if (SCM_SYMBOLP(entry)) {
            Scm_Define(where, SCM_SYMBOL(entry),
                       Scm_MakeAutoload(where, SCM_SYMBOL(entry),
                                        path, import_from));
        } else if (SCM_PAIRP(entry)
                   && SCM_EQ(key_macro, SCM_CAR(entry))
                   && SCM_PAIRP(SCM_CDR(entry))
                   && SCM_SYMBOLP(SCM_CADR(entry))) {
            ScmSymbol *sym = SCM_SYMBOL(SCM_CADR(entry));
            ScmObj autoload = Scm_MakeAutoload(where, sym, path, import_from);
            Scm_Define(where, sym,
                       Scm_MakeMacroAutoload(sym, SCM_AUTOLOAD(autoload)));
        } else {
            Scm_Error("autoload: bad autoload symbol entry: %S", entry);
        }
    }
}


ScmObj Scm_ResolveAutoload(ScmAutoload *adata, int flags)
{
    int circular = FALSE;
    ScmVM *vm = Scm_VM();

    /* shortcut in case if somebody else already did the job. */
    if (adata->loaded) return adata->value;

    /* check to see if this autoload is recursive.  if so, we just return
       SCM_UNBOUND and let the caller handle the issue (NB: it isn't
       necessarily an error.  For example, define-method searches if
       a generic function of the same name is already defined; if the
       name is set autoload and define-method is in the file that's being
       autoloaded, define-method finds the name is an autoload that points
       the currently autoloaded file.)
       we have to be careful to exclude the case that when one thread is
       resolving autoload another thread enters here and sees this autoload
       is already being resolved.
     */
    if ((adata->locker == NULL || adata->locker == vm)
        && !SCM_FALSEP(Scm_Assoc(SCM_OBJ(adata->path),
                                 ldinfo.providing,
                                 SCM_CMP_EQUAL))) {
        return SCM_UNBOUND;
    }

    /* obtain the lock to load this autoload */
    (void)SCM_INTERNAL_MUTEX_LOCK(adata->mutex);
    do {
        if (adata->loaded) break;
        if (adata->locker == NULL) {
            adata->locker = vm;
        } else if (adata->locker == vm) {
            /* bad circular dependency */
            circular = TRUE;
        } else if (adata->locker->state == SCM_VM_TERMINATED) {
            /* the loading thread have died prematurely.
               let's take over the task. */
            adata->locker = vm;
        } else {
            (void)SCM_INTERNAL_COND_WAIT(adata->cv, adata->mutex);
            continue;
        }
    } while (0);
    SCM_INTERNAL_MUTEX_UNLOCK(adata->mutex);
    if (adata->loaded) {
        /* ok, somebody did the work for me.  just use the result. */
        return adata->value;
    }

    if (circular) {
        /* Since we have already checked recursive loading, it isn't normal
           if we reach here.  Right now I have no idea how this happens, but
           just in case we raise an error. */
        adata->locker = NULL;
        SCM_INTERNAL_COND_BROADCAST(adata->cv);
        Scm_Error("Attempted to trigger the same autoload %S#%S recursively.  Maybe circular autoload dependency?\n",
                  adata->module, adata->name);
    }

    SCM_UNWIND_PROTECT {
        do_require(SCM_OBJ(adata->path), SCM_LOAD_PROPAGATE_ERROR,
                   adata->module, NULL);

        if (adata->import_from) {
            /* autoloaded file defines import_from module.  we need to
               import the binding individually. */
            ScmModule *m = Scm_FindModule(adata->import_from,
                                          SCM_FIND_MODULE_QUIET);
            if (m == NULL) {
                Scm_Error("Trying to autoload module %S from file %S, but the file doesn't define such a module",
                          adata->import_from, adata->path);
            }
            ScmGloc *f = Scm_FindBinding(SCM_MODULE(m), adata->name, 0);
            ScmGloc *g = Scm_FindBinding(adata->module, adata->name, 0);
            SCM_ASSERT(f != NULL);
            SCM_ASSERT(g != NULL);
            adata->value = SCM_GLOC_GET(f);
            if (SCM_UNBOUNDP(adata->value) || SCM_AUTOLOADP(adata->value)) {
                Scm_Error("Autoloaded symbol %S is not defined in the module %S",
                          adata->name, adata->import_from);
            }
            SCM_GLOC_SET(g, adata->value);
        } else {
            /* Normal import.  The binding must have been inserted to
               adata->module */
            ScmGloc *g = Scm_FindBinding(adata->module, adata->name, 0);
            SCM_ASSERT(g != NULL);
            adata->value = SCM_GLOC_GET(g);
            if (SCM_UNBOUNDP(adata->value) || SCM_AUTOLOADP(adata->value)) {
                Scm_Error("Autoloaded symbol %S is not defined in the file %S",
                          adata->name, adata->path);
            }
        }
    } SCM_WHEN_ERROR {
        adata->locker = NULL;
        SCM_INTERNAL_COND_BROADCAST(adata->cv);
        SCM_NEXT_HANDLER;
    } SCM_END_PROTECT;

    adata->loaded = TRUE;
    adata->locker = NULL;
    SCM_INTERNAL_COND_BROADCAST(adata->cv);
    return adata->value;
}

/*------------------------------------------------------------------
 * Dynamic parameter access
 */
ScmObj Scm_CurrentLoadHistory() { return PARAM_REF(Scm_VM(), load_history); }
ScmObj Scm_CurrentLoadNext()    { return PARAM_REF(Scm_VM(), load_next); }
ScmObj Scm_CurrentLoadPort()    { return PARAM_REF(Scm_VM(), load_port); }

/*------------------------------------------------------------------
 * Compatibility stuff
 */

/* TRANSIENT: Pre-0.9 Compatibility routine.  Kept for the binary compatibility.
   Will be removed on 1.0 */
void Scm__LoadFromPortCompat(ScmPort *port, int flags)
{
    Scm_LoadFromPort(port, flags|SCM_LOAD_PROPAGATE_ERROR, NULL);
}

/* TRANSIENT: Pre-0.9 Compatibility routine.  Kept for the binary compatibility.
   Will be removed on 1.0 */
int  Scm__LoadCompat(const char *file, int flags)
{
    return (0 == Scm_Load(file, flags|SCM_LOAD_PROPAGATE_ERROR, NULL));
}

/* TRANSIENT: Pre-0.9 Compatibility routine.  Kept for the binary compatibility.
   Will be removed on 1.0 */
ScmObj Scm__RequireCompat(ScmObj feature)
{
    Scm_Require(feature, SCM_LOAD_PROPAGATE_ERROR, NULL);
    return SCM_TRUE;
}

/* TRANSIENT: This is entirely moved to Scheme (libeval.scm).  The entry is
   kept only for the binary compatibility. */
ScmObj Scm_VMLoadFromPort(ScmPort *port, ScmObj next_paths,
                          ScmObj env, int flags)
{
    Scm_Error("[internal] Scm_VMLoadFromPort() is obsoleted; call load-from-port Scheme procedure.");
    return SCM_UNDEFINED;
}

/* TRANSIENT: Kept for the binary compatibility; the feature
   is in libeval.scm now. */
void Scm__RecordLoadStart(ScmObj load_file_path/*ARGSUSED*/)
{
}

/* TRANSIENT: Kept for the binary compatibility; not used anymore. */
ScmObj Scm_LoadMainScript()
{
    return SCM_UNDEFINED;
}

/*------------------------------------------------------------------
 * Initialization
 */

void Scm__InitLoad(void)
{
    ScmModule *m = Scm_GaucheModule();
    ScmObj t;

    ScmObj init_load_path = t = SCM_NIL;
    SCM_APPEND(init_load_path, t, break_env_paths("GAUCHE_LOAD_PATH"));
    SCM_APPEND1(init_load_path, t, Scm_SiteLibraryDirectory());
    SCM_APPEND1(init_load_path, t, Scm_LibraryDirectory());

    ScmObj init_dynload_path = t = SCM_NIL;
    SCM_APPEND(init_dynload_path, t, break_env_paths("GAUCHE_DYNLOAD_PATH"));
    SCM_APPEND1(init_dynload_path, t, Scm_SiteArchitectureDirectory());
    SCM_APPEND1(init_dynload_path, t, Scm_ArchitectureDirectory());

    ScmObj init_load_suffixes = t = SCM_NIL;
    SCM_APPEND1(init_load_suffixes, t, SCM_MAKE_STR(".sci"));
    SCM_APPEND1(init_load_suffixes, t, SCM_MAKE_STR(".scm"));
    SCM_APPEND1(init_load_suffixes, t, SCM_MAKE_STR(".sld")); /* R7RS library */

    (void)SCM_INTERNAL_MUTEX_INIT(ldinfo.path_mutex);
    (void)SCM_INTERNAL_MUTEX_INIT(ldinfo.prov_mutex);
    (void)SCM_INTERNAL_COND_INIT(ldinfo.prov_cv);
    (void)SCM_INTERNAL_MUTEX_INIT(ldinfo.dso_mutex);

    key_error_if_not_found = SCM_MAKE_KEYWORD("error-if-not-found");
    key_macro = SCM_MAKE_KEYWORD("macro");
    key_ignore_coding = SCM_MAKE_KEYWORD("ignore-coding");
    key_paths = SCM_MAKE_KEYWORD("paths");
    key_environment = SCM_MAKE_KEYWORD("environment");

    Scm_InitStaticClass(SCM_CLASS_DLOBJ, "<dlobj>",
                        m, dlobj_slots, 0);

#define DEF(rec, sym, val) \
    rec = SCM_GLOC(Scm_Define(m, SCM_SYMBOL(sym), val))

    DEF(ldinfo.load_path_rec,    SCM_SYM_LOAD_PATH, init_load_path);
    DEF(ldinfo.dynload_path_rec, SCM_SYM_DYNAMIC_LOAD_PATH, init_dynload_path);
    DEF(ldinfo.load_suffixes_rec, SCM_SYM_LOAD_SUFFIXES, init_load_suffixes);
    DEF(ldinfo.load_path_hooks_rec, SCM_SYM_LOAD_PATH_HOOKS, SCM_NIL);

    /* NB: Some modules are built-in.  We'll register them to the
       provided list, in libomega.scm. */
    ldinfo.provided = SCM_NIL;
    ldinfo.providing = SCM_NIL;
    ldinfo.waiting = SCM_NIL;
    ldinfo.dso_suffixes = SCM_LIST2(SCM_MAKE_STR(".la"),
                                    SCM_MAKE_STR("." SHLIB_SO_SUFFIX));
    ldinfo.dso_list = SCM_NIL;
    ldinfo.dso_prelinked = SCM_NIL;

#define PARAM_INIT(var, name, val) Scm_DefinePrimitiveParameter(m, name, val, &ldinfo.var)
    PARAM_INIT(load_history, "current-load-history", SCM_NIL);
    PARAM_INIT(load_next, "current-load-next", SCM_NIL);
    PARAM_INIT(load_port, "current-load-port", SCM_FALSE);
}
