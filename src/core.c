/*
 * core.c - core kernel interface
 *
 *   Copyright (c) 2000-2010  Shiro Kawai  <shiro@acm.org>
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
#include "gauche/arch.h"
#include "gauche/paths.h"
#include "gauche/builtin-syms.h"

/* GC_print_static_roots() is declared in private/gc_priv.h.  It is too much
   hassle to include it with other GC internal baggages, so we just declare
   it.  It's a private funciton of GC, so watch out the changes in GC. */
extern void GC_print_static_roots(void);

/*
 * out-of-memory handler.  this will be called by GC.
 */

static void *oom_handler(size_t bytes)
{
    Scm_Panic("out of memory (%lu).  aborting...", bytes);
    return NULL;                /* dummy */
}

/*
 * Features list used by cond-expand macro
 */
static struct {
    ScmObj alist;
    ScmInternalMutex mutex;
} cond_features = { SCM_NIL };

/*=============================================================
 * Program initialization
 */

extern void Scm__InitModule(void);
extern void Scm__InitModulePost(void);
extern void Scm__InitSymbol(void);
extern void Scm__InitKeyword(void);
extern void Scm__InitNumber(void);
extern void Scm__InitChar(void);
extern void Scm__InitClass(void);
extern void Scm__InitExceptions(void);
extern void Scm__InitPort(void);
extern void Scm__InitWrite(void);
extern void Scm__InitCompaux(void);
extern void Scm__InitMacro(void);
extern void Scm__InitLoad(void);
extern void Scm__InitProc(void);
extern void Scm__InitRegexp(void);
extern void Scm__InitRead(void);
extern void Scm__InitSignal(void);
extern void Scm__InitSystem(void);
extern void Scm__InitCode(void);
extern void Scm__InitVM(void);
extern void Scm__InitRepl(void);
extern void Scm__InitAutoloads(void);
extern void Scm__InitCollection(void);

extern void Scm_Init_stdlib(ScmModule *);
extern void Scm_Init_extlib(ScmModule *);
extern void Scm_Init_syslib(ScmModule *);
extern void Scm_Init_intlib(ScmModule *);

extern void Scm_Init_scmlib(void);
extern void Scm_Init_compile(void);
extern void Scm_Init_objlib(void);

static void finalizable(void);
static void init_cond_features(void);

#ifdef GAUCHE_USE_PTHREADS
/* a trick to make sure the gc thread object is linked */
static int (*ptr_pthread_create)(void) = NULL;
#endif

/*
 * Entry point of initlalizing Gauche runtime
 */
void Scm_Init(const char *signature)
{
    /* make sure the main program links the same version of libgauche */
    if (strcmp(signature, GAUCHE_SIGNATURE) != 0) {
        Scm_Panic("libgauche ABI version mismatch: libgauche %s, expected %s",
                  GAUCHE_SIGNATURE, signature);
    }

    /* Some platforms require this.  It is harmless if GC is
       already initialized, so we call it here just in case. */
    GC_init();

    /* Set up GC parameters.  We need to call finalizers at the safe
       point of VM loop, so we disable auto finalizer invocation, and
       ask GC to call us back when finalizers are queued. */
    GC_oom_fn = oom_handler;
    GC_finalize_on_demand = TRUE;
    GC_finalizer_notifier = finalizable;

    (void)SCM_INTERNAL_MUTEX_INIT(cond_features.mutex);

    /* Initialize components.  The order is important, for some components
       rely on the other components to be initialized. */
    Scm__InitVM();
    Scm__InitSymbol();
    Scm__InitModule();
    Scm__InitKeyword();
    Scm__InitNumber();
    Scm__InitChar();
    Scm__InitClass();
    Scm__InitModulePost();
    Scm__InitCollection();
    Scm__InitExceptions();
    Scm__InitProc();
    Scm__InitPort();
    Scm__InitWrite();
    Scm__InitCode();
    Scm__InitMacro();
    Scm__InitLoad();
    Scm__InitRegexp();
    Scm__InitRead();
    Scm__InitSignal();
    Scm__InitSystem();
    Scm__InitRepl();

    Scm_Init_stdlib(Scm_SchemeModule());
    Scm_Init_extlib(Scm_GaucheModule());
    Scm_Init_syslib(Scm_GaucheModule());
    Scm_Init_intlib(Scm_GaucheInternalModule());

    Scm_Init_scmlib();
    Scm_Init_compile();
    Scm_Init_objlib();

    Scm__InitCompaux();

    Scm_SelectModule(Scm_GaucheModule());
    Scm__InitAutoloads();

    Scm_SelectModule(Scm_UserModule());

    /* Final setup of cond-features alist. */
    init_cond_features();

#ifdef GAUCHE_USE_PTHREADS
    /* a trick to make sure the gc thread object is linked */
    ptr_pthread_create = (int (*)(void))GC_pthread_create;
#endif
}

/*=============================================================
 * GC utilities
 */

void Scm_GC()
{
    GC_gcollect();
}

void Scm_PrintStaticRoots()
{
    GC_print_static_roots();
}

/*
 * External API to register root set in dynamically loaded library.
 * Boehm GC doesn't do this automatically on some platforms.
 *
 * NB: The scheme we're using to find bss area (by Scm__bss{start|end})
 * is getting less effective, since more platforms are adopting the
 * linker that rearranges bss variables.  The extensions should not
 * keep GC_MALLOCED pointer into the bss variable.
 */
void Scm_RegisterDL(void *data_start, void *data_end,
                    void *bss_start, void *bss_end)
{
    if (data_start < data_end) {
        GC_add_roots(data_start, data_end);
    }
    if (bss_start < bss_end) {
        GC_add_roots(bss_start, bss_end);
    }
}

/*
 * Useful routine for debugging, to check if an object is inadvertently
 * collected.
 */
static void gc_sentinel(ScmObj obj, void *data)
{
    Scm_Printf(SCM_CURERR, "WARNING: object %s(%p) is inadvertently collected\n", (char *)data, obj);
}

void Scm_GCSentinel(void *obj, const char *name)
{
    Scm_RegisterFinalizer(SCM_OBJ(obj), gc_sentinel, (void*)name);
}


/*=============================================================
 * Finalization.  Scheme finalizers are added as NO_ORDER.
 */
void Scm_RegisterFinalizer(ScmObj z, ScmFinalizerProc finalizer, void *data)
{
    GC_finalization_proc ofn; void *ocd;
    GC_REGISTER_FINALIZER_NO_ORDER(z, (GC_finalization_proc)finalizer,
                                   data, &ofn, &ocd);
}

void Scm_UnregisterFinalizer(ScmObj z)
{
    GC_finalization_proc ofn; void *ocd;
    GC_REGISTER_FINALIZER_NO_ORDER(z, (GC_finalization_proc)NULL, NULL,
                                   &ofn, &ocd);
}

/* GC calls this back when finalizers are queued */
void finalizable(void)
{
    ScmVM *vm = Scm_VM();
    vm->finalizerPending = TRUE;
    vm->attentionRequest = TRUE;
}

/* Called from VM loop.  Queue is not empty. */
ScmObj Scm_VMFinalizerRun(ScmVM *vm)
{
    GC_invoke_finalizers();
    vm->finalizerPending = FALSE;
    return SCM_UNDEFINED;
}

/*=============================================================
 * Program cleanup & termination
 */

struct cleanup_handler_rec {
    void (*handler)(void *data);
    void *data;
    struct cleanup_handler_rec *next;
};

static struct {
    int dirty;                  /* Flag to avoid cleaning up more than once. */
    struct cleanup_handler_rec *handlers;
} cleanup = { TRUE, NULL }; 

/* Add cleanup handler.  Returns an opaque handle, which can be
   passed to DeleteCleanupHandler. */
void *Scm_AddCleanupHandler(void (*h)(void *d), void *d)
{
    struct cleanup_handler_rec *r = SCM_NEW(struct cleanup_handler_rec);
    r->handler = h;
    r->data = d;
    r->next = cleanup.handlers;
    cleanup.handlers = r;
    return r;
}

/* Delete cleanup handler.  HANDLE should be an opaque pointer
   returned from Scm_AddCleanupHandler, but it won't complain if
   other pointer is given. */
void Scm_DeleteCleanupHandler(void *handle)
{
    struct cleanup_handler_rec *x = NULL, *y = cleanup.handlers;
    while (y) {
        if (y == handle) {
            if (x == NULL) {
                cleanup.handlers = y->next;
            } else {
                x->next = y->next;
            }
            break;
        }
    }
}

/* pthread specific cleanup handler to unlock mutex */
#ifdef GAUCHE_USE_PTHREADS
void Scm__MutexCleanup(void *mutex_)
{
    ScmInternalMutex *mutex = mutex_;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(*mutex);
}
#endif /* GAUCHE_USE_PTHREADS */


/* Scm_Cleanup and Scm_Exit
   Usually calling Scm_Exit is the easiest way to terminate Gauche
   application safely.  If the application wants to continue operation
   after shutting down the Scheme part, however, it can call Scm_Cleanup().
*/

/* To avoid complication in supporting different platforms */
#define EXIT_CODE(code) ((code)&0xff)

void Scm_Exit(int code)
{
    Scm_Cleanup();
    exit(EXIT_CODE(code));
}

void Scm_Cleanup(void)
{
    ScmVM *vm = Scm_VM();
    ScmObj hp;
    struct cleanup_handler_rec *ch;

    if (!cleanup.dirty) return;
    cleanup.dirty = FALSE;
    
    /* Execute pending dynamic handlers.
       NB: we ignore errors here.  It may not be accurate behavior, though.
       (A handler may intentionally raise an error to skip the rest of
       handlers).  We may change to break from SCM_FOR_EACH in case if
       we detect error in Scm_Apply. */
    SCM_FOR_EACH(hp, vm->handlers) {
        vm->handlers = SCM_CDR(hp);
        Scm_Apply(SCM_CDAR(hp), SCM_NIL, NULL);
    }

    /* Call the C-registered cleanup handlers. */
    for (ch = cleanup.handlers; ch; ch = ch->next) {
        ch->handler(ch->data);
    }
    
    /* Flush Scheme ports. */
    Scm_FlushAllPorts(TRUE);
}

void Scm_Panic(const char *msg, ...)
{
    va_list args;
    va_start(args, msg);
    vfprintf(stderr, msg, args);
    va_end(args);
    fputc('\n', stderr);
    fflush(stderr);
    _exit(EXIT_CODE(1));
}

/* Use this for absolute emergency.  Newline is not attached to msg. */
void Scm_Abort(const char *msg)
{
    int size = (int)strlen(msg);
    /* this may return an error, but we don't care, since we exit anyway. */
    SCM_IGNORE_RESULT(write(2, msg, size));
    _exit(EXIT_CODE(1));
}

/*=============================================================
 * Inspect the configuration
 */

/* Returns the cond-features alist. */
ScmObj
Scm_GetFeatures()
{
    return cond_features.alist;
}

/*
 * Append FEATURE to the cond-features alist.  Once added, the symbol FEATURE
 * will be recognized by cond-expand.
 *
 *  (cond-expand (FEATURE body ...)) 
 *   
 * If loading a module is required in order to make FEATURE available,
 * such module name can be specified in MODULE argument.  It can be NULL
 * if the feature is built-in.
 *
 * This API is meant to expose configuration information to a Scheme
 * level.
 */
void
Scm_AddFeature(const char *feature, const char *module)
{
    ScmObj cell;

    if (module) {
        cell = SCM_LIST2(SCM_INTERN(feature), SCM_INTERN(module));
    } else {
        cell = SCM_LIST1(SCM_INTERN(feature));
    }
    
    (void)SCM_INTERNAL_MUTEX_LOCK(cond_features.mutex);
    cond_features.alist = Scm_Cons(cell, cond_features.alist);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(cond_features.mutex);
}

static void
init_cond_features()
{
    int i;
    /* The initial cond-features list. */
    static struct {
        const char *feature;
        const char *module;
    } init_features[] = {
        { "gauche", NULL },
#ifdef GAUCHE_WINDOWS
        { "gauche.os.windows", NULL },
        { "gauche-windows", NULL }, /* for backward compatibility */
#endif
#ifdef __CYGWIN__              /* cygwin is different enough to deserve this */
        { "gauche.os.cygwin", NULL },
#endif
#ifdef GAUCHE_USE_PTHREADS
        { "gauche.sys.pthreads", "gauche.threads" },
#endif
        { "srfi-0", NULL },         /* autoloaded */
        { "srfi-1", "srfi-1" },
        { "srfi-2", NULL },         /* builtin */
        { "srfi-4", "gauche.uvector" },
        { "srfi-5", "srfi-5" },
        { "srfi-6", NULL },         /* builtin */
        { "srfi-7", NULL },         /* autoloaded */
        { "srfi-8", NULL },         /* builtin */
        { "srfi-9", "gauche.record" },
        { "srfi-10", NULL },
        { "srfi-11", "srfi-11" },
        { "srfi-13", "srfi-13" },
        { "srfi-14", "srfi-14" },
        { "srfi-16", NULL },
        { "srfi-17", NULL },
        { "srfi-18", "gauche.threads" },
        { "srfi-19", "srfi-19" },
        { "srfi-22", NULL },
        { "srfi-23", NULL },
        { "srfi-25", "gauche.array" },
        { "srfi-26", NULL },
        { "srfi-27", "srfi-27" },
        { "srfi-28", NULL },
        { "srfi-29", "srfi-29" },
        { "srfi-30", NULL },
        { "srfi-31", NULL },
        { "srfi-34", NULL },
        { "srfi-35", NULL },
        { "srfi-36", NULL },
        { "srfi-37", "srfi-37" },
        { "srfi-38", NULL },
        { "srfi-39", "gauche.parameter" },
        { "srfi-40", "util.stream" },
        { "srfi-42", "srfi-42" },
        { "srfi-43", "srfi-43" },
        { "srfi-45", NULL },
        { "srfi-55", NULL },
        { "srfi-61", NULL },
        { "srfi-62", NULL },
        { "srfi-87", NULL },
        { "srfi-98", "srfi-98" },
        { "srfi-99", "gauche.record" },
        { NULL, NULL }
    };

    for (i=0; init_features[i].feature; i++) {
        Scm_AddFeature(init_features[i].feature, init_features[i].module);
    }
}

const char *Scm_HostArchitecture(void)
{
    return GAUCHE_ARCH;
}

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

ScmObj Scm_LibraryDirectory(void)
{
    static ScmObj dir = SCM_FALSE;
    if (SCM_FALSEP(dir)) {
        char buf[PATH_MAX];
        Scm_GetLibraryDirectory(buf, PATH_MAX, Scm_Error);
        dir = Scm_MakeString(buf, -1, -1,
                             SCM_STRING_COPYING|SCM_STRING_IMMUTABLE);
    }
    return dir;
}

ScmObj Scm_ArchitectureDirectory(void)
{
    static ScmObj dir = SCM_FALSE;
    if (SCM_FALSEP(dir)) {
        char buf[PATH_MAX];
        Scm_GetArchitectureDirectory(buf, PATH_MAX, Scm_Error);
        dir = Scm_MakeString(buf, -1, -1,
                             SCM_STRING_COPYING|SCM_STRING_IMMUTABLE);
    }
    return dir;
}

ScmObj Scm_SiteLibraryDirectory(void)
{
    static ScmObj dir = SCM_FALSE;
    if (SCM_FALSEP(dir)) {
        char buf[PATH_MAX];
        Scm_GetSiteLibraryDirectory(buf, PATH_MAX, Scm_Error);
        dir = Scm_MakeString(buf, -1, -1,
                             SCM_STRING_COPYING|SCM_STRING_IMMUTABLE);
    }
    return dir;
}

ScmObj Scm_SiteArchitectureDirectory(void)
{
    static ScmObj dir = SCM_FALSE;
    if (SCM_FALSEP(dir)) {
        char buf[PATH_MAX];
        Scm_GetSiteArchitectureDirectory(buf, PATH_MAX, Scm_Error);
        dir = Scm_MakeString(buf, -1, -1,
                             SCM_STRING_COPYING|SCM_STRING_IMMUTABLE);
    }
    return dir;
}

/*=============================================================
 * 'Main'
 */

/*
 * When creating DLL under Cygwin, we need the following dummy main()
 * or we get "undefined reference _WinMain@16" error.
 * (See cygwin FAQ, http://cygwin.com/faq/)
 */
#ifdef __CYGWIN__
int main(void)
{
    return 0;
}
#endif /*__CYGWIN__*/

/*
 * A simple main routine useful to build a binary executable.
 */

void Scm_SimpleMain(int argc, const char *argv[],
                    const char *script, u_long flags)
{
    ScmModule *user = Scm_UserModule();
    ScmObj mainproc, args;
    ScmLoadPacket lpak;

    SCM_ASSERT(argc > 0);
    if (Scm_Load("gauche-init.scm", 0, NULL)) {
        Scm_Printf(SCM_CURERR, "%s: Couldn't load gauche-init.scm: %A(%A).\n",
                   argv[0], 
                   Scm_ConditionMessage(lpak.exception),
                   Scm_ConditionTypeName(lpak.exception));
        Scm_Exit(1);
    }

    args = Scm_CStringArrayToList(argv, argc, SCM_STRING_IMMUTABLE);
    SCM_DEFINE(user, "*program-name*", SCM_CAR(args));
    SCM_DEFINE(user, "*argv*", SCM_CDR(args));

    if (script) {
        ScmObj s = SCM_MAKE_STR(script);
        ScmObj p = Scm_MakeInputStringPort(SCM_STRING(s), TRUE);
        Scm_LoadFromPort(SCM_PORT(p), SCM_LOAD_PROPAGATE_ERROR, NULL);
    }

    mainproc = Scm_GlobalVariableRef(user, SCM_SYMBOL(SCM_INTERN("main")), 0);
    if (SCM_PROCEDUREP(mainproc)) {
        ScmObj r = Scm_ApplyRec(mainproc, SCM_LIST1(args));
        if (SCM_INTP(r)) Scm_Exit(SCM_INT_VALUE(r));
        else             Scm_Exit(70);
    } else {
        Scm_Exit(70);
    }
}

