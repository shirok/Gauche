/*
 * core.c - core kernel interface
 *
 *   Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
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
 *
 *  $Id: core.c,v 1.61.2.4 2004-12-31 01:03:10 shirok Exp $
 */

#include <stdlib.h>
#include <unistd.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/arch.h"

/*
 * out-of-memory handler.  this will be called by GC.
 */

static GC_PTR oom_handler(size_t bytes)
{
    Scm_Panic("out of memory (%d).  aborting...", bytes);
    return NULL;                /* dummy */
}

/*
 * Program initialization and default error handlers.
 */

extern void Scm__InitModule(void);
extern void Scm__InitSymbol(void);
extern void Scm__InitKeyword(void);
extern void Scm__InitNumber(void);
extern void Scm__InitChar(void);
extern void Scm__InitClass(void);
extern void Scm__InitExceptions(void);
extern void Scm__InitPort(void);
extern void Scm__InitWrite(void);
extern void Scm__InitCompiler(void);
extern void Scm__InitMacro(void);
extern void Scm__InitLoad(void);
extern void Scm__InitProc(void);
extern void Scm__InitRegexp(void);
extern void Scm__InitRead(void);
extern void Scm__InitSignal(void);
extern void Scm__InitSystem(void);
extern void Scm__InitVM(void);
extern void Scm__InitRepl(void);
extern void Scm__InitParameter(void);
extern void Scm__InitAutoloads(void);

extern void Scm_Init_stdlib(ScmModule *);
extern void Scm_Init_extlib(ScmModule *);
extern void Scm_Init_syslib(ScmModule *);
extern void Scm_Init_moplib(ScmModule *);
extern void Scm_Init_intlib(ScmModule *);

extern void Scm__InitComp(void);

static void finalizable(void);


#ifdef GAUCHE_USE_PTHREADS
/* a trick to make sure the gc thread object is linked */
static int (*ptr_pthread_create)(void) = NULL;
#endif

void Scm_Init(const char *signature)
{
    /* make sure the main program links the same version of libgauche */
    if (strcmp(signature, GAUCHE_SIGNATURE) != 0) {
        Scm_Panic("libgauche version mismatch: libgauche %s, expected %s",
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

    /* Initialize components.  The order is important, for some components
       rely on the other components to be initialized. */
    Scm__InitSymbol();
    Scm__InitModule();
    Scm__InitKeyword();
    Scm__InitNumber();
    Scm__InitChar();
    Scm__InitClass();
    Scm__InitExceptions();
    Scm__InitProc();
    Scm__InitPort();
    Scm__InitWrite();
    Scm__InitVM();
    Scm__InitCompiler();
    Scm__InitParameter();
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
    Scm_Init_moplib(Scm_GaucheModule());
    Scm_Init_intlib(SCM_MODULE(SCM_FIND_MODULE("gauche.internal", TRUE)));

    Scm__InitComp();

    Scm_SelectModule(Scm_GaucheModule());
    Scm__InitAutoloads();

    Scm_SelectModule(Scm_UserModule());

#ifdef GAUCHE_USE_PTHREADS
    /* a trick to make sure the gc thread object is linked */
    ptr_pthread_create = (int (*)(void))GC_pthread_create;
#endif
}

void Scm_RegisterDL(void *data_start, void *data_end,
                    void *bss_start, void *bss_end)
{
    GC_add_roots((GC_PTR)data_start, (GC_PTR)data_end);
    GC_add_roots((GC_PTR)bss_start, (GC_PTR)bss_end);
}

/*
 * Finalization.  Scheme finalizers are added as NO_ORDER.
 */
void Scm_RegisterFinalizer(ScmObj z, ScmFinalizerProc finalizer, void *data)
{
    GC_finalization_proc ofn; GC_PTR ocd;
    GC_REGISTER_FINALIZER_NO_ORDER(z, (GC_finalization_proc)finalizer,
                                   data, &ofn, &ocd);
}

void Scm_UnregisterFinalizer(ScmObj z)
{
    GC_finalization_proc ofn; GC_PTR ocd;
    GC_REGISTER_FINALIZER_NO_ORDER(z, (GC_finalization_proc)NULL, NULL,
                                   &ofn, &ocd);
}

/* GC calls this back when finalizers are queued */
void finalizable(void)
{
    ScmVM *vm = Scm_VM();
    vm->queueNotEmpty |= SCM_VM_FINQ_MASK;
}

/* Called from VM loop.  Queue is not empty. */
ScmObj Scm_VMFinalizerRun(ScmVM *vm)
{
    GC_invoke_finalizers();
    vm->queueNotEmpty &= ~SCM_VM_FINQ_MASK;
}

/*
 * Program termination
 */

void Scm_Exit(int code)
{
    /* TODO: This should throw <application-exit> exception and
       let the VM handle unwinding dynamic handlers.  For now,
       we call the handlers here. */
    ScmVM *vm = Scm_VM();
    ScmObj hp;
    SCM_FOR_EACH(hp, vm->handlers) {
        vm->handlers = SCM_CDR(hp);
        Scm_Apply(SCM_CDAR(hp), SCM_NIL);
    }
    Scm_FlushAllPorts(TRUE);

    /* Print statistics.
       TODO: this should be enabled by separately from
       SCM_COLLECT_VM_STATS flag, so that application can cutomize
       how to utilize the statistics data. */
    if (SCM_VM_RUNTIME_FLAG_IS_SET(vm, SCM_COLLECT_VM_STATS)) {
        fprintf(stderr, "\n;; Statistics (*: main thread only):\n");
        fprintf(stderr,
                ";;  GC: %dbytes heap, %dbytes allocated\n",
                GC_get_heap_size(), GC_get_total_bytes());
        fprintf(stderr,
                ";;  stack overflow*: %dtimes, %.2fms total/%.2fms avg\n",
                vm->stat.sovCount,
                vm->stat.sovTime/1000.0,
                (vm->stat.sovCount > 0?
                 (vm->stat.sovTime/vm->stat.sovCount)/1000.0 :
                 0.0));
    }

    exit(code);
}

void Scm_Panic(const char *msg, ...)
{
    va_list args;
    va_start(args, msg);
    vfprintf(stderr, msg, args);
    va_end(args);
    fputc('\n', stderr);
    fflush(stderr);
    _exit(1);
}

/* Use this for absolute emergency.  Newline is not attached to msg. */
void Scm_Abort(const char *msg)
{
    int size = strlen(msg);
    write(2, msg, size); /* this may return an error, but we don't care */
    _exit(1);
}

/*
 * Inspect the configuration
 * For MinGW32, we don't know where things will be installed at
 * the compile time, so we don't use configure-variable GAUCHE_LIB_DIR etc.
 * Instead, we query the directory of DLL and calculate the paths
 *
 */

const char *Scm_HostArchitecture(void)
{
    return GAUCHE_ARCH;
}

#ifdef __MINGW32__

ScmObj get_install_dir(void)
{
    static ScmObj dir = SCM_FALSE;
    if (SCM_FALSEP(dir)) {
	HMODULE mod;
	DWORD r;
	char path[MAX_PATH];

	mod = GetModuleHandle("libgauche.dll");
	if (mod == NULL) {
	    Scm_Error("GetModuleHandle failed");
	}
	r = GetModuleFileName(mod, path, MAX_PATH);
	if (r == 0) {
	    Scm_Error("GetModuleFileName failed");
	}
	/* remove \libgauche.dll */
	if (!PathRemoveFileSpec(path)) {
	    Scm_Error("PathRemoveFileSpec failed on %s", path);
	}
	/* remobe \bin */
	if (!PathRemoveFileSpec(path)) {
	    Scm_Error("PathRemoveFileSpec failed on %s", path);
	}
	dir = SCM_MAKE_STR_COPYING(path);
    }
    return dir;
}

ScmObj Scm_LibraryDirectory(void)
{
    static ScmObj dir = SCM_FALSE;
    if (SCM_FALSEP(dir)) {
        dir = Scm_StringAppendC(SCM_STRING(get_install_dir()),
                                "\\share\\gauche\\"GAUCHE_VERSION"\\lib",
                                -1, -1);
    }
    return dir;
}

ScmObj Scm_ArchitectureDirectory(void)
{
    static ScmObj dir = SCM_FALSE;
    if (SCM_FALSEP(dir)) {
        dir = Scm_StringAppendC(SCM_STRING(get_install_dir()),
                                "\\lib\\gauche\\"GAUCHE_VERSION"\\"GAUCHE_ARCH,
                                -1, -1);
    }
    return dir;
}

ScmObj Scm_SiteLibraryDirectory(void)
{
    static ScmObj dir = SCM_FALSE;
    if (SCM_FALSEP(dir)) {
        dir = Scm_StringAppendC(SCM_STRING(get_install_dir()),
                                "\\share\\gauche\\site\\lib",
                                -1, -1);
    }
    return dir;
}

ScmObj Scm_SiteArchitectureDirectory(void)
{
    static ScmObj dir = SCM_FALSE;
    if (SCM_FALSEP(dir)) {
        dir = Scm_StringAppendC(SCM_STRING(get_install_dir()),
                                "\\lib\\gauche\\site\\"GAUCHE_VERSION"\\"GAUCHE_ARCH,
                                -1, -1);
    }
    return dir;
}

#else /* !__MINGW32__ */

#define DEFSTR(n, s) \
    static SCM_DEFINE_STRING_CONST(n, s, sizeof(s)-1, sizeof(s)-1)

DEFSTR(libdir,      GAUCHE_LIB_DIR);
DEFSTR(archdir,     GAUCHE_ARCH_DIR);
DEFSTR(sitelibdir,  GAUCHE_SITE_LIB_DIR);
DEFSTR(sitearchdir, GAUCHE_SITE_ARCH_DIR);

ScmObj Scm_LibraryDirectory(void)
{
    return SCM_OBJ(&libdir);
}

ScmObj Scm_ArchitectureDirectory(void)
{
    return SCM_OBJ(&archdir);
}

ScmObj Scm_SiteLibraryDirectory(void)
{
    return SCM_OBJ(&sitelibdir);
}

ScmObj Scm_SiteArchitectureDirectory(void)
{
    return SCM_OBJ(&sitearchdir);
}

#undef DEFSTR

#endif /* !__MINGW32__ */

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
