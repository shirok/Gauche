/*
 * core.c - core kernel interface
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
 *  $Id: core.c,v 1.45 2002-07-31 22:09:11 shirok Exp $
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

extern void Scm_Init_stdlib(ScmModule *);
extern void Scm_Init_extlib(ScmModule *);
extern void Scm_Init_syslib(ScmModule *);
extern void Scm_Init_moplib(ScmModule *);

#ifdef GAUCHE_USE_PTHREADS
/* a trick to make sure the gc thread object is linked */
static int (*ptr_pthread_create)(void) = NULL;
#endif

void Scm_Init(void)
{
    GC_oom_fn = oom_handler;

    /* Initialize components.  The order is important, for some components
       rely on the other components to be initialized. */
    Scm__InitSymbol();
    Scm__InitModule();
    Scm__InitKeyword();
    Scm__InitNumber();
    Scm__InitClass();
    Scm__InitExceptions();
    Scm__InitProc();
    Scm__InitPort();
    Scm__InitWrite();
    Scm__InitCompiler();
    Scm__InitMacro();
    Scm__InitLoad();
    Scm__InitRegexp();
    Scm__InitRead();
    Scm__InitSignal();
    Scm__InitSystem();
    Scm__InitVM();
    Scm__InitRepl();
    
    Scm_Init_stdlib(Scm_SchemeModule());
    Scm_Init_extlib(Scm_GaucheModule());
    Scm_Init_syslib(Scm_GaucheModule());
    Scm_Init_moplib(Scm_GaucheModule());
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
 */

const char *Scm_HostArchitecture(void)
{
    return GAUCHE_ARCH;
}

/*
 * Useful routine for debugging, to check if an object is inadvertently
 * collected.
 */

static void gc_sentinel(GC_PTR obj, GC_PTR data)
{
    Scm_Printf(SCM_CURERR, "WARNING: object %s(%p) is inadvertently collected\n", (char *)data, obj);
}

void Scm_GCSentinel(void *obj, const char *name)
{
    GC_finalization_proc ofn;
    GC_PTR ocd;
    GC_REGISTER_FINALIZER(obj, gc_sentinel, (void*)name, &ofn, &ocd);
}
