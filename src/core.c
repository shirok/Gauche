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
 *  $Id: core.c,v 1.33 2002-02-07 10:33:51 shirok Exp $
 */

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

extern void Scm_Init_stdlib(ScmModule *);
extern void Scm_Init_extlib(ScmModule *);
extern void Scm_Init_syslib(ScmModule *);
extern void Scm_Init_moplib(ScmModule *);

void Scm_Init(void)
{
    ScmVM *vm;
    GC_oom_fn = oom_handler;
    
    Scm__InitSymbol();
    Scm__InitModule();
    Scm__InitKeyword();
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

    vm = Scm_NewVM(NULL, Scm_SchemeModule());
    Scm_SetVM(vm);
    Scm_Init_stdlib(Scm_SchemeModule());
    Scm_Init_extlib(Scm_GaucheModule());
    Scm_Init_syslib(Scm_GaucheModule());
    Scm_Init_moplib(Scm_GaucheModule());
    Scm_SelectModule(Scm_UserModule());
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
    exit(code);
}

void Scm_Abort(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
    _exit(1);
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

/*
 * Inspect the configuration
 */

const char *Scm_HostArchitecture(void)
{
    return GAUCHE_ARCH;
}


