/*
 * core.c - core kernel interface
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
 *  $Id: core.c,v 1.11 2001-02-10 12:42:46 shiro Exp $
 */

#include "gauche.h"

/*
 * out-of-memory handler.  this will be called by GC.
 */

static GC_PTR oom_handler(size_t bytes)
{
    Scm_Panic("out of memory.  aborting...");
    return NULL;                /* dummy */
}

/*
 * Program initialization and default error handlers.
 */

extern void Scm__InitModule(void);
extern void Scm__InitSymbol(void);
extern void Scm__InitKeyword(void);
extern void Scm__InitClass(void);
extern void Scm__InitPort(void);
extern void Scm__InitLoad(void);

void Scm_Init(const char *initfile)
{
    ScmVM *vm;

    GC_oom_fn = oom_handler;
    
    Scm__InitModule();
    Scm__InitSymbol();
    Scm__InitKeyword();
    Scm__InitClass();
    Scm__InitPort();
    Scm__InitCompiler();
    Scm__InitLoad();

    vm = Scm_NewVM(NULL, Scm_UserModule());
    Scm_SetVM(vm);
    Scm_Init_stdlib();
    Scm_Init_extlib();
    Scm_Init_syslib();

    if (initfile) {
        SCM_PUSH_ERROR_HANDLER {
            Scm_Load(initfile);
        }
        SCM_POP_ERROR_HANDLER;
    }
}

/*
 * Program termination
 */

void Scm_Exit(int code)
{
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
    exit(1);
}

