/*
 * core.c - core kernel interface
 *
 *  Copyright(C) 2000 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: core.c,v 1.2 2001-01-12 11:33:37 shiro Exp $
 */

#include "gauche.h"

/*
 * Malloc wrapper
 */

void *Scm_Malloc(size_t size)
{
    void *p = GC_MALLOC(size);
    if (p == NULL) Scm_Panic("out of memory");
    return p;
}

void *Scm_MallocAtomic(size_t size)
{
    void *p = GC_MALLOC_ATOMIC(size);
    if (p == NULL) Scm_Panic("out of memory");
    return p;
}

void *Scm_Realloc(void *ptr, size_t size)
{
    void *p = GC_REALLOC(ptr, size);
    if (p == NULL) Scm_Panic("out of memory");
    return p;
}

/*
 * Program initialization and default error handlers.
 */

extern void Scm__InitModule(void);
extern void Scm__InitSymbol(void);
extern void Scm__InitClass(void);
extern void Scm__InitPort(void);

void Scm_Init(void)
{
    ScmVM *vm;
    
    Scm__InitModule();
    Scm__InitSymbol();
    Scm__InitClass();
    Scm__InitPort();
    Scm__InitCompiler();

    vm = Scm_NewVM(NULL, Scm_UserModule());
    Scm_SetVM(vm);
    Scm_Init_stdlib();
    Scm_Init_extlib();
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

/*
 * Error handler should interact with dynamic env.
 * for now, we just jump.
 */

void Scm_Error(const char *msg, ...)
{
    ScmVM *vm = Scm_VM();
    va_list args;
    
    if (vm && vm->escape) {
        SCM_PUSH_ERROR_HANDLER {
            ScmObj ostr = Scm_MakeOutputStringPort();
            va_start(args, msg);
            Scm_Vprintf(SCM_PORT(ostr), msg, args);
            va_end(args);
            vm->errstr = Scm_GetOutputString(SCM_PORT(ostr));
        }
        SCM_WHEN_ERROR {
            vm->errstr = SCM_MAKE_STR("Error occurred in error handler");
        }
        SCM_POP_ERROR_HANDLER;
        
        longjmp(vm->escape->jbuf, 1);
    } else {
        /* No error handler is installed.  Just exit. */
        va_start(args, msg);
        vfprintf(stderr, msg, args);
        va_end(args);
        exit(1);
    }
}
