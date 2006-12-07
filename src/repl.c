/*
 * repl.c - repl
 *
 *   Copyright (c) 2000-2006 Shiro Kawai, All rights reserved.
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
 *  $Id: repl.c,v 1.38 2006-12-07 01:27:16 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/vm.h"

/*
 * Conceptually, repl can be described by the following Scheme code.
 *
 *  (define (repl reader evaluator printer prompter)
 *    (let loop1 ()
 *      (and
 *        (with-error-handler
 *          (lambda (e) (report-error e) #t)
 *          (lambda ()
 *            (prompter)
 *            (let loop2 ((exp (reader)))
 *              (if (eof-object? loop2)
 *                  #f
 *                  (begin
 *                    (call-with-values
 *                      (lambda () (evaluator exp (current-module)))
 *                      printer)
 *                    (loop2 (reader)))))))
 *        (loop1))))
 *
 * It is implemented using trampoline so that it can run without crossing
 * C -> Scheme boundary.
 *
 *  VMRepl -> VMWithErrorHandler -> repl_main -> prompter
 *    -> repl_prompt_cc -> reader -> repl_read_cc -> evaluator
 *    -> repl_eval_cc -> printer -> repl_print_cc -> repl_main
 */

ScmObj Scm_VMRepl(GAUCHE_VMAPI_VM_ARG
                  ScmObj reader, ScmObj evaluator,
                  ScmObj printer, ScmObj prompter);
static ScmSubrProc repl_main;

/* trampolines */
static ScmObj repl_print_cc(GAUCHE_CC_VM_ARG ScmObj result, void **data)
{
#ifdef GAUCHE_SUBR_VM
    return repl_main(Scm_VM(), NULL, 0, data);
#else
    return repl_main(NULL, 0, data);
#endif
}

static ScmObj repl_eval_cc(GAUCHE_CC_VM_ARG ScmObj result, void **data)
{
    ScmObj *closure = (ScmObj *)data;
    ScmObj printer = closure[2];
    GAUCHE_CC_VM_DECL;
    
    if (SCM_PROCEDUREP(printer)) {
#ifdef GAUCHE_VMAPI_VM
        Scm_VMPushCC(vm, repl_print_cc, data, 4);
#else
        Scm_VMPushCC(repl_print_cc, data, 4);
#endif
        if (vm->numVals == 1) {
#ifdef GAUCHE_VMAPI_VM
            return Scm_VMApply1(vm, printer, result);
#else
            return Scm_VMApply1(printer, result);
#endif
        } else {
#ifdef GAUCHE_VMAPI_VM
            return Scm_VMApply(vm, printer, Scm_VMGetResult(vm));
#else
            return Scm_VMApply(printer, Scm_VMGetResult(vm));
#endif
        }
    } else {
        ScmObj result = Scm_VMGetResult(vm), cp;
        SCM_FOR_EACH(cp, result) {
#if 0
            Scm_Write(SCM_CAR(cp), SCM_OBJ(SCM_CUROUT), SCM_WRITE_WRITE);
#else
            Scm_Write(SCM_CAR(cp), SCM_OBJ(SCM_CUROUT), SCM_WRITE_SHARED);
#endif
            Scm_Putc('\n', SCM_CUROUT);
        }
        Scm_Flush(SCM_CUROUT);
#ifdef GAUCHE_SUBR_VM
        return repl_main(vm, NULL, 0, (void*)data);
#else
        return repl_main(NULL, 0, (void*)data);
#endif
    }
}

static ScmObj repl_read_cc(GAUCHE_CC_VM_ARG ScmObj result, void **data)
{
    ScmObj *closure = (ScmObj*)data;
    ScmObj evaluator = closure[1];
    if (SCM_EOFP(result)) {
        return SCM_FALSE;
    } else if (SCM_PROCEDUREP(evaluator)) {
#ifdef GAUCHE_VMAPI_VM
        GAUCHE_CC_VM_DECL;
        Scm_VMPushCC(vm, repl_eval_cc, data, 4);
        return Scm_VMApply2(vm, evaluator, result, SCM_OBJ(SCM_CURRENT_MODULE()));
#else
        Scm_VMPushCC(repl_eval_cc, data, 4);
        return Scm_VMApply2(evaluator, result, SCM_OBJ(SCM_CURRENT_MODULE()));
#endif
    } else {
#ifdef GAUCHE_VMAPI_VM
        GAUCHE_CC_VM_DECL;
        Scm_VMPushCC(vm, repl_eval_cc, data, 4);
        return Scm_VMEval(vm, result, SCM_FALSE);
#else
        Scm_VMPushCC(repl_eval_cc, data, 4);
        return Scm_VMEval(result, SCM_FALSE);
#endif
    }
}

static ScmObj repl_prompt_cc(GAUCHE_CC_VM_ARG ScmObj result, void **data)
{
    ScmObj *closure = (ScmObj*)data;
    ScmObj reader = closure[0];

    if (SCM_PROCEDUREP(reader)) {
#ifdef GAUCHE_VMAPI_VM
        GAUCHE_CC_VM_DECL;
        Scm_VMPushCC(vm, repl_read_cc, data, 4);
        return Scm_VMApply0(vm, reader);
#else
        Scm_VMPushCC(repl_read_cc, data, 4);
        return Scm_VMApply0(reader);
#endif
    } else {
        ScmObj exp = Scm_Read(SCM_OBJ(SCM_CURIN));
#ifdef GAUCHE_CC_VM
        return repl_read_cc(Scm_VM(), exp, data);
#else
        return repl_read_cc(exp, data);
#endif
    }
}

static ScmObj repl_main(GAUCHE_SUBR_VM_ARG ScmObj *args, int nargs, void *data)
{
    ScmObj *closure = (ScmObj*)data;
    ScmObj prompter = closure[3];
    GAUCHE_SUBR_VM_DECL;
    
    if (SCM_PROCEDUREP(prompter)) {
#ifdef GAUCHE_VMAPI_VM
        Scm_VMPushCC(vm, repl_prompt_cc, data, 4);
        return Scm_VMApply0(vm, prompter);
#else
        Scm_VMPushCC(repl_prompt_cc, data, 4);
        return Scm_VMApply0(prompter);
#endif
    } else {
        Scm_Write(SCM_MAKE_STR("gosh> "),
                  SCM_OBJ(SCM_CUROUT), SCM_WRITE_DISPLAY);
        Scm_Flush(SCM_CUROUT);
#ifdef GAUCHE_CC_VM
        return repl_prompt_cc(vm, SCM_UNDEFINED, (void**)data);
#else
        return repl_prompt_cc(SCM_UNDEFINED, (void**)data);
#endif
    }
}

static ScmObj repl_error_handle(GAUCHE_SUBR_VM_ARG ScmObj *args, int nargs, void *data)
{
    SCM_ASSERT(nargs == 1);
    Scm_ReportError(args[0]);
    return SCM_TRUE;
}

static ScmObj repl_loop_cc(GAUCHE_CC_VM_ARG ScmObj result, void **data)
{
    if (SCM_TRUEP(result)) {
        ScmObj *closure = (ScmObj*)data;
#ifdef GAUCHE_VMAPI_VM
        GAUCHE_CC_VM_DECL;
        return Scm_VMRepl(vm, closure[0], closure[1], closure[2], closure[3]);
#else
        return Scm_VMRepl(closure[0], closure[1], closure[2], closure[3]);
#endif
    } else {
        return SCM_FALSE;
    }
}

ScmObj Scm_VMRepl(GAUCHE_VMAPI_VM_ARG
                  ScmObj reader, ScmObj evaluator,
                  ScmObj printer, ScmObj prompter)
{
    ScmObj ehandler, reploop;
    ScmObj *packet = SCM_NEW_ARRAY(ScmObj, 4);
    packet[0] = reader;
    packet[1] = evaluator;
    packet[2] = printer;
    packet[3] = prompter;
    ehandler = Scm_MakeSubr(repl_error_handle, packet, 1, 0, SCM_FALSE);
    reploop = Scm_MakeSubr(repl_main, packet, 0, 0, SCM_FALSE);
#ifdef GAUCHE_VMAPI_VM
    Scm_VMPushCC(vm, repl_loop_cc, (void**)packet, 4);
    return Scm_VMWithErrorHandler(vm, ehandler, reploop);
#else
    Scm_VMPushCC(repl_loop_cc, (void**)packet, 4);
    return Scm_VMWithErrorHandler(ehandler, reploop);
#endif
}

static ScmObj repl_proc(GAUCHE_SUBR_VM_ARG ScmObj *args, int nargs, void *data)
{
    int argc = Scm_Length(args[0]);
    ScmObj reader =    (argc >= 1? SCM_CAR(args[0]) : SCM_FALSE);
    ScmObj evaluator = (argc >= 2? SCM_CADR(args[0]) : SCM_FALSE);
    ScmObj printer =   (argc >= 3? SCM_CAR(SCM_CDDR(args[0])) : SCM_FALSE);
    ScmObj prompter =  (argc >= 4? SCM_CADR(SCM_CDDR(args[0])) : SCM_FALSE);
#ifdef GAUCHE_VMAPI_VM
    GAUCHE_SUBR_VM_DECL;
    return Scm_VMRepl(vm, reader, evaluator, printer, prompter);
#else
    return Scm_VMRepl(reader, evaluator, printer, prompter);
#endif
}

static SCM_DEFINE_STRING_CONST(repl_NAME, "read-eval-print-loop", 20, 20);
static SCM_DEFINE_SUBR(repl_STUB, 0, 1, SCM_OBJ(&repl_NAME), repl_proc, NULL, NULL);

void Scm_Repl(ScmObj reader, ScmObj evaluator, ScmObj printer,
              ScmObj prompter)
{
    Scm_ApplyRec(SCM_OBJ(&repl_STUB),
                 SCM_LIST4(reader, evaluator, printer, prompter));
}

void Scm__InitRepl(void)
{
    Scm_Define(Scm_GaucheModule(),
               SCM_SYMBOL(Scm_Intern(&repl_NAME)),
               SCM_OBJ(&repl_STUB));
}
