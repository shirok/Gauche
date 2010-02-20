/*
 * repl.c - repl
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
 *
 *  $Id: repl.c,v 1.41 2008-05-10 13:36:21 shirok Exp $
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
 *              (if (eof-object? exp)
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

ScmObj Scm_VMRepl(ScmObj reader, ScmObj evaluator,
                  ScmObj printer, ScmObj prompter);
static ScmSubrProc repl_main;

/* trampolines */
static ScmObj repl_print_cc(ScmObj result, void **data)
{
    return repl_main(NULL, 0, data);
}

static ScmObj repl_eval_cc(ScmObj result, void **data)
{
    ScmObj *closure = (ScmObj *)data;
    ScmObj printer = closure[2];
    ScmVM *vm = Scm_VM();

    if (SCM_PROCEDUREP(printer)) {
        Scm_VMPushCC(repl_print_cc, data, 4);
        if (vm->numVals == 1) {
            return Scm_VMApply1(printer, result);
        } else {
            return Scm_VMApply(printer, Scm_VMGetResult(vm));
        }
    } else {
        ScmObj result = Scm_VMGetResult(vm), cp;
        SCM_FOR_EACH(cp, result) {
            Scm_Write(SCM_CAR(cp), SCM_OBJ(SCM_CUROUT), SCM_WRITE_SHARED);
            Scm_Putc('\n', SCM_CUROUT);
        }
        Scm_Flush(SCM_CUROUT);
        return repl_main(NULL, 0, (void*)data);
    }
}

static ScmObj repl_read_cc(ScmObj result, void **data)
{
    ScmObj *closure = (ScmObj*)data;
    ScmObj evaluator = closure[1];
    if (SCM_EOFP(result)) {
        return SCM_FALSE;
    } else if (SCM_PROCEDUREP(evaluator)) {
        Scm_VMPushCC(repl_eval_cc, data, 4);
        return Scm_VMApply2(evaluator, result, SCM_OBJ(SCM_CURRENT_MODULE()));
    } else {
        Scm_VMPushCC(repl_eval_cc, data, 4);
        return Scm_VMEval(result, SCM_FALSE);
    }
}

static ScmObj repl_prompt_cc(ScmObj result, void **data)
{
    ScmObj *closure = (ScmObj*)data;
    ScmObj reader = closure[0];

    if (SCM_PROCEDUREP(reader)) {
        Scm_VMPushCC(repl_read_cc, data, 4);
        return Scm_VMApply0(reader);
    } else {
        ScmObj exp = Scm_Read(SCM_OBJ(SCM_CURIN));
        return repl_read_cc(exp, data);
    }
}

static ScmObj repl_main(ScmObj *args, int nargs, void *data)
{
    ScmObj *closure = (ScmObj*)data;
    ScmObj prompter = closure[3];
    
    if (SCM_PROCEDUREP(prompter)) {
        Scm_VMPushCC(repl_prompt_cc, data, 4);
        return Scm_VMApply0(prompter);
    } else {
        Scm_Write(SCM_MAKE_STR("gosh> "),
                  SCM_OBJ(SCM_CUROUT), SCM_WRITE_DISPLAY);
        Scm_Flush(SCM_CUROUT);
        return repl_prompt_cc(SCM_UNDEFINED, (void**)data);
    }
}

static ScmObj repl_error_handle(ScmObj *args, int nargs, void *data)
{
    SCM_ASSERT(nargs == 1);
    Scm_ReportError(args[0]);
    return SCM_TRUE;
}

static ScmObj repl_loop_cc(ScmObj result, void **data)
{
    if (SCM_TRUEP(result)) {
        ScmObj *closure = (ScmObj*)data;
        return Scm_VMRepl(closure[0], closure[1], closure[2], closure[3]);
    } else {
        return SCM_FALSE;
    }
}

ScmObj Scm_VMRepl(ScmObj reader, ScmObj evaluator,
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
    Scm_VMPushCC(repl_loop_cc, (void**)packet, 4);
    return Scm_VMWithErrorHandler(ehandler, reploop);
}

static ScmObj repl_proc(ScmObj *args, int nargs, void *data)
{
    int argc = Scm_Length(args[0]);
    ScmObj reader =    (argc >= 1? SCM_CAR(args[0]) : SCM_FALSE);
    ScmObj evaluator = (argc >= 2? SCM_CADR(args[0]) : SCM_FALSE);
    ScmObj printer =   (argc >= 3? SCM_CAR(SCM_CDDR(args[0])) : SCM_FALSE);
    ScmObj prompter =  (argc >= 4? SCM_CADR(SCM_CDDR(args[0])) : SCM_FALSE);
    return Scm_VMRepl(reader, evaluator, printer, prompter);
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
