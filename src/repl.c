/*
 * repl.c - repl
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
 *  $Id: repl.c,v 1.21 2002-07-05 02:57:00 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/vm.h"

/*
 * Conceptually, repl can be described by the following Scheme code.
 *
 *  (define (repl reader evaluator printer prompter)
 *    (let loop1 ()
 *      (with-error-handler
 *        (lambda (e) (print-error e) (loop1))
 *        (lambda ()
 *          (display (prompter)) (flush)
 *          (let loop2 ((exp (reader)))
 *            (if (eof-object? loop2)
 *                (values)
 *                (begin
 *                  (call-with-values (lambda () (evaluator exp)) printer)
 *                  (loop2 (reader)))))))))
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
static ScmObj repl_main(ScmObj *args, int nargs, void *data);

/* trampolines */
static ScmObj repl_print_cc(ScmObj result, void **data)
{
    return repl_main(NULL, 0, data);
}

static ScmObj repl_eval_cc(ScmObj result, void **data)
{
    ScmObj *closure = (ScmObj *)data;
    ScmObj printer = closure[3];
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
            Scm_Write(SCM_CAR(cp), SCM_OBJ(SCM_CUROUT), SCM_WRITE_WRITE);
            Scm_Putc('\n', SCM_CUROUT);
        }
        Scm_Flush(SCM_CUROUT);
        return repl_main(NULL, 0, (void*)data);
    }
}

static ScmObj repl_read_cc(ScmObj result, void **data)
{
    ScmObj *closure = (ScmObj*)data;
    ScmObj evaluator = closure[2];
    if (SCM_EOFP(result)) {
        return SCM_UNDEFINED;
    } else if (SCM_PROCEDUREP(evaluator)) {
        Scm_VMPushCC(repl_eval_cc, data, 4);
        return Scm_VMApply1(evaluator, result);
    } else {
        Scm_VMPushCC(repl_eval_cc, data, 4);
        return Scm_VMEval(result, SCM_UNBOUND);
    }
}

static ScmObj repl_prompt_cc(ScmObj result, void **data)
{
    ScmObj *closure = (ScmObj*)data;
    ScmObj reader = closure[1];
    Scm_Write(result, SCM_OBJ(SCM_CUROUT), SCM_WRITE_DISPLAY);
    Scm_Flush(SCM_CUROUT);
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
    ScmObj prompter = closure[0];
    if (SCM_PROCEDUREP(prompter)) {
        Scm_VMPushCC(repl_prompt_cc, data, 4);
        return Scm_VMApply0(prompter);
    } else {
        return repl_prompt_cc(SCM_MAKE_STR("gosh> "), (void**)data);
    }
}

static ScmObj repl_error_handle(ScmObj *args, int nargs, void *data)
{
    ScmObj *closure = (ScmObj*)data;
    SCM_ASSERT(nargs == 1);
    Scm_ReportError(args[0]);
    return Scm_VMRepl(closure[0], closure[1], closure[2], closure[3]);
}

ScmObj Scm_VMRepl(ScmObj reader, ScmObj evaluator,
                  ScmObj printer, ScmObj prompter)
{
    ScmObj ehandler, reploop;
    ScmObj *packet = SCM_NEW2(ScmObj*, sizeof(ScmObj)*4);
    packet[0] = reader;
    packet[1] = evaluator;
    packet[2] = printer;
    packet[3] = prompter;
    ehandler = Scm_MakeSubr(repl_error_handle, packet, 1, 0, SCM_FALSE);
    reploop = Scm_MakeSubr(repl_main, packet, 0, 0, SCM_FALSE);
    return Scm_VMWithErrorHandler(ehandler, reploop);
}

static ScmObj repl_proc(ScmObj *args, int nargs, void *data)
{
    ScmObj reader =    (nargs >= 1? args[0] : SCM_FALSE);
    ScmObj evaluator = (nargs >= 2? args[1] : SCM_FALSE);
    ScmObj printer =   (nargs >= 3? args[2] : SCM_FALSE);
    ScmObj prompter =  (nargs >= 4? args[3] : SCM_FALSE);
    return Scm_VMRepl(reader, evaluator, printer, prompter);
}

static SCM_DEFINE_STRING_CONST(repl_NAME, "read-eval-print-loop", 20, 20);
static SCM_DEFINE_SUBR(repl_STUB, 0, 1, SCM_OBJ(&repl_NAME), repl_proc, NULL, NULL);

void Scm_Repl(ScmObj reader, ScmObj evaluator, ScmObj printer,
              ScmObj prompter)
{
    Scm_Apply(SCM_OBJ(&repl_STUB),
              SCM_LIST4(reader, evaluator, printer, prompter));
}

void Scm__InitRepl(void)
{
    Scm_Define(Scm_GaucheModule(),
               SCM_SYMBOL(Scm_Intern(&repl_NAME)),
               SCM_OBJ(&repl_STUB));
}
