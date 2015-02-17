/*
 * repl.c - repl
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
#include "gauche/vm.h"

/* This file will be gone in 1.0 release */

/* This has never been exposed in public header files, but it wasn't 'static'
   so it was visible.  To keep binary-level compatibility, we provide
   this stub---but nobody should've called this. */
ScmObj Scm_VMRepl(ScmObj reader, ScmObj evaluator,
                  ScmObj printer, ScmObj prompter)
{
    Scm_Panic("Scm_VMRepl is obsoleted.");
    return SCM_UNDEFINED;
}

/* C API - deprecated
   main.c used to call this to invoke repl, but now it uses
   Scm_EvalCString.   Now we have multiple versions of read-eval-print-loop
   (the 'bare' one in libeval.scm and the user-friendly one in
   gauche.interactive), we want to allow the caller to specify which.
   Furthermore, the caller always use Scm_Eval family to call desired
   read-eval-print-loop.   Hence the point of having this API as is
   is diminishing.
 */
void Scm_Repl(ScmObj reader, ScmObj evaluator, ScmObj printer,
              ScmObj prompter)
{
    static ScmObj repl = SCM_UNDEFINED;
    SCM_BIND_PROC(repl, "read-eval-print-loop", Scm_GaucheModule());
    Scm_ApplyRec(repl, SCM_LIST4(reader, evaluator, printer, prompter));
}
