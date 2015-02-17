/*
 * box.c - boxes
 *
 *   Copyright (c) 2010-2015  Shiro Kawai  <shiro@acm.org>
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

/* Box is a heap-allocated object that can hold one Scheme value.
 * It is internally used to realize indirection for mutable local
 * variables.   Conceptually, the compiler translates initialization,
 * reference and mutation of mutable local variables into construction,
 * dereference and mutation of box, as follows:
 *
 * Original code:
 *
 *   (let ((var 1))
 *     ...
 *     (foo var)
 *     ...
 *     (set! var 2)
 *     ...)
 *
 * Translated:
 *
 *   (let ((var (%make-box 1)))
 *     ...
 *     (foo (%box-ref var))
 *     ...
 *     (%box-set! var 2)
 *     ...)
 *
 * Local variables that are never set! won't be translated.
 *
 * After this translation, *all* local variables are immutable.  It makes
 * optimization a lot easier.
 */

/* SRFI-111 defines box API.  Scheme interface is in libmisc.scm. */
/* NB: SRFI-111 leave equal? behavior implementation-dependent (except
   it must return #f if eqv? returns #f).  We compare the contents.
 */
static void box_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<box %S>", SCM_BOX_VALUE(obj));
}

static int box_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        /* should return 0 if x == y */
        return (Scm_EqualP(SCM_BOX_VALUE(x), SCM_BOX_VALUE(y))? 0:1);
    } else {
        return Scm_Compare(SCM_BOX_VALUE(x), SCM_BOX_VALUE(y));
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_BoxClass, box_print, box_compare,
                         NULL, NULL, NULL);

ScmBox *Scm_MakeBox(ScmObj value)
{
    ScmBox *b = SCM_NEW(ScmBox);
    SCM_SET_CLASS(b, &Scm_BoxClass);
    SCM_BOX_SET(b, value);
    return b;
}
