/*
 * gloc.c - gloc implementation
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

/*---------------------------------------------------------------
 * GLOCs
 */

static void gloc_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmGloc *g = SCM_GLOC(obj);
    Scm_Printf(port, "#<gloc %S#%S%s>", g->module->name,
               g->name,
               (Scm_GlocConstP(g)
                ? " const"
                : (Scm_GlocInlinableP(g)
                   ? " inlinable"
                   : (SCM_GLOC_PHANTOM_BINDING_P(g)
                      ? " phantom"
                      : ""))));
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_GlocClass, gloc_print);

ScmObj Scm_MakeGloc(ScmSymbol *sym, ScmModule *module)
{
    ScmGloc *g = SCM_NEW(ScmGloc);
    SCM_SET_CLASS(g, &Scm_GlocClass);
    g->name = sym;
    g->module = module;
    g->value = SCM_UNBOUND;
    g->hidden = FALSE;
    g->getter = NULL;
    g->setter = NULL;
    return SCM_OBJ(g);
}

/* special setters for const and inlinable bindings. */
ScmObj Scm_GlocConstSetter(ScmGloc *gloc, ScmObj val)
{
    Scm_Error("cannot change constant value of %S#%S",
              gloc->module->name, gloc->name);
    return SCM_UNDEFINED;       /* dummy */
}

ScmObj Scm_GlocInlinableSetter(ScmGloc *gloc, ScmObj val)
{
    Scm_Warn("altering binding of inlinable procedure: %S#%S",
             gloc->module->name, gloc->name);
    return val;
}

int Scm_GlocConstP(ScmGloc *gloc)
{
    return ((gloc)->setter == Scm_GlocConstSetter);
}

int Scm_GlocInlinableP(ScmGloc *gloc)
{
    return ((gloc)->setter == Scm_GlocInlinableSetter);
}

/* Change binding flags.  Do not use casually. */
void Scm_GlocMark(ScmGloc *gloc, int flags)
{
    if (flags & SCM_BINDING_CONST) {
        gloc->setter = Scm_GlocConstSetter;
    } else if (flags & SCM_BINDING_INLINABLE) {
        gloc->setter = Scm_GlocInlinableSetter;
    } else {
        gloc->setter = NULL;
    }
}

/* For the backward ABI compatibility */
ScmObj Scm_GlocMarkConst(ScmGloc *gloc)
{
    Scm_GlocMark(gloc, SCM_BINDING_CONST);
    return SCM_OBJ(gloc);
}

/* For the backward ABI compatibility */
ScmObj Scm_GlocUnmarkConst(ScmGloc *gloc)
{
    Scm_GlocMark(gloc, 0);
    return SCM_OBJ(gloc);
}

