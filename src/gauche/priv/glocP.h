/*
 * glocP.h - Private API for Scheme glocs
 *
 *   Copyright (c) 2000-2021  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PRIV_GLOCP_H
#define GAUCHE_PRIV_GLOCP_H

/* See gauche/gloc.h for the public interface and other notes */

struct ScmGlocRec {
    SCM_HEADER;
    ScmSymbol *name;
    ScmModule *module;
    ScmObj value;               /* The actual value.  Have to be accessed
                                   via SCM_GLOC_{GET|SET} macros. */
    char hidden;                /* TRUE if this is a "negative binding",
                                   see below. */
    ScmObj (*getter)(ScmGloc *);         /* see 'hooks' below */
    ScmObj (*setter)(ScmGloc *, ScmObj); /* see 'hooks' below */
};

#define SCM_GLOC_GET(gloc) \
    ((gloc)->getter? (gloc)->getter(gloc) : (gloc)->value)
#define SCM_GLOC_SET(gloc, val) \
    ((gloc)->setter? (gloc)->setter((gloc), (val)) : ((gloc)->value = (val)))

#define SCM_GLOC_PHANTOM_BINDING_P(gloc) SCM_UNBOUNDP((gloc)->value)


SCM_EXTERN ScmObj Scm_MakeGloc(ScmSymbol *sym, ScmModule *module);
/* flags may be 0, SCM_BINDING_CONST, or SCM_BINDING_INLINABLE. */
SCM_EXTERN void   Scm_GlocMark(ScmGloc *g, int flags);
SCM_EXTERN ScmObj Scm_GlocConstSetter(ScmGloc *g, ScmObj val);
SCM_EXTERN ScmObj Scm_GlocInlinableSetter(ScmGloc *g, ScmObj val);

/* For ABI compatibility.  Should be gone at api1.0. */
#define SCM_GLOC_CONST_P(gloc)  Scm_GlocConstP(gloc)
SCM_EXTERN ScmObj Scm_GlocMarkConst(ScmGloc *g);
SCM_EXTERN ScmObj Scm_GlocUnmarkConst(ScmGloc *g);

#endif /* GAUCHE_PRIV_GLOCP_H */
