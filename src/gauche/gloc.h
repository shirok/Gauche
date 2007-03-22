/*
 * gloc.h - Public API for Scheme glocs
 *
 *   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
 *  $Id: gloc.h,v 1.2 2007-03-22 11:20:31 shirok Exp $
 */

/* This file is included from gauche.h */

#ifndef GAUCHE_GLOC_H
#define GAUCHE_GLOC_H

struct ScmGlocRec {
    SCM_HEADER;
    ScmSymbol *name;
    ScmModule *module;
    ScmObj value;
    int exported;
    ScmObj (*getter)(ScmGloc *);
    ScmObj (*setter)(ScmGloc *, ScmObj);
};

SCM_CLASS_DECL(Scm_GlocClass);
#define SCM_CLASS_GLOC          (&Scm_GlocClass)

#define SCM_GLOC(obj)            ((ScmGloc*)(obj))
#define SCM_GLOCP(obj)           SCM_XTYPEP(obj, SCM_CLASS_GLOC)

#define SCM_GLOC_GET(gloc) \
    ((gloc)->getter? (gloc)->getter(gloc) : (gloc)->value)
#define SCM_GLOC_SET(gloc, val) \
    ((gloc)->setter? (gloc)->setter((gloc), (val)) : ((gloc)->value = (val)))

#define SCM_GLOC_CONST_P(gloc) \
    ((gloc)->setter == Scm_GlocConstSetter)

/* INTERNAL */
SCM_EXTERN ScmObj Scm_MakeGloc(ScmSymbol *sym, ScmModule *module);
SCM_EXTERN ScmObj Scm_MakeConstGloc(ScmSymbol *sym, ScmModule *module);
SCM_EXTERN ScmObj Scm_GlocConstSetter(ScmGloc *g, ScmObj val);

#endif /* GAUCHE_GLOC_H */

