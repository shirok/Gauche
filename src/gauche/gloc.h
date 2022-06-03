/*
 * gloc.h - Public API for Scheme glocs
 *
 *   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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

/* This file is included from gauche.h */

#ifndef GAUCHE_GLOC_H
#define GAUCHE_GLOC_H

/* Definition of ScmGlocRec is in gauche/priv/glocP.h */

/* About negative binding:
 *
 *   Negative binding is a dummy binding inserted in an intermediate
 *   anonymous module created by 'import' form, to realize :except
 *   and :rename modifiers.   It *never* occurs in modules created
 *   explicitly by define-module form or make-module procedure.
 *
 *   While Scm_FindBinding searches imported modules and parent modules,
 *   if it finds negative binding, it stop searching for the MPL chain
 *   currently it is looking at.  In other word, a negative binding
 *   has an effect to remove the inherited binding from the exported list.
 *
 * About phantom binding.
 *
 *   A binding may be inserted either by definition or export special
 *   forms.  We don't know which comes first.  If we've seen only an
 *   'export' form but not an actual definition, the GLOC of the binding
 *   has value SCM_UNBOUND, and indicates it is a phantom binding.
 *
 *   When we meet a phantom binding during searching a binding of a symbol,
 *   we recursively search the symbol from the module where the phantom
 *   bindings are in, and returns bound GLOC if found.
 *
 *   Example:
 *     (define-module a (export foo) (define foo 1))
 *     (define-module b (import a) (export foo))
 *     Suppose we (import b) and then access 'foo'.  We search the import
 *     chain and find a phantom binding of 'foo' in #<module b>.
 *     In this case, we search 'foo' from #<module b>, and find a gloc
 *     from #<module a>, which is returned as the result of the search.
 *
 *   This makes it easy to create a transitive module, that doesn't
 *   define bindings by itself, but exports subset of imported bindings.
 *
 * Hooks (getter and setter)
 *   All reference and modification of toplevel binding go through
 *   SCM_GLOC_GET/SET macros, which check getter and setter handlers.
 *   These handlers allow to hook global variable reference.  They
 *   are reserved for internal use; its' not for application-level
 *   abstraction.  As of 0.9, getter is not used, and setter is only
 *   used to check the attempt of modifying constant bindings.
 *   Future plans include some kind of debugging facility.
 */

SCM_CLASS_DECL(Scm_GlocClass);
#define SCM_CLASS_GLOC          (&Scm_GlocClass)

#define SCM_GLOC(obj)            ((ScmGloc*)(obj))
#define SCM_GLOCP(obj)           SCM_XTYPEP(obj, SCM_CLASS_GLOC)

/* Scm_GlocGetValue can raise 'reference to a phantom gloc' error if
   the given gloc is a phantom binding.  Use Scm_GlocPhantomBindingP
   to exclude such case. */

SCM_EXTERN ScmObj Scm_GlocGetValue(ScmGloc *g);
SCM_EXTERN void   Scm_GlocSetValue(ScmGloc *g, ScmObj val);

SCM_EXTERN int    Scm_GlocPhantomBindingP(ScmGloc *g);
SCM_EXTERN int    Scm_GlocConstP(ScmGloc *g);
SCM_EXTERN int    Scm_GlocInlinableP(ScmGloc *g);
SCM_EXTERN int    Scm_GlocSupersedableP(ScmGloc *g, u_long flags, ScmObj val);

#endif /* GAUCHE_GLOC_H */
