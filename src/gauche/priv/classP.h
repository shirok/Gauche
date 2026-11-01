/*
 * classP.h - Gauche object system private header
 *
 *   Copyright (c) 2000-2025  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PRIV_CLASSP_H
#define GAUCHE_PRIV_CLASSP_H

/* Specialized constructor for records */
SCM_EXTERN ScmObj Scm__AllocateAndInitializeInstance(ScmClass *klass,
                                                     ScmObj *inits,
                                                     int numInits,
                                                     u_long flags);

/* Method dispatcher developer API */
SCM_EXTERN ScmObj Scm__GenericBuildDispatcher(ScmGeneric *gf, int axis);
SCM_EXTERN void   Scm__GenericInvalidateDispatcher(ScmGeneric *gf);
SCM_EXTERN ScmObj Scm__GenericDispatcherInfo(ScmGeneric *gf);
SCM_EXTERN void   Scm__GenericDispatcherDump(ScmGeneric *gf, ScmPort *port);


/* A proxy type holds a reference to another type, indirectly.  It is
   used in the following cases:

   - Global binding to a class appearing in a type expression: Since a class
     can be redefined, we can't use the direct reference to the class object.
     Instead we keep gloc of the binding---so that the type expression
     always use the most recent class definition.
   - Local binding appearing in a type expression: If the bound value
     is a generative type, we can't compute it at compile time.
     For every local-scope define-type, we create a shadow local
     binding to a proxy type that stands for the type value to be computed
     at runtime, and use that proxy type in a type expression.
   - Deferred proxy type.  This is used when we need a global type
     binding at compile time, before the binding is actually executed---
     e.g.
       (begin (define-class <foo> ...)
              (define (f x) (of-type? x (<?> <foo>))))
     Here, the global identifier <foo> would be bound when the entire begin
     form is executed.  But before it, we need to compile the type expression
     (<?> <foo>), and the compiler need to look up the compile-time binding
     of <foo>.  We can't retrieve the actual bound value of <foo>, which would
     be a class object computed at runtime, but we do know <foo> would be
     bound to something that can be used as a type---so we insert deferred
     proxy type in the compiling environment, which would be superseded
     with the real value at runtime.
*/
struct ScmProxyTypeRec {
    SCM_HEADER;
    ScmIdentifier *id;          /* Original Id (need to serialize in
                                   precomp output.
                                   NULL iff this is a local proxy type. */
    ScmGloc *ref;               /* GLOC that holds the actual class.
                                   It can be NULL, if it is computed
                                   from ID lazily.  Always NULL in a local
                                   proxy type. */
    ScmObj value;               /* The type this proxy stands for.
                                   Global proxy type - unused.
                                   Local proxy type - local type value.
                                   Deferred prox ytype - type value to
                                   be bound, iff it is computable at
                                   compile time.
                                   This value needs not be serialized.
                                */
};

#define SCM_LOCAL_PROXY_TYPE_P(obj) \
    (SCM_PROXY_TYPE_P(obj) && SCM_PROXY_TYPE(obj)->id == NULL)

/* Creates a local proxy type standing for TYPE.  TYPE must be a type, and
   it is held directly, so this can only be called at runtime---the compiler
   emits a call to it (via %make-local-proxy-type) instead of creating one
   itself. */
SCM_EXTERN ScmObj Scm_MakeLocalProxyType(ScmObj type);

#endif /*GAUCHE_PRIV_CLASSP_H*/
