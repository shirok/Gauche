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


/* A proxy type holds a reference to another type, indirectly, through the
   global binding the type is named by.
   It is used to keep reference to a type in another compound type
   structure.  We need an indirection because a class may be redefined,
   and because the compiler may have to refer to a type before its value
   is computed (see Scm_MakeProxyType in gauche/class.h).

   There's a variation, a *local* proxy type, which carries the type it
   stands for directly instead of going through a global binding.  It is
   used for a type held in a local binding (e.g. an internal define-type
   whose right hand side is generative); such a type has no global name to
   be redefined through, so the indirection isn't needed---and can't be had.
   A local proxy type is distinguished by ID == NULL, it is created per
   activation of the scope that binds the type, and it can't be serialized
   (see Scm_MakeLocalProxyType).
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
    ScmObj value;               /* The type this proxy stands for.  Only
                                   used in a local proxy type; SCM_FALSE
                                   otherwise. */
};

#define SCM_LOCAL_PROXY_TYPE_P(obj) \
    (SCM_PROXY_TYPE_P(obj) && SCM_PROXY_TYPE(obj)->id == NULL)

/* Creates a local proxy type standing for TYPE.  TYPE must be a type, and
   it is held directly, so this can only be called at runtime---the compiler
   emits a call to it (via %make-local-proxy-type) instead of creating one
   itself. */
SCM_EXTERN ScmObj Scm_MakeLocalProxyType(ScmObj type);

#endif /*GAUCHE_PRIV_CLASSP_H*/
