/*
 * parameter.h - parameter C API
 *
 *   Copyright (c) 2007-2022  Shiro Kawai  <shiro@acm.org>
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

/*
 *  Parameters keep thread-local state.   It is called 'fluids' in some
 *  Scheme implementations.  A thread inherits the parameters from its
 *  creator.
 */

/* NB: For the time being, we let all threads share the index into the
 * parameter array---if one thread creates a parameter, we globally allocate
 * an index for it.
 */

#ifndef GAUCHE_PARAMETER_H
#define GAUCHE_PARAMETER_H

/* ScmPrimitiveParameter is an opaque struct (Definition is
   in priv/parameterP.h) that implements a simple thread-local
   storage.  It doesn't have extra features such as filter
   procedure or hooks.  It is useful for the parameter that needs
   to be accessed from C as well.

   ScmParameter is Scheme's <parameter> object.  It inherits
   primitive parameter, but adds some bells and whistles.
*/
typedef struct ScmPrimitiveParameterRec ScmPrimitiveParameter;

SCM_CLASS_DECL(Scm_PrimitiveParameterClass);
#define SCM_CLASS_PRIMITIVE_PARAMETER  (&Scm_PrimitiveParameterClass)
#define SCM_PRIMITIVE_PARAMETER(obj)   ((ScmPrimitiveParameter*)obj)
#define SCM_PRIMITIVE_PARAMETER_P(obj) SCM_ISA(obj,SCM_CLASS_PRIMITIVE_PARAMETER)

/* Flag value for Scm_MakePrimitiveParameter */
enum {
    /* value may be a promise; dereference automaticlaly forces it */
    SCM_PARAMETER_LAZY = (1UL << 0)
};

SCM_EXTERN ScmPrimitiveParameter *Scm_MakePrimitiveParameter(ScmClass *klass,
                                                             ScmObj name,
                                                             ScmObj initval,
                                                             u_long flags);
SCM_EXTERN ScmObj Scm_MakePrimitiveParameterSubr(ScmPrimitiveParameter *p);
SCM_EXTERN ScmObj Scm_PrimitiveParameterRef(ScmVM *vm,
                                            const ScmPrimitiveParameter *p);
SCM_EXTERN ScmObj Scm_PrimitiveParameterSet(ScmVM *vm,
                                            const ScmPrimitiveParameter *p,
                                            ScmObj val);

/* A convenience function to create a new ScmPrimitiveParameter
   and bind it to NAME in MOD.  Returns a newly created primitive
   parameter so that it is accessible from C.  */
SCM_EXTERN ScmPrimitiveParameter *Scm_BindPrimitiveParameter(ScmModule *mod,
                                                             const char *name,
                                                             ScmObj initval,
                                                             u_long flags);

/* TRANSIENT - exposed only for the backward compatibility - will be gone by 1.0 */
#if GAUCHE_API_VERSION < 98
typedef struct ScmParameterLocRec {
    ScmPrimitiveParameter *p;
} ScmParameterLoc;

SCM_EXTERN void Scm_DefinePrimitiveParameter(ScmModule *mod,
                                             const char *name,
                                             ScmObj initval,
                                             ScmParameterLoc *location /*out*/);
SCM_EXTERN void   Scm_MakeParameterSlot(ScmVM *vm,
                                        ScmParameterLoc *location /*out*/);
SCM_EXTERN void   Scm_InitParameterLoc(ScmVM *vm,
                                       ScmParameterLoc *location,
                                       ScmObj initval);
SCM_EXTERN ScmObj Scm_ParameterRef(ScmVM *vm, const ScmParameterLoc *location);
SCM_EXTERN ScmObj Scm_ParameterSet(ScmVM *vm, const ScmParameterLoc *location,
                                   ScmObj value);
#endif /*GAUCHE_API_VERSION < 98*/

#endif /*GAUCHE_PARAMETER_H*/
