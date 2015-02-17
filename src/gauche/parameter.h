/*
 * parameter.h - parameter C API
 *
 *   Copyright (c) 2007-2015  Shiro Kawai  <shiro@acm.org>
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

/* Parameter location, C-level "handle" to the parameter.
   This is not a first-class object in Scheme; Scheme's <parameter>
   object contains more stuff like filter procedures or hooks.
   Those extra stuff is not accessible from C API. */
typedef struct ScmParameterLocRec {
    int  index;
    ScmObj initialValue;
} ScmParameterLoc;


SCM_EXTERN void Scm_InitParameterLoc(ScmVM *vm, ScmParameterLoc *location, ScmObj initval);
SCM_EXTERN ScmObj Scm_ParameterRef(ScmVM *vm, const ScmParameterLoc *location);
SCM_EXTERN ScmObj Scm_ParameterSet(ScmVM *vm, const ScmParameterLoc *location,
                                   ScmObj value);

/* A "primitive parameter" is a mere SUBR that acts like parameter
   (except it doesn't have filters and hooks). */
SCM_EXTERN void Scm_DefinePrimitiveParameter(ScmModule *mod,
                                             const char *name,
                                             ScmObj initval,
                                             ScmParameterLoc *location /*out*/);

/*OBSOLETED - exposed only for the backward compatiblity - will be gone by 1.0*/
SCM_EXTERN void   Scm_MakeParameterSlot(ScmVM *vm, ScmParameterLoc *location /*out*/);


/* PRIVATE STUFF */

typedef struct ScmVMParameterTableRec {
    int size;
    int dummy_;                 /* for ABI */
    ScmObj *vector;
    void *dummy2_;              /* for ABI */
} ScmVMParameterTable;

SCM_EXTERN void Scm__VMParameterTableInit(ScmVMParameterTable *table,
                                          ScmVM *base);

#endif /*GAUCHE_PARAMETER_H*/
