/*
 * priv/parameterP.h - private parameter C-level implementation
 *
 *   Copyright (c) 2018-2020  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PRIV_PARAMETERP_H
#define GAUCHE_PRIV_PARAMETERP_H

/* We keep the definition private, so that we can extend it later. */

struct ScmPrimitiveParameterRec {
    SCM_INSTANCE_HEADER;
    ScmObj name;                /* for debugging. #f or symbol. */
    ScmSize index;
    ScmObj initialValue;
    u_long flags;
};

/* Each VM has vector of parameter values.  vm->parameters points to this.
   The vector is extended on demand.
   We might swap this to more sophisticated data structure than
   a simple flat vector in future.
 */
struct ScmVMParameterTableRec {
    ScmSize size;
    ScmObj *vector;
};

SCM_EXTERN ScmVMParameterTable *Scm__MakeVMParameterTable(ScmVM *base);


#endif /*GAUCHE_PRIV_PARAMETERP_H*/
