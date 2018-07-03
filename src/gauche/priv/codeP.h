/*
 * codeP.h - ScmCompiledCode private API
 *
 *   Copyright (c) 2005-2018  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PRIV_CODEP_H
#define GAUCHE_PRIV_CODEP_H

SCM_DECL_BEGIN

#include <gauche/vm.h>

/* CompiledCode Builder API.
   Those are exposed through Scheme API, and other parts must use them
   instead of calling these directly.
 */

SCM_EXTERN ScmObj Scm_MakeCompiledCodeBuilder(int reqargs, int optargs,
                                              ScmObj name,
                                              ScmObj parent, ScmObj intForm);
SCM_EXTERN ScmObj Scm_CompiledCodeCurrentInsn(ScmCompiledCode *cc);
SCM_EXTERN void   Scm_CompiledCodeReplaceInsn(ScmCompiledCode *cc,
                                              ScmObj insn,
                                              ScmObj operand,
                                              ScmObj info);
SCM_EXTERN void   Scm_CompiledCodeFlushInsn(ScmCompiledCode *cc);
SCM_EXTERN void   Scm_CompiledCodePutInsn(ScmCompiledCode *cc,
                                          ScmObj insn,
                                          ScmObj operand,
                                          ScmObj info);
SCM_EXTERN ScmObj Scm_CompiledCodeNewLabel(ScmCompiledCode *cc);
SCM_EXTERN void   Scm_CompiledCodeSetLabel(ScmCompiledCode *cc, ScmObj label);
SCM_EXTERN void   Scm_CompiledCodePushInfo(ScmCompiledCode *cc, ScmObj info);
SCM_EXTERN void   Scm_CompiledCodeFinishBuilder(ScmCompiledCode *cc,
                                                int maxstack);
SCM_EXTERN void   Scm_CompiledCodeEmit(ScmCompiledCode *cc,
                                       int code,
                                       int arg0,
                                       int arg1,
                                       ScmObj operand,
                                       ScmObj info);

SCM_DECL_END

#endif /* GAUCHE_PRIV_CODEP_H */
