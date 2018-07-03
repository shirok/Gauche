/*
 * gauche/priv/dispatchP.h - Method dispatcher private API
 *
 *   Copyright (c) 2017-2018  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PRIV_DISPATCHP_H
#define GAUCHE_PRIV_DISPATCHP_H

/* We might use fast dispatch table when args to gf is equal to or
   smaller than this */
#define SCM_DISPATCHER_MAX_NARGS   4

typedef struct ScmMethodDispatcherRec ScmMethodDispatcher;

ScmMethodDispatcher *Scm__BuildMethodDispatcher(ScmObj methods, int axis);

void   Scm__MethodDispatcherAdd(ScmMethodDispatcher *dis, ScmMethod *m);
void   Scm__MethodDispatcherDelete(ScmMethodDispatcher *dis, ScmMethod *m);
ScmObj Scm__MethodDispatcherLookup(ScmMethodDispatcher *dis,
                                   ScmClass **typev, int argc);
void   Scm__MethodDispatcherDump(ScmMethodDispatcher *dis, ScmPort *port);

#endif  /*GAUCHE_PRIV_DISPATCHP_H*/
