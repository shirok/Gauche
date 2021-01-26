/*
 * gauche/procP.h - Private interface for procedures
 *
 *   Copyright (c) 2020  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PRIV_PROCP_H
#define GAUCHE_PRIV_PROCP_H

#if GAUCHE_API_VERSION >= 98
#define SCM_PROCEDURE_INIT(obj, req, opt, typ, inf)     \
    SCM_PROCEDURE(obj)->required = req,                 \
    SCM_PROCEDURE(obj)->optional = opt,                 \
    SCM_PROCEDURE(obj)->type = typ,                     \
    SCM_PROCEDURE(obj)->locked = FALSE,                 \
    SCM_PROCEDURE(obj)->currying = FALSE,               \
    SCM_PROCEDURE(obj)->constant = FALSE,               \
    SCM_PROCEDURE(obj)->leaf = FALSE,                   \
    SCM_PROCEDURE(obj)->reserved = 0,                   \
    SCM_PROCEDURE(obj)->reserved32 = 0,                 \
    SCM_PROCEDURE(obj)->info = inf,                     \
    SCM_PROCEDURE(obj)->setter = SCM_FALSE,             \
    SCM_PROCEDURE(obj)->inliner = SCM_FALSE,            \
    SCM_PROCEDURE(obj)->typeHint = NULL
#else  /* GAUCHE_API_VERSION < 98 */
#define SCM_PROCEDURE_INIT(obj, req, opt, typ, inf)     \
    SCM_PROCEDURE(obj)->required = req,                 \
    SCM_PROCEDURE(obj)->optional = opt,                 \
    SCM_PROCEDURE(obj)->type = typ,                     \
    SCM_PROCEDURE(obj)->locked = FALSE,                 \
    SCM_PROCEDURE(obj)->currying = FALSE,               \
    SCM_PROCEDURE(obj)->constant = FALSE,               \
    SCM_PROCEDURE(obj)->leaf = FALSE,                   \
    SCM_PROCEDURE(obj)->reserved = 0,                   \
    SCM_PROCEDURE(obj)->info = inf,                     \
    SCM_PROCEDURE(obj)->setter = SCM_FALSE,             \
    SCM_PROCEDURE(obj)->inliner = SCM_FALSE
#endif /* GAUCHE_API_VERSION < 98 */

        
#endif /*GAUCHE_PRIV_PROCP_H*/

