/*
 * vport.h - 'virtual port'
 *
 *   Copyright (c) 2004-2015  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_VPORT_H
#define GAUCHE_VPORT_H

#if defined(EXTVPORT_EXPORTS)
#define LIBGAUCHE_EXT_BODY
#endif
#include <gauche/extern.h>      /* redefine SCM_EXTERN */

SCM_CLASS_DECL(Scm_VirtualInputPortClass);
#define SCM_CLASS_VIRTUAL_INPUT_PORT    (&Scm_VirtualInputPortClass)

SCM_CLASS_DECL(Scm_VirtualOutputPortClass);
#define SCM_CLASS_VIRTUAL_OUTPUT_PORT   (&Scm_VirtualOutputPortClass)

SCM_CLASS_DECL(Scm_BufferedInputPortClass);
#define SCM_CLASS_BUFFERED_INPUT_PORT   (&Scm_BufferedInputPortClass)

SCM_CLASS_DECL(Scm_BufferedOutputPortClass);
#define SCM_CLASS_BUFFERED_OUTPUT_PORT  (&Scm_BufferedOutputPortClass)

#endif /* GAUCHE_VPORT_H */
