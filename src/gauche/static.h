/*
 * gauche/static.h - API for statically linked libgauche
 *
 *   Copyright (c) 2014-2017  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_STATIC_H
#define GAUCHE_STATIC_H

SCM_DECL_BEGIN

SCM_EXTERN void Scm_InitPrelinked(void);
SCM_EXTERN void Scm_InitPrelinked_gdbm(void);

/* A convenience initialization.  */

/* Linking gdbm makes the generated binary to be under GPL.  If one only
   wants BSD compatible license (and LGPL-dependency), define
   GAUCHE_STATIC_EXCLUDE_GDBM before calling SCM_INIT_STATIC. */

#ifdef GAUCHE_STATIC_EXCLUDE_GDBM
#define SCM_INIT_STATIC()                       \
    do {                                        \
        GC_INIT();                              \
        Scm_Init(GAUCHE_SIGNATURE);             \
        Scm_InitPrelinked();                    \
    } while (0)
#else  /*!GAUCHE_STATIC_EXCLUDE_GDBM*/
#define SCM_INIT_STATIC()                       \
    do {                                        \
        GC_INIT();                              \
        Scm_Init(GAUCHE_SIGNATURE);             \
        Scm_InitPrelinked();                    \
        Scm_InitPrelinked_gdbm();               \
    } while (0)
#endif
    

SCM_DECL_END

#endif /*GAUCHE_STATIC_H*/
