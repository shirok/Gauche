/*
 * gauche/static.h - API for statically linked libgauche
 *
 * ***NOTE: This must be included before gauche.h***
 *
 *   Copyright (c) 2014-2022  Shiro Kawai  <shiro@acm.org>
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
 * The main program that statically links Gauche needs to include this file
 * _before_ gauche.h, otherwise it can't find Scm_Init().
 */

#ifndef GAUCHE_STATIC_H
#define GAUCHE_STATIC_H

/* A convenience initialization.  */

/* Linking gdbm makes the generated binary to be under GPL.  If one only
   wants BSD compatible license (and LGPL-dependency), define
   GAUCHE_STATIC_EXCLUDE_GDBM before calling SCM_INIT_STATIC. */
/* Linking mbedTLS maks the generated binary to depend on libmbedtls.so.
   It's Apache 2.0 license so it's not such a big issue like gdbm, but
   if you want to eliminate runtime dependency, define
   GAUCHE_STATIC_EXCLUDE_MBEDTLS before calling SCM_INIT_STATIC. */

#ifdef GAUCHE_STATIC_EXCLUDE_GDBM
#define GAUCHE__STATIC_INIT_GDBM /*empty*/
#else
#define GAUCHE__STATIC_INIT_GDBM Scm_InitPrelinked_gdbm()
#endif

#ifdef GAUCHE_STATIC_EXCLUDE_MBEDTLS
#define GAUCHE__STATIC_INIT_MBED /*empty*/
#else
#define GAUCHE__STATIC_INIT_MBED Scm_InitPrelinked_mbed()
#endif

#define SCM_INIT_STATIC()                       \
    do {                                        \
        GC_INIT();                              \
        Scm_Init(GAUCHE_SIGNATURE);             \
        Scm_InitPrelinked();                    \
        GAUCHE__STATIC_INIT_GDBM;               \
        GAUCHE__STATIC_INIT_MBED;               \
    } while (0)

/* These functions are directly linked, so do not use SCM_EXTERN. */
#ifdef __cplusplus
extern "C" {
#endif

extern void Scm_InitPrelinked(void);
extern void Scm_InitPrelinked_gdbm(void);
extern void Scm_InitPrelinked_mbed(void);

#ifdef __cplusplus
}
#endif

#define LIBGAUCHE_BODY /* To refer to Scm_Init without declspec(dllexport) */

#endif /*GAUCHE_STATIC_H*/
