/*
 * extend.h - Stuff useful to write Gauche extension
 *
 *   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_EXTEND_H
#define GAUCHE_EXTEND_H

#ifndef GAUCHE_H
#include <gauche.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__CYGWIN__)
# ifdef __x86_64__
#  define SCM_INIT_EXTENSION(name)                 \
      do {                                         \
          Scm_RegisterDL((void*)&__data_start__,   \
                         (void*)&__data_end__,     \
                         (void*)&__bss_start__,    \
                         (void*)&__bss_end__);     \
      } while (0)
# else
#  define SCM_INIT_EXTENSION(name)                \
      do {                                        \
          Scm_RegisterDL((void*)&_data_start__,   \
                         (void*)&_data_end__,     \
                         (void*)&_bss_start__,    \
                         (void*)&_bss_end__);     \
      } while (0)
# endif
#else /* !__CYGWIN__ */
#define SCM_INIT_EXTENSION(name) /* nothing */
#endif /* !__CYGWIN__ */

#ifdef __cplusplus
#define SCM_EXTENSION_ENTRY_QUAL extern "C"
#else
#define SCM_EXTENSION_ENTRY_QUAL
#endif

/* Windows need dllexport magic */
#if defined(GAUCHE_WINDOWS)
#define SCM_EXTENSION_ENTRY SCM_EXTENSION_ENTRY_QUAL __declspec(dllexport)
#else
#define SCM_EXTENSION_ENTRY SCM_EXTENSION_ENTRY_QUAL
#endif

#ifdef __cplusplus
}
#endif

#endif /* GAUCHE_EXTEND_H */
