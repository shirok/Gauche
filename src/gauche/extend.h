/*
 * extend.h - Stuff useful to write Gauche extension
 *
 *   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
 *
 *  $Id: extend.h,v 1.4 2003-07-05 03:29:13 shirok Exp $
 */

#ifndef GAUCHE_EXTEND_H
#define GAUCHE_EXTEND_H

#ifndef GAUCHE_H
#include <gauche.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define SCM_INIT_EXTENSION(name)                                        \
   do {                                                                 \
       extern void *SCM_CPP_CAT(Scm__datastart_, name);                 \
       extern void *SCM_CPP_CAT(Scm__dataend_, name);                   \
       extern void *SCM_CPP_CAT(Scm__bssstart_, name);                  \
       extern void *SCM_CPP_CAT(Scm__bssend_, name);                    \
       Scm_RegisterDL((void*)&SCM_CPP_CAT(Scm__datastart_, name),       \
                      (void*)&SCM_CPP_CAT(Scm__dataend_, name),         \
                      (void*)&SCM_CPP_CAT(Scm__bssstart_, name),        \
                      (void*)&SCM_CPP_CAT(Scm__bssend_, name));         \
   } while (0)

#ifdef __cplusplus
}
#endif

#endif /* GAUCHE_EXTEND_H */
