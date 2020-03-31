/*
 * paths.c - get 'known' pathnames, such as the system's library directory.
 *
 *   Copyright (c) 2005-2019  Shiro Kawai  <shiro@acm.org>
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

/* This file is used by both libgauche (included from libeval.scm) and
 * gauche-config (included from gauche-config.c).  The latter
 * doesn't use ScmObj, so the function works on bare C strings.
 * Note that this also included in libextra.scm for testing, so be careful
 * not to put any public definitions here to avoid duplicate definitions.
 * Do not include this from other files.
 *
 * The includer must define two macros:
 *   const void *PATH_ALLOC(size_t size)  - allocation routine
 *   void PATH_ERROR(const char *fmt, ...) - print error message and exit
 */

#define LIBGAUCHE_BODY
#include "gauche.h"

#if !defined(PATH_ALLOC)
#define PATH_ALLOC(n)  malloc(n)
#endif
#if !defined(PATH_ERROR)
static void errfn(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    exit(1);
}

#define PATH_ERROR(...) errfn(__VA_ARGS__)
#endif

#if defined(GAUCHE_WINDOWS)
#include "getdir_win.c"
#elif defined(GAUCHE_MACOSX_FRAMEWORK)
#include "getdir_darwin.c"
#else
#include "getdir_procfs.c"
#endif

#include <string.h>

static const char *substitute_all(const char *input,
                                  const char *mark,
                                  const char *subst)
{
    size_t ilen = strlen(input);
    size_t mlen = strlen(mark);
    size_t slen = strlen(subst);
        
    int noccurs = 0;
    const char *p = input;
    const char *pend = p + ilen;
    while (p < pend) {
        const char *p1 = strstr(p, mark);
        if (p1 == NULL) break;
        noccurs++;
        p = p1 + mlen;
    }

    if (noccurs == 0) return input;
    size_t buflen = noccurs * slen + ilen - noccurs * mlen;
    char *buf = (char*)PATH_ALLOC(buflen+1);
    char *q = buf;
    for (p = input; noccurs > 0; noccurs--) {
        const char *p1 = strstr(p, mark);
        strncpy(q, p, p1-p);
        q += p1-p;
        strncpy(q, subst, slen);
        q += slen;
        p = p1 + mlen;
    }
    strncpy(q, p, pend-p);
    buf[buflen] = '\0';
    return buf;
}


/* The configure-generated path may have '@' in the pathnames.  We replace
   it with the installation directory. 

   NB: This is a static function, but called from gauche-config.c (it includes
   paths.c).
*/
static const char *replace_install_dir(const char *orig)
{
    if (strstr(orig, "@") == NULL) return orig; /* no replace */
    const char *idir =  get_install_dir();
    if (idir == NULL) {
        PATH_ERROR("Couldn't obtain installation directory.");
    }
    return substitute_all(orig, "@", idir);
}
