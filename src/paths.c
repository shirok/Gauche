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
#include <stdio.h>
#include <string.h>
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

/*
 * A couple of utilities used commoly in platform-specific routines:
 */

/* remove N components from path */
static char *remove_components(const char *path, int n)
{
    ssize_t len = strlen(path);
    for (ssize_t i = len-1, cnt = 0; i >= 0; i--) {
        if (path[i] == '/'
#if defined(GAUCHE_WINDOWS)
            || path[i] == '\\'
#endif
            ) {
            cnt++;
            if (cnt == n) {
                char *buf = PATH_ALLOC(i+1);
                memcpy(buf, path, i);
                buf[i] = '\0';
                return buf;
            }
        }
    }
    return NULL;
}

/* remove SUFFIX from dir */
static char *remove_suffix(char *dir, const char *suffix)
{
    size_t len = strlen(dir);
    size_t suflen = strlen(suffix);
    if (len > suflen && strncmp(dir + len - suflen, suffix, suflen) == 0) {
        dir[len-suflen] = '\0';
        return dir;
    }
    return NULL;
}

/*
 * Platform-specifc routines to obtain runtime directories.
 *
 * For each platform, we need the following procedures:
 *   get_libgauche_path()  - path of the running libgauche*.
 *   get_executable_path() - path of the running executable.
 *   get_install_dir()    - the installation directory (the libraries
 *                          can be found under lib/ of this directory).
 * Those may return NULL if the directory can't be determined.
 * Can return either static string or a string allocated by PATH_ALLOC; doesn't
 * need to worry about freeing it.
 * Those may throw an error with PATH_ERROR if unexpected situation occurs.
 */


#if defined(GAUCHE_WINDOWS)

static const char *get_libgauche_path()
{
    TCHAR path[MAX_PATH];
    const TCHAR *libname = _T("libgauche-"GAUCHE_ABI_VERSION".dll");

    HMODULE mod = GetModuleHandle(libname);
    if (mod == NULL) return NULL;
    DWORD r = GetModuleFileName(mod, path, MAX_PATH);
    if (r == 0) PATH_ERROR("GetModuleFileName failed");
    return SCM_WCS2MBS(path);
}

static const char *get_executable_path()
{
    TCHAR path[MAX_PATH];
    HMODULE mod = GetModuleHandle(NULL);
    if (mod == NULL) return NULL;
    DWORD r = GetModuleFileName(mod, path, MAX_PATH);
    if (r == 0) PATH_ERROR("GetModuleFileName failed");
    return SCM_WCS2MBS(path);
}

static const char *get_install_dir()
{
    const char * path= get_libgauche_path();
    if (path == NULL) path = get_executable_path();
    if (path == NULL) return NULL;

    /* On Windows, both libgauche.dll and gosh.exe are in $PREFIX\bin, and
       libraries can be found under $PREFIX\lib.  So we have to skip
       two directory separators. */
    char *dir = remove_components(path, 1);
    if (dir == NULL) return NULL;
    char *dir1 = remove_suffix(dir, "\\bin");
    if (dir1 != NULL) return dir1;
    dir1 = remove_suffix(dir, "\\src"); /* while we're buliding */
    if (dir1 != NULL) return dir1;
    /* At this moment, we're probably in a statically linked binary. 
       We don't need a particular path, but we need something to substitute
       '@' in the load path. */
    return "";
}

#elif defined(HAVE_LIBPROC_H) && defined(HAVE_LIBPROC)

/* OSX */
#include <libproc.h>

static const char *get_libgauche_path()
{
    return NULL;
}

static const char *get_executable_path()
{
    pid_t selfpid = getpid();
    char buf[PROC_PIDPATHINFO_MAXSIZE];
    memset(buf, 0, PROC_PIDPATHINFO_MAXSIZE);
    int r = proc_pidpath(selfpid, buf, PROC_PIDPATHINFO_MAXSIZE);
    if (r < 0) return NULL;
    if (r == PROC_PIDPATHINFO_MAXSIZE) return NULL; /* too long? something's funny */
    char *path = PATH_ALLOC(r+1);
    memcpy(path, buf, r);
    return path;
}

static const char *get_install_dir()
{
    const char *self = get_executable_path();
    if (self == NULL) return NULL;
    /* remove executable name */
    char *dir = remove_components(self, 1);
    if (dir == NULL) return NULL;
    /* first, try $PREFIX/bin */
    char *dir1 = remove_suffix(dir, "/bin");
    if (dir1 != NULL) return dir;
    /* next, try $PREFIX/lib/gauche-$ABI/$VERSION/$ARCH */
    dir1 = remove_components(dir, 3);
    if (dir1 == NULL) return NULL;
    return remove_suffix(dir1, "/lib");
}

#elif defined(GAUCHE_MACOSX_FRAMEWORK)

#include <libgen.h>
#include <CoreFoundation/CoreFoundation.h>

/* Must match the id in Info.plist */
#define LIBGAUCHE_ID  "com.schemearts.gauche"

/* Subdirs appended to the bundle path */
#define SUBDIR   "/Versions/Current/"

static const char *get_libgauche_path()
{
    return NULL;                /* Placeholder for now */
}

static const char *get_executable_path()
{
    return NULL;                /* Placeholder for now */
}

static const char *get_install_dir()
{
    CFBundleRef bundle     = NULL;
    CFURLRef    bundleURL  = NULL;
    CFStringRef bundlePath = NULL;

#define CLEANUP                                 \
    do {                                        \
        if (bundlePath) CFRelease(bundlePath);  \
        if (bundleURL) CFRelease(bundleURL);    \
        if (bundle) CFRelease(bundle);          \
    } while (0)

    bundle = CFBundleGetBundleWithIdentifier(CFSTR(LIBGAUCHE_ID));
    if (bundle == NULL) {
        /* This call fails when gosh is called during the build process
           (thus, the framework hasn't been created).  For the time
           being, we just return a dummy directory. */
        CLEANUP;
        return ".";
    }
    /* Ownership of bundle follows the Get Rule of Core Foundation.
       ie. we must claim ownership (with the CFRetain function).
       We are then responsible for relinquishing ownership when we
       have finished with it. */
    CFRetain(bundle);

    bundleURL = CFBundleCopyBundleURL(bundle);
    if (bundleURL == NULL) {
        CLEANUP;
        PATH_ERROR("CFBundleCopyBundleURL failed");
    }
    /* Ownership of bundleURL follows the Create Rule of Core Foundation.
       ie. it is our responsibility to relinquish ownership (using CFRelease)
       when we have finished with it. */

    bundlePath = CFURLCopyFileSystemPath(bundleURL, kCFURLPOSIXPathStyle);
    if (bundlePath == NULL) {
        CLEANUP;
        PATH_ERROR("CFURLCopyFileSystemPath failed");
    }
    /* Ownership follows the Create Rule. */

    /* Estimate string length in utf8.  This is provisional; we'll refine
       the code later. */
    size_t utf16len = (size_t)CFStringGetLength(bundlePath);
    size_t maxlen = 3 * (utf16len+1)/2;
    size_t bufsiz = maxlen + strlen(SUBDIR) + 1;
    char* buf = PATH_ALLOC(bufsiz);

    if (!CFStringGetCString(bundlePath, buf, maxlen, kCFStringEncodingUTF8)) {
        CLEANUP;
        PATH_ERROR("CFStringGetCString failed");
    }
    strcat(buf, SUBDIR);
    CLEANUP;
    return buf;
#undef CLEANUP
}

#else
/* 
 * The fallback case.  We try procfs, for it is supported on several platforms
 * and doesn't use special API functions.
 */
#define MAPS_LINE_MAX 4096

static const char *get_libgauche_path()
{
    FILE *fp = fopen("/proc/self/maps", "r");
    if (fp == NULL) return NULL;
    const char *const libgauche = "libgauche-"GAUCHE_ABI_VERSION".so";
    
    char buf[MAPS_LINE_MAX+1];
    while (fgets(buf, MAPS_LINE_MAX, fp) != NULL) {
        const char *p = strstr(buf, libgauche);
        if (p) {
            const char *tail = p + strlen(libgauche);
            for (const char *q = p-1; q >= buf; q--) {
                if (*q == ' ') {
                    q++;
                    if (q == p) {
                        return libgauche;
                    } else {
                        char *r = PATH_ALLOC(tail-q+1);
                        strncpy(r, q, tail-q);
                        r[tail-q] = '\0';
                        return r;
                    }
                }
            }
        }
    }
    if (ferror(fp)) PATH_ERROR("Read error from /proc/self/maps");
    return NULL;
}

static const char *get_executable_path()
{
    const char *self = "/proc/self/exe";
    ssize_t buflen = MAPS_LINE_MAX;
    char *buf = PATH_ALLOC(buflen);
    ssize_t r = readlink(self, buf, buflen);
    if (r < 0) return NULL;     /* procfs may not be available */
    if (r == buflen) return NULL; /* name is suspiciously long; something's wrong. */
    buf[r] = '\0';
    return buf;
}

static const char *get_install_dir()
{
    /* path is either $PREFIX/lib/gauche-$ABI/$VERSION/$ARCH or $PRFIX/lib.  */
    const char *path = get_libgauche_path();
    if (path != NULL) {
        /* remove libgauche-$ABI.so */
        char *dir = remove_components(path, 1);
        if (dir == NULL) return NULL;
        /* first, try $PREFIX/lib. */
        char *dir1 = remove_suffix(dir, "/lib");
        if (dir1 != NULL) return dir1;
        /* now we try $PREFIX/lib/gauche-$ABI/$VERSION/$ARCH */
        dir = remove_components(path, 3);
        if (dir != NULL) {
            dir1 = remove_suffix(dir, "/lib");
            if (dir1 != NULL) return dir1;
        }
    }
    /* executable is in $PREFIX/lib/gauche-$ABI/$VERSION/$ARCH or $PREFIX/bin */
    path = get_executable_path();
    if (path != NULL) {
        /* remove binary name */
        char *dir = remove_components(path, 1);
        if (dir == NULL) return NULL;
        /* first, try $PREFIX/bin. */
        char *dir1 = remove_suffix(dir, "/bin");
        if (dir1 != NULL) return dir1;
        /* now we try $PREFIX/lib/gauche-$ABI/$VERSION/$ARCH */
        dir1 = remove_components(dir, 3);
        if (dir1 != NULL) {
            dir1 = remove_suffix(dir1, "/lib");
            if (dir1 != NULL) return dir1;
        }
    }
    return NULL;
}
#endif

/*
 * Common routines
 */

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
        memcpy(q, p, p1-p);
        q += p1-p;
        memcpy(q, subst, slen);
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
