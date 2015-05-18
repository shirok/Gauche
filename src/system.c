/*
 * system.c - system interface
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"
#include "gauche/bignum.h"
#include "gauche/priv/builtin-syms.h"

#include <locale.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <fcntl.h>
#include <math.h>
#include <dirent.h>

#if !defined(GAUCHE_WINDOWS)
#include <grp.h>
#include <pwd.h>
#include <sys/times.h>
#include <sys/wait.h>

# if !defined(HAVE_CRT_EXTERNS_H)
/* POSIX defines environ, and ISO C defines __environ.
   Modern C seems to have the latter declared in unistd.h */
extern char **environ;
# else  /* HAVE_CRT_EXTERNS_H */
/* On newer OSX, we can't directly access global 'environ' variable.
   We need to use _NSGetEnviron(), and this header defines it. */
#include <crt_externs.h>
# endif /* HAVE_CRT_EXTERNS_H */
#else   /* GAUCHE_WINDOWS */
#include <lm.h>
#include <tlhelp32.h>
static HANDLE *win_prepare_handles(int *fds);
static int win_wait_for_handles(HANDLE *handles, int nhandles, int options,
                                int *status /*out*/);
#endif  /* GAUCHE_WINDOWS */

#ifdef HAVE_GLOB_H
#include <glob.h>
#endif
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif

/*
 * Auxiliary system interface functions.   See syslib.stub for
 * Scheme binding.
 */

/*===============================================================
 * Conversion between off_t and Scheme integer.
 * off_t can be either 32bit or 64bit.
 */
off_t Scm_IntegerToOffset(ScmObj i)
{
    if (SCM_INTP(i)) {
        return (off_t)SCM_INT_VALUE(i);
    } else if (SCM_BIGNUMP(i)) {
#if SIZEOF_OFF_T == SIZEOF_LONG
        return (off_t)Scm_GetIntegerClamp(i, SCM_CLAMP_ERROR, NULL);
#elif SIZEOF_OFF_T == 8 && !SCM_EMULATE_INT64
        return (off_t)Scm_GetInteger64Clamp(i, SCM_CLAMP_ERROR, NULL);
#else
        /* I don't think there's such an architecture. */
# error "off_t size on this platform is not suported."
#endif
    }
    Scm_Error("bad value as offset: %S", i);
    return (off_t)-1;       /* dummy */
}

ScmObj Scm_OffsetToInteger(off_t off)
{
#if SIZEOF_OFF_T == SIZEOF_LONG
    return Scm_MakeInteger(off);
#elif SIZEOF_OFF_T == 8 && !SCM_EMULATE_INT64
    return Scm_MakeInteger64((ScmInt64)off);
#else
# error "off_t size on this platform is not suported."
#endif
}

/*===============================================================
 * Windows specific - conversion between mbs and wcs.
 */
#if defined(GAUCHE_WINDOWS) && defined(UNICODE)
#include "win-compat.c"

WCHAR *Scm_MBS2WCS(const char *s)
{
    return mbs2wcs(s, TRUE, Scm_Error);
}

const char *Scm_WCS2MBS(const WCHAR *s)
{
    return wcs2mbs(s, TRUE, Scm_Error);
}
#endif /* defined(GAUCHE_WINDOWS) && defined(UNICODE) */

/*===============================================================
 * OBSOLETED: Wrapper to the system call to handle signals.
 * Use SCM_SYSCALL_{I|P} macro instead.
 */
int Scm_SysCall(int r)
{
    Scm_Warn("Obsoleted API Scm_SysCall is called.");
    if (r < 0 && errno == EINTR) {
        ScmVM *vm = Scm_VM();
        errno = 0;
        SCM_SIGCHECK(vm);
    }
    return r;
}

void *Scm_PtrSysCall(void *r)
{
    Scm_Warn("Obsoleted API Scm_PtrSysCall is called.");
    if (r == NULL && errno == EINTR) {
        ScmVM *vm = Scm_VM();
        errno = 0;
        SCM_SIGCHECK(vm);
    }
    return r;
}

/*
 * A utility function for the procedures that accepts either port or
 * integer file descriptor.  Returns the file descriptor.  If port_or_fd
 * is a port that is not associated with the system file, and needfd is
 * true, signals error.  Otherwise it returns -1.
 */
int Scm_GetPortFd(ScmObj port_or_fd, int needfd)
{
    int fd = -1;
    if (SCM_INTP(port_or_fd)) {
        fd = SCM_INT_VALUE(port_or_fd);
    } else if (SCM_PORTP(port_or_fd)) {
        fd = Scm_PortFileNo(SCM_PORT(port_or_fd));
        if (fd < 0 && needfd) {
            Scm_Error("the port is not associated with a system file descriptor: %S",
                      port_or_fd);
        }
    } else {
        Scm_Error("port or small integer required, but got %S", port_or_fd);
    }
    return fd;
}

/*===============================================================
 * Directory primitives (dirent.h)
 *   We don't provide the iterator primitives, but a function which
 *   reads entire directory.
 */

/* Returns a list of directory entries.  If pathname is not a directory,
   or can't be opened by some reason, an error is signalled. */
ScmObj Scm_ReadDirectory(ScmString *pathname)
{
    ScmObj head = SCM_NIL, tail = SCM_NIL;
#if !defined(GAUCHE_WINDOWS)
    ScmVM *vm = Scm_VM();
    struct dirent *dire;
    DIR *dirp = opendir(Scm_GetStringConst(pathname));

    if (dirp == NULL) {
        SCM_SIGCHECK(vm);
        Scm_SysError("couldn't open directory %S", pathname);
    }
    while ((dire = readdir(dirp)) != NULL) {
        ScmObj ent = SCM_MAKE_STR_COPYING(dire->d_name);
        SCM_APPEND1(head, tail, ent);
    }
    SCM_SIGCHECK(vm);
    closedir(dirp);
    return head;
#else  /* GAUCHE_WINDOWS */
    WIN32_FIND_DATA fdata;
    DWORD winerrno;
    ScmObj pattern;

    int pathlen = SCM_STRING_LENGTH(pathname);
    if (pathlen == 0) {
        Scm_Error("Couldn't open directory \"\"");
    }
    ScmChar lastchar = Scm_StringRef(pathname, pathlen-1, FALSE);
    if (lastchar == SCM_CHAR('/') || lastchar == SCM_CHAR('\\')) {
        pattern = Scm_StringAppendC(pathname, "*", 1, 1);
    } else {
        pattern = Scm_StringAppendC(pathname, "\\*", 2, 2);
    }
    const char *path = Scm_GetStringConst(SCM_STRING(pattern));

    HANDLE dirp = FindFirstFile(SCM_MBS2WCS(path), &fdata);
    if (dirp == INVALID_HANDLE_VALUE) {
        if ((winerrno = GetLastError()) != ERROR_FILE_NOT_FOUND) goto err;
        return head;
    }
    const char *tpath = SCM_WCS2MBS(fdata.cFileName);
    SCM_APPEND1(head, tail, SCM_MAKE_STR_COPYING(tpath));
    while (FindNextFile(dirp, &fdata) != 0) {
        tpath = SCM_WCS2MBS(fdata.cFileName);
        SCM_APPEND1(head, tail, SCM_MAKE_STR_COPYING(tpath));
    }
    winerrno = GetLastError();
    FindClose(dirp);
    if (winerrno != ERROR_NO_MORE_FILES) goto err;
    return head;
 err:
    Scm_Error("Searching directory failed by windows error %d",
              winerrno);
    return SCM_UNDEFINED;       /* dummy */
#endif
}

/* getcwd compatibility layer.
   Some implementations of getcwd accepts NULL as buffer to allocate
   enough buffer memory in it, but that's not standardized and we avoid
   relying on it.
 */
ScmObj Scm_GetCwd(void)
{
#if defined(GAUCHE_WINDOWS)&&defined(UNICODE)
#  define CHAR_T wchar_t
#  define GETCWD _wgetcwd
#else  /*!(defined(GAUCHE_WINDOWS)&&defined(UNICODE))*/
#  define CHAR_T char
#  define GETCWD getcwd
#endif /*!(defined(GAUCHE_WINDOWS)&&defined(UNICODE))*/

#define GETCWD_INITIAL_BUFFER_SIZE 1024
    int bufsiz = GETCWD_INITIAL_BUFFER_SIZE;
    CHAR_T sbuf[GETCWD_INITIAL_BUFFER_SIZE];
    CHAR_T *buf = sbuf;
    CHAR_T *r;

    for (;;) {
        SCM_SYSCALL3(r, GETCWD(buf, bufsiz), r == NULL);
        if (r != NULL) break;
        if (errno == ERANGE) {
            bufsiz *= 2;
            buf = SCM_NEW_ATOMIC_ARRAY(CHAR_T, bufsiz);
        } else {
            Scm_SysError("getcwd failed");
        }
    }
#if defined(GAUCHE_WINDOWS) && defined(UNICODE)
    return Scm_MakeString(Scm_WCS2MBS(buf), -1, -1, 0);
#else  /*!(defined(GAUCHE_WINDOWS) && defined(UNICODE))*/
    return Scm_MakeString(buf, -1, -1, SCM_STRING_COPYING);
#endif /*!(defined(GAUCHE_WINDOWS) && defined(UNICODE))*/
#undef CHAR_T
}

/*===============================================================
 * Pathname manipulation
 *
 *  It gets complicated since the byte '/' and '\\' can appear in
 *  the trailing octets of a multibyte character.
 *  Assuming these operations won't be a bottleneck, we use simple and
 *  straightforward code rather than tricky and fast one.
 */

/* Returns the system's native pathname delimiter. */
const char *Scm_PathDelimiter(void)
{
#if !defined(GAUCHE_WINDOWS)
    return "/";
#else  /* GAUCHE_WINDOWS */
    return "\\";
#endif /* GAUCHE_WINDOWS */
}

/* On Windows, '/' is *allowed* to be an alternative separator. */
#if defined(GAUCHE_WINDOWS)
#define SEPARATOR '\\'
#define ROOTDIR   "\\"
#define SEPARATOR_P(c)  ((c) == SEPARATOR || (c) == '/')
#else
#define SEPARATOR '/'
#define ROOTDIR   "/"
#define SEPARATOR_P(c)  ((c) == SEPARATOR)
#endif

/* Returns the pointer to the first path separator character,
   or NULL if no separator is found. */
static const char *get_first_separator(const char *path, const char *end)
{
    const char *p = path;
    while (p < end) {
        if (SEPARATOR_P(*p)) return p;
        p += SCM_CHAR_NFOLLOWS(*p)+1;
    }
    return NULL;
}

/* Returns the pointer to the last path separator character,
   or NULL if no separator is found. */
static const char *get_last_separator(const char *path, const char *end)
{
    const char *p = path, *last = NULL;
    while (p < end) {
        if (SEPARATOR_P(*p)) last = p;
        p += SCM_CHAR_NFOLLOWS(*p)+1;
    }
    return last;
}

static const char *skip_separators(const char *p, const char *end)
{
    while (p < end) {
        if (!SEPARATOR_P(*p)) break;
        p += SCM_CHAR_NFOLLOWS(*p)+1;
    }
    return p;
}

/* Returns the end pointer sans trailing separators. */
static const char *truncate_trailing_separators(const char *path,
                                                const char *end)
{
    const char *p = get_first_separator(path, end);
    if (p == NULL) return end;
    for (;;) {
        const char *q = skip_separators(p, end);
        if (q == end) return p;
        p = get_first_separator(q, end);
        if (p == NULL) return end;
    }
}

/* A utility for tilde expansion.  They are called only on
   Unix variants, so we only need to check '/'. */
static void put_user_home(ScmDString *dst,
                          const char *name,
                          const char *end)
{
    struct passwd *pwd;

    if (name == end) {
        pwd = getpwuid(geteuid());
        if (pwd == NULL) {
            Scm_SigCheck(Scm_VM());
            Scm_SysError("couldn't get home directory.\n");
        }
    } else {
        int namesiz = (int)(end - name);
        char *uname = SCM_STRDUP_PARTIAL(name, namesiz);
        pwd = getpwnam(uname);
        if (pwd == NULL) {
            Scm_SigCheck(Scm_VM());
            Scm_Error("couldn't get home directory of user \"%s\".\n", uname);
        }
    }
    int dirlen = (int)strlen(pwd->pw_dir);
    Scm_DStringPutz(dst, pwd->pw_dir, dirlen);
    if (pwd->pw_dir[dirlen-1] != '/') Scm_DStringPutc(dst, '/');
}

/* SRC points to the pathname string beginning with '~'.  Expand it
   to the user's home directory, leaving the partial result in DST.
   Returns the pointer into SRC sans tilde prefix (e.g. removed "~user/").
   The returned pointer may points just past the last char of SRC. */
static const char *expand_tilde(ScmDString *dst,
                                const char *src,
                                const char *end)
{
    const char *sep = get_first_separator(src, end);

    if (sep == NULL) {
        put_user_home(dst, src+1, end);
        return end;
    } else {
        put_user_home(dst, src+1, sep);
        return skip_separators(sep, end);
    }
}

/* Put current dir to DST */
static void put_current_dir(ScmDString *dst)
{
    ScmString *dir = SCM_STRING(Scm_GetCwd());
    u_int size;
    const char *sdir = Scm_GetStringContent(dir, &size, NULL, NULL);

    Scm_DStringAdd(dst, dir);
    if (!SEPARATOR_P(sdir[size-1])) {
        Scm_DStringPutc(dst, SEPARATOR);
    }
}


#if defined(GAUCHE_WINDOWS)
/* win32 specific; copy pathname with replacing '/' by '\\'. */
static void copy_win32_path(ScmDString *dst,
                            const char *srcp,
                            const char *end)
{
    while (srcp < end) {
        ScmChar ch;
        if (SEPARATOR_P(*srcp)) {
            ch = SEPARATOR;
        } else {
            SCM_CHAR_GET(srcp, ch);
        }
        Scm_DStringPutc(dst, ch);
        srcp += SCM_CHAR_NBYTES(ch);
    }
}
#endif /* GAUCHE_WINDOWS */

ScmObj Scm_NormalizePathname(ScmString *pathname, int flags)
{
    u_int size;
    const char *str = Scm_GetStringContent(pathname, &size, NULL, NULL);
    const char *srcp = str;
    const char *endp = str + size;
    ScmDString buf;

    Scm_DStringInit(&buf);

    /* Preprocess.  We expand tilde (on unix platform), and prepend the
       current directory to the relative pathname if absolutize is required.
       For canonicalization, we also put any absolute prefix into buf, so
       that srcp points to the relative path part after this. */
#if !defined(GAUCHE_WINDOWS)
    if ((flags & SCM_PATH_EXPAND) && size >= 1 && *str == '~') {
        srcp = expand_tilde(&buf, srcp, endp);
    } else if (endp > srcp && *srcp == '/') {
        /* Path is absolute */
        if (flags & SCM_PATH_CANONICALIZE) {
            Scm_DStringPutc(&buf, SEPARATOR);
            srcp = skip_separators(srcp, endp);
        }
    } else {
        /* Path is relative */
        if (flags & SCM_PATH_ABSOLUTE) {
            put_current_dir(&buf);
        }
    }
    if (!(flags & SCM_PATH_CANONICALIZE)) {
        Scm_DStringPutz(&buf, srcp, endp - srcp);
        return Scm_DStringGet(&buf, 0);
    }
#else /* GAUCHE_WINDOWS */
    if ((flags & SCM_PATH_EXPAND) && size >= 1 && *str == '~') {
        const char *home;
        if (size >= 2 && strchr("/\\", str[1]) == NULL) {
            Scm_Error("On windows native platforms, getting other user's home "
                      "directory is not supported (yet): %S",
                      SCM_OBJ(pathname));
        }
        srcp++;
        if ((home = Scm_GetEnv("HOME")) != NULL) { /* MSYS */
            Scm_DStringPutz(&buf, home, -1);
        } else if ((home = Scm_GetEnv("HOMEDRIVE")) != NULL) { /* cmd.exe */
            Scm_DStringPutz(&buf, home, -1);
            if ((home = Scm_GetEnv("HOMEPATH")) != NULL) {
                Scm_DStringPutz(&buf, home, -1);
            }
        }
        /*FALLTHROUGH - if no env is set we use root dir. */
    } else if (endp > srcp+1 && isalpha(*srcp) && *(srcp+1) == ':') {
        /* We first process the Evil Drive Letter */
        Scm_DStringPutc(&buf, *srcp++);
        Scm_DStringPutc(&buf, *srcp++);
    }
    if (endp > srcp && (SEPARATOR_P(*srcp))) {
        if (flags & SCM_PATH_CANONICALIZE) {
            Scm_DStringPutc(&buf, SEPARATOR);
            srcp = skip_separators(srcp, endp);
        }
    } else if (srcp == str) {
        /* Path is relative (the srcp==str condition rules out the
           drive letter case */
        if (flags & SCM_PATH_ABSOLUTE) {
            put_current_dir(&buf);
        }
    }
    if (!(flags & SCM_PATH_CANONICALIZE)) {
        copy_win32_path(&buf, srcp, endp);
        return Scm_DStringGet(&buf, 0);
    }
#endif /* GAUCHE_WINDOWS */

    /* Canonicalization.  We used to have a tricky piece of code here
       that avoids extra allocations, but have replaced it for
       simple-minded code, since speed gain here won't contribute to
       the overall performance anyway.  */
    {
        ScmObj comps = SCM_NIL; /* reverse list of components */
        int cnt = 0;            /* # of components except ".."'s */
        int final = FALSE;
        int wentup = FALSE;     /* true if the last loop went up a dir */

        for (;;) {
            const char *p = get_first_separator(srcp, endp);
            if (p == NULL) {
                final = TRUE;
                p = endp;
            }

            if (p == srcp+1 && *srcp == '.') {
                /* do nothing */
            } else if (p == srcp+2 && srcp[0] == '.' && srcp[1] == '.') {
                if (cnt > 0) {
                    SCM_ASSERT(SCM_PAIRP(comps));
                    comps = SCM_CDR(comps);
                    cnt--;
                    wentup = TRUE;
                } else {
                    comps = Scm_Cons(SCM_MAKE_STR(".."), comps);
                    wentup = FALSE;
                }
            } else {
                comps = Scm_Cons(Scm_MakeString(srcp, (int)(p-srcp), -1, 0),
                                 comps);
                cnt++;
                wentup = FALSE;
            }
            if (final) {
                /* If we just went up a directory, we want to preserve the
                   trailing separator in the result.  So we add an empty
                   component. */
                if (wentup) comps = Scm_Cons(SCM_MAKE_STR(""), comps);
                break;
            }
            srcp = skip_separators(p, endp);
        }
        if (SCM_PAIRP(comps)) {
            comps = Scm_ReverseX(comps);
            Scm_DStringAdd(&buf, SCM_STRING(SCM_CAR(comps)));
            SCM_FOR_EACH(comps, SCM_CDR(comps)) {
                Scm_DStringPutc(&buf, SEPARATOR);
                Scm_DStringAdd(&buf, SCM_STRING(SCM_CAR(comps)));
            }
        }
    }
    return Scm_DStringGet(&buf, 0);
}

/* Returns system's temporary directory. */
ScmObj Scm_TmpDir(void)
{
#if defined(GAUCHE_WINDOWS)
# define TMP_PATH_MAX 1024
    TCHAR buf[TMP_PATH_MAX+1], *tbuf = buf;
    /* According to the windows document, this API checks environment
       variables TMP, TEMP, and USERPROFILE.  Fallback is the Windows
       directory. */
    DWORD r = GetTempPath(TMP_PATH_MAX, buf);
    if (r == 0) Scm_SysError("GetTempPath failed");
    if (r > TMP_PATH_MAX) {
        tbuf = SCM_NEW_ATOMIC_ARRAY(TCHAR, r+1);
        DWORD r2 = GetTempPath(r, tbuf);
        if (r2 != r) Scm_SysError("GetTempPath failed");
    }
    return SCM_MAKE_STR_COPYING(SCM_WCS2MBS(tbuf));
#else  /*!GAUCHE_WINDOWS*/
    const char *s;
    if ((s = Scm_GetEnv("TMPDIR")) != NULL) return SCM_MAKE_STR_COPYING(s);
    if ((s = Scm_GetEnv("TMP")) != NULL) return SCM_MAKE_STR_COPYING(s);
    else return SCM_MAKE_STR("/tmp"); /* fallback */
#endif /*!GAUCHE_WINDOWS*/
}

/* Basename and dirname.
   On Win32, we need to treat drive names specially, e.g.:
   (sys-dirname "C:/a") == (sys-dirname "C:/") == (sys-dirname "C:") == "C:\\"
   (sys-basename "C:/") == (sys-basename "C:) == ""
*/

ScmObj Scm_BaseName(ScmString *filename)
{
    u_int size;
    const char *path = Scm_GetStringContent(filename, &size, NULL, NULL);

#if defined(GAUCHE_WINDOWS)
    /* Ignore drive letter, for it can never be a part of basename. */
    if (size >= 2 && path[1] == ':' && isalpha(path[0])) {
        path += 2;
        size -= 2;
    }
#endif /* GAUCHE_WINDOWS) */

    if (size == 0) return SCM_MAKE_STR("");
    const char *endp = truncate_trailing_separators(path, path+size);
    const char *last = get_last_separator(path, endp);
    if (last == NULL) {
        return Scm_MakeString(path, (int)(endp-path), -1, 0);
    } else {
        return Scm_MakeString(last+1, (int)(endp-last-1), -1, 0);
    }
}

ScmObj Scm_DirName(ScmString *filename)
{
    u_int size;
    const char *path = Scm_GetStringContent(filename, &size, NULL, NULL);
#if defined(GAUCHE_WINDOWS)
    int drive_letter = -1;
    if (size >= 2 && path[1] == ':' && isalpha(path[0])) {
        drive_letter = path[0];
        path += 2;
        size -= 2;
    }
#endif /* GAUCHE_WINDOWS */

    if (size == 0) { path = NULL; goto finale; }
    const char *endp = truncate_trailing_separators(path, path+size);
    if (endp == path) { path = ROOTDIR, size = 1; goto finale; }
    const char *last = get_last_separator(path, endp);
    if (last == NULL) { path = ".", size = 1; goto finale; }

    /* we have "something/", and 'last' points to the last separator. */
    last = truncate_trailing_separators(path, last);
    if (last == path) {
        path = ROOTDIR, size = 1;
    } else {
        size = (int)(last - path);
    }
 finale:
#if defined(GAUCHE_WINDOWS)
    if (drive_letter > 0) {
        ScmObj z;
        char p[3] = "x:";
        p[0] = (char)drive_letter;
        z = Scm_MakeString(p, 2, 2, SCM_MAKSTR_COPYING);
        if (path) {
            return Scm_StringAppendC(SCM_STRING(z), path, size, -1);
        } else {
            return Scm_StringAppendC(SCM_STRING(z), ROOTDIR, 1, -1);
        }
    }
#endif /* GAUCHE_WINDOWS */
    if (path) return Scm_MakeString(path, size, -1, 0);
    else      return Scm_MakeString(".", 1, 1, 0);
}

#undef ROOTDIR
#undef SEPARATOR

/* Make mkstemp() work even if the system doesn't have one. */
int Scm_Mkstemp(char *templat)
{
    int fd = -1;
#if defined(HAVE_MKSTEMP)
    SCM_SYSCALL(fd, mkstemp(templat));
    if (fd < 0) Scm_SysError("mkstemp failed");
    return fd;
#else   /*!defined(HAVE_MKSTEMP)*/
    /* Emulate mkstemp. */
    int siz = (int)strlen(templat);
    if (siz < 6) {
        Scm_Error("mkstemp - invalid template: %s", templat);
    }
#define MKSTEMP_MAX_TRIALS 65535   /* avoid infinite loop */
    {
        u_long seed = (u_long)time(NULL);
        int numtry, flags;
        char suffix[7];
#if defined(GAUCHE_WINDOWS)
        flags = O_CREAT|O_EXCL|O_WRONLY|O_BINARY;
#else  /* !GAUCHE_WINDOWS */
        flags = O_CREAT|O_EXCL|O_WRONLY;
#endif /* !GAUCHE_WINDOWS */
        for (numtry=0; numtry<MKSTEMP_MAX_TRIALS; numtry++) {
            snprintf(suffix, 7, "%06lx", (seed>>8)&0xffffff);
            memcpy(templat+siz-6, suffix, 7);
            SCM_SYSCALL(fd, open(templat, flags, 0600));
            if (fd >= 0) break;
            seed *= 2654435761UL;
        }
        if (numtry == MKSTEMP_MAX_TRIALS) {
            Scm_Error("mkstemp failed");
        }
    }
    return fd;
#endif /*!defined(HAVE_MKSTEMP)*/
}


ScmObj Scm_SysMkstemp(ScmString *templat)
{
#define MKSTEMP_PATH_MAX 1025  /* Geez, remove me */
    char name[MKSTEMP_PATH_MAX];
    u_int siz;
    const char *t = Scm_GetStringContent(templat, &siz, NULL, NULL);
    if (siz >= MKSTEMP_PATH_MAX-6) {
        Scm_Error("pathname too long: %S", templat);
    }
    memcpy(name, t, siz);
    memcpy(name + siz, "XXXXXX", 6);
    name[siz+6] = '\0';
    int fd = Scm_Mkstemp(name);
    ScmObj sname = SCM_MAKE_STR_COPYING(name);
    SCM_RETURN(Scm_Values2(Scm_MakePortWithFd(sname, SCM_PORT_OUTPUT, fd,
                                              SCM_PORT_BUFFER_FULL, TRUE),
                           sname));
}

/*===============================================================
 * Stat (sys/stat.h)
 */

static ScmObj stat_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmSysStat *s = SCM_ALLOCATE(ScmSysStat, klass);
    SCM_SET_CLASS(s, SCM_CLASS_SYS_STAT);
    return SCM_OBJ(s);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_SysStatClass,
                         NULL, NULL, NULL,
                         stat_allocate,
                         SCM_CLASS_DEFAULT_CPL);

ScmObj Scm_MakeSysStat(void)
{
    return stat_allocate(&Scm_SysStatClass, SCM_NIL);
}

static ScmObj stat_type_get(ScmSysStat *stat)
{
    if (S_ISDIR(SCM_SYS_STAT_STAT(stat)->st_mode)) return (SCM_SYM_DIRECTORY);
    if (S_ISREG(SCM_SYS_STAT_STAT(stat)->st_mode)) return (SCM_SYM_REGULAR);
    if (S_ISCHR(SCM_SYS_STAT_STAT(stat)->st_mode)) return (SCM_SYM_CHARACTER);
    if (S_ISBLK(SCM_SYS_STAT_STAT(stat)->st_mode)) return (SCM_SYM_BLOCK);
    if (S_ISFIFO(SCM_SYS_STAT_STAT(stat)->st_mode)) return (SCM_SYM_FIFO);
#ifdef S_ISLNK
    if (S_ISLNK(SCM_SYS_STAT_STAT(stat)->st_mode)) return (SCM_SYM_SYMLINK);
#endif
#ifdef S_ISSOCK
    if (S_ISSOCK(SCM_SYS_STAT_STAT(stat)->st_mode)) return (SCM_SYM_SOCKET);
#endif
    return (SCM_FALSE);
}

static ScmObj stat_perm_get(ScmSysStat *stat)
{
    return Scm_MakeIntegerFromUI(SCM_SYS_STAT_STAT(stat)->st_mode & 0777);
}

static ScmObj stat_size_get(ScmSysStat *stat)
{
    return Scm_OffsetToInteger(SCM_SYS_STAT_STAT(stat)->st_size);
}


#define STAT_GETTER_UI(name) \
  static ScmObj SCM_CPP_CAT3(stat_, name, _get)(ScmSysStat *s) \
  { return Scm_MakeIntegerFromUI((u_long)(SCM_SYS_STAT_STAT(s)->SCM_CPP_CAT(st_, name))); }
#define STAT_GETTER_TIME(name) \
  static ScmObj SCM_CPP_CAT3(stat_, name, _get)(ScmSysStat *s) \
  { return Scm_MakeSysTime(SCM_SYS_STAT_STAT(s)->SCM_CPP_CAT(st_, name)); }

STAT_GETTER_UI(mode)
STAT_GETTER_UI(ino)
STAT_GETTER_UI(dev)
STAT_GETTER_UI(rdev)
STAT_GETTER_UI(nlink)
STAT_GETTER_UI(uid)
STAT_GETTER_UI(gid)
STAT_GETTER_TIME(atime)
STAT_GETTER_TIME(mtime)
STAT_GETTER_TIME(ctime)

static ScmClassStaticSlotSpec stat_slots[] = {
    SCM_CLASS_SLOT_SPEC("type",  stat_type_get,  NULL),
    SCM_CLASS_SLOT_SPEC("perm",  stat_perm_get,  NULL),
    SCM_CLASS_SLOT_SPEC("mode",  stat_mode_get,  NULL),
    SCM_CLASS_SLOT_SPEC("ino",   stat_ino_get,   NULL),
    SCM_CLASS_SLOT_SPEC("dev",   stat_dev_get,   NULL),
    SCM_CLASS_SLOT_SPEC("rdev",  stat_rdev_get,  NULL),
    SCM_CLASS_SLOT_SPEC("nlink", stat_nlink_get, NULL),
    SCM_CLASS_SLOT_SPEC("uid",   stat_uid_get,   NULL),
    SCM_CLASS_SLOT_SPEC("gid",   stat_gid_get,   NULL),
    SCM_CLASS_SLOT_SPEC("size",  stat_size_get,  NULL),
    SCM_CLASS_SLOT_SPEC("atime", stat_atime_get, NULL),
    SCM_CLASS_SLOT_SPEC("mtime", stat_mtime_get, NULL),
    SCM_CLASS_SLOT_SPEC("ctime", stat_ctime_get, NULL),
    SCM_CLASS_SLOT_SPEC_END()
};

/*===============================================================
 * Time (sys/time.h and time.h)
 */

/* Gauche has two notion of time.  A simple number is used by the low-level
 * system interface (sys-time, sys-gettimeofday).  An object of <time> class
 * is used for higher-level interface, including threads.
 */

/* <time> object */

static ScmObj time_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmTime *t = SCM_ALLOCATE(ScmTime, klass);
    SCM_SET_CLASS(t, SCM_CLASS_TIME);
    t->type = SCM_SYM_TIME_UTC;
    SCM_SET_INT64_ZERO(t->sec);
    t->nsec = 0;
    return SCM_OBJ(t);
}

static void time_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmTime *t = SCM_TIME(obj);
    ScmObj sec = Scm_MakeInteger64(t->sec);
    long nsec = t->nsec;
    /* t->sec can be negative for time-difference. */
    if (Scm_Sign(sec) < 0 && t->nsec > 0) {
        sec = Scm_Abs(Scm_Add(sec, SCM_MAKE_INT(1)));
        nsec = 1000000000L - nsec;
        Scm_Printf(port, "#<%S -%S.%09lu>", t->type, sec, nsec);
    } else {
        Scm_Printf(port, "#<%S %S.%09lu>", t->type, sec, nsec);
    }
}

static int time_compare(ScmObj x, ScmObj y, int equalp)
{
    ScmTime *tx = SCM_TIME(x);
    ScmTime *ty = SCM_TIME(y);

    if (equalp) {
        if (SCM_EQ(tx->type, ty->type)
            && SCM_INT64_EQV(tx->sec, ty->sec)
            && tx->nsec == ty->nsec) {
            return 0;
        } else {
            return 1;
        }
    } else {
        if (!SCM_EQ(tx->type, ty->type)) {
            Scm_Error("cannot compare different types of time objects: %S vs %S", x, y);
        }
        if (SCM_INT64_CMP(<, tx->sec, ty->sec)) return -1;
        if (SCM_INT64_EQV(tx->sec, ty->sec)) {
            if (tx->nsec < ty->nsec) return -1;
            if (tx->nsec == ty->nsec) return 0;
            else return 1;
        }
        else return 1;
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_TimeClass,
                         time_print, time_compare, NULL,
                         time_allocate, SCM_CLASS_DEFAULT_CPL);

static ScmTime *make_time_int(ScmObj type)
{
    ScmTime *t = SCM_TIME(time_allocate(SCM_CLASS_TIME, SCM_NIL));
    t->type = SCM_FALSEP(type)? SCM_SYM_TIME_UTC : type;
    return t;
}


ScmObj Scm_MakeTime(ScmObj type, long sec, long nsec)
{
    ScmTime *t = make_time_int(type);
    SCM_SET_INT64_BY_LONG(t->sec, sec);
    t->nsec = nsec;
    return SCM_OBJ(t);
}

ScmObj Scm_MakeTime64(ScmObj type, ScmInt64 sec, long nsec)
{
    ScmTime *t = make_time_int(type);
    t->sec = sec;
    t->nsec = nsec;
    return SCM_OBJ(t);
}

/* Abstract gettimeofday() */
void Scm_GetTimeOfDay(u_long *sec, u_long *usec)
{
#if defined(HAVE_GETTIMEOFDAY)
    struct timeval tv;
    int r;
    SCM_SYSCALL(r, gettimeofday(&tv, NULL));
    if (r < 0) Scm_SysError("gettimeofday failed");
    *sec = (u_long)tv.tv_sec;
    *usec = (u_long)tv.tv_usec;
#elif defined(GAUCHE_WINDOWS)
    FILETIME ft;
    GetSystemTimeAsFileTime(&ft);
    SCM_FILETIME_TO_UNIXTIME(ft, *sec, *usec);
#else  /* !HAVE_GETTIMEOFDAY && !GAUCHE_WINDOWS */
    /* Last resort */
    *sec = (u_long)time(NULL);
    *usec = 0;
#endif /* !HAVE_GETTIMEOFDAY && !GAUCHE_WINDOWS */
}

/* Abstract clock_gettime and clock_getres.
   If the system doesn't have these, those API returns FALSE; the caller
   should make up fallback means.
 */
int Scm_ClockGetTimeMonotonic(u_long *sec, u_long *nsec)
{
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
    ScmTimeSpec ts;
    int r;
    SCM_SYSCALL(r, clock_gettime(CLOCK_MONOTONIC, &ts));
    if (r < 0) Scm_SysError("clock_gettime failed");
    *sec = (u_long)ts.tv_sec;
    *nsec = (u_long)ts.tv_nsec;
    return TRUE;
#else  /*!HAVE_CLOCK_GETTIME*/
    *sec = *nsec = 0;
    return FALSE;
#endif /*!HAVE_CLOCK_GETTIME*/
}

int Scm_ClockGetResMonotonic(u_long *sec, u_long *nsec)
{
#if defined(HAVE_CLOCK_GETRES) && defined(CLOCK_MONOTONIC)
    ScmTimeSpec ts;
    int r;
    SCM_SYSCALL(r, clock_getres(CLOCK_MONOTONIC, &ts));
    if (r < 0) Scm_SysError("clock_getres failed");
    *sec = (u_long)ts.tv_sec;
    *nsec = (u_long)ts.tv_nsec;
    return TRUE;
#else  /*!HAVE_CLOCK_GETTIME*/
    *sec = *nsec = 0;
    return FALSE;
#endif /*!HAVE_CLOCK_GETTIME*/
}


/* Experimental.  This returns the microsecond-resolution time, wrapped
   around the fixnum resolution.  In 32-bit architecture it's a bit more
   than 1000seconds.  Good for micro-profiling, since this guarantees
   no allocation.  Returned value can be negative. */
long Scm_CurrentMicroseconds()
{
    u_long sec, usec;
    Scm_GetTimeOfDay(&sec, &usec);
    /* we ignore overflow */
    usec += sec * 1000000;
    usec &= (1UL<<(SCM_SMALL_INT_SIZE+1)) - 1;
    if (usec > SCM_SMALL_INT_MAX) usec -= (1UL<<(SCM_SMALL_INT_SIZE+1));
    return (long)usec;
}

ScmObj Scm_CurrentTime(void)
{
    u_long sec, usec;
    Scm_GetTimeOfDay(&sec, &usec);
    return Scm_MakeTime(SCM_SYM_TIME_UTC, sec, usec*1000);
}

ScmObj Scm_IntSecondsToTime(long sec)
{
    return Scm_MakeTime(SCM_SYM_TIME_UTC, sec, 0);
}

ScmObj Scm_Int64SecondsToTime(ScmInt64 sec)
{
    return Scm_MakeTime64(SCM_SYM_TIME_UTC, sec, 0);
}

ScmObj Scm_RealSecondsToTime(double sec)
{
    double s;
    double frac = modf(sec, &s);
    ScmInt64 secs = Scm_DoubleToInt64(s);
    return Scm_MakeTime64(SCM_SYM_TIME_UTC, secs, (long)(frac * 1.0e9));
}

static ScmObj time_type_get(ScmTime *t)
{
    return t->type;
}

static void time_type_set(ScmTime *t, ScmObj val)
{
    if (!SCM_SYMBOLP(val)) {
        Scm_Error("time type must be a symbol, but got %S", val);
    }
    t->type = val;
}

static ScmObj time_sec_get(ScmTime *t)
{
    return Scm_MakeInteger64(t->sec);
}

static void time_sec_set(ScmTime *t, ScmObj val)
{
    if (!SCM_REALP(val)) {
        Scm_Error("real number required, but got %S", val);
    }
    t->sec = Scm_GetInteger64(val);
}

static ScmObj time_nsec_get(ScmTime *t)
{
    return Scm_MakeInteger(t->nsec);
}

static void time_nsec_set(ScmTime *t, ScmObj val)
{
    if (!SCM_REALP(val)) {
        Scm_Error("real number required, but got %S", val);
    }
    long l = Scm_GetInteger(val);
    if (l >= 1000000000) {
        Scm_Error("nanoseconds out of range: %ld", l);
    }
    t->nsec = l;
}

static ScmClassStaticSlotSpec time_slots[] = {
    SCM_CLASS_SLOT_SPEC("type",       time_type_get, time_type_set),
    SCM_CLASS_SLOT_SPEC("second",     time_sec_get, time_sec_set),
    SCM_CLASS_SLOT_SPEC("nanosecond", time_nsec_get, time_nsec_set),
    SCM_CLASS_SLOT_SPEC_END()
};

/* time_t and conversion routines */
/* NB: I assume time_t is typedefed to either an integral type or
 * a floating point type.  As far as I know it is true on most
 * current architectures.  POSIX doesn't specify so, however; it
 * may be some weird structure.  If you find such an architecture,
 * tweak configure.in and modify the following two functions.
 */
ScmObj Scm_MakeSysTime(time_t t)
{
#ifdef INTEGRAL_TIME_T
    return Scm_MakeIntegerFromUI((unsigned long)t);
#else
    double val = (double)t;
    return Scm_MakeFlonum(val);
#endif
}

time_t Scm_GetSysTime(ScmObj val)
{
    if (SCM_TIMEP(val)) {
#ifdef INTEGRAL_TIME_T
        /* NB: If time_t is 64bit, we assume ScmUInt64 is int64_t.
           So if we're emulating int64 then we reject conversion over 32bits*/
#  if SCM_EMULATE_INT64
        if (SCM_TIME(val)->sec.hi > 0) {
            Scm_Error("time object is out of range for Unix time: %S", val);
        }
        return (time_t)(SCM_TIME(val)->sec.lo);
#  else
        return (time_t)SCM_TIME(val)->sec;
#  endif
#else
        return (time_t)(Scm_Int64ToDouble(SCM_TIME(val)->sec) +
                        (double)SCM_TIME(val)->nsec/1.0e9);
#endif
    } else if (SCM_NUMBERP(val)) {
#ifdef INTEGRAL_TIME_T
        return (time_t)Scm_GetUInteger(val);
#else
        return (time_t)Scm_GetDouble(val);
#endif
    } else {
        Scm_Error("bad time value: either a <time> object or a real number is required, but got %S", val);
        return (time_t)0;       /* dummy */
    }
}

ScmObj Scm_TimeToSeconds(ScmTime *t)
{
    if (t->nsec) {
        return Scm_MakeFlonum(Scm_Int64ToDouble(t->sec) + (double)t->nsec/1.0e9);
    } else {
        return Scm_MakeInteger64(t->sec);
    }
}

/* Scheme time -> timespec conversion */
ScmTimeSpec *Scm_GetTimeSpec(ScmObj t, ScmTimeSpec *spec)
{
    if (SCM_FALSEP(t)) return NULL;
    if (SCM_TIMEP(t)) {
#if SCM_EMULATE_INT64
        /* if we don't have int64_t, it's very likely that timespec can't
           handle 64bit time. */
        if (SCM_TIME(t)->sec.hi > 0) {
            Scm_Error("cannot convert Scheme time to struct timespec: out of range: %S", t);
        }
        spec->tv_sec = SCM_TIME(t)->sec.lo;

#else  /*!SCM_EMULATE_INT64*/
        /* TODO: we might want to check if tv_sec can handle 64bit integer */
        spec->tv_sec = SCM_TIME(t)->sec;
#endif /*!SCM_EMULATE_INT64*/
        spec->tv_nsec = SCM_TIME(t)->nsec;
    } else if (!SCM_REALP(t)) {
        Scm_Error("bad timeout spec: <time> object or real number is required, but got %S", t);
    } else {
        ScmTime *ct = SCM_TIME(Scm_CurrentTime());
#if SCM_EMULATE_INT64
        spec->tv_sec = ct->sec.lo;  /* TODO: 2038 */
#else  /*!SCM_EMULATE_INT64*/
        spec->tv_sec = ct->sec;
#endif /*!SCM_EMULATE_INT64*/
        spec->tv_nsec = ct->nsec;
        if (SCM_INTP(t)) {
            spec->tv_sec += Scm_GetUInteger(t);
        } else if (!SCM_REALP(t)) {
            Scm_Panic("implementation error: Scm_GetTimeSpec: something wrong");
        } else {
            double s;
            spec->tv_nsec += (unsigned long)(modf(Scm_GetDouble(t), &s)*1.0e9);
            spec->tv_sec += (unsigned long)s;
            while (spec->tv_nsec >= 1000000000) {
                spec->tv_nsec -= 1000000000;
                spec->tv_sec += 1;
            }
        }
    }
    return spec;
}

/* <sys-tm> object */

static ScmObj tm_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmSysTm *st = SCM_ALLOCATE(ScmSysTm, klass);
    SCM_SET_CLASS(st, SCM_CLASS_SYS_TM);
    return SCM_OBJ(st);
}

static void tm_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
#define TM_BUFSIZ 50
    char buf[TM_BUFSIZ];
    ScmSysTm *st = SCM_SYS_TM(obj);
    strftime(buf, TM_BUFSIZ, "%a %b %e %T %Y", &st->tm);
    Scm_Printf(port, "#<sys-tm \"%s\">", buf);
#undef TM_BUFSIZ
}

SCM_DEFINE_BUILTIN_CLASS(Scm_SysTmClass,
                         tm_print, NULL, NULL,
                         tm_allocate, SCM_CLASS_DEFAULT_CPL);

ScmObj Scm_MakeSysTm(struct tm *tm)
{
    ScmSysTm *st = SCM_NEW(ScmSysTm);
    SCM_SET_CLASS(st, SCM_CLASS_SYS_TM);
    st->tm = *tm;               /* copy */
    return SCM_OBJ(st);
}

#define TM_ACCESSOR(name)                                               \
  static ScmObj SCM_CPP_CAT(name, _get)(ScmSysTm *tm) {                 \
    return Scm_MakeInteger(tm->tm.name);                                \
  }                                                                     \
  static void SCM_CPP_CAT(name, _set)(ScmSysTm *tm, ScmObj val) {       \
    if (!SCM_EXACTP(val))                                               \
      Scm_Error("exact integer required, but got %S", val);             \
    tm->tm.name = Scm_GetInteger(val);                                  \
  }

TM_ACCESSOR(tm_sec)
TM_ACCESSOR(tm_min)
TM_ACCESSOR(tm_hour)
TM_ACCESSOR(tm_mday)
TM_ACCESSOR(tm_mon)
TM_ACCESSOR(tm_year)
TM_ACCESSOR(tm_wday)
TM_ACCESSOR(tm_yday)
TM_ACCESSOR(tm_isdst)

static ScmClassStaticSlotSpec tm_slots[] = {
    SCM_CLASS_SLOT_SPEC("sec", tm_sec_get, tm_sec_set),
    SCM_CLASS_SLOT_SPEC("min", tm_min_get, tm_min_set),
    SCM_CLASS_SLOT_SPEC("hour", tm_hour_get, tm_hour_set),
    SCM_CLASS_SLOT_SPEC("mday", tm_mday_get, tm_mday_set),
    SCM_CLASS_SLOT_SPEC("mon", tm_mon_get, tm_mon_set),
    SCM_CLASS_SLOT_SPEC("year", tm_year_get, tm_year_set),
    SCM_CLASS_SLOT_SPEC("wday", tm_wday_get, tm_wday_set),
    SCM_CLASS_SLOT_SPEC("yday", tm_yday_get, tm_yday_set),
    SCM_CLASS_SLOT_SPEC("isdst", tm_isdst_get, tm_isdst_set),
    SCM_CLASS_SLOT_SPEC_END()
};

/*
 * nanosleep() compatibility layer
 */
int Scm_NanoSleep(const ScmTimeSpec *req, ScmTimeSpec *rem)
{
#if defined(GAUCHE_WINDOWS)
    /* Recent mingw32 includes nanosleep but it seems broken, so we keep
       using this compatibility code for the time being. */
    DWORD msecs = 0;
    time_t sec;
    u_long overflow = 0, c;
    const DWORD MSEC_OVERFLOW = 4294967; /* 4294967*1000 = 0xfffffed8 */

    /* It's very unlikely that we overflow msecs, but just in case... */
    if (req->tv_sec > 0 || (req->tv_sec == 0 && req->tv_nsec > 0)) {
        if (req->tv_sec >= MSEC_OVERFLOW) {
            overflow = req->tv_sec / MSEC_OVERFLOW;
            sec = req->tv_sec % MSEC_OVERFLOW;
        } else {
            sec = req->tv_sec;
        }
        msecs = (sec * 1000 + (req->tv_nsec + 999999)/1000000);
    }
    Sleep (msecs);
    for (c = 0; c < overflow; c++) {
        Sleep(MSEC_OVERFLOW * 1000);
    }
    if (rem) {
        rem->tv_sec = rem->tv_nsec = 0;
    }
    return 0;
#elif defined(HAVE_NANOSLEEP)
    return nanosleep(req, rem);
#else   /* !defined(HAVE_NANOSLEEP) && !defined(GAUCHE_WINDOWS) */
    /* This case should be excluded at the caller site */
    errno = EINVAL;
    return -1;
#endif
}

/*===============================================================
 * Yielding CPU (sched.h, if available)
 */

/* If sched_yield is not available, we make the calling thread sleep
   small amount of time, hoping there are other threads that can run
   in place. */
void
Scm_YieldCPU(void)
{
#if defined(HAVE_SCHED_YIELD)
    sched_yield();
#elif defined(GAUCHE_WINDOWS)
    /* Windows have select(), but it doesn't allow all fds are NULL. */
    Sleep(10);
#elif defined(HAVE_NANOSLEEP)
    /* We can use struct timespec instead of ScmTimeSpec here, for mingw
       won't use this path. */
    struct timespec spec;
    spec.tv_sec = 0;
    spec.tv_nsec = 1;
    nanosleep(&spec, NULL);
#elif defined(HAVE_SELECT)
    struct timeval tv;
    tv.tv_sec = 0;
    tv.tv_usec = 1;
    select(0, NULL, NULL, NULL, &tv);
#else /* the last resort */
    sleep(1);
#endif
}

/*===============================================================
 * Groups (grp.h)
 */

static void grp_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<sys-group %S>",
               SCM_SYS_GROUP(obj)->name);
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SysGroupClass, grp_print);

static ScmObj make_group(struct group *g)
{
    ScmSysGroup *sg = SCM_NEW(ScmSysGroup);
    SCM_SET_CLASS(sg, SCM_CLASS_SYS_GROUP);

    sg->name = SCM_MAKE_STR_COPYING(g->gr_name);
#ifdef HAVE_STRUCT_GROUP_GR_PASSWD
    sg->passwd = SCM_MAKE_STR_COPYING(g->gr_passwd);
#else
    sg->passwd = SCM_FALSE;
#endif
    sg->gid = Scm_MakeInteger(g->gr_gid);
    sg->mem = Scm_CStringArrayToList((const char**)g->gr_mem, -1,
                                     SCM_MAKSTR_COPYING);
    return SCM_OBJ(sg);
}

ScmObj Scm_GetGroupById(gid_t gid)
{
    struct group *gdata = getgrgid(gid);
    if (gdata == NULL) {
        Scm_SigCheck(Scm_VM());
        return SCM_FALSE;
    } else {
        return make_group(gdata);
    }
}

ScmObj Scm_GetGroupByName(ScmString *name)
{
    struct group *gdata = getgrnam(Scm_GetStringConst(name));
    if (gdata == NULL) {
        Scm_SigCheck(Scm_VM());
        return SCM_FALSE;
    } else {
        return make_group(gdata);
    }
}

#define GRP_GETTER(name) \
  static ScmObj SCM_CPP_CAT3(grp_, name, _get)(ScmSysGroup *s) \
  { return s->name; }

GRP_GETTER(name)
GRP_GETTER(gid)
GRP_GETTER(passwd)
GRP_GETTER(mem)

static ScmClassStaticSlotSpec grp_slots[] = {
    SCM_CLASS_SLOT_SPEC("name",   grp_name_get, NULL),
    SCM_CLASS_SLOT_SPEC("gid",    grp_gid_get, NULL),
    SCM_CLASS_SLOT_SPEC("passwd", grp_passwd_get, NULL),
    SCM_CLASS_SLOT_SPEC("mem",    grp_mem_get, NULL),
    SCM_CLASS_SLOT_SPEC_END()
};

/*===============================================================
 * Passwords (pwd.h)
 *   Patch provided by Yuuki Takahashi (t.yuuki@mbc.nifty.com)
 */

static void pwd_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<sys-passwd %S>",
               SCM_SYS_PASSWD(obj)->name);
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SysPasswdClass, pwd_print);

static ScmObj make_passwd(struct passwd *pw)
{
    ScmSysPasswd *sp = SCM_NEW(ScmSysPasswd);
    SCM_SET_CLASS(sp, SCM_CLASS_SYS_PASSWD);

    sp->name = SCM_MAKE_STR_COPYING(pw->pw_name);
    sp->uid = Scm_MakeInteger(pw->pw_uid);
    sp->gid = Scm_MakeInteger(pw->pw_gid);
#ifdef HAVE_STRUCT_PASSWD_PW_PASSWD
    sp->passwd = SCM_MAKE_STR_COPYING(pw->pw_passwd);
#else
    sp->passwd = SCM_FALSE;
#endif
#ifdef HAVE_STRUCT_PASSWD_PW_GECOS
    sp->gecos = SCM_MAKE_STR_COPYING(pw->pw_gecos);
#else
    sp->gecos = SCM_FALSE;
#endif
#ifdef HAVE_STRUCT_PASSWD_PW_CLASS
    sp->pwclass = SCM_MAKE_STR_COPYING(pw->pw_class);
#else
    sp->pwclass = SCM_FALSE;
#endif
    sp->dir = SCM_MAKE_STR_COPYING(pw->pw_dir);
    sp->shell = SCM_MAKE_STR_COPYING(pw->pw_shell);
    return SCM_OBJ(sp);
}

ScmObj Scm_GetPasswdById(uid_t uid)
{
    struct passwd *pdata = getpwuid(uid);
    if (pdata == NULL) {
        Scm_SigCheck(Scm_VM());
        return SCM_FALSE;
    } else {
        return make_passwd(pdata);
    }
}

ScmObj Scm_GetPasswdByName(ScmString *name)
{
    struct passwd *pdata = getpwnam(Scm_GetStringConst(name));
    if (pdata == NULL) {
        Scm_SigCheck(Scm_VM());
        return SCM_FALSE;
    } else {
        return make_passwd(pdata);
    }
}

#define PWD_GETTER(name) \
  static ScmObj SCM_CPP_CAT3(pwd_, name, _get)(ScmSysPasswd *p) \
  { return p->name; }

PWD_GETTER(name)
PWD_GETTER(uid)
PWD_GETTER(gid)
PWD_GETTER(passwd)
PWD_GETTER(gecos)
PWD_GETTER(dir)
PWD_GETTER(shell)
PWD_GETTER(pwclass)

static ScmClassStaticSlotSpec pwd_slots[] = {
    SCM_CLASS_SLOT_SPEC("name",   pwd_name_get, NULL),
    SCM_CLASS_SLOT_SPEC("uid",    pwd_uid_get, NULL),
    SCM_CLASS_SLOT_SPEC("gid",    pwd_gid_get, NULL),
    SCM_CLASS_SLOT_SPEC("passwd", pwd_passwd_get, NULL),
    SCM_CLASS_SLOT_SPEC("gecos",  pwd_gecos_get, NULL),
    SCM_CLASS_SLOT_SPEC("dir",    pwd_dir_get, NULL),
    SCM_CLASS_SLOT_SPEC("shell",  pwd_shell_get, NULL),
    SCM_CLASS_SLOT_SPEC("class",  pwd_pwclass_get, NULL),
    SCM_CLASS_SLOT_SPEC_END()
};

/*
 * check if we're suid/sgid-ed.
 * TODO: some system has a special syscall for it; use it if so.
 */
int Scm_IsSugid(void)
{
#if !defined(GAUCHE_WINDOWS)
    return (geteuid() != getuid() || getegid() != getgid());
#else  /* GAUCHE_WINDOWS */
    return FALSE;
#endif /* GAUCHE_WINDOWS */
}

/*===============================================================
 * Process management
 */

/* Child process management (windows only)
 *   On windows, parent-child relationship is very weak.  The system
 *   records parent's pid (and we can query it in a very twisted way), but
 *   the child's process record is discarded upon child's termination
 *   unless the parent keeps its process handle.   To emulate exec-wait
 *   semantics, we keep the list of child process handles whose status is
 *   unclaimed.
 *   One issue is that we cannot wait() for child processes that
 *   are created by Gauche extension code and not using Scm_SysExec API.
 */
#if defined(GAUCHE_WINDOWS)
static struct process_mgr_rec {
    ScmObj children;
    ScmInternalMutex mutex;
} process_mgr = { SCM_NIL, SCM_INTERNAL_MUTEX_INITIALIZER };

ScmObj win_process_register(ScmObj process)
{
    SCM_ASSERT(Scm_WinProcessP(process));
    ScmObj pair = Scm_Cons(process, SCM_NIL);
    SCM_INTERNAL_MUTEX_LOCK(process_mgr.mutex);
    SCM_SET_CDR(pair, process_mgr.children);
    process_mgr.children = pair;
    SCM_INTERNAL_MUTEX_UNLOCK(process_mgr.mutex);
    return process;
}

ScmObj win_process_unregister(ScmObj process)
{
    SCM_INTERNAL_MUTEX_LOCK(process_mgr.mutex);
    process_mgr.children = Scm_DeleteX(process, process_mgr.children,
                                       SCM_CMP_EQ);
    SCM_INTERNAL_MUTEX_UNLOCK(process_mgr.mutex);
    return process;
}

int win_process_active_child_p(ScmObj process)
{
    SCM_INTERNAL_MUTEX_LOCK(process_mgr.mutex);
    ScmObj r = Scm_Member(process, process_mgr.children, SCM_CMP_EQ);
    SCM_INTERNAL_MUTEX_UNLOCK(process_mgr.mutex);
    return !SCM_FALSEP(r);
}

ScmObj *win_process_get_array(int *size /*out*/)
{
    SCM_INTERNAL_MUTEX_LOCK(process_mgr.mutex);
    ScmObj *r = Scm_ListToArray(process_mgr.children, size, NULL, TRUE);
    SCM_INTERNAL_MUTEX_UNLOCK(process_mgr.mutex);
    return r;
}

void win_process_cleanup(void *data)
{
    SCM_INTERNAL_MUTEX_LOCK(process_mgr.mutex);
    ScmObj cp;
    SCM_FOR_EACH(cp, process_mgr.children) {
        CloseHandle(Scm_WinHandle(SCM_CAR(cp), SCM_FALSE));
    }
    process_mgr.children = SCM_NIL;
    SCM_INTERNAL_MUTEX_UNLOCK(process_mgr.mutex);
}
#endif /*GAUCHE_WINDOWS*/

/* Command line construction (Windows only)
 *   In order to use CreateProcess we have to concatenate all arguments
 *   into one command line string.  Proper escaping should be considered
 *   when the arguments include whitespaces or double-quotes.
 *   It's pretty silly that we have to do this, since the child process
 *   crt will re-parse the command line again.  Besides, since the parsing
 *   of the command line is up to each application, THERE IS NO WAY TO
 *   GUARANTEE TO QUOTE THE ARGUMENTS PROPERLY.   This is intolerably
 *   broken specification.
 */
#if defined(GAUCHE_WINDOWS)
char *win_create_command_line(ScmObj args)
{
    ScmObj ostr = Scm_MakeOutputStringPort(TRUE);
    static ScmObj proc = SCM_UNDEFINED;
    SCM_BIND_PROC(proc, "%sys-escape-windows-command-line", Scm_GaucheModule());
    ScmObj ap;
    SCM_FOR_EACH(ap, args) {
      ScmObj escaped = Scm_ApplyRec1(proc, SCM_CAR(ap));
      Scm_Printf(SCM_PORT(ostr), "%A ", escaped);
    }
    ScmObj out = Scm_GetOutputStringUnsafe(SCM_PORT(ostr), 0);
    return Scm_GetString(SCM_STRING(out));
}
#endif /*GAUCHE_WINDOWS*/

/* Scm_SysExec
 *   execvp(), with optionally setting stdios correctly.
 *
 *   iomap argument, when provided, specifies how the open file descriptors
 *   are treated.  If it is not a pair, nothing will be changed for open
 *   file descriptors.  If it is a pair, it must be a list of
 *   (<to> . <from>), where <tofd> is an integer file descriptor that
 *   executed process will get, and <from> is either an integer file descriptor
 *   or a port.   If a list is passed to iomap, any file descriptors other
 *   than specified in the list will be closed before exec().
 *
 *   If forkp arg is TRUE, this function forks before swapping file
 *   descriptors.  It is more reliable way to fork&exec in multi-threaded
 *   program.  In such a case, this function returns Scheme integer to
 *   show the children's pid.   If fork arg is FALSE, this procedure
 *   of course never returns.
 *
 *   On Windows port, this returns a process handle obejct instead of
 *   pid of the child process in fork mode.  We need to keep handle, or
 *   the process exit status will be lost when the child process terminates.
 */
ScmObj Scm_SysExec(ScmString *file, ScmObj args, ScmObj iomap,
                   ScmSysSigset *mask, ScmString *dir, int flags)
{
    int argc = Scm_Length(args);
    pid_t pid = 0;
    int forkp = flags & SCM_EXEC_WITH_FORK;
    int detachp = flags & SCM_EXEC_DETACHED;

    if (argc < 1) {
        Scm_Error("argument list must have at least one element: %S", args);
    }

    /* make a C array of C strings */
    char **argv = Scm_ListToCStringArray(args, TRUE, NULL);
    const char *program = Scm_GetStringConst(file);

    /* setting up iomap table */
    int *fds = Scm_SysPrepareFdMap(iomap);

    /*
     * From now on, we have totally different code for Unix and Windows.
     */
#if !defined(GAUCHE_WINDOWS)
    /*
     * Unix path
     */
    const char *cdir = NULL;
    if (dir != NULL) cdir = Scm_GetStringConst(dir);

    /* When requested, call fork() here. */
    if (forkp) {
        SCM_SYSCALL(pid, fork());
        if (pid < 0) Scm_SysError("fork failed");
    }

    if (!forkp || pid == 0) {   /* possibly the child process */

        /* If we're running the daemon, we fork again to detach the parent,
           and also reset the session id. */
        if (detachp) {
            SCM_SYSCALL(pid, fork());
            if (pid < 0) Scm_SysError("fork failed");
            if (pid > 0) exit(0);   /* not Scm_Exit(), for we don't want to
                                       run the cleanup stuff. */
            setsid();
        }

        if (cdir != NULL) {
            if (chdir(cdir) < 0) {
                Scm_Panic("chdir to %s failed before executing %s: %s",
                          cdir, program, strerror(errno));
            }
        }

        Scm_SysSwapFds(fds);
        if (mask) {
            Scm_ResetSignalHandlers(&mask->set);
            Scm_SysSigmask(SIG_SETMASK, mask);
        }

        execvp(program, (char *const*)argv);
        /* here, we failed */
        Scm_Panic("exec failed: %s: %s", program, strerror(errno));
    }

    /* We come here only when fork is requested. */
    return Scm_MakeInteger(pid);
#else  /* GAUCHE_WINDOWS */
    /*
     * Windows path
     */
    const char *cdir = NULL;
    if (dir != NULL) {
        /* we need full path for CreateProcess. */
        dir = SCM_STRING(Scm_NormalizePathname(dir, SCM_PATH_ABSOLUTE|SCM_PATH_CANONICALIZE));
        cdir = Scm_GetStringConst(dir);

        /* If the program is given in relative pathname,
           it must be adjusted relative to the specified directory. */
        if (program[0] != '/' && program[0] != '\\'
            && !(program[0] && program[1] == ':')) {
            ScmDString ds;
            int c = cdir[strlen(cdir)-1];
            Scm_DStringInit(&ds);
            Scm_DStringPutz(&ds, cdir, -1);
            if (c != '/' && c != '\\') Scm_DStringPutc(&ds, SCM_CHAR('/'));
            Scm_DStringPutz(&ds, program, -1);
            program = Scm_DStringGetz(&ds);
        }
    }

    if (forkp) {
        TCHAR  program_path[MAX_PATH+1], *filepart;
        HANDLE *hs = win_prepare_handles(fds);
        PROCESS_INFORMATION pi;
        DWORD creation_flags = 0;

        BOOL pathlen = SearchPath(NULL, SCM_MBS2WCS(program),
                                  _T(".exe"), MAX_PATH, program_path,
                                  &filepart);
        if (pathlen == 0) Scm_SysError("cannot find program '%s'", program);
        program_path[pathlen] = 0;

        STARTUPINFO si;
        GetStartupInfo(&si);
        if (hs != NULL) {
            si.dwFlags |= STARTF_USESTDHANDLES;
            si.hStdInput  = hs[0];
            si.hStdOutput = hs[1];
            si.hStdError  = hs[2];
        }

        LPCTSTR curdir = NULL;
        if (cdir != NULL) curdir = SCM_MBS2WCS(cdir);

        if (detachp) {
            creation_flags |= CREATE_NEW_PROCESS_GROUP;
        }

        BOOL r = CreateProcess(program_path,
                               SCM_MBS2WCS(win_create_command_line(args)),
                               NULL, /* process attr */
                               NULL, /* thread addr */
                               TRUE, /* inherit handles */
                               creation_flags, /* creation flags */
                               NULL, /* nenvironment */
                               curdir, /* current dir */
                               &si,  /* startup info */
                               &pi); /* process info */
        if (r == 0) Scm_SysError("spawning %s failed", program);
        CloseHandle(pi.hThread); /* we don't need it. */
        return win_process_register(Scm_MakeWinProcess(pi.hProcess));
    } else {
        Scm_SysSwapFds(fds);
        if (cdir != NULL) {
            if (_chdir(cdir) < 0) {
                Scm_SysError("Couldn't chdir to %s", cdir);
            }
        }
        /* TODO: We should probably use Windows API to handle various
           options consistently with fork-and-exec case above. */
        execvp(program, (const char *const*)argv);
        Scm_Panic("exec failed: %s: %s", program, strerror(errno));
    }
    return SCM_FALSE; /* dummy */
#endif /* GAUCHE_WINDOWS */
}

/* Two auxiliary functions to support iomap feature.  They are exposed
   so that the library can implement iomap feature as the same way as
   sys-exec.

   The first function, Scm_SysPrepareFdMap, walks iomap structure and
   prepare a table of file descriptors to modify.  The second function,
   Scm_SysSwapFds, takes the table and modifies process's file descriptors.

   We need to split this feature to two function, since it is unsafe
   to raise an error after fork() in multi-threaded environment.
   Scm_SysPrepareFdMap may throw an error if passed iomap contains
   invalid entries.  On the other hand, Scm_SysSwapFds just aborts if
   things goes wrong---not only because of the MT-safety issue, but also
   it is generally impossible to handle errors reasonably since we don't
   even sure we have stdios.   And the client code is supposed to call
   fork() between these functions.

   The client code should treat the returned pointer of Scm_SysPrepareFdMap
   opaque, and pass it to Scm_SysSwapFds as is.
*/
int *Scm_SysPrepareFdMap(ScmObj iomap)
{
    int *fds = NULL;
    if (SCM_PAIRP(iomap)) {
        int iollen = Scm_Length(iomap);

        /* check argument vailidity before duping file descriptors, so that
           we can still use Scm_Error */
        if (iollen < 0) {
            Scm_Error("proper list required for iolist, but got %S", iomap);
        }
        fds    = SCM_NEW_ATOMIC2(int *, 2 * iollen * sizeof(int) + 1);
        fds[0] = iollen;
        int *tofd   = fds + 1;
        int *fromfd = fds + 1 + iollen;

        ScmObj iop;
        int i = 0;
        SCM_FOR_EACH(iop, iomap) {
            ScmObj port, elt = SCM_CAR(iop);
            if (!SCM_PAIRP(elt) || !SCM_INTP(SCM_CAR(elt))
                || (!SCM_PORTP(SCM_CDR(elt)) && !SCM_INTP(SCM_CDR(elt)))) {
                Scm_Error("bad iomap specification: needs (int . int-or-port): %S", elt);
            }
            tofd[i] = SCM_INT_VALUE(SCM_CAR(elt));
            if (SCM_INTP(SCM_CDR(elt))) {
                fromfd[i] = SCM_INT_VALUE(SCM_CDR(elt));
            } else {
                port = SCM_CDAR(iop);
                fromfd[i] = Scm_PortFileNo(SCM_PORT(port));
                if (fromfd[i] < 0) {
                    Scm_Error("iolist requires a port that has associated file descriptor, but got %S",
                              SCM_CDAR(iop));
                }
                if (tofd[i] == 0 && !SCM_IPORTP(port))
                    Scm_Error("input port required to make it stdin: %S",
                              port);
                if (tofd[i] == 1 && !SCM_OPORTP(port))
                    Scm_Error("output port required to make it stdout: %S",
                              port);
                if (tofd[i] == 2 && !SCM_OPORTP(port))
                    Scm_Error("output port required to make it stderr: %S",
                              port);
            }
            i++;
        }
    }
    return fds;
}

void Scm_SysSwapFds(int *fds)
{
    if (fds == NULL) return;

    int maxfd;
    int nfds = fds[0];
    int *tofd   = fds + 1;
    int *fromfd = fds + 1 + nfds;

    /* TODO: use getdtablehi if available */
#if !defined(GAUCHE_WINDOWS)
    if ((maxfd = sysconf(_SC_OPEN_MAX)) < 0) {
        Scm_Panic("failed to get OPEN_MAX value from sysconf");
    }
#else  /*GAUCHE_WINDOWS*/
    maxfd = 256;        /* guess it and cross your finger */
#endif /*GAUCHE_WINDOWS*/

    /* Dup fromfd to the corresponding tofd.  We need to be careful
       not to override the destination fd if it will be used. */
    for (int i=0; i<nfds; i++) {
        if (tofd[i] == fromfd[i]) continue;
        for (int j=i+1; j<nfds; j++) {
            if (tofd[i] == fromfd[j]) {
                int tmp = dup(tofd[i]);
                if (tmp < 0) Scm_Panic("dup failed: %s", strerror(errno));
                fromfd[j] = tmp;
            }
        }
        if (dup2(fromfd[i], tofd[i]) < 0)
            Scm_Panic("dup2 failed: %s", strerror(errno));
    }

    /* Close unused fds */
    for (int fd=0; fd<maxfd; fd++) {
        int j;
        for (j=0; j<nfds; j++) if (fd == tofd[j]) break;
        if (j == nfds) close(fd);
    }
}

#if defined(GAUCHE_WINDOWS)
static HANDLE *win_prepare_handles(int *fds)
{
    if (fds == NULL) return NULL;

    /* For the time being, we only consider stdin, stdout, and stderr. */
    HANDLE *hs = SCM_NEW_ATOMIC_ARRAY(HANDLE, 3);
    int count = fds[0];

    for (int i=0; i<count; i++) {
        int to = fds[i+1], from = fds[i+1+count];
        if (to >= 0 && to < 3) {
            if (from >= 3) {
                /* from_fd may be a pipe. */
                HANDLE zh;
                if (!DuplicateHandle(GetCurrentProcess(),
                                     (HANDLE)_get_osfhandle(from),
                                     GetCurrentProcess(),
                                     &zh,
                                     0, TRUE,
                                     DUPLICATE_CLOSE_SOURCE
                                     |DUPLICATE_SAME_ACCESS)) {
                    Scm_SysError("DuplicateHandle failed");
                }
                hs[to] = zh;
            } else {
                hs[to] = (HANDLE)_get_osfhandle(from);
            }
        }
    }
    for (int i=0; i<3; i++) {
        if (hs[i] == NULL) {
            hs[i] = (HANDLE)_get_osfhandle(i);
        }
    }
    return hs;
}
#endif /*GAUCHE_WINDOWS*/

/*===============================================================
 * Kill
 *
 *  It is simple on Unix, but on windows it is a lot more involved,
 *  mainly due to the lack of signals as the means of IPC.
 */
void Scm_SysKill(ScmObj process, int signal)
{
#if !defined(GAUCHE_WINDOWS)
    pid_t pid;
    int r;
    if (!SCM_INTEGERP(process)) SCM_TYPE_ERROR(process, "integer process id");
    pid = Scm_GetInteger(process);
    SCM_SYSCALL(r, kill(pid, signal));
    if (r < 0) Scm_SysError("kill failed");
#else  /*GAUCHE_WINDOWS*/
    /* You cannot really "send" singals to other processes on Windows.
       We try to emulate SIGKILL and SIGINT by Windows API.
       To send a signal to the current process we can use raise(). */
    HANDLE p;
    BOOL r;
    DWORD errcode;
    int pid_given = FALSE;
    pid_t pid;

    if (SCM_INTEGERP(process)) {
        pid_given = TRUE; pid = Scm_GetInteger(process);
    } else if (Scm_WinProcessP(process)) {
        pid = Scm_WinProcessPID(process);
    } else {
        SCM_TYPE_ERROR(process, "process handle or integer process id");
    }

    if (signal == SIGKILL) {
        if (pid_given) {
            p = OpenProcess(PROCESS_TERMINATE, FALSE, pid);
            if (p == NULL) Scm_SysError("OpenProcess failed for pid %d", pid);
        } else {
            p = Scm_WinProcess(process);
        }
        /* We send 0xff00 + KILL, so that the receiving process (if it is
           Gauche) can yield an exit status that indicates it is kill. */
        r = TerminateProcess(p, SIGKILL+0xff00);
        errcode = GetLastError();
        if (pid_given) CloseHandle(p);
        SetLastError(errcode);
        if (r == 0) Scm_SysError("TerminateProcess failed");
        return;
    }
    /* another idea; we may map SIGTERM to WM_CLOSE message. */

    if (signal == 0) {
        /* We're supposed to do the error check without actually sending
           the signal.   For now we just pretend nothing's wrong. */
        return;
    }
    if (pid == getpid()) {
        /* we're sending signal to the current process. */
        int r = raise(signal); /* r==0 is success */
        if (r < 0) Scm_SysError("raise failed");
        return;
    }
    if (signal == SIGINT || signal == SIGABRT) {
        /* we can emulate these signals by console event, although the
           semantics of process group differ from unix significantly.
           Process group id is the same as the pid of the process
           that started the group.  So you cannot send SIGABRT only
           to the process group leader.  OTOH, for SIGINT, the windows
           manual says it always directed to the specified process,
           not the process group, unless pid == 0 */
        r = GenerateConsoleCtrlEvent(abs(pid),
                                     (signal == SIGINT)?
                                     CTRL_C_EVENT : CTRL_BREAK_EVENT);
        if (r == 0) {
            Scm_SysError("GenerateConsoleCtrlEvent failed for process %d", pid);
        }
        return;
    }
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
#endif /*GAUCHE_WINDOWS*/
}

/*===============================================================
 * Wait
 *
 *  A wrapper of waitpid.  Returns two values---the process object or pid that
 *  whose status has been taken, and the exit status.
 *  Again, it is simple on Unix, but on windows it is a lot more involved.
 */

ScmObj Scm_SysWait(ScmObj process, int options)
{
#if !defined(GAUCHE_WINDOWS)
    pid_t r;
    int status = 0;
    if (!SCM_INTEGERP(process)) SCM_TYPE_ERROR(process, "integer process id");
    SCM_SYSCALL(r, waitpid(Scm_GetInteger(process), &status, options));
    if (r < 0) Scm_SysError("waitpid() failed");
    return Scm_Values2(Scm_MakeInteger(r), Scm_MakeInteger(status));
#else  /* GAUCHE_WINDOWS */
    /* We have four cases
       process is integer and < -1   -> not supported.
       process is -1 or 0 -> wait for all children (we ignore process group)
       process is integer and > 0  -> wait for specific pid
       process is #<win:process-handle> -> wait for specified process
       The common op is factored out in win_wait_for_handles. */
    int r, status = 0;

    if (SCM_INTEGERP(process)) {
        pid_t pid = Scm_GetInteger(process);
        if (pid < -1) {
            /* Windows doesn't have the concept of "process group id" */
            SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
            Scm_SysError("waitpid cannot wait for process group on Windows.");
        }
        if (pid > 0) {
            /* wait for specific pid */
            HANDLE handle = OpenProcess(SYNCHRONIZE|PROCESS_QUERY_INFORMATION,
                                        FALSE, pid);
            DWORD errcode;
            if (handle == NULL) {
                Scm_SysError("OpenProcess failed for pid %d", pid);
            }
            r = win_wait_for_handles(&handle, 1, options, &status);
            errcode = GetLastError();
            CloseHandle(handle);
            SetLastError(errcode);
            if (r == -2) goto timeout;
            if (r == -1) goto error;
            return Scm_Values2(Scm_MakeInteger(pid), Scm_MakeInteger(status));
        }
        else {
            /* wait for any children. */
            ScmObj *children;
            int num_children, i;
            HANDLE *handles;
            children = win_process_get_array(&num_children);
            if (num_children == 0) {
                SetLastError(ERROR_WAIT_NO_CHILDREN);
                Scm_SysError("waitpid failed");
            }
            handles = SCM_NEW_ATOMIC_ARRAY(HANDLE, num_children);
            for (i=0; i<num_children; i++) {
                handles[i] = Scm_WinProcess(children[i]);
            }
            r = win_wait_for_handles(handles, num_children, options, &status);
            if (r == -2) goto timeout;
            if (r == -1) goto error;
            win_process_unregister(children[r]);
            return Scm_Values2(children[r], Scm_MakeInteger(status));
        }
    } else if (Scm_WinProcessP(process)) {
        /* wait for the specified process */
        HANDLE handle;
        if (!win_process_active_child_p(process)) {
            SetLastError(ERROR_WAIT_NO_CHILDREN);
            Scm_SysError("waitpid failed");
        }
        handle = Scm_WinProcess(process);
        r = win_wait_for_handles(&handle, 1, options, &status);
        if (r == -2) goto timeout;
        if (r == -1) goto error;
        win_process_unregister(process);
        return Scm_Values2(process, Scm_MakeInteger(status));
    }
  timeout:
    return Scm_Values2(SCM_MAKE_INT(0), SCM_MAKE_INT(0));
  error:
    Scm_SysError("waitpid failed");
    return SCM_UNDEFINED;  /* dummy */
#endif /* GAUCHE_WINDOWS */
}

#if defined(GAUCHE_WINDOWS)
/* aux fn. */
static int win_wait_for_handles(HANDLE *handles, int nhandles, int options,
                                int *status /*out*/)
{
    DWORD r = MsgWaitForMultipleObjects(nhandles,
                                        handles,
                                        FALSE,
                                        (options&WNOHANG)? 0 : INFINITE,
                                        0);
    if (r == WAIT_FAILED) return -1;
    if (r == WAIT_TIMEOUT) return -2;
    if (r >= WAIT_OBJECT_0 && r < WAIT_OBJECT_0 + nhandles) {
        DWORD exitcode;
        int index = r - WAIT_OBJECT_0;
        r = GetExitCodeProcess(handles[index], &exitcode);
        if (r == 0) return -1;
        *status = exitcode;
        return index;
    }
    return -1;
}
#endif /*GAUCHE_WINDOWS*/

/*===============================================================
 * select
 */

#ifdef HAVE_SELECT
static ScmObj fdset_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmSysFdset *set = SCM_ALLOCATE(ScmSysFdset, klass);
    SCM_SET_CLASS(set, SCM_CLASS_SYS_FDSET);
    set->maxfd = -1;
    FD_ZERO(&set->fdset);
    return SCM_OBJ(set);
}

static ScmSysFdset *fdset_copy(ScmSysFdset *fdset)
{
    ScmSysFdset *set = SCM_NEW(ScmSysFdset);
    SCM_SET_CLASS(set, SCM_CLASS_SYS_FDSET);
    set->maxfd = fdset->maxfd;
    set->fdset = fdset->fdset;
    return set;
}

SCM_DEFINE_BUILTIN_CLASS(Scm_SysFdsetClass, NULL, NULL, NULL,
                         fdset_allocate, SCM_CLASS_DEFAULT_CPL);

static ScmSysFdset *select_checkfd(ScmObj fds)
{
    if (SCM_FALSEP(fds)) return NULL;
    if (!SCM_SYS_FDSET_P(fds))
        Scm_Error("sys-fdset object or #f is required, but got %S", fds);
    return SCM_SYS_FDSET(fds);
}

static struct timeval *select_timeval(ScmObj timeout, struct timeval *tm)
{
    if (SCM_FALSEP(timeout)) return NULL;
    if (SCM_INTP(timeout)) {
        int val = SCM_INT_VALUE(timeout);
        if (val < 0) goto badtv;
        tm->tv_sec = val / 1000000;
        tm->tv_usec = val % 1000000;
        return tm;
    } else if (SCM_BIGNUMP(timeout)) {
        long usec;
        ScmObj sec;
        if (Scm_Sign(timeout) < 0) goto badtv;
        sec = Scm_BignumDivSI(SCM_BIGNUM(timeout), 1000000, &usec);
        tm->tv_sec = Scm_GetInteger(sec);
        tm->tv_usec = usec;
        return tm;
    } else if (SCM_FLONUMP(timeout)) {
        long val = Scm_GetInteger(timeout);
        if (val < 0) goto badtv;
        tm->tv_sec = val / 1000000;
        tm->tv_usec = val % 1000000;
        return tm;
    } else if (SCM_PAIRP(timeout) && SCM_PAIRP(SCM_CDR(timeout))) {
        ScmObj sec = SCM_CAR(timeout);
        ScmObj usec = SCM_CADR(timeout);
        long isec, iusec;
        if (!Scm_IntegerP(sec) || !Scm_IntegerP(usec)) goto badtv;
        isec = Scm_GetInteger(sec);
        iusec = Scm_GetInteger(usec);
        if (isec < 0 || iusec < 0) goto badtv;
        tm->tv_sec = isec;
        tm->tv_usec = iusec;
        return tm;
    }
  badtv:
    Scm_Error("timeval needs to be a real number (in microseconds) or a list of two integers (seconds and microseconds), but got %S", timeout);
    return NULL;                /* dummy */
}

static ScmObj select_int(ScmSysFdset *rfds, ScmSysFdset *wfds,
                         ScmSysFdset *efds, ScmObj timeout)
{
    int numfds, maxfds = 0;
    struct timeval tm;
    if (rfds) maxfds = rfds->maxfd;
    if (wfds && wfds->maxfd > maxfds) maxfds = wfds->maxfd;
    if (efds && efds->maxfd > maxfds) maxfds = efds->maxfd;

    SCM_SYSCALL(numfds,
                select(maxfds+1,
                       (rfds? &rfds->fdset : NULL),
                       (wfds? &wfds->fdset : NULL),
                       (efds? &efds->fdset : NULL),
                       select_timeval(timeout, &tm)));
    if (numfds < 0) Scm_SysError("select failed");
    return Scm_Values4(Scm_MakeInteger(numfds),
                       (rfds? SCM_OBJ(rfds) : SCM_FALSE),
                       (wfds? SCM_OBJ(wfds) : SCM_FALSE),
                       (efds? SCM_OBJ(efds) : SCM_FALSE));
}

ScmObj Scm_SysSelect(ScmObj rfds, ScmObj wfds, ScmObj efds, ScmObj timeout)
{
    ScmSysFdset *r = select_checkfd(rfds);
    ScmSysFdset *w = select_checkfd(wfds);
    ScmSysFdset *e = select_checkfd(efds);
    return select_int((r? fdset_copy(r) : NULL),
                      (w? fdset_copy(w) : NULL),
                      (e? fdset_copy(e) : NULL),
                      timeout);
}

ScmObj Scm_SysSelectX(ScmObj rfds, ScmObj wfds, ScmObj efds, ScmObj timeout)
{
    ScmSysFdset *r = select_checkfd(rfds);
    ScmSysFdset *w = select_checkfd(wfds);
    ScmSysFdset *e = select_checkfd(efds);
    return select_int(r, w, e, timeout);
}

#endif /* HAVE_SELECT */

/*===============================================================
 * Environment
 */

/* We provide a compatibility layer for getenv/setenv stuff, whose semantics
   slightly differ among platforms.

   POSIX putenv() has a flaw that passed string can't be freed reliably;
   the system may retain the pointer, so the caller can't free it afterwards,
   while putenv() itself can't know if the passed pointer is malloc-ed or
   static.  Some Unixes appears to change the semantics, guaranteeing
   the system copies the passed string so that the caller can free it;
   however, it's not easy to check which semantics the given platform uses.

   What POSIX suggests is setenv() when you want to pass malloc-ed
   strings.  Unfortunately it is a newer addition and not all platforms
   supports it.  Windows doesn't, either, but it offers _[w]putenv_s
   as an alternative.  Unfortunately again, current MinGW doesn't include
   _[w]putenv_s in its headers and import libraries.

   So, for those platforms, we use putenv/_wputenv.  We track allocated
   memory in env_string table, keyed by names of envvars, and free them
   whenever we put a new definition of envvars we've inserted before.

   Another merit of this compatibility layer is to guarantee MT-safeness;
   Putenv/setenv aren't usally MT-safe, neither is getenv when environment
   is being modified.
*/

static ScmInternalMutex env_mutex;
static ScmHashCore env_strings; /* name -> malloc-ed mem.
                                   used with putenv()/_wputenv() to prevent
                                   leak. */

const char *Scm_GetEnv(const char *name)
{
#if defined(GAUCHE_WINDOWS) && defined(UNICODE)
    const wchar_t *wname = Scm_MBS2WCS(name);
    const char *value = NULL;
    (void)SCM_INTERNAL_MUTEX_LOCK(env_mutex);
    const wchar_t *wvalue = _wgetenv(wname);
    if (wvalue != NULL) {
        value = Scm_WCS2MBS(wvalue);
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(env_mutex);
    return value;
#else  /*!(defined(GAUCHE_WINDOWS) && defined(UNICODE))*/
    (void)SCM_INTERNAL_MUTEX_LOCK(env_mutex);
    const char *value = SCM_STRDUP(getenv(name));
    (void)SCM_INTERNAL_MUTEX_UNLOCK(env_mutex);
    return value;
#endif /*!(defined(GAUCHE_WINDOWS) && defined(UNICODE))*/
}

void Scm_SetEnv(const char *name, const char *value, int overwrite)
{
#if defined(GAUCHE_WINDOWS) && defined(UNICODE)
    /* We need to use _wputenv for wide-character support.  Since we pass
       the converted strings to OS, we have to allocate them by malloc.
       To prevent leak, we register the allocated memory to the global
       hash table, and free it when Scm_SetEnv is called with the same NAME
       again. */
    wchar_t *wname = Scm_MBS2WCS(name);
    wchar_t *wvalue = Scm_MBS2WCS(value);
    int nlen = wcslen(wname);
    int vlen = wcslen(wvalue);
    wchar_t *wnameval = (wchar_t*)malloc((nlen+vlen+2)*sizeof(wchar_t));
    if (wnameval == NULL) {
        Scm_Error("sys-setenv: out of memory");
    }
    wcscpy(wnameval, wname);
    wcscpy(wnameval+nlen, L"=");
    wcscpy(wnameval+nlen+1, wvalue);

    ScmObj sname = Scm_MakeString(name, -1, -1, SCM_STRING_COPYING);

    int result = 0;
    wchar_t *prev_mem = NULL;
    
    (void)SCM_INTERNAL_MUTEX_LOCK(env_mutex);
    if (overwrite || _wgetenv(wname) == NULL) {
        result = _wputenv(wnameval);
        if (result >= 0) {
            ScmDictEntry *e = Scm_HashCoreSearch(&env_strings,
                                                 (intptr_t)sname,
                                                 SCM_DICT_CREATE);
            /* SCM_DICT_VALUE is only for ScmObj, so we directly access value
               field here. */
            prev_mem = (wchar_t*)e->value;
            e->value = (intptr_t)wnameval; 
        }
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(env_mutex);

    if (result < 0) {
        free(wnameval);
        Scm_SysError("setenv failed on '%s=%s'", name, value);
    }
    if (prev_mem != NULL) {
        free(prev_mem);
    }
#elif defined(HAVE_SETENV)
    (void)SCM_INTERNAL_MUTEX_LOCK(env_mutex);
    int r = setenv(name, value, overwrite);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(env_mutex);
    if (r < 0) Scm_SysError("setenv failed on '%s=%s'", name, value);
#elif defined(HAVE_PUTENV)
    int nlen = (int)strlen(name);
    int vlen = (int)strlen(value);
    char *nameval = (char*)malloc(nlen+vlen+2);
    if (nameval == NULL) {
        Scm_Error("sys-setenv: out of memory");
    }
    strcpy(nameval, name);
    strcpy(nameval+nlen, "=");
    strcpy(nameval+nlen+1, value);

    ScmObj sname = Scm_MakeString(name, -1, -1, SCM_STRING_COPYING);

    int result = 0;
    chat *prev_mem = NULL;
    
    (void)SCM_INTERNAL_MUTEX_LOCK(env_mutex);
    if (overwrite || getenv(name) == NULL) {
        result = putenv(nameval);
        if (result >= 0) {
            ScmDictEntry *e = Scm_HashCoreSearch(&env_strings,
                                                 (intptr_t)sname,
                                                 SCM_DICT_CREATE);
            /* SCM_DICT_VALUE is only for ScmObj, so we directly access value
               field here. */
            prev_mem = (char*)e->value;
            e->value = (intptr_t)nameval; 
        }
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(env_mutex);
    if (r < 0) {
        free (nameval);
        Scm_SysError("putenv failed on '%s=%s'", name, value);
    }
    if (prev_mem != NULL) {
        free(prev_mem);
    }
#else /* !HAVE_SETENV && !HAVE_PUTENV */
    /* We can't do much.  we may replace environ by ourselves, but
       it is unlikely that the system have extern environ and not putenv.
    */
    Scm_Error("neither setenv nor putenv is supported on this platform.");
#endif
}

/* Returns the system's environment table as a list of strings.
   Each string is in the format of "key=value". */
ScmObj Scm_Environ(void)
{
#if defined(GAUCHE_WINDOWS)
#define ENV_BUFSIZ 64
    LPVOID ss = GetEnvironmentStrings();
    ScmObj h = SCM_NIL, t = SCM_NIL;
    TCHAR *cp = (TCHAR*)ss, *pp;
    TCHAR sbuf[ENV_BUFSIZ], *buf=sbuf;
    int bsize = ENV_BUFSIZ, size;

    do {
        for (pp=cp; *pp; pp++) /*proceed ptr*/;
        size = (int)(pp - cp) + 1;
        if (size >= bsize) {
            buf = SCM_NEW_ATOMIC_ARRAY(TCHAR, size);
            bsize = size;
        }
        memcpy(buf, cp, size*sizeof(TCHAR));
        SCM_APPEND1(h, t, SCM_MAKE_STR_COPYING(SCM_WCS2MBS(buf)));
        cp = pp+1;
    } while (pp[1] != 0);
    FreeEnvironmentStrings(ss);
    return h;
#else
    (void)SCM_INTERNAL_MUTEX_LOCK(env_mutex);
#  if defined(HAVE_CRT_EXTERNS_H)
    char **environ = *_NSGetEnviron();  /* OSX Hack*/
#  endif
    ScmObj r = (environ == NULL 
                ? SCM_NIL
                : Scm_CStringArrayToList((const char**)environ, -1,
                                         SCM_STRING_COPYING));
    (void)SCM_INTERNAL_MUTEX_UNLOCK(env_mutex);
    return r;
#endif /*!GAUCHE_WINDOWS*/
}

void Scm_UnsetEnv(const char *name)
{
#if defined(HAVE_UNSETENV)
    /* NB: If we HAVE_SETENV, we don't have any entries in env_strings,
       so the lookup of snv_strings is a waste; but the result is always
       NULL and it won't harm the operation, and we expect sys-unsetenv
       is rarely used, so we just let it waste cpu cycles. */
    char *prev_mem = NULL;
    ScmObj sname = Scm_MakeString(name, -1, -1, SCM_STRING_COPYING);
    (void)SCM_INTERNAL_MUTEX_LOCK(env_mutex);
    int r = unsetenv(name);
    ScmDictEntry *e = Scm_HashCoreSearch(&env_strings,
                                         (intptr_t)sname,
                                         SCM_DICT_DELETE);
    if (e != NULL) { prev_mem = (char*)e->value; e->value = (intptr_t)NULL; }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(env_mutex);
    if (r < 0) Scm_SysError("unsetenv failed on %s", name);
    if (prev_mem != NULL) free(prev_mem);
#else  /*!HAVE_UNSETENV*/
    Scm_Error("sys-unsetenv is not supported on this platform.");
#endif /*!HAVE_UNSETENV*/
}

void Scm_ClearEnv()
{
#if defined(HAVE_CLEARENV)
    /* As in Scm_UnsetEnv, we don't need env_strings business if
       we HAVE_SETENV, but it does no harm either. */
    (void)SCM_INTERNAL_MUTEX_LOCK(env_mutex);
    int r = clearenv();
    ScmHashIter iter;
    Scm_HashIterInit(&iter, &env_strings);
    ScmDictEntry *e;
    while ((e = Scm_HashIterNext(&iter)) != NULL) {
        free((void*)e->value);
        e->value = (intptr_t)NULL;
    }
    Scm_HashCoreClear(&env_strings);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(env_mutex);
    if (r < 0) Scm_SysError("clearenv failed");
#else  /*!HAVE_UNSETENV*/
    Scm_Error("sys-clearenv is not supported on this platform.");
#endif /*!HAVE_UNSETENV*/
}

/*===============================================================
 * Closer-to-metal thingy
 */

/* Try to find # of available processors.  If we don't know how to
   find that info on the platform, we fall back to 1.
   If GAUCHE_AVAILABLE_PROCESSORS environment variable is defined and
   has the value interpreted as a positive integer, we use that value
   instead.
*/
int Scm_AvailableProcessors()
{
    const char *env = Scm_GetEnv("GAUCHE_AVAILABLE_PROCESSORS");
    if (env && env[0] != '\0') {
        char *ep;
        long v = strtol(env, &ep, 10);
        if (v > 0 && *ep == '\0') return (int)v;
    }
#if !defined(GAUCHE_WINDOWS)
#if   defined(_SC_NPROCESSORS_ONLN)
    return (int)sysconf(_SC_NPROCESSORS_ONLN);
#else  /*!defined(_SC_NPROCESSORS_ONLN)*/
    return 1;                   /* fallback */
#endif /*!defined(_SC_NPROCESSORS_ONLN)*/
#else  /*defined(GAUCHE_WINDOWS)*/
    SYSTEM_INFO sysinfo;
    GetSystemInfo( &sysinfo );
    return (int)sysinfo.dwNumberOfProcessors;
#endif /*defined(GAUCHE_WINDOWS)*/
}

/*===============================================================
 * Emulation layer for Windows
 */
#if defined(GAUCHE_WINDOWS)

/* Dynamically obtain an entry point that may not be available on
   all Windows versions.  If throw_error is TRUE, throws an error
   if DLL mapping failed, or entry cannot be found.  Otherwise,
   returns NULL on error. */
static void *get_api_entry(const TCHAR *module, const char *proc,
                           int throw_error)
{
    void *entry;
    HMODULE m = LoadLibrary(module);
    if (m == NULL) {
        if (throw_error)
            Scm_SysError("LoadLibrary(%s) failed", SCM_WCS2MBS(module));
        else
            return NULL;
    }
    entry = (void*)GetProcAddress(m, proc);
    if (entry == NULL) {
        DWORD errcode = GetLastError();
        FreeLibrary(m);
        SetLastError(errcode);
        if (throw_error)
            Scm_SysError("GetProcAddress(%s) failed", proc);
        else
            return NULL;
    }
    return entry;
}

/* Scan the processes to find out either the parent process, or the
   child processes of the current process.  I cannot imagine why we
   need such a hassle to perform this kind of simple task, but this
   is the way the MS document suggests.
   Returns a single Scheme integer of the parent process id if childrenp
   is FALSE; returns a list of Scheme integers of child process ids if
   childrenp is TRUE. */
static ScmObj get_relative_processes(int childrenp)
{
    HANDLE snapshot;
    PROCESSENTRY32 entry;
    DWORD myid = GetCurrentProcessId(), parentid;
    int found = FALSE;
    ScmObj h = SCM_NIL, t = SCM_NIL; /* children pids */

    snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if (snapshot == INVALID_HANDLE_VALUE) {
        Scm_Error("couldn't take process snapshot in getppid()");
    }
    entry.dwSize = sizeof(PROCESSENTRY32);
    if (!Process32First(snapshot, &entry)) {
        CloseHandle(snapshot);
        Scm_Error("Process32First failed in getppid()");
    }
    do {
        if (childrenp) {
            if (entry.th32ParentProcessID == myid) {
                SCM_APPEND1(h, t, Scm_MakeInteger(entry.th32ProcessID));
            }
        } else {
            if (entry.th32ProcessID == myid) {
                parentid = entry.th32ParentProcessID;
                found = TRUE;
                break;
            }
        }
    } while (Process32Next(snapshot, &entry));
    CloseHandle(snapshot);

    if (childrenp) {
        return h;
    } else {
        if (!found) {
            Scm_Error("couldn't find the current process entry in getppid()");
        }
        return Scm_MakeInteger(parentid);
    }
}

/* Retrieve PID from windows process handle wrapper.  */

pid_t Scm_WinProcessPID(ScmObj handle)
{
    /* GetProcessId seems very primitive procedure, but somehow Windows
       only provides it in XP SP1 or after.  Before that it seems you
       can only map pid -> handle by OpenProcess but you can't do the
       reverse (except you enumerate all process ids, calling OpenProcess
       on each and look for one whose handle matches the given handle.
       Sounds expensive. */
    static DWORD (WINAPI *pGetProcessId)(HANDLE) = NULL;
    static int queried = FALSE;

    if (!Scm_WinProcessP(handle)) {
        SCM_TYPE_ERROR(handle, "<win:handle process>");
    }

    if (pGetProcessId == NULL) {
        if (queried) return (pid_t)-1;
        pGetProcessId = get_api_entry(_T("kernel32.dll"), "GetProcessId",
                                      FALSE);
        if (pGetProcessId == NULL) {
            queried = TRUE;
            return (pid_t)-1;
        }
    }
    return pGetProcessId(Scm_WinProcess(handle));
}

/*
 * Users and groups
 * Kinda Kluge, since we don't have "user id" associated with each
 * user.  (If a domain server is active, Windows security manager seems
 * to assign an unique user id for every user; but it doesn't seem available
 * for stand-alone machine.)
 */

static void convert_user(const USER_INFO_2 *wuser, struct passwd *res)
{
    res->pw_name    = (const char*)SCM_WCS2MBS(wuser->usri2_name);
    res->pw_passwd  = "*";
    res->pw_uid     = 0;
    res->pw_gid     = 0;
    res->pw_comment = (const char*)SCM_WCS2MBS(wuser->usri2_comment);
    res->pw_gecos   = (const char*)SCM_WCS2MBS(wuser->usri2_full_name);
    res->pw_dir     = (const char*)SCM_WCS2MBS(wuser->usri2_home_dir);
    res->pw_shell   = "";
}

/* Arrgh! thread unsafe!  just for the time being...*/
static struct passwd pwbuf = { "dummy" };

struct passwd *getpwnam(const char *name)
{
    USER_INFO_2 *res;
    if (NetUserGetInfo(NULL, (LPCWSTR)SCM_MBS2WCS(name), 2, (LPBYTE*)&res)
        != NERR_Success) {
        return NULL;
    }
    convert_user(res, &pwbuf);
    NetApiBufferFree(res);
    return &pwbuf;
}

struct passwd *getpwuid(uid_t uid)
{
    /* for the time being, we just ignore uid and returns the current
       user info. */
#define NAMELENGTH 256
    TCHAR buf[NAMELENGTH];
    DWORD len = NAMELENGTH;
    if (GetUserName(buf, &len) == 0) {
        return NULL;
    }
    return getpwnam(SCM_WCS2MBS(buf));
}

static struct group dummy_group = {
    "dummy",
    "",
    100,
    NULL
};

struct group *getgrgid(gid_t gid)
{
    return &dummy_group;
}

struct group *getgrnam(const char *name)
{
    return &dummy_group;
}

/* Kluge kluge kluge */
uid_t getuid(void)
{
    return 0;
}

uid_t geteuid(void)
{
    return 0;
}

gid_t getgid(void)
{
    return 0;
}

gid_t getegid(void)
{
    return 0;
}

pid_t getppid(void)
{
    ScmObj ppid = get_relative_processes(FALSE);
    return Scm_GetInteger(ppid);
}

const char *getlogin(void)
{
    static TCHAR buf[256]; /* this isn't thread-safe, but getlogin() is
                              inherently thread-unsafe call anyway */
    DWORD size = sizeof(buf)/sizeof(TCHAR);
    BOOL r;
    r = GetUserName(buf, &size);
    if (r) {
        return SCM_WCS2MBS(buf);
    } else {
        return NULL;
    }
}

clock_t times(struct tms *info)
{
    HANDLE process = GetCurrentProcess();
    FILETIME ctime, xtime, utime, stime;
    int64_t val;
    const int factor = 10000000/CLK_TCK;
    const int bias   = factor/2;

    if (!GetProcessTimes(process, &ctime, &xtime, &stime, &utime)) {
        Scm_SysError("GetProcessTimes failed");
    }
    val = ((int64_t)stime.dwHighDateTime << 32) + stime.dwLowDateTime;
    info->tms_stime = (u_int)((val+bias) / factor);
    val = ((int64_t)utime.dwHighDateTime << 32) + utime.dwLowDateTime;
    info->tms_utime = (u_int)((val+bias) / factor);

    info->tms_cstime = 0;
    info->tms_cutime = 0;
    return 0;
}

/*
 * Other obscure stuff
 */

int fork(void)
{
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
    return -1;
}

int pipe(int fd[])
{
#define PIPE_BUFFER_SIZE 512
    /* NB: We create pipe with NOINHERIT to avoid complication when spawning
       child process.  Sys_Exec will dups the handle with inheritable flag
       for the children.  */
    int r = _pipe(fd, PIPE_BUFFER_SIZE, O_BINARY|O_NOINHERIT);
    return r;
}

char *ttyname(int desc)
{
    return NULL;
}

static int win_truncate(HANDLE file, off_t len)
{
    typedef BOOL (WINAPI *pSetEndOfFile_t)(HANDLE);
    typedef BOOL (WINAPI *pSetFilePointer_t)(HANDLE, LONG, PLONG, DWORD);

    static pSetEndOfFile_t pSetEndOfFile = NULL;
    static pSetFilePointer_t pSetFilePointer = NULL;

    BOOL r;

    if (pSetEndOfFile == NULL) {
        pSetEndOfFile = (pSetEndOfFile_t)get_api_entry(_T("kernel32.dll"),
                                                       "SetEndOfFile",
                                                       FALSE);
        if (pSetEndOfFile == NULL) return -1;
    }
    if (pSetFilePointer == NULL) {
        pSetFilePointer = (pSetFilePointer_t)get_api_entry(_T("kernel32.dll"),
                                                           "SetFilePointer",
                                                           FALSE);
        if (pSetFilePointer == NULL) return -1;
    }

    /* TODO: 64bit size support! */
    r = pSetFilePointer(file, (LONG)len, NULL, FILE_BEGIN);
    if (r == INVALID_SET_FILE_POINTER) return -1;
    r = pSetEndOfFile(file);
    if (r == 0) return -1;
    return 0;
}

int truncate(const char *path, off_t len)
{
    HANDLE file;
    int r;

    file = CreateFile(SCM_MBS2WCS(path), GENERIC_WRITE,
                      FILE_SHARE_READ|FILE_SHARE_WRITE,
                      NULL, OPEN_EXISTING, 0, NULL);
    if (file == INVALID_HANDLE_VALUE) return -1;
    r = win_truncate(file, len);
    if (r < 0) {
        DWORD errcode = GetLastError();
        CloseHandle(file);
        SetLastError(errcode);
        return -1;
    }
    CloseHandle(file);
    return 0;
}

int ftruncate(int fd, off_t len)
{
    HANDLE h = (HANDLE)_get_osfhandle(fd);
    int r;
    if (h == INVALID_HANDLE_VALUE) return -1;
    r = win_truncate(h, len);
    if (r < 0) return -1;
    return 0;
}

unsigned int alarm(unsigned int seconds)
{
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
    Scm_SysError("alarm");
    return 0;
}

/* file links */
int link(const char *existing, const char *newpath)
{
    /* CreateHardLink only exists in WinNT or later.  Officially we don't
       support anything before, but let's try to be kind for the legacy
       system ...*/
    typedef BOOL (WINAPI *pCreateHardLink_t)(LPTSTR, LPTSTR,
                                             LPSECURITY_ATTRIBUTES);
    static pCreateHardLink_t pCreateHardLink = NULL;
    BOOL r;
#if defined(UNICODE)
#define CREATEHARDLINK  "CreateHardLinkW"
#else
#define CREATEHARDLINK  "CreateHardLinkA"
#endif

    if (pCreateHardLink == NULL) {
        pCreateHardLink = (pCreateHardLink_t)get_api_entry(_T("kernel32.dll"),
                                                           CREATEHARDLINK,
                                                           TRUE);
    }
    r = pCreateHardLink((LPTSTR)SCM_MBS2WCS(newpath),
                        (LPTSTR)SCM_MBS2WCS(existing), NULL);
    return r? 0 : -1;
}

/* Winsock requires some obscure initialization.
   We perform initialization here, since winsock module is used
   in both gauche.net and gauche.auxsys. */
static WSADATA wsaData;

static void init_winsock(void)
{
    int opt;
    int r = WSAStartup(MAKEWORD(2,2), &wsaData);
    if (r != 0) {
        SetLastError(r);
        Scm_SysError("WSAStartup failed");
    }
    /* windows voodoo to make _open_osfhandle magic work */
    opt = SO_SYNCHRONOUS_NONALERT;
    r = setsockopt(INVALID_SOCKET, SOL_SOCKET,
                   SO_OPENTYPE, (char*)&opt, sizeof(opt));
    if (r == SOCKET_ERROR) {
        Scm_SysError("winsock initialization failed");
    }
}

static void fini_winsock(void *data)
{
    (void)WSACleanup();
}

/* Win32 thread support.  See also gauche/wthread.h */

#if defined(GAUCHE_USE_WTHREADS)

HANDLE Scm__WinCreateMutex()
{
    HANDLE m = CreateMutex(NULL, FALSE, NULL);
    if (m == NULL) Scm_SysError("couldn't create a mutex");
    return m;
}

int Scm__WinMutexLock(HANDLE mutex)
{
    DWORD r = WaitForSingleObject(mutex, INFINITE);
    if (r == WAIT_OBJECT_0) return 0;
    else return 1;              /* TODO: proper error handling */
}

/* Win32 conditional variable emulation.
   Native condition variable support is only available on Windows Vista
   and later.  We don't want to drop XP support (yet), so we avoid using
   it.  Instead we emulate posix condition variable semantics.
   We enhanced the implementation described as the SignalObjectAndWait
   solution shown in
   <http://www1.cse.wustl.edu/~schmidt/win32-cv-1.html>
 */
void Scm__InternalCondInit(ScmInternalCond *cond)
{
    cond->numWaiters = 0;
    cond->broadcast = FALSE;
    cond->mutex = NULL;         /* set by the first CondWait */
    cond->sem = CreateSemaphore(NULL,          /* no security */
                                0, 0x7fffffff, /* initial and max val */
                                NULL);         /* name */
    if (cond->sem == NULL) {
        Scm_SysError("couldn't create a semaphore for a condition variable");
    }
    cond->done = CreateEvent(NULL,  /* no security */
                             FALSE, /* auto-reset */
                             FALSE, /* initially non-signalled */
                             NULL); /* name */
    if (cond->done == NULL) {
        DWORD err = GetLastError();
        CloseHandle(cond->sem);
        SetLastError(err);
        Scm_SysError("couldn't create event for a condition variable");
    }
    InitializeCriticalSection(&cond->numWaitersLock);
}

int Scm__InternalCondWait(ScmInternalCond *cond, ScmInternalMutex *mutex,
                          ScmTimeSpec *pts)
{
    DWORD r0, r1;
    DWORD timeout_msec;
    int badMutex = FALSE, lastWaiter;

    if (pts) {
        u_long now_sec, now_usec;
        u_long target_sec, target_usec;
        Scm_GetTimeOfDay(&now_sec, &now_usec);
        target_sec = pts->tv_sec;
        target_usec = pts->tv_nsec / 1000;
        if (target_sec < now_sec
            || (target_sec == now_sec && target_usec <= now_usec)) {
            timeout_msec = 0;
        } else if (target_usec >= now_usec) {
            timeout_msec = ceil((target_sec - now_sec) * 1000
                                + (target_usec - now_usec)/1000.0);
        } else {
            timeout_msec = ceil((target_sec - now_sec - 1) * 1000
                                + (1.0e6 + target_usec - now_usec)/1000.0);
        }
    } else {
        timeout_msec = INFINITE;
    }

    EnterCriticalSection(&cond->numWaitersLock);
    /* If we're the first one to wait on this cond var, set cond->mutex.
       We don't allow to use multiple mutexes together with single cond var.
     */
    if (cond->mutex != NULL && cond->mutex != mutex) {
        badMutex = TRUE;
    } else {
        cond->numWaiters++;
        if (cond->mutex == NULL) cond->mutex = mutex;
    }
    LeaveCriticalSection(&cond->numWaitersLock);

    if (badMutex) {
        Scm_Error("Attempt to wait on condition variable %p with different"
                  " mutex %p\n", cond, mutex);
    }

    /* Signals mutex and atomically waits on the semaphore */
    r0 = SignalObjectAndWait(*mutex, cond->sem, timeout_msec, FALSE);

    /* We're signaled, or timed out.   There can be a case that cond is
       broadcasted between the timeout of SignalObjectAndWait and the
       following EnterCriticalSection.  So we should check lastWaiter
       anyway. */
    EnterCriticalSection(&cond->numWaitersLock);
    cond->numWaiters--;
    lastWaiter = cond->broadcast && cond->numWaiters == 0;
    LeaveCriticalSection(&cond->numWaitersLock);

    if (lastWaiter) {
        /* tell the broadcaster that all the waiters have gained
           control, and wait to aquire mutex. */
        r1 = SignalObjectAndWait(cond->done, *mutex, INFINITE, FALSE);
    } else {
        /* Aquire mutex */
        r1 = WaitForSingleObject(*mutex, INFINITE);
    }
    if (r0 == WAIT_TIMEOUT) return SCM_INTERNAL_COND_TIMEDOUT;
    if (r0 != WAIT_OBJECT_0 || r1 != WAIT_OBJECT_0) return -1;
    return 0;
}

int Scm__InternalCondSignal(ScmInternalCond *cond)
{
    int haveWaiters;
    BOOL r = TRUE;

    if (!cond->mutex) return 0; /* nobody ever waited on this cond var. */

    SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(cond->mutex);

    EnterCriticalSection(&cond->numWaitersLock);
    haveWaiters = (cond->numWaiters > 0);
    LeaveCriticalSection(&cond->numWaitersLock);

    if (haveWaiters) {
        r = ReleaseSemaphore(cond->sem, 1, 0);
    }

    SCM_INTERNAL_MUTEX_SAFE_LOCK_END();
    if (!r) return -1;
    return 0;
}

int Scm__InternalCondBroadcast(ScmInternalCond *cond)
{
    int haveWaiters;
    DWORD err = 0;
    BOOL r0 = TRUE;
    DWORD r1 = WAIT_OBJECT_0;

    if (!cond->mutex) return 0; /* nobody ever waited on this cond var. */

    SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN(cond->mutex);

    EnterCriticalSection(&cond->numWaitersLock);
    cond->broadcast = haveWaiters = (cond->numWaiters > 0);

    if (haveWaiters) {
        r0 = ReleaseSemaphore(cond->sem, cond->numWaiters, 0);
        if (!r0) err = GetLastError();
        LeaveCriticalSection(&cond->numWaitersLock);

        if (r0) {
            /* Each waiter aquires mutex in turn, until the last waiter
               who will signal on 'done'. */
            r1 = WaitForSingleObject(cond->done, INFINITE);
            cond->broadcast = FALSE; /* safe; nobody will check this */
        }
    } else {
        /* nobody's waiting */
        LeaveCriticalSection(&cond->numWaitersLock);
    }

    SCM_INTERNAL_MUTEX_SAFE_LOCK_END();

    if (!r0) { SetLastError(err); return -1; }
    if (r1 != WAIT_OBJECT_0) return -1;
    return 0;
}

void Scm__InternalCondDestroy(ScmInternalCond *cond)
{
    CloseHandle(cond->sem);
    cond->sem = NULL;
    CloseHandle(cond->done);
    cond->done = NULL;
}

void Scm__WinThreadExit()
{
    ScmVM *vm = Scm_VM();
    ScmWinCleanup *cup = vm->winCleanup;
    while (cup) {
        cup->cleanup(cup->data);
        cup = cup->prev;
    }
    GC_ExitThread(0);
}

#endif /* GAUCHE_USE_WTHREADS */

#endif /* GAUCHE_WINDOWS */

/*===============================================================
 * Initialization
 */
void Scm__InitSystem(void)
{
    ScmModule *mod = Scm_GaucheModule();
    Scm_InitStaticClass(&Scm_SysStatClass, "<sys-stat>", mod, stat_slots, 0);
    Scm_InitStaticClass(&Scm_TimeClass, "<time>", mod, time_slots, 0);
    Scm_InitStaticClass(&Scm_SysTmClass, "<sys-tm>", mod, tm_slots, 0);
    Scm_InitStaticClass(&Scm_SysGroupClass, "<sys-group>", mod, grp_slots, 0);
    Scm_InitStaticClass(&Scm_SysPasswdClass, "<sys-passwd>", mod, pwd_slots, 0);
#ifdef HAVE_SELECT
    Scm_InitStaticClass(&Scm_SysFdsetClass, "<sys-fdset>", mod, NULL, 0);
#endif
    SCM_INTERNAL_MUTEX_INIT(env_mutex);
    Scm_HashCoreInitSimple(&env_strings, SCM_HASH_STRING, 0, NULL);

#ifdef GAUCHE_WINDOWS
    init_winsock();
    SCM_INTERNAL_MUTEX_INIT(process_mgr.mutex);
    Scm_AddCleanupHandler(fini_winsock, NULL);
    Scm_AddCleanupHandler(win_process_cleanup, NULL);
#endif
}
