/*
 * system.c - system interface
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: system.c,v 1.22 2001-06-21 08:52:29 shirok Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <grp.h>
#include <pwd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "gauche.h"

#ifdef HAVE_GLOB_H
#include <glob.h>
#endif

/*
 * Auxiliary system interface functions.   See syslib.stub for
 * Scheme binding.
 */

/*
 * Directory primitives (dirent.h)
 *   We don't provide the iterator primitives, but a function which
 *   reads entire directory.
 */

/* Returns a list of directory entries.  If pathname is not a directory,
   or can't be opened by some reason, an error is signalled. */
ScmObj Scm_ReadDirectory(ScmString *pathname)
{
    ScmObj head = SCM_NIL, tail = SCM_NIL;
    struct dirent *dire;
    DIR *dirp = opendir(Scm_GetStringConst(pathname));
    
    if (dirp == NULL) Scm_SysError("couldn't open directory %S", pathname);
    while ((dire = readdir(dirp)) != NULL) {
        ScmObj ent = SCM_MAKE_STR_COPYING(dire->d_name);
        SCM_APPEND1(head, tail, ent);
    }
    closedir(dirp);
    return head;
}

/* Glob()function. */
/* TODO: allow to take optional flags */
ScmObj Scm_GlobDirectory(ScmString *pattern)
{
#ifdef HAVE_GLOB_H
    glob_t globbed;
    ScmObj head = SCM_NIL, tail = SCM_NIL;
    int i, r = glob(Scm_GetStringConst(pattern), 0, NULL, &globbed);
    if (r < 0) Scm_Error("Couldn't glob %S", pattern);
    for (i = 0; i < globbed.gl_pathc; i++) {
        ScmObj path = SCM_MAKE_STR_COPYING(globbed.gl_pathv[i]);
        SCM_APPEND1(head, tail, path);
    }
    globfree(&globbed);
    return head;
#else
    Scm_Error("glob-directory is not supported on this architecture.");
    return SCM_UNDEFINED;
#endif
}

/*
 * Pathname manipulation
 */

ScmObj Scm_NormalizePathname(ScmString *pathname, int flags)
{
    const char *str = SCM_STRING_START(pathname), *srcp = str;
    int size = SCM_STRING_SIZE(pathname);
    char *buf = NULL, *dstp;
    int bottomp = FALSE;
    
#define SKIP_SLASH \
    while (*srcp == '/' && srcp < str+size) { srcp++; }

    if ((flags & SCM_PATH_EXPAND) && size >= 1 && *str == '~') {
        /* ~user magic */
        const char *p = str+1;
        struct passwd *pwd;
        int dirlen;
        
        for (; p < str+size && *p != '/' && *p != '\0'; p++)
            ;
        if (p == str+1) {
            pwd = getpwuid(geteuid());
            if (pwd == NULL) Scm_SysError("couldn't get home directory.\n");
        } else {
            char *user = (char *)SCM_MALLOC_ATOMIC(p-str);
            memcpy(user, str+1, p-str-1);
            user[p-str-1] = '\0';
            pwd = getpwnam(user);
            if (pwd == NULL)
                Scm_Error("couldn't get home directory of user \"%s\".\n",
                          user);
        }
        srcp = p;
        SKIP_SLASH;
        dirlen = strlen(pwd->pw_dir);
        buf = SCM_NEW_ATOMIC2(char*, dirlen+size+1);
        strcpy(buf, pwd->pw_dir);
        dstp = buf + dirlen;
        if (*(dstp-1) != '/') { *dstp++ = '/'; *(dstp+1) = '\0'; }
    } else if ((flags & SCM_PATH_ABSOLUTE) && *str != '/') {
        int dirlen;
#define GETCWD_PATH_MAX 1024  /* TODO: must be configured */
        char p[GETCWD_PATH_MAX];
        if (getcwd(p, GETCWD_PATH_MAX-1) == NULL)
            Scm_SysError("couldn't get current directory.");
        dirlen = strlen(p);
        buf = SCM_NEW_ATOMIC2(char*, dirlen+size+1);
        strcpy(buf, p);
        dstp = buf + dirlen;
        if (*(dstp-1) != '/') *dstp++ = '/';
    } else if (flags & SCM_PATH_CANONICALIZE) {
        dstp = buf = SCM_NEW_ATOMIC2(char*, size+1);
        if (*str == '/') {
            *dstp++ = '/';
            SKIP_SLASH;
        }
    } else {
        return SCM_OBJ(pathname); /* nothing to do */
    }

    if (!(flags & SCM_PATH_CANONICALIZE)) {
        size -= srcp-str;
        memcpy(dstp, srcp, size);
        *(dstp + size) = '\0';
        return Scm_MakeString(buf, (dstp-buf)+size, -1, SCM_MAKSTR_COPYING);
    }

    while (srcp < str+size) {
        if (*srcp == '.') {
            if (srcp == str+size-1) {
                *dstp++ = '.'; /* preserve the last dot */
                break;
            }
            if (*(srcp+1) == '/') {
                srcp++;
                SKIP_SLASH;
                continue;
            }
            if (!bottomp
                && *(srcp+1) == '.'
                && (srcp == str+size-2 || *(srcp+2) == '/')) {

                /* back up to parent dir */
                char *q = dstp-2;
                for (;q >= buf; q--) {
                    if (*q == '/') break;
                }
                if (q >= buf) {
                    dstp = q+1;
                } else {
                    bottomp = TRUE;
                    *dstp++ = '.';
                    *dstp++ = '.';
                    *dstp++ = '/';
                }
                srcp += 3;
                continue;
            }
        }
        while ((*dstp++ = *srcp++) != '/' && srcp < str+size)
            ;
        SKIP_SLASH;
    }
    *dstp = '\0';
    return Scm_MakeString(buf, dstp-buf, -1, SCM_MAKSTR_COPYING);
}

ScmObj Scm_BaseName(ScmString *filename)
{
    const char *p, *str = SCM_STRING_START(filename);
    int i, size = SCM_STRING_SIZE(filename);

    if (size == 0) return SCM_MAKE_STR("");
    p = str+size-1;
    while (*p == '/' && size > 0) { p--; size--; } /* ignore trailing '/' */
    if (size == 0) return SCM_MAKE_STR("");
    for (i = 0; i < size; i++, p--) {
        if (*p == '/') break;
    }
    return Scm_MakeString(p+1, i, -1, 0);
}

ScmObj Scm_DirName(ScmString *filename)
{
    const char *p, *str = SCM_STRING_START(filename);
    int i, size = SCM_STRING_SIZE(filename);

    if (size == 0) return SCM_MAKE_STR(".");
    p = str+size-1;
    while (*p == '/' && size > 0) { p--; size--; } /* ignore trailing '/' */
    if (size == 0) return SCM_MAKE_STR("/");
    for (i = size; i > 0; i--, p--) {
        if (*p == '/') break;
    }
    if (i == 0) return SCM_MAKE_STR(".");
    while (*p == '/' && i > 0) { p--; i--; } /* delete trailing '/' */
    if (i == 0) return SCM_MAKE_STR("/");
    return Scm_MakeString(str, i, -1, 0);
}

/*
 * Stat (sys/stat.h)
 */

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SysStatClass, NULL);

ScmObj Scm_MakeSysStat(void)
{
    ScmSysStat *s = SCM_NEW(ScmSysStat);
    SCM_SET_CLASS(s, SCM_CLASS_SYS_STAT);
    return SCM_OBJ(s);
}

/*
 * Time (sys/time.h)
 */

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
#ifdef INTEGRAL_TIME_T
    return (time_t)Scm_GetUInteger(val);
#else
    return (tiem_t)Scm_GetDouble(val);
#endif
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SysTmClass, NULL);

ScmObj Scm_MakeSysTm(struct tm *tm)
{
    ScmSysTm *st = SCM_NEW(ScmSysTm);
    SCM_SET_CLASS(st, SCM_CLASS_SYS_TM);
    st->tm = *tm;               /* copy */
    return SCM_OBJ(st);
}

/*
 * Groups (grp.h)
 */

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SysGroupClass, NULL);

static ScmObj make_group(struct group *g)
{
    ScmObj head = SCM_NIL, tail = SCM_NIL, p;
    char **memp;
    ScmSysGroup *sg = SCM_NEW(ScmSysGroup);
    SCM_SET_CLASS(sg, SCM_CLASS_SYS_GROUP);
    
    sg->name = SCM_MAKE_STR_COPYING(g->gr_name);
#ifdef HAVE_GR_PASSWD
    sg->passwd = SCM_MAKE_STR_COPYING(g->gr_passwd);
#else
    sg->passwd = SCM_FALSE;
#endif
    sg->gid = Scm_MakeInteger(g->gr_gid);
    for (memp = g->gr_mem; *memp; memp++) {
        p = SCM_MAKE_STR_COPYING(*memp);
        SCM_APPEND1(head, tail, p);
    }
    sg->mem = head;
    return SCM_OBJ(sg);
}

ScmObj Scm_GetGroupById(gid_t gid)
{
    struct group *gdata;
    gdata = getgrgid(gid);
    if (gdata == NULL) return SCM_FALSE;
    else return make_group(gdata);
}

ScmObj Scm_GetGroupByName(ScmString *name)
{
    struct group *gdata;
    gdata = getgrnam(Scm_GetStringConst(name));
    if (gdata == NULL) return SCM_FALSE;
    else return make_group(gdata);
}

/*
 * Passwords (pwd.h)
 *   Patch provided by Yuuki Takahashi (t.yuuki@mbc.nifty.com)
 */

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SysPasswdClass, NULL);

static ScmObj make_passwd(struct passwd *pw)
{
    ScmSysPasswd *sp = SCM_NEW(ScmSysPasswd);
    SCM_SET_CLASS(sp, SCM_CLASS_SYS_PASSWD);

    sp->name = SCM_MAKE_STR_COPYING(pw->pw_name);
    sp->uid = Scm_MakeInteger(pw->pw_uid);
    sp->gid = Scm_MakeInteger(pw->pw_gid);
#ifdef HAVE_PW_PASSWD
    sp->passwd = SCM_MAKE_STR_COPYING(pw->pw_passwd);
#else
    sp->passwd = SCM_FALSE;
#endif
#ifdef HAVE_PW_GECOS
    sp->gecos = SCM_MAKE_STR_COPYING(pw->pw_gecos);
#else
    sp->gecos = SCM_FALSE;
#endif
#ifdef HAVE_PW_CLASS
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
    struct passwd *pdata;
    pdata = getpwuid(uid);
    if (pdata == NULL) return SCM_FALSE;
    else return make_passwd(pdata);
}

ScmObj Scm_GetPasswdByName(ScmString *name)
{
    struct passwd *pdata;
    pdata = getpwnam(Scm_GetStringConst(name));
    if (pdata == NULL) return SCM_FALSE;
    else return make_passwd(pdata);
}

/*
 * Exec
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
 *   Don't expect this function to raise a catchable error.  Once the file
 *   descriptors are set up, it's likely that Scheme's standard ports
 *   are useless, so Scm_Error() is not an option.  It just exits by
 *   Scm_Panic().
 */

void Scm_SysExec(ScmString *file, ScmObj args, ScmObj iomap)
{
    int argc = Scm_Length(args), i;
    char **argv;
    ScmObj ap, iop;

    if (argc < 1)
        Scm_Error("argument list must have at least one element: %S", args);
    
    argv = SCM_NEW2(char **, sizeof(char *)*(argc+1));
    for (i=0, ap = args; i<argc; i++, ap = SCM_CDR(ap)) {
        if (!SCM_STRINGP(SCM_CAR(ap)))
            Scm_Error("bad argument (string required): %S", SCM_CAR(ap));
        argv[i] = Scm_GetString(SCM_STRING(SCM_CAR(ap)));
    }
    argv[i] = NULL;

    /* swappling file descriptors. */
    if (SCM_PAIRP(iomap)) {
        int iollen = Scm_Length(iomap), maxfd, j;
        int *tofd, *fromfd, *tmpfd;

        /* check argument vailidity before duping file descriptors, so that
           we can still use Scm_Error */
        if (iollen < 0)
            Scm_Error("proper list required for iolist, but got %S", iomap);
        tofd   = SCM_NEW_ATOMIC2(int *, iollen * sizeof(int));
        fromfd = SCM_NEW_ATOMIC2(int *, iollen * sizeof(int));
        tmpfd  = SCM_NEW_ATOMIC2(int *, iollen * sizeof(int));
        i = 0;
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

        /* TODO: use getdtablehi if available */
        if ((maxfd = sysconf(_SC_OPEN_MAX)) < 0)
            Scm_Error("failed to get OPEN_MAX value from sysconf");

        for (i=0; i<iollen; i++) {
            if (tofd[i] == fromfd[i]) continue;
            for (j=i+1; j<iollen; j++) {
                if (tofd[i] == fromfd[j]) {
                    int tmp = dup(tofd[i]);
                    if (tmp < 0) Scm_Panic("dup failed: %s", strerror(errno));
                    fromfd[j] = tmp;
                }
            }
            if (dup2(fromfd[i], tofd[i]) < 0)
                Scm_Panic("dup2 failed: %s", strerror(errno));
        }
        for (i=0; i<maxfd; i++) {
            for (j=0; j<iollen; j++) {
                if (i == tofd[j]) break;
            }
            if (j == iollen) close(i);
        }
    }

    execvp(Scm_GetStringConst(file), (char *const*)argv);
    /* here, we failed */
    Scm_Panic("exec failed: %s", strerror(errno));
}

/*
 * select
 */

#ifdef HAVE_SELECT
static ScmObj fdset_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmSysFdset *set = SCM_NEW(ScmSysFdset);
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
    
    numfds = select(maxfds+1,
                    (rfds? &rfds->fdset : NULL),
                    (wfds? &wfds->fdset : NULL),
                    (efds? &efds->fdset : NULL),
                    select_timeval(timeout, &tm));
    
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
