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
 *  $Id: system.c,v 1.10 2001-03-14 10:31:01 shiro Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <grp.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "gauche.h"

#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

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
    ScmObj head = SCM_NIL, tail;
    struct dirent *dire;
    DIR *dirp = opendir(Scm_GetStringConst(pathname));
    
    if (dirp == NULL) Scm_SysError("couldn't open directory %S", pathname);
    while ((dire = readdir(dirp)) != NULL) {
        ScmObj ent = Scm_MakeString(dire->d_name, -1, -1);
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
    ScmObj head = SCM_NIL, tail;
    int i, r = glob(Scm_GetStringConst(pattern), 0, NULL, &globbed);
    if (r < 0) Scm_Error("Couldn't glob %S", pattern);
    for (i = 0; i < globbed.gl_pathc; i++) {
        ScmObj path = Scm_MakeString(globbed.gl_pathv[i], -1, -1);
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
        
        for (; *p != '/' && *p != '\0'; p++)
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
        dirlen = strlen(pwd->pw_dir);
        buf = SCM_NEW_ATOMIC2(char*, dirlen+size+1);
        strcpy(buf, pwd->pw_dir);
        dstp = buf + dirlen;
        if (*(dstp-1) != '/') *dstp++ = '/';
    } else if ((flags & SCM_PATH_ABSOLUTE) && *str != '/') {
        int dirlen;
        const char *p = getcwd(NULL, -1);
        if (p == NULL) Scm_Error("couldn't get current directory.");
        dirlen = strlen(p);
        buf = SCM_NEW_ATOMIC2(char*, dirlen+size+1);
        strcpy(buf, p);
        free(p);                /* allocated by getcwd() */
        dstp = buf + dirlen;
        if (*(dstp-1) != '/') *dstp++ = '/';
    } else if (flags & SCM_PATH_CANONICALIZE) {
        dstp = buf = SCM_NEW_ATOMIC2(char*, size+1);
        if (*str == '/') {
            *dstp++ = '/';
            SKIP_SLASH;
        }
    } else {
        return SCM_OBJ(pathname); /* nothing to do (short path) */
    }

    if (!(flags & SCM_PATH_CANONICALIZE)) {
        memcpy(dstp, srcp, size);
        *(dstp + size) = '\0';
        return Scm_MakeStringConst(buf, (dstp-buf)+size, -1);
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
    return Scm_MakeString(buf, dstp-buf, -1);
}

ScmObj Scm_BaseName(ScmString *filename)
{
    const char *p, *str = SCM_STRING_START(filename);
    int i, size = SCM_STRING_SIZE(filename);

    if (size == 0) return SCM_OBJ(filename);
    p = str+size-1;
    if (*p == '/') { p--; size--; } /* ignore trailing '/' */
    for (i = 0; i < size; i++, p--) {
        if (*p == '/') break;
    }
    return Scm_MakeString(p+1, i, -1);
}

ScmObj Scm_DirName(ScmString *filename)
{
    const char *p, *str = SCM_STRING_START(filename);
    int i, size = SCM_STRING_SIZE(filename);

    if (size == 0) return SCM_MAKE_STR(".");
    if (size == 1 && *str == '/') return SCM_MAKE_STR("/");
    
    p = str+size-1;
    if (*p == '/') { p--; size--; } /* ignore trailing '/' */
    for (i = size; i > 0; i--, p--) {
        if (*p == '/') break;
    }
    if (i == 0) return SCM_MAKE_STR(".");
    return Scm_MakeString(str, i, -1);
}

/*
 * Stat (sys/stat.h)
 */

SCM_DEFCLASS(Scm_SysStatClass, "<sys-stat>", NULL, SCM_CLASS_DEFAULT_CPL);

ScmObj Scm_MakeSysStat(void)
{
    ScmSysStat *s = SCM_NEW(ScmSysStat);
    SCM_SET_CLASS(s, SCM_CLASS_SYS_STAT);
    return SCM_OBJ(s);
}

/*
 * Time (sys/time.h)
 */

SCM_DEFCLASS(Scm_SysTimeClass, "<sys-time>", NULL, SCM_CLASS_DEFAULT_CPL);

ScmObj Scm_MakeSysTime(time_t t)
{
    ScmSysTime *st = SCM_NEW(ScmSysTime);
    SCM_SET_CLASS(st, SCM_CLASS_SYS_TIME);
    st->time = t;
    return SCM_OBJ(st);
}

SCM_DEFCLASS(Scm_SysTmClass, "<sys-tm>", NULL, SCM_CLASS_DEFAULT_CPL);

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

SCM_DEFCLASS(Scm_SysGroupClass, "<sys-group>", NULL, SCM_CLASS_DEFAULT_CPL);

static ScmObj make_group(struct group *g)
{
    ScmObj head = SCM_NIL, tail, p;
    char **memp;
    ScmSysGroup *sg = SCM_NEW(ScmSysGroup);
    SCM_SET_CLASS(sg, SCM_CLASS_SYS_GROUP);
    
    sg->name = Scm_MakeString(g->gr_name, -1, -1);
#ifdef HAVE_GR_PASSWD
    sg->passwd = Scm_MakeString(g->gr_passwd, -1, -1);
#else
    sg->passwd = SCM_FALSE;
#endif
    sg->gid = Scm_MakeInteger(g->gr_gid);
    for (memp = g->gr_mem; *memp; memp++) {
        p = Scm_MakeString(*memp, -1, -1);
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

SCM_DEFCLASS(Scm_SysPasswdClass, "<sys-passwd>", NULL, SCM_CLASS_DEFAULT_CPL);

static ScmObj make_passwd(struct passwd *pw)
{
    ScmSysPasswd *sp = SCM_NEW(ScmSysPasswd);
    SCM_SET_CLASS(sp, SCM_CLASS_SYS_PASSWD);

    sp->name = Scm_MakeString(pw->pw_name, -1, -1);
    sp->uid = Scm_MakeInteger(pw->pw_uid);
    sp->gid = Scm_MakeInteger(pw->pw_gid);
#ifdef HAVE_PW_PASSWD
    sp->passwd = Scm_MakeString(pw->pw_passwd, -1, -1);
#else
    sp->passwd = SCM_FALSE;
#endif
#ifdef HAVE_PW_GECOS
    sp->gecos = Scm_MakeString(pw->pw_gecos, -1, -1);
#else
    sp->gecos = SCM_FALSE;
#endif
#ifdef HAVE_PW_CLASS
    sp->pwclass = Scm_MakeString(pw->pw_class, -1, -1);
#else
    sp->pwclass = SCM_FALSE;
#endif
    sp->dir = Scm_MakeString(pw->pw_dir, -1, -1);
    sp->shell = Scm_MakeString(pw->pw_shell, -1, -1);
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
