/*
 * system.c - system interface
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, ditribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: system.c,v 1.2 2001-02-12 12:52:07 shiro Exp $
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
    char buf[PATH_MAX*2+1], *dstp = buf;
#define SKIP_SLASH \
    while (*srcp == '/' && srcp < str+size) { srcp++; }

    if (size >= PATH_MAX) Scm_Error("pathname too long");
    if ((flags & SCM_PATH_EXPAND) && size >= 1 && *str == '~') {
        /* ~user magic */
        const char *p = str+1;
        struct passwd *pwd;
        int dirlen;
        
        for (; *p == '/' || *p == '\0'; p++)
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
        if (dirlen >= PATH_MAX) Scm_Error("pathname too long\n");
        strcpy(buf, pwd->pw_dir);
        dstp = buf + dirlen;
        if (*(dstp-1) != '/') *dstp++ = '/';
    } else if ((flags & SCM_PATH_ABSOLUTE) && *str != '/') {
        const char *p = getcwd(buf, PATH_MAX);
        if (p == NULL) Scm_Error("couldn't get current directory.");
        dstp = buf + strlen(p);
        if (*(dstp-1) != '/') *dstp++ = '/';
    } else if (*str == '/') {
        *dstp++ = '/';
        SKIP_SLASH;
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
            if (*(srcp+1) == '.' && (srcp == str+size-2 || *(srcp+2) == '/')) {
                /* TODO: this implementation is buggy! */
                /* back up to parent dir */
                char *q = dstp-2;
                for (;q >= buf; q--) {
                    if (*q == '/') break;
                }
                fprintf(stderr, "twak, buf=%x, dstp=%x, q=%x\n", buf, dstp, q);
                if (q >= buf) {
                    dstp = q+1;
                } else {
                    *dstp++ = '.';
                    *dstp++ = '.';
                    *dstp++ = '/';
                }
                srcp += 3;
                fprintf(stderr, "watk, buf=%x, dstp=%x, q=%x\n", buf, dstp, q);
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
 * Groups (grp.h)
 */

static ScmObj decode_group(struct group *g)
{
    ScmObj head = SCM_NIL, tail, memhead = SCM_NIL, memtail;
    ScmObj p;
    char **memp;
    p = Scm_MakeString(g->gr_name, -1, -1);
    SCM_APPEND1(head, tail, p);
    p = Scm_MakeString(g->gr_passwd, -1, -1);
    SCM_APPEND1(head, tail, p);
    p = Scm_MakeInteger(g->gr_gid);
    SCM_APPEND1(head, tail, p);
    for (memp = g->gr_mem; *memp; memp++) {
        p = Scm_MakeString(*memp, -1, -1);
        SCM_APPEND1(memhead, memtail, p);
    }
    SCM_APPEND1(head, tail, memhead);
    return head;
}

ScmObj Scm_GetGroupById(gid_t gid)
{
    struct group *gdata;
    gdata = getgrgid(gid);
    if (gdata == NULL) return SCM_FALSE;
    else return decode_group(gdata);
}

ScmObj Scm_GetGroupByName(ScmString *name)
{
    struct group *gdata;
    gdata = getgrnam(Scm_GetStringConst(name));
    if (gdata == NULL) return SCM_FALSE;
    else return decode_group(gdata);
}

