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
 *  $Id: system.c,v 1.1 2001-02-10 12:41:03 shiro Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <grp.h>
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

