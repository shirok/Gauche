/*
 * file.c - file utilities
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
 *  $Id: file.c,v 1.3 2001-02-02 10:32:18 shiro Exp $
 */

#include <unistd.h>
#include <pwd.h>
#include <sys/types.h>
#include <limits.h>
#include <string.h>
#include "gauche.h"

/*
 * Pathname
 */

/*
 * Gauche doesn't have a special pathname object, but converts pathname
 * string into an intermediate format for conveninence.
 * The intermediate format is a reverse list of pathname components.
 * If the pathname is relative, #f is added to the head of the list.
 * Here's some examples of translation
 *
 *   "/foo/bar/baz"  => ("baz" "bar" "foo")
 *   "foo/bar/baz"   => (#f "baz" "bar" "foo")
 *   "/"             => ()
 *   "."             => (#f)
 */

/* Split given pathname into a list of components. */
static ScmObj split_pathname(const char *pathname, ScmObj parent)
{
    ScmObj path = parent;
    int absolutep = (parent == SCM_NIL)? FALSE : TRUE;
    char *p;

    p = SCM_NEW_ATOMIC2(char *, strlen(pathname)+1);
    strcpy(p, pathname);
    
    if (*p == '/') {
        absolutep = TRUE;
        do p++; while (*p == '/');
    }

    while (*p) {
        char *q = strchr(p, '/');
        if (!q) {
            if (strcmp(p, ".") != 0) {
                path = Scm_Cons(Scm_MakeStringConst(p, -1, -1), path);
            }
            break;
        }
        if (q-p != 1 || p[0] != '.') {
            path = Scm_Cons(Scm_MakeStringConst(p, q-p, q-p), path);
        }
        p = q;
        do p++; while (*p == '/');
    }
    if (!absolutep) path = Scm_Cons(SCM_FALSE, path);
    return path;
}

/* Get cwd and split */
static ScmObj get_cwd(void)
{
    char buf[PATH_MAX+1], *p;
    p = getcwd(buf, PATH_MAX);
    if (p == NULL) Scm_Error("couldn't get current directory.");
    return split_pathname(p, SCM_NIL);
}

/* Get user's home dir and split */
static ScmObj get_user_home(const char *user)
{
    struct passwd *pwd;
    /* TODO: check if we should free *pwd or not */
    if (user == NULL) {
        uid_t uid = geteuid();
        pwd = getpwuid(uid);
        if (pwd == NULL) Scm_Error("couldn't get home directory.\n");
    } else {
        pwd = getpwnam(user);
        if (pwd == NULL)
            Scm_Error("couldn't get home directory of user \"%s\".\n", user);
    }
    return split_pathname(pwd->pw_dir, SCM_NIL);
}

/* Given splitted path, remove ".."'s */
static ScmObj normalize_path(ScmObj path)
{
    ScmObj head = SCM_NIL, tail, cp;
    int up = 0;
    
    SCM_FOR_EACH(cp, path) {
        if (SCM_STRINGP(SCM_CAR(cp))) {
            ScmObj str = SCM_CAR(cp);
            const char *p = SCM_STRING_START(str);
            int len = SCM_STRING_LENGTH(str);
            if (len == 2 && strncmp(p, "..", 2) == 0) { up++; continue; }
        }
        if (up > 0) { up--; continue; }
        SCM_APPEND1(head, tail, SCM_CAR(cp));
    }

    while (up-- > 0) SCM_APPEND1(head, tail, SCM_MAKE_STR(".."));
    return head;
}

/*
 * External API
 */
ScmObj Scm_ParsePathname(const char *pathname, int makeAbsolute, int normalize)
{
    ScmObj path, parent = SCM_NIL;
    char buf[PATH_MAX*2+2], *p;

    if (makeAbsolute && *pathname != '/' && *pathname != '~') {
        parent = get_cwd();
    }
    else if (normalize && *pathname == '~') {
        const char *r = strchr(pathname, '/');
        const char *user = NULL;
        if (r == NULL) {
            if (pathname[1] != '\0') user = pathname + 1;
            pathname = "";
        } else {
            if (r - pathname > 1) {
                char *ubuf = SCM_NEW_ATOMIC2(char *, r - pathname);
                strncpy(ubuf, pathname+1, r-pathname-1);
                user = ubuf;
            }
            pathname = r + 1;
        }
        parent = get_user_home(user);
    }

    path = split_pathname(pathname, parent);
    if (normalize) path = normalize_path(path);
    return path;
}

ScmObj Scm_PathToPathname(ScmObj list)
{
    int absolutep = SCM_NULLP(list) || SCM_STRINGP(SCM_CAR(list));
    ScmString *delim = SCM_STRING(SCM_MAKE_STR("/"));
    
    if (absolutep) {
        return Scm_StringJoin(Scm_Cons(SCM_MAKE_STR(""), Scm_Reverse(list)),
                              delim);
    } else {
        return Scm_StringJoin(Scm_Reverse(SCM_CDR(list)), delim);
    }
}
