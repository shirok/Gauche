/*
 * dl_win.c - windows LoadLibrary interface
 *
 *  Copyright(C) 2003 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: dl_win.c,v 1.1 2003-05-15 11:32:54 shirok Exp $
 */

/* This file is included in load.c */

#include <windows.h>

static void *dl_open(const char *path)
{
    return (void*)LoadLibrary(path);
}

static const char *dl_error(void)
{
    char buf[80], *p;
    DWORD code = GetLastError(void);
    sprintf(buf, "error code %d", code);
    p = SCM_NEW_ATOMIC2(strlen(buf)+1, char *);
    strcpy(p, buf);
    return p;
}

static ScmDynLoadInitFn dl_sym(void *handle, const char *name)
{
    return (ScmDynLoadInitFn)GetProcAddress((HMODULE)handle, name);
}

static void dl_close(void *handle)
{
    (void)FreeLibrary((HMODULE)handle);
}
