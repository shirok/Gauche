/*
 * dl_dummy.c - dummy dlopen() interface
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
 *  $Id: dl_dummy.c,v 1.1 2003-05-15 11:32:54 shirok Exp $
 */

/* This file is included in load.c */

#include <dlfcn.h>

static void *dl_open(const char *path)
{
    return NULL;
}

static const char *dl_error(void)
{
    return "dynamic loading is not supported on this platform";
}

static void *dl_sym(void *handle, const char *name)
{
    return NULL;
}

static void dl_close(void *handle)
{
    /* nothing to do */
}


