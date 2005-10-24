/*
 * getdir_dummy.c - dummy funcion for get_install_dir
 *  included from paths.c
 *
 * $Id: getdir_dummy.c,v 1.1 2005-10-24 01:37:21 shirok Exp $
 */

static int get_install_dir(char *buf, int buflen,
                           void (*errfn)(const char *, ...))
{
    errfn("We can't obtain runtime pathname on this platform");
    return 0;
}
