/*
 * getdir_dummy.c - dummy funcion for get_install_dir
 *  included from paths.c
 */

static int get_install_dir(char *buf, int buflen,
                           void (*errfn)(const char *, ...))
{
    errfn("We can't obtain runtime pathname on this platform");
    return 0;
}
