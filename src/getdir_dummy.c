/*
 * getdir_dummy.c - dummy funcion for get_install_dir
 *  included from paths.c
 */

static const char *get_install_dir(void (*errfn)(const char *, ...))
{
    errfn("We can't obtain runtime pathname on this platform");
    return "/";                 /* dummy */
}
