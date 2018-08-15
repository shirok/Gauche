/*
 * getdir_dummy.c - dummy funcion for get_install_dir
 *  included from paths.c
 */

static int get_install_dir(char *buf SCM_UNUSED,
                           int buflen SCM_UNUSED,
                           void (*errfn)(const char *, ...) SCM_UNUSED)
{
    errfn("We can't obtain runtime pathname on this platform");
    return 0;
}
