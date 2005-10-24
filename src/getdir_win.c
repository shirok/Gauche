/*
 * getdir_win.c - get the library directory at runtime (fow windows)
 *  included from paths.c
 *
 * $Id: getdir_win.c,v 1.1 2005-10-24 01:37:21 shirok Exp $
 */

#include <windows.h>
#include <string.h>

static int get_install_dir(char *buf, int buflen,
                           void (*errfn)(const char *msg, ...))
{
    HMODULE mod;
    DWORD r;
    char path[MAX_PATH];
    int len;

    if ((mod = GetModuleHandle("libgauche.dll")) == NULL) {
        errfn("GetModuleHandle failed");
    }
    if ((r = GetModuleFileName(mod, path, MAX_PATH)) == 0) {
        errfn("GetModuleFileName failed");
    }
    /* remove \libgauche.dll */
    if (!PathRemoveFileSpec(path)) {
        errfn("PathRemoveFileSpec failed on %s", path);
    }
    /* remobe \bin */
    if (!PathRemoveFileSpec(path)) {
        errfn("PathRemoveFileSpec failed on %s", path);
    }
    len = strlen(path);
    if (len >= buflen-1) {
        errfn("Pathname too long: %s", path);
    }
    strcpy(buf, path);
    return len;
}
