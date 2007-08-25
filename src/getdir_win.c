/*
 * getdir_win.c - get the library directory at runtime (fow windows)
 *  included from paths.c
 *
 * $Id: getdir_win.c,v 1.4 2007-08-25 02:29:31 shirok Exp $
 */

#include <windows.h>
#include <shlwapi.h>
#include <string.h>

static int get_install_dir(char *buf, int buflen,
                           void (*errfn)(const char *msg, ...))
{
    HMODULE mod;
    DWORD r;
    int len;
    const char *mbpath;
#if defined(UNICODE)
    WCHAR path[MAX_PATH];
    const WCHAR *libname = L"libgauche.dll";
#else
    char path[MAX_PATH];
    const char *libname = "libgauche.dll";
#endif

    if ((mod = GetModuleHandle(libname)) == NULL) {
        errfn("GetModuleHandle failed");
    }
    if ((r = GetModuleFileName(mod, path, MAX_PATH)) == 0) {
        errfn("GetModuleFileName failed");
    }
    /* remove \libgauche.dll */
    if (!PathRemoveFileSpec(path)) {
        errfn("PathRemoveFileSpec failed on %s", SCM_WCS2MBS(path));
    }
    /* remobe \bin */
    if (!PathRemoveFileSpec(path)) {
        errfn("PathRemoveFileSpec failed on %s", SCM_WCS2MBS(path));
    }
    mbpath = SCM_WCS2MBS(path);
    len = (int)strlen(mbpath);
    if (len >= buflen-1) {
        errfn("Pathname too long: %s", mbpath);
    }
    strcpy(buf, mbpath);
    return len;
}
