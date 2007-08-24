/*
 * getdir_win.c - get the library directory at runtime (fow windows)
 *  included from paths.c
 *
 * $Id: getdir_win.c,v 1.3 2007-08-24 23:55:42 shirok Exp $
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
#define WCS2MBS(x) Scm_WCS2MBS(x)
#else
    char path[MAX_PATH];
    const char *libname = "libgauche.dll";
#define WCS2MBS(x) (x)
#endif

    if ((mod = GetModuleHandle(libname)) == NULL) {
        errfn("GetModuleHandle failed");
    }
    if ((r = GetModuleFileName(mod, path, MAX_PATH)) == 0) {
        errfn("GetModuleFileName failed");
    }
    /* remove \libgauche.dll */
    if (!PathRemoveFileSpec(path)) {
        errfn("PathRemoveFileSpec failed on %s", WCS2MBS(path));
    }
    /* remobe \bin */
    if (!PathRemoveFileSpec(path)) {
        errfn("PathRemoveFileSpec failed on %s", WCS2MBS(path));
    }
    mbpath = WCS2MBS(path);
    len = (int)strlen(mbpath);
    if (len >= buflen-1) {
        errfn("Pathname too long: %s", mbpath);
    }
    strcpy(buf, mbpath);
    return len;
}
