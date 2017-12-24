/*
 * getdir_win.c - get the library directory at runtime (fow windows)
 *  included from paths.c
 */

#include <string.h>

static int get_install_dir(char *buf, int buflen,
                           void (*errfn)(const char *msg, ...))
{
    HMODULE mod;
    DWORD r;
    int len;
    const char *mbpath;
    TCHAR path[MAX_PATH];
    const TCHAR *libname = _T("libgauche-"GAUCHE_ABI_VERSION".dll");

    /* We try libugauche.dll, then the process itself.  The latter is
       for the case when gauche is statically linked. */
    if ((mod = GetModuleHandle(libname)) == NULL
	&& (mod = GetModuleHandle(NULL)) == NULL) {
	errfn("GetModuleHandle failed");
    }
    if ((r = GetModuleFileName(mod, path, MAX_PATH)) == 0) {
        errfn("GetModuleFileName failed");
    }
    /* remove \libgauche.dll */
    if (!PathRemoveFileSpec(path)) {
        errfn("PathRemoveFileSpec failed on %s", SCM_WCS2MBS(path));
    }
    /* remove \bin */
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
