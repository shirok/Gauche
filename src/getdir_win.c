/*
 * getdir_win.c - get the library directory at runtime (fow windows)
 *  included from paths.c
 *  This routine is shared between libgauche and gauche-config.c.
 *  In libgauche, it returns GC_malloc-ed buffer.  In gauche-config.c,
 *  it returns malloc-ed buffer.
 */

#include <string.h>

static const char *get_install_dir(void (*errfn)(const char *msg, ...))
{
    HMODULE mod;
    DWORD r;
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
    return SCM_WCS2MBS(path);
}
