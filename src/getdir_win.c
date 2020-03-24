/*
 * getdir_win.c - get the library directory at runtime (fow windows)
 *  included from paths.c
 *
 *  This file must define a static function get_install_dir, which returns
 *  the pathanme of the directory where libgauche DSO is installed.
 *  It can return NULL if such directory can't be determined.
 *  If an unexpected error occurs during operations, call errfn(), which
 *  won't return.
 *
 *  The function may be called within gauche-config, which doesn't link Gauche
 *  runtime.  So it shouldn't use any ScmObj stuff.
 *
 *  If it needs to allocate, use PATH_ALLOC() macro.  You don't need to
 *  worry about freeing; gauche-config is short-living program so it doesn't
 *  bother to free stuff, and libgauche manages allocated memory with GC.
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
