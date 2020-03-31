/*
 * getdir_procfs.c - get the library directory at runtime
 *                   for procfs supported systems
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

#include <stdio.h>
#include <gauche/config.h>

#define MAPS_LINE_MAX 4096

static const char *get_install_dir()
{
    FILE *fp = fopen("/proc/self/maps", "r");
    if (fp == NULL) return NULL;
    
    char buf[MAPS_LINE_MAX+1];
    while (fgets(buf, MAPS_LINE_MAX, fp) != NULL) {
        const char *p = strstr(buf, "libgauche-"GAUCHE_ABI_VERSION".so");
        if (p) {
            for (const char *q = p-1; q >= buf; q--) {
                if (*q == ' ') {
                    q++;
                    if (q == p) {
                        return ".";
                    } else {
                        char *r = PATH_ALLOC(p - q + 1);
                        strncpy(r, q, p-q);
                        r[p-q] = '\0';
                        return r;
                    }
                }
            }
        }
    }
    if (ferror(fp)) PATH_ERROR("Read error from /proc/self/maps");
    return NULL;
}
