/*
 * Small utilities for Windows support.
 *
 * Note: Most compatibility stuff are in system.c.  This is splitted
 * away so that it can be used from both libgauche and gauche-config,
 * the latter of which doesn't link to the rest of gauche stuff.
 *
 * This file is to included from both system.c and gauche-config.c.
 * We distinguish which file we're included by checking GAUCHE_H.
 *
 * If used from gauche-config.c, we always allocate memory by malloc
 * (gauche-config doesn't link to GC), regardless of use_gc flag.
 * If used from system.c, we allocate GC-able memory when use_gc is true,
 * and malloc when it is false.
 * The caller is responsible to free it if malloc is used.
 */

static WCHAR *mbs2wcs(const char *s, int use_gc,
                      void (*errfn)(const char *, ...))
{
    WCHAR *wb;
    int nc = MultiByteToWideChar(CP_UTF8, 0, s, -1, NULL, 0);
    if (nc == 0) {
        errfn("Windows error %d on MultiByteToWideChar", GetLastError());
    }
#if defined(GAUCHE_H)
    if (use_gc) wb = SCM_NEW_ATOMIC_ARRAY(WCHAR, nc);
    else        wb = (WCHAR*)malloc(nc * sizeof(WCHAR));
#else
    wb = (WCHAR*)malloc(nc * sizeof(WCHAR));
#endif
    if (MultiByteToWideChar(CP_UTF8, 0, s, -1, wb, nc) == 0) {
        errfn("Windows error %d on MultiByteToWideChar", GetLastError());
    }
    return wb;
}

static const char *wcs2mbs(const WCHAR *s, int use_gc,
                           void (*errfn)(const char*, ...))
{
    char *mb;
    int nb = WideCharToMultiByte(CP_UTF8, 0, s, -1, NULL, 0, 0, 0);
    if (nb == 0) {
        errfn("Windows error %d on WideCharToMultiByte", GetLastError());
    }
#if defined(GAUCHE_H)
    if (use_gc) mb = SCM_NEW_ATOMIC_ARRAY(char, nb);
    else        mb = (char*)malloc(nb);
#else
    mb = (char*)malloc(nb);
#endif
    if (WideCharToMultiByte(CP_UTF8, 0, s, -1, mb, nb, 0, 0) == 0) {
        errfn("Windows error %d on WideCharToMultiByte", GetLastError());
    }
    return mb;
}

