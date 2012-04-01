/*
 * Small utilities for Windows support.
 *
 * Note: Most compatibility stuff are in system.c.  This is splitted
 * away so that it can be used from both libgauche and gauche-config,
 * the latter of which doesn't link to the rest of gauche stuff.
 *
 * This file is to included from both system.c and gauche-config.c.
 */

/* If used from gauche-config.c, the caller must free the allocated
   buffer (gauche-config doesn't link to GC). */

static WCHAR *mbs2wcs(const char *s, void (*errfn)(const char *, ...))
{
    WCHAR *wb;
    int nc = MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, s, -1, NULL, 0);
    if (nc == 0) {
        errfn("Windows error %d on MultiByteToWideChar", GetLastError());
    }
#if defined(GAUCHE_H)
    wb = SCM_NEW_ATOMIC_ARRAY(WCHAR, nc);
#else
    wb = (WCHAR*)malloc(nc * sizeof(WCHAR));
#endif
    if (MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, s, -1, wb, nc) == 0) {
        errfn("Windows error %d on MultiByteToWideChar", GetLastError());
    }
    return wb;
}

static const char *wcs2mbs(const WCHAR *s, void (*errfn)(const char*, ...))
{
    char *mb;
    int nb = WideCharToMultiByte(CP_UTF8, 0, s, -1, NULL, 0, 0, 0);
    if (nb == 0) {
        errfn("Windows error %d on WideCharToMultiByte", GetLastError());
    }
#if defined(GAUCHE_H)
    mb = SCM_NEW_ATOMIC_ARRAY(char, nb);
#else
    mb = (char*)malloc(nb);
#endif
    if (WideCharToMultiByte(CP_UTF8, 0, s, -1, mb, nb, 0, 0) == 0) {
        errfn("Windows error %d on WideCharToMultiByte", GetLastError());
    }
    return mb;
}

