/*
 * Common routine to find cacert.pem installed by tools/get-cacert.
 * This is used by system_cert_loader and Scm_TLSSystemCABundleAvailable.
 *
 * We can't consolidate it, for tls-mbed.c may be compiled into a separate
 * DLL, and it is cumbersome on Windows to do cross-DLL reference.
 */

static const char *in_gauche_cacert_path()
{
    ScmObj path = Scm_StringAppendC(SCM_STRING(Scm_LibraryDirectory()),
                                    "/../cacert.pem", -1, -1);
    return Scm_GetStringConst(SCM_STRING(path));
}
