/*
 * getdir_darwin.c - get the library directory at runtime
 *                   for MacOSX Framework build.
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

/* NB: This is used only if --enable-framework is given to the
   configure script.   Otherwise, the absolute paths determined at
   configuration time are embedded in the binary. */

#include <libgen.h>
#include <CoreFoundation/CoreFoundation.h>

/* Must match the id in Info.plist */
#define LIBGAUCHE_ID  "com.schemearts.gauche"

/* Subdirs appended to the bundle path */
#define SUBDIR   "/Versions/Current/"

static const char *get_install_dir()
{
    CFBundleRef bundle     = NULL;
    CFURLRef    bundleURL  = NULL;
    CFStringRef bundlePath = NULL;

#define CLEANUP                                 \
    do {                                        \
        if (bundlePath) CFRelease(bundlePath);  \
        if (bundleURL) CFRelease(bundleURL);    \
        if (bundle) CFRelease(bundle);          \
    } while (0)

    bundle = CFBundleGetBundleWithIdentifier(CFSTR(LIBGAUCHE_ID));
    if (bundle == NULL) {
        /* This call fails when gosh is called during the build process
           (thus, the framework hasn't been created).  For the time
           being, we just return a dummy directory. */
        CLEANUP;
        return ".";
    }
    /* Ownership of bundle follows the Get Rule of Core Foundation.
       ie. we must claim ownership (with the CFRetain function).
       We are then responsible for relinquishing ownership when we
       have finished with it. */
    CFRetain(bundle);

    bundleURL = CFBundleCopyBundleURL(bundle);
    if (bundleURL == NULL) {
        CLEANUP;
        PATH_ERROR("CFBundleCopyBundleURL failed");
    }
    /* Ownership of bundleURL follows the Create Rule of Core Foundation.
       ie. it is our responsibility to relinquish ownership (using CFRelease)
       when we have finished with it. */

    bundlePath = CFURLCopyFileSystemPath(bundleURL, kCFURLPOSIXPathStyle);
    if (bundlePath == NULL) {
        CLEANUP;
        PATH_ERROR("CFURLCopyFileSystemPath failed");
    }
    /* Ownership follows the Create Rule. */

    /* Estimate string length in utf8.  This is provisional; we'll refine
       the code later. */
    size_t utf16len = (size_t)CFStringGetLength(bundlePath);
    size_t maxlen = 3 * (utf16len+1)/2;
    size_t bufsiz = maxlen + strlen(SUBDIR) + 1;
    char* buf = PATH_ALLOC(bufsiz);

    if (!CFStringGetCString(bundlePath, buf, maxlen, kCFStringEncodingUTF8)) {
        CLEANUP;
        PATH_ERROR("CFStringGetCString failed");
    }
    strcat(buf, SUBDIR);
    CLEANUP;
    return buf;
}

