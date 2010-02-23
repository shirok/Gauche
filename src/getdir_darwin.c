/*
 * getdir_darwin.c - get the library directory at runtime
 *                   for MacOSX Framework build.
 *  included from paths.c
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

static int get_install_dir(char *buf, int buflen,
                           void (*errfn)(const char *, ...))
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
        goto error;
    }
    /* Ownership of bundle follows the Get Rule of Core Foundation.
       ie. we must claim ownership (with the CFRetain function).
       We are then responsible for relinquishing ownership when we
       have finished with it. */
    CFRetain(bundle);

    bundleURL = CFBundleCopyBundleURL(bundle);
    if (bundleURL == NULL) {
        CLEANUP;
        errfn("CFBundleCopyBundleURL failed");
        goto error;
    }
    /* Ownership of bundleURL follows the Create Rule of Core Foundation.
       ie. it is our responsibility to relinquish ownership (using CFRelease)
       when we have finished with it. */

    bundlePath = CFURLCopyFileSystemPath(bundleURL, kCFURLPOSIXPathStyle);
    if (bundlePath == NULL) {
        CLEANUP;
        errfn("CFURLCopyFileSystemPath failed");
        goto error;
    }
    /* Ownership follows the Create Rule. */

    if (!CFStringGetCString(bundlePath, buf, buflen, kCFStringEncodingUTF8)) {
        CLEANUP;
        errfn("CFStringGetCString failed");
        goto error;
    }
    if (strlen(buf) + strlen(SUBDIR) + 1 >= buflen) {
        CLEANUP;
        errfn("pathname too long");
        goto error;
    }

    strcat(buf, SUBDIR);
    CLEANUP;
    return strlen(buf);
 error:
    /* For the time being, we just return a dummy directory. */
    strcpy(buf, ".");
    return 1;
}

