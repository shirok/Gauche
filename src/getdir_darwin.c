/*
 * getdir_darwin.c - get the library directory at runtime
 *                   for MacOSX Framework build.
 *  included from paths.c
 *
 * $Id: getdir_darwin.c,v 1.1 2005-10-24 01:37:21 shirok Exp $
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
    CFBundleRef bundle;
    CFURLRef bundleURL;
    CFStringRef bundlePath;

    bundle = CFBundleGetBundleWithIdentifier(CFSTR(LIBGAUCHE_ID));
    if (bundle == NULL) {
        /* This call fails when gosh is called during the build process
           (thus, the framework hasn't been created).  For the time
           being, we just return a dummy directory. */
        strcpy(buf, ".");
        return 1;
    }
    bundleURL = CFBundleCopyBundleURL(bundle);
    if (bundleURL == NULL) {
        errfn("CFBundleCopyBundleURL failed");
    }
    bundlePath = CFURLCopyFileSystemPath(bundleURL, kCFURLPOSIXPathStyle);
    if (bundlePath == NULL) {
        errfn("CFURLCopyFileSystemPath failed");
    }
    if (!CFStringGetCString(bundlePath, buf, buflen, kCFStringEncodingUTF8)) {
        errfn("CFStringGetCString failed");
    }
    if (strlen(buf) + strlen(SUBDIR) + 1 >= buflen) {
        errfn("pathname too long");
    }
    strcat(buf, SUBDIR);
    CFRelease(bundlePath);
    CFRelease(bundleURL);
    CFRelease(bundle);
    return strlen(buf);
}

