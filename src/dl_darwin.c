/*
 * dl_darwin.c - dlopen() interface for MacOS X/Darwin
 *
 *  Copyright(C) 2003 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: dl_darwin.c,v 1.3 2003-05-21 08:54:02 shirok Exp $
 */

/* Cf: Technical Note TN2071 "Porting Command Line Unix Tools to MacOS X"
   http://developer.apple.com/technotes/tn2002/tn2071.html
   Cf: Object File Image Functions
   http://developer.apple.com/techpubs/macosx/DeveloperTools/MachORuntime/5rt_api_reference/chapter_5_section_3.html
*/

/* This file is included in load.c */

/* NB: This isn't really used for now, since GC requires a special wrapper
   around dlopen() and we can't casually replace it.  I need to tweak
   GC's external API so that we can safely wrap these calls. */

#include <mach-o/dyld.h>

/* we can store the error message in static variable, because
   the caller guarantees dl_open() and subsequent dl_error()
   (in case dl_open() fails) is atomic. */
static const char *dl_open_errstr = NULL;

static void *dl_open(const char *path)
{
    NSObjectFileImage image;
    NSObjectFileImageReturnCode r;
    NSModule module;
    unsigned long options = NSLINKMODULE_OPTION_BINDNOW|NSLINKMODULE_OPTION_RETURN_ON_ERROR;

    r = NSCreateObjectFileImageFromFile(path, &image);
    if (r != NSObjectFileImageSuccess) {
        switch (r) {
        case NSObjectFileImageInappropriateFile:
            dl_open_errstr = "The specified Mach-O file is not of a type this function can operate upon.";
            break;
        case NSObjectFileImageArch:
            dl_open_errstr = "The specified Mach-O file is for a different CPU architecture.";
            break;
        case NSObjectFileImageFormat:
            dl_open_errstr = "The specified file does not appear to be a Mach-O file.";
            break;
        case NSObjectFileImageAccess:
            dl_open_errstr = "The access permissions for the specified file do not permit the creation of the image.";
            break;
        default:
            dl_open_errstr = "Unknown error.";
            break;
        }
        return NULL;
    }

    module = NSLinkModule(image, path, options);
    NSDestroyObjectFileImage(image);
    if (module == NULL) {
        dl_open_errstr = "NSLinkModule failed";
    }
    return module;
}

static const char *dl_error(void)
{
    return dl_open_errstr;
}

static ScmDynLoadInitFn dl_sym(void *handle, const char *name)
{
    NSSymbol sym = NSLookupSymbolInModule((NSModule)handle, name);
    if (sym == NULL) return NULL;
    return (ScmDynLoadInitFn)NSAddressOfSymbol(sym);
}

static void dl_close(void *handle)
{
    (void)NSUnLinkModule((NSModule)handle, NSUNLINKMODULE_OPTION_NONE);
}

