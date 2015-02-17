/*
 * dl_darwin.c - dlopen() interface for MacOS X/Darwin
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
    unsigned long options = NSLINKMODULE_OPTION_BINDNOW|NSLINKMODULE_OPTION_RETURN_ON_ERROR;

    NSObjectFileImageReturnCode r = NSCreateObjectFileImageFromFile(path, &image);
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

    NSModule module = NSLinkModule(image, path, options);
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
