/*
 * charconv.c - character code conversion library
 *
 *  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: charconv.c,v 1.5 2001-05-30 09:01:17 shirok Exp $
 */

#include <errno.h>
#include <gauche.h>

#ifdef HAVE_ICONV_H
#include <iconv.h>
#else
typedef void *iconv_t;
iconv_t iconv_open(const char *tocode, const char *fromcode);
int iconv_close(iconv_t handle);
int iconv(iconv_t handle, const char **inbuf, size_t *inroom, char **outbuf, size_t *outroom);
#endif /* !HAVE_ICONV_H */

#define DEFAULT_CONVERSION_BUFFER_SIZE 1024

typedef struct conv_info_rec {
    iconv_t handle;
    ScmPort *remote;
    int bufsiz;
    char *inbuf;
    char *inptr;
    char *outbuf;
    char *outptr;
} conv_info;

/* Called when the parent port needs more data in its buffer, i.e. 
   in conv_info.outbuf. */
static int conv_input_filler(char *buf, int len, void *data)
{
    conv_info *info = (conv_info*)data;
    size_t inleft, outleft, converted;
    const char *inbuf = info->inbuf;
    char *outbuf = info->outbuf;

    /* fill the input buffer */
    
    inleft = info->inptr - info->inbuf;
    outleft = info->outptr - info->outbuf;
    converted = iconv(info->handle, &inbuf, &inleft, &outbuf, &outleft);
    
    if (converted == (size_t)-1) {
        if (errno == EINVAL) {
            /* conversion stopped due to an incomplete character at the
               end of the input buffer. */
        } else if (errno == E2BIG) {
            /* output buffer is full. */
        } else {
            /* it's likely that input contains invalid sequence. */
        }
    }
    return 0;
}

ScmObj Scm_MakeInputConversionPort(ScmPort *fromPort,
                                   ScmString *fromCode,
                                   ScmString *toCode,
                                   int bufsiz)
{
    conv_info *cinfo;
    iconv_t handle;
    ScmPort *newport;
    
    if (!SCM_IPORTP(fromPort))
        Scm_Error("input port required, but got %S", fromPort);

    handle = iconv_open(Scm_GetStringConst(toCode),
                        Scm_GetStringConst(fromCode));
    if (handle == (iconv_t)-1) {
        if (errno == EINVAL) {
            Scm_Error("conversion from code %S to code %S is not supported",
                      SCM_OBJ(fromCode), SCM_OBJ(toCode));
        } else {
            /* TODO: try to call gc to collect unused file descriptors */
            Scm_SysError("iconv_open failed");
        }
    }
    cinfo = SCM_NEW(conv_info);
    cinfo->handle = handle;
    cinfo->remote = fromPort;
    cinfo->bufsiz = (bufsiz > 0)? bufsiz : DEFAULT_CONVERSION_BUFFER_SIZE;
    cinfo->inbuf = SCM_NEW_ATOMIC2(char *, bufsiz);
    cinfo->inptr = cinfo->inbuf;
    cinfo->outbuf = SCM_NEW_ATOMIC2(char *, bufsiz);
    cinfo->outptr = cinfo->outbuf;
    
    return Scm_MakeBufferedPort(SCM_PORT_INPUT, cinfo->bufsiz, 0,
                                cinfo->outbuf, conv_input_filler,
                                (void*)cinfo);
}

/* Initialization */
extern void Scm_Init_convlib(ScmModule *module);

void Scm_Init_charconv(void)
{
    Scm_Init_convlib(Scm_GaucheModule());
}

#ifndef HAVE_ICONV_H
/* substitution of iconv for the platforms where it is not available */

/* for now, we only have dummy functions */
iconv_t iconv_open(const char *tocode, const char *fromcode)
{
    return (iconv_t)-1;
}

int iconv_close(iconv_t handle)
{
    return 0;
}

int iconv(iconv_t handle, const char **inbuf, size_t *inroom, char **outbuf, size_t *outroom)
{
    return 0;
}
#endif /* !HAVE_ICONV_H */

