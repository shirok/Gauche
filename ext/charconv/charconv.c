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
 *  $Id: charconv.c,v 1.8 2001-06-04 20:07:48 shirok Exp $
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

/*------------------------------------------------------------
 * Query
 */

int Scm_ConversionSupportedP(const char *from, const char *to)
{
    iconv_t cd = iconv_open(to, from);
    if (cd == (iconv_t)-1) return FALSE;
    iconv_close(cd);
    return TRUE;
}

/*------------------------------------------------------------
 * Input conversion
 *
 *  <-- Bufferd port <--outbuf-- filler <--inbuf--- getz(remote)
 */

static int conv_input_filler(char *buf, int len, void *data)
{
    conv_info *info = (conv_info*)data;
    size_t insize, inroom, outroom, result;
    int nread;
    const char *inbuf = info->inbuf;
    char *outbuf = info->outbuf;

    /* Fill the input buffer.  There are some remaining bytes in the
       inbuf from the last conversion (insize), so we try to fill the
       rest. */
    insize = info->inptr - info->inbuf, 
    nread = Scm_Getz(info->inptr, info->bufsiz - insize, info->remote);
    if (nread <= 0) {
        if (insize == 0) return 0; /* All done. */
    } else {
        insize += nread;
    }

    /* Conversion. */
    inroom = insize;
    outroom = info->bufsiz;
#ifdef DEBUG
    fprintf(stderr, "=> in(%p,%p)%d out(%p,%p)%d\n",
            info->inbuf, info->inptr, insize,
            info->outbuf, info->outptr, outroom);
#endif
    result = iconv(info->handle, &inbuf, &inroom, &outbuf, &outroom);
#ifdef DEBUG
    fprintf(stderr, "<= r=%d, in(%p)%d out(%p)%d\n",
           result, inbuf, inroom, outbuf, outroom);
#endif
    if (result == (size_t)-1) {
        if (errno == EINVAL || errno == E2BIG) {
            /* Conversion stopped due to an incomplete character at the
               end of the input buffer, or the output buffer is full.
               We shift the unconverted bytes to the beginning of input
               buffer. */
            memmove(info->inbuf, info->inbuf+insize-inroom, inroom);
            info->inptr = info->inbuf + inroom;
            return info->bufsiz - outroom;
        } else {
            /* it's likely that input contains invalid sequence.
               TODO: we should handle this case gracefully. */
            Scm_Error("invalid character sequence in the input stream");
            return 0;           /* dummy */
        }
    } else {
        /* Conversion is done completely. */
        SCM_ASSERT(inroom == 0);
        info->inptr = info->inbuf;
        return info->bufsiz - outroom;
    }
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
    cinfo->inbuf = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
    cinfo->inptr = cinfo->inbuf;
    cinfo->outbuf = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
    cinfo->outptr = cinfo->outbuf;
    
    return Scm_MakeBufferedPort(SCM_PORT_INPUT, cinfo->bufsiz, 0,
                                cinfo->outbuf, conv_input_filler,
                                (void*)cinfo);
}

/*------------------------------------------------------------
 * Output conversion
 *
 *   Bufferd port -->outbuf--> flusher -->inbuf--> putz(remote)
 */

static int conv_output_flusher(char *buf, int len, void *data)
{
    conv_info *info = (conv_info*)data;
    size_t outsize, inroom, outroom, result;
    int nread;
    const char *inbuf;
    char *outbuf;

    inbuf = info->inbuf;
    outbuf = info->outptr;
    for (;;) {
        /* Conversion. */
        outsize = info->bufsiz - (info->outptr - info->outbuf);
        inroom = len;
        outroom = outsize;
#ifdef DEBUG
        fprintf(stderr, "=> in(%p,%p)%d out(%p,%p)%d\n",
                info->inbuf, info->inptr, insize,
                info->outbuf, info->outptr, outroom);
#endif
        result = iconv(info->handle, &inbuf, &inroom, &outbuf, &outroom);
#ifdef DEBUG
        fprintf(stderr, "<= r=%d, in(%p)%d out(%p)%d\n",
                result, inbuf, inroom, outbuf, outroom);
#endif
        if (result == (size_t)-1) {
            if (errno == EINVAL) {
                /* Conversion stopped due to an incomplete character at the
                   end of the input buffer.  We just return # of bytes
                   flushed.  (Shifting unconverted characters is done by
                   buffered port routine) */
                return len - outroom;
            } else if (errno == E2BIG) {
                /* Output buffer got full.  Flush it. */
                Scm_Putz(info->outbuf, info->bufsiz - outroom, info->remote);
                outbuf = info->outptr = info->outbuf;
                continue;
            } else {
                /* it's likely that input contains invalid sequence.
                   TODO: we should handle this case gracefully. */
                Scm_Error("invalid character sequence in the input stream");
                return 0;           /* dummy */
            }
        } else {
            /* Conversion is done completely.  Update outptr. */
            SCM_ASSERT(inroom == 0);
            info->outptr = outbuf;
            if (buf == NULL) {
                /* the port is closed.  flush outbuf */
                Scm_Putz(info->outbuf, info->outptr - info->outbuf,
                         info->remote);
            }
            return len;
        }
    }
}

ScmObj Scm_MakeOutputConversionPort(ScmPort *toPort,
                                    ScmString *toCode,
                                    ScmString *fromCode,
                                    int bufsiz)
{
    conv_info *cinfo;
    iconv_t handle;
    ScmPort *newport;
    
    if (!SCM_OPORTP(toPort))
        Scm_Error("output port required, but got %S", toPort);

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
    cinfo->remote = toPort;
    cinfo->bufsiz = (bufsiz > 0)? bufsiz : DEFAULT_CONVERSION_BUFFER_SIZE;
    cinfo->inbuf = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
    cinfo->inptr = cinfo->inbuf;
    cinfo->outbuf = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
    cinfo->outptr = cinfo->outbuf;
    
    return Scm_MakeBufferedPort(SCM_PORT_OUTPUT, cinfo->bufsiz, 0,
                                cinfo->inbuf, conv_output_flusher,
                                (void*)cinfo);
}

/*====================================================================
 * Initialization
 */
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

