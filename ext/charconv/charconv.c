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
 *  $Id: charconv.c,v 1.2 2001-05-23 09:53:32 shirok Exp $
 */

#include <iconv.h>
#include <errno.h>
#include <gauche.h>


typedef struct conv_info_rec {
    iconv_t handle;
    ScmPort *remote;
} conv_info;

ScmObj Scm_MakeConversionInputPort(ScmString *fromCode,
                                   ScmString *toCode,
                                   ScmPort *fromPort)
{
    conv_info *cinfo;
    iconv_t handle;
    ScmPort *newport;
    
    if (!SCM_IPORTP(fromPort))
        Scm_Error("input port required, but got %S", fromPort);

    handle = iconv_open(Scm_GetStringConst(toCode),
                        Scm_GetStringConst(fromCode));
    if (handle == NULL) {
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
    /*WRITEME*/
    return SCM_UNDEFINED;
}


