/*
 * charconv.h - character code conversion library
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
 *  $Id: charconv.h,v 1.2 2001-05-23 09:53:32 shirok Exp $
 */

#ifndef GAUCHE_CHARCONV_H
#define GAUCHE_CHARCONV_H

#include <gauche.h>

#ifdef __cplusplus
extern "C" {
#endif

extern ScmObj Scm_MakeConversionInputPort(ScmString *fromCode,
                                          ScmString *toCode,
                                          ScmPort *fromPort);
extern ScmObj Scm_MakeConversionOutputPort(ScmString *fromCode,
                                           ScmString *toCode,
                                           ScmPort *toPort);

#ifdef __cplusplus
}
#endif

#endif /*GAUCHE_CHARCONV_H*/
