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
 *  $Id: charconv.h,v 1.7 2001-06-07 19:42:07 shirok Exp $
 */

#ifndef GAUCHE_CHARCONV_H
#define GAUCHE_CHARCONV_H

#include <gauche.h>

#ifdef __cplusplus
extern "C" {
#endif

extern ScmObj Scm_MakeInputConversionPort(ScmPort *source,
                                          const char *fromCode,
                                          const char *toCode,
                                          int bufsiz,
                                          int ownerp);
extern ScmObj Scm_MakeOutputConversionPort(ScmPort *sink,
                                           const char *toCode,
                                           const char *fromCode,
                                           int bufsiz,
                                           int ownerp);

typedef const char *(*ScmCodeGuessingProc)(const char *buf,
                                           int bufsiz,
                                           void *data);

extern const char *Scm_GetCESName(ScmObj code, const char *argname);
extern int Scm_ConversionSupportedP(const char *from, const char *to);

extern void Scm_RegisterCodeGuessingProc(const char *code,
                                         ScmCodeGuessingProc proc,
                                         void *data);

#ifdef __cplusplus
}
#endif

#endif /*GAUCHE_CHARCONV_H*/
