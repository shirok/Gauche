/*
 * vminline.h - declarations of pre-defined inliners
 *
 *  Copyright(C) 2000 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, ditribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: vminline.h,v 1.3 2001-01-16 09:08:46 shiro Exp $
 */

#ifndef GAUCHE_VMINLINE_H_
#define GAUCHE_VMINLINE_H_

extern int x;
extern ScmObj Scm_inline_cons(ScmSubr *, ScmObj, ScmObj, int);


extern ScmObj Scm_inline_memv(ScmSubr *, ScmObj, ScmObj, int);


extern ScmObj Scm_inline_vector(ScmSubr *, ScmObj, ScmObj, int);
extern ScmObj Scm_inline_vector_length(ScmSubr *, ScmObj, ScmObj, int);
extern ScmObj Scm_inline_vector_ref(ScmSubr *, ScmObj, ScmObj, int);
extern ScmObj Scm_inline_vector_set(ScmSubr *, ScmObj, ScmObj, int);


#endif /*GAUCHE_VMINLINE_H_*/

