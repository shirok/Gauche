/*
 * extend.h - Stuff useful to write Gauche extension
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: extend.h,v 1.2 2002-06-10 00:28:21 shirok Exp $
 */

#ifndef GAUCHE_EXTEND_H
#define GAUCHE_EXTEND_H

#ifndef GAUCHE_H
#include <gauche.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define SCM_INIT_EXTENSION(name)					\
   do {									\
       extern GC_PTR SCM_CPP_CAT(Scm__datastart_, name);		\
       extern GC_PTR SCM_CPP_CAT(Scm__dataend_, name);			\
       extern GC_PTR SCM_CPP_CAT(Scm__bssstart_, name);		        \
       extern GC_PTR SCM_CPP_CAT(Scm__bssend_, name);			\
       GC_register_dlopen_data(&SCM_CPP_CAT(Scm__datastart_, name),     \
                               &SCM_CPP_CAT(Scm__dataend_, name),       \
                               &SCM_CPP_CAT(Scm__bssstart_, name),      \
                               &SCM_CPP_CAT(Scm__bssend_, name));       \
   } while (0)

#ifdef __cplusplus
}
#endif

#endif /* GAUCHE_EXTEND_H */
