/*
 * class.h - Gauche object system private header
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
 *  $Id: class.h,v 1.1 2001-03-15 08:01:10 shiro Exp $
 */

#ifndef GAUCHE_CLASS_H
#define GAUCHE_CLASS_H

#ifdef __cplusplus
extern "C" {
#endif

/* object system base classes */
typedef struct ScmGenericFunctionRec {
    SCM_HEADER;
    
} ScmGenericFunction;


/* for subclassing */
typedef struct ScmSubClassRec {
    ScmClass klassrec;
    ScmObj slots[1];
} ScmSubClass;


extern ScmObj Scm_ClassAllocate(ScmClass *klass, int nslots);
extern ScmObj Scm_AllocateInstance(ScmClass *klass, int nslots);

#ifdef __cplusplus
}
#endif

#endif /* GAUCHE_CLASS_H */
