/*
 * bsddb.h - BSD DB interface
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
 *  $Id: bsddb.h,v 1.1 2001-11-17 11:16:28 shirok Exp $
 */

#ifndef GAUCHE_BSDDB_H
#define GAUCHE_BSDDB_H

#include <gauche.h>
#include <dbconfig.h>
#include <db.h>

typedef struct ScmBsdDbRec {
    SCM_HEADER;
    const char *file;
    int flags;
    int mode;
    DBTYPE type;
    DB *db;
} ScmBsdDb;

extern ScmClass Scm_BsdDbClass;
#define SCM_CLASS_BSD_DB     (&Scm_BsdDbClass)
#define SCM_BSD_DB(obj)      ((ScmBsdDb*)obj)
#define SCM_BSD_DB_T(obj)    Scm_TypeP(obj, SCM_CLASS_BSD_DB)

#define SCM_BSD_DB_TYPE(obj) SCM_BSD_DB(obj)->type

ScmObj Scm_BsdDbType(ScmBsdDb *db);

#endif /*GAUCHE_BSDDB_H*/
