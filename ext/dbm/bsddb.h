/*
 * bsddb.h - BSD DB interface
 *
 *  Copyright(C) 2001-2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: bsddb.h,v 1.4 2002-12-31 09:36:51 shirok Exp $
 */

#ifndef GAUCHE_BSDDB_H
#define GAUCHE_BSDDB_H

#include <gauche.h>
#include <gauche/class.h>
#include "dbmconfig.h"
#include <db.h>

/* I try to expose as many features of BSD DB as possible, but
   had to drop a few features from Scheme interface. */

typedef struct ScmBsdDbRec {
    SCM_HEADER;
    ScmObj name;
    DB *db;
    void *info;
} ScmBsdDb;

extern ScmClass Scm_BsdDbClass;
#define SCM_CLASS_BSD_DB     (&Scm_BsdDbClass)
#define SCM_BSD_DB(obj)      ((ScmBsdDb*)obj)
#define SCM_BSD_DB_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_BSD_DB)

extern ScmClass Scm_BsdBtreeClass;
#define SCM_CLASS_BSD_BTREE     (&Scm_BsdBtreeClass)
#define SCM_BSD_BTREE(obj)      ((ScmBsdBtree*)obj)
#define SCM_BSD_BTREE_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_BSD_BTREE)

extern ScmClass Scm_BsdHashClass;
#define SCM_CLASS_BSD_HASH     (&Scm_BsdHashClass)
#define SCM_BSD_HASH(obj)      ((ScmBsdHash*)obj)
#define SCM_BSD_HASH_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_BSD_HASH)

extern ScmClass Scm_BsdRecnoClass;
#define SCM_CLASS_BSD_RECNO     (&Scm_BsdRecnoClass)
#define SCM_BSD_RECNO(obj)      ((ScmBsdRecno*)obj)
#define SCM_BSD_RECNO_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_BSD_RECNO)

extern void Scm_Init_bsddb(ScmModule *mod);

#endif /*GAUCHE_BSDDB_H*/
