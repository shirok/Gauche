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
 *  $Id: bsddb.h,v 1.3 2001-12-03 09:46:53 shirok Exp $
 */

#ifndef GAUCHE_BSDDB_H
#define GAUCHE_BSDDB_H

#include <gauche.h>
#include "dbmconfig.h"
#include <db.h>

/* I try to expose as many features of BSD DB as possible, but
   had to drop a few features from Scheme interface. */

typedef struct ScmBsdDbRec {
    SCM_HEADER;
    ScmObj name;
    DB *db;
    int type;                   /* DB_BTREE, DB_HASH, DB_RECNO or DB_UNKNOWN */
    u_long flags;               /* btree, recno */
    u_int cachesize;            /* btree, hash, recno */
    int maxkeypage;             /* btree */
    int minkeypage;             /* btree */
    u_int psize;                /* btree, recno */
    int lorder;                 /* btree, hash, recno; 1 for BE, -1 for LE,
                                   0 for unknown. */
    int bsize;                  /* hash */
    int ffactor;                /* hash */
    int nelem;                  /* hash */
    size_t reclen;              /* recno */
    u_char bval;                /* recno */
} ScmBsdDb;

extern ScmClass Scm_BsdDbClass;
#define SCM_CLASS_BSD_DB     (&Scm_BsdDbClass)
#define SCM_BSD_DB(obj)      ((ScmBsdDb*)obj)
#define SCM_BSD_DB_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_BSD_DB)
#define SCM_BSD_DB_TYPE(obj) SCM_BSD_DB(obj)->type

extern void Scm_Init_bsddb(ScmModule *mod);

#endif /*GAUCHE_BSDDB_H*/
