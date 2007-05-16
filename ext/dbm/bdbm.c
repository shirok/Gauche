/*
 * bsddb.c - BSD DB interface
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
 *  $Id: bdbm.c,v 1.4 2007-05-16 03:27:04 shirok Exp $
 */

#include "bsddb.h"
#include <gauche/class.h>

static ScmObj bsddb_allocate(ScmClass *klass, ScmObj initargs);
static void   bsddb_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);

SCM_DEFINE_BUILTIN_CLASS(Scm_BsdDbClass,
                         bsddb_print, NULL, NULL,
                         bsddb_allocate,
                         SCM_CLASS_COLLECTION_CPL);

static void bsddb_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    char *type;
    switch (SCM_BSD_DB_TYPE(obj)) {
    case DB_BTREE: type = "btree"; break;
    case DB_HASH:  type = "hash"; break;
    case DB_RECNO: type = "recno"; break;
    default: type = "unknown"; break;
    }
    Scm_Printf(port, "#<bsd-db:%s %S>", type, SCM_BSD_DB(obj)->name);
}

static ScmObj bsddb_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmBsdDb *db = SCM_NEW(ScmBsdDb);
    SCM_SET_CLASS(db, SCM_CLASS_BSD_DB);
    db->name      = SCM_FALSE;
    db->db        = NULL;
    db->type      = DB_UNKNOWN;
    db->flags     = 0;
    db->cachesize = 0;
    db->maxkeypage = 0;
    db->minkeypage = 0;
    db->psize     = 0;
    db->lorder    = 0;
    db->bsize     = 0;
    db->ffactor   = 0;
    db->nelem     = 0;
    db->reclen    = 0;
    db->bval      = 0;
    return SCM_OBJ(db);
}

static ScmObj db_name_get(ScmBsdDb *db)
{
    return db->name;
}

static void   db_name_set(ScmBsdDb *db, ScmObj obj)
{
    if (!SCM_STRINGPP(obj)) {
        Scm_Error("string required, but got %S", obj);
    }
    db->name = obj;
}

static ScmObj db_type_get(ScmBsdDb *db)
{
    return SCM_MAKE_INT(db->type);
}

static void   db_type_set(ScmBsdDb *db, ScmObj obj)
{
    if (!SCM_INTP(obj)) {
        Scm_Error("small integer required, but got %S", obj);
    }
    switch (SCM_INT_VALUE(obj)) {
    case DB_BTREE:;
    case DB_HASH:;
    case DB_RECNO:;
    case DB_UNKNOWN: db->type = SCM_INT_VALUE(obj); break;
    default:
        Scm_Error("invalid BSD DB type: %S", obj);
    }
}

#define CAT2(a, b)    a##b
#define CAT3(a, b, c) a##b##c

#define INTEGER_SLOT(name)                                      \
  static ScmObj CAT3(db_, name, _get)(ScmBsdDb *db)             \
  {                                                             \
    return SCM_MAKE_INT(db->name);                              \
  }                                                             \
  static void   CAT3(db_, name, _set)(ScmBsdDb *db, ScmObj obj) \
  {                                                             \
    if (!SCM_INTP(obj)) {                                       \
        Scm_Error("small integer required, but got %S", obj);   \
    }                                                           \
    db->name = SCM_INT_VALUE(obj);                              \
  }

INTEGER_SLOT(flags)
INTEGER_SLOT(cachesize)
INTEGER_SLOT(maxkeypage)
INTEGER_SLOT(minkeypage)
INTEGER_SLOT(psize)
INTEGER_SLOT(lorder)
INTEGER_SLOT(bsize)
INTEGER_SLOT(ffactor)
INTEGER_SLOT(nelem)
INTEGER_SLOT(reclen)
INTEGER_SLOT(bval)

static ScmClassStaticSlotSpec db_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", db_name_get, db_name_set),
    SCM_CLASS_SLOT_SPEC("type", db_type_get, db_type_set),
    SCM_CLASS_SLOT_SPEC("flags", db_flags_get, db_flags_set),
    SCM_CLASS_SLOT_SPEC("cachesize", db_cachesize_get, db_cachesize_set),
    SCM_CLASS_SLOT_SPEC("maxkeypage", db_maxkeypage_get, db_maxkeypage_set),
    SCM_CLASS_SLOT_SPEC("minkeypage", db_minkeypage_get, db_minkeypage_set),
    SCM_CLASS_SLOT_SPEC("psize", db_psize_get, db_psize_set),
    SCM_CLASS_SLOT_SPEC("lorder", db_lorder_get, db_lorder_set),
    SCM_CLASS_SLOT_SPEC("bsize", db_bsize_get, db_bsize_set),
    SCM_CLASS_SLOT_SPEC("ffactor", db_ffactor_get, db_ffactor_set),
    SCM_CLASS_SLOT_SPEC("nelem", db_nelem_get, db_nelem_set),
    SCM_CLASS_SLOT_SPEC("reclen", db_reclen_get, db_reclen_set),
    SCM_CLASS_SLOT_SPEC("bval", db_bval_get, db_bval_set),
    SCM_CLASS_SLOT_SPEC_END()
};

void Scm_Init_bdbm(void)
{
    ScmModule *mod = SCM_FIND_MODULE("dbm.bdbm", SCM_FIND_MODULE_CREATE);
    Scm_InitStaticClass(&Scm_BsdDbClass, "<bsd-db>",
                        mod, db_slots, 0);
    Scm_Init_bsddb(mod);
}
