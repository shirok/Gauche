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
 *  $Id: bsddb.c,v 1.1 2001-11-17 11:16:28 shirok Exp $
 */

#include "bsddb.h"

/*========================================================
 * <bsd-db> class
 */

static void bsddb_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static ScmObj bsddb_allocate(ScmClass *, ScmObj);

ScmClass *Scm_BsdDbCPL[] = { SCM_CLASS_BSD_DB, SCM_CLASS_TOP, NULL };

SCM_DEFINE_BUILTIN_CLASS(Scm_BsdDbClass, bsddb_print,
                         NULL, NULL, bsddb_allocate,
                         SCM_CLASS_DEFAULT_CPL);

void bsddb_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
{
    char *type;
    switch (SCM_BSD_DB_TYPE(obj)) {
    case DB_BTREE: type = "btree"; break;
    case DB_HASH:  type = "hash"; break;
    case DB_RECNO: type = "recno"; break;
    default: type = "unknown"; break;
    }
    Scm_Printf(port, "#<bsd-%s \"%s\">", type, SCM_BSD_DB(obj)->file);
}

ScmObj bsddb_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmBsdDb *d = SCM_NEW(ScmBsdDb);
    SCM_SET_CLASS(d, SCM_CLASS_BSD_DB);
    d->file = NULL;
    d->flags = d->mode = 0;
    d->type = DB_UNKNOWN;
    d->db = NULL;
}

/* Accessors */

static ScmObj bsddb_file_get(ScmBsdDb *db)
{
    if (db->file) return SCM_MAKE_STR_IMMUTABLE(db->file);
    else return SCM_FALSE;
}

static void bsddb_file_set(ScmBsdDb *db, ScmObj val)
{
    if (!SCM_STRINGP(val)) Scm_Error("string required, but got %S", val);
    db->file = Scm_GetStringConst(SCM_STRING(val));
}

static ScmObj bsddb_flags_get(ScmBsdDb *db)
{
    return Scm_MakeIntegerFromUI(db->flags);
}

static void bsddb_flags_set(ScmBsdDb *db, ScmObj val)
{
    if (!SCM_EXACTP(val)) Scm_Error("exact integer required, but got %S", val);
    db->flags = Scm_GetUInteger(val);
}

static ScmObj bsddb_mode_get(ScmBsdDb *db)
{
    return Scm_MakeIntegerFromUI(db->mode);
}

static void bsddb_mode_set(ScmBsdDb *db, ScmObj val)
{
    if (!SCM_EXACTP(val)) Scm_Error("exact integer required, but got %S", val);
    db->mode = Scm_GetUInteger(val);
}

static ScmObj bsddb_type_get(ScmBsdDb *db)
{
    return Scm_MakeInteger(db->type);
}

static void bsddb_type_set(ScmBsdDb *db, ScmObj val)
{
    int v;
    if (!SCM_INTP(val)) Scm_Error("exact integer required, but got %S", val);
    v = SCM_INT_VALUE(val);
    if (v < DB_BTREE || v > DB_UNKNOWN)
        Scm_Error("value out of range: %d", v);
    db->type = v;
}

static ScmClassStaticSlotSpec bsddb_slots[] = {
    SCM_CLASS_SLOT_SPEC("file",  bsddb_file_get, bsddb_file_set);
    SCM_CLASS_SLOT_SPEC("flags", bsddb_flags_get, bsddb_flags_set);
    SCM_CLASS_SLOT_SPEC("mode",  bsddb_mode_get, bsddb_mode_set);
    SCM_CLASS_SLOT_SPEC("type",  bsddb_type_get, bsddb_type_set);
    { NULL }
};

/* Open */


/* Initialization */
ScmObj Scm_Init_bsddb(void)
{
    ScmModule *mod = SCM_MODULE(SCM_FIND_MODULE("dbm.bdbm", TRUE));
    Scm_InitBuiltinClass(&Scm_BsdDbClass, "<bsd-db>", bsddb_slots,
                         sizeof(ScmBsdDb), mod);
}


