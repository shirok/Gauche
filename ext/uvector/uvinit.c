/*
 * uvinit.c - initialize routine for uvector extension
 *
 *  Copyright(C) 2003 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: uvinit.c,v 1.3 2003-05-30 12:12:11 shirok Exp $
 */

#include <gauche.h>
#include <gauche/extend.h>
#include "gauche/uvector.h"
#include "uvectorP.h"

/*
 * Reader extension
 */
static ScmObj read_uvector(ScmPort *port, const char *tag,
                           ScmReadContext *ctx)
{
    ScmChar c;
    ScmObj list, uv = SCM_UNDEFINED;

    SCM_GETC(c, port);
    if (c != '(') Scm_Error("bad uniform vector syntax for %s", tag);
    list = Scm_ReadList(SCM_OBJ(port), ')');
    if (strcmp(tag, "s8") == 0)  uv = Scm_ListToS8Vector(list, 0);
    else if (strcmp(tag, "u8") == 0)  uv = Scm_ListToU8Vector(list, 0);
    else if (strcmp(tag, "s16") == 0) uv = Scm_ListToS16Vector(list, 0);
    else if (strcmp(tag, "u16") == 0) uv = Scm_ListToU16Vector(list, 0);
    else if (strcmp(tag, "s32") == 0) uv = Scm_ListToS32Vector(list, 0);
    else if (strcmp(tag, "u32") == 0) uv = Scm_ListToU32Vector(list, 0);
    else if (strcmp(tag, "s64") == 0) uv = Scm_ListToS64Vector(list, 0);
    else if (strcmp(tag, "u64") == 0) uv = Scm_ListToU64Vector(list, 0);
    else if (strcmp(tag, "f32") == 0) uv = Scm_ListToF32Vector(list, 0);
    else if (strcmp(tag, "f64") == 0) uv = Scm_ListToF64Vector(list, 0);
    else Scm_Error("invalid unform vector tag: %s", tag);
    /* If we are reading source file, let literal uvectors be immutable. */
    if (ctx->flags & SCM_READ_LITERAL_IMMUTABLE) {
        SCM_UVECTOR_IMMUTABLE_P(uv) = TRUE;
    }
    return uv;
}

/*
 * Initialization
 */
extern void Scm_Init_uvlib(ScmModule *);
SCM_EXTERN ScmObj (*Scm_ReadUvectorHook)(ScmPort *port, const char *tag,
                                         ScmReadContext *ctx);
 
void Scm_Init_libgauche_uvector(void)
{
    ScmModule *m;
    ScmObj t;

    SCM_INIT_EXTENSION(uvector);
    m = SCM_MODULE(SCM_FIND_MODULE("gauche.uvector", TRUE));
    Scm_InitBuiltinClass(&Scm_UVectorClass,   "<uvector>",  NULL, 0, m);
    Scm_InitBuiltinClass(&Scm_S8VectorClass,  "<s8vector>",  NULL, 0, m);
    Scm_InitBuiltinClass(&Scm_U8VectorClass,  "<u8vector>",  NULL, 0, m);
    Scm_InitBuiltinClass(&Scm_S16VectorClass, "<s16vector>", NULL, 0, m);
    Scm_InitBuiltinClass(&Scm_U16VectorClass, "<u16vector>", NULL, 0, m);
    Scm_InitBuiltinClass(&Scm_S32VectorClass, "<s32vector>", NULL, 0, m);
    Scm_InitBuiltinClass(&Scm_U32VectorClass, "<u32vector>", NULL, 0, m);
    Scm_InitBuiltinClass(&Scm_S64VectorClass, "<s64vector>", NULL, 0, m);
    Scm_InitBuiltinClass(&Scm_U64VectorClass, "<u64vector>", NULL, 0, m);
    Scm_InitBuiltinClass(&Scm_F32VectorClass, "<f32vector>", NULL, 0, m);
    Scm_InitBuiltinClass(&Scm_F64VectorClass, "<f64vector>", NULL, 0, m);

    /* initialize constant values */
    t = Scm_Ash(SCM_MAKE_INT(1), 31);  /* 2^31 */
    Scm_UvectorS32Max = Scm_Subtract2(t, SCM_MAKE_INT(1));
    Scm_UvectorS32Min = Scm_Negate(t);
    t = Scm_Ash(SCM_MAKE_INT(1), 32);  /* 2^32 */
    Scm_UvectorU32Max = Scm_Subtract2(t, SCM_MAKE_INT(1));
    Scm_UvectorU32Min = SCM_MAKE_INT(0);
    t = Scm_Ash(SCM_MAKE_INT(1), 63);  /* 2^63 */
    Scm_UvectorS64Max = Scm_Subtract2(t, SCM_MAKE_INT(1));
    Scm_UvectorS64Min = Scm_Negate(t);
    t = Scm_Ash(SCM_MAKE_INT(1), 64);  /* 2^64 */
    Scm_UvectorU64Max = Scm_Subtract2(t, SCM_MAKE_INT(1));
    Scm_UvectorU64Min = SCM_MAKE_INT(0);

    Scm_Init_uvlib(m);
    Scm_ReadUvectorHook = read_uvector;
}
