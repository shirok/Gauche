#! /bin/sh
#
#  Generate uvector.c
#

# prologue -----------------------------------------------------------
cat <<EOF
/*
 * uvector.c - Homogeneous vector
 *
 *  Copyright(C) 1999-2001 by Shiro Kawai (shiro@acm.org)
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
 *  \$Id: uvector.c.sh,v 1.10 2001-07-24 19:30:00 shirok Exp $
 */

#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <string.h>  /* for memcpy() */
#include <gauche.h>
#include "gauche/uvector.h"
#include "uvectorP.h"

#ifndef EPSILON
#define EPSILON  10e-5
#endif /*EPSILON*/
EOF

# template ------------------------------------------------------------
emit() {
    vecttag=$1
    vecttype=$2
    itemtype=$3
    extrafns=$4
    VECTTYPE=`echo $vecttype | tr '[a-z]' '[A-Z]'`
    cat <<EOF

/*---------------------------------------------------------------
 * ${vecttype}
 */

/*
 * Class stuff
 */

static void print_${vecttype}(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    int i;
    Scm_Printf(out, "#${vecttag}(");
    for (i=0; i<SCM_${VECTTYPE}_SIZE(obj); i++) {
        ${itemtype} elt = SCM_${VECTTYPE}_ELEMENTS(obj)[i];
        if (i != 0) Scm_Printf(out, " ");
        SCM_${VECTTYPE}_PRINT_ELT(out, elt);
    }
    Scm_Printf(out, ")");
}

static int compare_${vecttype}(ScmObj x, ScmObj y)
{
    int len = SCM_${VECTTYPE}_SIZE(x), i;
    ${itemtype} xx, yy;
    if (SCM_${VECTTYPE}_SIZE(y) != len) return -1;
    for (i=0; i<len; i++) {
        xx = SCM_${VECTTYPE}_ELEMENTS(x)[i];
        yy = SCM_${VECTTYPE}_ELEMENTS(y)[i];
        if (!SCM_${VECTTYPE}_EQUAL_ELT(xx, yy)) {
            return -1;
        }
    }
    return 0;
}

SCM_DEFINE_BUILTIN_CLASS(Scm_${vecttype}Class,
                         print_${vecttype}, compare_${vecttype}, NULL, NULL,
                         SCM_CLASS_SEQUENCE_CPL);

/*
 * Constructor
 */
static Scm${vecttype} *make_${vecttype}(int size)
{
    Scm${vecttype} *vec =
      SCM_NEW_ATOMIC2(Scm${vecttype} *,
                      sizeof(Scm${vecttype}) + (size-1)*sizeof(${itemtype}));
    SCM_SET_CLASS(vec, SCM_CLASS_${VECTTYPE});
    vec->size = size;
    return vec;
}

ScmObj Scm_Make${vecttype}(int size, ${itemtype} fill)
{
    Scm${vecttype} *vec = make_${vecttype}(size);
    int i;
    for (i=0; i<size; i++) {
        vec->elements[i] = fill;
    }
    return SCM_OBJ(vec);
}

ScmObj Scm_Make${vecttype}FromArray(int size, ${itemtype} array[])
{
    Scm${vecttype} *vec = make_${vecttype}(size);
    int i;
    for (i=0; i<size; i++) {
        vec->elements[i] = array[i];
    }
    return SCM_OBJ(vec);
}

ScmObj Scm_ListTo${vecttype}(ScmObj list)
{
    int length = Scm_Length(list), i;
    Scm${vecttype} *vec;
    ScmObj cp;

    if (length < 0) Scm_Error("improper list not allowed: %S", list);
    vec = make_${vecttype}(length);
    for (i=0, cp=list; i<length; i++, cp = SCM_CDR(cp)) {
        ${itemtype} elt;
        SCM_${VECTTYPE}_UNBOX(elt, SCM_CAR(cp));
        vec->elements[i] = elt;
    }
    return SCM_OBJ(vec);
}

ScmObj Scm_VectorTo${vecttype}(ScmVector *ivec)
{
    int length = SCM_VECTOR_SIZE(ivec), i;
    Scm${vecttype} *vec = make_${vecttype}(length);
    ScmObj cp;
    for (i=0; i<length; i++) {
        ${itemtype} elt;
        SCM_${VECTTYPE}_UNBOX(elt, SCM_VECTOR_ELEMENT(ivec, i));
        vec->elements[i] = elt;
    }
    return SCM_OBJ(vec);
}

/*
 * Accessors and modifiers
 */

ScmObj Scm_${vecttype}Fill(Scm${vecttype} *vec, ${itemtype} fill)
{
    int i;
    for (i=0; i<SCM_${VECTTYPE}_SIZE(vec); i++) {
        vec->elements[i] = fill;
    }
    return SCM_OBJ(vec);
}

ScmObj Scm_${vecttype}Ref(Scm${vecttype} *vec, int index)
{
    ScmObj r;
    if (index < 0 || index >= SCM_${VECTTYPE}_SIZE(vec))
        Scm_Error("index out of range: %d", index);
    SCM_${VECTTYPE}_BOX(r, vec->elements[index]);
    return r;
}

ScmObj Scm_${vecttype}Set(Scm${vecttype} *vec, int index, ScmObj val)
{
    ${itemtype} elt;
    if (index < 0 || index >= SCM_${VECTTYPE}_SIZE(vec))
        Scm_Error("index out of range: %d", index);
    SCM_${VECTTYPE}_UNBOX(elt, val);
    vec->elements[index] = elt;
    return SCM_OBJ(vec);
}

ScmObj Scm_${vecttype}ToList(Scm${vecttype} *vec) 
{
    ScmObj head = SCM_NIL, tail;
    int i;
    for (i=0; i<SCM_${VECTTYPE}_SIZE(vec); i++) {
        ScmObj elt;
        SCM_${VECTTYPE}_BOX(elt, vec->elements[i]);
        SCM_APPEND1(head, tail, elt);
    }
    return head;
}

ScmObj Scm_${vecttype}ToVector(Scm${vecttype} *vec)
{
    ScmObj ovec = Scm_MakeVector(SCM_${VECTTYPE}_SIZE(vec), SCM_UNDEFINED);
    int i;
    for (i=0; i<SCM_${VECTTYPE}_SIZE(vec); i++) {
        ScmObj elt;
        SCM_${VECTTYPE}_BOX(elt, vec->elements[i]);
        SCM_VECTOR_ELEMENT(ovec, i) = elt;
    }
    return ovec;
}

ScmObj Scm_${vecttype}Copy(Scm${vecttype} *vec)
{
    return Scm_Make${vecttype}FromArray(SCM_${VECTTYPE}_SIZE(vec),
                                        SCM_${VECTTYPE}_ELEMENTS(vec));
}

ScmObj Scm_${vecttype}CopyX(Scm${vecttype} *dst, Scm${vecttype} *src)
{
    int len = SCM_${VECTTYPE}_SIZE(src);
    if (SCM_${VECTTYPE}_SIZE(dst) != len) {
        Scm_Error("same size of vectors are required: %S, %S", dst, src);
    }
    memcpy(SCM_${VECTTYPE}_ELEMENTS(dst), SCM_${VECTTYPE}_ELEMENTS(src),
           len * sizeof(${itemtype}));
    return SCM_OBJ(dst);
}
EOF
}  # end of emit

emit s8 S8Vector "signed char"
emit u8 U8Vector "unsigned char"
emit s16 S16Vector "short"
emit u16 U16Vector "unsigned short"
emit s32 S32Vector "SCM_UVECTOR_INT32"
emit u32 U32Vector "SCM_UVECTOR_UINT32"
emit s64 S64Vector "SCM_UVECTOR_INT64"
emit u64 U64Vector "SCM_UVECTOR_UINT64"
emit f32 F32Vector "float"
emit f64 F64Vector "double"

# epilogue -----------------------------------------------------------

cat <<EOF

/*
 * Reader extension
 */
static ScmObj read_uvector(ScmPort *port, const char *tag)
{
    ScmChar c;
    ScmObj list;

    SCM_GETC(c, port);
    if (c != '(') Scm_Error("bad uniform vector syntax for %s", tag);
    list = Scm_ReadList(SCM_OBJ(port), ')');
    if (strcmp(tag, "s8") == 0)  return Scm_ListToS8Vector(list);
    if (strcmp(tag, "u8") == 0)  return Scm_ListToU8Vector(list);
    if (strcmp(tag, "s16") == 0) return Scm_ListToS16Vector(list);
    if (strcmp(tag, "u16") == 0) return Scm_ListToU16Vector(list);
    if (strcmp(tag, "s32") == 0) return Scm_ListToS32Vector(list);
    if (strcmp(tag, "u32") == 0) return Scm_ListToU32Vector(list);
    if (strcmp(tag, "s64") == 0) return Scm_ListToS64Vector(list);
    if (strcmp(tag, "u64") == 0) return Scm_ListToU64Vector(list);
    if (strcmp(tag, "f32") == 0) return Scm_ListToF32Vector(list);
    if (strcmp(tag, "f64") == 0) return Scm_ListToF64Vector(list);
    Scm_Error("invalid unform vector tag: %s", tag);
    return SCM_UNDEFINED; /* dummy */
}

/*
 * Initialization
 */
extern void Scm_Init_uvlib(ScmModule *);
extern ScmObj (*Scm_ReadUvectorHook)(ScmPort *port, const char *tag);
 
void Scm_Init_libuvector(void)
{
    ScmModule *m = Scm_SchemeModule();
    Scm_InitBuiltinClass(&Scm_S8VectorClass,  "<s8vector>",  NULL, m);
    Scm_InitBuiltinClass(&Scm_U8VectorClass,  "<u8vector>",  NULL, m);
    Scm_InitBuiltinClass(&Scm_S16VectorClass, "<s16vector>", NULL, m);
    Scm_InitBuiltinClass(&Scm_U16VectorClass, "<u16vector>", NULL, m);
    Scm_InitBuiltinClass(&Scm_S32VectorClass, "<s32vector>", NULL, m);
    Scm_InitBuiltinClass(&Scm_U32VectorClass, "<u32vector>", NULL, m);
    Scm_InitBuiltinClass(&Scm_F32VectorClass, "<f32vector>", NULL, m);
    Scm_InitBuiltinClass(&Scm_F64VectorClass, "<f64vector>", NULL, m);
    Scm_Init_uvlib(m);
    Scm_ReadUvectorHook = read_uvector;
}
EOF
