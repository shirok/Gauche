#! /bin/sh
#
#  Generate uvector.c
#

# prologue -----------------------------------------------------------
cat <<EOF
/*
 * uvector.c - Homogeneous vector
 *
 *  Copyright(C) 1999-2002 by Shiro Kawai (shiro@acm.org)
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
 *  \$Id: uvector.c.sh,v 1.17 2002-06-19 05:52:08 shirok Exp $
 */

#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <string.h>  /* for memcpy() */
#include <gauche.h>
#include <gauche/extend.h>
#include "gauche/uvector.h"
#include "uvectorP.h"

static ScmClass *sequence_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_SequenceClass),
    SCM_CLASS_STATIC_PTR(Scm_CollectionClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

/* Useful constants.  Module initialization routine sets the actual value.
   Initialization to SCM_NIL is necessary to place these vars in data area
   instead of bss area. */
ScmObj Scm_UvectorS32Max = SCM_NIL;
ScmObj Scm_UvectorS32Min = SCM_NIL;
ScmObj Scm_UvectorU32Max = SCM_NIL;
ScmObj Scm_UvectorU32Min = SCM_NIL;
ScmObj Scm_UvectorS64Max = SCM_NIL;
ScmObj Scm_UvectorS64Min = SCM_NIL;
ScmObj Scm_UvectorU64Max = SCM_NIL;
ScmObj Scm_UvectorU64Min = SCM_NIL;

#define CHECK_START_END(start, end, size)                   \\
  do {                                                      \\
    if (start < 0 || start >= size) {                       \\
        Scm_Error("start index out of range: %d\n", start); \\
    }                                                       \\
    if (end < 0) end = size;                                \\
    else if (end < start || end >= size) {                  \\
        Scm_Error("end index out of range: %d\n", end);     \\
    }                                                       \\
  } while (0)

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
                         sequence_cpl);

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

ScmObj Scm_VectorTo${vecttype}(ScmVector *ivec, int start, int end)
{
    int length = SCM_VECTOR_SIZE(ivec), i;
    Scm${vecttype} *vec;
    ScmObj cp;
    CHECK_START_END(start, end, length);
    vec = make_${vecttype}(end-start);
    for (i=start; i<end; i++) {
        ${itemtype} elt;
        SCM_${VECTTYPE}_UNBOX(elt, SCM_VECTOR_ELEMENT(ivec, i));
        vec->elements[i-start] = elt;
    }
    return SCM_OBJ(vec);
}

/*
 * Accessors and modifiers
 */

ScmObj Scm_${vecttype}Fill(Scm${vecttype} *vec, ${itemtype} fill, int start, int end)
{
    int i, size = SCM_${VECTTYPE}_SIZE(vec);
    CHECK_START_END(start, end, size);
    for (i=start; i<end; i++) vec->elements[i] = fill;
    return SCM_OBJ(vec);
}

ScmObj Scm_${vecttype}Ref(Scm${vecttype} *vec, int index, ScmObj fallback)
{
    ScmObj r;
    if (index < 0 || index >= SCM_${VECTTYPE}_SIZE(vec)) {
        if (SCM_UNBOUNDP(fallback)) 
            Scm_Error("index out of range: %d", index);
        return fallback;
    }
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

ScmObj Scm_${vecttype}ToList(Scm${vecttype} *vec, int start, int end)
{
    ScmObj head = SCM_NIL, tail;
    int i, size = SCM_${VECTTYPE}_SIZE(vec);
    CHECK_START_END(start, end, size);
    for (i=start; i<end; i++) {
        ScmObj elt;
        SCM_${VECTTYPE}_BOX(elt, vec->elements[i]);
        SCM_APPEND1(head, tail, elt);
    }
    return head;
}

ScmObj Scm_${vecttype}ToVector(Scm${vecttype} *vec, int start, int end)
{
    ScmObj ovec;
    int i, size = SCM_${VECTTYPE}_SIZE(vec);
    CHECK_START_END(start, end, size);
    ovec = Scm_MakeVector(end-start, SCM_UNDEFINED);
    for (i=start; i<end; i++) {
        ScmObj elt;
        SCM_${VECTTYPE}_BOX(elt, vec->elements[i]);
        SCM_VECTOR_ELEMENT(ovec, i-start) = elt;
    }
    return ovec;
}

ScmObj Scm_${vecttype}Copy(Scm${vecttype} *vec, int start, int end)
{
    int size = SCM_${VECTTYPE}_SIZE(vec);
    CHECK_START_END(start, end, size);
    return Scm_Make${vecttype}FromArray(end-start,
                                        SCM_${VECTTYPE}_ELEMENTS(vec)+start);
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
SCM_EXTERN ScmObj (*Scm_ReadUvectorHook)(ScmPort *port, const char *tag);
 
void Scm_Init_libuvector(void)
{
    ScmModule *m;
    ScmObj t;

    SCM_INIT_EXTENSION(uvector);
    m = SCM_MODULE(SCM_FIND_MODULE("srfi-4", TRUE));
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
EOF
