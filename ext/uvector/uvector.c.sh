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
 *  \$Id: uvector.c.sh,v 1.3 2001-02-19 14:48:49 shiro Exp $
 */

#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <string.h>  /* for memcpy() */
#include <gauche.h>
#include "uvector.h"

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

static int print_${vecttype}(ScmObj obj, ScmPort *out, int mode)
{
    int nc, i;
    nc = Scm_Printf(out, "#${vecttag}(");
    for (i=0; i<SCM_${VECTTYPE}_LENGTH(obj); i++) {
        ${itemtype} elt = SCM_${VECTTYPE}_DATA(obj)[i];
        if (i != 0) nc += Scm_Printf(out, ", ");
        PRINT_ELT(out, elt, nc);
    }
    nc += Scm_Printf(out, ")");
    return nc;
}

SCM_DEFCLASS(Scm_${vecttype}Class, "<${vecttag}vector>",
             print_${vecttype}, SCM_CLASS_SEQUENCE_CPL);

/*
 * Constructor
 */
static Scm${vecttype} *make_${vecttype}(int length)
{
    Scm${vecttype} *vec =
      SCM_NEW_ATOMIC2(Scm${vecttype} *,
                      sizeof(Scm${vecttype}) + (length-1)*sizeof(${itemtype}));
    SCM_SET_CLASS(vec, SCM_CLASS_${VECTTYPE});
    vec->length = length;
    return vec;
}

ScmObj Scm_Make${vecttype}(int length, ${itemtype} fill)
{
    Scm${vecttype} *vec = make_${vecttype}(length);
    int i;
    for (i=0; i<length; i++) {
        vec->data[i] = fill;
    }
    return SCM_OBJ(vec);
}

ScmObj Scm_Make${vecttype}FromArray(int length, ${itemtype} array[])
{
    Scm${vecttype} *vec = make_${vecttype}(length);
    int i;
    for (i=0; i<length; i++) {
        vec->data[i] = array[i];
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
        UNBOX(elt, SCM_CAR(cp));
        vec->data[i] = elt;
    }
    return SCM_OBJ(vec);
}

ScmObj Scm_VectorTo${vecttype}(ScmVector *ivec)
{
    int length = SCM_VECTOR_LENGTH(ivec), i;
    Scm${vecttype} *vec = make_${vecttype}(length);
    ScmObj cp;
    for (i=0; i<length; i++) {
        ${itemtype} elt;
        UNBOX(elt, SCM_VECTOR_ELEMENT(ivec, i));
        vec->data[i] = elt;
    }
    return SCM_OBJ(vec);
}

/*
 * Accessors and modifiers
 */

ScmObj Scm_${vecttype}Fill(Scm${vecttype} *vec, ${itemtype} fill)
{
    int i;
    for (i=0; i<SCM_${VECTTYPE}_LENGTH(vec); i++) {
        vec->data[i] = fill;
    }
    return SCM_OBJ(vec);
}

ScmObj Scm_${vecttype}Ref(Scm${vecttype} *vec, int index)
{
    ScmObj r;
    if (index < 0 || index >= SCM_${VECTTYPE}_LENGTH(vec))
        Scm_Error("index out of range: %d", index);
    BOX(r, vec->data[index]);
    return r;
}

ScmObj Scm_${vecttype}Set(Scm${vecttype} *vec, int index, ScmObj val)
{
    ${itemtype} elt;
    if (index < 0 || index >= SCM_${VECTTYPE}_LENGTH(vec))
        Scm_Error("index out of range: %d", index);
    UNBOX(elt, val);
    vec->data[index] = elt;
    return SCM_OBJ(vec);
}

ScmObj Scm_${vecttype}ToList(Scm${vecttype} *vec) 
{
    ScmObj head = SCM_NIL, tail;
    int i;
    for (i=0; i<SCM_${VECTTYPE}_LENGTH(vec); i++) {
        ScmObj elt;
        BOX(elt, vec->data[i]);
        SCM_APPEND1(head, tail, elt);
    }
    return head;
}

ScmObj Scm_${vecttype}ToVector(Scm${vecttype} *vec)
{
    ScmObj ovec = Scm_MakeVector(SCM_${VECTTYPE}_LENGTH(vec), SCM_UNDEFINED);
    int i;
    for (i=0; i<SCM_${VECTTYPE}_LENGTH(vec); i++) {
        ScmObj elt;
        BOX(elt, vec->data[i]);
        SCM_VECTOR_ELEMENT(ovec, i) = elt;
    }
    return ovec;
}

ScmObj Scm_${vecttype}Copy(Scm${vecttype} *vec)
{
    return Scm_Make${vecttype}FromArray(SCM_${VECTTYPE}_LENGTH(vec),
                                        SCM_${VECTTYPE}_DATA(vec));
}

ScmObj Scm_${vecttype}CopyX(Scm${vecttype} *dst, Scm${vecttype} *src)
{
    int len = SCM_${VECTTYPE}_LENGTH(src);
    if (SCM_${VECTTYPE}_LENGTH(dst) != len) {
        Scm_Error("same length of vectors are required: %S, %S", dst, src);
    }
    memcpy(SCM_${VECTTYPE}_DATA(dst), SCM_${VECTTYPE}_DATA(src),
           len * sizeof(${itemtype}));
    return SCM_OBJ(dst);
}
EOF
}  # end of emit

# s8vector -----------------------------------------------------------

cat <<EOF
#define BOX(obj, elt)    obj = SCM_MAKE_INT(elt)
#define UNBOX(elt, obj)                                                    \
    do {                                                                   \
        int v;                                                             \
        if (!SCM_INT_P(obj)) Scm_Error("argument out of domain: %S", obj); \
        v = SCM_INT_VALUE(obj);                                            \
        if (v < -128 || v > 127)                                           \
            Scm_Error("argument out of bound: %d", v);                     \
        elt = (signed char)v;                                              \
    } while (0)
#define PRINT_ELT(out, elt, nc) \
    nc += Scm_Printf(out, "%d", elt)
EOF
emit s8 S8Vector "signed char"
cat <<EOF
#undef BOX
#undef UNBOX
#undef PRINT_ELT
EOF

# u8vector -----------------------------------------------------------

cat <<EOF
#define BOX(obj, elt)    obj = SCM_MAKE_INT(elt)
#define UNBOX(elt, obj)                                                    \
    do {                                                                   \
        int v;                                                             \
        if (!SCM_INT_P(obj)) Scm_Error("argument out of domain: %S", obj); \
        v = SCM_INT_VALUE(obj);                                            \
        if (v < 0 || v > 255)                                              \
            Scm_Error("argument out of bound: %d", v);                     \
        elt = (unsigned char)v;                                            \
    } while (0)
#define PRINT_ELT(out, elt, nc) \
    nc += Scm_Printf(out, "%ud", elt)
EOF
emit u8 U8Vector "unsigned char"
cat <<EOF
#undef BOX
#undef UNBOX
#undef PRINT_ELT
EOF

# s16vector ----------------------------------------------------------

cat <<EOF
#define BOX(obj, elt)    obj = SCM_MAKE_INT(elt)
#define UNBOX(elt, obj)                                                    \
    do {                                                                   \
        int v;                                                             \
        if (!SCM_INT_P(obj)) Scm_Error("argument out of domain: %S", obj); \
        v = SCM_INT_VALUE(obj);                                            \
        if (v < -32768 || v > 32767)                                       \
            Scm_Error("argument out of bound: %d", v);                     \
        elt = (short)v;                                                    \
    } while (0)
#define PRINT_ELT(out, elt, nc) \
    nc += Scm_Printf(out, "%d", elt)
EOF
emit s16 S16Vector "short"
cat <<EOF
#undef BOX
#undef UNBOX
#undef PRINT_ELT
EOF

# u16vector ----------------------------------------------------------

cat <<EOF
#define BOX(obj, elt)    obj = SCM_MAKE_INT(elt)
#define UNBOX(elt, obj)                                                    \
    do {                                                                   \
        int v;                                                             \
        if (!SCM_INT_P(obj)) Scm_Error("argument out of domain: %S", obj); \
        v = SCM_INT_VALUE(obj);                                            \
        if (v < 0 || v > 65536)                                            \
            Scm_Error("argument out of bound: %d", v);                     \
        elt = (unsigned short)v;                                           \
    } while (0)
#define PRINT_ELT(out, elt, nc) \
    nc += Scm_Printf(out, "%ud", elt)
EOF
emit u16 U16Vector "unsigned short"
cat <<EOF
#undef BOX
#undef UNBOX
#undef PRINT_ELT
EOF



# epilogue -----------------------------------------------------------

cat <<EOF

/*
 * Initialization
 */
void Scm_DLInit(void)
{
    ScmModule *m = Scm_SchemeModule();
    SCM_DEFINE(m, "<s8vector>",  SCM_OBJ(&Scm_S8VectorClass));
    SCM_DEFINE(m, "<u8vector>",  SCM_OBJ(&Scm_U8VectorClass));
    SCM_DEFINE(m, "<s16vector>", SCM_OBJ(&Scm_S16VectorClass));
    SCM_DEFINE(m, "<u16vector>", SCM_OBJ(&Scm_U16VectorClass));
}
EOF


