#! /bin/sh
#
#  Generate uvector.c
#

# prologue -----------------------------------------------------------
cat <<EOF
/*
 * uvector.c - Homogeneous vector
 *
 *   Copyright (c) 1999-2003 Shiro Kawai, All rights reserved.
 * 
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 * 
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  \$Id: uvector.c.sh,v 1.33 2003-11-27 17:10:40 shirok Exp $
 */

#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <string.h>  /* for memcpy() */
#include <gauche.h>
#include <gauche/extend.h>
#include "gauche/uvector.h"
#include "uvectorP.h"

static ScmClass *uvector_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_UVectorClass),
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

SCM_DEFINE_BUILTIN_CLASS(Scm_UVectorClass, NULL, NULL, NULL, NULL,
                         uvector_cpl+1);

/* Returns the size of element of the uvector of given class */
int Scm_UVectorElementSize(ScmClass *klass)
{
    if (SCM_EQ(klass, SCM_CLASS_S8VECTOR)) return sizeof(S8ELTTYPE[1]);
    if (SCM_EQ(klass, SCM_CLASS_U8VECTOR)) return sizeof(U8ELTTYPE[1]);
    if (SCM_EQ(klass, SCM_CLASS_S16VECTOR)) return sizeof(S16ELTTYPE[1]);
    if (SCM_EQ(klass, SCM_CLASS_U16VECTOR)) return sizeof(U16ELTTYPE[1]);
    if (SCM_EQ(klass, SCM_CLASS_S32VECTOR)) return sizeof(S32ELTTYPE[1]);
    if (SCM_EQ(klass, SCM_CLASS_U32VECTOR)) return sizeof(U32ELTTYPE[1]);
    if (SCM_EQ(klass, SCM_CLASS_F32VECTOR)) return sizeof(F32ELTTYPE[1]);
    if (SCM_EQ(klass, SCM_CLASS_S64VECTOR)) return sizeof(S64ELTTYPE[1]);
    if (SCM_EQ(klass, SCM_CLASS_U64VECTOR)) return sizeof(U64ELTTYPE[1]);
    if (SCM_EQ(klass, SCM_CLASS_F64VECTOR)) return sizeof(F64ELTTYPE[1]);
    return -1;
}

/* Generic constructor */
ScmObj Scm_MakeUVectorFull(ScmClass *klass, int size, void *init, int immutable, void *owner)
{
    ScmUVector *vec;
    int eltsize = Scm_UVectorElementSize(klass);
    SCM_ASSERT(eltsize >= 1);
    vec = SCM_NEW(ScmUVector);
    SCM_SET_CLASS(vec, klass);
    if (init) {
        vec->elements = init;   /* trust the caller */
    } else {
        vec->elements = SCM_NEW_ATOMIC2(void*, size*eltsize);
    }
    vec->size = size;
    vec->immutable = immutable;
    vec->owner = owner;
    return SCM_OBJ(vec);
}

ScmObj Scm_MakeUVector(ScmClass *klass, int size, void *init)
{
    return Scm_MakeUVectorFull(klass, size, init, FALSE, NULL);
}
EOF

# template ------------------------------------------------------------
emit() {
    vecttag=$1
    VECTTAG=`echo $vecttag | tr '[a-z]' '[A-Z]'`
    vecttype="${VECTTAG}Vector"
    VECTTYPE="${VECTTAG}VECTOR"
    itemtype="${VECTTAG}ELTTYPE"
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
        ${VECTTAG}ELTPRINT(out, elt);
    }
    Scm_Printf(out, ")");
}

static int compare_${vecttype}(ScmObj x, ScmObj y, int equalp)
{
    int len = SCM_${VECTTYPE}_SIZE(x), i;
    ${itemtype} xx, yy;
    if (SCM_${VECTTYPE}_SIZE(y) != len) return -1;
    for (i=0; i<len; i++) {
        xx = SCM_${VECTTYPE}_ELEMENTS(x)[i];
        yy = SCM_${VECTTYPE}_ELEMENTS(y)[i];
        if (!${VECTTAG}ELTEQ(xx, yy)) {
            return -1;
        }
    }
    return 0;
}

SCM_DEFINE_BUILTIN_CLASS(Scm_${vecttype}Class,
                         print_${vecttype}, compare_${vecttype}, NULL, NULL,
                         uvector_cpl);

/*
 * Constructor
 */
static Scm${vecttype} *make_${vecttype}(int size, ${itemtype} *eltp)
{
    return (Scm${vecttype}*)Scm_MakeUVector(SCM_CLASS_${VECTTYPE}, size, eltp);
}

ScmObj Scm_Make${vecttype}(int size, ${itemtype} fill)
{
    Scm${vecttype} *vec = make_${vecttype}(size, NULL);
    int i;
    for (i=0; i<size; i++) {
        vec->elements[i] = fill;
    }
    return SCM_OBJ(vec);
}

ScmObj Scm_Make${vecttype}FromArray(int size, const ${itemtype} array[])
{
    Scm${vecttype} *vec = make_${vecttype}(size, NULL);
    int i;
    for (i=0; i<size; i++) {
        vec->elements[i] = array[i];
    }
    return SCM_OBJ(vec);
}

ScmObj Scm_Make${vecttype}FromArrayShared(int size, ${itemtype} array[])
{
    Scm${vecttype} *vec = make_${vecttype}(size, array);
    return SCM_OBJ(vec);
}

ScmObj Scm_ListTo${vecttype}(ScmObj list, int clamp)
{
    int length = Scm_Length(list), i;
    Scm${vecttype} *vec;
    ScmObj cp;

    if (length < 0) Scm_Error("improper list not allowed: %S", list);
    vec = make_${vecttype}(length, NULL);
    for (i=0, cp=list; i<length; i++, cp = SCM_CDR(cp)) {
        ${itemtype} elt;
        ScmObj obj = SCM_CAR(cp);
        ${VECTTAG}UNBOX(elt, obj);
        vec->elements[i] = elt;
    }
    return SCM_OBJ(vec);
}

ScmObj Scm_VectorTo${vecttype}(ScmVector *ivec, int start, int end, int clamp)
{
    int length = SCM_VECTOR_SIZE(ivec), i;
    Scm${vecttype} *vec;
    SCM_CHECK_START_END(start, end, length);
    vec = make_${vecttype}(end-start, NULL);
    for (i=start; i<end; i++) {
        ${itemtype} elt;
        ScmObj obj = SCM_VECTOR_ELEMENT(ivec, i);
        ${VECTTAG}UNBOX(elt, obj);
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
    SCM_CHECK_START_END(start, end, size);
    SCM_UVECTOR_CHECK_MUTABLE(vec);
    for (i=start; i<end; i++) vec->elements[i] = fill;
    return SCM_OBJ(vec);
}

ScmObj Scm_${vecttype}Ref(Scm${vecttype} *vec, int index, ScmObj fallback)
{
    ScmObj r;
    ${itemtype} elt;
    if (index < 0 || index >= SCM_${VECTTYPE}_SIZE(vec)) {
        if (SCM_UNBOUNDP(fallback)) 
            Scm_Error("index out of range: %d", index);
        return fallback;
    }
    elt = vec->elements[index];
    ${VECTTAG}BOX(r, elt);
    return r;
}

ScmObj Scm_${vecttype}Set(Scm${vecttype} *vec, int index, ScmObj val, int clamp)
{
    ${itemtype} elt;
    if (index < 0 || index >= SCM_${VECTTYPE}_SIZE(vec))
        Scm_Error("index out of range: %d", index);
    SCM_UVECTOR_CHECK_MUTABLE(vec);
    ${VECTTAG}UNBOX(elt, val);
    vec->elements[index] = elt;
    return SCM_OBJ(vec);
}

ScmObj Scm_${vecttype}ToList(Scm${vecttype} *vec, int start, int end)
{
    ScmObj head = SCM_NIL, tail;
    int i, size = SCM_${VECTTYPE}_SIZE(vec);
    SCM_CHECK_START_END(start, end, size);
    for (i=start; i<end; i++) {
        ScmObj obj;
        ${itemtype} elt = vec->elements[i];
        ${VECTTAG}BOX(obj, elt);
        SCM_APPEND1(head, tail, obj);
    }
    return head;
}

ScmObj Scm_${vecttype}ToVector(Scm${vecttype} *vec, int start, int end)
{
    ScmObj ovec;
    int i, size = SCM_${VECTTYPE}_SIZE(vec);
    SCM_CHECK_START_END(start, end, size);
    ovec = Scm_MakeVector(end-start, SCM_UNDEFINED);
    for (i=start; i<end; i++) {
        ScmObj obj;
        ${itemtype} elt = vec->elements[i];
        ${VECTTAG}BOX(obj, elt);
        SCM_VECTOR_ELEMENT(ovec, i-start) = obj;
    }
    return ovec;
}

ScmObj Scm_${vecttype}Copy(Scm${vecttype} *vec, int start, int end)
{
    int size = SCM_${VECTTYPE}_SIZE(vec);
    SCM_CHECK_START_END(start, end, size);
    return Scm_Make${vecttype}FromArray(end-start,
                                        SCM_${VECTTYPE}_ELEMENTS(vec)+start);
}

ScmObj Scm_${vecttype}CopyX(Scm${vecttype} *dst, Scm${vecttype} *src)
{
    int len = SCM_${VECTTYPE}_SIZE(src);
    if (SCM_${VECTTYPE}_SIZE(dst) != len) {
        Scm_Error("same size of vectors are required: %S, %S", dst, src);
    }
    SCM_UVECTOR_CHECK_MUTABLE(dst);
    memcpy(SCM_${VECTTYPE}_ELEMENTS(dst), SCM_${VECTTYPE}_ELEMENTS(src),
           len * sizeof(${itemtype}));
    return SCM_OBJ(dst);
}
EOF
}  # end of emit

emit s8
emit u8
emit s16
emit u16
emit s32
emit u32
emit s64
emit u64
emit f32
emit f64

