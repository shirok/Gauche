/*
 * gauche/priv/typeP.h - Type-related stuff
 *
 *   Copyright (c) 2021-2025  Shiro Kawai  <shiro@acm.org>
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
 */

#ifndef GAUCHE_PRIV_TYPEP_H
#define GAUCHE_PRIV_TYPEP_H

/* Some internal type mechanisms, such as type constructors.
   We expose them only through Scheme public API.

   See src/libtype.scm for the detailed explanations of those types
   and their usage. */

extern ScmObj Scm__VMOfType(ScmObj obj, ScmObj type);

/* <type-constructor-meta> in Scheme.
   It inherits <class> in C level. */
typedef struct ScmTypeConstructorRec {
    ScmClass common;
    ScmObj   initializer;
    ScmObj   deconstructor;
    ScmObj   validator;
    ScmObj   subtypeP;
    ScmObj   supertypeP;
} ScmTypeConstructor;

/* <native-type> */
typedef struct ScmNativeTypeRec {
    SCM_INSTANCE_HEADER;
    ScmObj   name;
    ScmObj   super;
    int      (*c_of_type)(struct ScmNativeTypeRec*, ScmObj);
    ScmObj   (*c_ref)(struct ScmNativeTypeRec*, void*);
    void     (*c_set)(struct ScmNativeTypeRec*, void*, ScmObj);
    const char *c_type_name;
    const char *c_typecheck_name;
    const char *c_boxer_name;
    const char *c_unboxer_name;
    size_t   size;              /* if unbounded, minimum size. */
    size_t   alignment;
    _Bool    unsigned_p;        /* only relevant for integral times */
    _Bool    bounded_p;         /* #f if unbounded array or aggregates
                                   having unbounded array. */
} ScmNativeType;

SCM_CLASS_DECL(Scm_NativeTypeClass);
#define SCM_CLASS_NATIVE_TYPE    (&Scm_NativeTypeClass)
#define SCM_NATIVE_TYPE(obj)     ((ScmNativeType*)(obj))
#define SCM_NATIVE_TYPE_P(obj)   (SCM_ISA(obj, SCM_CLASS_NATIVE_TYPE))

SCM_EXTERN int Scm_NativeTypeEqualP(ScmNativeType *a, ScmNativeType *b);
SCM_EXTERN int Scm_NativeTypeIntegralP(ScmNativeType *np);
SCM_EXTERN int Scm_NativeTypeUnsignedP(ScmNativeType *np);

SCM_EXTERN ScmObj Scm__MakeNativeTypeVariant(const ScmNativeType *orig,
                                             const char *name,
                                             size_t alignment,
                                             ScmObj (*c_ref)(ScmNativeType*, void*),
                                             void (*c_set)(ScmNativeType*, void*, ScmObj));

/* Primitive native type singletons, accessed via functions (DLL-safe) */
SCM_EXTERN ScmObj Scm_NativeFixnumType(void);
SCM_EXTERN ScmObj Scm_NativeUfixnumType(void);
SCM_EXTERN ScmObj Scm_NativeShortType(void);
SCM_EXTERN ScmObj Scm_NativeUshortType(void);
SCM_EXTERN ScmObj Scm_NativeIntType(void);
SCM_EXTERN ScmObj Scm_NativeUintType(void);
SCM_EXTERN ScmObj Scm_NativeLongType(void);
SCM_EXTERN ScmObj Scm_NativeUlongType(void);
SCM_EXTERN ScmObj Scm_NativeInt8Type(void);
SCM_EXTERN ScmObj Scm_NativeUint8Type(void);
SCM_EXTERN ScmObj Scm_NativeInt16Type(void);
SCM_EXTERN ScmObj Scm_NativeUint16Type(void);
SCM_EXTERN ScmObj Scm_NativeInt32Type(void);
SCM_EXTERN ScmObj Scm_NativeUint32Type(void);
SCM_EXTERN ScmObj Scm_NativeInt64Type(void);
SCM_EXTERN ScmObj Scm_NativeUint64Type(void);
SCM_EXTERN ScmObj Scm_NativeSizetType(void);
SCM_EXTERN ScmObj Scm_NativeSsizetType(void);
SCM_EXTERN ScmObj Scm_NativePtrdifftType(void);
SCM_EXTERN ScmObj Scm_NativeOfftType(void);
SCM_EXTERN ScmObj Scm_NativeIntptrtType(void);
SCM_EXTERN ScmObj Scm_NativeUintptrtType(void);
SCM_EXTERN ScmObj Scm_NativeFloatType(void);
SCM_EXTERN ScmObj Scm_NativeDoubleType(void);
SCM_EXTERN ScmObj Scm_NativeCCharType(void);
SCM_EXTERN ScmObj Scm_NativeCStringType(void);
SCM_EXTERN ScmObj Scm_NativeVoidType(void);
SCM_EXTERN ScmObj Scm_NativeVoidPointerType(void);
SCM_EXTERN ScmObj Scm_NativeScmObjType(void);

/* <c-pointer-type> - a pointer to another native type */
typedef struct ScmCPointerTypeRec {
    ScmNativeType common;
    ScmNativeType *pointee_type;
} ScmCPointerType;

SCM_CLASS_DECL(Scm_CPointerTypeClass);
#define SCM_CLASS_C_POINTER_TYPE (&Scm_CPointerTypeClass)
#define SCM_C_POINTER_TYPE(obj)  ((ScmCPointerType*)(obj))
#define SCM_C_POINTER_TYPE_P(obj) (SCM_ISA(obj, SCM_CLASS_C_POINTER_TYPE))

/* <c-function-type> - a function pointer type */
typedef struct ScmCFunctionTypeRec {
    ScmNativeType common;
    ScmNativeType *return_type;
    ScmObj argument_types;       /* list of ScmNativeType* */
    int variadic;                /* boolean: does it accept varargs? */
} ScmCFunctionType;

SCM_CLASS_DECL(Scm_CFunctionTypeClass);
#define SCM_CLASS_C_FUNCTION_TYPE (&Scm_CFunctionTypeClass)
#define SCM_C_FUNCTION_TYPE(obj) ((ScmCFunctionType*)(obj))
#define SCM_C_FUNCTION_TYPE_P(obj) (SCM_ISA(obj, SCM_CLASS_C_FUNCTION_TYPE))

/* <c-array-type> - an array of a native type */
typedef struct ScmCArrayTypeRec {
    ScmNativeType common;
    ScmNativeType *element_type;
    ScmObj dimensions;           /* list of fixnums, first elt can be '* */
} ScmCArrayType;

SCM_CLASS_DECL(Scm_CArrayTypeClass);
#define SCM_CLASS_C_ARRAY_TYPE   (&Scm_CArrayTypeClass)
#define SCM_C_ARRAY_TYPE(obj)    ((ScmCArrayType*)(obj))
#define SCM_C_ARRAY_TYPE_P(obj)  (SCM_ISA(obj, SCM_CLASS_C_ARRAY_TYPE))

/* <c-struct-type> - a C struct type */
typedef struct ScmCStructTypeRec {
    ScmNativeType common;
    ScmObj tag;                  /* symbol or #f */
    ScmObj fields;               /* list of (name type offset) */
} ScmCStructType;

SCM_CLASS_DECL(Scm_CStructTypeClass);
#define SCM_CLASS_C_STRUCT_TYPE  (&Scm_CStructTypeClass)
#define SCM_C_STRUCT_TYPE(obj)   ((ScmCStructType*)(obj))
#define SCM_C_STRUCT_TYPE_P(obj) (SCM_ISA(obj, SCM_CLASS_C_STRUCT_TYPE))

/* <c-union-type> - a C union type.
   The C-level structure is the same as native-struct;
   the difference (all field offsets are 0) is handled at the Scheme level. */
typedef ScmCStructType ScmCUnionType;

SCM_CLASS_DECL(Scm_CUnionTypeClass);
#define SCM_CLASS_C_UNION_TYPE   (&Scm_CUnionTypeClass)
#define SCM_C_UNION_TYPE(obj)    ((ScmCUnionType*)(obj))
#define SCM_C_UNION_TYPE_P(obj)  (SCM_ISA(obj, SCM_CLASS_C_UNION_TYPE))

/* <c-enum-type> - a C enum type.
   type-spec is C23's enum-type-specifier, e.g. 'int8_t' in
     'enum foo : int8_t { ... }'.
*/
typedef struct ScmCEnumTypeRec {
    ScmNativeType common;
    ScmObj tag;                 /* symbol or #f */
    ScmObj type_spec;           /* native-type or #f */
    ScmObj enumerator_alist;    /* ((id . value) ...) */
} ScmCEnumType;

SCM_CLASS_DECL(Scm_CEnumTypeClass);
#define SCM_CLASS_C_ENUM_TYPE   (&Scm_CEnumTypeClass)
#define SCM_C_ENUM_TYPE(obj)    ((ScmCEnumType*)(obj))
#define SCM_C_ENUM_TYPE_P(obj)  (SCM_ISA(obj, SCM_CLASS_C_ENUM_TYPE))

/* <c-opaque-type> - an opaque type.
   In C, this is something typedef'ed.  We treat them as unknown type,
   unreferenceable/unmodifiable, but we can treat pointer to it just fine.
   The instance is the same as ScmNativeType.
 */

typedef ScmNativeType ScmCOpaqueType;
SCM_CLASS_DECL(Scm_COpaqueTypeClass);
#define SCM_CLASS_C_OPAQUE_TYPE  (&Scm_COpaqueTypeClass)
#define SCM_C_OPAQUE_TYPE(obj)   ((ScmCOpaqueType*)(obj))
#define SCM_C_OPAQUE_TYPE_P(obj) (SCM_ISA(obj, SCM_CLASS_C_OPAQUE_TYPE))

/*
 * Native handle
 */

struct ScmNativeHandleRec {
    SCM_HEADER;
    void *ptr;                  /* If it is <c-pointer-type>, this is
                                   the pointer value.  Otherwise, this
                                   is a pointer to the typed value.
                                   (In another word, <c-pointer-type>
                                   does not have extra indirection).
                                */
    ScmNativeType *type;        /* One of native aggregate types.
                                   Can be <void*> */
    void *region_min;
    void *region_max;           /* Specifies valid memory region, min
                                   inclusive, max exclusive.
                                   Used to validate pointer arithmetics.
                                */
    ScmObj name;                /* Typically simplified type name,
                                   used for printing. */
    ScmObj owner;               /* If this points to another Gauche-
                                   allocated object, keep it here to prevent
                                   it from GC-ed.  Can be #<undef>. */
    ScmObj attrs;               /* Alist of attributes */
    u_long flags;               /* Reserved. */
};


SCM_EXTERN ScmObj Scm__MakeNativeHandle(void *ptr,
                                        ScmNativeType *type,
                                        ScmObj name,
                                        void *region_min,
                                        void *region_max,
                                        ScmObj owner,
                                        ScmObj attrs,
                                        u_long flags);



#endif  /*GAUCHE_PRIV_TYPEP_H*/
