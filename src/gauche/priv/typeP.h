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
    int      (*c_of_type)(ScmObj);
    ScmObj   (*c_ref)(void*);
    void     (*c_set)(void*, ScmObj);
    ScmObj   super;
    const char *c_type_name;
    size_t   size;
    size_t   alignment;
} ScmNativeType;

SCM_CLASS_DECL(Scm_NativeTypeClass);
#define SCM_CLASS_NATIVE_TYPE    (&Scm_NativeTypeClass)
#define SCM_NATIVE_TYPE(obj)     ((ScmNativeType*)(obj))
#define SCM_NATIVE_TYPE_P(obj)   (SCM_ISA(obj, SCM_CLASS_NATIVE_TYPE))

/* <native-pointer> - a pointer to another native type */
typedef struct ScmNativePointerRec {
    ScmNativeType common;
    ScmNativeType *pointee_type;
} ScmNativePointer;

SCM_CLASS_DECL(Scm_NativePointerClass);
#define SCM_CLASS_NATIVE_POINTER (&Scm_NativePointerClass)
#define SCM_NATIVE_POINTER(obj)  ((ScmNativePointer*)(obj))
#define SCM_NATIVE_POINTER_P(obj) (SCM_ISA(obj, SCM_CLASS_NATIVE_POINTER))

/* <native-function> - a function pointer type */
typedef struct ScmNativeFunctionRec {
    ScmNativeType common;
    ScmNativeType *return_type;
    ScmObj arg_types;            /* list of ScmNativeType* */
    int varargs;                 /* boolean: does it accept varargs? */
} ScmNativeFunction;

SCM_CLASS_DECL(Scm_NativeFunctionClass);
#define SCM_CLASS_NATIVE_FUNCTION (&Scm_NativeFunctionClass)
#define SCM_NATIVE_FUNCTION(obj) ((ScmNativeFunction*)(obj))
#define SCM_NATIVE_FUNCTION_P(obj) (SCM_ISA(obj, SCM_CLASS_NATIVE_FUNCTION))

/* <native-array> - an array of a native type */
typedef struct ScmNativeArrayRec {
    ScmNativeType common;
    ScmNativeType *element_type;
    ScmObj dimensions;           /* list of fixnums */
} ScmNativeArray;

SCM_CLASS_DECL(Scm_NativeArrayClass);
#define SCM_CLASS_NATIVE_ARRAY   (&Scm_NativeArrayClass)
#define SCM_NATIVE_ARRAY(obj)    ((ScmNativeArray*)(obj))
#define SCM_NATIVE_ARRAY_P(obj)  (SCM_ISA(obj, SCM_CLASS_NATIVE_ARRAY))

/* <native-struct> - a C struct type */
typedef struct ScmNativeStructRec {
    ScmNativeType common;
    ScmObj tag;                  /* symbol or #f */
    ScmObj fields;               /* list of (name type offset) */
} ScmNativeStruct;

SCM_CLASS_DECL(Scm_NativeStructClass);
#define SCM_CLASS_NATIVE_STRUCT  (&Scm_NativeStructClass)
#define SCM_NATIVE_STRUCT(obj)   ((ScmNativeStruct*)(obj))
#define SCM_NATIVE_STRUCT_P(obj) (SCM_ISA(obj, SCM_CLASS_NATIVE_STRUCT))

/* <native-union> - a C union type.
   The C-level structure is the same as native-struct;
   the difference (all field offsets are 0) is handled at the Scheme level. */
typedef ScmNativeStruct ScmNativeUnion;

SCM_CLASS_DECL(Scm_NativeUnionClass);
#define SCM_CLASS_NATIVE_UNION   (&Scm_NativeUnionClass)
#define SCM_NATIVE_UNION(obj)    ((ScmNativeUnion*)(obj))
#define SCM_NATIVE_UNION_P(obj)  (SCM_ISA(obj, SCM_CLASS_NATIVE_UNION))

/*
 * Native handle
 *
 *  <native-handle> is a typed handle to access low-level data structure;
 *  foreign pointers and other data structues can be accessed from Scheme
 *  world via native handles.
 *  (It is effectively a wrapped pointer, but it may be a C pointer
 *  or C struct; e.g. in the contect of FFI, we need to distinguish
 *  "C pointer to a struct" and "C struct itself".  To avoid confusion,
 *  we expose 'handle' to the Scheme world.)
 */

typedef struct ScmNativeHandleRec {
    SCM_HEADER;
    void *ptr;                  /* If it is <native-pointer>, this is
                                   the pointer value.  Otherwise, this
                                   is a pointer to the typed value.
                                   (In another word, <native-pointer>
                                   does not have extra indirection).
                                */
    ScmNativeType *type;        /* One of native aggregate types.
                                   Can be <native-pointer <void*>) */
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
} ScmNativeHandle;

SCM_CLASS_DECL(Scm_NativeHandleClass);
#define SCM_CLASS_NATIVE_HANDLE   (&Scm_NativeHandleClass)
#define SCM_NATIVE_HANDLE(obj)    ((ScmNativeHandle*)(obj))
#define SCM_NATIVE_HANDLE_P(obj)  (SCM_ISA(obj, SCM_CLASS_NATIVE_HANDLE))

SCM_EXTERN ScmObj Scm__MakeNativeHandle(void *ptr,
                                        ScmNativeType *type,
                                        ScmObj name,
                                        void *region_min,
                                        void *region_max,
                                        ScmObj owner,
                                        ScmObj attrs,
                                        u_long flags);



#endif  /*GAUCHE_PRIV_TYPEP_H*/
