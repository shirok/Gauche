
#define SCM_S8VECTOR_BOX(obj, elt)    (obj) = SCM_MAKE_INT(elt)
#define SCM_S8VECTOR_UNBOX(elt, obj)                                       \
    do {                                                                   \
        int v;                                                             \
        if (!SCM_INTP(obj)) Scm_Error("argument out of domain: %S", obj);  \
        v = SCM_INT_VALUE(obj);                                            \
        if (v < -128 || v > 127)                                           \
            Scm_Error("argument out of bound: %d", v);                     \
        elt = (signed char)v;                                              \
    } while (0)
#define SCM_S8VECTOR_PRINT_ELT(out, elt)    Scm_Printf(out, "%d", elt)
#define SCM_S8VECTOR_EQUAL_ELT(x, y)      ((x)==(y))

#define SCM_U8VECTOR_BOX(obj, elt)    (obj) = SCM_MAKE_INT(elt)
#define SCM_U8VECTOR_UNBOX(elt, obj)                                       \
    do {                                                                   \
        int v;                                                             \
        if (!SCM_INTP(obj)) Scm_Error("argument out of domain: %S", obj);  \
        v = SCM_INT_VALUE(obj);                                            \
        if (v < 0 || v > 255)                                              \
            Scm_Error("argument out of bound: %d", v);                     \
        elt = (unsigned char)v;                                            \
    } while (0)
#define SCM_U8VECTOR_PRINT_ELT(out, elt)    Scm_Printf(out, "%u", elt)
#define SCM_U8VECTOR_EQUAL_ELT(x, y)      ((x)==(y))

#define SCM_S16VECTOR_BOX(obj, elt)    (obj) = SCM_MAKE_INT(elt)
#define SCM_S16VECTOR_UNBOX(elt, obj)                                      \
    do {                                                                   \
        int v;                                                             \
        if (!SCM_INTP(obj)) Scm_Error("argument out of domain: %S", obj);  \
        v = SCM_INT_VALUE(obj);                                            \
        if (v < -32768 || v > 32767)                                       \
            Scm_Error("argument out of bound: %d", v);                     \
        elt = (short)v;                                                    \
    } while (0)
#define SCM_S16VECTOR_PRINT_ELT(out, elt)   Scm_Printf(out, "%d", elt)
#define SCM_S16VECTOR_EQUAL_ELT(x, y)      ((x)==(y))

#define SCM_U16VECTOR_BOX(obj, elt)    (obj) = SCM_MAKE_INT(elt)
#define SCM_U16VECTOR_UNBOX(elt, obj)                                      \
    do {                                                                   \
        int v;                                                             \
        if (!SCM_INTP(obj)) Scm_Error("argument out of domain: %S", obj);  \
        v = SCM_INT_VALUE(obj);                                            \
        if (v < 0 || v > 65536)                                            \
            Scm_Error("argument out of bound: %d", v);                     \
        elt = (unsigned short)v;                                           \
    } while (0)
#define SCM_U16VECTOR_PRINT_ELT(out, elt)   Scm_Printf(out, "%u", elt)
#define SCM_U16VECTOR_EQUAL_ELT(x, y)      ((x)==(y))


#define SCM_S32VECTOR_BOX(obj, elt)    (obj) = Scm_MakeInteger(elt)
#define SCM_S32VECTOR_UNBOX(elt, obj)                                   \
    do {                                                                \
        SCM_UVECTOR_INT32 v;                                            \
        if (SCM_INTP(obj)) v = SCM_INT_VALUE(obj);                      \
        else if (SCM_BIGNUMP(obj)) v = Scm_BignumToSI(SCM_BIGNUM(obj)); \
        else Scm_Error("argument out of domain: %S", obj);              \
        elt = v;                                                        \
    } while (0)
#define SCM_S32VECTOR_PRINT_ELT(out, elt)  Scm_Printf(out, "%d", elt)
#define SCM_S32VECTOR_EQUAL_ELT(x, y)      ((x)==(y))


#define SCM_U32VECTOR_BOX(obj, elt)             \
    do {                                        \
       if (elt <= SCM_SMALL_INT_MAX) {          \
           (obj) = Scm_MakeInteger(elt);        \
       } else {                                 \
           (obj) = Scm_MakeBignumFromUI(elt);   \
       }                                        \
    } while (0)
#define SCM_U32VECTOR_UNBOX(elt, obj)                                   \
    do {                                                                \
        u_long v;                                                       \
        if (SCM_INTP(obj)) v = SCM_INT_VALUE(obj);                      \
        else if (SCM_BIGNUMP(obj)) v = Scm_BignumToUI(SCM_BIGNUM(obj)); \
        else Scm_Error("argument out of domain: %S", obj);              \
        elt = v;                                                        \
    } while (0)
#define SCM_U32VECTOR_PRINT_ELT(out, elt)   Scm_Printf(out, "%d", elt)
#define SCM_U32VECTOR_EQUAL_ELT(x, y)      ((x)==(y))

#if SIZEOF_LONG >= 8
#define SCM_S64VECTOR_BOX(obj, elt)    (obj) = Scm_MakeInteger(elt)
#define SCM_S64VECTOR_UNBOX(elt, obj)                                   \
    do {                                                                \
        long v;                                                         \
        if (SCM_INTP(obj)) v = SCM_INT_VALUE(obj);                      \
        else if (SCM_BIGNUMP(obj)) v = Scm_BignumToSI(SCM_BIGNUM(obj)); \
        else Scm_Error("argument out of domain: %S", obj);              \
        elt = v;                                                        \
    } while (0)
#define SCM_S64VECTOR_PRINT_ELT(out, elt)  Scm_Printf(out, "%ld", elt)
#define SCM_S64VECTOR_EQUAL_ELT(x, y)      ((x)==(y))
#else /* assuming SIZEOF_LONG == 4 */

static inline int valid_int64(ScmObj obj)
{
    if (SCM_BIGNUMP(obj)) {
        if (!((SCM_BIGNUM(obj)->size <= 8/SIZEOF_LONG)
               || (SCM_BIGNUM(obj)->sign < 0
                   && SCM_BIGNUM(obj)->size == 8/SIZEOF_LONG + 1
                   && SCM_BIGNUM(obj)->values[8/SIZEOF_LONG + 1] == 1)))
            return FALSE;
        else
            return TRUE;
    } else if (SCM_INTP(obj)) {
        return TRUE;
    } else {
        return FALSE;
    }
}

#define SCM_S64VECTOR_BOX(obj, elt)    (obj) = (elt)
#define SCM_S64VECTOR_UNBOX(elt, obj)                                   \
    do {                                                                \
        if (!valid_int64(obj))                                          \
            Scm_Error("argument out of domain: %S", obj);               \
        elt = obj;                                                      \
    } while (0)
#define SCM_S64VECTOR_PRINT_ELT(out, elt)  Scm_Printf(out, "%S", elt)
#define SCM_S64VECTOR_EQUAL_ELT(x, y)      Scm_NumEq(x, y)
#endif


#if SIZEOF_LONG >= 8
#define SCM_U64VECTOR_BOX(obj, elt)    (obj) = Scm_MakeInteger(elt)
#define SCM_U64VECTOR_UNBOX(elt, obj)                                   \
    do {                                                                \
        u_long v;                                                       \
        if (SCM_INTP(obj)) v = SCM_INT_VALUE(obj);                      \
        else if (SCM_BIGNUMP(obj)) v = Scm_BignumToSI(SCM_BIGNUM(obj)); \
        else Scm_Error("argument out of domain: %S", obj);              \
        elt = v;                                                        \
    } while (0)
#define SCM_U64VECTOR_PRINT_ELT(out, elt)  Scm_Printf(out, "%ld", elt)
#define SCM_U64VECTOR_EQUAL_ELT(x, y)      ((x)==(y))
#else /* assuming SIZEOF_LONG == 4 */

static inline int valid_uint64(ScmObj obj)
{
    if (SCM_BIGNUMP(obj)) {
        if (SCM_BIGNUM(obj)->sign < 0) return FALSE;
        if (SCM_BIGNUM(obj)->size > 8/SIZEOF_LONG) return FALSE;
        return TRUE;
    } else if (SCM_INTP(obj)) {
        if (SCM_INT_VALUE(obj) < 0) return FALSE;
        return TRUE;
    } else {
        return FALSE;
    }
}

#define SCM_U64VECTOR_BOX(obj, elt)    (obj) = (elt)
#define SCM_U64VECTOR_UNBOX(elt, obj)                                   \
    do {                                                                \
        if (!valid_uint64(obj))                                         \
            Scm_Error("argument out of domain: %S", obj);               \
        elt = obj;                                                      \
    } while (0)
#define SCM_U64VECTOR_PRINT_ELT(out, elt)  Scm_Printf(out, "%S", elt)
#define SCM_U64VECTOR_EQUAL_ELT(x, y)      Scm_NumEq(x, y)
#endif


#define SCM_F32VECTOR_BOX(obj, elt)  (obj) = Scm_MakeFlonum((double)elt)
#define SCM_F32VECTOR_UNBOX(elt, obj)                                   \
    do {                                                                \
        float v;                                                        \
        if (SCM_FLONUMP(obj)) v = (float)SCM_FLONUM_VALUE(obj);         \
        else if (SCM_INTP(obj)) v = (float)SCM_INT_VALUE(obj);          \
        else if (SCM_BIGNUMP(obj)) v = Scm_BignumToDouble(SCM_BIGNUM(obj)); \
        else Scm_Error("argument out of domain: %S", obj);              \
        elt = v;                                                        \
    } while (0)
#define SCM_F32VECTOR_PRINT_ELT(out, elt)   Scm_Printf(out, "%f", elt)
#define SCM_F32VECTOR_EQUAL_ELT(x, y)      ((x)==(y))


#define SCM_F64VECTOR_BOX(obj, elt)  (obj) = Scm_MakeFlonum(elt)
#define SCM_F64VECTOR_UNBOX(elt, obj)                                   \
    do {                                                                \
        double v;                                                       \
        if (SCM_FLONUMP(obj)) v = SCM_FLONUM_VALUE(obj);                \
        else if (SCM_INTP(obj)) v = (double)SCM_INT_VALUE(obj);         \
        else if (SCM_BIGNUMP(obj)) v = Scm_BignumToDouble(SCM_BIGNUM(obj)); \
        else Scm_Error("argument out of domain: %S", obj);              \
        elt = v;                                                        \
    } while (0)
#define SCM_F64VECTOR_PRINT_ELT(out, elt)   Scm_Printf(out, "%lf", elt)
#define SCM_F64VECTOR_EQUAL_ELT(x, y)      ((x)==(y))
