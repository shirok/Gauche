/*
 * gauche.h - Gauche scheme system header
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, ditribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: gauche.h,v 1.20 2001-02-02 10:11:40 shiro Exp $
 */

#ifndef GAUCHE_H
#define GAUCHE_H

#include <stdio.h>
#include <sys/types.h>
#include <stdarg.h>
#include <setjmp.h>
#include <gc.h>

#define SCM_INLINE_MALLOC_PRIMITIVES
#undef SCM_VM_USE_STACK
#define SCM_VM_STACK_SIZE     10000
#define CHARCODE_EUC_JP

#ifdef CHARCODE_EUC_JP
#include "gauche/char_euc_jp.h"
#else
#ifdef CHARCODE_SJIS
#include "gauche/char_sjis.h"
#else
#include "gauche/char_utf8.h"
#endif
#endif

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE (!FALSE)
#endif

/*-------------------------------------------------------------
 * BASIC TYPES
 */

/*
 * A word large enough to hold a pointer
 */
typedef unsigned long ScmWord;

/*
 * A byte
 */
typedef unsigned char ScmByte;

/*
 * A character.
 */
typedef long ScmChar;

/*
 * An opaque pointer.  All Scheme objects are represented by
 * this type.
 */
typedef struct ScmHeaderRec *ScmObj;

/* TAG STRUCTURE
 *
 * [Pointer]
 *      -------- -------- -------- ------00
 *      Points to cell (4-byte aligned)
 *
 * [Fixnum]
 *      -------- -------- -------- ------01
 *      30-bit signed integer
 *
 * [Character]
 *      -------- -------- -------- -----010
 *      29-bit
 *
 * [Miscellaneous]
 *      -------- -------- -------- ----0110
 *      #f, #t, '(), eof-object, undefined
 *
 * [VM Instructions]
 *      -------- -------- -------- ----1110
 *      Only appears in a compiled code.
 */

/* Type coercer */

#define	SCM_OBJ(obj)      ((ScmObj)(obj))
#define	SCM_WORD(obj)     ((ScmWord)(obj))

/*
 * PRIMARY TAG IDENTIFICATION
 */

#define	SCM_TAG(obj)     (SCM_WORD(obj) & 0x03)
#define SCM_PTRP(obj)    (SCM_TAG(obj) == 0)

/*
 * IMMEDIATE OBJECTS
 */

#define SCM_IMMEDIATEP(obj) ((SCM_WORD(obj)&0x0f) == 6)
#define SCM_ITAG(obj)       (SCM_WORD(obj)>>4)

#define SCM__MAKE_ITAG(num)  (((num)<<4) + 6)
#define SCM_FALSE           SCM_OBJ(SCM__MAKE_ITAG(0)) /* #f */
#define SCM_TRUE            SCM_OBJ(SCM__MAKE_ITAG(1)) /* #t  */
#define SCM_NIL             SCM_OBJ(SCM__MAKE_ITAG(2)) /* '() */
#define SCM_EOF             SCM_OBJ(SCM__MAKE_ITAG(3)) /* eof-object */
#define SCM_UNDEFINED       SCM_OBJ(SCM__MAKE_ITAG(4)) /* #undefined */
#define SCM_UNBOUND         SCM_OBJ(SCM__MAKE_ITAG(5)) /* unbound value */

#define SCM_FALSEP(obj)     ((obj) == SCM_FALSE)
#define SCM_TRUEP(obj)      ((obj) == SCM_TRUE)
#define SCM_NULLP(obj)      ((obj) == SCM_NIL)
#define SCM_EOFP(obj)       ((obj) == SCM_EOF)
#define SCM_UNDEFINEDP(obj) ((obj) == SCM_UNDEFINED)
#define SCM_UNBOUNDP(obj)   ((obj) == SCM_UNBOUNDP)

/*
 * BOOLEAN
 */
#define SCM_BOOLP(obj)       ((obj) == SCM_TRUE || (obj == SCM_FALSE))
#define	SCM_MAKE_BOOL(obj)   ((obj)? SCM_TRUE:SCM_FALSE)

#define SCM_EQ(x, y)         ((x) == (y))

extern ScmObj Scm_Not(ScmObj obj);
extern ScmObj Scm_EqP(ScmObj x, ScmObj y);
extern ScmObj Scm_EqvP(ScmObj x, ScmObj y);
extern ScmObj Scm_EqualP(ScmObj x, ScmObj y);

/*
 * FIXNUM
 */

#define SCM_INTP(obj)        (SCM_TAG(obj) == 1)
#define SCM_INT_VALUE(obj)   (((signed long int)(obj)) >> 2)
#define SCM_MAKE_INT(obj)    SCM_OBJ(((obj) << 2) + 1)

/*
 * CHARACTERS
 *
 *  A character is represented by (up to) 29-bit integer.  The actual
 *  encoding depends on compile-time flags.
 *
 *  For character cases, I only care about ASCII chars (at least for now)
 */

#define	SCM_CHAR(obj)           ((ScmChar)(obj))
#define	SCM_CHARP(obj)          ((SCM_WORD(obj)&0x07L) == 2)
#define	SCM_CHAR_VALUE(obj)     SCM_CHAR(SCM_WORD(obj) >> 3)
#define	SCM_MAKE_CHAR(ch)       SCM_OBJ(((ch) << 3) + 2)

#define SCM_CHAR_INVALID        ((ScmChar)(-1)) /* indicate invalid char */

#define SCM_CHAR_ASCII_P(ch)    ((ch) < 0x80)
#define SCM_CHAR_UPPER_P(ch)    (('A' <= (ch)) && ((ch) <= 'Z'))
#define SCM_CHAR_LOWER_P(ch)    (('a' <= (ch)) && ((ch) <= 'z'))
#define SCM_CHAR_UPCASE(ch)     (SCM_CHAR_LOWER_P(ch)?((ch)-('a'-'A')):(ch))
#define SCM_CHAR_DOWNCASE(ch)   (SCM_CHAR_UPPER_P(ch)?((ch)+('a'-'A')):(ch))


/*
 * HEAP ALLOCATED OBJECTS
 *
 *  Common header of all the heap-allocated objects.
 */

typedef struct ScmHeaderRec {
    struct ScmClassRec *klass;
} ScmHeader;

#define SCM_HEADER       ScmHeader hdr /* for declaration */

#define SCM_CLASS_OF(obj)      (SCM_OBJ(obj)->klass)
#define SCM_XTYPEP(obj, klass) (SCM_PTRP(obj)&&(SCM_CLASS_OF(obj) == (klass)))

#define SCM_CLASS(obj)        ((ScmClass*)(obj))
#define SCM_CLASSP(obj)       SCM_XTYPEP(obj, SCM_CLASS_CLASS)

#define SCM_NEW(type)         ((type*)(Scm_Malloc(sizeof(type))))
#define SCM_NEW2(type, size)  ((type)(Scm_Malloc(size)))
#define SCM_NEW_ATOMIC(type)  ((type*)(Scm_MallocAtomic(sizeof(type))))
#define SCM_NEW_ATOMIC2(type, size) ((type)(Scm_MallocAtomic(size)))

extern void *Scm_MallocWords(size_t words);
extern void *Scm_Malloc(size_t size);
extern void *Scm_MallocAtomic(size_t size);

typedef struct ScmVMRec        ScmVM;
typedef struct ScmClassRec     ScmClass;
typedef struct ScmPairRec      ScmPair;
typedef struct ScmStringRec    ScmString;
typedef struct ScmDStringRec   ScmDString;
typedef struct ScmVectorRec    ScmVector;
typedef struct ScmBignumRec    ScmBignum;
typedef struct ScmFlonumRec    ScmFlonum;
typedef struct ScmComplexRec   ScmComplex;
typedef struct ScmPortRec      ScmPort;
typedef struct ScmHashTableRec ScmHashTable;
typedef struct ScmModuleRec    ScmModule;
typedef struct ScmSymbolRec    ScmSymbol;
typedef struct ScmGlocRec      ScmGloc;
typedef struct ScmProcedureRec ScmProcedure;
typedef struct ScmClosureRec   ScmClosure;
typedef struct ScmSubrRec      ScmSubr;
typedef struct ScmSyntaxRec    ScmSyntax;
typedef struct ScmPromiseRec   ScmPromise;
typedef struct ScmExceptionRec ScmException;

/*---------------------------------------------------------
 * VM STUFF
 */

/* Detailed definitions are in vm.h.  Here I expose external interface */

#include "gauche/vm.h"

#define SCM_VM(obj)          ((ScmVM *)(obj))
#define SCM_VMP(obj)         SCM_XTYPEP(obj, SCM_CLASS_VM)

#define SCM_VM_CURRENT_INPUT_PORT(vm)   (SCM_VM(vm)->curin)
#define SCM_VM_CURRENT_OUTPUT_PORT(vm)  (SCM_VM(vm)->curout)
#define SCM_VM_CURRENT_ERROR_PORT(vm)   (SCM_VM(vm)->curerr)

#define SCM_VM_ERRSTR(vm)               (SCM_VM(vm)->errstr)

extern ScmVM *Scm_VM(void);     /* Returns the current VM */

extern ScmObj Scm_Compile(ScmObj form, ScmObj env, int context);
extern void   Scm_Run(ScmObj program);

extern ScmObj Scm_VMGetResult(ScmVM *vm);
extern ScmObj Scm_VMGetErrorString(ScmVM *vm);
extern ScmObj Scm_VMGetStack(ScmVM *vm);

extern ScmObj Scm_VMApply(ScmObj proc, ScmObj args);
extern ScmObj Scm_VMApply0(ScmObj proc);
extern ScmObj Scm_VMApply1(ScmObj proc, ScmObj arg);
extern ScmObj Scm_VMApply2(ScmObj proc, ScmObj arg1, ScmObj arg2);

extern ScmObj Scm_VMEval(ScmObj expr, ScmObj env);

extern ScmObj Scm_VMCall(ScmObj *args, int argcnt, void *data);

extern ScmObj Scm_VMDynamicWind(ScmObj pre, ScmObj body, ScmObj post);

extern ScmObj Scm_VMThrowException(ScmObj exception);

/*---------------------------------------------------------
 * CLASS
 */

struct ScmClassRec {
    SCM_HEADER;
    char *name;
    int (*print)(ScmObj obj, ScmPort *sink, int level);
    struct ScmClassRec **cpl;
};

extern ScmClass *Scm_ClassOf(ScmObj obj);
extern ScmObj Scm_ClassCPL(ScmClass *klass);
extern ScmObj Scm_SubtypeP(ScmClass *sub, ScmClass *type);
extern ScmObj Scm_TypeP(ScmObj obj, ScmClass *type);

extern ScmClass *Scm_MakeBuiltinClass(const char *name,
                                      int (*printer)(ScmObj, ScmPort*, int),
                                      ScmObj supers);

/* built-in classes */
extern ScmClass Scm_TopClass;
extern ScmClass Scm_BoolClass;
extern ScmClass Scm_CharClass;
extern ScmClass Scm_ClassClass;
extern ScmClass Scm_UnknownClass;
extern ScmClass Scm_CollectionClass;
extern ScmClass Scm_SequenceClass;

#define SCM_CLASS_TOP          (&Scm_TopClass)
#define SCM_CLASS_BOOL         (&Scm_BoolClass)
#define SCM_CLASS_CHAR         (&Scm_CharClass)
#define SCM_CLASS_CLASS        (&Scm_ClassClass)
#define SCM_CLASS_UNKNOWN      (&Scm_UnknownClass)
#define SCM_CLASS_COLLECTION   (&Scm_CollectionClass)
#define SCM_CLASS_SEQUENCE     (&Scm_SequenceClass)

/*--------------------------------------------------------
 * PAIR AND LIST
 */

struct ScmPairRec {
    SCM_HEADER;
    ScmObj car;
    ScmObj cdr;
    ScmObj attributes;
};

#define SCM_PAIRP(obj)          SCM_XTYPEP(obj, SCM_CLASS_PAIR)
#define SCM_PAIR(obj)           ((ScmPair*)(obj))
#define SCM_CAR(obj)            (SCM_PAIR(obj)->car)
#define SCM_CDR(obj)            (SCM_PAIR(obj)->cdr)
#define SCM_CAAR(obj)           (SCM_CAR(SCM_CAR(obj)))
#define SCM_CADR(obj)           (SCM_CAR(SCM_CDR(obj)))
#define SCM_CDAR(obj)           (SCM_CDR(SCM_CAR(obj)))
#define SCM_CDDR(obj)           (SCM_CDR(SCM_CDR(obj)))

#define SCM_SET_CAR(obj, value) (SCM_CAR(obj) = (value))
#define SCM_SET_CDR(obj, value) (SCM_CDR(obj) = (value))
#define SCM_PAIR_ATTR(obj)      (SCM_PAIR(obj)->attributes)

extern ScmClass Scm_ListClass;
extern ScmClass Scm_PairClass;
extern ScmClass Scm_NullClass;
#define SCM_CLASS_LIST      (&Scm_ListClass)
#define SCM_CLASS_PAIR      (&Scm_PairClass)
#define SCM_CLASS_NULL      (&Scm_NullClass)

#define SCM_LISTP(obj)          (SCM_NULLP(obj) || SCM_PAIRP(obj))

/* Useful macros to manipulating lists. */

#define	SCM_FOR_EACH(p, list) \
    for((p) = (list); SCM_PAIRP(p); (p) = SCM_CDR(p))

#define	SCM_APPEND1(start, last, obj)                           \
    do {                                                        \
	if (SCM_NULLP(start)) {                                 \
	    (start) = (last) = Scm_Cons((obj), SCM_NIL);        \
	} else {                                                \
	    SCM_SET_CDR((last), Scm_Cons((obj), SCM_NIL));      \
	    (last) = SCM_CDR(last);                             \
	}                                                       \
    } while (0)

#define	SCM_APPEND(start, last, obj)                    \
    do {                                                \
        ScmObj list_SCM_GLS = (obj);                    \
	if (SCM_NULLP(start)) {                         \
	    (start) = (list_SCM_GLS);                   \
            if (!SCM_NULLP(list_SCM_GLS)) {             \
                (last) = Scm_LastPair(list_SCM_GLS);    \
            }                                           \
        } else {                                        \
	    SCM_SET_CDR((last), (list_SCM_GLS));        \
	    (last) = Scm_LastPair(last);                \
	}                                               \
    } while (0)

#define SCM_LIST1(a)             Scm_Cons(a, SCM_NIL)
#define SCM_LIST2(a,b)           Scm_Cons(a, SCM_LIST1(b))
#define SCM_LIST3(a,b,c)         Scm_Cons(a, SCM_LIST2(b, c))
#define SCM_LIST4(a,b,c,d)       Scm_Cons(a, SCM_LIST3(b, c, d))
#define SCM_LIST5(a,b,c,d,e)     Scm_Cons(a, SCM_LIST4(b, c, d, e))

extern ScmObj Scm_Cons(ScmObj car, ScmObj cdr);
extern ScmObj Scm_List(ScmObj elt, ...);
extern ScmObj Scm_Conses(ScmObj elt, ...);
extern ScmObj Scm_VaList(va_list elts);
extern ScmObj Scm_VaCons(va_list elts);

extern ScmObj Scm_PairP(ScmObj obj);
extern ScmObj Scm_Car(ScmObj obj);
extern ScmObj Scm_Cdr(ScmObj obj);
extern ScmObj Scm_Caar(ScmObj obj);
extern ScmObj Scm_Cadr(ScmObj obj);
extern ScmObj Scm_Cdar(ScmObj obj);
extern ScmObj Scm_Cddr(ScmObj obj);
extern ScmObj Scm_Caaar(ScmObj obj);
extern ScmObj Scm_Caadr(ScmObj obj);
extern ScmObj Scm_Cadar(ScmObj obj);
extern ScmObj Scm_Caddr(ScmObj obj);
extern ScmObj Scm_Cdaar(ScmObj obj);
extern ScmObj Scm_Cdadr(ScmObj obj);
extern ScmObj Scm_Cddar(ScmObj obj);
extern ScmObj Scm_Cdddr(ScmObj obj);
extern ScmObj Scm_Caaaar(ScmObj obj);
extern ScmObj Scm_Caaadr(ScmObj obj);
extern ScmObj Scm_Caadar(ScmObj obj);
extern ScmObj Scm_Caaddr(ScmObj obj);
extern ScmObj Scm_Cadaar(ScmObj obj);
extern ScmObj Scm_Cadadr(ScmObj obj);
extern ScmObj Scm_Caddar(ScmObj obj);
extern ScmObj Scm_Cadddr(ScmObj obj);
extern ScmObj Scm_Cdaaar(ScmObj obj);
extern ScmObj Scm_Cdaadr(ScmObj obj);
extern ScmObj Scm_Cdadar(ScmObj obj);
extern ScmObj Scm_Cdaddr(ScmObj obj);
extern ScmObj Scm_Cddaar(ScmObj obj);
extern ScmObj Scm_Cddadr(ScmObj obj);
extern ScmObj Scm_Cdddar(ScmObj obj);
extern ScmObj Scm_Cddddr(ScmObj obj);

extern int    Scm_Length(ScmObj obj);
extern ScmObj Scm_CopyList(ScmObj list);
extern ScmObj Scm_MakeList(int len, ScmObj fill);
extern ScmObj Scm_Append2X(ScmObj list, ScmObj obj);
extern ScmObj Scm_Append2(ScmObj list, ScmObj obj);
extern ScmObj Scm_Append(ScmObj args);
extern ScmObj Scm_ReverseX(ScmObj list);
extern ScmObj Scm_Reverse(ScmObj list);
extern ScmObj Scm_ListTail(ScmObj list, int i);
extern ScmObj Scm_ListRef(ScmObj list, int i);
extern ScmObj Scm_LastPair(ScmObj list);

extern ScmObj Scm_Memq(ScmObj obj, ScmObj list);
extern ScmObj Scm_Memv(ScmObj obj, ScmObj list);
extern ScmObj Scm_Member(ScmObj obj, ScmObj list);
extern ScmObj Scm_Assq(ScmObj obj, ScmObj alist);
extern ScmObj Scm_Assv(ScmObj obj, ScmObj alist);
extern ScmObj Scm_Assoc(ScmObj obj, ScmObj alist);

extern ScmObj Scm_Union(ScmObj list1, ScmObj list2);
extern ScmObj Scm_Intersection(ScmObj list1, ScmObj list2);

extern ScmObj Scm_PairAttrGet(ScmPair *pair, ScmObj key, ScmObj fallback);
extern ScmObj Scm_PairAttrSet(ScmPair *pair, ScmObj key, ScmObj value);

extern ScmObj Scm_NullP(ScmObj obj);
extern ScmObj Scm_ListP(ScmObj obj);

/*--------------------------------------------------------
 * STRING
 */

struct ScmStringRec {
    SCM_HEADER;
    long length;
    long size;
    const char *start;
};

#define SCM_STRINGP(obj)        SCM_XTYPEP(obj, SCM_CLASS_STRING)
#define SCM_STRING(obj)         ((ScmString*)(obj))
#define SCM_STRING_LENGTH(obj)  (SCM_STRING(obj)->length)
#define SCM_STRING_SIZE(obj)    (SCM_STRING(obj)->size)
#define SCM_STRING_START(obj)   (SCM_STRING(obj)->start)

#define SCM_STRING_COMPLETE_P(obj) (SCM_STRING_LENGTH(obj) >= 0)

#define SCM_MAKE_STR(cstr)   Scm_MakeStringConst(cstr, -1, -1)

extern ScmClass Scm_StringClass;
#define SCM_CLASS_STRING        (&Scm_StringClass)

extern int     Scm_MBLen(const char *str);

extern ScmObj  Scm_MakeString(const char *str, int size, int len);
extern ScmObj  Scm_MakeStringConst(const char *str, int size, int len);
extern ScmObj  Scm_MakeFillString(int len, ScmChar fill);
extern ScmObj  Scm_MakeStringFromList(ScmObj chars);
extern ScmObj  Scm_CopyString(ScmString *str);

extern char*   Scm_GetString(ScmString *str);
extern const char* Scm_GetStringConst(ScmString *str);

extern ScmObj  Scm_StringEqual(ScmString *x, ScmString *y);
extern ScmObj  Scm_StringLt(ScmString *x, ScmString *y);
extern ScmObj  Scm_StringLe(ScmString *x, ScmString *y);
extern ScmObj  Scm_StringGt(ScmString *x, ScmString *y);
extern ScmObj  Scm_StringGe(ScmString *x, ScmString *y);

extern ScmObj  Scm_StringCiEqual(ScmString *x, ScmString *y);
extern ScmObj  Scm_StringCiLt(ScmString *x, ScmString *y);
extern ScmObj  Scm_StringCiLe(ScmString *x, ScmString *y);
extern ScmObj  Scm_StringCiGt(ScmString *x, ScmString *y);
extern ScmObj  Scm_StringCiGe(ScmString *x, ScmString *y);

extern ScmChar Scm_StringRef(ScmString *str, int k);
extern ScmObj  Scm_StringSet(ScmString *str, int k, ScmChar sc);
extern int     Scm_StringByteRef(ScmString *str, int k);
extern ScmObj  Scm_StringByteSet(ScmString *str, int k, ScmByte b);
extern ScmObj  Scm_Substring(ScmString *x, int start, int end);

extern ScmObj  Scm_StringAppend2(ScmString *x, ScmString *y);
extern ScmObj  Scm_StringAppendC(ScmString *x, const char *s, int size, int len);
extern ScmObj  Scm_StringAppend(ScmObj strs);
extern ScmObj  Scm_StringJoin(ScmObj strs, ScmString *delim);

extern ScmObj  Scm_StringP(ScmObj obj);
extern ScmObj  Scm_StringToList(ScmString *str);
extern ScmObj  Scm_ListToString(ScmObj chars);
extern ScmObj  Scm_StringFill(ScmString *str, ScmChar c);

/* You can allocate a constant string statically, if you calculate
   the length by yourself. */
#define SCM_DEFINE_STRING_CONST(name, str, len, siz)        \
    static ScmString name = {                           \
        SCM_CLASS_STRING, (len), (siz), (str)           \
    };

/* Auxiliary structure to construct a string.  This is not an ScmObj. */
struct ScmDStringRec {
    char *start;
    char *end;
    char *current;
    int length;
};

extern void        Scm_DStringInit(ScmDString *dstr);
extern ScmObj      Scm_DStringGet(ScmDString *dstr);
extern const char *Scm_DStringGetCstr(ScmDString *dstr);
extern void        Scm_DStringPutCstr(ScmDString *dstr, const char *str);
extern void        Scm_DStringAdd(ScmDString *dstr, ScmString *str);
extern void        Scm_DStringPutb(ScmDString *dstr, char byte);
extern void        Scm_DStringPutc(ScmDString *dstr, ScmChar ch);

#define SCM_DSTRING_START(dstr)   ((dstr)->start)
#define SCM_DSTRING_SIZE(dstr)    ((dstr)->end - (dstr)->start)

#define SCM_DSTRING_PUTB(dstr, byte)                                     \
    do {                                                                 \
        if ((dstr)->current >= (dstr)->end) Scm__DStringRealloc(dstr, 1);\
        *(dstr)->current++ = (char)(byte);                               \
        (dstr)->length = -1;    /* may be incomplete */                  \
    } while (0)

#define SCM_DSTRING_PUTC(dstr, ch)                      \
    do {                                                \
        ScmChar ch_DSTR = (ch);                         \
        ScmDString *d_DSTR = (dstr);                    \
        int siz_DSTR = SCM_CHAR_NBYTES(ch_DSTR);        \
        if (d_DSTR->current + siz_DSTR >= d_DSTR->end)  \
            Scm__DStringRealloc(d_DSTR, siz_DSTR);      \
        SCM_STR_PUTC(d_DSTR->current, ch_DSTR);         \
        d_DSTR->current += siz_DSTR;                    \
        if (d_DSTR->length >= 0) d_DSTR->length++;      \
    } while (0)

#define SCM_DSTRING_PUTS(dstr, str, size, len)          \
    do {                                                \
        if ((dstr)->current + (size) >= (dstr)->end)    \
            Scm__DStringRealloc(dstr, size);            \
        memcpy((dstr)->current, (str), (size));         \
        if ((len) >= 0 && (dstr)->length >= 0)          \
            (dstr)->length += (len);                    \
        else (dstr)->length = -1;                       \
    } while (0)

extern void Scm__DStringRealloc(ScmDString *dstr, int min_incr);


/*--------------------------------------------------------
 * VECTOR
 */

struct ScmVectorRec {
    SCM_HEADER;
    int size;
    ScmObj elements[1];
};

#define SCM_VECTOR(obj)          ((ScmVector*)(obj))
#define SCM_VECTORP(obj)         SCM_XTYPEP(obj, SCM_CLASS_VECTOR)
#define SCM_VECTOR_SIZE(obj)     (SCM_VECTOR(obj)->size)
#define SCM_VECTOR_ELEMENTS(obj) (SCM_VECTOR(obj)->elements)
#define SCM_VECTOR_ELEMENT(obj, i)   (SCM_VECTOR(obj)->elements[i])

extern ScmClass Scm_VectorClass;
#define SCM_CLASS_VECTOR     (&Scm_VectorClass)

extern ScmObj Scm_MakeVector(int size, ScmObj fill);
extern ScmObj Scm_VectorRef(ScmVector *vec, int i);
extern ScmObj Scm_VectorSet(ScmVector *vec, int i, ScmObj obj);
extern ScmObj Scm_VectorFill(ScmVector *vec, ScmObj fill);

extern ScmObj Scm_ListToVector(ScmObj l);
extern ScmObj Scm_VectorToList(ScmVector *v);

#define SCM_VECTOR_FOR_EACH(cnt, obj, vec)           \
    for (cnt = 0, obj = SCM_VECTOR_ELEMENT(vec, 0);  \
         cnt < SCM_VECTOR_SIZE(vec);                 \
         obj = SCM_VECTOR_ELEMENT(vec, ++cnt)) 

/*--------------------------------------------------------
 * PORT
 */

/* Since a character is no longer the same as a byte, there is a subtle
   problem in I/O.  What will happen if you mix read-byte and read-char
   from the same port?  What will happen if you try to read a character
   and then found the input stream contains invalid character?

   The strategy is to optimize for the common case.
*/

struct ScmPortRec {
    SCM_HEADER;
    char direction;
    char type;
    char bufcnt;                /* # of bytes in the incomplete buffer */
    char buf[SCM_CHAR_MAX_BYTES]; /* incomplete buffer */
    
    ScmChar ungotten;           /* ungotten character */

    union {
        struct ScmFilePort {
            FILE *fp;
            int line;
            int column;
            ScmObj name;        /* ScmString if it has name */
        } file;
        struct ScmIStrPort {
            const char *start;
            int rest;
            const char *current;
        } istr;
        ScmDString ostr;
        struct ScmProcPort {
            struct ScmPortVTableRec *vtable;
            void *clientData;
        } proc;
    } src;
};

typedef struct ScmPortVTableRec {
    ScmByte   (*Getb)(void *);
    ScmChar   (*Getc)(void *);
    ScmString (*Gets)(void *);
    int       (*Ready)(void *);
    int       (*Putb)(void *, ScmByte);
    int       (*Putc)(void *, ScmChar);
    int       (*Putcstr)(void *, const char *);
    int       (*Puts)(void *, ScmString *);
    int       (*Close)(void *);
} ScmPortVTable;

enum ScmPortDirection {
    SCM_PORT_INPUT = 1,
    SCM_PORT_OUTPUT = 2
};

enum ScmPortType {
    SCM_PORT_FILE,
    SCM_PORT_ISTR,
    SCM_PORT_OSTR,
    SCM_PORT_PROC,
    SCM_PORT_CLOSED
};

#define SCM_PORTP(obj)      (SCM_XTYPEP(obj, SCM_CLASS_PORT))

#define SCM_PORT(obj)       ((ScmPort *)(obj))
#define SCM_PORT_TYPE(obj)  (SCM_PORT(obj)->type)
#define SCM_PORT_DIR(obj)   (SCM_PORT(obj)->direction)
#define SCM_PORT_UNGOTTEN(obj)  (SCM_PORT(obj)->ungotten)

#define SCM_PORT_CLOSED_P(obj)  (SCM_PORT_TYPE(obj) == SCM_PORT_CLOSED)

#define SCM_IPORTP(obj)  (SCM_PORTP(obj)&&(SCM_PORT_DIR(obj)&SCM_PORT_INPUT))
#define SCM_OPORTP(obj)  (SCM_PORTP(obj)&&(SCM_PORT_DIR(obj)&SCM_PORT_OUTPUT))

extern ScmClass Scm_PortClass;
#define SCM_CLASS_PORT      (&Scm_PortClass)

extern ScmObj Scm_Stdin(void);
extern ScmObj Scm_Stdout(void);
extern ScmObj Scm_Stderr(void);

extern ScmObj Scm_OpenFilePort(const char *path, const char *mode);
extern ScmObj Scm_MakeFilePort(FILE *fp, ScmObj name, const char *mode);

extern ScmObj Scm_MakeInputStringPort(ScmString *str);
extern ScmObj Scm_MakeOutputStringPort(void);
extern ScmObj Scm_GetOutputString(ScmPort *port);

extern ScmObj Scm_MakeVirtualPort(int direction,
                                  ScmPortVTable *vtable,
                                  void *clientData);

extern ScmObj Scm_PortName(ScmPort *port);
extern int    Scm_PortLine(ScmPort *port);
extern int    Scm_PortPosition(ScmPort *port);

extern ScmObj Scm_ClosePort(ScmPort *port);

extern void Scm_Putb(ScmByte b, ScmPort *port);
extern void Scm_Putc(ScmChar c, ScmPort *port);
extern void Scm_Puts(ScmString *s, ScmPort *port);
extern void Scm_PutCStr(const char *s, ScmPort *port);
extern void Scm_Putnl(ScmPort *port);
extern void Scm_Flush(ScmPort *port);

extern void Scm_Ungetc(ScmChar ch, ScmPort *port);
extern int Scm_Getb(ScmPort *port);
extern int Scm_Getc(ScmPort *port);

#define SCM_CURRENT_INPUT_PORT    SCM_VM_CURRENT_INPUT_PORT(Scm_VM())
#define SCM_CURRENT_OUTPUT_PORT   SCM_VM_CURRENT_OUTPUT_PORT(Scm_VM())
#define SCM_CURRENT_ERROR_PORT    SCM_VM_CURRENT_ERROR_PORT(Scm_VM())

/* Inlined operation for better performance.  Assuming the port is
   confirmed as input/output port. */

/* output */

#define SCM__FILE_PUTB(b, port) \
    putc(b, SCM_PORT(port)->src.file.fp)
#define SCM__FILE_PUTC(c, port)                         \
    do { char buf_PORT[SCM_CHAR_MAX_BYTES];             \
         SCM_STR_PUTC(buf_PORT, c);                     \
         fwrite(buf_PORT, 1, SCM_CHAR_NBYTES(c),        \
                SCM_PORT(port)->src.file.fp);           \
    } while(0)
#define SCM__FILE_PUTCSTR(s, port) \
    fputs(s, SCM_PORT(port)->src.file.fp)
#define SCM__FILE_PUTS(s, port)                 \
    fwrite(SCM_STRING_START(s), 1,              \
           SCM_STRING_SIZE(s),                  \
           SCM_PORT(port)->src.file.fp)
#define SCM__FILE_FLUSH(port) \
    fflush(SCM_PORT(port)->src.file.fp)

#define SCM__OSTR_PUTB(b, port)                         \
    SCM_DSTRING_PUTB(&SCM_PORT(port)->src.ostr, b)
#define SCM__OSTR_PUTC(c, port)                         \
    SCM_DSTRING_PUTC(&SCM_PORT(port)->src.ostr, c)
#define SCM__OSTR_PUTCSTR(s, port)                      \
    Scm_DStringPutCstr(&SCM_PORT(port)->src.ostr, s)
#define SCM__OSTR_PUTS(s, port)                         \
    Scm_DStringAdd(&SCM_PORT(port)->src.ostr, SCM_STRING(s))

#define SCM__PROC_PUTB(b, port)                         \
    SCM_PORT(port)->src.proc.vtable->Putb(SCM_PORT(port), b)
#define SCM__PROC_PUTC(c, port)                         \
    SCM_PORT(port)->src.proc.vtable->Putc(SCM_PORT(port), c)
#define SCM__PROC_PUTCSTR(s, port)                      \
    SCM_PORT(port)->src.proc.vtable->Putcstr(SCM_PORT(port), s)
#define SCM__PROC_PUTS(s, port)                           \
    SCM_PORT(port)->src.proc.vtable->Puts(SCM_PORT(port), \
                                          SCM_STRING(s))

#define SCM_PUTB(byte, port)                                            \
    do {                                                                \
        switch (SCM_PORT_TYPE(port)) {                                  \
          case SCM_PORT_FILE: SCM__FILE_PUTB(byte, port); break;        \
          case SCM_PORT_OSTR: SCM__OSTR_PUTB(byte, port); break;        \
          case SCM_PORT_PROC: SCM__PROC_PUTB(byte, port); break;        \
          case SCM_PORT_CLOSED:                                         \
            Scm_Error("port already closed: %S", SCM_OBJ(port));        \
          default: Scm_Panic("SCM_PUTB: something screwed up");         \
        }                                                               \
    } while (0)

#define SCM_PUTC(ch, port)                                              \
    do {                                                                \
        switch (SCM_PORT_TYPE(port)) {                                  \
          case SCM_PORT_FILE: SCM__FILE_PUTC(ch, port); break;          \
          case SCM_PORT_OSTR: SCM__OSTR_PUTC(ch, port); break;          \
          case SCM_PORT_PROC: SCM__PROC_PUTC(ch, port); break;          \
          case SCM_PORT_CLOSED:                                         \
            Scm_Error("port already closed: %S", SCM_OBJ(port));        \
          default: Scm_Panic("SCM_PUTC: something screwed up");         \
        }                                                               \
    } while (0)

#define SCM_PUTCSTR(str, port)                                          \
    do {                                                                \
        switch (SCM_PORT_TYPE(port)) {                                  \
          case SCM_PORT_FILE: SCM__FILE_PUTCSTR(str, port); break;      \
          case SCM_PORT_OSTR: SCM__OSTR_PUTCSTR(str, port); break;      \
          case SCM_PORT_PROC: SCM__PROC_PUTCSTR(str, port); break;      \
          case SCM_PORT_CLOSED:                                         \
            Scm_Error("port already closed: %S", SCM_OBJ(port));        \
          default: Scm_Panic("SCM_PUTCSTR: something screwed up");      \
        }                                                               \
    } while (0)

#define SCM_PUTS(str, port)                                             \
    do {                                                                \
        switch (SCM_PORT_TYPE(port)) {                                  \
          case SCM_PORT_FILE: SCM__FILE_PUTS(str, port); break;         \
          case SCM_PORT_OSTR: SCM__OSTR_PUTS(str, port); break;         \
          case SCM_PORT_PROC: SCM__PROC_PUTS(str, port); break;         \
          case SCM_PORT_CLOSED:                                         \
            Scm_Error("port already closed: %S", SCM_OBJ(port));        \
          default: Scm_Panic("SCM_PUTS: something screwed up");         \
        }                                                               \
    } while (0)

#define SCM_FLUSH(port)                                                 \
    do {                                                                \
        switch (SCM_PORT_TYPE(port)) {                                  \
          case SCM_PORT_FILE: SCM__FILE_FLUSH(port); break;             \
          case SCM_PORT_OSTR: break;                                    \
          case SCM_PORT_PROC: break;                                    \
          case SCM_PORT_CLOSED: break;                                  \
          default: Scm_Panic("SCM_FLUSH: something screwed up");        \
        }                                                               \
    } while (0)

#define SCM_PUTNL(port)      SCM_PUTC('\n', port)

/* input */

/* only one-char unget is supported */
#define SCM_UNGETC(c, port)      (SCM_PORT(port)->ungotten = (c))

#define SCM__FILE_GETB(b, port)                                 \
    do {                                                        \
        if ((b = getc(SCM_PORT(port)->src.file.fp)) == '\n') {  \
            SCM_PORT(port)->src.file.line++;                    \
            SCM_PORT(port)->src.file.column = 0;                \
        } else {                                                \
            SCM_PORT(port)->src.file.column++;                  \
        }                                                       \
    } while(0)
    
#define SCM__FILE_GETC(c, port)                                         \
    do {                                                                \
        int nbytes_SCM_GETC;                                            \
        c = getc(SCM_PORT(port)->src.file.fp);                          \
        SCM_PORT(port)->src.file.column++;                              \
        if (c != EOF && (nbytes_SCM_GETC = SCM_CHAR_NFOLLOWS(c))) {     \
            c = Scm__PortFileGetc(c, SCM_PORT(port));                   \
            SCM_PORT(port)->src.file.column += nbytes_SCM_GETC;         \
        } else if (c == '\n') {                                         \
            SCM_PORT(port)->src.file.line++;                            \
            SCM_PORT(port)->src.file.column = 0;                        \
        }                                                               \
    } while (0)

extern int Scm__PortFileGetc(int prefetch, ScmPort *port);

#define SCM__ISTR_GETB(b, port)                                 \
    ((b) =                                                      \
      ((SCM_PORT(port)->src.istr.rest <= 0) ?                   \
        EOF :                                                   \
        (ScmByte)(*SCM_PORT(port)->src.istr.current++)))
#define SCM__ISTR_GETC(c, port)                                 \
    do {                                                        \
       if (SCM_PORT(port)->src.istr.rest <= 0) {                \
           (c) = EOF;                                           \
       } else {                                                 \
           const char *cp = SCM_PORT(port)->src.istr.current;   \
           unsigned char uc = (unsigned char)*cp;               \
           int siz = SCM_CHAR_NFOLLOWS(uc);                     \
           if (SCM_PORT(port)->src.istr.rest < siz) {           \
               (c) = EOF;                                       \
           } else {                                             \
               SCM_STR_GETC(cp, c);                             \
           }                                                    \
           SCM_PORT(port)->src.istr.current += siz + 1;         \
           SCM_PORT(port)->src.istr.rest -= siz + 1;            \
       }                                                        \
    } while (0)

#define SCM__PROC_GETB(b, port) \
    ((b) = SCM_PORT(port)->src.proc.vtable->Getb(SCM_PORT(port)))
#define SCM__PROC_GETC(c, port) \
    ((c) = SCM_PORT(port)->src.proc.vtable->Getc(SCM_PORT(port)))

#define SCM_GETB(var, port)                                             \
    do {                                                                \
        if (SCM_PORT(port)->ungotten != SCM_CHAR_INVALID                \
            || SCM_PORT(port)->bufcnt != 0) {                           \
            (var) = Scm__PortGetbInternal(SCM_PORT(port));              \
        } else {                                                        \
            switch (SCM_PORT_TYPE(port)) {                              \
              case SCM_PORT_FILE: SCM__FILE_GETB(var, port); break;     \
              case SCM_PORT_ISTR: SCM__ISTR_GETB(var, port); break;     \
              case SCM_PORT_PROC: SCM__PROC_GETB(var, port); break;     \
              case SCM_PORT_CLOSED:                                     \
                Scm_Error("port already closed: %S", SCM_OBJ(port));    \
              default: Scm_Panic("SCM_GETB: something screwed up");     \
            }                                                           \
        }                                                               \
    } while (0)

extern int Scm__PortGetbInternal(ScmPort *port);

#define SCM_GETC(var, port)                                             \
    do {                                                                \
        if (SCM_PORT(port)->ungotten != SCM_CHAR_INVALID) {             \
            var = SCM_PORT(port)->ungotten;                             \
            SCM_PORT(port)->ungotten = SCM_CHAR_INVALID;                \
        } else if (SCM_PORT(port)->bufcnt != 0) {                       \
            var = Scm__PortGetcInternal(SCM_PORT(port));                \
        } else {                                                        \
            switch (SCM_PORT_TYPE(port)) {                              \
              case SCM_PORT_FILE: SCM__FILE_GETC(var, port); break;     \
              case SCM_PORT_ISTR: SCM__ISTR_GETC(var, port); break;     \
              case SCM_PORT_PROC: SCM__PROC_GETC(var, port); break;     \
              case SCM_PORT_CLOSED:                                     \
                Scm_Error("port already closed: %S", SCM_OBJ(port));    \
              default: Scm_Panic("SCM_GETC: something screwed up");     \
            }                                                           \
        }                                                               \
    } while (0)

extern int Scm__PortGetcInternal(ScmPort *port);

/*--------------------------------------------------------
 * WRITE
 */

/* Print mode flags */
enum {
    SCM_PRINT_WRITE,            /* write mode   */
    SCM_PRINT_DISPLAY,          /* display mode */
    SCM_PRINT_DEBUG,            /* debug mode   */
    SCM_PRINT_SCAN              /* this mode of call is only initiated
                                   by Scm_WriteStar (write*).
                                */
};

#define SCM_PRINT_MODE(mode)     ((mode)&0x0f)

typedef struct ScmWriteInfoRec ScmWriteInfo;

extern int Scm_Write(ScmObj obj, ScmObj port, int mode);
extern int Scm_WriteLimited(ScmObj obj, ScmObj port, int mode, int width);
extern ScmObj Scm_Format(ScmObj port, ScmString *fmt, ScmObj args);
extern ScmObj Scm_Cformat(ScmObj port, const char *fmt, ...);
extern int Scm_Printf(ScmPort *port, const char *fmt, ...);
extern int Scm_Vprintf(ScmPort *port, const char *fmt, va_list args);

/* Convenient for debug */
#define SCM_DBGPRINT(form)                                      \
    (Scm_Write(form, Scm_Stderr(), SCM_PRINT_WRITE),            \
     Scm_Putc('\n', SCM_PORT(Scm_Stderr())),                    \
     form)

/*---------------------------------------------------------
 * READ
 */
extern ScmObj Scm_Read(ScmObj port);

/*--------------------------------------------------------
 * IO
 */

extern void Scm_CallWithFile(ScmString *path, ScmProcedure *proc, int inputp);

/*--------------------------------------------------------
 * FILE
 */

extern ScmObj Scm_ParsePathname(const char *pathname,
                                int makeAbsolute,
                                int normalize);

/*--------------------------------------------------------
 * HASHTABLE
 */

typedef struct ScmHashEntryRec ScmHashEntry;

typedef ScmHashEntry *(*ScmHashAccessProc)(ScmHashTable*,
                                           ScmObj, int, ScmObj);
typedef unsigned long (*ScmHashProc)(ScmObj);
typedef int (*ScmHashCmpProc)(ScmObj, ScmHashEntry *);

struct ScmHashTableRec {
    SCM_HEADER;
    ScmHashEntry **buckets;
    int numBuckets;
    int numEntries;
    int maxChainLength;
    int type;
    int mask;
    ScmHashAccessProc accessfn;
    ScmHashProc hashfn;
    ScmHashCmpProc cmpfn;
};

#define SCM_HASHTABLE(obj)   ((ScmHashTable*)(obj))
#define SCM_HASHTABLEP(obj)  SCM_XTYPEP(obj, SCM_CLASS_HASHTABLE)

extern ScmClass Scm_HashTableClass;
#define SCM_CLASS_HASHTABLE  (&Scm_HashTableClass)

#define SCM_HASH_DEFAULT   ((ScmHashProc)0)
#define SCM_HASH_STRING    ((ScmHashProc)1)
#define SCM_HASH_SMALLINT  ((ScmHashProc)2)
#define SCM_HASH_ADDRESS   ((ScmHashProc)3)

/* auxiliary structure; not an ScmObj. */
struct ScmHashEntryRec {
    ScmObj key;
    ScmObj value;
    struct ScmHashEntryRec *next;
};

typedef  struct ScmHashIterRec {
    ScmHashTable *table;
    int currentBucket;
    ScmHashEntry *currentEntry;
} ScmHashIter;

extern ScmObj Scm_MakeHashTable(ScmHashProc hashfn,
                                ScmHashCmpProc cmpfn,
                                unsigned int initSize);
extern ScmObj Scm_CopyHashTable(ScmHashTable *tab);

extern ScmHashEntry *Scm_HashTableGet(ScmHashTable *hash, ScmObj key);
extern ScmHashEntry *Scm_HashTableAdd(ScmHashTable *hash,
                                      ScmObj key, ScmObj value);
extern ScmHashEntry *Scm_HashTablePut(ScmHashTable *hash,
                                      ScmObj key, ScmObj value);
extern ScmHashEntry *Scm_HashTableDelete(ScmHashTable *hash, ScmObj key);

extern void Scm_HashIterInit(ScmHashTable *hash, ScmHashIter *iter);
extern ScmHashEntry *Scm_HashIterNext(ScmHashIter *iter);

/*--------------------------------------------------------
 * MODULE
 */

struct ScmModuleRec {
    SCM_HEADER;
    ScmString *name;
    ScmObj parents;
    ScmHashTable *table;
};

#define SCM_MODULE(obj)       ((ScmModule*)(obj))
#define SCM_MODULEP(obj)      SCM_XTYPEP(obj, SCM_CLASS_MODULE)

extern ScmClass Scm_ModuleClass;
#define SCM_CLASS_MODULE     (&Scm_ModuleClass)

extern ScmGloc *Scm_FindBinding(ScmModule *module, ScmSymbol *symbol,
                                int stay_in_module);
extern ScmObj Scm_MakeModule(ScmString *name, ScmObj parentList);
extern ScmObj Scm_SymbolValue(ScmModule *module, ScmSymbol *symbol);
extern ScmObj Scm_Define(ScmModule *module, ScmSymbol *symbol, ScmObj value);
extern ScmObj Scm_GlobalSet(ScmModule *module, ScmSymbol *symbol, ScmObj value);

extern ScmModule *Scm_SchemeModule(void);
extern ScmModule *Scm_UserModule(void);

#define SCM_DEFINE(module, cstr, val)                                     \
    Scm_Define(SCM_MODULE(module),                                        \
               SCM_SYMBOL(Scm_Intern(SCM_STRING(Scm_MakeStringConst(cstr, -1, -1)))), \
               SCM_OBJ(val))

/*--------------------------------------------------------
 * SYMBOL
 */

struct ScmSymbolRec {
    SCM_HEADER;
    ScmString *name;
};

#define SCM_SYMBOL(obj)        ((ScmSymbol*)(obj))
#define SCM_SYMBOLP(obj)       SCM_XTYPEP(obj, SCM_CLASS_SYMBOL)
#define SCM_SYMBOL_NAME(obj)   (SCM_SYMBOL(obj)->name)

extern ScmObj Scm_Intern(ScmString *name);
#define SCM_INTERN(cstr)       Scm_Intern(SCM_STRING(SCM_MAKE_STR(cstr)))

extern ScmObj Scm_Gensym(ScmString *prefix);

extern ScmClass Scm_SymbolClass;
#define SCM_CLASS_SYMBOL        (&Scm_SymbolClass)

/* predefined symbols */
extern ScmSymbol ScmQquote;
extern ScmSymbol ScmQquasiquote;
extern ScmSymbol ScmQunquote;
extern ScmSymbol ScmQunquoteSplicing;
extern ScmSymbol ScmQdefine;
extern ScmSymbol ScmQlambda;
extern ScmSymbol ScmQif;
extern ScmSymbol ScmQset;
extern ScmSymbol ScmQlet;
extern ScmSymbol ScmQletStar;
extern ScmSymbol ScmQletrec;
extern ScmSymbol ScmQbegin;
extern ScmSymbol ScmQwhen;
extern ScmSymbol ScmQunless;
extern ScmSymbol ScmQand;
extern ScmSymbol ScmQor;
extern ScmSymbol ScmQcond;
extern ScmSymbol ScmQcase;
extern ScmSymbol ScmQelse;
extern ScmSymbol ScmQyields;
extern ScmSymbol ScmQdo;
extern ScmSymbol ScmQdelay;

extern ScmSymbol ScmQcons;
extern ScmSymbol ScmQcar;
extern ScmSymbol ScmQcdr;
extern ScmSymbol ScmQlist;
extern ScmSymbol ScmQeq;
extern ScmSymbol ScmQeqv;
extern ScmSymbol ScmQequal;
extern ScmSymbol ScmQmemv;

#define SCM_SYM_QUOTE            SCM_OBJ(&ScmQquote)
#define SCM_SYM_QUASIQUOTE       SCM_OBJ(&ScmQquasiquote)
#define SCM_SYM_UNQUOTE          SCM_OBJ(&ScmQunquote)
#define SCM_SYM_UNQUOTE_SPLICING SCM_OBJ(&ScmQunquoteSplicing)
#define SCM_SYM_DEFINE           SCM_OBJ(&ScmQdefine)
#define SCM_SYM_LAMBDA           SCM_OBJ(&ScmQlambda)
#define SCM_SYM_IF               SCM_OBJ(&ScmQif)
#define SCM_SYM_SET              SCM_OBJ(&ScmQset)
#define SCM_SYM_LET              SCM_OBJ(&ScmQlet)
#define SCM_SYM_LET_STAR         SCM_OBJ(&ScmQletStar)
#define SCM_SYM_LETREC           SCM_OBJ(&ScmQletrec)
#define SCM_SYM_BEGIN            SCM_OBJ(&ScmQbegin)
#define SCM_SYM_WHEN             SCM_OBJ(&ScmQwhen)
#define SCM_SYM_UNLESS           SCM_OBJ(&ScmQunless)
#define SCM_SYM_AND              SCM_OBJ(&ScmQand)
#define SCM_SYM_OR               SCM_OBJ(&ScmQor)
#define SCM_SYM_COND             SCM_OBJ(&ScmQcond)
#define SCM_SYM_CASE             SCM_OBJ(&ScmQcase)
#define SCM_SYM_ELSE             SCM_OBJ(&ScmQelse)
#define SCM_SYM_YIELDS           SCM_OBJ(&ScmQyields) /* => */
#define SCM_SYM_DO               SCM_OBJ(&ScmQdo)
#define SCM_SYM_DELAY            SCM_OBJ(&ScmQdelay)

#define SCM_SYM_CONS             SCM_OBJ(&ScmQcons)
#define SCM_SYM_CAR              SCM_OBJ(&ScmQcar)
#define SCM_SYM_CDR              SCM_OBJ(&ScmQcdr)
#define SCM_SYM_LIST             SCM_OBJ(&ScmQlist)
#define SCM_SYM_EQ               SCM_OBJ(&ScmQeq)
#define SCM_SYM_EQV              SCM_OBJ(&ScmQeqv)
#define SCM_SYM_EQUAL            SCM_OBJ(&ScmQequal)
#define SCM_SYM_MEMV             SCM_OBJ(&ScmQmemv)

/* Gloc (global location) */
struct ScmGlocRec {
    SCM_HEADER;
    ScmSymbol *name;
    ScmModule *module;
    ScmObj value;
};

#define SCM_GLOC(obj)            ((ScmGloc*)(obj))
#define SCM_GLOCP(obj)           SCM_XTYPEP(obj, SCM_CLASS_GLOC)

extern ScmObj Scm_MakeGloc(ScmSymbol *sym, ScmModule *module);
extern void Scm__GlocPrint(ScmObj obj, ScmPort *port, int mode);

extern ScmClass Scm_GlocClass;
#define SCM_CLASS_GLOC          (&Scm_GlocClass)

/*--------------------------------------------------------
 * NUMBER
 */

#define SCM_SMALL_INT_MAX          ((1L << 29) - 1)
#define SCM_SMALL_INT_MIN          (-SCM_SMALL_INT_MAX-1)

#define SCM_INTEGERP(obj)          (SCM_INTP(obj) || SCM_BIGNUMP(obj))
#define SCM_REALP(obj)             (SCM_INTEGERP(obj)||SCM_FLONUMP(obj))
#define SCM_NUMBERP(obj)           (SCM_REALP(obj)||SCM_COMPLEXP(obj))
#define SCM_EXACTP(obj)            SCM_INTEGERP(obj)
#define SCM_INEXACTP(obj)          (SCM_FLONUMP(obj)||SCM_COMPLEXP(obj))

extern ScmClass  Scm_NumberClass;
extern ScmClass  Scm_ComplexClass;
extern ScmClass  Scm_RealClass;
extern ScmClass  Scm_IntegerClass;
#define SCM_CLASS_NUMBER     (&Scm_NumberClass)
#define SCM_CLASS_COMPLEX    (&Scm_ComplexClass)
#define SCM_CLASS_REAL       (&Scm_RealClass)
#define SCM_CLASS_INTEGER    (&Scm_IntegerClass)

struct ScmBignumRec {
    SCM_HEADER;
    char sign;
    short size;                 /* length of values vector */
    long values[1];             /* values[0] is the least significant */
};

#define SCM_BIGNUM(obj)            ((ScmBignum*)(obj))
#define SCM_BIGNUMP(obj)           0 /* for now */

extern ScmObj Scm_MakeBignum(int sign, int size, long *values);
extern ScmObj Scm_IntToBignum(long value, int size);

struct ScmFlonumRec {
    SCM_HEADER;
    double value;
};

#define SCM_FLONUM(obj)            ((ScmFlonum*)(obj))
#define SCM_FLONUMP(obj)           SCM_XTYPEP(obj, SCM_CLASS_REAL)
#define SCM_FLONUM_VALUE(obj)      (SCM_FLONUM(obj)->value)

struct ScmComplexRec {
    SCM_HEADER;
    double real;
    double imag;
};

#define SCM_COMPLEX(obj)           ((ScmComplex*)(obj))
#define SCM_COMPLEXP(obj)          SCM_XTYPEP(obj, SCM_CLASS_COMPLEX)
#define SCM_COMPLEX_REAL(obj)      SCM_COMPLEX(obj)->real
#define SCM_COMPLEX_IMAG(obj)      SCM_COMPLEX(obj)->imag

extern ScmObj Scm_MakeInteger(long i);
extern long Scm_GetInteger(ScmObj obj);

extern ScmObj Scm_MakeFlonum(double d);
extern double Scm_GetDouble(ScmObj obj);

extern ScmObj Scm_MakeComplex(double real, double imag);

extern ScmObj Scm_NumberP(ScmObj obj);
extern ScmObj Scm_IntegerP(ScmObj obj);
extern ScmObj Scm_Abs(ScmObj obj);
extern int    Scm_Sign(ScmObj obj);
extern ScmObj Scm_Negate(ScmObj obj);
extern ScmObj Scm_Reciprocal(ScmObj obj);
extern ScmObj Scm_ExactToInexact(ScmObj obj);
extern ScmObj Scm_InexactToExact(ScmObj obj);

extern ScmObj Scm_Add(ScmObj args);
extern ScmObj Scm_Subtract(ScmObj arg1, ScmObj arg2, ScmObj args);
extern ScmObj Scm_Multiply(ScmObj args);
extern ScmObj Scm_Divide(ScmObj arg1, ScmObj arg2, ScmObj args);

extern ScmObj Scm_Quotient(ScmObj arg1, ScmObj arg2);
extern ScmObj Scm_Remainder(ScmObj arg1, ScmObj arg2);
extern ScmObj Scm_Modulo(ScmObj arg1, ScmObj arg2, int remainder);

extern ScmObj Scm_NumEq(ScmObj arg0, ScmObj arg1, ScmObj args); /* = */
extern ScmObj Scm_NumLt(ScmObj arg0, ScmObj arg1, ScmObj args); /* < */
extern ScmObj Scm_NumLe(ScmObj arg0, ScmObj arg1, ScmObj args); /* <= */
extern ScmObj Scm_NumGt(ScmObj arg0, ScmObj arg1, ScmObj args); /* > */
extern ScmObj Scm_NumGe(ScmObj arg0, ScmObj arg1, ScmObj args); /* >= */

extern ScmObj Scm_Max(ScmObj arg0, ScmObj args);
extern ScmObj Scm_Min(ScmObj arg0, ScmObj args);

enum {
    SCM_ROUND_FLOOR,
    SCM_ROUND_CEIL,
    SCM_ROUND_TRUNC,
    SCM_ROUND_ROUND
};
extern ScmObj Scm_Round(ScmObj num, int mode);

extern ScmObj Scm_Exp(ScmObj z);
extern ScmObj Scm_Log(ScmObj z);
extern ScmObj Scm_Sin(ScmObj z);
extern ScmObj Scm_Cos(ScmObj z);
extern ScmObj Scm_Tan(ScmObj z);
extern ScmObj Scm_Asin(ScmObj z);
extern ScmObj Scm_Acos(ScmObj z);
extern ScmObj Scm_Atan(ScmObj z);
extern ScmObj Scm_Atan2(ScmObj y, ScmObj x);
extern ScmObj Scm_Sqrt(ScmObj z);
extern ScmObj Scm_Expt(ScmObj z1, ScmObj z2);

extern ScmObj Scm_Magnitude(ScmObj z);
extern ScmObj Scm_Angle(ScmObj z);

extern ScmObj Scm_NumberToString(ScmObj num, int radix);
extern ScmObj Scm_StringToNumber(ScmString *str, int radix);

/*--------------------------------------------------------
 * PROCEDURE
 */

struct ScmProcedureRec {
    SCM_HEADER;
    unsigned char required;     /* # of required args */
    unsigned char optional;     /* 1 if it takes rest args */
    ScmObj info;                /* source code info */
};

#define SCM_PROCEDURE(obj)          ((ScmProcedure*)(obj))
#define SCM_PROCEDURE_REQUIRED(obj) SCM_PROCEDURE(obj)->required
#define SCM_PROCEDURE_OPTIONAL(obj) SCM_PROCEDURE(obj)->optional
#define SCM_PROCEDURE_INFO(obj)     SCM_PROCEDURE(obj)->info

extern ScmClass Scm_ProcedureClass;
#define SCM_CLASS_PROCEDURE   (&Scm_ProcedureClass)

#define SCM_PROCEDUREP(obj)         (SCM_SUBRP(obj)||SCM_CLOSUREP(obj))

struct ScmClosureRec {
    ScmProcedure common;
    ScmObj code;                /* compiled code */
    ScmEnvFrame *env;           /* environment */
};

#define SCM_CLOSUREP(obj)          SCM_XTYPEP(obj, SCM_CLASS_CLOSURE)
#define SCM_CLOSURE(obj)           ((ScmClosure*)(obj))

extern ScmClass Scm_ClosureClass;
#define SCM_CLASS_CLOSURE     (&Scm_ClosureClass)

extern ScmObj Scm_MakeClosure(int required, int optional,
                              ScmObj code, ScmEnvFrame *env, ScmObj info);

struct ScmSubrRec {
    ScmProcedure common;
    ScmObj (*func)(ScmObj *, int, void*);
    ScmObj (*inliner)(ScmSubr *, ScmObj, ScmObj, int);
    void *data;
};

#define SCM_SUBRP(obj)             SCM_XTYPEP(obj, SCM_CLASS_SUBR)
#define SCM_SUBR(obj)              ((ScmSubr*)(obj))

#define SCM_SUBR_FUNC(obj)         SCM_SUBR(obj)->func
#define SCM_SUBR_INLINER(obj)      SCM_SUBR(obj)->inliner
#define SCM_SUBR_DATA(obj)         SCM_SUBR(obj)->data

extern ScmClass Scm_SubrClass;
#define SCM_CLASS_SUBR   (&Scm_SubrClass)

extern ScmObj Scm_MakeSubr(ScmObj (*func)(ScmObj*, int, void*),
                           void *data,
                           int required, int optional,
                           ScmObj info);

extern ScmObj Scm_ForEach1(ScmProcedure *proc, ScmObj args);
extern ScmObj Scm_ForEach(ScmProcedure *proc, ScmObj arg1, ScmObj args);
extern ScmObj Scm_Map1(ScmProcedure *proc, ScmObj args);
extern ScmObj Scm_Map(ScmProcedure *proc, ScmObj arg1, ScmObj args);

/*--------------------------------------------------------
 * MACROS AND SYNTAX
 */

typedef ScmObj (*ScmCompileProc)(ScmObj, ScmObj, int, void*);

/* Syntax is a built-in procedure to compile given form. */
struct ScmSyntaxRec {
    SCM_HEADER;
    ScmSymbol *name;            /* for debug */
    ScmCompileProc compiler;
    void *data;
};

#define SCM_SYNTAX(obj)             ((ScmSyntax*)(obj))
#define SCM_SYNTAXP(obj)            SCM_XTYPEP(obj, SCM_CLASS_SYNTAX)

extern ScmClass Scm_SyntaxClass;
#define SCM_CLASS_SYNTAX            (&Scm_SyntaxClass)

extern ScmObj Scm_MakeSyntax(ScmSymbol *name,
                             ScmCompileProc compiler, void *data);


/*--------------------------------------------------------
 * PROMISE
 */

struct ScmPromiseRec {
    SCM_HEADER;
    int forced;
    ScmObj code;
};

extern ScmClass Scm_PromiseClass;
#define SCM_CLASS_PROMISE           (&Scm_PromiseClass)

extern ScmObj Scm_MakePromise(ScmObj code);
extern ScmObj Scm_Force(ScmObj p);

/*--------------------------------------------------------
 * EXCEPTION
 */

struct ScmExceptionRec {
    SCM_HEADER;
    ScmObj data;
};

extern ScmClass Scm_ExceptionClass;
#define SCM_CLASS_EXCEPTION       (&Scm_ExceptionClass)

#define SCM_EXCEPTIONP(obj)       SCM_XTYPEP(obj, SCM_CLASS_EXCEPTION)
#define SCM_EXCEPTION(obj)        ((ScmException*)(obj))
#define SCM_EXCEPTION_DATA(obj)   SCM_EXCEPTION(obj)->data

/* Throwing error */
extern void Scm_Error(const char *msg, ...);
extern ScmObj Scm_SError(ScmObj fmt, ScmObj args);


/*-------------------------------------------------------
 * STUB MACROS
 */
#define SCM_ENTER_SUBR(name)

#define SCM_ARGREF(count)           (SCM_FP[count])
#define SCM_RETURN(value)           return value
#define SCM_CURRENT_MODULE()        (Scm_VM()->module)

/*---------------------------------------------------
 * UTILITY STUFF
 */

/* Program start and termination */

extern void Scm_Init(void);
extern void Scm_Exit(int code);
extern void Scm_Abort(const char *msg);
extern void Scm_Panic(const char *msg, ...);

/* Load */

extern ScmObj Scm_VMLoadFromPort(ScmPort *port);
extern ScmObj Scm_VMLoad(const char *file);
extern void Scm_Load(const char *file);

/* Assertion */

#ifdef GAUCHE_RECKLESS
#define SCM_ASSERT(expr)   /* nothing */
#else

#ifdef __GNUC__

#define SCM_ASSERT(expr)                                                \
    do {                                                                \
        if (!(expr))                                                    \
            Scm_Panic("\"%s\", line %d (%s): Assertion failed: %s",     \
                      __FILE__, __LINE__, __PRETTY_FUNCTION__, #expr);  \
    } while (0)

#else

#define SCM_ASSERT(expr)                                        \
    do {                                                        \
        if (!(expr))                                            \
            Scm_Panic("\"%s\", line %d: Assertion failed: %s",  \
                      __FILE__, __LINE__, #expr);               \
    } while (0)

#endif /* !__GNUC__ */

#endif /* !GAUCHE_RECKLESS */

/* Interrupt handling */



#endif /* GAUCHE_H */
