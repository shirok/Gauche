/*
 * gauche.h - Gauche scheme system header
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: gauche.h,v 1.197 2001-12-21 07:08:56 shirok Exp $
 */

#ifndef GAUCHE_H
#define GAUCHE_H

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdarg.h>
#include <setjmp.h>
#include <limits.h>
#include <gc.h>

#ifdef __cplusplus
extern "C" {
#endif

#include <gauche/config.h>

#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#define SCM_INLINE_MALLOC_PRIMITIVES
#define SCM_VM_STACK_SIZE     10000

/* Some useful macros */

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE (!FALSE)
#endif

#define SCM_CPP_CAT(a, b)   a ## b
#define SCM_CPP_CAT3(a, b, c)  a ## b ## c

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
#define SCM_UNBOUNDP(obj)   ((obj) == SCM_UNBOUND)

/*
 * BOOLEAN
 */
#define SCM_BOOLP(obj)       ((obj) == SCM_TRUE || (obj == SCM_FALSE))
#define	SCM_MAKE_BOOL(obj)   ((obj)? SCM_TRUE:SCM_FALSE)

#define SCM_EQ(x, y)         ((x) == (y))

extern int Scm_EqP(ScmObj x, ScmObj y);
extern int Scm_EqvP(ScmObj x, ScmObj y);
extern int Scm_EqualP(ScmObj x, ScmObj y);

enum {
    SCM_CMP_EQ,
    SCM_CMP_EQV,
    SCM_CMP_EQUAL
};

extern int Scm_EqualM(ScmObj x, ScmObj y, int mode);

/*
 * FIXNUM
 */

#define SCM_INTP(obj)        (SCM_TAG(obj) == 1)
#define SCM_INT_VALUE(obj)   (((signed long int)(obj)) >> 2)
#define SCM_MAKE_INT(obj)    SCM_OBJ(((long)(obj) << 2) + 1)

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
#define	SCM_MAKE_CHAR(ch)       SCM_OBJ((long)((ch) << 3) + 2)

#define SCM_CHAR_INVALID        ((ScmChar)(-1)) /* indicate invalid char */
#if SIZEOF_LONG == 4
#define SCM_CHAR_MAX            (LONG_MAX/8)
#else
#define SCM_CHAR_MAX            (0x1fffffff)
#endif

#define SCM_CHAR_ASCII_P(ch)    ((ch) < 0x80)
#define SCM_CHAR_UPPER_P(ch)    (('A' <= (ch)) && ((ch) <= 'Z'))
#define SCM_CHAR_LOWER_P(ch)    (('a' <= (ch)) && ((ch) <= 'z'))
#define SCM_CHAR_UPCASE(ch)     (SCM_CHAR_LOWER_P(ch)?((ch)-('a'-'A')):(ch))
#define SCM_CHAR_DOWNCASE(ch)   (SCM_CHAR_UPPER_P(ch)?((ch)+('a'-'A')):(ch))

extern int Scm_DigitToInt(ScmChar ch, int radix);
extern ScmChar Scm_IntToDigit(int n, int radix);
extern ScmObj Scm_CharEncodingName(void);
extern const char **Scm_SupportedCharacterEncodings(void);
extern int Scm_SupportedCharacterEncodingP(const char *encoding);

#if   defined(GAUCHE_CHAR_ENCODING_EUC_JP)
#include "gauche/char_euc_jp.h"
#elif defined(GAUCHE_CHAR_ENCODING_UTF_8)
#include "gauche/char_utf_8.h"
#elif defined(GAUCHE_CHAR_ENCODING_SJIS)
#include "gauche/char_sjis.h"
#else
#include "gauche/char_none.h"
#endif

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

#define SCM_MALLOC(size)          GC_MALLOC(size)
#define SCM_MALLOC_ATOMIC(size)   GC_MALLOC_ATOMIC(size)
#define SCM_REALLOC(ptr, size)    GC_REALLOC(ptr, size)

#define SCM_NEW(type)         ((type*)(SCM_MALLOC(sizeof(type))))
#define SCM_NEW2(type, size)  ((type)(SCM_MALLOC(size)))
#define SCM_NEW_ATOMIC(type)  ((type*)(SCM_MALLOC_ATOMIC(sizeof(type))))
#define SCM_NEW_ATOMIC2(type, size) ((type)(SCM_MALLOC_ATOMIC(size)))

#define SCM_SET_CLASS(obj, type)   (SCM_OBJ(obj)->klass = type)

typedef struct ScmVMRec        ScmVM;
typedef struct ScmClassRec     ScmClass;
typedef struct ScmPairRec      ScmPair;
typedef struct ScmCharSetRec   ScmCharSet;
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
typedef struct ScmKeywordRec   ScmKeyword;
typedef struct ScmProcedureRec ScmProcedure;
typedef struct ScmClosureRec   ScmClosure;
typedef struct ScmSubrRec      ScmSubr;
typedef struct ScmGenericRec   ScmGeneric;
typedef struct ScmMethodRec    ScmMethod;
typedef struct ScmNextMethodRec ScmNextMethod;
typedef struct ScmSyntaxRec    ScmSyntax;
typedef struct ScmPromiseRec   ScmPromise;
typedef struct ScmRegexpRec    ScmRegexp;
typedef struct ScmRegMatchRec  ScmRegMatch;
typedef struct ScmExceptionRec ScmException;
typedef struct ScmWriteContextRec ScmWriteContext;

/*---------------------------------------------------------
 * VM STUFF
 */

/* Detailed definitions are in vm.h.  Here I expose external interface */

#include <gauche/vm.h>

#define SCM_VM(obj)          ((ScmVM *)(obj))
#define SCM_VMP(obj)         SCM_XTYPEP(obj, SCM_CLASS_VM)

#define SCM_VM_CURRENT_INPUT_PORT(vm)   (SCM_VM(vm)->curin)
#define SCM_VM_CURRENT_OUTPUT_PORT(vm)  (SCM_VM(vm)->curout)
#define SCM_VM_CURRENT_ERROR_PORT(vm)   (SCM_VM(vm)->curerr)

extern ScmVM *Scm_VM(void);     /* Returns the current VM */

extern ScmObj Scm_Compile(ScmObj form, ScmObj env, int context);
extern ScmObj Scm_CompileBody(ScmObj form, ScmObj env, int context);
extern ScmObj Scm_CompileLookupEnv(ScmObj sym, ScmObj env, int op);
extern ScmObj Scm_CompileInliner(ScmObj form, ScmObj env,
                                 int reqargs, int optargs,
                                 int insn, char *proc);
extern ScmObj Scm_Eval(ScmObj form, ScmObj env);
extern ScmObj Scm_Apply(ScmObj proc, ScmObj args);
extern ScmObj Scm_Values(ScmObj args);
extern ScmObj Scm_Values2(ScmObj val0, ScmObj val1);
extern ScmObj Scm_Values3(ScmObj val0, ScmObj val1, ScmObj val2);
extern ScmObj Scm_Values4(ScmObj val0, ScmObj val1, ScmObj val2, ScmObj val3);
extern ScmObj Scm_Values5(ScmObj val0, ScmObj val1, ScmObj val2, ScmObj val3, ScmObj val4);

extern ScmObj Scm_MakeMacroTransformer(ScmSymbol *name,
                                       ScmProcedure *proc);

extern ScmObj Scm_VMGetResult(ScmVM *vm);
extern ScmObj Scm_VMGetStackLite(ScmVM *vm);
extern ScmObj Scm_VMGetStack(ScmVM *vm);

extern ScmObj Scm_VMApply(ScmObj proc, ScmObj args);
extern ScmObj Scm_VMApply0(ScmObj proc);
extern ScmObj Scm_VMApply1(ScmObj proc, ScmObj arg);
extern ScmObj Scm_VMApply2(ScmObj proc, ScmObj arg1, ScmObj arg2);
extern ScmObj Scm_VMEval(ScmObj expr, ScmObj env);
extern ScmObj Scm_VMCall(ScmObj *args, int argcnt, void *data);

extern ScmObj Scm_VMDynamicWind(ScmObj pre, ScmObj body, ScmObj post);
extern ScmObj Scm_VMDynamicWindC(ScmObj (*before)(ScmObj *, int, void *),
                                 ScmObj (*body)(ScmObj *, int, void *),
                                 ScmObj (*after)(ScmObj *, int, void *),
                                 void *data);

extern ScmObj Scm_VMWithErrorHandler(ScmObj handler, ScmObj thunk);
extern ScmObj Scm_VMWithExceptionHandler(ScmObj handler, ScmObj thunk);
extern ScmObj Scm_VMThrowException(ScmObj exception);

/*---------------------------------------------------------
 * CLASS
 */

/* See class.c for the description of function pointer members. */
struct ScmClassRec {
    SCM_HEADER;
    void (*print)(ScmObj obj, ScmPort *sink, ScmWriteContext *mode);
    int (*compare)(ScmObj x, ScmObj y);
    int (*serialize)(ScmObj obj, ScmPort *sink, ScmObj context);
    ScmObj (*allocate)(ScmClass *klass, ScmObj initargs);
    struct ScmClassRec **cpa;
    short numInstanceSlots;     /* # of instance slots */
    unsigned char instanceSlotOffset;
    unsigned char flags;
    ScmObj name;                /* scheme name */
    ScmObj directSupers;        /* list of classes */
    ScmObj cpl;                 /* list of classes */
    ScmObj accessors;           /* alist of slot-name & slot-accessor */
    ScmObj directSlots;         /* alist of slot-name & slot-definition */
    ScmObj slots;               /* alist of slot-name & slot-definition */
    /* The following slots are for class redefinition protocol,
       not used yet. */
    ScmObj directSubclasses;    /* list of direct subclasses */
    ScmObj directMethods;       /* list of methods that has this class in
                                   its specializer */
    ScmObj redefined;           /* if this class is obsoleted by class
                                   redefinition, points to the new class.
                                   otherwise #f */
};

typedef struct ScmClassStaticSlotSpecRec ScmClassStaticSlotSpec;

#define SCM_CLASS(obj)        ((ScmClass*)(obj))
#define SCM_CLASSP(obj)       SCM_XTYPEP(obj, SCM_CLASS_CLASS)

/* Class flags (bitmask) */
enum {
    SCM_CLASS_BUILTIN = 0x01,   /* true if builtin class */
    SCM_CLASS_FINAL = 0x02,     /* true if the class is final */
    SCM_CLASS_APPLICABLE = 0x04 /* true if the instance is an applicable
                                   object. */
};

#define SCM_CLASS_FLAGS(obj)     (SCM_CLASS(obj)->flags)
#define SCM_CLASS_BUILTIN_P(obj) (SCM_CLASS_FLAGS(obj)&SCM_CLASS_BUILTIN)
#define SCM_CLASS_SCHEME_P(obj)  (!SCM_CLASS_BUILTIN_P(obj))
#define SCM_CLASS_FINAL_P(obj)   (SCM_CLASS_FLAGS(obj)&SCM_CLASS_FINAL)
#define SCM_CLASS_APPLICABLE_P(obj) (SCM_CLASS_FLAGS(obj)&SCM_CLASS_APPLICABLE)

extern void Scm_InitBuiltinClass(ScmClass *c, const char *name,
                                 ScmClassStaticSlotSpec *slots,
                                 int instanceSize, ScmModule *m);

extern ScmClass *Scm_ClassOf(ScmObj obj);
extern int Scm_SubtypeP(ScmClass *sub, ScmClass *type);
extern int Scm_TypeP(ScmObj obj, ScmClass *type);

extern ScmObj Scm_VMSlotRef(ScmObj obj, ScmObj slot, int boundp);
extern ScmObj Scm_VMSlotSet(ScmObj obj, ScmObj slot, ScmObj value);
extern ScmObj Scm_VMSlotBoundP(ScmObj obj, ScmObj slot);

/* built-in classes */
extern ScmClass Scm_TopClass;
extern ScmClass Scm_BoolClass;
extern ScmClass Scm_CharClass;
extern ScmClass Scm_ClassClass;
extern ScmClass Scm_UnknownClass;
extern ScmClass Scm_CollectionClass;
extern ScmClass Scm_SequenceClass;
extern ScmClass Scm_ObjectClass; /* base of Scheme-defined objects */

#define SCM_CLASS_TOP          (&Scm_TopClass)
#define SCM_CLASS_BOOL         (&Scm_BoolClass)
#define SCM_CLASS_CHAR         (&Scm_CharClass)
#define SCM_CLASS_CLASS        (&Scm_ClassClass)
#define SCM_CLASS_UNKNOWN      (&Scm_UnknownClass)
#define SCM_CLASS_COLLECTION   (&Scm_CollectionClass)
#define SCM_CLASS_SEQUENCE     (&Scm_SequenceClass)
#define SCM_CLASS_OBJECT       (&Scm_ObjectClass)

extern ScmClass *Scm_DefaultCPL[];
extern ScmClass *Scm_CollectionCPL[];
extern ScmClass *Scm_SequenceCPL[];
extern ScmClass *Scm_ObjectCPL[];

#define SCM_CLASS_DEFAULT_CPL     (Scm_DefaultCPL)
#define SCM_CLASS_COLLECTION_CPL  (Scm_CollectionCPL)
#define SCM_CLASS_SEQUENCE_CPL    (Scm_SequenceCPL)
#define SCM_CLASS_OBJECT_CPL      (Scm_ObjectCPL)

/* Static definition of classes
 *   SCM_DEFINE_BUILTIN_CLASS
 *   SCM_DEFINE_BUILTIN_CLASS_SIMPLE
 *   SCM_DEFINE_BASE_CLASS
 */

/* internal macro. do not use directly */
#define SCM__DEFINE_CLASS_COMMON(cname, size, flag, printer, compare, serialize, allocate, cpa) \
    ScmClass cname = {                          \
        { SCM_CLASS_CLASS },                    \
        printer,                                \
        compare,                                \
        serialize,                              \
        allocate,                               \
        cpa,                                    \
        0,                                      \
        size,                                   \
        flag,                                   \
        SCM_FALSE,                              \
        SCM_FALSE,                              \
        SCM_FALSE,                              \
        SCM_NIL,                                \
        SCM_NIL,                                \
        SCM_NIL,                                \
        SCM_NIL,                                \
        SCM_NIL,                                \
        SCM_FALSE                               \
    }
    
/* Define built-in class statically -- full-featured version */
#define SCM_DEFINE_BUILTIN_CLASS(cname, printer, compare, serialize, allocate, cpa) \
    SCM__DEFINE_CLASS_COMMON(cname, 0,                                    \
                             SCM_CLASS_BUILTIN|SCM_CLASS_FINAL,           \
                             printer, compare, serialize, allocate, cpa)

/* Define built-in class statically -- simpler version */
#define SCM_DEFINE_BUILTIN_CLASS_SIMPLE(cname, printer)         \
    SCM_DEFINE_BUILTIN_CLASS(cname, printer, NULL, NULL, NULL,  \
                             SCM_CLASS_DEFAULT_CPL)

/* define an abstract class */
#define SCM_DEFINE_ABSTRACT_CLASS(cname, cpa)            \
    SCM__DEFINE_CLASS_COMMON(cname, 0,                   \
                             SCM_CLASS_BUILTIN,          \
                             NULL, NULL, NULL, NULL, cpa)

/* define a class that can be subclassed by Scheme */
#define SCM_DEFINE_BASE_CLASS(cname, ctype, printer, compare, serialize, allocate, cpa) \
    SCM__DEFINE_CLASS_COMMON(cname,                                           \
                             (sizeof(ctype)+sizeof(ScmObj)-1)/sizeof(ScmObj), \
                             SCM_CLASS_BUILTIN,                               \
                             printer, compare, serialize, allocate, cpa)

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
extern ScmObj Scm_Acons(ScmObj caar, ScmObj cdar, ScmObj cdr);
extern ScmObj Scm_List(ScmObj elt, ...);
extern ScmObj Scm_Conses(ScmObj elt, ...);
extern ScmObj Scm_VaList(va_list elts);
extern ScmObj Scm_VaCons(va_list elts);
extern ScmObj Scm_ArrayToList(ScmObj *elts, int nelts);
extern ScmObj *Scm_ListToArray(ScmObj list, int *nelts, ScmObj *store, int alloc);

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
extern ScmObj Scm_ListRef(ScmObj list, int i, ScmObj fallback);
extern ScmObj Scm_LastPair(ScmObj list);

extern ScmObj Scm_Memq(ScmObj obj, ScmObj list);
extern ScmObj Scm_Memv(ScmObj obj, ScmObj list);
extern ScmObj Scm_Member(ScmObj obj, ScmObj list, int cmpmode);
extern ScmObj Scm_Assq(ScmObj obj, ScmObj alist);
extern ScmObj Scm_Assv(ScmObj obj, ScmObj alist);
extern ScmObj Scm_Assoc(ScmObj obj, ScmObj alist, int cmpmode);

extern ScmObj Scm_Delete(ScmObj obj, ScmObj list, int cmpmode);
extern ScmObj Scm_DeleteX(ScmObj obj, ScmObj list, int cmpmode);
extern ScmObj Scm_AssocDelete(ScmObj elt, ScmObj alist, int cmpmode);
extern ScmObj Scm_AssocDeleteX(ScmObj elt, ScmObj alist, int cmpmode);

extern ScmObj Scm_DeleteDuplicates(ScmObj list, int cmpmode);
extern ScmObj Scm_DeleteDuplicatesX(ScmObj list, int cmpmode);

extern ScmObj Scm_TopologicalSort(ScmObj edges);
extern ScmObj Scm_MonotonicMerge(ScmObj start, ScmObj sequences,
                                 ScmObj (*get_super)(ScmObj, void*),
                                 void* data);
extern ScmObj Scm_Union(ScmObj list1, ScmObj list2);
extern ScmObj Scm_Intersection(ScmObj list1, ScmObj list2);

extern ScmObj Scm_PairAttrGet(ScmPair *pair, ScmObj key, ScmObj fallback);
extern ScmObj Scm_PairAttrSet(ScmPair *pair, ScmObj key, ScmObj value);

extern ScmObj Scm_NullP(ScmObj obj);
extern ScmObj Scm_ListP(ScmObj obj);

/*--------------------------------------------------------
 * CHAR-SET
 */

#define SCM_CHARSET_MASK_CHARS 128
#define SCM_CHARSET_MASK_SIZE  (SCM_CHARSET_MASK_CHARS/(SIZEOF_LONG*8))

struct ScmCharSetRec {
    SCM_HEADER;
    unsigned long mask[SCM_CHARSET_MASK_SIZE];
    struct ScmCharSetRange {
        struct ScmCharSetRange *next;
        ScmChar lo;             /* lower boundary of range (inclusive) */
        ScmChar hi;             /* higher boundary of range (inclusive) */
    } *ranges;
};

extern ScmClass Scm_CharSetClass;
#define SCM_CLASS_CHARSET  (&Scm_CharSetClass)
#define SCM_CHARSET(obj)   ((ScmCharSet*)obj)
#define SCM_CHARSETP(obj)  SCM_XTYPEP(obj, SCM_CLASS_CHARSET)

#define SCM_CHARSET_SMALLP(obj)  (SCM_CHARSET(obj)->ranges == NULL)

ScmObj Scm_MakeEmptyCharSet(void);
ScmObj Scm_CopyCharSet(ScmCharSet *src);
int    Scm_CharSetEq(ScmCharSet *x, ScmCharSet *y);
ScmObj Scm_CharSetAddRange(ScmCharSet *cs, ScmChar from, ScmChar to);
ScmObj Scm_CharSetAdd(ScmCharSet *dest, ScmCharSet *src);
ScmObj Scm_CharSetComplement(ScmCharSet *cs);
ScmObj Scm_CharSetRanges(ScmCharSet *cs);
ScmObj Scm_CharSetRead(ScmPort *input, int *complement_p,
                       int error_p, int bracket_syntax);

int    Scm_CharSetContains(ScmCharSet *cs, ScmChar c);

/* predefined character set API */
enum {
    SCM_CHARSET_ALNUM,
    SCM_CHARSET_ALPHA,
    SCM_CHARSET_BLANK,
    SCM_CHARSET_CNTRL,
    SCM_CHARSET_DIGIT,
    SCM_CHARSET_GRAPH,
    SCM_CHARSET_LOWER,
    SCM_CHARSET_PRINT,
    SCM_CHARSET_PUNCT,
    SCM_CHARSET_SPACE,
    SCM_CHARSET_UPPER,
    SCM_CHARSET_XDIGIT,
    SCM_CHARSET_NUM_PREDEFINED_SETS
};
ScmObj Scm_GetStandardCharSet(int id);
    
/*--------------------------------------------------------
 * STRING
 */

/* NB: Conceptually, object immutablility is not specific for strings,
 * so the immutable flag has to be in SCM_HEADER or somewhere else.
 * In practical situations, however, what ususally matters is string
 * immutability (like the return value of symbol->string).  So I keep
 * string specific immutable flag.
 */

struct ScmStringRec {
    SCM_HEADER;
    unsigned int incomplete : 1;
    unsigned int immutable : 1;
    unsigned int length : (SIZEOF_INT*CHAR_BIT-2);
    unsigned int size;
    const char *start;
};

#define SCM_STRINGP(obj)        SCM_XTYPEP(obj, SCM_CLASS_STRING)
#define SCM_STRING(obj)         ((ScmString*)(obj))
#define SCM_STRING_LENGTH(obj)  (SCM_STRING(obj)->length)
#define SCM_STRING_SIZE(obj)    (SCM_STRING(obj)->size)
#define SCM_STRING_START(obj)   (SCM_STRING(obj)->start)

#define SCM_STRING_INCOMPLETE_P(obj) (SCM_STRING(obj)->incomplete)
#define SCM_STRING_IMMUTABLE_P(obj)  (SCM_STRING(obj)->immutable)
#define SCM_STRING_SINGLE_BYTE_P(obj) \
    (SCM_STRING_SIZE(obj)==SCM_STRING_LENGTH(obj))

/* constructor flags */
#define SCM_MAKSTR_COPYING     (1L<<0)
#define SCM_MAKSTR_INCOMPLETE  (1L<<1)
#define SCM_MAKSTR_IMMUTABLE   (1L<<2)

#define SCM_MAKE_STR(cstr) \
    Scm_MakeString(cstr, -1, -1, 0)
#define SCM_MAKE_STR_COPYING(cstr) \
    Scm_MakeString(cstr, -1, -1, SCM_MAKSTR_COPYING)
#define SCM_MAKE_STR_IMMUTABLE(cstr) \
    Scm_MakeString(cstr, -1, -1, SCM_MAKSTR_IMMUTABLE)

extern ScmClass Scm_StringClass;
#define SCM_CLASS_STRING        (&Scm_StringClass)

/* grammer spec for StringJoin (see SRFI-13) */
enum {
    SCM_STRING_JOIN_INFIX,
    SCM_STRING_JOIN_STRICT_INFIX,
    SCM_STRING_JOIN_SUFFIX,
    SCM_STRING_JOIN_PREFIX
};

extern int     Scm_MBLen(const char *str, const char *stop);

extern ScmObj  Scm_MakeString(const char *str, int size, int len, int flags);
extern ScmObj  Scm_MakeFillString(int len, ScmChar fill);
extern ScmObj  Scm_CopyString(ScmString *str);

extern char*   Scm_GetString(ScmString *str);
extern const char* Scm_GetStringConst(ScmString *str);

extern ScmObj  Scm_StringMakeImmutable(ScmString *str);
extern ScmObj  Scm_StringCompleteToIncompleteX(ScmString *str);
extern ScmObj  Scm_StringIncompleteToCompleteX(ScmString *str);
extern ScmObj  Scm_StringCompleteToIncomplete(ScmString *str);
extern ScmObj  Scm_StringIncompleteToComplete(ScmString *str);

extern int     Scm_StringCmp(ScmString *x, ScmString *y);
extern int     Scm_StringCiCmp(ScmString *x, ScmString *y);

extern ScmChar Scm_StringRef(ScmString *str, int k);
extern ScmObj  Scm_StringSet(ScmString *str, int k, ScmChar sc);
extern int     Scm_StringByteRef(ScmString *str, int k);
extern ScmObj  Scm_StringByteSet(ScmString *str, int k, ScmByte b);
extern ScmObj  Scm_StringSubstitute(ScmString *target, int start,
                                    ScmString *str);

extern ScmObj  Scm_Substring(ScmString *x, int start, int end);
extern ScmObj  Scm_MaybeSubstring(ScmString *x, ScmObj start, ScmObj end);
extern ScmObj  Scm_StringTake(ScmString *x, int nchars, int takefirst, int fromright);

extern ScmObj  Scm_StringAppend2(ScmString *x, ScmString *y);
extern ScmObj  Scm_StringAppendC(ScmString *x, const char *s, int size, int len);
extern ScmObj  Scm_StringAppend(ScmObj strs);
extern ScmObj  Scm_StringJoin(ScmObj strs, ScmString *delim, int grammer);

extern ScmObj  Scm_StringSplitByChar(ScmString *str, ScmChar ch);
extern ScmObj  Scm_StringScan(ScmString *s1, ScmString *s2, int retmode);
extern ScmObj  Scm_StringScanChar(ScmString *s1, ScmChar ch, int retmode);

/* "retmode" argument for string scan */
enum {
    SCM_STRING_SCAN_INDEX,      /* return index */
    SCM_STRING_SCAN_BEFORE,     /* return substring of s1 before s2 */
    SCM_STRING_SCAN_AFTER,      /* return substring of s1 after s2 */
    SCM_STRING_SCAN_BEFORE2,    /* return substr of s1 before s2 and rest */
    SCM_STRING_SCAN_AFTER2,     /* return substr of s1 up to s2 and rest */
    SCM_STRING_SCAN_BOTH        /* return substr of s1 before and after s2 */
};

extern ScmObj  Scm_StringP(ScmObj obj);
extern ScmObj  Scm_StringToList(ScmString *str);
extern ScmObj  Scm_ListToString(ScmObj chars);
extern ScmObj  Scm_StringFill(ScmString *str, ScmChar c,
                              ScmObj maybeStart, ScmObj maybeEnd);

extern ScmObj Scm_ConstCStringArrayToList(const char **array, int size);
extern ScmObj Scm_CStringArrayToList(char **array, int size);

/* You can allocate a constant string statically, if you calculate
   the length by yourself. */
#define SCM_DEFINE_STRING_CONST(name, str, len, siz)    \
    ScmString name = {                                  \
        { SCM_CLASS_STRING }, 0, 1, (len), (siz), (str) \
    }

/* Auxiliary structure to construct a string.  This is not an ScmObj. */
struct ScmDStringRec {
    char *start;
    char *end;
    char *current;
    int length;
};

extern void        Scm_DStringInit(ScmDString *dstr);
extern ScmObj      Scm_DStringGet(ScmDString *dstr);
extern const char *Scm_DStringGetz(ScmDString *dstr);
extern void        Scm_DStringPutz(ScmDString *dstr, const char *str, int siz);
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
        SCM_CHAR_PUT(d_DSTR->current, ch_DSTR);         \
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

/* Efficient way to access string from Scheme */
typedef struct ScmStringPointerRec {
    SCM_HEADER;
    int length;
    int size;
    const char *start;
    int index;
    const char *current;
} ScmStringPointer;

extern ScmClass Scm_StringPointerClass;
#define SCM_CLASS_STRING_POINTER  (&Scm_StringPointerClass)
#define SCM_STRING_POINTERP(obj)  SCM_XTYPEP(obj, SCM_CLASS_STRING_POINTER)
#define SCM_STRING_POINTER(obj)   ((ScmStringPointer*)obj)

extern ScmObj Scm_MakeStringPointer(ScmString *src, int index, int start, int end);
extern ScmObj Scm_StringPointerNext(ScmStringPointer *sp);
extern ScmObj Scm_StringPointerPrev(ScmStringPointer *sp);
extern ScmObj Scm_StringPointerSet(ScmStringPointer *sp, int index);
extern ScmObj Scm_StringPointerSubstring(ScmStringPointer *sp, int beforep);

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
extern ScmObj Scm_VectorRef(ScmVector *vec, int i, ScmObj fallback);
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

/* Port is the Scheme way of I/O abstraction.  R5RS's definition of
 * of the port is very simple and straightforward.   Practical
 * applications, however, require far more detailed control over
 * the I/O channel, as well as the reasonable performance.
 *
 * Current implementation is a bit messy, trying to achieve both
 * performance and feature requirements.  In the core API level,
 * ports are categorized in one of three types: file ports, string
 * ports and procedural ports.   A port may be an input port or
 * an output port.   A port may handle byte (binary) streams, as
 * well as character streams.  Some port may interchange byte (binary)
 * I/O versus character I/O, while some may signal an error if you
 * mix those operations.
 * (Right now, binary/character mixed I/O is not well supported and
 * contains some serious bugs).
 */
    
struct ScmPortRec {
    SCM_HEADER;
    unsigned char direction;    /* SCM_PORT_INPUT or SCM_PORT_OUTPUT */
    unsigned char type;         /* SCM_PORT_{FILE|ISTR|OSTR|PORT|CLOSED} */
    unsigned char scrcnt;       /* # of bytes in the scratch buffer */

    unsigned int ownerp    : 1; /* TRUE if this ports owns underlying
                                   file pointer */
    unsigned int icpolicy  : 2; /* Policy to handle incomplete characters */
    unsigned int biop      : 1; /* byte/block I/O capable? */

    char scratch[SCM_CHAR_MAX_BYTES]; /* incomplete buffer */
    
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

typedef struct ScmProcPortInfoRec {
    ScmObj name;
    int line;
    int position;
    int fd;
    FILE *fp;
} ScmProcPortInfo;
    
typedef struct ScmPortVTableRec {
    int       (*Getb)(ScmPort *p);
    int       (*Getc)(ScmPort *p);
    int       (*Getz)(char *buf, int buflen, ScmPort *p);
    ScmObj    (*Getline)(ScmPort *p);
    int       (*Ready)(ScmPort *p);
    int       (*Putb)(ScmByte b, ScmPort *p);
    int       (*Putc)(ScmChar c, ScmPort *p);
    int       (*Putz)(const char *buf, int len, ScmPort *p);
    int       (*Puts)(ScmString *s, ScmPort *p);
    int       (*Flush)(ScmPort *p);
    int       (*Close)(ScmPort *p);
    ScmProcPortInfo *(*Info)(ScmPort *p);
} ScmPortVTable;

/* Port direction */
enum ScmPortDirection {
    SCM_PORT_INPUT = 1,
    SCM_PORT_OUTPUT = 2
};

/* Port types */
enum ScmPortType {
    SCM_PORT_FILE,
    SCM_PORT_ISTR,
    SCM_PORT_OSTR,
    SCM_PORT_PROC,
    SCM_PORT_CLOSED
};

/* Incomplete character handling policy.
   When Scm_Getc encounters a byte sequence that doesn't consist a valid
   multibyte character, it may take one of the following actions,
   according to the port's icpolicy field. */
enum ScmPortICPolicy {
    SCM_PORT_IC_ERROR,          /* signal an error */
    SCM_PORT_IC_IGNORE,         /* ignore bytes until Getc finds a
                                   valid multibyte character */
    SCM_PORT_IC_REPLACE,        /* replace invalid byte to a designated
                                   character. */
};

/* Predicates & accessors */
#define SCM_PORTP(obj)          (SCM_XTYPEP(obj, SCM_CLASS_PORT))

#define SCM_PORT(obj)           ((ScmPort *)(obj))
#define SCM_PORT_TYPE(obj)      (SCM_PORT(obj)->type)
#define SCM_PORT_DIR(obj)       (SCM_PORT(obj)->direction)
#define SCM_PORT_FLAGS(obj)     (SCM_PORT(obj)->flags)
#define SCM_PORT_UNGOTTEN(obj)  (SCM_PORT(obj)->ungotten)
#define SCM_PORT_ICPOLICY(obj)  (SCM_PORT(obj)->icpolicy)

#define SCM_PORT_CLOSED_P(obj)  (SCM_PORT_TYPE(obj) == SCM_PORT_CLOSED)
#define SCM_PORT_OWNER_P(obj)   (SCM_PORT(obj)->ownerp)

#define SCM_IPORTP(obj)  (SCM_PORTP(obj)&&(SCM_PORT_DIR(obj)&SCM_PORT_INPUT))
#define SCM_OPORTP(obj)  (SCM_PORTP(obj)&&(SCM_PORT_DIR(obj)&SCM_PORT_OUTPUT))

extern ScmClass Scm_PortClass;
#define SCM_CLASS_PORT      (&Scm_PortClass)

extern ScmObj Scm_Stdin(void);
extern ScmObj Scm_Stdout(void);
extern ScmObj Scm_Stderr(void);

extern ScmObj Scm_OpenFilePort(const char *path, const char *mode);
extern ScmObj Scm_MakeFilePort(FILE *fp, ScmObj name, const char *mode,
                               int ownerp);

extern ScmObj Scm_MakeInputStringPort(ScmString *str);
extern ScmObj Scm_MakeOutputStringPort(void);
extern ScmObj Scm_GetOutputString(ScmPort *port);

extern ScmObj Scm_MakeVirtualPort(int direction,
                                  ScmPortVTable *vtable,
                                  void *clientData, int ownerp);
extern ScmObj Scm_MakeBufferedPort(int direction,
                                   int bufsize,
                                   int chars,
                                   char *buffer,
                                   int (*filler)(char *, int, void *),
                                   void *data);
extern ScmObj Scm_MakePortWithFd(ScmObj name,
                                 int direction,
                                 int fd,
                                 int buffered,
                                 int ownerp);

extern ScmObj Scm_PortName(ScmPort *port);
extern int    Scm_PortLine(ScmPort *port);
extern int    Scm_PortPosition(ScmPort *port);
extern int    Scm_PortFileNo(ScmPort *port);
extern int    Scm_CharReady(ScmPort *port);

extern ScmObj Scm_ClosePort(ScmPort *port);

extern void Scm_Putb(ScmByte b, ScmPort *port);
extern void Scm_Putc(ScmChar c, ScmPort *port);
extern void Scm_Puts(ScmString *s, ScmPort *port);
extern void Scm_Putz(const char *s, int len, ScmPort *port);
extern void Scm_Putnl(ScmPort *port);
extern void Scm_Flush(ScmPort *port);

extern void Scm_Ungetc(ScmChar ch, ScmPort *port);
extern int Scm_Getb(ScmPort *port);
extern int Scm_Getc(ScmPort *port);
extern int Scm_Getz(char *buf, int buflen, ScmPort *port);

extern ScmObj Scm_ReadLine(ScmPort *port);

extern ScmObj Scm_WithPort(ScmPort *port[], ScmProcedure *thunk,
                           int mask, int closep);
#define SCM_PORT_CURIN  (1<<0)
#define SCM_PORT_CUROUT (1<<1)
#define SCM_PORT_CURERR (1<<2)

#define SCM_CURIN    SCM_VM_CURRENT_INPUT_PORT(Scm_VM())
#define SCM_CUROUT   SCM_VM_CURRENT_OUTPUT_PORT(Scm_VM())
#define SCM_CURERR   SCM_VM_CURRENT_ERROR_PORT(Scm_VM())

/* Inlined operations are defined in the separater file */
#include <gauche/portmacros.h>

/*--------------------------------------------------------
 * WRITE
 */

struct ScmWriteContextRec {
    short mode;                 /* print mode */
    short flags;                /* internal */
    int limit;                  /* internal */
    int ncirc;                  /* internal */
    ScmHashTable *table;        /* internal */
};

/* Print mode flags */
enum {
    SCM_WRITE_WRITE,            /* write mode   */
    SCM_WRITE_DISPLAY,          /* display mode */
    SCM_WRITE_DEBUG,            /* debug mode   */
    SCM_WRITE_SCAN              /* this mode of call is only initiated
                                   by Scm_WriteStar (write*).
                                */
};

#define SCM_WRITE_MODE(ctx)   ((ctx)->mode)

extern void Scm_Write(ScmObj obj, ScmObj port, int mode);
extern int Scm_WriteLimited(ScmObj obj, ScmObj port, int mode, int width);
extern ScmObj Scm_Format(ScmObj port, ScmString *fmt, ScmObj args);
extern ScmObj Scm_Cformat(ScmObj port, const char *fmt, ...);
extern void Scm_Printf(ScmPort *port, const char *fmt, ...);
extern void Scm_Vprintf(ScmPort *port, const char *fmt, va_list args);

/*---------------------------------------------------------
 * READ
 */

typedef struct ScmReadContextRec {
    int flags;
    ScmHashTable *table;
} ScmReadContext;

enum {
    SCM_READ_SOURCE_INFO = (1L<<0) /* preserving souce file information */
};

extern ScmObj Scm_Read(ScmObj port);
extern ScmObj Scm_ReadList(ScmObj port, ScmChar closer);
extern ScmObj Scm_ReadFromString(ScmString *string);
extern ScmObj Scm_ReadFromCString(const char *string);

extern ScmObj Scm_DefineReaderCtor(ScmObj symbol, ScmObj proc);
    
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
    int mask;
    int type;
    ScmHashAccessProc accessfn;
    ScmHashProc hashfn;
    ScmHashCmpProc cmpfn;
};

#define SCM_HASHTABLE(obj)   ((ScmHashTable*)(obj))
#define SCM_HASHTABLEP(obj)  SCM_XTYPEP(obj, SCM_CLASS_HASHTABLE)

extern ScmClass Scm_HashTableClass;
#define SCM_CLASS_HASHTABLE  (&Scm_HashTableClass)

#define SCM_HASH_ADDRESS   (0)  /* eq?-hash */
#define SCM_HASH_EQV       (1)
#define SCM_HASH_EQUAL     (2)
#define SCM_HASH_STRING    (3)
#define SCM_HASH_GENERAL   (4)

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
extern ScmObj Scm_HashTableKeys(ScmHashTable *table);
extern ScmObj Scm_HashTableValues(ScmHashTable *table);
extern ScmObj Scm_HashTableStat(ScmHashTable *table);

extern void Scm_HashIterInit(ScmHashTable *hash, ScmHashIter *iter);
extern ScmHashEntry *Scm_HashIterNext(ScmHashIter *iter);

extern unsigned long Scm_HashString(ScmString *str, unsigned long bound);

/*--------------------------------------------------------
 * MODULE
 */

struct ScmModuleRec {
    SCM_HEADER;
    ScmSymbol *name;
    ScmObj imported;
    ScmObj exported;
    ScmModule *parent;
    ScmHashTable *table;
};

#define SCM_MODULE(obj)       ((ScmModule*)(obj))
#define SCM_MODULEP(obj)      SCM_XTYPEP(obj, SCM_CLASS_MODULE)

extern ScmClass Scm_ModuleClass;
#define SCM_CLASS_MODULE     (&Scm_ModuleClass)

extern ScmGloc *Scm_FindBinding(ScmModule *module, ScmSymbol *symbol,
                                int stay_in_module);
extern ScmObj Scm_MakeModule(ScmSymbol *name);
extern ScmObj Scm_SymbolValue(ScmModule *module, ScmSymbol *symbol);
extern ScmObj Scm_Define(ScmModule *module, ScmSymbol *symbol, ScmObj value);

extern ScmObj Scm_ImportModules(ScmModule *module, ScmObj list);
extern ScmObj Scm_ExportSymbols(ScmModule *module, ScmObj list);
extern ScmObj Scm_ExportAll(ScmModule *module);
extern ScmObj Scm_FindModule(ScmSymbol *name, int createp);
extern ScmObj Scm_AllModules(void);
extern void   Scm_SelectModule(ScmModule *mod);

#define SCM_FIND_MODULE(name, createp) \
    Scm_FindModule(SCM_SYMBOL(SCM_INTERN(name)), createp)

extern ScmModule *Scm_NullModule(void);
extern ScmModule *Scm_SchemeModule(void);
extern ScmModule *Scm_GaucheModule(void);
extern ScmModule *Scm_UserModule(void);
extern ScmModule *Scm_CurrentModule(void);

#define SCM_DEFINE(module, cstr, val)           \
    Scm_Define(SCM_MODULE(module),              \
               SCM_SYMBOL(SCM_INTERN(cstr)),    \
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
#define SCM_INTERN(cstr)  Scm_Intern(SCM_STRING(SCM_MAKE_STR_IMMUTABLE(cstr)))
extern ScmObj Scm_Gensym(ScmString *prefix);

extern ScmClass Scm_SymbolClass;
#define SCM_CLASS_SYMBOL        (&Scm_SymbolClass)

/* predefined symbols */
#define DEFSYM(cname, sname)   extern ScmSymbol cname
#define DEFSYM_DEFINES
#include <gauche/predef-syms.h>
#undef DEFSYM_DEFINES
#undef DEFSYM

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
 * KEYWORD
 */

struct ScmKeywordRec {
    SCM_HEADER;
    ScmString *name;
};

extern ScmClass Scm_KeywordClass;
#define SCM_CLASS_KEYWORD       (&Scm_KeywordClass)

#define SCM_KEYWORD(obj)        ((ScmKeyword*)(obj))
#define SCM_KEYWORDP(obj)       SCM_XTYPEP(obj, SCM_CLASS_KEYWORD)
#define SCM_KEYWORD_NAME(obj)   (SCM_KEYWORD(obj)->name)

ScmObj Scm_MakeKeyword(ScmString *name);
ScmObj Scm_GetKeyword(ScmObj key, ScmObj list, ScmObj fallback);

#define SCM_MAKE_KEYWORD(cstr) \
    Scm_MakeKeyword(SCM_STRING(SCM_MAKE_STR_IMMUTABLE(cstr)))
#define SCM_GET_KEYWORD(cstr, list, fallback) \
    Scm_GetKeyword(SCM_MAKE_KEYWORD(cstr), list, fallback)

/*--------------------------------------------------------
 * NUMBER
 */

#define SCM_SMALL_INT_MAX          ((1L << 29) - 1)
#define SCM_SMALL_INT_MIN          (-SCM_SMALL_INT_MAX-1)
#define SCM_SMALL_INT_FITS(k) \
    (((k)<=SCM_SMALL_INT_MAX)&&((k)>=SCM_SMALL_INT_MIN))

#define SCM_RADIX_MAX              36

#define SCM_INTEGERP(obj)          (SCM_INTP(obj) || SCM_BIGNUMP(obj))
#define SCM_REALP(obj)             (SCM_INTEGERP(obj)||SCM_FLONUMP(obj))
#define SCM_NUMBERP(obj)           (SCM_REALP(obj)||SCM_COMPLEXP(obj))
#define SCM_EXACTP(obj)            SCM_INTEGERP(obj)
#define SCM_INEXACTP(obj)          (SCM_FLONUMP(obj)||SCM_COMPLEXP(obj))

extern ScmClass  Scm_NumberClass;
extern ScmClass  Scm_ComplexClass;
extern ScmClass  Scm_RealClass;
extern ScmClass  Scm_IntegerClass;

#define SCM_CLASS_NUMBER        (&Scm_NumberClass)
#define SCM_CLASS_COMPLEX       (&Scm_ComplexClass)
#define SCM_CLASS_REAL          (&Scm_RealClass)
#define SCM_CLASS_INTEGER       (&Scm_IntegerClass)

struct ScmBignumRec {
    SCM_HEADER;
    short sign;
    u_short size;
    u_long values[1];           /* variable length */
};

#define SCM_BIGNUM(obj)        ((ScmBignum*)(obj))
#define SCM_BIGNUMP(obj)       SCM_XTYPEP(obj, SCM_CLASS_INTEGER)
#define SCM_BIGNUM_SIZE(obj)   SCM_BIGNUM(obj)->size
#define SCM_BIGNUM_SIGN(obj)   SCM_BIGNUM(obj)->sign

extern ScmObj Scm_MakeBignumFromSI(long val);
extern ScmObj Scm_MakeBignumFromUI(u_long val);
extern ScmObj Scm_MakeBignumFromDouble(double val);
extern ScmObj Scm_BignumCopy(ScmBignum *b);
extern ScmObj Scm_BignumToString(ScmBignum *b, int radix, int use_upper);

extern long   Scm_BignumToSI(ScmBignum *b);
extern u_long Scm_BignumToUI(ScmBignum *b);
extern double Scm_BignumToDouble(ScmBignum *b);
extern ScmObj Scm_NormalizeBignum(ScmBignum *b);
extern ScmObj Scm_BignumNegate(ScmBignum *b);
extern int    Scm_BignumCmp(ScmBignum *bx, ScmBignum *by);
extern int    Scm_BignumAbsCmp(ScmBignum *bx, ScmBignum *by);

extern ScmObj Scm_BignumAdd(ScmBignum *bx, ScmBignum *by);
extern ScmObj Scm_BignumAddSI(ScmBignum *bx, long y);
extern ScmObj Scm_BignumAddN(ScmBignum *bx, ScmObj args);
extern ScmObj Scm_BignumSub(ScmBignum *bx, ScmBignum *by);
extern ScmObj Scm_BignumSubSI(ScmBignum *bx, long y);
extern ScmObj Scm_BignumSubN(ScmBignum *bx, ScmObj args);
extern ScmObj Scm_BignumMul(ScmBignum *bx, ScmBignum *by);
extern ScmObj Scm_BignumMulSI(ScmBignum *bx, long y);
extern ScmObj Scm_BignumMulN(ScmBignum *bx, ScmObj args);
extern ScmObj Scm_BignumDivSI(ScmBignum *bx, long y, long *r);
extern ScmObj Scm_BignumDivRem(ScmBignum *bx, ScmBignum *by);

extern ScmObj Scm_BignumLogAndSI(ScmBignum *bx, long y);
extern ScmObj Scm_BignumLogAnd(ScmBignum *bx, ScmBignum *by);
extern ScmObj Scm_BignumLogIor(ScmBignum *bx, ScmBignum *by);
extern ScmObj Scm_BignumLogXor(ScmBignum *bx, ScmBignum *by);
extern ScmObj Scm_BignumLogNot(ScmBignum *bx);
extern ScmObj Scm_BignumLogBit(ScmBignum *bx, int bit);
extern ScmObj Scm_BignumAsh(ScmBignum *bx, int cnt);

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
extern ScmObj Scm_MakeIntegerFromUI(u_long i);
extern long Scm_GetInteger(ScmObj obj);
extern u_long Scm_GetUInteger(ScmObj obj);

extern ScmObj Scm_MakeFlonum(double d);
extern double Scm_GetDouble(ScmObj obj);

extern ScmObj Scm_MakeComplex(double real, double imag);

extern int    Scm_IntegerP(ScmObj obj);
extern int    Scm_OddP(ScmObj obj);
extern ScmObj Scm_Abs(ScmObj obj);
extern int    Scm_Sign(ScmObj obj);
extern ScmObj Scm_Negate(ScmObj obj);
extern ScmObj Scm_Reciprocal(ScmObj obj);
extern ScmObj Scm_ExactToInexact(ScmObj obj);
extern ScmObj Scm_InexactToExact(ScmObj obj);

extern ScmObj Scm_Add(ScmObj arg1, ScmObj arg2, ScmObj args);
extern ScmObj Scm_Subtract(ScmObj arg1, ScmObj arg2, ScmObj args);
extern ScmObj Scm_Multiply(ScmObj arg1, ScmObj arg2, ScmObj args);
extern ScmObj Scm_Divide(ScmObj arg1, ScmObj arg2, ScmObj args);

extern ScmObj Scm_Quotient(ScmObj arg1, ScmObj arg2);
extern ScmObj Scm_Remainder(ScmObj arg1, ScmObj arg2);
extern ScmObj Scm_Modulo(ScmObj arg1, ScmObj arg2, int remainder);

extern ScmObj Scm_Expt(ScmObj x, ScmObj y);

extern int    Scm_NumEq(ScmObj x, ScmObj y);
extern int    Scm_NumCmp(ScmObj x, ScmObj y);
extern ScmObj Scm_Max(ScmObj arg0, ScmObj args);
extern ScmObj Scm_Min(ScmObj arg0, ScmObj args);

extern ScmObj Scm_LogAnd(ScmObj x, ScmObj y);
extern ScmObj Scm_LogIor(ScmObj x, ScmObj y);
extern ScmObj Scm_LogXor(ScmObj x, ScmObj y);
extern ScmObj Scm_LogNot(ScmObj x);
extern int    Scm_LogTest(ScmObj x, ScmObj y);
extern int    Scm_LogBit(ScmObj x, int bit);
extern ScmObj Scm_Ash(ScmObj x, int cnt);
    
enum {
    SCM_ROUND_FLOOR,
    SCM_ROUND_CEIL,
    SCM_ROUND_TRUNC,
    SCM_ROUND_ROUND
};
extern ScmObj Scm_Round(ScmObj num, int mode);

extern ScmObj Scm_Magnitude(ScmObj z);
extern ScmObj Scm_Angle(ScmObj z);

extern ScmObj Scm_NumberToString(ScmObj num, int radix, int use_upper);
extern ScmObj Scm_StringToNumber(ScmString *str, int radix);

/*--------------------------------------------------------
 * PROCEDURE (APPLICABLE OBJECT)
 */

/* Base structure */
struct ScmProcedureRec {
    SCM_HEADER;
    unsigned char required;     /* # of required args */
    unsigned char optional;     /* 1 if it takes rest args */
    unsigned char type;         /* procedure type  */
    unsigned char locked;       /* setter locked? */
    ScmObj info;                /* source code info */
    ScmObj setter;              /* setter, if exists. */
};

/* procedure type */
enum {
    SCM_PROC_SUBR,
    SCM_PROC_CLOSURE,
    SCM_PROC_GENERIC,
    SCM_PROC_METHOD,
    SCM_PROC_NEXT_METHOD
};

#define SCM_PROCEDURE(obj)          ((ScmProcedure*)(obj))
#define SCM_PROCEDURE_REQUIRED(obj) SCM_PROCEDURE(obj)->required
#define SCM_PROCEDURE_OPTIONAL(obj) SCM_PROCEDURE(obj)->optional
#define SCM_PROCEDURE_TYPE(obj)     SCM_PROCEDURE(obj)->type
#define SCM_PROCEDURE_INFO(obj)     SCM_PROCEDURE(obj)->info
#define SCM_PROCEDURE_SETTER(obj)   SCM_PROCEDURE(obj)->setter

extern ScmClass Scm_ProcedureClass;
#define SCM_CLASS_PROCEDURE   (&Scm_ProcedureClass)
#define SCM_PROCEDUREP(obj) \
    (SCM_PTRP(obj) && SCM_CLASS_APPLICABLE_P(SCM_CLASS_OF(obj)))
#define SCM_PROCEDURE_TAKE_NARG_P(obj, narg) \
    (SCM_PROCEDUREP(obj)&&SCM_PROCEDURE_REQUIRED(obj)==(narg))
#define SCM_PROCEDURE_INIT(obj, req, opt, typ, inf)     \
    SCM_PROCEDURE(obj)->required = req,                 \
    SCM_PROCEDURE(obj)->optional = opt,                 \
    SCM_PROCEDURE(obj)->type = typ,                     \
    SCM_PROCEDURE(obj)->info = inf

#define SCM__PROCEDURE_INITIALIZER(klass, req, opt, typ, inf) \
    { { (klass) }, (req), (opt), (typ), FALSE, (inf), SCM_FALSE }

/* Closure - Scheme defined procedure */
struct ScmClosureRec {
    ScmProcedure common;
    ScmObj code;                /* compiled code */
    ScmEnvFrame *env;           /* environment */
};

#define SCM_CLOSUREP(obj) \
    (SCM_PROCEDUREP(obj)&&(SCM_PROCEDURE_TYPE(obj)==SCM_PROC_CLOSURE))
#define SCM_CLOSURE(obj)           ((ScmClosure*)(obj))

extern ScmObj Scm_MakeClosure(int required, int optional,
                              ScmObj code, ScmObj info);

/* Subr - C defined procedure */
struct ScmSubrRec {
    ScmProcedure common;
    ScmObj (*func)(ScmObj *, int, void*);
    ScmObj (*inliner)(ScmSubr *, ScmObj, ScmObj, int);
    void *data;
};

#define SCM_SUBRP(obj) \
    (SCM_PROCEDUREP(obj)&&(SCM_PROCEDURE_TYPE(obj)==SCM_PROC_SUBR))
#define SCM_SUBR(obj)              ((ScmSubr*)(obj))
#define SCM_SUBR_FUNC(obj)         SCM_SUBR(obj)->func
#define SCM_SUBR_INLINER(obj)      SCM_SUBR(obj)->inliner
#define SCM_SUBR_DATA(obj)         SCM_SUBR(obj)->data

#define SCM_DEFINE_SUBR(cvar, req, opt, inf, func, inliner, data)       \
    ScmSubr cvar = {                                                    \
        SCM__PROCEDURE_INITIALIZER(SCM_CLASS_PROCEDURE,                 \
                                   req, opt, SCM_PROC_SUBR, inf),       \
        (func), (inliner), (data)                                       \
    }

extern ScmObj Scm_MakeSubr(ScmObj (*func)(ScmObj*, int, void*),
                           void *data,
                           int required, int optional,
                           ScmObj info);
extern ScmObj Scm_NullProc(void);

extern ScmObj Scm_SetterSet(ScmProcedure *proc, ScmProcedure *setter,
                            int lock);
extern ScmObj Scm_Setter(ScmProcedure *proc);

/* Generic - Generic function */
struct ScmGenericRec {
    ScmProcedure common;
    ScmObj methods;
    ScmObj (*fallback)(ScmObj *args, int nargs, ScmGeneric *gf);
    void *data;
};

extern ScmClass Scm_GenericClass;
#define SCM_CLASS_GENERIC          (&Scm_GenericClass)
#define SCM_GENERICP(obj)          SCM_XTYPEP(obj, SCM_CLASS_GENERIC)
#define SCM_GENERIC(obj)           ((ScmGeneric*)obj)

#define SCM_DEFINE_GENERIC(cvar, cfunc, data)                           \
    ScmGeneric cvar = {                                                 \
        SCM__PROCEDURE_INITIALIZER(SCM_CLASS_GENERIC,                   \
                                   0, 0, SCM_PROC_GENERIC, SCM_FALSE),  \
        SCM_NIL, cfunc, data                                            \
    }

void Scm_InitBuiltinGeneric(ScmGeneric *gf, const char *name, ScmModule *mod);
ScmObj Scm_MakeBaseGeneric(ScmObj name,
                           ScmObj (*fallback)(ScmObj *, int, ScmGeneric*),
                           void *data);
ScmObj Scm_NoNextMethod(ScmObj *args, int nargs, ScmGeneric *gf);
ScmObj Scm_NoOperation(ScmObj *args, int nargs, ScmGeneric *gf);

/* Method - method
   A method can be defined either by C or by Scheme.  C-defined method
   have func ptr, with optional data.   Scheme-define method has NULL
   in func, code in data, and optional environment in env.
   We can't use union here since we want to initialize C-defined method
   statically. */
struct ScmMethodRec {
    ScmProcedure common;
    ScmGeneric *generic;
    ScmClass **specializers;    /* array of specializers, size==required */
    ScmObj (*func)(ScmNextMethod *nm, ScmObj *args, int nargs, void * data);
    void *data;                 /* closure, or code */
    ScmEnvFrame *env;           /* environment (for Scheme created method) */
};

extern ScmClass Scm_MethodClass;
#define SCM_CLASS_METHOD           (&Scm_MethodClass)
#define SCM_METHODP(obj)           SCM_XTYPEP(obj, SCM_CLASS_METHOD)
#define SCM_METHOD(obj)            ((ScmMethod*)obj)

#define SCM_DEFINE_METHOD(cvar, gf, req, opt, specs, func, data)        \
    ScmMethod cvar = {                                                  \
        SCM__PROCEDURE_INITIALIZER(SCM_CLASS_METHOD,                    \
                                   req, opt, SCM_PROC_METHOD,           \
                                   SCM_FALSE),                          \
        gf, specs, func, data, NULL                                     \
    }

void Scm_InitBuiltinMethod(ScmMethod *m);

/* Next method object
   Next method is just another callable entity, with memorizing
   the arguments. */
struct ScmNextMethodRec {
    ScmProcedure common;
    ScmGeneric *generic;
    ScmObj methods;             /* list of applicable methods */
    ScmObj *args;               /* original arguments */
    int nargs;                  /* # of original arguments */
};

extern ScmClass Scm_NextMethodClass;
#define SCM_CLASS_NEXT_METHOD      (&Scm_NextMethodClass)
#define SCM_NEXT_METHODP(obj)      SCM_XTYPEP(obj, SCM_CLASS_NEXT_METHOD)
#define SCM_NEXT_METHOD(obj)       ((ScmNextMethod*)obj)

/* Other APIs */
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

/* The same structure is used for <exception>, <error>, <sys-error> and
   <signal> class. */

struct ScmExceptionRec {
    SCM_HEADER;
    ScmObj message;             /* common */
    ScmObj data;                /* unused for <exception> and <error>.
                                   errno for <sys-error>, and signal number
                                   for <signal> */
};

extern ScmClass Scm_ExceptionClass;
#define SCM_CLASS_EXCEPTION        (&Scm_ExceptionClass)
#define SCM_EXCEPTIONP(obj)        SCM_XTYPEP(obj, SCM_CLASS_EXCEPTION)
#define SCM_EXCEPTION(obj)         ((ScmException*)(obj))
#define SCM_EXCEPTION_MESSAGE(obj) SCM_EXCEPTION(obj)->message
#define SCM_EXCEPTION_DATA(obj)    SCM_EXCEPTION(obj)->data

extern ScmClass Scm_ErrorClass;
#define SCM_CLASS_ERROR            (&Scm_ErrorClass)
extern ScmClass Scm_SysErrorClass;
#define SCM_CLASS_SYS_ERROR        (&Scm_SysErrorClass)
extern ScmClass Scm_SignalClass;
#define SCM_CLASS_SIGNAL           (&Scm_SignalClass)

extern int Scm_ExceptionP(ScmObj e);
extern int Scm_NoncontinuableExceptionP(ScmObj e);

/* Throwing error */
extern void Scm_Error(const char *msg, ...);
extern void Scm_SysError(const char *msg, ...);
extern ScmObj Scm_SError(ScmString *reason, ScmObj args);
extern ScmObj Scm_FError(ScmObj fmt, ScmObj args);

/*--------------------------------------------------------
 * REGEXP
 */

struct ScmRegexpRec {
    SCM_HEADER;
    const char *code;
    int numGroups;
    int numCodes;
    ScmCharSet **sets;
    int numSets;
    const char *mustMatch;
    int mustMatchLen;
};

extern ScmClass Scm_RegexpClass;
#define SCM_CLASS_REGEXP          (&Scm_RegexpClass)
#define SCM_REGEXP(obj)           ((ScmRegexp*)obj)
#define SCM_REGEXPP(obj)          SCM_XTYPEP(obj, SCM_CLASS_REGEXP)

extern ScmObj Scm_RegComp(ScmString *pattern);
extern ScmObj Scm_RegExec(ScmRegexp *rx, ScmString *input);
extern void Scm_RegDump(ScmRegexp *rx);

struct ScmRegMatchRec {
    SCM_HEADER;
    const char *input;
    int inputSize;
    int inputLen;
    int numMatches;
    struct ScmRegMatchSub {
        int start;
        int length;
        const char *startp;
        const char *endp;
    } *matches;
};

extern ScmClass Scm_RegMatchClass;
#define SCM_CLASS_REGMATCH        (&Scm_RegMatchClass)
#define SCM_REGMATCH(obj)         ((ScmRegMatch*)obj)
#define SCM_REGMATCHP(obj)        SCM_XTYPEP(obj, SCM_CLASS_REGMATCH)

extern ScmObj Scm_RegMatchSubstr(ScmRegMatch *rm, int i);
extern ScmObj Scm_RegMatchStart(ScmRegMatch *rm, int i);
extern ScmObj Scm_RegMatchEnd(ScmRegMatch *rm, int i);
extern ScmObj Scm_RegMatchAfter(ScmRegMatch *rm, int i);
extern ScmObj Scm_RegMatchBefore(ScmRegMatch *rm, int i);
extern void Scm_RegMatchDump(ScmRegMatch *match);

/*-------------------------------------------------------
 * STUB MACROS
 */
#define SCM_ENTER_SUBR(name)

#define SCM_ARGREF(count)           (SCM_FP[count])
#define SCM_RETURN(value)           return value
#define SCM_CURRENT_MODULE()        (Scm_VM()->module)

/*---------------------------------------------------
 * SYSTEM
 */

extern int Scm_GetPortFd(ScmObj port_or_fd, int needfd);

extern ScmObj Scm_ReadDirectory(ScmString *pathname);
extern ScmObj Scm_GlobDirectory(ScmString *pattern);

#define SCM_PATH_ABSOLUTE       (1L<<0)
#define SCM_PATH_EXPAND         (1L<<1)
#define SCM_PATH_CANONICALIZE   (1L<<2)
#define SCM_PATH_FOLLOWLINK     (1L<<3) /* not supported yet */
extern ScmObj Scm_NormalizePathname(ScmString *pathname, int flags);
extern ScmObj Scm_DirName(ScmString *filename);
extern ScmObj Scm_BaseName(ScmString *filename);

/* struct stat */
typedef struct ScmSysStatRec {
    SCM_HEADER;
    struct stat statrec;
} ScmSysStat;
    
extern ScmClass Scm_SysStatClass;
#define SCM_CLASS_SYS_STAT    (&Scm_SysStatClass)
#define SCM_SYS_STAT(obj)     ((ScmSysStat*)(obj))
#define SCM_SYS_STAT_P(obj)   (SCM_XTYPEP(obj, SCM_CLASS_SYS_STAT))

extern ScmObj Scm_MakeSysStat(void); /* returns empty SysStat */

/* time_t
 * NB: POSIX defines time_t to be a type to represent number of seconds
 * since Epoch.  It may be a structure.  In Gauche we just convert it
 * to a number.
 */
extern ScmObj Scm_MakeSysTime(time_t time);
extern time_t Scm_GetSysTime(ScmObj val);

/* struct tm */
typedef struct ScmSysTmRec {
    SCM_HEADER;
    struct tm tm;
} ScmSysTm;
    
extern ScmClass Scm_SysTmClass;
#define SCM_CLASS_SYS_TM      (&Scm_SysTmClass)
#define SCM_SYS_TM(obj)       ((ScmSysTm*)(obj))
#define SCM_SYS_TM_P(obj)     (SCM_XTYPEP(obj, SCM_CLASS_SYS_TM))
#define SCM_SYS_TM_TM(obj)    SCM_SYS_TM(obj)->tm

extern ScmObj Scm_MakeSysTm(struct tm *);
    
/* struct group */
typedef struct ScmSysGroupRec {
    SCM_HEADER;
    ScmObj name;
    ScmObj gid;
    ScmObj passwd;
    ScmObj mem;
} ScmSysGroup;

extern ScmClass Scm_SysGroupClass;
#define SCM_CLASS_SYS_GROUP    (&Scm_SysGroupClass)
#define SCM_SYS_GROUP(obj)     ((ScmSysGroup*)(obj))
#define SCM_SYS_GROUP_P(obj)   (SCM_XTYPEP(obj, SCM_CLASS_SYS_GROUP))
    
extern ScmObj Scm_GetGroupById(gid_t gid);
extern ScmObj Scm_GetGroupByName(ScmString *name);

/* struct passwd */
typedef struct ScmSysPasswdRec {
    SCM_HEADER;
    ScmObj name;
    ScmObj passwd;
    ScmObj uid;
    ScmObj gid;
    ScmObj gecos;
    ScmObj dir;
    ScmObj shell;
    ScmObj pwclass;
} ScmSysPasswd;

extern ScmClass Scm_SysPasswdClass;
#define SCM_CLASS_SYS_PASSWD    (&Scm_SysPasswdClass)
#define SCM_SYS_PASSWD(obj)     ((ScmSysPasswd*)(obj))
#define SCM_SYS_PASSWD_P(obj)   (SCM_XTYPEP(obj, SCM_CLASS_SYS_PASSWD))

extern ScmObj Scm_GetPasswdById(uid_t uid);
extern ScmObj Scm_GetPasswdByName(ScmString *name);

extern void Scm_SysExec(ScmString *file, ScmObj args, ScmObj iomap);

/* select */
#ifdef HAVE_SELECT
typedef struct ScmSysFdsetRec {
    SCM_HEADER;
    int maxfd;
    fd_set fdset;
} ScmSysFdset;

extern ScmClass Scm_SysFdsetClass;
#define SCM_CLASS_SYS_FDSET     (&Scm_SysFdsetClass)
#define SCM_SYS_FDSET(obj)      ((ScmSysFdset*)(obj))
#define SCM_SYS_FDSET_P(obj)    (SCM_XTYPEP(obj, SCM_CLASS_SYS_FDSET))

extern ScmObj Scm_SysSelect(ScmObj rfds, ScmObj wfds, ScmObj efds,
                            ScmObj timeout);
extern ScmObj Scm_SysSelectX(ScmObj rfds, ScmObj wfds, ScmObj efds,
                             ScmObj timeout);
#else  /*!HAVE_SELECT*/
/* dummy definitions */
typedef struct ScmHeaderRec ScmSysFdset;
#define SCM_SYS_FDSET(obj)      (obj)
#define SCM_SYS_FDSET_P(obj)    (FALSE)
#endif /*!HAVE_SELECT*/

/*---------------------------------------------------
 * LOAD AND DYNAMIC LINK
 */

extern ScmObj Scm_VMLoadFromPort(ScmPort *port);
extern ScmObj Scm_VMLoad(ScmString *file, int error_if_not_found);
extern void Scm_LoadFromPort(ScmPort *port);
extern void Scm_Load(const char *file, int error_if_not_found);

extern ScmObj Scm_GetLoadPath(void);
extern ScmObj Scm_AddLoadPath(const char *cpath, int afterp);

extern ScmObj Scm_DynLoad(ScmString *path, ScmObj initfn, int export);

extern ScmObj Scm_Require(ScmObj feature);
extern ScmObj Scm_Provide(ScmObj feature);
extern ScmObj Scm_ProvidedP(ScmObj feature);
    
typedef struct ScmAutoloadRec {
    SCM_HEADER;
    ScmSymbol *name;            /* variable to be autoloaded */
    ScmModule *module;          /* where the binding should be inserted.
                                   this is where autoload is defined. */
    ScmString *path;            /* file to load */
    ScmSymbol *import_from;     /* module to be imported after loading */
    ScmModule *import_to;       /* module to where import_from should be
                                   imported */
    int loaded;
} ScmAutoload;

extern ScmClass Scm_AutoloadClass;
#define SCM_CLASS_AUTOLOAD      (&Scm_AutoloadClass)
#define SCM_AUTOLOADP(obj)      SCM_XTYPEP(obj, SCM_CLASS_AUTOLOAD)
#define SCM_AUTOLOAD(obj)       ((ScmAutoload*)(obj))

extern ScmObj Scm_MakeAutoload(ScmSymbol *name, ScmString *path,
                               ScmSymbol *import_from);
extern ScmObj Scm_LoadAutoload(ScmAutoload *autoload);

/*---------------------------------------------------
 * UTILITY STUFF
 */

/* Program start and termination */

extern void Scm_Init(void);
extern void Scm_Exit(int code);
extern void Scm_Abort(const char *msg);
extern void Scm_Panic(const char *msg, ...);

/* repl */
extern void Scm_Repl(ScmObj prompt, ScmPort *in, ScmPort *out);

/* Inspect the configuration */
extern const char *Scm_HostArchitecture(void);

/* Compare and Sort */
int Scm_Compare(ScmObj x, ScmObj y);
void Scm_SortArray(ScmObj *elts, int nelts, ScmObj cmpfn);
ScmObj Scm_SortList(ScmObj objs, ScmObj fn);
ScmObj Scm_SortListX(ScmObj objs, ScmObj fn);

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


#ifdef __cplusplus
}
#endif

#endif /* GAUCHE_H */
