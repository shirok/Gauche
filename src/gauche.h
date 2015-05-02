/*
 * gauche.h - Gauche scheme system header
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_H
#define GAUCHE_H

/* Read config.h _before_ other headers, for it may affect the behavior
   of system header files.  Currently the only known instance of it is
   sigwait() on Solaris---we need to define _POSIX_PTHREAD_SEMANTICS to
   get pthread-compatible sigwait()---but we may encounter more of such
   instances. */
#include <gauche/config.h>
#include <gauche/config_threads.h>

#if     GAUCHE_API_0_95         /* A provisional API towards 1.0 */
#define GAUCHE_API_0_9   0
#endif  /*GAUCHE_API_0_95*/

#ifndef GAUCHE_API_0_9
#define GAUCHE_API_0_9   1      /* 0.9 API */
#endif

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdarg.h>
#include <setjmp.h>
#include <limits.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <gauche/int64.h>
#include <gauche/float.h>

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

/* For Windows platforms, we need some compatibility tricks.
   This defines GAUCHE_WINDOWS preprocessor symbol.
   (This should come before including gc.h) */
#if defined(__MINGW32__) || defined(MSVC)
#include <gauche/win-compat.h>
#endif /* MINGW32 || WINDOWS */

#if defined(LIBGAUCHE_BODY)
#if !defined(GC_DLL)
#define GC_DLL    /* for gc.h to handle Win32 crazyness */
#endif
#if !defined(GC_BUILD)
#define GC_BUILD  /* ditto */
#endif
#endif /* LIBGAUCHE_BODY */ 
#include <gc.h>

#ifndef SCM_DECL_BEGIN
#ifdef __cplusplus
#define SCM_DECL_BEGIN  extern "C" {
#define SCM_DECL_END    }
#else  /*! __cplusplus */
#define SCM_DECL_BEGIN
#define SCM_DECL_END
#endif /*! __cplusplus */
#endif /*!defined(SCM_DECL_BEGIN)*/

SCM_DECL_BEGIN

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /*HAVE_UNISTD_H*/

/* Defines SCM_EXTERN magic. */
#include <gauche/extern.h>

/* Some useful macros */
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE (!FALSE)
#endif

/* Define this to 0 to turn off fast flonum extension.  See the comment in
   gauche/number.h for the details. */
#define GAUCHE_FFX 1

/* Define this to 0 to turn off lazy-pair feature. */
#define GAUCHE_LAZY_PAIR 1

/* Enable an option to make keywords and symbols disjoint.
   (Transient: Will be gone once we completely migrate to
   unified keyword-symbol system */
#define GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION 1

/* Include appropriate threading interface.  Threading primitives are
   abstracted with SCM_INTERNAL_* macros and ScmInternal* typedefs.
   See gauche/uthread.h for the semantics of these primitives. */
#ifdef GAUCHE_USE_PTHREADS
# include <gauche/pthread.h>
#elif  GAUCHE_USE_WTHREADS
# include <gauche/wthread.h>
#else  /* !GAUCHE_USE_PTHREADS */
# include <gauche/uthread.h>
#endif /* !GAUCHE_USE_PTHREADS */

#define SCM_WORD_BITS   (SIZEOF_LONG*8)

/* Newer gcc/glibc adds lots of __attribute__((warn_unused_result)) that
   causes excessive warnings for the code that intentionally ignores the
   return value.  Casting the result to void won't silence it.
   Hence this macro. */
#define SCM_IGNORE_RESULT(expr)  do { if(expr) {} } while(0)

/* ScmFlonum and ScmClass must always be aligned in 8-byte boundaries.
   (All other Scheme objects can be in 4-byte boundary.)
   Some platform doesn't align static double in 8-byte boundaries, so
   we try this as well.  */
#ifdef __GNUC__
#define SCM_ALIGN8  __attribute__ ((aligned (8)))
#else  /* !__GNUC__ */
#define SCM_ALIGN8  /*empty*/
#endif /* !__GNUC__ */

/* 'No return' attribute */
#ifdef __GNUC__
#define SCM_NORETURN  __attribute__((__noreturn__))
#else  /*__GNUC__*/
#define SCM_NORETURN  /*empty*/
#endif /*__GNUC__*/

/*-------------------------------------------------------------
 * BASIC TYPES
 */

/*
 * A word large enough to hold a pointer
 */
typedef intptr_t ScmWord;

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

/*
 * The class structure.  ScmClass is actually a subclass of ScmObj.
 */
typedef struct ScmClassRec ScmClass;

/* TAG STRUCTURE
 *
 * [Pointer]
 *      -------- -------- -------- ------00
 *      Points to a pair or other heap-allocated objects.
 *      If the lower 3 bits of the pointed word are '111',
 *      it's a heap object (see below).  Otherwise, it's
 *      a pair.
 *
 * [Fixnum]
 *      -------- -------- -------- ------01
 *      30 or 62-bit signed integer
 *
 * [Flonum]
 *      -------- -------- -------- -----M10
 *      Points to C double.  M=0 if the double is in the VM
 *      register, M=1 if it is on the heap.  See the comment in
 *      gauche/number.h for the details.
 *
 * [Character]
 *      -------- -------- -------- 00000011
 *      24-bit.  20bits are enough to cover all UCS, but we
 *      reserve a few extra bits for possible future extension.
 *
 * [Miscellaneous]
 *      -------- -------- -------- 00001011
 *      #f, #t, '(), eof-object, undefined
 *
 * [Pattern variable]
 *      -------- -------- -------- 00010011
 *      Used in macro expander.
 *
 * [Heap object]
 *      -------- -------- -------- -----111
 *      Only appears at the first word of heap-allocated
 *      objects except pairs and flonums.   Masking lower
 *      3bits gives a pointer to ScmClass.  
 */

/* Type coercer */

#define SCM_OBJ(obj)      ((ScmObj)(obj))
#define SCM_WORD(obj)     ((ScmWord)(obj))

/*
 * PRIMARY TAG IDENTIFICATION
 */

#define SCM_TAG1(obj)    (SCM_WORD(obj) & 0x01)
#define SCM_TAG2(obj)    (SCM_WORD(obj) & 0x03)
#define SCM_TAG3(obj)    (SCM_WORD(obj) & 0x07)
#define SCM_TAG8(obj)    (SCM_WORD(obj) & 0xff)

/* Check if the ScmObj is a 'pointer'---either to a pair,
   a heap object, or a ScmFlonum. */
#define SCM_PTRP(obj)    (SCM_TAG1(obj) == 0)

/* Check if the ScmObj is a pointer to either a pair or a heap
   (That is, we can safely take SCM_OBJ(obj)->tag) */
#define SCM_HPTRP(obj)   (SCM_TAG2(obj) == 0)

/* This macro further takes the lower three bits of the word pointed
   by OBJ, to distinguish whether it's a pair or a heap object. */
#define SCM_HTAG(obj)    (SCM_WORD(SCM_OBJ(obj)->tag)&7)

/*
 * IMMEDIATE OBJECTS
 */

#define SCM_IMMEDIATEP(obj) (SCM_TAG8(obj) == 0x0b)
#define SCM_ITAG(obj)       (SCM_WORD(obj)>>8)

#define SCM__MAKE_ITAG(num)  (((num)<<8) + 0x0b)
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
#define SCM_BOOLP(obj)       ((obj) == SCM_TRUE || (obj) == SCM_FALSE)
#define SCM_BOOL_VALUE(obj)  (!SCM_FALSEP(obj))
#define SCM_MAKE_BOOL(obj)   ((obj)? SCM_TRUE:SCM_FALSE)

#define SCM_EQ(x, y)         ((x) == (y))

SCM_EXTERN int Scm_EqP(ScmObj x, ScmObj y);
SCM_EXTERN int Scm_EqvP(ScmObj x, ScmObj y);
SCM_EXTERN int Scm_EqualP(ScmObj x, ScmObj y);

/* comparison mode */
enum {
    SCM_CMP_EQ,
    SCM_CMP_EQV,
    SCM_CMP_EQUAL
};

SCM_EXTERN int Scm_EqualM(ScmObj x, ScmObj y, int mode);

/*
 * FIXNUM
 */

#define SCM_INTP(obj)        (SCM_TAG2(obj) == 1)
#define SCM_INT_VALUE(obj)   (((signed long int)SCM_WORD(obj)) >> 2)
#define SCM_MAKE_INT(obj)    SCM_OBJ(((intptr_t)(obj) << 2) + 1)

#define SCM_UINTP(obj)       (SCM_INTP(obj)&&((signed long int)SCM_WORD(obj)>=0))
typedef long ScmSmallInt;    /* C integer type corresponds to Scheme fixnum
                                See SCM_SMALL_* macros in gauche/number.h */

/*
 * FLONUM
 */

typedef struct ScmFlonumRec {
    double val;
} ScmFlonum SCM_ALIGN8;

#define SCM_FLONUM(obj)            ((ScmFlonum*)(SCM_WORD(obj)&~0x07))
#define SCM_FLONUMP(obj)           (SCM_TAG2(obj) == 2)
#define SCM_FLONUM_VALUE(obj)      (SCM_FLONUM(obj)->val)

/*
 * CHARACTERS
 *
 *  A character is represented by (up to) 29-bit integer.  The actual
 *  encoding depends on compile-time flags.
 *
 *  For character cases, I only care about ASCII chars (at least for now)
 */

#define SCM_CHAR(obj)           ((ScmChar)(obj))
#define SCM_CHARP(obj)          ((SCM_WORD(obj)&0xff) == 3)
#define SCM_CHAR_VALUE(obj)     SCM_CHAR(((unsigned long)SCM_WORD(obj)) >> 8)
#define SCM_MAKE_CHAR(ch)       SCM_OBJ((((unsigned long)(ch))<<8) + 3)

#define SCM_CHAR_INVALID        ((ScmChar)(-1)) /* indicate invalid char */
#define SCM_CHAR_MAX            (0xffffff)

#define SCM_CHAR_ASCII_P(ch)    ((ch) < 0x80)

/* The following four macros are obsoleted; use API version instead.*/
#define SCM_CHAR_UPPER_P(ch)    Scm_CharUppercaseP(ch)
#define SCM_CHAR_LOWER_P(ch)    Scm_CharLowercaseP(ch)
#define SCM_CHAR_UPCASE(ch)     Scm_CharUpcase(ch)
#define SCM_CHAR_DOWNCASE(ch)   Scm_CharDowncase(ch)

SCM_EXTERN int Scm_DigitToInt(ScmChar ch, int radix, int extended);
SCM_EXTERN ScmChar Scm_IntToDigit(int n, int radix, int basechar1, int basechar2);
SCM_EXTERN int Scm_CharToUcs(ScmChar ch);
SCM_EXTERN ScmChar Scm_UcsToChar(int ucs);
SCM_EXTERN ScmObj Scm_CharEncodingName(void);
SCM_EXTERN const char **Scm_SupportedCharacterEncodings(void);
SCM_EXTERN int Scm_SupportedCharacterEncodingP(const char *encoding);

SCM_EXTERN int Scm_CharGeneralCategory(ScmChar ch);
SCM_EXTERN int Scm_CharAlphabeticP(ScmChar ch);
SCM_EXTERN int Scm_CharUppercaseP(ScmChar ch);
SCM_EXTERN int Scm_CharLowercaseP(ScmChar ch);
SCM_EXTERN int Scm_CharTitlecaseP(ScmChar ch);
SCM_EXTERN int Scm_CharNumericP(ScmChar ch);

SCM_EXTERN ScmChar Scm_CharUpcase(ScmChar ch);
SCM_EXTERN ScmChar Scm_CharDowncase(ScmChar ch);
SCM_EXTERN ScmChar Scm_CharTitlecase(ScmChar ch);
SCM_EXTERN ScmChar Scm_CharFoldcase(ScmChar ch);

SCM_EXTERN void Scm__InstallCharconvHooks(ScmChar (*u2c)(int),
                                          int (*c2u)(ScmChar));

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
 *  A heap allocated object has its class tag in the first word
 *  (except pairs).  Masking the lower three bits of class tag
 *  gives a pointer to the class object.
 */

#define SCM_HOBJP(obj)  (SCM_HPTRP(obj)&&(SCM_HTAG(obj)==7))

#define SCM_CPP_CAT(a, b)   a##b
#define SCM_CPP_CAT3(a, b, c)  a ## b ## c

/* We use a pointer to the class structure (with low-bit tag) as
   the generic type tag.   NB: The ScmClass structure is always
   aligned on 8-byte boundary, so +7 makes the tag's lower
   3 bits '111'.  Such pattern never appears in tagged pointer,
   so we can distinguish heap allocated objects from ScmPair.  */
#define SCM_CLASS2TAG(klass)  ((ScmByte*)(klass) + 7)

/* A common header for heap-allocated objects */
typedef struct ScmHeaderRec {
    ScmByte *tag;                /* private.  should be accessed
                                    only via SCM_CLASS_OF and SCM_SET_CLASS
                                    macros. */
} ScmHeader;

#define SCM_HEADER       ScmHeader hdr /* for declaration */

/* Here comes the ugly part.  To understand the general idea, just ignore
   GAUCHE_BROKEN_LINKER_WORKAROUND part; except that, it's pretty simple.
   Every heap allocated object contains (pointer to its class + 7) in its
   tag field.  */
#if !defined(GAUCHE_BROKEN_LINKER_WORKAROUND)

# define SCM_CLASS_DECL(klass) extern ScmClass klass
# define SCM_CLASS_STATIC_PTR(klass) (&klass)
# define SCM_CLASS_STATIC_TAG(klass) SCM_CLASS2TAG(&klass)

/* Extract the class pointer from the tag.
   You can use these only if SCM_HOBJP(obj) != FALSE */
# define SCM_CLASS_OF(obj)      SCM_CLASS((SCM_OBJ(obj)->tag - 7))
# define SCM_SET_CLASS(obj, k)  (SCM_OBJ(obj)->tag = (ScmByte*)(k) + 7)

/* Check if classof(OBJ) equals to an extended class KLASS.
   We can check SCM_HPTRP instead of SCM_HOBJP here, since a pair never
   satisfies the second test. */
# define SCM_XTYPEP(obj, klass) \
    (SCM_HPTRP(obj)&&(SCM_OBJ(obj)->tag == SCM_CLASS2TAG(klass)))

#else  /*GAUCHE_BROKEN_LINKER_WORKAROUND*/

/* You don't want to understand these. */
# define SCM_CLASS_DECL(klass) \
    SCM_EXTERN ScmClass klass; \
    extern ScmClass *SCM_CPP_CAT(_imp__, klass) 
# define SCM_CLASS_STATIC_PTR(klass) ((ScmClass*)(&SCM_CPP_CAT(_imp__,klass)))
# define SCM_CLASS_STATIC_TAG(klass) SCM_CLASS2TAG(SCM_CLASS_STATIC_PTR(klass))

# define SCM_CLASS_OF(obj)      (*(ScmClass**)((SCM_OBJ(obj)->tag - 7)))
# define SCM_SET_CLASS(obj, k)  (SCM_OBJ(obj)->tag = (ScmByte*)((k)->classPtr) + 7)

# define SCM_XTYPEP(obj, klass) \
    (SCM_HOBJP(obj)&&(SCM_CLASS_OF(obj) == klass))
#endif /*GAUCHE_BROKEN_LINKER_WORKAROUND*/



/* Check if classof(OBJ) is a subtype of an extended class KLASS */
#define SCM_ISA(obj, klass) (SCM_XTYPEP(obj,klass)||Scm_TypeP(SCM_OBJ(obj),klass))

/* A common header for objects whose class is defined in Scheme */
typedef struct ScmInstanceRec {
    ScmByte *tag;               /* private */
    ScmObj *slots;              /* private */
} ScmInstance;

#define SCM_INSTANCE_HEADER  ScmInstance hdr  /* for declaration */

#define SCM_INSTANCE(obj)        ((ScmInstance*)(obj))
#define SCM_INSTANCE_SLOTS(obj)  (SCM_INSTANCE(obj)->slots)

/* Fundamental allocators */
#define SCM_MALLOC(size)          GC_MALLOC(size)
#define SCM_MALLOC_ATOMIC(size)   GC_MALLOC_ATOMIC(size)
#define SCM_STRDUP(s)             GC_STRDUP(s)
#define SCM_STRDUP_PARTIAL(s, n)  Scm_StrdupPartial(s, n)

#define SCM_NEW(type)         ((type*)(SCM_MALLOC(sizeof(type))))
#define SCM_NEW_ARRAY(type, nelts) ((type*)(SCM_MALLOC(sizeof(type)*(nelts))))
#define SCM_NEW2(type, size)  ((type)(SCM_MALLOC(size)))
#define SCM_NEW_ATOMIC(type)  ((type*)(SCM_MALLOC_ATOMIC(sizeof(type))))
#define SCM_NEW_ATOMIC_ARRAY(type, nelts)  ((type*)(SCM_MALLOC_ATOMIC(sizeof(type)*(nelts))))
#define SCM_NEW_ATOMIC2(type, size) ((type)(SCM_MALLOC_ATOMIC(size)))

typedef void (*ScmFinalizerProc)(ScmObj z, void *data);
SCM_EXTERN void Scm_RegisterFinalizer(ScmObj z, ScmFinalizerProc finalizer,
                                      void *data);
SCM_EXTERN void Scm_UnregisterFinalizer(ScmObj z);

/* Safe coercer */
#define SCM_OBJ_SAFE(obj)     ((obj)?SCM_OBJ(obj):SCM_UNDEFINED)

typedef struct ScmVMRec        ScmVM;
typedef struct ScmPairRec      ScmPair;
typedef struct ScmExtendedPairRec ScmExtendedPair;
typedef struct ScmLazyPairRec  ScmLazyPair;
typedef struct ScmCharSetRec   ScmCharSet;
typedef struct ScmStringRec    ScmString;
typedef struct ScmDStringRec   ScmDString;
typedef struct ScmVectorRec    ScmVector;
typedef struct ScmBignumRec    ScmBignum;
typedef struct ScmRatnumRec    ScmRatnum;
typedef struct ScmCompnumRec   ScmCompnum;
typedef struct ScmPortRec      ScmPort;
typedef struct ScmHashTableRec ScmHashTable;
typedef struct ScmTreeMapRec   ScmTreeMap;
typedef struct ScmModuleRec    ScmModule;
typedef struct ScmSymbolRec    ScmSymbol;
typedef struct ScmGlocRec      ScmGloc;
typedef struct ScmProcedureRec ScmProcedure;
typedef struct ScmClosureRec   ScmClosure;
typedef struct ScmSubrRec      ScmSubr;
typedef struct ScmGenericRec   ScmGeneric;
typedef struct ScmMethodRec    ScmMethod;
typedef struct ScmNextMethodRec ScmNextMethod;
typedef struct ScmSyntaxRec    ScmSyntax;
typedef struct ScmMacroRec     ScmMacro;
typedef struct ScmPromiseRec   ScmPromise;
typedef struct ScmRegexpRec    ScmRegexp;
typedef struct ScmRegMatchRec  ScmRegMatch;
typedef struct ScmWriteContextRec ScmWriteContext;
typedef struct ScmAutoloadRec  ScmAutoload;
typedef struct ScmComparatorRec ScmComparator;
typedef struct ScmDLObjRec     ScmDLObj;         /* see load.c */
typedef struct ScmReadContextRec ScmReadContext; /* see read.c */

typedef ScmObj ScmSubrProc(ScmObj *, int, void*);

#include <gauche/bits.h>

/*---------------------------------------------------------
 * VM STUFF
 */

/* Detailed definitions are in vm.h.  Here I expose external interface */

#include <gauche/parameter.h>
#include <gauche/vm.h>

#define SCM_VM(obj)          ((ScmVM *)(obj))
#define SCM_VMP(obj)         SCM_XTYPEP(obj, SCM_CLASS_VM)

#define SCM_VM_CURRENT_INPUT_PORT(vm)   (SCM_VM(vm)->curin)
#define SCM_VM_CURRENT_OUTPUT_PORT(vm)  (SCM_VM(vm)->curout)
#define SCM_VM_CURRENT_ERROR_PORT(vm)   (SCM_VM(vm)->curerr)

SCM_EXTERN ScmVM *Scm_VM(void);     /* Returns the current VM */

/* The new APIs to run Scheme code from C.
   Returns # of results (>=0) if operation is successful, 
   -1 if an error is occurred and captured.
   All result values are available in ScmEvalPacket.
   Exceptions are captured and returned in the ScmEvalPacket. */
typedef struct ScmEvalPacketRec {
    ScmObj results[SCM_VM_MAX_VALUES];
    int    numResults;
    ScmObj exception;
    ScmModule *module;          /* 'Current module' after evaluation */
} ScmEvalPacket;

SCM_EXTERN int Scm_Eval(ScmObj form, ScmObj env, ScmEvalPacket *packet);
SCM_EXTERN int Scm_EvalCString(const char *form, ScmObj env,
                               ScmEvalPacket *packet);
SCM_EXTERN int Scm_Apply(ScmObj proc, ScmObj args,
                         ScmEvalPacket *packet);

/* Calls VM recursively to evaluate the Scheme code.  These
   ones does not capture exceptions. */
SCM_EXTERN ScmObj Scm_EvalRec(ScmObj form, ScmObj env);
SCM_EXTERN ScmObj Scm_ApplyRec(ScmObj proc, ScmObj args);
SCM_EXTERN ScmObj Scm_ApplyRec0(ScmObj proc);
SCM_EXTERN ScmObj Scm_ApplyRec1(ScmObj proc, ScmObj arg0);
SCM_EXTERN ScmObj Scm_ApplyRec2(ScmObj proc, ScmObj arg0, ScmObj arg1);
SCM_EXTERN ScmObj Scm_ApplyRec3(ScmObj proc, ScmObj arg0, ScmObj arg1,
                                ScmObj arg2);
SCM_EXTERN ScmObj Scm_ApplyRec4(ScmObj proc, ScmObj arg0, ScmObj arg1,
                                ScmObj arg2, ScmObj arg3);
SCM_EXTERN ScmObj Scm_ApplyRec5(ScmObj proc, ScmObj arg0, ScmObj arg1,
                                ScmObj arg2, ScmObj arg3, ScmObj arg4);

/* for compatibility */
#define Scm_EvalCStringRec(f, e)  Scm_EvalRec(Scm_ReadFromCString(f), e)

/* Returns multiple values.  Actually these functions just sets
   extra values in VM and returns the primary value. */
SCM_EXTERN ScmObj Scm_Values(ScmObj args);
SCM_EXTERN ScmObj Scm_Values2(ScmObj val0, ScmObj val1);
SCM_EXTERN ScmObj Scm_Values3(ScmObj val0, ScmObj val1, ScmObj val2);
SCM_EXTERN ScmObj Scm_Values4(ScmObj val0, ScmObj val1, ScmObj val2,
                              ScmObj val3);
SCM_EXTERN ScmObj Scm_Values5(ScmObj val0, ScmObj val1, ScmObj val2,
                              ScmObj val3, ScmObj val4);

/* CPS API for evaluating Scheme fragments on VM. */
SCM_EXTERN ScmObj Scm_VMApply(ScmObj proc, ScmObj args);
SCM_EXTERN ScmObj Scm_VMApply0(ScmObj proc);
SCM_EXTERN ScmObj Scm_VMApply1(ScmObj proc, ScmObj arg);
SCM_EXTERN ScmObj Scm_VMApply2(ScmObj proc, ScmObj arg1, ScmObj arg2);
SCM_EXTERN ScmObj Scm_VMApply3(ScmObj proc, ScmObj arg1, ScmObj arg2,
                               ScmObj arg3);
SCM_EXTERN ScmObj Scm_VMApply4(ScmObj proc, ScmObj arg1, ScmObj arg2,
                               ScmObj arg3, ScmObj arg4);
SCM_EXTERN ScmObj Scm_VMEval(ScmObj expr, ScmObj env);
SCM_EXTERN ScmObj Scm_VMCall(ScmObj *args, int argcnt, void *data);

SCM_EXTERN ScmObj Scm_VMCallCC(ScmObj proc);
SCM_EXTERN ScmObj Scm_VMCallPC(ScmObj proc);
SCM_EXTERN ScmObj Scm_VMDynamicWind(ScmObj pre, ScmObj body, ScmObj post);
SCM_EXTERN ScmObj Scm_VMDynamicWindC(ScmSubrProc *before,
                                     ScmSubrProc *body,
                                     ScmSubrProc *after,
                                     void *data);

SCM_EXTERN ScmObj Scm_VMWithErrorHandler(ScmObj handler, ScmObj thunk);
SCM_EXTERN ScmObj Scm_VMWithGuardHandler(ScmObj handler, ScmObj thunk);
SCM_EXTERN ScmObj Scm_VMWithExceptionHandler(ScmObj handler, ScmObj thunk);

/* Miscellaneous stuff */
SCM_EXTERN int    Scm_VMGetNumResults(ScmVM *vm);
SCM_EXTERN ScmObj Scm_VMGetResult(ScmVM *vm);
SCM_EXTERN ScmObj Scm_VMGetStackLite(ScmVM *vm);
SCM_EXTERN ScmObj Scm_VMGetStack(ScmVM *vm);

/* A box is to keep a reference.  It is mainly used for mutable local variables.
 */

typedef struct ScmBoxRec {
    SCM_HEADER;
    ScmObj value;
} ScmBox;

SCM_CLASS_DECL(Scm_BoxClass);
#define SCM_CLASS_BOX            (&Scm_BoxClass)
#define SCM_BOX(obj)             ((ScmBox*)(obj))
#define SCM_BOXP(obj)            (SCM_XTYPEP(obj, SCM_CLASS_BOX))
#define SCM_BOX_VALUE(obj)       (SCM_BOX(obj)->value)
#define SCM_BOX_SET(obj, val)    (SCM_BOX(obj)->value = (val))

SCM_EXTERN ScmBox *Scm_MakeBox(ScmObj value);

/*---------------------------------------------------------
 * CLASS
 */

typedef void (*ScmClassPrintProc)(ScmObj obj,
                                  ScmPort *sink,
                                  ScmWriteContext *mode);
typedef int  (*ScmClassCompareProc)(ScmObj x, ScmObj y, int equalp);
typedef int  (*ScmClassSerializeProc)(ScmObj obj,
                                      ScmPort *sink,
                                      ScmObj context);
typedef ScmObj (*ScmClassAllocateProc)(ScmClass *klass, ScmObj initargs);

/* See class.c for the description of function pointer members.
   There's a lot of voodoo magic in class structure, so don't touch
   those fields casually.  Also, the order of these fields must be
   reflected to the class definition macros below. */
struct ScmClassRec {
    /* A trick to align statically allocated class structure on 8-byte
       boundary.  This doesn't guarantee, though, so we use __alignment__
       attribute as well, whenever possible (see SCM_ALIGN8 macro). */
    union {
        SCM_INSTANCE_HEADER;
        double align_dummy;
    } classHdr;
#if defined(GAUCHE_BROKEN_LINKER_WORKAROUND)
    ScmClass **classPtr;
#endif
    ScmClassPrintProc     print;
    ScmClassCompareProc   compare;
    ScmClassSerializeProc serialize;
    ScmClassAllocateProc  allocate;
    ScmClass **cpa;             /* class precedence array, NULL terminated */
    int numInstanceSlots;       /* # of instance slots */
    int coreSize;               /* size of core structure; 0 == unknown */
    unsigned int flags;
    ScmObj name;                /* scheme name */
    ScmObj directSupers;        /* list of classes */
    ScmObj cpl;                 /* list of classes */
    ScmObj accessors;           /* alist of slot-name & slot-accessor */
    ScmObj directSlots;         /* alist of slot-name & slot-definition */
    ScmObj slots;               /* alist of slot-name & slot-definition */
    ScmObj directSubclasses;    /* list of direct subclasses */
    ScmObj directMethods;       /* list of methods that has this class in
                                   its specializer */
    ScmObj initargs;            /* saved key-value list for redefinition */
    ScmObj modules;             /* modules where this class is defined */
    ScmObj redefined;           /* if this class is obsoleted by class
                                   redefinition, points to the new class.
                                   if this class is being redefined, points
                                   to a thread that is handling the
                                   redefinition.  (it won't be seen by
                                   Scheme; see class.c)
                                   otherwise #f */
    ScmInternalMutex mutex;     /* to protect from MT hazard */
    ScmInternalCond cv;         /* wait on this while a class being updated */
    void   *data;               /* extra data to do nasty trick.  See the note
                                   in class.c */
} SCM_ALIGN8;

typedef struct ScmClassStaticSlotSpecRec ScmClassStaticSlotSpec;

#define SCM_CLASS(obj)        ((ScmClass*)(obj))
#define SCM_CLASSP(obj)       SCM_ISA(obj, SCM_CLASS_CLASS)

#define SCM_CLASS_NUM_INSTANCE_SLOTS(obj)  SCM_CLASS(obj)->numInstanceSlots

/* Class categories

   In C level, there are four categories of classes.  The category of
   class can be obtained by masking the lower two bits of flags field.

   SCM_CLASS_BUILTIN
       An instance of this class doesn't have "slots" member (thus
       cannot be cast to ScmInstance*).   From Scheme level, this
       class cannot be redefined.   It cannot be inherited in Scheme
       code with the standard inheritance mechanism; though it can have
       subclasses, provided a special allocator and initializer.

   SCM_CLASS_ABSTRACT 
       This class is defined in C, but doesn't allowed to create an
       instance by its own.  It is intended to be used as a mixin from
       both C and Scheme-defined class.   This class shouldn't have
       C members other than SCM_HEADER.   This class cannot be redefined.

   SCM_CLASS_BASE
       This class is defined in C, and can be subclassed in Scheme.
       An instance of this class must have "slots" member and be
       able to be cast to ScmInstance.  The instance may have other
       C members.  This class cannot be redefined.

   SCM_CLASS_SCHEME
       A Scheme-defined class.  This class will have one or more
       SCM_CLASS_BASE classes in its CPL.  Specifically, <object>
       class is always included in its CPL.  This class can be
       redefined.

   This classification and its rules are to integrate C structures
   and Scheme classes.   C structure level inheritance has to be
   single-inheritance, with the subclass structure including its
   parent structure.  Scheme level inheritance is more flexible,
   but for that flexibility it has to have "slots" member in its
   instance (i.e. it has to be castable to ScmInstance*).

   Here's the basic inheritance rules:
                   
   - First, ABSTRACT class can be inserted at any place in the
     inheritance chain.  It doesn't affect C-level operation.  It is
     only to add the type information in Scheme-level.
     In the following rules we ignore ABSTRACT classes.

   - BASE class can be inherited from BASE classes, and its
     inheritance chain must form a single inheritance.

   - BUILTIN class can be inherited from BUILTIN classes, and
     its inheritance chain must form a single inheritance
     
   - SCHEME class can be inherited from SCHEME or BASE classes.
     It can inherite from multiple SCHEME and/or BASE classes.
*/

enum {
    SCM_CLASS_BUILTIN  = 0,
    SCM_CLASS_ABSTRACT = 1,
    SCM_CLASS_BASE     = 2,
    SCM_CLASS_SCHEME   = 3,

    /* A special flag that only be used for "natively applicable"
       objects, which basically inherits ScmProcedure. */
    SCM_CLASS_APPLICABLE = 0x04,

    /* If this flag is set, important slots such as class-precedence-list
       or class-slots becomes settable.
       We reset this flag at the end of class initialization, so that
       we can avoid the behavior of a class from being accidentally
       chnaged.  The flag may be set during updating a class metaobject
       triggered by metaclass change (see lib/gauche/redefutil.scm).
     */
    SCM_CLASS_MALLEABLE = 0x08
};

#define SCM_CLASS_FLAGS(obj)        (SCM_CLASS(obj)->flags)
#define SCM_CLASS_APPLICABLE_P(obj) (SCM_CLASS_FLAGS(obj)&SCM_CLASS_APPLICABLE)

#define SCM_CLASS_CATEGORY(obj)     (SCM_CLASS_FLAGS(obj)&3)
#define SCM_CLASS_MALLEABLE_P(obj)  (SCM_CLASS_FLAGS(obj)&SCM_CLASS_MALLEABLE)

SCM_EXTERN void Scm_InitStaticClass(ScmClass *klass, const char *name,
                                    ScmModule *mod,
                                    ScmClassStaticSlotSpec *slots,
                                    int flags);
SCM_EXTERN void Scm_InitStaticClassWithSupers(ScmClass *klass,
                                              const char *name,
                                              ScmModule *mod,
                                              ScmObj supers,
                                              ScmClassStaticSlotSpec *slots,
                                              int flags);
SCM_EXTERN void Scm_InitStaticClassWithMeta(ScmClass *klass,
                                            const char *name,
                                            ScmModule *mod,
                                            ScmClass *meta,
                                            ScmObj supers,
                                            ScmClassStaticSlotSpec *slots,
                                            int flags);

/* OBSOLETE */
SCM_EXTERN void Scm_InitBuiltinClass(ScmClass *c, const char *name,
                                     ScmClassStaticSlotSpec *slots,
                                     int withMeta,
                                     ScmModule *m);

SCM_EXTERN ScmClass *Scm_ClassOf(ScmObj obj);
SCM_EXTERN int Scm_SubtypeP(ScmClass *sub, ScmClass *type);
SCM_EXTERN int Scm_TypeP(ScmObj obj, ScmClass *type);
SCM_EXTERN ScmClass *Scm_BaseClassOf(ScmClass *klass);

SCM_EXTERN void   Scm_ClassMalleableSet(ScmClass *klass, int flag);

SCM_EXTERN ScmObj Scm_VMSlotRef(ScmObj obj, ScmObj slot, int boundp);
SCM_EXTERN ScmObj Scm_VMSlotSet(ScmObj obj, ScmObj slot, ScmObj value);
SCM_EXTERN ScmObj Scm_VMSlotBoundP(ScmObj obj, ScmObj slot);


/* built-in classes */
SCM_CLASS_DECL(Scm_TopClass);
SCM_CLASS_DECL(Scm_BottomClass);
SCM_CLASS_DECL(Scm_BoolClass);
SCM_CLASS_DECL(Scm_CharClass);
SCM_CLASS_DECL(Scm_ClassClass);
SCM_CLASS_DECL(Scm_EOFObjectClass);
SCM_CLASS_DECL(Scm_UndefinedObjectClass);
SCM_CLASS_DECL(Scm_UnknownClass);
SCM_CLASS_DECL(Scm_ObjectClass); /* base of Scheme-defined objects */
SCM_CLASS_DECL(Scm_ForeignPointerClass);


#define SCM_CLASS_TOP              (&Scm_TopClass)
#define SCM_CLASS_BOTTOM           (&Scm_BottomClass)
#define SCM_CLASS_BOOL             (&Scm_BoolClass)
#define SCM_CLASS_CHAR             (&Scm_CharClass)
#define SCM_CLASS_CLASS            (&Scm_ClassClass)
#define SCM_CLASS_EOF_OBJECT       (&Scm_EOFObjectClass)
#define SCM_CLASS_UNDEFINED_OBJECT (&Scm_UndefinedObjectClass)
#define SCM_CLASS_UNKNOWN          (&Scm_UnknownClass)
#define SCM_CLASS_OBJECT           (&Scm_ObjectClass)
#define SCM_CLASS_FOREIGN_POINTER  (&Scm_ForeignPointerClass)

/* NB: we can't use SCM_EXTERN because Windows DLL can't use the address of
   dllimport-ed variables as constants. */
extern ScmClass *Scm_DefaultCPL[];
extern ScmClass *Scm_ObjectCPL[];

#define SCM_CLASS_DEFAULT_CPL     (Scm_DefaultCPL)
#define SCM_CLASS_OBJECT_CPL      (Scm_ObjectCPL)

/* Static definition of classes
 *   SCM_DEFINE_BUILTIN_CLASS
 *   SCM_DEFINE_BUILTIN_CLASS_SIMPLE
 *   SCM_DEFINE_ABSTRACT_CLASS
 *   SCM_DEFINE_BASE_CLASS
 */

/* internal macro.  do not use directly */
#if defined(GAUCHE_BROKEN_LINKER_WORKAROUND)
#define SCM__CLASS_PTR_SLOT(cname)  (&SCM_CPP_CAT(_imp__, cname)),
#define SCM__CLASS_PTR_BODY(cname) \
    ; ScmClass *SCM_CPP_CAT(_imp__, cname) = &cname
#else  /*!GAUCHE_BROKEN_LINKER_WORKAROUND*/
#define SCM__CLASS_PTR_SLOT(cname)  /* none */
#define SCM__CLASS_PTR_BODY(cname)  /* none */
#endif /*!GAUCHE_BROKEN_LINKER_WORKAROUND*/

#define SCM__DEFINE_CLASS_COMMON(cname, coreSize, flag, printer, compare, serialize, allocate, cpa) \
    ScmClass cname = {                           \
        {{ SCM_CLASS_STATIC_TAG(Scm_ClassClass), NULL }},       \
        SCM__CLASS_PTR_SLOT(cname)               \
        printer,                                 \
        compare,                                 \
        serialize,                               \
        allocate,                                \
        cpa,                                     \
        0,        /*numInstanceSlots*/           \
        coreSize, /*coreSize*/                   \
        flag,     /*flags*/                      \
        SCM_FALSE,/*name*/                       \
        SCM_NIL,  /*directSupers*/               \
        SCM_NIL,  /*cpl*/                        \
        SCM_NIL,  /*accessors*/                  \
        SCM_NIL,  /*directSlots*/                \
        SCM_NIL,  /*slots*/                      \
        SCM_NIL,  /*directSubclasses*/           \
        SCM_NIL,  /*directMethods*/              \
        SCM_NIL,  /*initargs*/                   \
        SCM_NIL,  /*modules*/                    \
        SCM_FALSE, /*redefined*/                 \
        SCM_INTERNAL_MUTEX_INITIALIZER,          \
        SCM_INTERNAL_COND_INITIALIZER,           \
    } SCM__CLASS_PTR_BODY(cname)
    
/* Define built-in class statically -- full-featured version */
#define SCM_DEFINE_BUILTIN_CLASS(cname, printer, compare, serialize, allocate, cpa) \
    SCM__DEFINE_CLASS_COMMON(cname, 0,                    \
                             SCM_CLASS_BUILTIN,           \
                             printer, compare, serialize, allocate, cpa)

/* Define built-in class statically -- simpler version */
#define SCM_DEFINE_BUILTIN_CLASS_SIMPLE(cname, printer)         \
    SCM_DEFINE_BUILTIN_CLASS(cname, printer, NULL, NULL, NULL, NULL)

/* define an abstract class */
#define SCM_DEFINE_ABSTRACT_CLASS(cname, cpa)             \
    SCM__DEFINE_CLASS_COMMON(cname, 0,                    \
                             SCM_CLASS_ABSTRACT,          \
                             NULL, NULL, NULL, NULL, cpa)

/* define a class that can be subclassed by Scheme */
#define SCM_DEFINE_BASE_CLASS(cname, ctype, printer, compare, serialize, allocate, cpa) \
    SCM__DEFINE_CLASS_COMMON(cname, sizeof(ctype),        \
                             SCM_CLASS_BASE,              \
                             printer, compare, serialize, allocate, cpa)

/*
 * A simple class and instance API to wrap C pointer.
 * This is for C programs that want to define a visible class from Scheme
 * but don't want to go through full-fledged class mechanism.
 */
typedef struct ScmForeignPointerRec {
    SCM_HEADER;
    void *ptr;                  /* foreign object.  this pointer shouldn't
                                   be modified once <foreign-pointer> is
                                   constructed by Scm_MakeForeignPointer. */
    ScmObj attributes;          /* alist.  useful to store e.g. callbacks.
                                   use accessor procedures. */
    ScmWord flags;              /* used internally.  We use ScmWord to keep
                                   ScmForeignPointer fit in 4 words. */
} ScmForeignPointer;

#define SCM_FOREIGN_POINTER_P(obj)   SCM_ISA(obj, SCM_CLASS_FOREIGN_POINTER)
#define SCM_FOREIGN_POINTER(obj)     ((ScmForeignPointer*)(obj))
#define SCM_FOREIGN_POINTER_REF(type, obj) \
    ((type)(Scm_ForeignPointerRef(SCM_FOREIGN_POINTER(obj))))

typedef void (*ScmForeignCleanupProc)(ScmObj);

SCM_EXTERN ScmClass *Scm_MakeForeignPointerClass(ScmModule *module,
                                                 const char *name,
                                                 ScmClassPrintProc print,
                                                 ScmForeignCleanupProc cleanup,
                                                 int flags);
SCM_EXTERN ScmObj Scm_MakeForeignPointer(ScmClass *klass, void *ptr);
SCM_EXTERN ScmObj Scm_MakeForeignPointerWithAttr(ScmClass *klass, void *ptr,
                                                 ScmObj attr);
SCM_EXTERN void  *Scm_ForeignPointerRef(ScmForeignPointer *fp);
SCM_EXTERN int    Scm_ForeignPointerInvalidP(ScmForeignPointer *fp);
SCM_EXTERN void   Scm_ForeignPointerInvalidate(ScmForeignPointer *fp);

/* foreign pointer class flags */
enum {
    SCM_FOREIGN_POINTER_KEEP_IDENTITY = (1L<<0),
         /* If set, a foreign pointer class keeps a weak hash table that maps
            PTR to the wrapping ScmObj, so Scm_MakeForeignPointer returns
            eq? object if the same PTR is given.  This incurs some overhead,
            but cleanup procedure can safely free the foreign object without
            worring if there's other ScmObj that's pointing to PTR.
            Do not use this flag if PTR is also allocated by GC_malloc.  The
            used hash table is only weak for its value, so PTR wouldn't be
            GCed. */
    SCM_FOREIGN_POINTER_MAP_NULL = (1L<<1)
         /* If set, Scm_MakeForeignPointer returns SCM_FALSE whenever the
            given PTR is NULL.   It is the only case that
            Scm_MakeForeignPointer returns non-ForeignPointer object. */
};

/* foreign pointer attributes.  you can attach info to each foreign pointer.
   possible applications:
   - Keep Scheme objects that are set in the foreign object, preventing
     them from begin GCed.
   - Keep mutex to use the foreign object from multiple threads */

SCM_EXTERN ScmObj Scm_ForeignPointerAttr(ScmForeignPointer *fp);
SCM_EXTERN ScmObj Scm_ForeignPointerAttrGet(ScmForeignPointer *fp,
                                            ScmObj key, ScmObj fallback);
SCM_EXTERN ScmObj Scm_ForeignPointerAttrSet(ScmForeignPointer *fp,
                                            ScmObj key, ScmObj value);

/*--------------------------------------------------------
 * COLLECTION INTERFACE
 */

#include <gauche/collection.h>

/*--------------------------------------------------------
 * PAIR AND LIST
 */

/* Ordinary pair uses two words.  It can be distinguished from
 * other heap allocated objects by checking the first word doesn't
 * have "11" in the lower bits.
 */
struct ScmPairRec {
    ScmObj car;                 /* should be accessed via macros */
    ScmObj cdr;                 /* ditto */
};

/* To keep extra information such as source-code info, some pairs
 * actually have one extra word for attribute assoc-list.  Checking
 * whether a pair is an extended one or not isn't a very lightweight
 * operation, so the use of extended pair should be kept minimal.
 */
struct ScmExtendedPairRec {
    ScmObj car;                 /* should be accessed via macros */
    ScmObj cdr;                 /* ditto */
    ScmObj attributes;          /* should be accessed via API func. */
};

#if GAUCHE_LAZY_PAIR
#define SCM_PAIRP(obj)  \
    (SCM_HPTRP(obj)&&(SCM_HTAG(obj)!=7||Scm_PairP(SCM_OBJ(obj))))
#else  /*!GAUCHE_LAZY_PAIR*/
#define SCM_PAIRP(obj)          (SCM_HPTRP(obj)&&(SCM_HTAG(obj)!=7))
#endif /*!GAUCHE_LAZY_PAIR*/

#define SCM_PAIR(obj)           ((ScmPair*)(obj))
#define SCM_CAR(obj)            (SCM_PAIR(obj)->car)
#define SCM_CDR(obj)            (SCM_PAIR(obj)->cdr)
#define SCM_CAAR(obj)           (SCM_CAR(SCM_CAR(obj)))
#define SCM_CADR(obj)           (SCM_CAR(SCM_CDR(obj)))
#define SCM_CDAR(obj)           (SCM_CDR(SCM_CAR(obj)))
#define SCM_CDDR(obj)           (SCM_CDR(SCM_CDR(obj)))

#define SCM_SET_CAR(obj, value) (SCM_CAR(obj) = (value))
#define SCM_SET_CDR(obj, value) (SCM_CDR(obj) = (value))

#define SCM_EXTENDED_PAIR_P(obj) \
    (SCM_PAIRP(obj)&&GC_base(obj)&&GC_size(obj)>=sizeof(ScmExtendedPair))
#define SCM_EXTENDED_PAIR(obj)  ((ScmExtendedPair*)(obj))


SCM_CLASS_DECL(Scm_ListClass);
SCM_CLASS_DECL(Scm_PairClass);
SCM_CLASS_DECL(Scm_NullClass);
#define SCM_CLASS_LIST          (&Scm_ListClass)
#define SCM_CLASS_PAIR          (&Scm_PairClass)
#define SCM_CLASS_NULL          (&Scm_NullClass)

#define SCM_LISTP(obj)          (SCM_NULLP(obj) || SCM_PAIRP(obj))

/* Useful macros to manipulate lists. */

#define SCM_FOR_EACH(p, list) \
    for((p) = (list); SCM_PAIRP(p); (p) = SCM_CDR(p))

#define SCM_APPEND1(start, last, obj)                           \
    do {                                                        \
        if (SCM_NULLP(start)) {                                 \
            (start) = (last) = Scm_Cons((obj), SCM_NIL);        \
        } else {                                                \
            SCM_SET_CDR((last), Scm_Cons((obj), SCM_NIL));      \
            (last) = SCM_CDR(last);                             \
        }                                                       \
    } while (0)

#define SCM_APPEND(start, last, obj)                    \
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

/* special return value of Scm_Length */
enum {
    SCM_LIST_DOTTED = -1,       /* dotted list */
    SCM_LIST_CIRCULAR = -2      /* circular list */
};

#define SCM_PROPER_LIST_P(obj)   (Scm_Length(obj) >= 0)
#define SCM_DOTTED_LIST_P(obj)   (Scm_Length(obj) == SCM_LIST_DOTTED)
#define SCM_CIRCULAR_LIST_P(obj) (Scm_Length(obj) == SCM_LIST_CIRCULAR)

SCM_EXTERN ScmObj Scm_Cons(ScmObj car, ScmObj cdr);
SCM_EXTERN ScmObj Scm_Acons(ScmObj caar, ScmObj cdar, ScmObj cdr);
SCM_EXTERN ScmObj Scm_List(ScmObj elt, ...);
SCM_EXTERN ScmObj Scm_Conses(ScmObj elt, ...);
SCM_EXTERN ScmObj Scm_VaList(va_list elts);
SCM_EXTERN ScmObj Scm_VaCons(va_list elts);
SCM_EXTERN ScmObj Scm_ArrayToList(ScmObj *elts, int nelts);
SCM_EXTERN ScmObj Scm_ArrayToListWithTail(ScmObj *elts, int nelts, ScmObj tail);
SCM_EXTERN ScmObj *Scm_ListToArray(ScmObj list, int *nelts, ScmObj *store,
                                   int alloc);

SCM_EXTERN ScmObj Scm_Car(ScmObj obj);
SCM_EXTERN ScmObj Scm_Cdr(ScmObj obj);
SCM_EXTERN ScmObj Scm_Caar(ScmObj obj);
SCM_EXTERN ScmObj Scm_Cadr(ScmObj obj);
SCM_EXTERN ScmObj Scm_Cdar(ScmObj obj);
SCM_EXTERN ScmObj Scm_Cddr(ScmObj obj);

SCM_EXTERN int    Scm_Length(ScmObj obj);
SCM_EXTERN ScmObj Scm_CopyList(ScmObj list);
SCM_EXTERN ScmObj Scm_MakeList(ScmSmallInt len, ScmObj fill);
SCM_EXTERN ScmObj Scm_Append2X(ScmObj list, ScmObj obj);
SCM_EXTERN ScmObj Scm_Append2(ScmObj list, ScmObj obj);
SCM_EXTERN ScmObj Scm_Append(ScmObj args);
SCM_EXTERN ScmObj Scm_ReverseX(ScmObj list);
SCM_EXTERN ScmObj Scm_Reverse(ScmObj list);
SCM_EXTERN ScmObj Scm_Reverse2X(ScmObj list, ScmObj tail);
SCM_EXTERN ScmObj Scm_Reverse2(ScmObj list, ScmObj tail);
SCM_EXTERN ScmObj Scm_ListTail(ScmObj list, ScmSmallInt i, ScmObj fallback);
SCM_EXTERN ScmObj Scm_ListRef(ScmObj list, ScmSmallInt i, ScmObj fallback);
SCM_EXTERN ScmObj Scm_LastPair(ScmObj list);

SCM_EXTERN ScmObj Scm_Memq(ScmObj obj, ScmObj list);
SCM_EXTERN ScmObj Scm_Memv(ScmObj obj, ScmObj list);
SCM_EXTERN ScmObj Scm_Member(ScmObj obj, ScmObj list, int cmpmode);
SCM_EXTERN ScmObj Scm_Assq(ScmObj obj, ScmObj alist);
SCM_EXTERN ScmObj Scm_Assv(ScmObj obj, ScmObj alist);
SCM_EXTERN ScmObj Scm_Assoc(ScmObj obj, ScmObj alist, int cmpmode);

SCM_EXTERN ScmObj Scm_Delete(ScmObj obj, ScmObj list, int cmpmode);
SCM_EXTERN ScmObj Scm_DeleteX(ScmObj obj, ScmObj list, int cmpmode);
SCM_EXTERN ScmObj Scm_AssocDelete(ScmObj elt, ScmObj alist, int cmpmode);
SCM_EXTERN ScmObj Scm_AssocDeleteX(ScmObj elt, ScmObj alist, int cmpmode);

SCM_EXTERN ScmObj Scm_DeleteDuplicates(ScmObj list, int cmpmode);
SCM_EXTERN ScmObj Scm_DeleteDuplicatesX(ScmObj list, int cmpmode);

SCM_EXTERN ScmObj Scm_MonotonicMerge(ScmObj start, ScmObj sequences);
SCM_EXTERN ScmObj Scm_MonotonicMerge1(ScmObj sequences);
SCM_EXTERN ScmObj Scm_Union(ScmObj list1, ScmObj list2);
SCM_EXTERN ScmObj Scm_Intersection(ScmObj list1, ScmObj list2);

SCM_EXTERN ScmObj Scm_ExtendedCons(ScmObj car, ScmObj cdr);
SCM_EXTERN ScmObj Scm_PairAttr(ScmPair *pair);
SCM_EXTERN ScmObj Scm_PairAttrGet(ScmPair *pair, ScmObj key, ScmObj fallback);
SCM_EXTERN ScmObj Scm_PairAttrSet(ScmPair *pair, ScmObj key, ScmObj value);

/*--------------------------------------------------------
 * CHARACTERS
 */

/* Illegal character handling mode.  Used in some APIs that handles
   character conversion, such as input ports and string-incomplete->complete.
*/
typedef enum {
    SCM_ILLEGAL_CHAR_REJECT,    /* Refuse to handle illegal chars.  For ports
                                   this means raising an error.  For string
                                   conversion procedure, this makes it to
                                   return #f. */
    SCM_ILLEGAL_CHAR_OMIT,      /* Silently discard the illegal chars. */
    SCM_ILLEGAL_CHAR_REPLACE    /* Replace an illegal char to a substitute
                                   char, specified elsewhere. */
} ScmIllegalCharHandling;

    
/*--------------------------------------------------------
 * STRING
 */

#include <gauche/string.h>

/*--------------------------------------------------------
 * VECTOR
 */

#include <gauche/vector.h>

/*--------------------------------------------------------
 * PORT
 */

#include <gauche/port.h>


/*--------------------------------------------------------
 * WRITE
 */

#include <gauche/writer.h>

/*---------------------------------------------------------
 * READ
 */

#include <gauche/reader.h>

/*--------------------------------------------------------
 * HASHTABLE
 */

#include <gauche/hash.h>

/*--------------------------------------------------------
 * TREEMAP
 */

#include <gauche/treemap.h>

/*--------------------------------------------------------
 * WEAK VECTOR, WEAK BOX & WEAK HASH TABLES
 */

#include <gauche/weak.h>

/*--------------------------------------------------------
 * CHAR-SET
 */

#include <gauche/charset.h>

/*--------------------------------------------------------
 * MODULE
 */

#include <gauche/module.h>

/*--------------------------------------------------------
 * SYMBOL
 */

#include <gauche/symbol.h>

/*--------------------------------------------------------
 * GLOC
 */

#include <gauche/gloc.h>

/*--------------------------------------------------------
 * NUMBER
 */

#include <gauche/number.h>

/*--------------------------------------------------------
 * PROCEDURE (APPLICABLE OBJECT)
 */


typedef ScmObj (*ScmTransformerProc)(ScmObj self, ScmObj form, ScmObj env,
                                     void *data);

/* Base structure */
struct ScmProcedureRec {
    SCM_INSTANCE_HEADER;
    unsigned int required : 16;    /* # of required args */
    unsigned int optional : 8;     /* >=1 if it takes opt args. see below.*/
    unsigned int type     : 3;     /* ScmProcedureType */
    unsigned int locked   : 1;     /* setter locked? */
    unsigned int currying : 1;     /* autocurrying */
    unsigned int constant : 1;     /* constant procedure. see below. */
    unsigned int reserved : 2;     /* unused yet. */
    ScmObj info;                   /* source code info */
    ScmObj setter;                 /* setter, if exists. */
    ScmObj inliner;                /* inliner information.  see below. */
};

/* About optional slot:
   If this slot is non-zero, the procedure takes optional arguments.
   For Standard Scheme procedures with 'rest' arguments, this slot is 1
   and all excessive arguments are 'folded' in a list.

   This slot may have a value more than 1.  If it is N (>1), then up to N-1
   optional arguments are passed wihtout being folded (that is, passed
   'on the stack'.  Only when the given argument is more than or equal to
   N + reqargs, the excessive arguments are folded and passed in a list.
   Thus, such procedure may get between reqargs values and N+reqargs values
   after folding (NB: Fixed argument procedure always get regargs values,
   and standard Scheme variable argument procedure always get reqargs+1 values
   after argument folding).

   This special treatment is to avoid unnecessary consing of argumets;
   if we know the callee immediately unfolds the rest argument, it's no 
   use to fold excessive arguments anyway.
 */

/* About 'constant' flag:
   This flag being TRUE means this procedure returns the same constant
   value if given same constant arguments, and it does not have any other
   external effects.   The compiler may use this info to replace a call
   of this proc with the resulting value, if all the arguments are known
   at compile-time.  The resulting value must be serializable to the
   precompiled file.  The result shouldn't be affected
   by the timing of the compile, architecture on which the compiler runs,
   or the compiler configuration (e.g. internal encoding).
 */

/* About procedure inliner:
   This slot holds information to inline procedures.  The value of this slot
   can be one of the following kinds:
   
   #f: No inliner associated to this procedure.  (For historical
      reasons, the code that access to this slot expects this slot can be
      NULL and treats it as SCM_FALSE in that case)

   <integer>: Only appears in some built-in procedures, and specifies
      the VM instruction number.  This should be considered as a special
      hack.   The set of procedures that can have this type of inliner
      is tied to the VM definition.

   <vector>: Procedures defined with define-inline have this.  The vector
      encodes intermediate form (IForm) of the procedure code, which will be
      expanded into the caller.
 */

/* procedure type */
enum ScmProcedureType {
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
#define SCM_PROCEDURE_CURRYING(obj) SCM_PROCEDURE(obj)->currying
#define SCM_PROCEDURE_INFO(obj)     SCM_PROCEDURE(obj)->info
#define SCM_PROCEDURE_SETTER(obj)   SCM_PROCEDURE(obj)->setter
#define SCM_PROCEDURE_INLINER(obj)  SCM_PROCEDURE(obj)->inliner

SCM_CLASS_DECL(Scm_ProcedureClass);
#define SCM_CLASS_PROCEDURE    (&Scm_ProcedureClass)
#define SCM_PROCEDUREP(obj) \
    (SCM_HOBJP(obj) && SCM_CLASS_APPLICABLE_P(SCM_CLASS_OF(obj)))
#define SCM_PROCEDURE_TAKE_NARG_P(obj, narg) \
    (SCM_PROCEDUREP(obj)&& \
     (  (!SCM_PROCEDURE_OPTIONAL(obj)&&SCM_PROCEDURE_REQUIRED(obj)==(narg)) \
      ||(SCM_PROCEDURE_OPTIONAL(obj)&&SCM_PROCEDURE_REQUIRED(obj)<=(narg))))
#define SCM_PROCEDURE_THUNK_P(obj) \
    (SCM_PROCEDUREP(obj)&& \
     (  (!SCM_PROCEDURE_OPTIONAL(obj)&&SCM_PROCEDURE_REQUIRED(obj)==0) \
      ||(SCM_PROCEDURE_OPTIONAL(obj))))
#define SCM_PROCEDURE_INIT(obj, req, opt, typ, inf)     \
    SCM_PROCEDURE(obj)->required = req,                 \
    SCM_PROCEDURE(obj)->optional = opt,                 \
    SCM_PROCEDURE(obj)->type = typ,                     \
    SCM_PROCEDURE(obj)->locked = FALSE,                 \
    SCM_PROCEDURE(obj)->currying = FALSE,               \
    SCM_PROCEDURE(obj)->constant = FALSE,               \
    SCM_PROCEDURE(obj)->reserved = 0,                   \
    SCM_PROCEDURE(obj)->info = inf,                     \
    SCM_PROCEDURE(obj)->setter = SCM_FALSE,             \
    SCM_PROCEDURE(obj)->inliner = SCM_FALSE

#define SCM__PROCEDURE_INITIALIZER(klass, req, opt, typ, cst, inf, inl)  \
    { { klass }, (req), (opt), (typ), FALSE, FALSE, cst, 0,              \
      (inf), SCM_FALSE, (inl) }

SCM_EXTERN ScmObj Scm_CurryProcedure(ScmObj proc, ScmObj *given,
                                     int ngiven, int foldlen);

/* Closure - Scheme defined procedure */
struct ScmClosureRec {
    ScmProcedure common;
    ScmObj code;                /* compiled code */
    ScmEnvFrame *env;           /* environment */
};

#define SCM_CLOSUREP(obj) \
    (SCM_PROCEDUREP(obj)&&(SCM_PROCEDURE_TYPE(obj)==SCM_PROC_CLOSURE))
#define SCM_CLOSURE(obj)           ((ScmClosure*)(obj))

SCM_EXTERN ScmObj Scm_MakeClosure(ScmObj code, ScmEnvFrame *env);

/* Subr - C defined procedure */
struct ScmSubrRec {
    ScmProcedure common;
    int flags;
    ScmSubrProc *func;
    void *data;
};

#define SCM_SUBRP(obj) \
    (SCM_PROCEDUREP(obj)&&(SCM_PROCEDURE_TYPE(obj)==SCM_PROC_SUBR))
#define SCM_SUBR(obj)              ((ScmSubr*)(obj))
#define SCM_SUBR_FLAGS(obj)        SCM_SUBR(obj)->flags
#define SCM_SUBR_FUNC(obj)         SCM_SUBR(obj)->func
#define SCM_SUBR_DATA(obj)         SCM_SUBR(obj)->data

/* flags */
#define SCM_SUBR_IMMEDIATE_ARG  (1L<<0) /* This subr will not retain a reference
                                           to the flonums given to args.  VM
                                           can safely pass the register flonums
                                           to the subr.  This is added when
                                           the :fast-flonum flag is given to
                                           define-cproc. */

#define SCM__DEFINE_SUBR_INT(cvar, req, opt, cst, inf, flags, func, inliner, data) \
    ScmSubr cvar = {                                                        \
        SCM__PROCEDURE_INITIALIZER(SCM_CLASS_STATIC_TAG(Scm_ProcedureClass),\
            req, opt, SCM_PROC_SUBR, cst, inf, inliner),                    \
        flags, (func), (data)                                               \
    }

#define SCM_DEFINE_SUBR(cvar, req, opt, inf, func, inliner, data) \
    SCM__DEFINE_SUBR_INT(cvar, req, opt, 0, inf, 0, func, inliner, data)
#define SCM_DEFINE_SUBRX(cvar, req, opt, cst, inf, flags, func, inliner, data) \
    SCM__DEFINE_SUBR_INT(cvar, req, opt, cst, inf, flags, func, inliner, data)

/* TRANSIENT: old interface.  will be gone. */
#define SCM_DEFINE_SUBRI(cvar, req, opt, inf, func, inliner, data) \
    SCM__DEFINE_SUBR_INT(cvar, req, opt, 0, inf, SCM_SUBR_IMMEDIATE_ARG, \
                         func, inliner, data)

SCM_EXTERN ScmObj Scm_MakeSubr(ScmSubrProc *func,
                               void *data,
                               int required, int optional,
                               ScmObj info);
SCM_EXTERN ScmObj Scm_NullProc(void);

SCM_EXTERN ScmObj Scm_SetterSet(ScmProcedure *proc, ScmProcedure *setter,
                                int lock);
SCM_EXTERN ScmObj Scm_Setter(ScmObj proc);
SCM_EXTERN int    Scm_HasSetter(ScmObj proc);

/* Generic - Generic function */
struct ScmGenericRec {
    ScmProcedure common;
    ScmObj methods;             /* list of methods */
    int   maxReqargs;           /* maximum # of args required to select
                                   applicable methods */
    ScmObj (*fallback)(ScmObj *argv, int argc, ScmGeneric *gf);
    void *data;
    ScmInternalMutex lock;
};

SCM_CLASS_DECL(Scm_GenericClass);
#define SCM_CLASS_GENERIC          (&Scm_GenericClass)
#define SCM_GENERICP(obj)          SCM_XTYPEP(obj, SCM_CLASS_GENERIC)
#define SCM_GENERIC(obj)           ((ScmGeneric*)obj)
#define SCM_GENERIC_DATA(obj)      (SCM_GENERIC(obj)->data)

#define SCM_DEFINE_GENERIC(cvar, cfunc, data)                           \
    ScmGeneric cvar = {                                                 \
        SCM__PROCEDURE_INITIALIZER(SCM_CLASS_STATIC_TAG(Scm_GenericClass),\
                                   0, 0, SCM_PROC_GENERIC, 0,           \
                                   SCM_FALSE, NULL),                    \
        SCM_NIL, 0, cfunc, data                                         \
    }

SCM_EXTERN void Scm_InitBuiltinGeneric(ScmGeneric *gf, const char *name,
                                       ScmModule *mod);
SCM_EXTERN ScmObj Scm_MakeBaseGeneric(ScmObj name,
                                      ScmObj (*fallback)(ScmObj *, int, ScmGeneric*),
                                      void *data);
SCM_EXTERN ScmObj Scm_NoNextMethod(ScmObj *argv, int argc, ScmGeneric *gf);
SCM_EXTERN ScmObj Scm_NoOperation(ScmObj *argv, int argc, ScmGeneric *gf);
SCM_EXTERN ScmObj Scm_InvalidApply(ScmObj *argv, int argc, ScmGeneric *gf);

/* Method - method
   A method can be defined either by C or by Scheme.  C-defined method
   have func ptr, with optional data.   Scheme-define method has NULL
   in func, code in data, and optional environment in env. */
struct ScmMethodRec {
    ScmProcedure common;
    ScmGeneric *generic;
    ScmClass **specializers;    /* array of specializers, size==required */
    ScmObj (*func)(ScmNextMethod *nm, ScmObj *argv, int argc, void * data);
    void *data;                 /* closure, or code */
    ScmEnvFrame *env;           /* environment (for Scheme created method) */
};

SCM_CLASS_DECL(Scm_MethodClass);
#define SCM_CLASS_METHOD           (&Scm_MethodClass)
#define SCM_METHODP(obj)           SCM_ISA(obj, SCM_CLASS_METHOD)
#define SCM_METHOD(obj)            ((ScmMethod*)obj)

#define SCM_DEFINE_METHOD(cvar, gf, req, opt, specs, func, data)        \
    ScmMethod cvar = {                                                  \
        SCM__PROCEDURE_INITIALIZER(SCM_CLASS_STATIC_TAG(Scm_MethodClass),\
                                   req, opt, SCM_PROC_METHOD, 0,        \
                                   SCM_FALSE, NULL),                    \
        gf, specs, func, data, NULL                                     \
    }

SCM_EXTERN void Scm_InitBuiltinMethod(ScmMethod *m);

/* Next method object
   Next method is just another callable entity, with memoizing
   the arguments. */
struct ScmNextMethodRec {
    ScmProcedure common;
    ScmGeneric *generic;
    ScmObj methods;          /* list of applicable methods */
    ScmObj *argv;            /* original arguments */
    int argc;                /* # of original arguments */
    int applyargs;           /* if TRUE, argv[argc-1] has a list of rest args */
};

SCM_CLASS_DECL(Scm_NextMethodClass);
#define SCM_CLASS_NEXT_METHOD      (&Scm_NextMethodClass)
#define SCM_NEXT_METHODP(obj)      SCM_XTYPEP(obj, SCM_CLASS_NEXT_METHOD)
#define SCM_NEXT_METHOD(obj)       ((ScmNextMethod*)obj)

/* Calling a Scheme function from C
 *
 *  static ScmObj proc = SCM_UNDEFINED;
 *
 *  SCM_BIND_PROC(proc, "scheme-proc-name", module);
 *
 *  Scm_ApplyRec(proc, args);
 *   or
 *  Scm_Apply(proc, args, &result);
 *
 * SCM_BIND_PROC macro initializes the C variable proc to the value of
 * the global Scheme variable scheme-proc-name in the module.
 * It is idempotent operation, so it's MT-safe.
 */
#define SCM_BIND_PROC(var, name, module)                                \
    do {                                                                \
        if (SCM_UNDEFINEDP(var)) {                                      \
            ScmObj v__ =                                                \
                Scm_GlobalVariableRef(module,                           \
                                      SCM_SYMBOL(SCM_INTERN(name)),     \
                                      0);                               \
            if (SCM_UNBOUNDP(v__)) {                                    \
                Scm_Error("Procedure %s is unbound", name);             \
            }                                                           \
            var = v__;                                                  \
        }                                                               \
    } while (0)


/* OBSOLETED - These are defined in Scheme now. */
SCM_EXTERN ScmObj Scm_ForEach1(ScmObj proc, ScmObj args);
SCM_EXTERN ScmObj Scm_ForEach(ScmObj proc, ScmObj arg1, ScmObj args);
SCM_EXTERN ScmObj Scm_Map1(ScmObj proc, ScmObj args);
SCM_EXTERN ScmObj Scm_Map(ScmObj proc, ScmObj arg1, ScmObj args);

/*--------------------------------------------------------
 * MACROS AND SYNTAX
 */

/* The actual definitions of ScmSyntax and ScmMacro are private.*/

#define SCM_SYNTAX(obj)             ((ScmSyntax*)(obj))
#define SCM_SYNTAXP(obj)            SCM_XTYPEP(obj, SCM_CLASS_SYNTAX)
SCM_CLASS_DECL(Scm_SyntaxClass);
#define SCM_CLASS_SYNTAX            (&Scm_SyntaxClass)

SCM_EXTERN ScmObj Scm_MakeSyntax(ScmSymbol *name, ScmObj handler);

#define SCM_MACRO(obj)             ((ScmMacro*)(obj))
#define SCM_MACROP(obj)            SCM_XTYPEP(obj, SCM_CLASS_MACRO)
SCM_CLASS_DECL(Scm_MacroClass);
#define SCM_CLASS_MACRO            (&Scm_MacroClass)

SCM_EXTERN ScmObj Scm_MakeMacro(ScmSymbol *name, ScmObj transformer);
SCM_EXTERN ScmObj Scm_MacroTransformer(ScmMacro *mac);

SCM_EXTERN ScmObj Scm_MakeMacroTransformer(ScmSymbol *name,
                                           ScmObj proc);
SCM_EXTERN ScmObj Scm_MakeMacroAutoload(ScmSymbol *name,
                                        ScmAutoload *al);

SCM_EXTERN ScmObj Scm_UnwrapSyntax(ScmObj form);

/*--------------------------------------------------------
 * PROMISE
 */

struct ScmPromiseRec {
    SCM_HEADER;
    ScmObj kind;                /* promise kind */
    struct ScmPromiseContentRec *content; /* opaque */
};

SCM_CLASS_DECL(Scm_PromiseClass);
#define SCM_CLASS_PROMISE           (&Scm_PromiseClass)
#define SCM_PROMISE(obj)            ((ScmPromise*)(obj))
#define SCM_PROMISEP(obj)           SCM_XTYPEP(obj, SCM_CLASS_PROMISE)

SCM_EXTERN ScmObj Scm_MakePromise(int forced, ScmObj code);
SCM_EXTERN ScmObj Scm_Force(ScmObj p);

/* Lazy pair structur is opaque to public.  Whenever you apply to an
   ScmObj SCM_PAIRP, a lazy pair morphs itself to a pair, so the normal
   code never see lazy pairs. */

SCM_CLASS_DECL(Scm_LazyPairClass);
#define SCM_CLASS_LAZY_PAIR        (&Scm_LazyPairClass)
#define SCM_LAZY_PAIR(obj)         ((ScmLazyPair*)(obj))
#define SCM_LAZY_PAIR_P(obj)       SCM_XTYPEP(obj, SCM_CLASS_LAZY_PAIR)

SCM_EXTERN ScmObj Scm_MakeLazyPair(ScmObj item, ScmObj generator);
SCM_EXTERN int    Scm_DecomposeLazyPair(ScmObj obj, ScmObj *item, ScmObj *generator);
SCM_EXTERN ScmObj Scm_ForceLazyPair(volatile ScmLazyPair *lp);
SCM_EXTERN int Scm_PairP(ScmObj x);

/*--------------------------------------------------------
 * condition
 */

/* Condition classes are defined in a separate file */
#include <gauche/exception.h>

/* 'reason' flag for Scm_PortError */
enum {
    SCM_PORT_ERROR_INPUT,
    SCM_PORT_ERROR_OUTPUT,
    SCM_PORT_ERROR_CLOSED,
    SCM_PORT_ERROR_UNIT,
    SCM_PORT_ERROR_OTHER
};

/* Throwing error */
SCM_EXTERN void Scm_Error(const char *msg, ...);
SCM_EXTERN void Scm_SysError(const char *msg, ...);
SCM_EXTERN void Scm_TypeError(const char *what,
                              const char *expected, ScmObj got);
SCM_EXTERN void Scm_PortError(ScmPort *port, int reason, const char *msg, ...);

/* common pattern */
#define SCM_TYPE_ERROR(arg, expected)  Scm_TypeError(#arg, expected, arg)

SCM_EXTERN void Scm_Warn(const char *msg, ...);
SCM_EXTERN void Scm_FWarn(ScmString *fmt, ScmObj args);

/* TRANSIENT: Scm_Raise2 is to keep ABI compatibility.  Will be gone
   in 1.0.  */
#if    GAUCHE_API_0_95
SCM_EXTERN ScmObj Scm_Raise(ScmObj exception, u_long flags);
#define Scm_Raise2(e, f)  Scm_Raise(e, f)
#else  /*!GAUCHE_API_0_95*/
SCM_EXTERN ScmObj Scm_Raise(ScmObj exception);
SCM_EXTERN ScmObj Scm_Raise2(ScmObj exception, u_long flags);
#endif /*!GAUCHE_API_0_95*/

/* flags for Scm_Raise */
enum {
    SCM_RAISE_NON_CONTINUABLE = (1L<<0)
};

SCM_EXTERN ScmObj Scm_RaiseCondition(ScmObj conditionType, ...);

/* A marker to insert between key-value pair and formatting string
   in Scm_RaiseCondition. */
#define SCM_RAISE_CONDITION_MESSAGE  ((const char *)1)

SCM_EXTERN int    Scm_ConditionHasType(ScmObj c, ScmObj k);
SCM_EXTERN ScmObj Scm_ConditionMessage(ScmObj c);
SCM_EXTERN ScmObj Scm_ConditionTypeName(ScmObj c);

enum {
    /* predefined stack trace formats.  EXPERIMENTAL. */
    SCM_STACK_TRACE_FORMAT_ORIGINAL, /* original format */
    SCM_STACK_TRACE_FORMAT_CC        /* compiler-message-like format */
};

SCM_EXTERN void Scm_ShowStackTrace(ScmPort *out, ScmObj stacklite,
                                   int maxdepth, int skip, int offset,
                                   int format);

/* TRANSIENT: Scm_ReportErrr2 is to keep ABI compatibility.  Will be gone
   in 1.0.  */
#if    GAUCHE_API_0_95
SCM_EXTERN ScmObj Scm_ReportError(ScmObj e, ScmObj out);
#else  /*!GAUCHE_API_0_95*/
SCM_EXTERN ScmObj Scm_ReportError(ScmObj e);
SCM_EXTERN ScmObj Scm_ReportError2(ScmObj e, ScmObj out);
#endif /*!GAUCHE_API_0_95*/

/*--------------------------------------------------------
 * REGEXP
 */

/* The definition of Scm_RegexpRec and Scm_RegeMatchRec is hidden
   in gauche/regexp.h */

SCM_CLASS_DECL(Scm_RegexpClass);
#define SCM_CLASS_REGEXP          (&Scm_RegexpClass)
#define SCM_REGEXP(obj)           ((ScmRegexp*)obj)
#define SCM_REGEXPP(obj)          SCM_XTYPEP(obj, SCM_CLASS_REGEXP)

/* flags */
#define SCM_REGEXP_CASE_FOLD      (1L<<0)
#define SCM_REGEXP_PARSE_ONLY     (1L<<1)

SCM_EXTERN ScmObj Scm_RegComp(ScmString *pattern, int flags);
SCM_EXTERN ScmObj Scm_RegCompFromAST(ScmObj ast);
SCM_EXTERN ScmObj Scm_RegOptimizeAST(ScmObj ast);
SCM_EXTERN ScmObj Scm_RegExec(ScmRegexp *rx, ScmString *input);
SCM_EXTERN void Scm_RegDump(ScmRegexp *rx);

SCM_CLASS_DECL(Scm_RegMatchClass);
#define SCM_CLASS_REGMATCH        (&Scm_RegMatchClass)
#define SCM_REGMATCH(obj)         ((ScmRegMatch*)obj)
#define SCM_REGMATCHP(obj)        SCM_XTYPEP(obj, SCM_CLASS_REGMATCH)

SCM_EXTERN ScmObj Scm_RegMatchSubstr(ScmRegMatch *rm, ScmObj obj);
SCM_EXTERN ScmObj Scm_RegMatchStart(ScmRegMatch *rm, ScmObj obj);
SCM_EXTERN ScmObj Scm_RegMatchEnd(ScmRegMatch *rm, ScmObj obj);
SCM_EXTERN ScmObj Scm_RegMatchAfter(ScmRegMatch *rm, ScmObj obj);
SCM_EXTERN ScmObj Scm_RegMatchBefore(ScmRegMatch *rm, ScmObj obj);
SCM_EXTERN void Scm_RegMatchDump(ScmRegMatch *match);

/*-------------------------------------------------------
 * STUB MACROS
 */
#define SCM_ENTER_SUBR(name)

#define SCM_ARGREF(count)           (SCM_FP[count])
#define SCM_RETURN(value)           return value
#define SCM_CURRENT_MODULE()        (Scm_VM()->module)
#define SCM_VOID_RETURN_VALUE(expr) ((void)(expr), SCM_UNDEFINED)

#define SCM_MAYBE_P(pred, obj)      (SCM_FALSEP(obj)||(pred(obj)))
#define SCM_MAYBE(unboxer, obj)     (SCM_FALSEP(obj)?NULL:(unboxer(obj)))
#define SCM_MAKE_MAYBE(boxer, obj)  ((obj)?(boxer(obj)):SCM_FALSE)

/*---------------------------------------------------
 * SIGNAL
 */

typedef struct ScmSysSigsetRec {
    SCM_HEADER;
    sigset_t set;
} ScmSysSigset;

SCM_CLASS_DECL(Scm_SysSigsetClass);
#define SCM_CLASS_SYS_SIGSET   (&Scm_SysSigsetClass)
#define SCM_SYS_SIGSET(obj)    ((ScmSysSigset*)(obj))
#define SCM_SYS_SIGSET_P(obj)  SCM_XTYPEP(obj, SCM_CLASS_SYS_SIGSET)

SCM_EXTERN ScmObj Scm_SysSigsetOp(ScmSysSigset*, ScmObj, int);
SCM_EXTERN ScmObj Scm_SysSigsetFill(ScmSysSigset*, int);
SCM_EXTERN ScmObj Scm_GetSignalHandler(int);
SCM_EXTERN ScmObj Scm_GetSignalHandlerMask(int);
SCM_EXTERN ScmObj Scm_GetSignalHandlers(void);
SCM_EXTERN ScmObj Scm_SetSignalHandler(ScmObj, ScmObj, ScmSysSigset*);
SCM_EXTERN ScmObj Scm_SysSigmask(int how, ScmSysSigset *newmask);
SCM_EXTERN ScmObj Scm_Pause(void);
SCM_EXTERN ScmObj Scm_SigSuspend(ScmSysSigset *mask);
SCM_EXTERN int    Scm_SigWait(ScmSysSigset *mask);
SCM_EXTERN sigset_t Scm_GetMasterSigmask(void);
SCM_EXTERN void   Scm_SetMasterSigmask(sigset_t *set);
SCM_EXTERN ScmObj Scm_SignalName(int signum);
SCM_EXTERN void   Scm_ResetSignalHandlers(sigset_t *mask);

SCM_EXTERN void   Scm_GetSigmask(sigset_t *mask);
SCM_EXTERN void   Scm_SetSigmask(sigset_t *mask);

/*---------------------------------------------------
 * SYSTEM
 */

#include <gauche/system.h>

/*---------------------------------------------------
 * LOAD AND DYNAMIC LINK
 */

#include <gauche/load.h>

/*---------------------------------------------------
 * PROFILER INTERFACE
 */

SCM_EXTERN void   Scm_ProfilerStart(void);
SCM_EXTERN int    Scm_ProfilerStop(void);
SCM_EXTERN void   Scm_ProfilerReset(void);

/*---------------------------------------------------
 * UTILITY STUFF
 */

/* Program start and termination */

SCM_EXTERN void Scm_Init(const char *signature);
SCM_EXTERN int  Scm_InitializedP(void);
SCM_EXTERN void Scm_Cleanup(void);
SCM_EXTERN void Scm_Exit(int code) SCM_NORETURN;
SCM_EXTERN void Scm_Abort(const char *msg) SCM_NORETURN;
SCM_EXTERN void Scm_Panic(const char *msg, ...) SCM_NORETURN;
SCM_EXTERN ScmObj Scm_InitCommandLine(int argc, const char *argv[]);

SCM_EXTERN void Scm_SimpleMain(int argc, const char *argv[],
                               const char *script, u_long flags);

SCM_EXTERN void Scm_GC(void);
SCM_EXTERN void Scm_PrintStaticRoots(void);
SCM_EXTERN void Scm_RegisterDL(void *data_start, void *data_end,
                               void *bss_start, void *bss_end);
SCM_EXTERN void Scm_GCSentinel(void *obj, const char *name);

SCM_EXTERN ScmObj Scm_GetFeatures(void);
SCM_EXTERN void   Scm_AddFeature(const char *feature, const char *mod);

SCM_EXTERN void *Scm_AddCleanupHandler(void (*proc)(void *data), void *data);
SCM_EXTERN void  Scm_DeleteCleanupHandler(void *handle);

/* repl */
SCM_EXTERN void Scm_Repl(ScmObj reader, ScmObj evaluator, ScmObj printer,
                         ScmObj prompter);

/* Inspect the configuration */
SCM_EXTERN const char *Scm_HostArchitecture(void);

SCM_EXTERN ScmObj Scm_LibraryDirectory(void);
SCM_EXTERN ScmObj Scm_ArchitectureDirectory(void);
SCM_EXTERN ScmObj Scm_SiteLibraryDirectory(void);
SCM_EXTERN ScmObj Scm_SiteArchitectureDirectory(void);
SCM_EXTERN ScmObj Scm__RuntimeDirectory(void); /* private */

/* Compare and Sort */

#include <gauche/compare.h>

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

#include <gauche/scmconst.h>

SCM_DECL_END

#endif /* GAUCHE_H */
