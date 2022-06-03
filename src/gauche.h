/*
 * gauche.h - Gauche scheme system header
 *
 *   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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

/* GAUCHE_API_VERSION is GAUCHE_MAJOR_VERSION*1000 + revision.
   The revision is only incremented when we change API, which we expect
   rare during the same major revision.

   As of 0.9.10, API_VERSION is 97, corresponding to libgauche-0.97.so.

   As of 0.9.11, API_VERSION is 98, corresponding to libgauche-0.98.so.

   This can be overridden by --with-api-version configure option.
 */
#ifndef GAUCHE_API_VERSION
#define GAUCHE_API_VERSION  98
//#define GAUCHE_API_VERSION 1000
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
#include <stdint.h>
#include <inttypes.h>
#include <gauche/int64.h>

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

typedef ssize_t ScmSize;

/* For Windows platforms, we need some compatibility tricks.
   This defines GAUCHE_WINDOWS preprocessor symbol.
   (This should come before including gc.h) */
#if defined(__MINGW32__) || defined(MSVC)
#include <gauche/win-compat.h>
#endif /* MINGW32 || WINDOWS */

/* Defines SCM_EXTERN magic. */
#include <gauche/extern.h>

#if defined(LIBGAUCHE_BODY)
/* We incoporate gc files within libgauche, so we need to refer to them
   "internally" while we're compiling gauche body.  SCM_EXTERN handles it. */
#define GC_API SCM_EXTERN
#if !defined(GC_DLL)
#define GC_DLL    /* for gc.h to handle Win32 crazyness */
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

/* This must come after gauche/extern.h */
#include <gauche/float.h>

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

/* Temporary - to test alignment of pairs */
#define GAUCHE_CHECK_PAIR_ALIGNMENT 0

/* Enable an option to make keywords and symbols disjoint.
   (Transient: Will be gone once we completely migrate to
   unified keyword-symbol system */
#define GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION 1

/* Experimental: Enable lightweight continuation capturing in exception
   handling. */
#define GAUCHE_SPLIT_STACK 0

/* Define to 1 if debugging the bootstrap module */
#define GAUCHE_DEBUG_BOOTSTRAP 0

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
   Some platform doesn't align static double in 8-byte boundaries, so
   we try this as well.  */
#ifdef __GNUC__
#define SCM_ALIGN8  __attribute__ ((aligned (8)))
#else  /* !__GNUC__ */
#define SCM_ALIGN8  /*empty*/
#endif /* !__GNUC__ */

/* Statically allocated ScmPair must be aligned in two ScmWords boundary.*/
#ifdef __GNUC__
#define SCM_PAIR_ALWAYS_ALIGNED_EVEN_WORDS  1
#define SCM_ALIGN_PAIR  __attribute__ ((aligned(sizeof(ScmWord)*2)))
#else  /* !__GNUC__ */
#define SCM_PAIR_ALWAYS_ALIGNED_EVEN_WORDS  0
#define SCM_ALIGN_PAIR  /*empty*/
#endif /* !__GNUC__ */

/* 'No return' attribute */
#ifdef __GNUC__
#define SCM_NORETURN  __attribute__((__noreturn__))
#else  /*__GNUC__*/
#define SCM_NORETURN  /*empty*/
#endif /*__GNUC__*/

/* 'unused' attribute */
#ifdef __GNUC__
#define SCM_UNUSED   __attribute__((__unused__))
#else  /*__GNUC__*/
#define SCM_UNUSED  /*empty*/
#endif /*__GNUC__*/

/* 'noinline' attribute */
#ifdef __GNUC__
#define SCM_NOINLINE __attribute__((__noinline__))
#else  /*__GNUC__*/
#define SCM_NOINLINE  /*empty*/
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
 *      #f, #t, '(), eof-object, undefined, uninitialized
 *
 * [Pattern variable]
 *      -------- -------- -------- 00010011
 *      Used in macro expander.
 *
 * [String cursor]
 *      -------- -------- -------- 00011011
 *      Represent short string cursors.
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
#define SCM_UNINITIALIZED   SCM_OBJ(SCM__MAKE_ITAG(6)) /* uninitialized */

#define SCM_FALSEP(obj)         ((obj) == SCM_FALSE)
#define SCM_TRUEP(obj)          ((obj) == SCM_TRUE)
#define SCM_NULLP(obj)          ((obj) == SCM_NIL)
#define SCM_EOFP(obj)           ((obj) == SCM_EOF)
#define SCM_UNDEFINEDP(obj)     ((obj) == SCM_UNDEFINED)
#define SCM_UNBOUNDP(obj)       ((obj) == SCM_UNBOUND)
#define SCM_UNINITIALIZEDP(obj) ((obj) == SCM_UNINITIALIZED)

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
#define SCM_MAKE_INT(obj)    SCM_OBJ(((uintptr_t)(obj) << 2) + 1)

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
#define SCM_MAKE_CHAR(ch)       SCM_OBJ((intptr_t)(((unsigned long)(ch))<<8) + 3)

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

#if   defined(GAUCHE_CHAR_ENCODING_EUC_JP)
#include "gauche/char_euc_jp.h"
#elif defined(GAUCHE_CHAR_ENCODING_UTF_8)
#include "gauche/char_utf_8.h"
#elif defined(GAUCHE_CHAR_ENCODING_SJIS)
#include "gauche/char_sjis.h"
#else
#include "gauche/char_none.h"
#endif

/* Character lexer category.  See 7.1.1 of R7RS */
typedef enum {
    SCM_CHAR_INITIAL,
    SCM_CHAR_SUBSEQUENT,
    SCM_CHAR_SIGN_SUBSEQUENT,
} ScmCharLexerCategory;

SCM_EXTERN int Scm_CharLexerCategoryP(ScmChar c, ScmCharLexerCategory cat);

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
   You can use these only if SCM_HOBJP(obj) != FALSE.
   To get a class of a given object, always use Scm_ClassOf().
 */
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

typedef struct ScmVMRec             ScmVM;
typedef struct ScmPairRec           ScmPair;
typedef struct ScmExtendedPairRec   ScmExtendedPair;
typedef struct ScmLazyPairRec       ScmLazyPair;
typedef struct ScmCharSetRec        ScmCharSet;
typedef struct ScmStringRec         ScmString;
typedef struct ScmDStringRec        ScmDString;
typedef struct ScmVectorRec         ScmVector;
typedef struct ScmBignumRec         ScmBignum;
typedef struct ScmRatnumRec         ScmRatnum;
typedef struct ScmCompnumRec        ScmCompnum;
typedef struct ScmPortRec           ScmPort;
typedef struct ScmHashTableRec      ScmHashTable;
typedef struct ScmTreeMapRec        ScmTreeMap;
typedef struct ScmModuleRec         ScmModule;
typedef struct ScmSymbolRec         ScmSymbol;
typedef struct ScmGlocRec           ScmGloc;
typedef struct ScmProcedureRec      ScmProcedure;
typedef struct ScmClosureRec        ScmClosure;
typedef struct ScmSubrRec           ScmSubr;
typedef struct ScmGenericRec        ScmGeneric;
typedef struct ScmMethodRec         ScmMethod;
typedef struct ScmNextMethodRec     ScmNextMethod;
typedef struct ScmSyntaxRec         ScmSyntax;
typedef struct ScmMacroRec          ScmMacro;
typedef struct ScmMemoryRegionRec   ScmMemoryRegion;   /* see mmapP.h */
typedef struct ScmPromiseRec        ScmPromise;
typedef struct ScmRegexpRec         ScmRegexp;
typedef struct ScmRegMatchRec       ScmRegMatch;
typedef struct ScmWriteControlsRec  ScmWriteControls;  /* see writerP.h */
typedef struct ScmWriteContextRec   ScmWriteContext;   /* see writerP.h */
typedef struct ScmWriteStateRec     ScmWriteState;     /* see wrtierP.h */
typedef struct ScmAutoloadRec       ScmAutoload;
typedef struct ScmComparatorRec     ScmComparator;
typedef struct ScmDLObjRec          ScmDLObj;          /* see load.c */
typedef struct ScmReadContextRec    ScmReadContext;    /* see read.c */

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
SCM_EXTERN ScmObj Scm_ValuesFromArray(ScmObj *argv, ScmSmallInt argc);

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
SCM_EXTERN ScmObj Scm_VMReset(ScmObj proc);
SCM_EXTERN ScmObj Scm_VMDynamicWind(ScmObj pre, ScmObj body, ScmObj post);

SCM_EXTERN ScmObj Scm_VMWithErrorHandler(ScmObj handler, ScmObj thunk);
SCM_EXTERN ScmObj Scm_VMWithGuardHandler(ScmObj handler, ScmObj thunk);
SCM_EXTERN ScmObj Scm_VMWithExceptionHandler(ScmObj handler, ScmObj thunk);
SCM_EXTERN ScmObj Scm_VMReraise();

/* Miscellaneous stuff */
SCM_EXTERN int    Scm_VMGetNumResults(ScmVM *vm);
SCM_EXTERN ScmObj Scm_VMGetResult(ScmVM *vm);
SCM_EXTERN ScmObj Scm_VMGetStackLite(ScmVM *vm);
SCM_EXTERN ScmObj Scm_VMGetCallTraceLite(ScmVM *vm);
SCM_EXTERN ScmObj Scm_VMGetStack(ScmVM *vm);

/* A box is to keep a reference.  Internally, it is used for mutable
   local variables.  srfi-111 defines Scheme interface. */
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

/* An mv-box is multi-valued box.  Srfi-195 extends srfi-111 to support
   arbitrary number of values in a box.  We use a different type <mv-box>,
   in order to keep the one-value box lightweight. */
typedef struct ScmMVBoxRec {
    SCM_HEADER;
    ScmSmallInt size;
    ScmObj values[1];            /* variable length */
} ScmMVBox;

SCM_CLASS_DECL(Scm_MVBoxClass);
#define SCM_CLASS_MVBOX            (&Scm_MVBoxClass)
#define SCM_MVBOX(obj)             ((ScmMVBox*)(obj))
#define SCM_MVBOXP(obj)            (SCM_XTYPEP(obj, SCM_CLASS_MVBOX))
#define SCM_MVBOX_SIZE(obj)        (SCM_MVBOX(obj)->size)
#define SCM_MVBOX_VALUES(obj)      (SCM_MVBOX(obj)->values)
#define SCM_MVBOX_SET(obj, k, val) (SCM_MVBOX(obj)->values[k] = (val))

SCM_EXTERN ScmMVBox *Scm_MakeMVBox(ScmSmallInt size, ScmObj init);
SCM_EXTERN ScmMVBox *Scm_ListToMVBox(ScmObj elts);

/*---------------------------------------------------------
 * CLASS AND TYPE
 */

/* A type is a metaobject that defines/describes certain properties of
   a group of objects.  In Gauche, we have a few different kind of types.

   CLASS - A class defines the structure and behavior of the group of
   objects (instances).  Every runtime object directly belongs to a class.
   Classes are inheritable, so an instance can have is-a relationship to
   more than one classes.   The class structure is defined in gauche/class.h,
   and mainly implemented in class.c.

   DESCRIPTIVE TYPE - Represents type constraints.  You can create a
   descriptive type such as "this value should be either an integer or a
   string".  It can be used to validate a value, but cannot be used
   to create an instance.  This is implemented in libtype.scm.

   PROXY TYPE - This is a special wrapper of classes, and usually it isn't
   visible for the users.  This is required because Gauche allows
   classes to be redefined.  See ScmProxyType defintion below.
 */

#include <gauche/class.h>

/*--------------------------------------------------------
 * COLLECTION INTERFACE
 */

#include <gauche/collection.h>

/*--------------------------------------------------------
 * CONNECTION INTERFACE
 */

SCM_CLASS_DECL(Scm_ConnectionClass);
#define SCM_CLASS_CONNECTION         (&Scm_ConnectionClass)

/*--------------------------------------------------------
 * PAIR AND LIST
 */

/* An ordinary pair uses two words.  It can be distinguished from
 * other heap allocated objects by checking the first word doesn't
 * have "111" in the lower bits.
 */
struct ScmPairRec {
    ScmObj car;                 /* should be accessed via macros */
    ScmObj cdr;                 /* ditto */
};

/* An extended pair behaves like an ordinary pair for read operations,
 * but can keep extra information in attributes.  It also has
 * hidden field, and can behave differently on mutating operations.
 * Immutable pairs are implemented on that mechanism.
 * See priv/pairP.h for the real structure of an extended pair.
 */
struct ScmExtendedPairRec {
    ScmObj car;                 /* should be accessed via macros */
    ScmObj cdr;                 /* ditto */
    ScmObj attributes;          /* should be accessed via API func. */
};

#if GAUCHE_CHECK_PAIR_ALIGNMENT
#  define SCM_PAIRP(obj)  (Scm_CheckingPairP(SCM_OBJ(obj)))
SCM_EXTERN int Scm_CheckingPairP(ScmObj);
#else
#  define SCM_PAIRP(obj)                                                  \
     (SCM_HPTRP(obj)&&(SCM_HTAG(obj)!=7||Scm_PairP(SCM_OBJ(obj))))
#endif

#define SCM_PAIR(obj)           ((ScmPair*)(obj))
#define SCM_CAR(obj)            (SCM_PAIR(obj)->car)
#define SCM_CDR(obj)            (SCM_PAIR(obj)->cdr)
#define SCM_CAAR(obj)           (SCM_CAR(SCM_CAR(obj)))
#define SCM_CADR(obj)           (SCM_CAR(SCM_CDR(obj)))
#define SCM_CDAR(obj)           (SCM_CDR(SCM_CAR(obj)))
#define SCM_CDDR(obj)           (SCM_CDR(SCM_CDR(obj)))

#define SCM_SET_CAR(obj, value) Scm_SetCar(obj, value)
#define SCM_SET_CDR(obj, value) Scm_SetCdr(obj, value)

/* Use these only if you know OBJ is a mutable pair */
#define SCM_SET_CAR_UNCHECKED(obj, value) (SCM_CAR(obj) = (value))
#define SCM_SET_CDR_UNCHECKED(obj, value) (SCM_CDR(obj) = (value))

#if SIZEOF_INTPTR_T == 4
#define SCM_ODD_WORD_POINTER_P(p) (SCM_WORD(p) & 0x4)
#else /*SIZEOF_INTPTR_T == 8*/
#define SCM_ODD_WORD_POINTER_P(p) (SCM_WORD(p) & 0x8)
#endif

#if SCM_PAIR_ALWAYS_ALIGNED_EVEN_WORDS
#define SCM_EXTENDED_PAIR_P(obj) \
    (SCM_ODD_WORD_POINTER_P(obj)&&SCM_PAIRP(obj))
#else  /*!SCM_PAIR_ALWAYS_ALIGNED_EVEN_WORDS*/
#define SCM_EXTENDED_PAIR_P(obj) \
    (SCM_ODD_WORD_POINTER_P(obj)&&SCM_PAIRP(obj)&&SCM_HOBJP(((ScmObj*)(obj))-1))
#endif /*!SCM_PAIR_ALWAYS_ALIGNED_EVEN_WORDS*/
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
SCM_EXTERN ScmObj Scm_MakeImmutablePair(ScmObj car, ScmObj cdr);
SCM_EXTERN ScmObj Scm_List(ScmObj elt, ...);
SCM_EXTERN ScmObj Scm_Conses(ScmObj elt, ...);
SCM_EXTERN ScmObj Scm_VaList(va_list elts);
SCM_EXTERN ScmObj Scm_VaCons(va_list elts);
SCM_EXTERN ScmObj Scm_ArrayToList(ScmObj *elts, ScmSize nelts);
SCM_EXTERN ScmObj Scm_ArrayToListWithTail(ScmObj *elts, ScmSize nelts,
                                          ScmObj tail);
SCM_EXTERN ScmObj *Scm_ListToArray(ScmObj list, ScmSize *nelts, ScmObj *store,
                                   int alloc);

SCM_EXTERN ScmObj Scm_Car(ScmObj obj);
SCM_EXTERN ScmObj Scm_Cdr(ScmObj obj);
SCM_EXTERN ScmObj Scm_Caar(ScmObj obj);
SCM_EXTERN ScmObj Scm_Cadr(ScmObj obj);
SCM_EXTERN ScmObj Scm_Cdar(ScmObj obj);
SCM_EXTERN ScmObj Scm_Cddr(ScmObj obj);

SCM_EXTERN int    Scm_ImmutablePairP(ScmObj obj);
SCM_EXTERN void   Scm_SetCar(ScmObj pair, ScmObj value);
SCM_EXTERN void   Scm_SetCdr(ScmObj pair, ScmObj value);

SCM_EXTERN ScmSize Scm_Length(ScmObj obj);
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

SCM_EXTERN ScmObj Scm_MakeExtendedPair(ScmObj car, ScmObj cdr, ScmObj attrs);
SCM_EXTERN ScmObj Scm_ExtendedCons(ScmObj car, ScmObj cdr);
SCM_EXTERN ScmObj Scm_PairAttr(ScmPair *pair);
SCM_EXTERN ScmObj Scm_PairAttrGet(ScmPair *pair, ScmObj key, ScmObj fallback);
SCM_EXTERN ScmObj Scm_PairAttrSet(ScmPair *pair, ScmObj key, ScmObj value);

#if GAUCHE_API_VERSION >= 98
SCM_EXTERN ScmObj Scm_MonotonicMerge(ScmObj sequences);
#define Scm_MonotonicMerge1(x) Scm_MonotonicMerge(x)
#else  /* GAUCHE_API_VERSION < 98 */
SCM_EXTERN ScmObj Scm_MonotonicMerge(ScmObj start, ScmObj sequences);
SCM_EXTERN ScmObj Scm_MonotonicMerge1(ScmObj sequences);
#endif /* GAUCHE_API_VERSION < 98 */

/*--------------------------------------------------------
 * CHARACTERS
 */

/* OBSOLETED */
/* This kind of thing is now handled by string-incomplete->complete
   in libstr.scm. */
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
    unsigned int locked   : 1;     /* setter locked? (see below) */
    unsigned int currying : 1;     /* autocurrying */
    unsigned int constant : 1;     /* constant procedure. see below. */
    unsigned int leaf     : 1;     /* leaf procedure/method */
    unsigned int reserved : 1;     /* unused yet. */
#if GAUCHE_API_VERSION >= 98
    unsigned int reserved32 : 32;  /* unused yet. */
#endif /*GAUCHE_API_VERSION >= 98*/
    ScmObj info;                   /* source code info (see below) */
    ScmObj setter;                 /* setter, if exists. */
    ScmObj inliner;                /* inliner information (see below) */
#if GAUCHE_API_VERSION >= 98
    ScmObj typeHint;               /* info to be used for type checking.
                                      shouldn't be accessed directly, for
                                      we do some tricky stuff here.
                                      Use Scheme API procedure-type
                                      to get the type info.
                                   */
    ScmObj tagsAlist;              /* Alist of procedure tags. Procedure
                                      tags can carry extra info about
                                      the procedure.  They should be treated
                                      as immutable property, for procedures
                                      can be duplicated/consolidated for
                                      optimizations and other runtime handlings.
                                      NB: The value of a tag can have a
                                      mutable structure.
                                      SRFI-229 procedure tag is held as
                                      a value corresponds to 'srfi-229-tag.
                                      This shouldn't be accessed directly;
                                      use API instead.
                                    */
#endif /*GAUCHE_API_VERSION >= 98*/
};

/* About locked slot:
   For <procedure> and <generic>, it shows whether the setter is locked.
   For <method>, it shows whether the alteration of the method is disallowed,
   i.e. one can't redefine a method with matching signature.
   (These two roles are reflected to the two macors,
   SCM_PROCEDURE_SETTER_LOCKED and SCM_PROCEDURE_METHOD_LOCKED)
   TODO: When we change ABI, maybe split these roles to different flags.
 */

/* About optional slot:
   If this slot is non-zero, the procedure takes optional arguments.
   For Standard Scheme procedures with 'rest' arguments, this slot is 1
   and all excessive arguments are 'folded' in a list.

   This slot may have a value more than 1.  If it is N (>1), then up to N-1
   optional arguments are passed without being folded (that is, passed
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

   For a <procedure> and <method>, this flag being TRUE means it returns
   the same constant value if given same constant arguments, and it does
   not have any other external effects.   The compiler may use this info
   to replace a call of this proc with the resulting value,
   if all the arguments are known at compile-time.
   The resulting value must be serializable to the
   precompiled file.  The result shouldn't be affected
   by the timing of the compile, architecture on which the compiler runs,
   or the compiler configuration (e.g. internal encoding).

   If <generic> has this flag, it tells the compiler that it can calculate
   applicable method at the compile time.  It is independent from method's
   constantness---the selected method may or may not be used as a compile-time
   calculation; but it is safe to pre-select that method, given that
   enough information is available at the compile time.
   We warn if a new method is added to a 'constant' generic.
 */

/* About 'leaf' flag:
   For METHOD, this flag indicates the method doesn't refer to next-method
   argument at all, so we can skip creating next-method instance when
   making a call.
   For CLOSURE, we *plan* to use this to indicate the closure body doesn't
   make a call to another procedures, to allow certain optimizations.
 */

/* About 'info' slot:
   This is a sort of the kitchen sink slot, keeping whatever miscellaneous
   information as our implementation evolves.  Since this can be a part of
   statically allocated structure, we can't change its format in a way
   that breaks the backward compatibility.

   SUBR, CLOSURE:
           This slot may contain one of this:
           - Signature: For example, the subr `cons' has (cons obj1 obj2)
             in it.  The first pair may have the following pair attributes.

               `source-info'   (<filename> <lineno>)
                   The source location the procedure is defined, if known.
                   This info can be retrieved with (source-location PROC).
               `bind-info'     (<module-name> <var-name>)
                   The proc is bound to <var-name> in a module named
                   <module-name>, and it's inlinable binding.  When the
                   compiler can pre-calculate the proc to be called in a
                   code, it can replace the original code with a global
                   variable reference to <var-name>.  (We can't directly
                   insert reference to the proc, for it may not be
                   serializable for AOT compilation).

           - A <primitive-parameter> or <parameter> object.  R7RS requires
             parameters to be a procedure, responding #t to procedure?.
             We need to adapt Gauche parameter into that, saving the
             actual parameter instance here.

           - Subr's name, as a string or a symbol.  This is the old format.
             It may also the case that subr is created from C function
             Scm_MakeSubr(), for it's cumbersome in C routine to construct
             the signature list.  Accept it, but not recommended to use
             this format in the new code.
           - #f.  Indicates there's no useful info.

   GENERIC:
           This slot contains the "name" of the gf, which is a symbol.
           A kludge: For setter gf, which can be created indirectly
           via (define-method (setter GF) ...), we use a weird name
           |setter of GF|.  This is a quick hack to make it work, but ideally
           we should accept a list (setter GF) as the name.  Anticipate
           this change in future.
           Furthermore, in order to hold source-info, we might just make
           it a pair, e.g. (NAME) or ((setter NAME)).

   METHOD:
           This slot contains (<name> <specializer> ...),
           where <name> is the name of the generic function, and
           <specializer>s are the name of classes.

   NEXT_METHOD:
           This slot isn't used.
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

   <macro>:  A compiler macro.  The macro expander is invoked with the
      original source and macro-use environment, just like the ordinary macro
      call.  The expander must return an Sexpr.  If the expander returns
      the input as is, it indicates expansion is not possible and the form
      is compiled as the ordinary procedure call.

   <procedure>: A procedural inliner.  It has signature Sexpr,[IForm] -> IForm,
      where Sexpr is the original source of call site (just for debug info) and
      input [IForm] is the IForm for list of arguments.  See compiler-1.scm.
      It returns the modified IForm.  It can return #<undef>, to indicate
      inlining isn't possible.
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
#define SCM_PROCEDURE_CONSTANT(obj) SCM_PROCEDURE(obj)->constant
#define SCM_PROCEDURE_CURRYING(obj) SCM_PROCEDURE(obj)->currying
#define SCM_PROCEDURE_INFO(obj)     SCM_PROCEDURE(obj)->info
#define SCM_PROCEDURE_SETTER(obj)   SCM_PROCEDURE(obj)->setter
#define SCM_PROCEDURE_INLINER(obj)  SCM_PROCEDURE(obj)->inliner
#define SCM_PROCEDURE_SETTER_LOCKED(obj) SCM_PROCEDURE(obj)->locked
#define SCM_PROCEDURE_LEAF(obj)     SCM_PROCEDURE(obj)->leaf

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

/* This is internal - should never be used directly */
#if GAUCHE_API_VERSION >= 98
#define SCM__PROCEDURE_INITIALIZER(klass, req, opt, typ, cst, lef, inf, inl) \
    { { klass, NULL }, (req), (opt), (typ), FALSE, FALSE, cst, lef, 0, 0,    \
      (inf), SCM_FALSE, (inl), SCM_FALSE, SCM_NIL }
#else  /* GAUCHE_API_VERSION < 98 */
#define SCM__PROCEDURE_INITIALIZER(klass, req, opt, typ, cst, lef, inf, inl) \
    { { klass, NULL }, (req), (opt), (typ), FALSE, FALSE, cst, lef, 0,       \
      (inf), SCM_FALSE, (inl) }
#endif /* GAUCHE_API_VERSION < 98 */

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
#define SCM_CLOSURE_CODE(obj)      SCM_CLOSURE(obj)->code
#define SCM_CLOSURE_ENV(obj)       SCM_CLOSURE(obj)->env

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
             req, opt, SCM_PROC_SUBR, cst, 0, inf, inliner),                \
        flags, (func), (data)                                               \
    }

#define SCM_DEFINE_SUBR(cvar, req, opt, inf, func, inliner, data) \
    SCM__DEFINE_SUBR_INT(cvar, req, opt, 0, inf, 0, func, inliner, data)
#define SCM_DEFINE_SUBRX(cvar, req, opt, cst, inf, flags, func, inliner, data) \
    SCM__DEFINE_SUBR_INT(cvar, req, opt, cst, inf, flags, func, inliner, data)

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
    void *dispatcher;
    void *data;
    ScmInternalMutex lock;
};

SCM_CLASS_DECL(Scm_GenericClass);
#define SCM_CLASS_GENERIC          (&Scm_GenericClass)
#define SCM_GENERICP(obj)          SCM_XTYPEP(obj, SCM_CLASS_GENERIC)
#define SCM_GENERIC(obj)           ((ScmGeneric*)obj)
#define SCM_GENERIC_DATA(obj)      (SCM_GENERIC(obj)->data)

/* we share 'constant' flag for sealed generic */
#define SCM_GENERIC_SEALED_P(obj)  SCM_PROCEDURE_CONSTANT(obj)

#define SCM_DEFINE_GENERIC(cvar, cfunc, data)                           \
    ScmGeneric cvar = {                                                 \
        SCM__PROCEDURE_INITIALIZER(SCM_CLASS_STATIC_TAG(Scm_GenericClass),\
                                   0, 0, SCM_PROC_GENERIC, 0, 0,        \
                                   SCM_FALSE, NULL),                    \
        SCM_NIL, 0, cfunc, NULL, data,                                  \
        SCM_INTERNAL_MUTEX_INITIALIZER                                  \
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
#define SCM_METHOD_LOCKED(obj)     SCM_METHOD(obj)->common.locked
#define SCM_METHOD_LEAF_P(obj)     SCM_METHOD(obj)->common.leaf

#define SCM_DEFINE_METHOD(cvar, gf, req, opt, specs, func, data)        \
    ScmMethod cvar = {                                                  \
        SCM__PROCEDURE_INITIALIZER(SCM_CLASS_STATIC_TAG(Scm_MethodClass),\
                                   req, opt, SCM_PROC_METHOD, 0, 0,     \
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


/*--------------------------------------------------------
 * MACROS AND SYNTAX
 */

/* The actual definitions of ScmSyntax and ScmMacro are private.*/

#define SCM_SYNTAX(obj)             ((ScmSyntax*)(obj))
#define SCM_SYNTAXP(obj)            SCM_XTYPEP(obj, SCM_CLASS_SYNTAX)
SCM_CLASS_DECL(Scm_SyntaxClass);
#define SCM_CLASS_SYNTAX            (&Scm_SyntaxClass)

#define SCM_MACRO(obj)             ((ScmMacro*)(obj))
#define SCM_MACROP(obj)            SCM_XTYPEP(obj, SCM_CLASS_MACRO)
SCM_CLASS_DECL(Scm_MacroClass);
#define SCM_CLASS_MACRO            (&Scm_MacroClass)

SCM_EXTERN ScmObj Scm_MakeMacro(ScmObj name,
                                ScmObj transformer,
                                ScmObj info, /* alist */
                                u_long flags);
SCM_EXTERN ScmObj Scm_MacroTransformer(ScmMacro *mac);
SCM_EXTERN ScmObj Scm_MacroName(ScmMacro *mac);

SCM_EXTERN ScmObj Scm_MakeMacroAutoload(ScmSymbol *name,
                                        ScmAutoload *al);

#if GAUCHE_API_VERSION >= 98
SCM_EXTERN ScmObj Scm_UnwrapSyntax(ScmObj form, int immutablep);
#define Scm_UnwrapSyntax2(form, imm) Scm_UnwrapSyntax(form, imm)
#else  /* GAUCHE_API_VERSION < 98 */
SCM_EXTERN ScmObj Scm_UnwrapSyntax(ScmObj form);
SCM_EXTERN ScmObj Scm_UnwrapSyntax2(ScmObj form, int immutablep);
#endif /* GAUCHE_API_VERSION < 98 */

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
SCM_EXTERN ScmObj Scm_VMForce(ScmObj p); /* CPS, lightweight */
SCM_EXTERN ScmObj Scm_Force(ScmObj p);

/* Lazy pair structure is opaque to public.  Whenever you apply to an
   ScmObj SCM_PAIRP, a lazy pair morphs itself to a pair, so the normal
   code never see lazy pairs. */

SCM_CLASS_DECL(Scm_LazyPairClass);
#define SCM_CLASS_LAZY_PAIR        (&Scm_LazyPairClass)
#define SCM_LAZY_PAIR(obj)         ((ScmLazyPair*)(obj))
#define SCM_LAZY_PAIR_P(obj)       SCM_XTYPEP(obj, SCM_CLASS_LAZY_PAIR)

SCM_EXTERN ScmObj Scm_MakeLazyPair(ScmObj item, ScmObj generator, ScmObj attrs);
SCM_EXTERN ScmObj Scm_LazyCons(ScmObj item, ScmObj thunk, ScmObj attrs);
SCM_EXTERN ScmObj Scm_GeneratorToLazyPair(ScmObj generator);
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
    SCM_PORT_ERROR_DECODING,
    SCM_PORT_ERROR_ENCODING,
    SCM_PORT_ERROR_SEEK,
    SCM_PORT_ERROR_INVALID_POSITION,
    SCM_PORT_ERROR_OTHER
};

/* Throwing error */
SCM_EXTERN void Scm_Error(const char *msg, ...) SCM_NORETURN;
SCM_EXTERN void Scm_SysError(const char *msg, ...) SCM_NORETURN;
SCM_EXTERN void Scm_TypeError(const char *what,
                              const char *expected, ScmObj got) SCM_NORETURN;
SCM_EXTERN void Scm_PortError(ScmPort *port, int reason,
                              const char *msg, ...) SCM_NORETURN;
SCM_EXTERN void Scm_PortErrorWithAux(ScmPort *port, int reason,
                                     ScmObj auxinfo,
                                     const char *msg, ...) SCM_NORETURN;

/* common pattern */
#define SCM_TYPE_ERROR(arg, expected)  Scm_TypeError(#arg, expected, arg)

SCM_EXTERN void Scm_Warn(const char *msg, ...);
SCM_EXTERN void Scm_FWarn(ScmString *fmt, ScmObj args);

SCM_EXTERN ScmObj Scm_Raise(ScmObj exception, u_long flags);

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

SCM_EXTERN void Scm_SetCallTraceSize(u_long size);

SCM_EXTERN ScmObj Scm_ReportError(ScmObj e, ScmObj out);

/*--------------------------------------------------------
 * REGEXP
 */

/* The definition of Scm_RegexpRec and Scm_RegeMatchRec is hidden
   in gauche/priv/regexpP.h */

SCM_CLASS_DECL(Scm_RegexpClass);
#define SCM_CLASS_REGEXP          (&Scm_RegexpClass)
#define SCM_REGEXP(obj)           ((ScmRegexp*)obj)
#define SCM_REGEXPP(obj)          SCM_XTYPEP(obj, SCM_CLASS_REGEXP)

/* flags */
#define SCM_REGEXP_CASE_FOLD      (1L<<0)
#define SCM_REGEXP_PARSE_ONLY     (1L<<1)
/* bits 2 and 3 are used internally */
#define SCM_REGEXP_MULTI_LINE     (1L<<4)

SCM_EXTERN ScmObj Scm_RegComp(ScmString *pattern, int flags);
#if GAUCHE_API_VERSION >= 98
SCM_EXTERN ScmObj Scm_RegCompFromAST(ScmObj ast, int flags);
#define Scm_RegCompFromAST2(a,b) Scm_RegCompFromAST(a,b)
#else   /* GAUCHE_API_VERSION < 98 */
SCM_EXTERN ScmObj Scm_RegCompFromAST(ScmObj ast);
SCM_EXTERN ScmObj Scm_RegCompFromAST2(ScmObj ast, int flags);
#endif  /* GAUCHE_API_VERSION < 98 */
SCM_EXTERN ScmObj Scm_RegOptimizeAST(ScmObj ast);
SCM_EXTERN ScmObj Scm_RegExec(ScmRegexp *rx, ScmString *input, ScmObj start, ScmObj end);
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
SCM_EXTERN void   Scm_SigFillSetMostly(sigset_t *set);
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

#if GAUCHE_API_VERSION < 98
SCM_EXTERN void   Scm_GetSigmask(sigset_t *mask);
SCM_EXTERN void   Scm_SetSigmask(sigset_t *mask);
#endif /*GAUCHE_API_VERSION < 98*/

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

/* 'kind' argument of Scm_InitCommandLine */
enum {
    SCM_COMMAND_LINE_SCRIPT = 1,    /* for (command-line) */
    SCM_COMMAND_LINE_OS = 2,        /* for (os-command-line) */
    SCM_COMMAND_LINE_BOTH = (SCM_COMMAND_LINE_SCRIPT|SCM_COMMAND_LINE_OS)
};

#if GAUCHE_API_VERSION >= 98
SCM_EXTERN ScmObj Scm_InitCommandLine(int argc, const char *argv[],
                                      int kind);
#define Scm_InitCommandLine2(ac, av, kind) Scm_InitCommandLine(ac, av, kind)
#else  /* GAUCHE_API_VERSION < 98 */
SCM_EXTERN ScmObj Scm_InitCommandLine(int argc, const char *argv[]);
SCM_EXTERN ScmObj Scm_InitCommandLine2(int argc, const char *argv[], int kind);
#endif /* GAUCHE_API_VERSION < 98 */

SCM_EXTERN void Scm_SimpleMain(int argc, const char *argv[],
                               const char *script, u_long flags);

SCM_EXTERN void Scm_GC(void);
SCM_EXTERN void Scm_PrintStaticRoots(void);
SCM_EXTERN void Scm_RegisterDL(void *data_start, void *data_end,
                               void *bss_start, void *bss_end);
SCM_EXTERN void Scm_GCSentinel(void *obj, const char *name);

SCM_EXTERN ScmObj Scm_GetFeatures(void);
SCM_EXTERN void   Scm_AddFeature(const char *feature, const char *mod);
SCM_EXTERN void   Scm_DisableFeature(const char *feature);

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
SCM_EXTERN ScmObj Scm_RuntimeDirectory(void); /* may return SCM_FALSE */
SCM_EXTERN ScmObj Scm_LibgauchePath(void);    /* may return SCM_FALSE */
SCM_EXTERN ScmObj Scm_ExecutablePath(void);   /* may return SCM_FALSE */

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
#include <gauche/endian.h>

SCM_DECL_END

#endif /* GAUCHE_H */
