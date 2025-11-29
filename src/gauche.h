/*
 * gauche.h - Gauche scheme system header
 *
 *   Copyright (c) 2000-2025  Shiro Kawai  <shiro@acm.org>
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

/* As of 1.0, We always use utf8 in internal encoding.  However,
   existing code may check this. */
#define GAUCHE_CHAR_ENCODING_UTF8 1

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
#include <math.h>
#include <complex.h>            /* needs to be outside of extern "C" */

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif
#include <time.h>

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

/* Experimental: Enable lightweight continuation capturing in exception
   handling. */
#define GAUCHE_SPLIT_STACK 0

/* Define to 1 if debugging the bootstrap module */
#define GAUCHE_DEBUG_BOOTSTRAP 0

/* Include appropriate threading interface.  Threading primitives are
   abstracted with SCM_INTERNAL_* macros and ScmInternal* typedefs.
   See gauche/pthread.h for the semantics of these primitives. */
#ifdef GAUCHE_USE_PTHREADS
# include <gauche/pthread.h>
#elif  GAUCHE_USE_WTHREADS
# include <gauche/wthread.h>
#else
# error "No thread support"
#endif

/* For the backward compatibility.  Now we always have threads. */
#define GAUCHE_HAS_THREADS 1

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
 *  A character is represented by an integer Unicode codepoint.
 */

#define SCM_CHAR(obj)           ((ScmChar)(obj))
#define SCM_CHARP(obj)          ((SCM_WORD(obj)&0xff) == 3)
#define SCM_CHAR_VALUE(obj)     SCM_CHAR(((unsigned long)SCM_WORD(obj)) >> 8)
#define SCM_MAKE_CHAR(ch)       SCM_OBJ((intptr_t)(((unsigned long)(ch))<<8) + 3)

#define SCM_CHAR_INVALID        ((ScmChar)(-2)) /* indicate invalid char (-2 is used to avoid conflict with libc's EOF) */
#define SCM_CHAR_MAX            (0x10ffff)

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

#include "gauche/char_utf_8.h"

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
SCM_EXTERN void   Scm_VMPushDynamicHandlers(ScmObj, ScmObj, ScmObj);
SCM_EXTERN ScmObj Scm_VMDynamicWind(ScmObj pre, ScmObj body, ScmObj post);
SCM_EXTERN int    Scm_ContinuationP(ScmObj proc);

SCM_EXTERN ScmObj Scm_VMCallWithContinuationPrompt(ScmObj, ScmObj, ScmObj);
SCM_EXTERN ScmObj Scm_VMAbortCurrentContinuation(ScmObj, ScmObj);

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

/*---------------------------------------------------------
 * BOX
 */

#include <gauche/box.h>

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

/* flag bit for Scm_Length2 */
enum {
    SCM_LENGTH_LAZY = 1L<<0     /* don't realize lazy pairs */
};

/* special return value of Scm_Length */
#define SCM_LIST_DOTTED   (-1)       /* dotted list */
#define SCM_LIST_CIRCULAR (-2)     /* circular list */
#define SCM_LIST_LAZY     (SCM_SMALL_INT_MAX) /* proper list but length unknown
                                                 (can be infinite) */

#define SCM_PROPER_LIST_P(obj) (Scm_Length2(obj, SCM_LENGTH_LAZY) >= 0)
#define SCM_DOTTED_LIST_P(obj) \
    (Scm_Length2(obj, SCM_LENGTH_LAZY) == SCM_LIST_DOTTED)
#define SCM_CIRCULAR_LIST_P(obj) \
    (Scm_Length2(obj, SCM_LENGTH_LAZY) == SCM_LIST_CIRCULAR)

SCM_EXTERN ScmSize Scm_Length2(ScmObj obj, u_long flags);
SCM_EXTERN ScmSize Scm_Length(ScmObj obj);

SCM_EXTERN ScmObj Scm_Cons(ScmObj car, ScmObj cdr);
SCM_EXTERN ScmObj Scm_Acons(ScmObj caar, ScmObj cdar, ScmObj cdr);
SCM_EXTERN ScmObj Scm_MakeImmutablePair(ScmObj car, ScmObj cdr, ScmObj attrs);
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

SCM_EXTERN ScmObj Scm_CopyList(ScmObj list);
SCM_EXTERN ScmObj Scm_MakeList(ScmSmallInt len, ScmObj fill);
SCM_EXTERN ScmObj Scm_AlistCopy(ScmObj alist);
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

#include <gauche/procedure.h>

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
SCM_EXTERN void Scm_AssertionError(ScmObj irritants,
                                   const char *msg, ...) SCM_NORETURN;
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
 * THREADS
 */

#include <gauche/thread.h>

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
SCM_EXTERN void Scm_Cleanup(void);
SCM_EXTERN void Scm_Exit(int code) SCM_NORETURN;
SCM_EXTERN int  Scm_ObjToExitCode(ScmObj obj);
SCM_EXTERN void Scm_Abort(const char *msg) SCM_NORETURN;
SCM_EXTERN void Scm_Panic(const char *msg, ...) SCM_NORETURN;

/* Runtime state, obtainable by Scm_RuntimeState().
   This is used internally for diagnostic purposes.  E.g. an elaborated error
   repoter may not function if a module required for that reporter hasn't
   been initialized.
 */
typedef enum {
    /* We're executing Scm_Init().  Not all fundamental components are
       functional.  */
    SCM_RUNTIME_INITIALIZING,

    /* All pre-compiled components are initialized, ready to load Scheme
       program.  Scheme script is run in this state. */
    SCM_RUNTIME_INITIALIZED,

    /* Minimum REPL is working. This is the REPL you get with 'gosh -q'.
       You can get to this state without loading any Scheme files. */
    SCM_RUNTIME_MINI_REPL,

    /* REPL is working.  This is the normal REPL you get with gosh.
       You can assume full Gauche functionalities are available. */
    SCM_RUNTIME_FULL_REPL,
} ScmRuntimeState;

SCM_EXTERN ScmRuntimeState Scm_RuntimeState(void);
SCM_EXTERN void Scm_SetRuntimeReplState(int);

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
SCM_EXTERN void   Scm_DeleteFeature(const char *feature);
SCM_EXTERN void   Scm_DisableFeature(const char *feature);
SCM_EXTERN ScmObj Scm_BuildGoshVersion(void);

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
