/*
 * gauche/memory.h - internal macros for memory-related stuff
 *
 *   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
 *  $Id: memory.h,v 1.10 2008-05-04 18:41:47 shirok Exp $
 */

#ifndef GAUCHE_MEM_H
#define GAUCHE_MEM_H

/*
 * This file defines some macros to inline memory allocation.  
 * The macro have to interact with some internal gc stuff, so they should
 * be used with care.   By including this file you'll get whole bunch
 * of internal macros of gc, which may conflict with your code if used
 * carelessly.
 *
 * If SCM_INLINE_MALLOC_PRIMITIVES is not defined, these macros are
 * replaced for "safer" function calls.
 */

#if SCM_INLINE_MALLOC_PRIMITIVES

/* These #define's must match the ones libgc.a was compiled with. */
/* NB: when cross-compiling, SMALL_CONFIG is defined in GC. */
#ifndef DONT_ADD_BYTE_AT_END
#define DONT_ADD_BYTE_AT_END
#endif
#ifndef ALL_INTERIOR_POINTERS
#define ALL_INTERIOR_POINTERS
#endif

#include "gc_inline.h"

/* Basic macros.  Allocate N words obj.  N must be smaller than MAXOBJSZ. */
#define SCM_MALLOC_WORDS(p, n, type)            \
    do {                                        \
        void *SCMMW__tmpptr;                    \
        GC_MALLOC_WORDS(SCMMW__tmpptr, n);      \
        p = (type)SCMMW__tmpptr;                \
    } while (0)

#define SCM_MALLOC_ATOMIC_WORDS(p, n)                   \
    do {                                                \
        void *SCMMW__tmpptr;                            \
        GC_MALLOC_ATOMIC_WORDS(SCMMW__tmpptr, n);       \
        p = (type)SCMMW__tmpptr;                        \
    } while (0)

/* Type-specific macros */
#define SCM_NEW_PAIR(p, car_, cdr_)                                      \
    do {                                                                 \
        SCM_MALLOC_WORDS(p, sizeof(ScmPair)/sizeof(GC_word), ScmPair *); \
        SCM_SET_CLASS(p, SCM_CLASS_PAIR);                                \
        SCM_SET_CAR(p, car_);                                            \
        SCM_SET_CDR(p, cdr_);                                            \
    } while (0)

#define SCM_NEW_LIST1(p, obj0) \
    SCM_NEW_PAIR(p, obj0, SCM_NIL)

#define SCM_NEW_LIST2(p, obj0, obj1)                    \
    do {                                                \
        ScmObj SCML2__tmpptr;                           \
        SCM_NEW_PAIR(SCML2__tmpptr, obj1, SCM_NIL);     \
        SCM_NEW_PAIR(p, obj0, SCM_OBJ(SCML2__tmpptr));  \
    } while (0)

#define SCM_NEW_LIST3(p, obj0, obj1, obj2)                              \
    do {                                                                \
        ScmObj SCML3__tmpptr0, SCML3__tmpptr1;                          \
        SCM_NEW_PAIR(SCML3__tmpptr0, obj2, SCM_NIL);                    \
        SCM_NEW_PAIR(SCML3__tmpptr1, obj1, SCM_OBJ(SCML3__tmpptr0));    \
        SCM_NEW_PAIR(p, obj0, SCM_OBJ(SCML3__tmpptr1));                 \
    } while (0)
   
#else /* !SCM_INLINE_MALLOC_PRIMITIVES */

#define SCM_MALLOC_WORDS(p, n, type) \
    (p = (type)SCM_MALLOC(n * sizeof(GC_word)))

#define SCM_MALLOC_ATOMIC_WORDS(p, n, type) \
    (p = (type)SCM_MALLOC_ATOMIC(n * sizeof(GC_word)))

#define SCM_NEW_PAIR(p, car_, cdr_) (p = (ScmPair*)Scm_Cons(car_, cdr_))

#define SCM_NEW_LIST1(p, obj0)        (p = Scm_Cons(obj0, SCM_NIL))
#define SCM_NEW_LIST2(p, obj0, obj1) \
    (p = Scm_Cons(obj0, Scm_Cons(obj1, SCM_NIL))
#define SCM_NEW_LIST3(p, obj0, obj1, obj2) \
    (p = Scm_Cons(obj0, Scm_Cons(obj1, Scm_Cons(obj2, SCM_NIL))))

#endif /* SCM_INLINE_MALLOC_PRIMITIVES */

#endif /* GAUCHE_MEM_H */
