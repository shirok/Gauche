/*
 * gauche/memory.h - internal macros for memory-related stuff
 *
 *  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: memory.h,v 1.1 2001-01-18 19:41:39 shiro Exp $
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

#ifdef SCM_INLINE_MALLOC_PRIMITIVES

/* These #define's must match the ones libgc.a was compiled with. */
#define SMALL_CONFIG
#define DONT_ADD_BYTE_AT_END
#define ALL_INTERIOR_POINTERS
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
        p->hdr.klass = SCM_CLASS_PAIR;                                   \
        SCM_SET_CAR(p, car_);                                            \
        SCM_SET_CDR(p, cdr_);                                            \
        p->attributes = SCM_NIL;                                         \
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
    (p = (type)Scm_Malloc(n * sizeof(GC_word)))

#define SCM_MALLOC_ATOMIC_WORDS(p, n, type) \
    (p = (type)Scm_MallocAtomic(n * sizeof(GC_word)))

#define SCM_NEW_PAIR(p, car_, cdr_) (p = Scm_Cons(car_, cdr_))

#define SCM_NEW_LIST1(p, obj0)        (p = Scm_Cons(obj0, SCM_NIL))
#define SCM_NEW_LIST2(p, obj0, obj1) \
    (p = Scm_Cons(obj0, Scm_Cons(obj1, SCM_NIL))
#define SCM_NEW_LIST3(p, obj0, obj1, obj2)
    (p = Scm_Cons(obj0, Scm_Cons(obj1, Scm_Cons(obj2, SCM_NIL))))

#endif /* SCM_INLINE_MALLOC_PRIMITIVES */

#endif /* GAUCHE_MEM_H */
