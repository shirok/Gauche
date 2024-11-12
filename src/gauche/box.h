/*
 * box.h - Public API for Scheme boxes
 *
 *   Copyright (c) 2000-2024  Shiro Kawai  <shiro@acm.org>
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

/* This file is included from gauche.h */

#ifndef GAUCHE_BOX_H
#define GAUCHE_BOX_H

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

/* An atomic box.  We don't expose internals; see priv/atomicP.h
 * In C-level, we only have one structure, ScmAtomicBox. In Scheme
 * level, we follow srfi-230, which defines 4 distinct atomic structures:
 * atomic flag, atomic box, atomic fxbox, and atomic pair.
 * All can be implemented on top of atomic box, but it is more convenient
 * to have disjoint types.
 */

typedef struct ScmAtomicBoxRec ScmAtomicBox;

SCM_CLASS_DECL(Scm_AtomicBaseClass);
#define SCM_CLASS_ATOMIC_BASE   (&Scm_AtomicBaseClass)
#define SCM_ATOMIC_BASE(obj)    ((ScmAtomicBox*)(obj))
#define SCM_ATOMIX_BASE_P(obj)  (Scm_TypeP(obj, SCM_CLASS_ATOMIC_BASE))

SCM_CLASS_DECL(Scm_AtomicBoxClass);
#define SCM_CLASS_ATOMIC_BOX     (&Scm_AtomicBoxClass)
#define SCM_ATOMIC_BOX(obj)     ((ScmAtomicBox*)(obj))
#define SCM_ATOMIC_BOX_P(obj)   (SCM_XTYPEP(obj, SCM_CLASS_ATOMIC_BOX))

SCM_CLASS_DECL(Scm_AtomicFlagClass);
#define SCM_CLASS_ATOMIC_FLAG   (&Scm_AtomicFlagClass)
#define SCM_ATOMIC_FLAG(obj)    ((ScmAtomicBox*)(obj))
#define SCM_ATOMIC_FLAG_P(obj)  (SCM_XTYPEP(obj, SCM_CLASS_ATOMIC_FLAG))

SCM_CLASS_DECL(Scm_AtomicFxboxClass);
#define SCM_CLASS_ATOMIC_FXBOX  (&Scm_AtomicFxboxClass)
#define SCM_ATOMIC_FXBOX(obj)   ((ScmAtomicBox*)(obj))
#define SCM_ATOMIC_FXBOX_P(obj) (SCM_XTYPEP(obj, SCM_CLASS_ATOMIC_FXBOX))

SCM_CLASS_DECL(Scm_AtomicPairClass);
#define SCM_CLASS_ATOMIC_PAIR   (&Scm_AtomicPairClass)
#define SCM_ATOMIC_PAIR(obj)    ((ScmAtomicBox*)(obj))
#define SCM_ATOMIC_PAIR_P(obj)  (SCM_XTYPEP(obj, SCM_CLASS_ATOMIC_PAIR))

SCM_EXTERN ScmAtomicBox *Scm_MakeAtomicBox(ScmClass *klass, ScmObj obj);
SCM_EXTERN ScmObj Scm_AtomicBoxRef(ScmAtomicBox *abox);
SCM_EXTERN void   Scm_AtomicBoxSet(ScmAtomicBox *abox, ScmObj val);
SCM_EXTERN ScmObj Scm_AtomicBoxSwap(ScmAtomicBox *abox, ScmObj val);
SCM_EXTERN ScmObj Scm_AtomicBoxCompareAndSwap(ScmAtomicBox *abox,
                                              ScmObj expected,
                                              ScmObj desired);

#endif /*GAUCHE_BOX_H*/
