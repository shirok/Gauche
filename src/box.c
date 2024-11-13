/*
 * box.c - boxes
 *
 *   Copyright (c) 2010-2024  Shiro Kawai  <shiro@acm.org>
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

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/priv/configP.h"
#include "gauche/priv/boxP.h"

/* Srfi-195 extends box type to allow muliple values.  We use separate
   types for single-value box and multi-value box, for we want to optimize
   single-value box; it is used internally for mutable local variables. */

/* Box is a heap-allocated object that can hold one Scheme value.
 * It is internally used to realize indirection for mutable local
 * variables.   Conceptually, the compiler translates initialization,
 * reference and mutation of mutable local variables into construction,
 * dereference and mutation of box, as follows:
 *
 * Original code:
 *
 *   (let ((var 1))
 *     ...
 *     (foo var)
 *     ...
 *     (set! var 2)
 *     ...)
 *
 * Translated:
 *
 *   (let ((var (%make-box 1)))
 *     ...
 *     (foo (%box-ref var))
 *     ...
 *     (%box-set! var 2)
 *     ...)
 *
 * Local variables that are never set! won't be translated.
 *
 * After this translation, *all* local variables are immutable.  It makes
 * optimization a lot easier.
 */

/* SRFI-111 defines box API.  Scheme interface is in libalpha.scm. */
/* NB: SRFI-111 leave equal? behavior implementation-dependent (except
   it must return #f if eqv? returns #f).  We compare the contents.
 */
static void box_print(ScmObj obj, ScmPort *port,
                      ScmWriteContext *ctx SCM_UNUSED)
{
    Scm_Printf(port, "#<box %S>", SCM_BOX_VALUE(obj));
}

static int box_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        /* should return 0 if x == y */
        return (Scm_EqualP(SCM_BOX_VALUE(x), SCM_BOX_VALUE(y))? 0:1);
    } else {
        return Scm_Compare(SCM_BOX_VALUE(x), SCM_BOX_VALUE(y));
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_BoxClass, box_print, box_compare,
                         NULL, NULL, SCM_CLASS_DEFAULT_CPL);

ScmBox *Scm_MakeBox(ScmObj value)
{
    ScmBox *b = SCM_NEW(ScmBox);
    SCM_SET_CLASS(b, &Scm_BoxClass);
    SCM_BOX_SET(b, value);
    return b;
}

/* MVBox is a box that can hold multiple values.
 */

static void mvbox_print(ScmObj obj, ScmPort *port,
                        ScmWriteContext *ctx SCM_UNUSED)
{
    Scm_Printf(port, "#<mv-box[%d]", SCM_MVBOX_SIZE(obj));
    ScmObj *vs = SCM_MVBOX_VALUES(obj);
    for (ScmSmallInt i = 0; i < SCM_MVBOX_SIZE(obj); i++) {
        Scm_Printf(port, " %S", vs[i]);
    }
    Scm_Printf(port, ">");
}

static int mvbox_compare(ScmObj x, ScmObj y, int equalp)
{
    ScmMVBox *mx = SCM_MVBOX(x);
    ScmMVBox *my = SCM_MVBOX(y);
    if (equalp) {
        /* should return 0 if x == y */
        if (mx->size != my->size) return 1;
        for (ScmSmallInt i=0; i<mx->size; i++) {
            if (!Scm_EqualP(mx->values[i], my->values[i])) return 1;
        }
        return 0;
    } else {
        if (mx->size < my->size) return -1;
        if (mx->size > my->size) return 1;
        for (ScmSmallInt i=0; i<mx->size; i++) {
            int r = Scm_Compare(mx->values[i], my->values[i]);
            if (r < 0 || r > 0) return r;
        }
        return 0;
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_MVBoxClass, mvbox_print, mvbox_compare,
                         NULL, NULL, SCM_CLASS_DEFAULT_CPL);

static ScmMVBox *make_mvbox(ScmSmallInt size)
{
    SCM_ASSERT(size == 0 || size >= 2);
    ScmSmallInt allocsize = (size == 0)? 1 : size;
    ScmMVBox *b =
        SCM_NEW2(ScmMVBox*, sizeof(ScmMVBox) + allocsize*sizeof(ScmObj));
    SCM_SET_CLASS(b, &Scm_MVBoxClass);
    b->size = size;
    return b;
}

ScmMVBox *Scm_MakeMVBox(ScmSmallInt size, ScmObj init)
{
    ScmMVBox *b = make_mvbox(size);
    for (ScmSmallInt i=0; i<size; i++) {
        b->values[i] = init;
    }
    return b;
}

ScmMVBox *Scm_ListToMVBox(ScmObj elts)
{
    ScmSmallInt size = Scm_Length(elts);
    if (size < 0) Scm_Error("Improper list not allowed: %S", elts);
    ScmMVBox *b = make_mvbox(size);
    for (ScmSmallInt i=0; i<size; i++, elts = SCM_CDR(elts)) {
        b->values[i] = SCM_CAR(elts);
    }
    return b;
}

/* Atomic Box.
 *  SRFI-230 defines four types of atomic boxes.
 *  Internally all of them shared the same C structure, and derived
 *  from a common base class, <atomic-base>.
 */

static ScmObj atomic_box_allocate(ScmClass *klass, ScmObj initargs SCM_UNUSED)
{
    return SCM_OBJ(Scm_MakeAtomicBox(klass, SCM_UNDEFINED));
}

static void atomic_box_print(ScmObj obj, ScmPort *port,
                             ScmWriteContext *ctx SCM_UNUSED)
{
    Scm_Printf(port, "#<%A %S>",
               Scm_ShortClassName(Scm_ClassOf(obj)),
               Scm_AtomicBoxRef(SCM_ATOMIC_BOX(obj)));
}


SCM_DEFINE_BUILTIN_CLASS(Scm_AtomicBaseClass, atomic_box_print, NULL, NULL,
                         atomic_box_allocate,
                         SCM_CLASS_DEFAULT_CPL);

static ScmClass *atomic_box_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_AtomicBaseClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

SCM_DEFINE_BUILTIN_CLASS(Scm_AtomicBoxClass, atomic_box_print, NULL, NULL,
                         atomic_box_allocate,
                         atomic_box_cpl);
SCM_DEFINE_BUILTIN_CLASS(Scm_AtomicFlagClass, atomic_box_print, NULL, NULL,
                         atomic_box_allocate,
                         atomic_box_cpl);
SCM_DEFINE_BUILTIN_CLASS(Scm_AtomicFxboxClass, atomic_box_print, NULL, NULL,
                         atomic_box_allocate,
                         atomic_box_cpl);
SCM_DEFINE_BUILTIN_CLASS(Scm_AtomicPairClass, atomic_box_print, NULL, NULL,
                         atomic_box_allocate,
                         atomic_box_cpl);

ScmAtomicBox *Scm_MakeAtomicBox(ScmClass *klass, ScmObj obj)
{
    SCM_ASSERT(Scm_SubclassP(klass, SCM_CLASS_ATOMIC_BASE));
    ScmAtomicBox *z = SCM_NEW(ScmAtomicBox);
    SCM_SET_CLASS(z, klass);
    Scm_AtomicStoreFull(&z->val, (ScmAtomicWord)obj);
    return z;
}

ScmObj Scm_AtomicBoxRef(ScmAtomicBox *abox)
{
    return SCM_OBJ(Scm_AtomicLoad(&abox->val));
}

void Scm_AtomicBoxSet(ScmAtomicBox *abox, ScmObj obj)
{
    Scm_AtomicStoreFull(&abox->val, (ScmAtomicWord)obj);
}

ScmObj Scm_AtomicBoxSwap(ScmAtomicBox *abox, ScmObj obj)
{
    ScmAtomicWord old = Scm_AtomicExchange(&abox->val, (ScmAtomicWord)obj);
    return SCM_OBJ(old);
}

/* Returns old value.  Caller can compare expected and returned value
   to see if swap is succeeded. */
ScmObj Scm_AtomicBoxCompareAndSwap(ScmAtomicBox *abox,
                                   ScmObj expected,
                                   ScmObj obj)
{
    ScmAtomicWord desired = (ScmAtomicWord)obj;
    ScmAtomicWord old = (ScmAtomicWord)expected;;
    (void)Scm_AtomicCompareExchange(&abox->val, &old, desired);
    return SCM_OBJ(old);
}
