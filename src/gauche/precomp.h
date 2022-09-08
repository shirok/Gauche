/*
 * precomp.h - included from precompiled C code
 *
 *   Copyright (c) 2022  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PRECOMP_H
#define GAUCHE_PRECOMP_H

/*
 * Macros used by generated C code.
 * 'PC' stands for precomp.
 */

/*
 * Numeric comparison ops
 */

#define SCM_PC_NUMCMP2(a, b, op)                        \
    (((SCM_INTP(a) && SCM_INTP(b))                      \
      ? (SCM_INT_VALUE(a) op SCM_INT_VALUE(b))          \
      : ((SCM_FLONUMP(a) && SCM_FLONUMP(b))             \
         ? (SCM_FLONUM_VALUE(a) op SCM_FLONUM_VALUE(b)) \
         : (Scm_NumCmp(a, b) op 0)))                    \
     ? SCM_TRUE                                         \
     : SCM_FALSE)

#define SCM_PC_NUMEQ2(a, b)  SCM_PC_NUMCMP2(a, b, ==)
#define SCM_PC_NUMLT2(a, b)  SCM_PC_NUMCMP2(a, b, <)
#define SCM_PC_NUMLE2(a, b)  SCM_PC_NUMCMP2(a, b, <=)
#define SCM_PC_NUMGT2(a, b)  SCM_PC_NUMCMP2(a, b, >)
#define SCM_PC_NUMGE2(a, b)  SCM_PC_NUMCMP2(a, b, >=)

#define SCM_PC_NUMCMPI(a, b, op)                        \
    (((SCM_INTP(a))                                     \
      ? (SCM_INT_VALUE(a) op b)                         \
      : ((SCM_FLONUMP(a))                               \
         ? (SCM_FLONUM_VALUE(a) op (double)(b))         \
         : (Scm_NumCmp(a, SCM_MAKE_INT(b)) op 0)))      \
     ? SCM_TRUE                                         \
     : SCM_FALSE)

#define SCM_PC_NUMEQI(a, b)  SCM_PC_NUMCMPI(a, b, ==)
#define SCM_PC_NUMLTI(a, b)  SCM_PC_NUMCMPI(a, b, <)
#define SCM_PC_NUMLEI(a, b)  SCM_PC_NUMCMPI(a, b, <=)
#define SCM_PC_NUMGTI(a, b)  SCM_PC_NUMCMPI(a, b, >)
#define SCM_PC_NUMGEI(a, b)  SCM_PC_NUMCMPI(a, b, >=)

/*
 * Arithmetic ops
 */

static inline ScmObj SCM_PC_NUMADD2(ScmObj a, ScmObj b)
{
    if (SCM_INTP(a) && SCM_INTP(b)) {
        ScmSmallInt r = SCM_INT_VALUE(a) + SCM_INT_VALUE(b);
        if (SCM_SMALL_INT_FITS(r)) return SCM_MAKE_INT(r);
        else return Scm_MakeInteger(r);
    }
    if (SCM_FLONUMP(a) && SCM_FLONUMP(b)) {
        return Scm_MakeFlonum(SCM_FLONUM_VALUE(a) + SCM_FLONUM_VALUE(b));
    }
    return Scm_Add(a, b);
}

static inline ScmObj SCM_PC_NUMADDI(ScmObj a, ScmSmallInt b)
{
    if (SCM_INTP(a)) {
        ScmSmallInt r = SCM_INT_VALUE(a) + b;
        if (SCM_SMALL_INT_FITS(r)) return SCM_MAKE_INT(r);
        else return Scm_MakeInteger(r);
    }
    if (SCM_FLONUMP(a)) {
        return Scm_MakeFlonum(SCM_FLONUM_VALUE(a) + (double)b);
    }
    return Scm_Add(a, SCM_MAKE_INT(b));
}

static inline ScmObj SCM_PC_NUMSUB2(ScmObj a, ScmObj b)
{
    if (SCM_INTP(a) && SCM_INTP(b)) {
        ScmSmallInt r = SCM_INT_VALUE(a) - SCM_INT_VALUE(b);
        if (SCM_SMALL_INT_FITS(r)) return SCM_MAKE_INT(r);
        else return Scm_MakeInteger(r);
    }
    if (SCM_FLONUMP(a) && SCM_FLONUMP(b)) {
        return Scm_MakeFlonum(SCM_FLONUM_VALUE(a) - SCM_FLONUM_VALUE(b));
    }
    return Scm_Add(a, b);
}

static inline ScmObj SCM_PC_NUMSUBI(ScmObj a, ScmSmallInt b)
{
    if (SCM_INTP(a)) {
        ScmSmallInt r = SCM_INT_VALUE(a) - b;
        if (SCM_SMALL_INT_FITS(r)) return SCM_MAKE_INT(r);
        else return Scm_MakeInteger(r);
    }
    if (SCM_FLONUMP(a)) {
        return Scm_MakeFlonum(SCM_FLONUM_VALUE(a) - (double)b);
    }
    return Scm_Add(a, SCM_MAKE_INT(b));
}

/*
 * Index extraction
 */

#define SCM_PC_GET_INDEX(obj)                                           \
    (SCM_UINTP(obj)                                                     \
     ? SCM_INT_VALUE(obj)                                               \
     : (Scm_Error("small nonnegative integer required, but got: %S", obj), \
        0))

/*
 * Type check
 */

#define SCM_PC_ENSURE_VEC(obj)                                  \
    (SCM_VECTORP(obj)                                           \
     ? SCM_VECTOR(obj)                                          \
     : (Scm_Error("vector required, but got %S", obj), NULL))

#define SCM_PC_BOUND_CHECK(size, n)                     \
    do {                                                \
        if ((n) >= (size))                              \
            Scm_Error("index out of range: %ld", (n));  \
    } whlie (0)

/*
 * Precompiled code specific API
 *
 *   These are to be called from machine-generated code, and not for
 *   general use.  Some APIs require specific precondition/postcondition
 *   about VM state.
 *   Those APIs takes ScmVM* pointer, which should match the executing
 *   thread's VM.  It is merely to avoid overhead of calling Scm_VM().
 */

typedef ScmObj ScmPContinuationProc(ScmVM *vm, ScmObj val0, ScmObj *data);

SCM_EXTERN ScmObj *Scm_pc_Alloca(ScmVM*, size_t);
SCM_EXTERN ScmObj *Scm_pc_PushCC(ScmVM*, ScmPContinuationProc, int);

SCM_EXTERN ScmObj Scm_pc_Apply0(ScmVM*, ScmObj);
SCM_EXTERN ScmObj Scm_pc_Apply1(ScmVM*, ScmObj, ScmObj);
SCM_EXTERN ScmObj Scm_pc_Apply2(ScmVM*, ScmObj, ScmObj, ScmObj);
SCM_EXTERN ScmObj Scm_pc_Apply3(ScmVM*, ScmObj, ScmObj, ScmObj, ScmObj);
SCM_EXTERN ScmObj Scm_pc_Apply4(ScmVM*, ScmObj, ScmObj, ScmObj, ScmObj, ScmObj);

#endif /* GAUCHE_PRECOMP_H */
