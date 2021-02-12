/*
 * native.c - dynamic native code generation 
 *
 *   Copyright (c) 2021  Shiro Kawai  <shiro@acm.org>
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

/* EXPERIMENTAL */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/priv/vmP.h"
#include "gauche/priv/mmapP.h"
#include "gauche/priv/nativeP.h"

#if defined(HAVE_SYS_MMAN_H)
#include <sys/mman.h>
#endif

/* 
 * For the time being, we use a fixed area (mmapped executable page)
 * as the scratch code pad, and simply manage it like a stack.  That is,
 * the region below free is 'used'.  It's enough for simple FFI, since
 * the generated code won't live after the dynamic extent of FFI call.
 */
struct ScmCodeCacheRec {
    ScmMemoryRegion *pad;
    void *free;
};

#define CODE_PAD_SIZE 4096

static ScmObj sym_o;
static ScmObj sym_p;
static ScmObj sym_i;
static ScmObj sym_d;
static ScmObj sym_f;
static ScmObj sym_s;
static ScmObj sym_v;

static void init_code_cache(ScmVM *vm) {
    if (vm->codeCache != NULL) return;
    
    ScmCodeCache *cc = SCM_NEW(ScmCodeCache);
    cc->pad = SCM_MEMORY_REGION(Scm_SysMmap(NULL, -1, CODE_PAD_SIZE, 0,
                                            PROT_READ|PROT_WRITE|PROT_EXEC,
                                            MAP_PRIVATE|MAP_ANONYMOUS));
    cc->free = cc->pad->ptr;
    vm->codeCache = cc;
}

static inline void *allocate_code_cache(ScmVM *vm, size_t size)
{
    ScmCodeCache *cc = vm->codeCache;
    if (cc->free + size > cc->pad->ptr + cc->pad->size) {
        Scm_Error("VM code cache overflow");
    }
    void *region = cc->free;
    cc->free += size;
    return region;
}

static inline void free_code_cache(ScmVM *vm, void *ptr)
{
    ScmCodeCache *cc = vm->codeCache;
    SCM_ASSERT(ptr >= cc->pad->ptr && ptr < cc->pad->ptr + cc->pad->size);
    cc->free = ptr;
}

/* 
 * some utility to 'patch' the code array
 */

typedef union {
    intptr_t n;
    uint8_t bn[SIZEOF_INTPTR_T];

    double d;
    uint8_t bd[SIZEOF_DOUBLE];
               
    float f;
    uint8_t bf[SIZEOF_FLOAT];
} pun_t;

static inline void patch1(void *dst, ScmSmallInt pos,
                         uint8_t *src, ScmSmallInt size, void *lim)
{
    if (dst + pos + size > lim) {
        Scm_Error("filler position out of range: %ld", pos);
    }
    memcpy(dst+pos, src, size);
}

/*  
 * Copy CODE to the scratch pad, starting from START-th byte up to right before
 * END-th byte, from the pad's TSTART-th position.
 *
 * TEND is, when non-zero, it must be greater than TSTART+(END-START).  The
 * scratch pad area is allocated up to TEND, and filled with zero after the
 * code.
 *
 * Then the pad is patched according to PATCHER, as explained below.
 *
 * Finally, the code is called from the entry offset ENTRY.
 *
 * PATCHER has the following list:
 *   ((<pos> <type> <value>) ...)
 *
 *    <pos>  - specifies the position in the byte array to be filled.
 *          
 *    <type> - one of the following symbols:
 *        o : ScmObj.   <value>'s ScmObj is used as is.
 *        p : pointer.  <value> is <dlptr>.
 *        i : integer.  <value> must be an integral type or a character; 
 *                      its integer value is used.
 *        d : double.   <value> must be a real number.
 *        f : float.    <value> must be a real number.
 *        s : string    <value> must be a string type.  Pointer to the cstring
 *                      is used.
 *
 *    <value> - Scheme value to pass.
 *
 * RETTYPE is also a symbol similar to <type>, plus 'v' as no value.
 */

ScmObj Scm__VMCallNative(ScmVM *vm, 
                         ScmSmallInt tstart,
                         ScmSmallInt tend,
                         ScmUVector *code,
                         ScmSmallInt start,
                         ScmSmallInt end,
                         ScmSmallInt entry,
                         ScmObj patcher,
                         ScmObj rettype)
{
    init_code_cache(vm);

    SCM_ASSERT(SCM_U8VECTORP(code));

    ScmSmallInt uvsize = SCM_UVECTOR_SIZE(code);
    SCM_CHECK_START_END(start, end, uvsize);

    size_t codesize = tstart + end - start;
    if (entry < 0 || (size_t)entry >= codesize) {
        Scm_Error("entry out of range: %ld", entry);
    }

    size_t realcodesize = codesize;
    if (tend > codesize) realcodesize = tend;
    
    void *codepad = allocate_code_cache(vm, realcodesize);
    if (tstart > 0) memset(codepad, 0, tstart);
    memcpy(codepad + tstart,
           SCM_UVECTOR_ELEMENTS(code)+start,
           end - start);
    if (realcodesize > codesize) {
        memset(codepad + codesize, 0, realcodesize - codesize);
        codesize = realcodesize;
    }

    ScmObj result = SCM_UNDEFINED;

    SCM_UNWIND_PROTECT {
        /* 
         * Patch it
         */
        void *limit = codepad + codesize;

        ScmObj cp;
        SCM_FOR_EACH(cp, patcher) {
            ScmObj e = SCM_CAR(cp);
            if (Scm_Length(e) != 3) {
                Scm_Error("malformed filler entry: %S", e);
            }
            ScmObj s_pos = SCM_CAR(e);
            ScmObj type = SCM_CADR(e);
            ScmObj val = SCM_CAR(SCM_CDDR(e));
        
            if (!SCM_INTP(s_pos) || !SCM_SYMBOLP(type)) {
                Scm_Error("bad filler entry: %S", e);
            }
            ScmSmallInt pos = SCM_INT_VALUE(s_pos);

            pun_t pun;
        
            if (SCM_EQ(type, sym_o)) {
                pun.n = (intptr_t)val;
                patch1(codepad, pos, pun.bn, SIZEOF_INTPTR_T, limit);
            } else if (SCM_EQ(type, sym_p)) {
                if (!Scm_DLPtrP(val)) SCM_TYPE_ERROR(val, "dlptr");
                pun.n = SCM_FOREIGN_POINTER_REF(intptr_t, val);
                patch1(codepad, pos, pun.bn, SIZEOF_INTPTR_T, limit);
            } else if (SCM_EQ(type, sym_i)) {
                pun.n = Scm_IntegerToIntptr(val);
                patch1(codepad, pos, pun.bn, SIZEOF_INTPTR_T, limit);
            } else if (SCM_EQ(type, sym_d)) {
                pun.d = Scm_GetDouble(val);
                patch1(codepad, pos, pun.bd, SIZEOF_DOUBLE, limit);
            } else if (SCM_EQ(type, sym_f)) {
                pun.f = (float)Scm_GetDouble(val);
                patch1(codepad, pos, pun.bf, SIZEOF_FLOAT, limit);
            } else if (SCM_EQ(type, sym_s)) {
                /* NB: If the callee retains the pointer, we need malloc. */
                if (!SCM_STRINGP(val)) SCM_TYPE_ERROR(val, "string");
                pun.n = (intptr_t)Scm_GetStringConst(SCM_STRING(val));
                patch1(codepad, pos, pun.bn, SIZEOF_INTPTR_T, limit);
            } else {
                Scm_Error("unknown patch type: %S", type);
            }
        }

        /* 
         * Call the code
         */
        void *entryPtr = codepad + entry;
        if (SCM_EQ(rettype, sym_d)) {
            double r = ((double (*)())entryPtr)();
            result = Scm_VMReturnFlonum(r);
        } else if (SCM_EQ(rettype, sym_f)) {
            float r = ((float (*)())entryPtr)();
            result = Scm_VMReturnFlonum((double)r);
        } else if (SCM_EQ(rettype, sym_s)) {
            intptr_t r = ((intptr_t (*)())entryPtr)();
            result = SCM_MAKE_STR_COPYING((const char*)r);
        } else if (SCM_EQ(rettype, sym_i)) {
            intptr_t r = ((intptr_t (*)())entryPtr)();
            result = Scm_IntptrToInteger(r);
        } else if (SCM_EQ(rettype, sym_o)) {
            intptr_t r = ((intptr_t (*)())entryPtr)();
            result = SCM_OBJ(r);      /* trust the caller */
        } else if (SCM_EQ(rettype, sym_v)) {
            ((void (*)())entryPtr)();
        } else {
            Scm_Error("unknown return type: %S", rettype);
        }
    } SCM_WHEN_ERROR {
        free_code_cache(vm, codepad);
        SCM_NEXT_HANDLER;
    } SCM_END_PROTECT;
    
    free_code_cache(vm, codepad);
    return result;
}

void Scm__InitNative(void)
{
    /* symbols for type */
    sym_o = SCM_INTERN("o");
    sym_p = SCM_INTERN("p");
    sym_i = SCM_INTERN("i");
    sym_d = SCM_INTERN("d");
    sym_f = SCM_INTERN("f");
    sym_s = SCM_INTERN("s");
    sym_v = SCM_INTERN("v");
}

