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

/* We eventually manage mmap'ed page wisely as a code cache */
struct ScmCodeCacheRec {
    ScmMemoryRegion *pad;
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
    vm->codeCache = cc;
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
    if (tstart < 0 || tstart + end - start > vm->codeCache->pad->size) {
        Scm_Error("tstart out of range: %ld", tstart);
    }
    if (entry < 0 || entry >= tstart + end - start) {
        Scm_Error("entry out of range: %ld", entry);
    }

    if (tstart > 0) memset(vm->codeCache->pad->ptr, 0, tstart);
    memcpy(vm->codeCache->pad->ptr + tstart,
           SCM_UVECTOR_ELEMENTS(code)+start,
           end - start);

    /* 
     * Patch it
     */
    void *base  = vm->codeCache->pad->ptr;
    void *limit = base + vm->codeCache->pad->size;

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
            patch1(base, pos, pun.bn, SIZEOF_INTPTR_T, limit);
        } else if (SCM_EQ(type, sym_p)) {
            if (!Scm_DLPtrP(val)) SCM_TYPE_ERROR(val, "dlptr");
            pun.n = SCM_FOREIGN_POINTER_REF(intptr_t, val);
            patch1(base, pos, pun.bn, SIZEOF_INTPTR_T, limit);
        } else if (SCM_EQ(type, sym_i)) {
            pun.n = Scm_IntegerToIntptr(val);
            patch1(base, pos, pun.bn, SIZEOF_INTPTR_T, limit);
        } else if (SCM_EQ(type, sym_d)) {
            pun.d = Scm_GetDouble(val);
            patch1(base, pos, pun.bd, SIZEOF_DOUBLE, limit);
        } else if (SCM_EQ(type, sym_f)) {
            pun.f = (float)Scm_GetDouble(val);
            patch1(base, pos, pun.bf, SIZEOF_FLOAT, limit);
        } else if (SCM_EQ(type, sym_s)) {
            /* NB: If the callee retains the pointer, we need malloc. */
            if (!SCM_STRINGP(val)) SCM_TYPE_ERROR(val, "string");
            pun.n = (intptr_t)Scm_GetStringConst(SCM_STRING(val));
            patch1(base, pos, pun.bn, SIZEOF_INTPTR_T, limit);
        } else {
            Scm_Error("unknown patch type: %S", type);
        }
    }

    /* 
     * Call the code
     */
    void *entryPtr = vm->codeCache->pad->ptr + entry;
    if (SCM_EQ(rettype, sym_d)) {
        double r = ((double (*)())entryPtr)();
        return Scm_VMReturnFlonum(r);
    } else if (SCM_EQ(rettype, sym_f)) {
        float r = ((float (*)())entryPtr)();
        return Scm_VMReturnFlonum((double)r);
    } else if (SCM_EQ(rettype, sym_s)) {
        intptr_t r = ((intptr_t (*)())entryPtr)();
        return SCM_MAKE_STR_COPYING((const char*)r);
    } else if (SCM_EQ(rettype, sym_i)) {
        intptr_t r = ((intptr_t (*)())entryPtr)();
        return Scm_IntptrToInteger(r);
    } else if (SCM_EQ(rettype, sym_o)) {
        intptr_t r = ((intptr_t (*)())entryPtr)();
        return SCM_OBJ(r);      /* trust the caller */
    } else if (SCM_EQ(rettype, sym_v)) {
        ((void (*)())entryPtr)();
        return SCM_UNDEFINED;
    } else {
        Scm_Error("unknown return type: %S", rettype);
        return SCM_UNDEFINED;   /* dummy */
    }
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

