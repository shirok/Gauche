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

#if defined(HAVE_SYS_MMAN_H)

#include <sys/mman.h>

/* We eventually manage mmap'ed page wisely as a code cache */
struct ScmCodeCacheRec {
    ScmMemoryRegion *pad;
};


#define CODE_PAD_SIZE 4096

static void init_code_cache(ScmVM *vm) {
    if (vm->codeCache != NULL) return;
    
    ScmCodeCache *cc = SCM_NEW(ScmCodeCache);
    cc->pad = SCM_MEMORY_REGION(Scm_SysMmap(NULL, -1, CODE_PAD_SIZE, 0,
                                            PROT_READ|PROT_WRITE|PROT_EXEC,
                                            MAP_PRIVATE|MAP_ANONYMOUS));
    vm->codeCache = cc;
}

ScmObj Scm__VMCallNative(ScmVM *vm, ScmUVector *code)
{
    init_code_cache(vm);

    SCM_ASSERT(SCM_U8VECTORP(code));

    ScmSmallInt size = SCM_UVECTOR_SIZE(code);
    memcpy(vm->codeCache->pad->ptr, SCM_UVECTOR_ELEMENTS(code), size);
    ScmObj z = ((ScmObj (*)())vm->codeCache->pad->ptr)();
    return z;
}

#endif /*HAVE_SYS_MMAM_H*/
