/*
 * uvinit.c - initialize routine for uvector extension
 *
 *   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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

#include <gauche.h>
#include <gauche/extend.h>
#define EXTUVECTOR_EXPORTS
#include "gauche/uvector.h"
#include "uvectorP.h"

/*
 * Reader extension
 */
static ScmObj read_uvector(ScmPort *port, const char *tag,
                           ScmReadContext *ctx)
{
    ScmChar c;
    ScmObj list, uv = SCM_UNDEFINED;

    SCM_GETC(c, port);
    if (c != '(') Scm_Error("bad uniform vector syntax for %s", tag);
    list = Scm_ReadList(SCM_OBJ(port), ')');
    if (strcmp(tag, "s8") == 0)  uv = Scm_ListToS8Vector(list, 0);
    else if (strcmp(tag, "u8") == 0)  uv = Scm_ListToU8Vector(list, 0);
    else if (strcmp(tag, "s16") == 0) uv = Scm_ListToS16Vector(list, 0);
    else if (strcmp(tag, "u16") == 0) uv = Scm_ListToU16Vector(list, 0);
    else if (strcmp(tag, "s32") == 0) uv = Scm_ListToS32Vector(list, 0);
    else if (strcmp(tag, "u32") == 0) uv = Scm_ListToU32Vector(list, 0);
    else if (strcmp(tag, "s64") == 0) uv = Scm_ListToS64Vector(list, 0);
    else if (strcmp(tag, "u64") == 0) uv = Scm_ListToU64Vector(list, 0);
    else if (strcmp(tag, "f16") == 0) uv = Scm_ListToF16Vector(list, 0);
    else if (strcmp(tag, "f32") == 0) uv = Scm_ListToF32Vector(list, 0);
    else if (strcmp(tag, "f64") == 0) uv = Scm_ListToF64Vector(list, 0);
    else Scm_Error("invalid unform vector tag: %s", tag);
    /* If we are reading source file, let literal uvectors be immutable. */
    if (ctx->flags & SCM_READ_LITERAL_IMMUTABLE) {
        SCM_UVECTOR_IMMUTABLE_P(uv) = TRUE;
    }
    return uv;
}

/*
 * Initialization
 */
extern void Scm_Init_uvlib(ScmModule *);
extern void Scm_Init_uvutil();
extern void Scm_Init_uvseq(void);

SCM_EXTENSION_ENTRY void Scm_Init_libgauche_uvector(void)
{
    ScmModule *m;

    SCM_INIT_EXTENSION(uvector);
    m = SCM_FIND_MODULE("gauche.uvector", SCM_FIND_MODULE_CREATE);
    Scm_InitStaticClassWithMeta(&Scm_UVectorClass,   "<uvector>", m, NULL, SCM_NIL, NULL, 0);
    Scm_InitStaticClassWithMeta(&Scm_S8VectorClass,  "<s8vector>", m, NULL, SCM_NIL, NULL, 0);
    Scm_InitStaticClassWithMeta(&Scm_U8VectorClass,  "<u8vector>", m, NULL, SCM_NIL, NULL, 0);
    Scm_InitStaticClassWithMeta(&Scm_S16VectorClass, "<s16vector>", m, NULL, SCM_NIL, NULL, 0);
    Scm_InitStaticClassWithMeta(&Scm_U16VectorClass, "<u16vector>", m, NULL, SCM_NIL, NULL, 0);
    Scm_InitStaticClassWithMeta(&Scm_S32VectorClass, "<s32vector>", m, NULL, SCM_NIL, NULL, 0);
    Scm_InitStaticClassWithMeta(&Scm_U32VectorClass, "<u32vector>", m, NULL, SCM_NIL, NULL, 0);
    Scm_InitStaticClassWithMeta(&Scm_S64VectorClass, "<s64vector>", m, NULL, SCM_NIL, NULL, 0);
    Scm_InitStaticClassWithMeta(&Scm_U64VectorClass, "<u64vector>", m, NULL, SCM_NIL, NULL, 0);
    Scm_InitStaticClassWithMeta(&Scm_F16VectorClass, "<f16vector>", m, NULL, SCM_NIL, NULL, 0);
    Scm_InitStaticClassWithMeta(&Scm_F32VectorClass, "<f32vector>", m, NULL, SCM_NIL, NULL, 0);
    Scm_InitStaticClassWithMeta(&Scm_F64VectorClass, "<f64vector>", m, NULL, SCM_NIL, NULL, 0);

    Scm_Init_uvlib(m);
    Scm_Init_uvutil();
    Scm__InstallReadUvectorHook(read_uvector);
}
