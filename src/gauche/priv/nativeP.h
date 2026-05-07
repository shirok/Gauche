/*
 * gauche/nativeP.h - Internal interface for native code generation
 *
 *   Copyright (c) 2021-2025  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PRIV_NATIVEP_H
#define GAUCHE_PRIV_NATIVEP_H

#if defined(HAVE_STDALIGN_H)
#define SCM_ALIGNOF(type) _Alignof(type)
#else  /*!defined(HAVE_STDALIGN_H)*/
#define SCM_ALIGNOF(type) offsetof(struct { char c; type m; }, m)
#endif /*!defined(HAVE_STDALIGN_H)*/


SCM_EXTERN ScmObj Scm__VMCallNative(ScmVM *vm,
                                    ScmSmallInt tstart,
                                    ScmSmallInt tend,
                                    ScmUVector *code,
                                    ScmSmallInt start,
                                    ScmSmallInt end,
                                    ScmSmallInt entry,
                                    ScmSmallInt win_prolog_end,
                                    ScmSmallInt win_frame_size);

SCM_EXTERN ScmObj Scm__AllocateCodePage(ScmU8Vector *code);

/*
 * FFI callback codepad
 *
 * An executable region that holds the trampoline machine code
 * for FFI callbacks.
 *
 * We keep the writable view (wpad) and the executable view (xpad)
 * separately, mirroring ScmCodeCache.  On platforms that allow a
 * single W|X mapping (the current case) Scm_SysMmapWX returns the
 * same ScmMemoryRegion for both, but on platforms with W^X (e.g.
 * Pax MPROTECT) they will differ; addresses written into wpad are
 * translated to xpad-relative addresses for execution and external
 * exposure.
 */

typedef struct ScmFFICallbackPadRec ScmFFICallbackPad;

struct ScmFFICallbackPadRec {
    SCM_HEADER;
    ScmMemoryRegion *wpad;      /* writable view; NULL after destroy */
    ScmMemoryRegion *xpad;      /* executable view; may equal wpad */
    size_t code_size;           /* bytes copied into wpad */
    size_t entry_off;           /* entry offset in bytes from xpad->ptr */
    int    destroyed;           /* set by Scm__DestroyFFICallbackPad */
#if defined(GAUCHE_WINDOWS) && defined(__MINGW64__)
    void  *pdata;               /* RUNTIME_FUNCTION* registered with the OS;
                                   NULL until Phase 7 */
#endif
};

SCM_CLASS_DECL(Scm_FFICallbackPadClass);
#define SCM_CLASS_FFI_CALLBACK_PAD     (&Scm_FFICallbackPadClass)
#define SCM_FFI_CALLBACK_PAD(obj)      ((ScmFFICallbackPad*)obj)
#define SCM_FFI_CALLBACK_PAD_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_FFI_CALLBACK_PAD)

SCM_EXTERN ScmObj Scm__InstallFFICallbackOne(ScmU8Vector *code,
                                             ScmSmallInt  entry,
                                             ScmSmallInt  win_prolog_end,
                                             ScmSmallInt  win_frame_size);
SCM_EXTERN void  *Scm__FFICallbackPadEntry(ScmFFICallbackPad *pad);
SCM_EXTERN void   Scm__DestroyFFICallbackPad(ScmFFICallbackPad *pad);


typedef struct ScmFFICallbackContextRec ScmFFICallbackContext;

/* One entry of a batched install request. */
typedef struct {
    ScmU8Vector *code;
    ScmSmallInt  entry;
    ScmSmallInt  win_prolog_end;   /* 0 on non-Windows */
    ScmSmallInt  win_frame_size;   /* 0 on non-Windows */
} ScmFFICallbackSpec;

struct ScmFFICallbackContextRec {
    SCM_HEADER;
    ScmMemoryRegion *wpad;       /* writable view; NULL after destroy */
    ScmMemoryRegion *xpad;       /* executable view; may equal wpad */
    int     n_entries;
    size_t *entry_offs;          /* n_entries elements, byte offsets in xpad */
    int     destroyed;
#if defined(GAUCHE_WINDOWS) && defined(__MINGW64__)
    void  *pdata_table;          /* RUNTIME_FUNCTION array; NULL until Phase 7 */
#endif
};

SCM_CLASS_DECL(Scm_FFICallbackContextClass);
#define SCM_CLASS_FFI_CALLBACK_CONTEXT   (&Scm_FFICallbackContextClass)
#define SCM_FFI_CALLBACK_CONTEXT(obj)    ((ScmFFICallbackContext*)obj)
#define SCM_FFI_CALLBACK_CONTEXT_P(obj)  \
    SCM_XTYPEP(obj, SCM_CLASS_FFI_CALLBACK_CONTEXT)

SCM_EXTERN ScmObj Scm__InstallFFICallbackContext(const ScmFFICallbackSpec *specs,
                                                 int nspecs);
SCM_EXTERN void  *Scm__FFICallbackContextEntry(ScmFFICallbackContext *ctx,
                                               int i);
SCM_EXTERN void   Scm__DestroyFFICallbackContext(ScmFFICallbackContext *ctx);

#endif /*GAUCHE_PRIV_NATIVEP_H*/
