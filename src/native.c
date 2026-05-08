/*
 * native.c - dynamic native code generation
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

/* EXPERIMENTAL */

/* CAUTION
 *   Be careful not to create a code path to call these APIs directly
 *   from Scheme.  It would open up for casual Scheme code to execute
 *   arbitrary machine code, bypassing our runtime check completely.
 *
 *   Instead, the Scheme API for these functionalities should be placed in
 *   gauche.bootstrap module.  The module is only alive during initialization
 *   process, so the built-in Scheme procedures can call them, but they'll
 *   be unaccessible from the user code.  See libnative.scm.
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/priv/configP.h"
#include "gauche/priv/vmP.h"
#include "gauche/priv/mmapP.h"
#include "gauche/priv/nativeP.h"
#include "gauche/priv/typeP.h"

#if defined(HAVE_SYS_MMAN_H)
#include <sys/mman.h>
#endif

static long sys_getpagesize()
{
#if defined(GAUCHE_WINDOWS)
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return (long)sysinfo.dwPageSize;
#else  /* !GAUCHE_WINDOWS */
    return sysconf(_SC_PAGESIZE);
#endif /* !GAUCHE_WINDOWS */
}

/*======================================================================
 * FFI support
 */

/*
 * For the time being, we use a fixed area (mmapped executable page)
 * as the scratch code pad, and simply manage it like a stack.  That is,
 * the region below free is 'used'.  It's enough for simple FFI, since
 * the generated code won't live after the dynamic extent of FFI call.
 */
struct ScmCodeCacheRec {
    ScmMemoryRegion *wpad;      /* writable page */
    ScmMemoryRegion *xpad;      /* executable page */
    void *free;
};

#define CODE_PAD_SIZE 65536

static void init_code_cache(ScmVM *vm) {
    if (vm->codeCache != NULL) return;

    ScmCodeCache *cc = SCM_NEW(ScmCodeCache);
    Scm_SysMmapWX(CODE_PAD_SIZE, &cc->wpad, &cc->xpad);
    cc->free = cc->wpad->ptr;
    vm->codeCache = cc;
}

static inline void *allocate_code_cache(ScmVM *vm, size_t size)
{
    ScmCodeCache *cc = vm->codeCache;
    if (cc->free + size > cc->wpad->ptr + cc->wpad->size) {
        Scm_Error("VM code cache overflow");
    }
    void *region = cc->free;
    cc->free += size;
    return region;
}

static inline void free_code_cache(ScmVM *vm, void *ptr)
{
    ScmCodeCache *cc = vm->codeCache;
    SCM_ASSERT(ptr >= cc->wpad->ptr && ptr < cc->wpad->ptr + cc->wpad->size);
    cc->free = ptr;
}

/* Returns address in xpad that corresponds to the wpad_ptr. */
static inline void *get_entry_address(ScmCodeCache *cc, void *wpad_ptr)
{
    return cc->xpad->ptr + (wpad_ptr - cc->wpad->ptr);
}

#if defined(GAUCHE_WINDOWS)
/*
 * Windows x64 exception unwinding requires RUNTIME_FUNCTION + UNWIND_INFO
 * for every non-leaf function in dynamically generated code.  We pack both
 * structs into a single CpPdata block placed right after the codepad code,
 * register it with RtlAddFunctionTable before each call, and deregister it
 * afterward.
 * Cf.  https://learn.microsoft.com/en-us/cpp/build/exception-handling-x64
 *
 * Unwind operation codes used here:
 *   UWOP_ALLOC_SMALL (2): 1-node form, OpInfo = (size/8 - 1), size in 8..128
 *   UWOP_ALLOC_LARGE (1): 2-node form with OpInfo=0, extra slot = size/8
 */
#define CP_UWOP_ALLOC_LARGE  1
#define CP_UWOP_ALLOC_SMALL  2

/*
 * UNWIND_INFO with a single ALLOC unwind code, sized to fit two
 * UNWIND_CODE slots (UNWIND_INFO requires an even slot count).
 *
 *   bytes 0-3  UNWIND_INFO header
 *   bytes 4-5  UnwindCode[0]
 *   bytes 6-7  UnwindCode[1] (extra slot for ALLOC_LARGE; padding for SMALL)
 *
 * 8 bytes, 4-byte aligned.  The single-ALLOC simplification is
 * documented in memory/project_winx64_unwind.md.
 */
typedef struct {
    uint8_t  VersionFlags;      /* Version:3=1, Flags:5=0 */
    uint8_t  SizeOfProlog;
    uint8_t  CountOfCodes;
    uint8_t  FrameInfo;         /* FrameRegister=0, FrameOffset=0 */
    uint8_t  Code0Offset;
    uint8_t  Code0Op;           /* UnwindOp:4, OpInfo:4 */
    uint16_t Code1;
} UnwindBlock;

/*
 * CpPdata layout (20 bytes): RUNTIME_FUNCTION followed by its
 * UNWIND_INFO, co-located.  Used by the FFI-call codepad
 * (Scm__VMCallNative) and the single-callback pad
 * (Scm__InstallFFICallbackOne) — both register exactly one entry
 * with RtlAddFunctionTable.  For multi-entry tables (the callback
 * context path), the OS expects a stride-12 RUNTIME_FUNCTION array,
 * which is incompatible with this co-located layout, so that path
 * lays the RT_FUNC and UNWIND_INFO arrays out separately.
 */
typedef struct {
    uint32_t BeginAddress;
    uint32_t EndAddress;
    uint32_t UnwindData;        /* RVA of the embedded UnwindBlock */
    UnwindBlock unwind;
} CpPdata;

/* Allocation granularity (round to 8 bytes so the next codepad item aligns) */
#define CODEPAD_PDATA_SIZE  ((sizeof(CpPdata) + 7) & ~(size_t)7)

/* Fill an UNWIND_INFO block describing a prolog of size prolog_sz that
   adjusts RSP by frame_size bytes (single ALLOC; see CpPdata comment). */
static void fill_unwind_block(UnwindBlock *ub,
                              uint8_t prolog_sz,
                              ScmSmallInt frame_size)
{
    SCM_ASSERT(frame_size > 0 && (frame_size % 8) == 0);

    ub->VersionFlags = 1;            /* Version=1, Flags=0 */
    ub->SizeOfProlog = prolog_sz;
    ub->FrameInfo    = 0;
    ub->Code0Offset  = prolog_sz;

    if (frame_size <= 128) {
        /* UWOP_ALLOC_SMALL: 1 node, OpInfo = (size/8 - 1) */
        ub->CountOfCodes = 1;
        ub->Code0Op = (uint8_t)(CP_UWOP_ALLOC_SMALL
                                | (uint8_t)(((frame_size / 8) - 1) << 4));
        ub->Code1   = 0;
    } else {
        /* UWOP_ALLOC_LARGE OpInfo=0: 2 nodes, Code1 = size/8 */
        ub->CountOfCodes = 2;
        ub->Code0Op = (uint8_t)(CP_UWOP_ALLOC_LARGE);
        ub->Code1   = (uint16_t)(frame_size / 8);
    }
}

/* Fill a co-located CpPdata block (RUNTIME_FUNCTION + UNWIND_INFO).
   Used by the single-entry paths. */
static void fill_cpdata(CpPdata *pd,
                        uint32_t begin_rva,
                        uint32_t end_rva,
                        uint32_t unwind_rva,
                        uint8_t  prolog_sz,
                        ScmSmallInt frame_size)
{
    pd->BeginAddress = begin_rva;
    pd->EndAddress   = end_rva;
    pd->UnwindData   = unwind_rva;
    fill_unwind_block(&pd->unwind, prolog_sz, frame_size);
}

static void setup_codepad_pdata(ScmCodeCache *cc,
                                void *codepad,
                                size_t code_end,   /* EndAddress: past last instruction */
                                size_t pdata_off,  /* where to write CpPdata block */
                                ScmSmallInt entry,
                                ScmSmallInt prolog_end,
                                ScmSmallInt frame_size)
{
    SCM_ASSERT(prolog_end > entry && (prolog_end - entry) <= 255);

    size_t cp_xoff = (size_t)((char*)codepad - (char*)cc->wpad->ptr);
    fill_cpdata((CpPdata *)((char*)codepad + pdata_off),
                (uint32_t)(cp_xoff + entry),
                (uint32_t)(cp_xoff + code_end),
                (uint32_t)(cp_xoff + pdata_off
                           + offsetof(CpPdata, unwind)),
                (uint8_t)(prolog_end - entry),
                frame_size);
}
#endif /* GAUCHE_WINDOWS */

/*
 * Copy CODE to the scratch pad, starting from START-th byte up to right before
 * END-th byte, from the pad's TSTART-th position.
 *
 * TEND is, when non-zero, it must be greater than TSTART+(END-START).  The
 * scratch pad area is allocated up to TEND, and filled with zero after the
 * code.
 *
 * Finally, the code is called from the entry offset ENTRY.
 */

ScmObj Scm__VMCallNative(ScmVM *vm,
                         ScmSmallInt tstart,
                         ScmSmallInt tend,
                         ScmUVector *code,
                         ScmSmallInt start,
                         ScmSmallInt end,
                         ScmSmallInt entry,
                         /* The following two args are used on Windows.
                            On Linux, just pass 0. */
                         ScmSmallInt win_prolog_end SCM_UNUSED,
                         ScmSmallInt win_frame_size SCM_UNUSED)
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
    if (tend > (ScmSmallInt)codesize) realcodesize = tend;

    size_t alloc_size = realcodesize;
#if defined(GAUCHE_WINDOWS)
    /* For Windows, we add PDATA area after the code.  It should start
       at 4-byte alignment. */
    size_t orig_codesize = codesize;  /* before extending for spill slots */
    size_t pdata_off = (realcodesize + 3) & ~(size_t)3;
    if (win_prolog_end > 0 && win_frame_size > 0) {
        alloc_size = pdata_off + CODEPAD_PDATA_SIZE;
    }
#endif

    void *codepad = allocate_code_cache(vm, alloc_size);
    if (tstart > 0) memset(codepad, 0, tstart);
    memcpy(codepad + tstart,
           SCM_UVECTOR_ELEMENTS(code)+start,
           end - start);
    if (realcodesize > codesize) {
        memset(codepad + codesize, 0, realcodesize - codesize);
        codesize = realcodesize;
    }

    ScmObj result = SCM_UNDEFINED;
#if defined(GAUCHE_WINDOWS)
    void * volatile xpad_pdata = NULL;
#endif

    SCM_UNWIND_PROTECT {
        /*
         * On Windows x64, register RUNTIME_FUNCTION + UNWIND_INFO for the
         * codepad so the OS exception unwinder can traverse this frame.
         */
#if defined(GAUCHE_WINDOWS) && defined(__MINGW64__)
        if (win_prolog_end > 0 && win_frame_size > 0) {
            setup_codepad_pdata(vm->codeCache, codepad, orig_codesize,
                                pdata_off,
                                entry, win_prolog_end, win_frame_size);
            xpad_pdata = get_entry_address(vm->codeCache,
                                           (char*)codepad + pdata_off);
            RtlAddFunctionTable((PRUNTIME_FUNCTION)(void*)xpad_pdata, 1,
                                (DWORD64)vm->codeCache->xpad->ptr);
        }
#endif

        /*
         * Call the code
         */
        void *entryPtr = get_entry_address(vm->codeCache, codepad + entry);
        result = ((ScmObj (*)())entryPtr)();
    } SCM_WHEN_ERROR {
#if defined(GAUCHE_WINDOWS) && defined(__MINGW64__)
        if (xpad_pdata) {
            RtlDeleteFunctionTable((PRUNTIME_FUNCTION)(void*)xpad_pdata);
        }
#endif
        free_code_cache(vm, codepad);
        SCM_NEXT_HANDLER;
    } SCM_END_PROTECT;

#if defined(GAUCHE_WINDOWS) && defined(__MINGW64__)
    if (xpad_pdata) {
        RtlDeleteFunctionTable((PRUNTIME_FUNCTION)(void*)xpad_pdata);
    }
#endif
    free_code_cache(vm, codepad);
    return result;
}

/*======================================================================
 * FFI callback support
 *
 * For each group of foreign callbacks defined in a with-ffi block,
 * we mmap a fresh W/X region that holds the trampoline machine code.
 */

static size_t round_up_to_page(size_t n)
{
    long page = sys_getpagesize();
    return (n + (size_t)page - 1) / (size_t)page * (size_t)page;
}

/*--------------------------------------------------------------------
 * Single-callback install
 */

ScmObj Scm__InstallFFICallbackOne(ScmU8Vector *code,
                                  ScmSmallInt  entry,
                                  ScmSmallInt  win_prolog_end SCM_UNUSED,
                                  ScmSmallInt  win_frame_size SCM_UNUSED)
{
    SCM_ASSERT(SCM_U8VECTORP(code));
    ScmSmallInt csize = SCM_UVECTOR_SIZE(code);
    if (csize <= 0) Scm_Error("FFI callback code is empty");
    if (entry < 0 || entry >= csize) {
        Scm_Error("FFI callback entry out of range: %ld (code size %ld)",
                  (long)entry, (long)csize);
    }

    size_t alloc_size = (size_t)csize;
#if defined(GAUCHE_WINDOWS) && defined(__MINGW64__)
    /* Append a single CpPdata block, 4-byte aligned. */
    int need_pdata = (win_prolog_end > 0 && win_frame_size > 0);
    size_t pdata_off = ((size_t)csize + 3) & ~(size_t)3;
    if (need_pdata) alloc_size = pdata_off + CODEPAD_PDATA_SIZE;
#endif
    alloc_size = round_up_to_page(alloc_size);
    ScmMemoryRegion *wpad = NULL, *xpad = NULL;
    Scm_SysMmapWX(alloc_size, &wpad, &xpad);

    memcpy(wpad->ptr, SCM_UVECTOR_ELEMENTS(code), (size_t)csize);

    ScmFFICallbackPad *pad = SCM_NEW(ScmFFICallbackPad);
    SCM_SET_CLASS(pad, SCM_CLASS_FFI_CALLBACK_PAD);
    pad->wpad      = wpad;
    pad->xpad      = xpad;
    pad->code_size = (size_t)csize;
    pad->entry_off = (size_t)entry;
    pad->destroyed = 0;
#if defined(GAUCHE_WINDOWS) && defined(__MINGW64__)
    pad->pdata = NULL;
    if (need_pdata) {
        if (win_prolog_end <= entry || (win_prolog_end - entry) > 255) {
            Scm_Error("FFI callback: invalid prolog range "
                      "(entry=%ld, prolog-end=%ld)",
                      (long)entry, (long)win_prolog_end);
        }
        CpPdata *pd = (CpPdata *)((char*)wpad->ptr + pdata_off);
        fill_cpdata(pd,
                    (uint32_t)entry,
                    (uint32_t)csize,
                    (uint32_t)(pdata_off + offsetof(CpPdata, unwind)),
                    (uint8_t)(win_prolog_end - entry),
                    win_frame_size);
        pad->pdata = (char*)xpad->ptr + pdata_off;
        RtlAddFunctionTable((PRUNTIME_FUNCTION)pad->pdata, 1,
                            (DWORD64)xpad->ptr);
    }
#endif
    return SCM_OBJ(pad);
}

void *Scm__FFICallbackPadEntry(ScmFFICallbackPad *pad)
{
    if (pad->destroyed || pad->xpad == NULL || pad->xpad->ptr == NULL) {
        Scm_Error("FFI callback pad has been destroyed");
    }
    return (char*)pad->xpad->ptr + pad->entry_off;
}

void Scm__DestroyFFICallbackPad(ScmFFICallbackPad *pad)
{
    if (pad->destroyed) return;
    pad->destroyed = 1;
#if defined(GAUCHE_WINDOWS) && defined(__MINGW64__)
    if (pad->pdata) {
        RtlDeleteFunctionTable((PRUNTIME_FUNCTION)pad->pdata);
        pad->pdata = NULL;
    }
#endif
    Scm_SysMunmap(pad->wpad);
    Scm_SysMunmap(pad->xpad);
    pad->wpad = NULL;
    pad->xpad = NULL;
}

/*--------------------------------------------------------------------
 * Batched per-with-ffi context install
 */

ScmObj Scm__InstallFFICallbackContext(const ScmFFICallbackSpec *specs,
                                      int nspecs)
{
    if (nspecs < 0) Scm_Error("nspecs must be non-negative: %d", nspecs);
    if (nspecs == 0) Scm_Error("FFI callback context with no entries");

    /* Lay out all callbacks back-to-back.  Each callback's code is
       8-byte aligned so that any data section embedded in the
       trampoline (label tables, helper addresses, etc.) keeps its
       natural alignment.  Later, the PDATA blocks will be appended
       after the last callback, also 4-byte aligned. */
    size_t *offsets = SCM_NEW_ARRAY(size_t, nspecs);
    size_t  total = 0;
    for (int i = 0; i < nspecs; i++) {
        ScmU8Vector *code = specs[i].code;
        SCM_ASSERT(SCM_U8VECTORP(code));
        ScmSmallInt csize = SCM_UVECTOR_SIZE(code);
        ScmSmallInt e     = specs[i].entry;
        if (csize <= 0) Scm_Error("FFI callback %d: empty code", i);
        if (e < 0 || e >= csize) {
            Scm_Error("FFI callback %d: entry out of range: %ld (code size %ld)",
                      i, (long)e, (long)csize);
        }
        total = (total + 7) & ~(size_t)7;
        offsets[i] = total;
        total += (size_t)csize;
    }

    size_t alloc_size = total;
#if defined(GAUCHE_WINDOWS) && defined(__MINGW64__)
    /* RtlAddFunctionTable consumes a contiguous stride-12 array of
       RUNTIME_FUNCTIONs, so for multi-entry tables we cannot use the
       CpPdata co-located layout from Scm__VMCallNative (where each
       RT_FUNC is followed by its own UNWIND_INFO).  Lay out two
       parallel arrays instead — RT_FUNCs first, then UNWIND_INFOs —
       and have UnwindData RVAs cross-reference into the second array. */
    int any_pdata = 0;
    for (int i = 0; i < nspecs; i++) {
        if (specs[i].win_prolog_end > 0 && specs[i].win_frame_size > 0) {
            any_pdata = 1;
            break;
        }
    }
    /* RUNTIME_FUNCTION = 3 DWORDs = 12 bytes.  UnwindBlock is 8 bytes. */
    size_t rt_off = (total + 3) & ~(size_t)3;
    size_t ui_off = rt_off + (size_t)nspecs * 12;
    if (any_pdata) {
        alloc_size = ui_off + (size_t)nspecs * sizeof(UnwindBlock);
    }
#endif
    alloc_size = round_up_to_page(alloc_size);
    ScmMemoryRegion *wpad = NULL, *xpad = NULL;
    Scm_SysMmapWX(alloc_size, &wpad, &xpad);

    /* Copy each code blob into its slot via the writable view. */
    for (int i = 0; i < nspecs; i++) {
        ScmU8Vector *code = specs[i].code;
        ScmSmallInt csize = SCM_UVECTOR_SIZE(code);
        memcpy((char*)wpad->ptr + offsets[i],
               SCM_UVECTOR_ELEMENTS(code),
               (size_t)csize);
    }

    /* Translate offsets[i] -> absolute entry offset in xpad. */
    size_t *entry_offs = SCM_NEW_ARRAY(size_t, nspecs);
    for (int i = 0; i < nspecs; i++) {
        entry_offs[i] = offsets[i] + (size_t)specs[i].entry;
    }

    ScmFFICallbackContext *ctx = SCM_NEW(ScmFFICallbackContext);
    SCM_SET_CLASS(ctx, SCM_CLASS_FFI_CALLBACK_CONTEXT);
    ctx->wpad       = wpad;
    ctx->xpad       = xpad;
    ctx->n_entries  = nspecs;
    ctx->entry_offs = entry_offs;
    ctx->destroyed  = 0;
#if defined(GAUCHE_WINDOWS) && defined(__MINGW64__)
    ctx->pdata_table = NULL;
    if (any_pdata) {
        /* Fill the parallel RT_FUNC and UNWIND_INFO arrays, then register
           the RT_FUNC array with the OS in one RtlAddFunctionTable call. */
        for (int i = 0; i < nspecs; i++) {
            ScmSmallInt csize = SCM_UVECTOR_SIZE(specs[i].code);
            ScmSmallInt e     = specs[i].entry;
            ScmSmallInt pe    = specs[i].win_prolog_end;
            ScmSmallInt fs    = specs[i].win_frame_size;
            if (pe <= 0 || fs <= 0) {
                Scm_Error("FFI callback %d: missing PDATA "
                          "(prolog-end=%ld, frame-size=%ld); "
                          "all callbacks in a context must supply unwind info "
                          "on Windows", i, (long)pe, (long)fs);
            }
            if (pe <= e || (pe - e) > 255) {
                Scm_Error("FFI callback %d: invalid prolog range "
                          "(entry=%ld, prolog-end=%ld)",
                          i, (long)e, (long)pe);
            }

            size_t this_rt_off = rt_off + (size_t)i * 12;
            size_t this_ui_off = ui_off + (size_t)i * sizeof(UnwindBlock);

            /* RUNTIME_FUNCTION (3 DWORDs); written as a uint32 triple to
               avoid relying on winnt.h's struct definition leaking into
               the layout. */
            uint32_t *rt = (uint32_t *)((char*)wpad->ptr + this_rt_off);
            rt[0] = (uint32_t)(offsets[i] + e);     /* BeginAddress */
            rt[1] = (uint32_t)(offsets[i] + csize); /* EndAddress */
            rt[2] = (uint32_t)this_ui_off;          /* UnwindData (RVA) */

            UnwindBlock *ub = (UnwindBlock *)((char*)wpad->ptr + this_ui_off);
            fill_unwind_block(ub, (uint8_t)(pe - e), fs);
        }
        ctx->pdata_table = (char*)xpad->ptr + rt_off;
        RtlAddFunctionTable((PRUNTIME_FUNCTION)ctx->pdata_table, nspecs,
                            (DWORD64)xpad->ptr);
    }
#endif
    return SCM_OBJ(ctx);
}

void *Scm__FFICallbackContextEntry(ScmFFICallbackContext *ctx, int i)
{
    if (ctx->destroyed || ctx->xpad == NULL || ctx->xpad->ptr == NULL) {
        Scm_Error("FFI callback context has been destroyed");
    }
    if (i < 0 || i >= ctx->n_entries) {
        Scm_Error("FFI callback context: index out of range: %d (have %d)",
                  i, ctx->n_entries);
    }
    return (char*)ctx->xpad->ptr + ctx->entry_offs[i];
}

void Scm__DestroyFFICallbackContext(ScmFFICallbackContext *ctx)
{
    if (ctx->destroyed) return;
    ctx->destroyed = 1;
#if defined(GAUCHE_WINDOWS) && defined(__MINGW64__)
    if (ctx->pdata_table) {
        RtlDeleteFunctionTable((PRUNTIME_FUNCTION)ctx->pdata_table);
        ctx->pdata_table = NULL;
    }
#endif
    Scm_SysMunmap(ctx->wpad);
    Scm_SysMunmap(ctx->xpad);
    ctx->wpad = NULL;
    ctx->xpad = NULL;
}

/*======================================================================
 * JIT support
 */

/* Allocate executable page and copy the machine code in CODE,
   returns an executable <memory-region>.
 */

ScmObj Scm__AllocateCodePage(ScmU8Vector *code)
{
    ScmMemoryRegion *wpad, *xpad;
    long codesize = SCM_U8VECTOR_SIZE(code);
    long pagesize = sys_getpagesize();
    long padsize = ((codesize+pagesize-1)/pagesize)*pagesize;
    Scm_SysMmapWX(padsize, &wpad, &xpad);
    memcpy(wpad->ptr, SCM_U8VECTOR_ELEMENTS(code), codesize);
    /* TODO: If writable page and executable page are mapped into
       two regions, we can unmap writable page here.  Currently we don't
       have an interface to do so explicitly, leaving GC finalizer to
       do the job.
     */
    return SCM_OBJ(xpad);
}


/*======================================================================
 * Initialization
 */

void Scm__InitNative(void)
{
}
