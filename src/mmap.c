/*
 * mmap.c - memory mapping
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

/*
 * Abstraction of memory mapping
 * Scheme API and class definition of <memory-region> is in libsys.scm
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/priv/atomicP.h"
#include "gauche/priv/mmapP.h"

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

static void mem_finalize(ScmObj obj, void *data SCM_UNUSED)
{
    ScmMemoryRegion *m = SCM_MEMORY_REGION(obj);
    if (m->ptr != NULL) {
#if !defined(GAUCHE_WINDOWS)
        int r;
        SCM_SYSCALL(r, munmap(m->ptr, m->size));
        if (r < 0) Scm_Warn("munmap failed");
        m->ptr = NULL;
#else  /*GAUCHE_WINDOWS*/
        if (!UnmapViewOfFile(m->ptr)) {
            Scm_SysError("UnmapViewOfFile failed");
        }
        m->ptr = NULL;
        if (!CloseHandle(m->fileMapping)) {
            Scm_SysError("CloseHandle failed");
        }
        m->fileMapping = INVALID_HANDLE_VALUE;
#endif /*GAUCHE_WINDOWS*/
    }
}

static ScmObj make_memory_region(void *ptr, size_t size, int prot, int flags
#if defined(GAUCHE_WINDOWS)
                                 , HANDLE fileMapping
#endif
                                 )
{
    ScmMemoryRegion *m = SCM_NEW(ScmMemoryRegion);
    SCM_SET_CLASS(m, SCM_CLASS_MEMORY_REGION);
    m->ptr = ptr;
    m->size = size;
    m->prot = prot;
    m->flags = flags;
#if defined(GAUCHE_WINDOWS)
    m->fileMapping = fileMapping;
#endif
    Scm_RegisterFinalizer(SCM_OBJ(m), mem_finalize, NULL);
    return SCM_OBJ(m);
}

static void *mmap_int(void *addrhint, size_t len, int prot, int flags,
                      int fd, size_t off
#if defined(GAUCHE_WINDOWS)
                      , HANDLE *pmapping
#endif
                      )
{
#if !defined(GAUCHE_WINDOWS)
    void *ptr;
    SCM_SYSCALL3(ptr, mmap(addrhint, len, prot, flags, fd, off),
                 ptr == MAP_FAILED);
    if (ptr == MAP_FAILED) Scm_SysError("mmap failed");
    return ptr;
#else  /*GAUCHE_WINDOWS*/
    HANDLE fhandle = INVALID_HANDLE_VALUE;
    if (fd >= 0) {
        fhandle = (HANDLE)_get_osfhandle(fd);
        if (fhandle == INVALID_HANDLE_VALUE) {
            Scm_Error("fd is not associated to a file: %d", fd);
        }
    }

    DWORD wprot = 0;
    if (prot & PROT_EXEC) {
        if (prot & PROT_WRITE) wprot = PAGE_EXECUTE_READWRITE;
        else wprot = PAGE_EXECUTE_READ;
    } else {
        if (prot & PROT_WRITE) wprot = PAGE_READWRITE;
        else wprot = PAGE_READONLY;
    }
    if (wprot == 0) {
        Scm_Error("PROT_NONE mmap protection isn't allowed on MinGW");
    }

    DWORD size_hi = len >> 32;
    DWORD size_lo = len & 0xffffffffULL;
    HANDLE mapping = CreateFileMappingA(fhandle, NULL, wprot,
                                        size_hi, size_lo, NULL);
    if (mapping == NULL) {
        Scm_SysError("CreateFileMapping failed");
    }

    DWORD off_hi = off >> 32;
    DWORD off_lo = off & 0xffffffffULL;
    DWORD accmode = 0;
    if (prot & PROT_WRITE) accmode = FILE_MAP_WRITE;
    else accmode = FILE_MAP_READ;
    if (prot & PROT_EXEC) accmode |= FILE_MAP_EXECUTE;
    LPVOID ptr = MapViewOfFileEx(mapping, accmode, off_hi, off_lo,
                                 0, addrhint);
    if (ptr == NULL) {
        DWORD errcode = GetLastError();
        (void)CloseHandle(mapping);
        SetLastError(errcode);
        Scm_SysError("MapViewOfFileEx failed");
    }

    *pmapping = mapping;
    return ptr;
#endif /*GAUCHE_WINDOWS*/
}

/* Check if PaX mprotect is active. */
#if !defined(GAUCHE_WINDOWS)
static int pax_active_p()
{
    static ScmAtomicVar result_cache = -1; /* -1: unknown */

    /* No MT hazard - operation is idempotent*/
    if (result_cache < 0) {
        size_t pagesize = sysconf(_SC_PAGESIZE);
        SCM_ASSERT(pagesize > 0);
        void *ptr = mmap(NULL, pagesize, PROT_WRITE,
                         MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
        SCM_ASSERT(ptr != NULL);
        int r = mprotect(ptr, pagesize, PROT_WRITE|PROT_EXEC);
        (void)munmap(ptr, pagesize);
        AO_store_full(&result_cache, (r < 0));
    }
    return result_cache;
}
#endif /*!GAUCHE_WINDOWS*/

/*
 * API
 */

/* Straight mmap() interface.   Scheme's sys-mmap calls this. */
ScmObj Scm_SysMmap(void *addrhint, int fd, size_t len, off_t off,
                   int prot, int flags)
{
#if !defined(GAUCHE_WINDOWS)
    void *ptr = mmap_int(addrhint, len, prot, flags, fd, off);
    return make_memory_region(ptr, len, prot, flags);
#else  /*GAUCHE_WINDOWS*/
    HANDLE mapping = INVALID_HANDLE_VALUE;
    void *ptr = mmap_int(addrhint, len, prot, flags, fd, off, &mapping);
    return make_memory_region(ptr, len, prot, flags, mapping);
#endif /*GAUCHE_WINDOWS*/
}

/* Mmap for runtime code generation.  Returns two ScmMemoryRegions
   of the same size, one for write and one for execute.  If the system
   allows a page being both writable and executable, two regions may
   be the same. */
void Scm_SysMmapWX(size_t len,
                   ScmMemoryRegion **writable,   /* ret */
                   ScmMemoryRegion **executable) /* ret */
{
#if !defined(GAUCHE_WINDOWS)
    if (!pax_active_p()) goto fallback;
    /* TODO */
    Scm_Error("System has Pax MPROTECT activated.  We haven't suppored it yet.");
#endif /*GAUCHE_WINDOWS*/
 fallback:
    {
        ScmMemoryRegion *m =
            SCM_MEMORY_REGION(Scm_SysMmap(NULL, -1, len, 0,
                                          PROT_READ|PROT_WRITE|PROT_EXEC,
                                          MAP_PRIVATE|MAP_ANONYMOUS));
        *writable = m;
        *executable = m;
    }
}
