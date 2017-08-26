/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1999-2004 Hewlett-Packard Development Company, L.P.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

#include "private/gc_priv.h"
#include "gc_inline.h" /* for GC_malloc_kind */

#include <stdio.h>
#include <string.h>

/* Allocate reclaim list for kind:      */
/* Return TRUE on success               */
STATIC GC_bool GC_alloc_reclaim_list(struct obj_kind *kind)
{
    struct hblk ** result = (struct hblk **)
                GC_scratch_alloc((MAXOBJGRANULES+1) * sizeof(struct hblk *));
    if (result == 0) return(FALSE);
    BZERO(result, (MAXOBJGRANULES+1)*sizeof(struct hblk *));
    kind -> ok_reclaim_list = result;
    return(TRUE);
}

GC_INNER GC_bool GC_collect_or_expand(word needed_blocks,
                                      GC_bool ignore_off_page,
                                      GC_bool retry); /* from alloc.c */

/* Allocate a large block of size lb bytes.     */
/* The block is not cleared.                    */
/* Flags is 0 or IGNORE_OFF_PAGE.               */
/* EXTRA_BYTES were already added to lb.        */
GC_INNER ptr_t GC_alloc_large(size_t lb, int k, unsigned flags)
{
    struct hblk * h;
    word n_blocks;
    ptr_t result;
    GC_bool retry = FALSE;

    GC_ASSERT(I_HOLD_LOCK());
    lb = ROUNDUP_GRANULE_SIZE(lb);
    n_blocks = OBJ_SZ_TO_BLOCKS(lb);
    if (!EXPECT(GC_is_initialized, TRUE)) {
      DCL_LOCK_STATE;
      UNLOCK(); /* just to unset GC_lock_holder */
      GC_init();
      LOCK();
    }
    /* Do our share of marking work */
        if (GC_incremental && !GC_dont_gc)
            GC_collect_a_little_inner((int)n_blocks);
    h = GC_allochblk(lb, k, flags);
#   ifdef USE_MUNMAP
        if (0 == h) {
            GC_merge_unmapped();
            h = GC_allochblk(lb, k, flags);
        }
#   endif
    while (0 == h && GC_collect_or_expand(n_blocks, flags != 0, retry)) {
        h = GC_allochblk(lb, k, flags);
        retry = TRUE;
    }
    if (h == 0) {
        result = 0;
    } else {
        size_t total_bytes = n_blocks * HBLKSIZE;
        if (n_blocks > 1) {
            GC_large_allocd_bytes += total_bytes;
            if (GC_large_allocd_bytes > GC_max_large_allocd_bytes)
                GC_max_large_allocd_bytes = GC_large_allocd_bytes;
        }
        /* FIXME: Do we need some way to reset GC_max_large_allocd_bytes? */
        result = h -> hb_body;
    }
    return result;
}

/* Allocate a large block of size lb bytes.  Clear if appropriate.      */
/* EXTRA_BYTES were already added to lb.                                */
STATIC ptr_t GC_alloc_large_and_clear(size_t lb, int k, unsigned flags)
{
    ptr_t result;
    word n_blocks = OBJ_SZ_TO_BLOCKS(lb);

    GC_ASSERT(I_HOLD_LOCK());
    result = GC_alloc_large(lb, k, flags);
    if (result != NULL
          && (GC_debugging_started || GC_obj_kinds[k].ok_init)) {
        /* Clear the whole block, in case of GC_realloc call. */
        BZERO(result, n_blocks * HBLKSIZE);
    }
    return result;
}

/* allocate lb bytes for an object of kind k.   */
/* Should not be used to directly to allocate   */
/* objects such as STUBBORN objects that        */
/* require special handling on allocation.      */
GC_INNER void * GC_generic_malloc_inner(size_t lb, int k)
{
    void *op;

    GC_ASSERT(I_HOLD_LOCK());
    GC_ASSERT(k < MAXOBJKINDS);
    if (SMALL_OBJ(lb)) {
        struct obj_kind * kind = GC_obj_kinds + k;
        size_t lg = GC_size_map[lb];
        void ** opp = &(kind -> ok_freelist[lg]);

        op = *opp;
        if (EXPECT(0 == op, FALSE)) {
          if (lg == 0) {
            if (!EXPECT(GC_is_initialized, TRUE)) {
              DCL_LOCK_STATE;
              UNLOCK(); /* just to unset GC_lock_holder */
              GC_init();
              LOCK();
              lg = GC_size_map[lb];
            }
            if (0 == lg) {
              GC_extend_size_map(lb);
              lg = GC_size_map[lb];
              GC_ASSERT(lg != 0);
            }
            /* Retry */
            opp = &(kind -> ok_freelist[lg]);
            op = *opp;
          }
          if (0 == op) {
            if (0 == kind -> ok_reclaim_list &&
                !GC_alloc_reclaim_list(kind))
              return NULL;
            op = GC_allocobj(lg, k);
            if (0 == op)
              return NULL;
          }
        }
        *opp = obj_link(op);
        obj_link(op) = 0;
        GC_bytes_allocd += GRANULES_TO_BYTES(lg);
    } else {
        op = (ptr_t)GC_alloc_large_and_clear(ADD_SLOP(lb), k, 0);
        GC_bytes_allocd += lb;
    }

    return op;
}

#if defined(DBG_HDRS_ALL) || defined(GC_GCJ_SUPPORT) \
    || !defined(GC_NO_FINALIZATION)
  /* Allocate a composite object of size n bytes.  The caller           */
  /* guarantees that pointers past the first page are not relevant.     */
  GC_INNER void * GC_generic_malloc_inner_ignore_off_page(size_t lb, int k)
  {
    word lb_adjusted;
    void * op;

    GC_ASSERT(I_HOLD_LOCK());
    if (lb <= HBLKSIZE)
        return GC_generic_malloc_inner(lb, k);
    GC_ASSERT(k < MAXOBJKINDS);
    lb_adjusted = ADD_SLOP(lb);
    op = GC_alloc_large_and_clear(lb_adjusted, k, IGNORE_OFF_PAGE);
    GC_bytes_allocd += lb_adjusted;
    return op;
  }
#endif

#ifdef GC_COLLECT_AT_MALLOC
  /* Parameter to force GC at every malloc of size greater or equal to  */
  /* the given value.  This might be handy during debugging.            */
  size_t GC_dbg_collect_at_malloc_min_lb = (GC_COLLECT_AT_MALLOC);
#endif

GC_API GC_ATTR_MALLOC void * GC_CALL GC_generic_malloc(size_t lb, int k)
{
    void * result;
    DCL_LOCK_STATE;

    GC_ASSERT(k < MAXOBJKINDS);
    if (EXPECT(GC_have_errors, FALSE))
      GC_print_all_errors();
    GC_INVOKE_FINALIZERS();
    GC_DBG_COLLECT_AT_MALLOC(lb);
    if (SMALL_OBJ(lb)) {
        LOCK();
        result = GC_generic_malloc_inner(lb, k);
        UNLOCK();
    } else {
        size_t lg;
        size_t lb_rounded;
        word n_blocks;
        GC_bool init;

        lg = ROUNDED_UP_GRANULES(lb);
        lb_rounded = GRANULES_TO_BYTES(lg);
        if (lb_rounded < lb)
            return((*GC_get_oom_fn())(lb));
        n_blocks = OBJ_SZ_TO_BLOCKS(lb_rounded);
        init = GC_obj_kinds[k].ok_init;
        LOCK();
        result = (ptr_t)GC_alloc_large(lb_rounded, k, 0);
        if (0 != result) {
          if (GC_debugging_started) {
            BZERO(result, n_blocks * HBLKSIZE);
          } else {
#           ifdef THREADS
              /* Clear any memory that might be used for GC descriptors */
              /* before we release the lock.                            */
                ((word *)result)[0] = 0;
                ((word *)result)[1] = 0;
                ((word *)result)[GRANULES_TO_WORDS(lg)-1] = 0;
                ((word *)result)[GRANULES_TO_WORDS(lg)-2] = 0;
#           endif
          }
        }
        GC_bytes_allocd += lb_rounded;
        UNLOCK();
        if (init && !GC_debugging_started && 0 != result) {
            BZERO(result, n_blocks * HBLKSIZE);
        }
    }
    if (0 == result) {
        return((*GC_get_oom_fn())(lb));
    } else {
        return(result);
    }
}

GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc_kind_global(size_t lb, int k)
{
    void *op;
    void **opp;
    size_t lg;
    DCL_LOCK_STATE;

    GC_ASSERT(k < MAXOBJKINDS);
    if (SMALL_OBJ(lb)) {
        GC_DBG_COLLECT_AT_MALLOC(lb);
        lg = GC_size_map[lb];
        LOCK();
        opp = &GC_obj_kinds[k].ok_freelist[lg];
        op = *opp;
        if (EXPECT(op != NULL, TRUE)) {
            if (k == PTRFREE) {
                *opp = obj_link(op);
            } else {
                GC_ASSERT(0 == obj_link(op)
                          || ((word)obj_link(op)
                                <= (word)GC_greatest_plausible_heap_addr
                              && (word)obj_link(op)
                                >= (word)GC_least_plausible_heap_addr));
                *opp = obj_link(op);
                obj_link(op) = 0;
            }
            GC_bytes_allocd += GRANULES_TO_BYTES(lg);
            UNLOCK();
            return op;
        }
        UNLOCK();
    }
    return GENERAL_MALLOC(lb, k);
}

#if defined(THREADS) && !defined(THREAD_LOCAL_ALLOC)
  GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc_kind(size_t lb, int k)
  {
    return GC_malloc_kind_global(lb, k);
  }
#endif

/* Allocate lb bytes of atomic (pointer-free) data.     */
GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc_atomic(size_t lb)
{
    return GC_malloc_kind(lb, PTRFREE);
}

/* Allocate lb bytes of composite (pointerful) data.    */
GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc(size_t lb)
{
    return GC_malloc_kind(lb, NORMAL);
}

GC_API GC_ATTR_MALLOC void * GC_CALL GC_generic_malloc_uncollectable(
                                                        size_t lb, int k)
{
    void *op;
    void **opp;
    size_t lg;
    DCL_LOCK_STATE;

    GC_ASSERT(k < MAXOBJKINDS);
    if (SMALL_OBJ(lb)) {
        GC_DBG_COLLECT_AT_MALLOC(lb);
        if (EXTRA_BYTES != 0 && lb != 0) lb--;
                  /* We don't need the extra byte, since this won't be  */
                  /* collected anyway.                                  */
        lg = GC_size_map[lb];
        LOCK();
        opp = &GC_obj_kinds[k].ok_freelist[lg];
        op = *opp;
        if (EXPECT(op != NULL, TRUE)) {
            *opp = obj_link(op);
            obj_link(op) = 0;
            GC_bytes_allocd += GRANULES_TO_BYTES(lg);
            /* Mark bit was already set on free list.  It will be       */
            /* cleared only temporarily during a collection, as a       */
            /* result of the normal free list mark bit clearing.        */
            GC_non_gc_bytes += GRANULES_TO_BYTES(lg);
            UNLOCK();
        } else {
            UNLOCK();
            op = GC_generic_malloc(lb, k);
            /* For small objects, the free lists are completely marked. */
        }
        GC_ASSERT(0 == op || GC_is_marked(op));
    } else {
        hdr * hhdr;

        op = GC_generic_malloc(lb, k);
        if (NULL == op)
            return NULL;

        GC_ASSERT(((word)op & (HBLKSIZE - 1)) == 0); /* large block */
        hhdr = HDR(op);
        /* We don't need the lock here, since we have an undisguised    */
        /* pointer.  We do need to hold the lock while we adjust        */
        /* mark bits.                                                   */
        LOCK();
        set_mark_bit_from_hdr(hhdr, 0); /* Only object. */
#       ifndef THREADS
          GC_ASSERT(hhdr -> hb_n_marks == 0);
                /* This is not guaranteed in the multi-threaded case    */
                /* because the counter could be updated before locking. */
#       endif
        hhdr -> hb_n_marks = 1;
        UNLOCK();
    }
    return op;
}

/* Allocate lb bytes of pointerful, traced, but not collectible data.   */
GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc_uncollectable(size_t lb)
{
  return GC_generic_malloc_uncollectable(lb, UNCOLLECTABLE);
}

#ifdef GC_ATOMIC_UNCOLLECTABLE
  /* Allocate lb bytes of pointer-free, untraced, uncollectible data    */
  /* This is normally roughly equivalent to the system malloc.          */
  /* But it may be useful if malloc is redefined.                       */
  GC_API GC_ATTR_MALLOC void * GC_CALL
        GC_malloc_atomic_uncollectable(size_t lb)
  {
    return GC_generic_malloc_uncollectable(lb, AUNCOLLECTABLE);
  }
#endif /* GC_ATOMIC_UNCOLLECTABLE */

#if defined(REDIRECT_MALLOC) && !defined(REDIRECT_MALLOC_IN_HEADER)

# ifndef MSWINCE
#  include <errno.h>
# endif

  /* Avoid unnecessary nested procedure calls here, by #defining some   */
  /* malloc replacements.  Otherwise we end up saving a meaningless     */
  /* return address in the object.  It also speeds things up, but it is */
  /* admittedly quite ugly.                                             */
# define GC_debug_malloc_replacement(lb) GC_debug_malloc(lb, GC_DBG_EXTRAS)

  void * malloc(size_t lb)
  {
    /* It might help to manually inline the GC_malloc call here.        */
    /* But any decent compiler should reduce the extra procedure call   */
    /* to at most a jump instruction in this case.                      */
#   if defined(I386) && defined(GC_SOLARIS_THREADS)
      /* Thread initialization can call malloc before we are ready for. */
      /* It is not clear that this is enough to help matters.           */
      /* The thread implementation may well call malloc at other        */
      /* inopportune times.                                             */
      if (!EXPECT(GC_is_initialized, TRUE)) return sbrk(lb);
#   endif
    return (void *)REDIRECT_MALLOC(lb);
  }

# if defined(GC_LINUX_THREADS)
    STATIC ptr_t GC_libpthread_start = 0;
    STATIC ptr_t GC_libpthread_end = 0;
    STATIC ptr_t GC_libld_start = 0;
    STATIC ptr_t GC_libld_end = 0;

    STATIC void GC_init_lib_bounds(void)
    {
      if (GC_libpthread_start != 0) return;
      GC_init(); /* if not called yet */
      if (!GC_text_mapping("libpthread-",
                           &GC_libpthread_start, &GC_libpthread_end)) {
          WARN("Failed to find libpthread.so text mapping: Expect crash\n", 0);
          /* This might still work with some versions of libpthread,      */
          /* so we don't abort.  Perhaps we should.                       */
          /* Generate message only once:                                  */
            GC_libpthread_start = (ptr_t)1;
      }
      if (!GC_text_mapping("ld-", &GC_libld_start, &GC_libld_end)) {
          WARN("Failed to find ld.so text mapping: Expect crash\n", 0);
      }
    }
# endif /* GC_LINUX_THREADS */

# include <limits.h>
# ifdef SIZE_MAX
#   define GC_SIZE_MAX SIZE_MAX
# else
#   define GC_SIZE_MAX (~(size_t)0)
# endif

# define GC_SQRT_SIZE_MAX ((1U << (WORDSZ / 2)) - 1)

  void * calloc(size_t n, size_t lb)
  {
    if ((lb | n) > GC_SQRT_SIZE_MAX /* fast initial test */
        && lb && n > GC_SIZE_MAX / lb)
      return NULL;
#   if defined(GC_LINUX_THREADS)
      /* libpthread allocated some memory that is only pointed to by    */
      /* mmapped thread stacks.  Make sure it is not collectible.       */
      {
        static GC_bool lib_bounds_set = FALSE;
        ptr_t caller = (ptr_t)__builtin_return_address(0);
        /* This test does not need to ensure memory visibility, since   */
        /* the bounds will be set when/if we create another thread.     */
        if (!EXPECT(lib_bounds_set, TRUE)) {
          GC_init_lib_bounds();
          lib_bounds_set = TRUE;
        }
        if (((word)caller >= (word)GC_libpthread_start
             && (word)caller < (word)GC_libpthread_end)
            || ((word)caller >= (word)GC_libld_start
                && (word)caller < (word)GC_libld_end))
          return GC_generic_malloc_uncollectable(n * lb, UNCOLLECTABLE);
        /* The two ranges are actually usually adjacent, so there may   */
        /* be a way to speed this up.                                   */
      }
#   endif
    return (void *)REDIRECT_MALLOC(n * lb);
  }

# ifndef strdup
    char *strdup(const char *s)
    {
      size_t lb = strlen(s) + 1;
      char *result = (char *)REDIRECT_MALLOC(lb);
      if (result == 0) {
        errno = ENOMEM;
        return 0;
      }
      BCOPY(s, result, lb);
      return result;
    }
# endif /* !defined(strdup) */
 /* If strdup is macro defined, we assume that it actually calls malloc, */
 /* and thus the right thing will happen even without overriding it.     */
 /* This seems to be true on most Linux systems.                         */

# ifndef strndup
    /* This is similar to strdup().     */
    char *strndup(const char *str, size_t size)
    {
      char *copy;
      size_t len = strlen(str);
      if (len > size)
        len = size;
      copy = (char *)REDIRECT_MALLOC(len + 1);
      if (copy == NULL) {
        errno = ENOMEM;
        return NULL;
      }
      BCOPY(str, copy, len);
      copy[len] = '\0';
      return copy;
    }
# endif /* !strndup */

# undef GC_debug_malloc_replacement

#endif /* REDIRECT_MALLOC */

/* Explicitly deallocate an object p.                           */
GC_API void GC_CALL GC_free(void * p)
{
    struct hblk *h;
    hdr *hhdr;
    size_t sz; /* In bytes */
    size_t ngranules;   /* sz in granules */
    void **flh;
    int knd;
    struct obj_kind * ok;
    DCL_LOCK_STATE;

    if (p == 0) return;
        /* Required by ANSI.  It's not my fault ...     */
#   ifdef LOG_ALLOCS
      GC_log_printf("GC_free(%p) after GC #%lu\n",
                    p, (unsigned long)GC_gc_no);
#   endif
    h = HBLKPTR(p);
    hhdr = HDR(h);
#   if defined(REDIRECT_MALLOC) && \
        (defined(GC_SOLARIS_THREADS) || defined(GC_LINUX_THREADS) \
         || defined(MSWIN32))
        /* For Solaris, we have to redirect malloc calls during         */
        /* initialization.  For the others, this seems to happen        */
        /* implicitly.                                                  */
        /* Don't try to deallocate that memory.                         */
        if (0 == hhdr) return;
#   endif
    GC_ASSERT(GC_base(p) == p);
    sz = hhdr -> hb_sz;
    ngranules = BYTES_TO_GRANULES(sz);
    knd = hhdr -> hb_obj_kind;
    ok = &GC_obj_kinds[knd];
    if (EXPECT(ngranules <= MAXOBJGRANULES, TRUE)) {
        LOCK();
        GC_bytes_freed += sz;
        if (IS_UNCOLLECTABLE(knd)) GC_non_gc_bytes -= sz;
                /* Its unnecessary to clear the mark bit.  If the       */
                /* object is reallocated, it doesn't matter.  O.w. the  */
                /* collector will do it, since it's on a free list.     */
        if (ok -> ok_init) {
            BZERO((word *)p + 1, sz-sizeof(word));
        }
        flh = &(ok -> ok_freelist[ngranules]);
        obj_link(p) = *flh;
        *flh = (ptr_t)p;
        UNLOCK();
    } else {
        size_t nblocks = OBJ_SZ_TO_BLOCKS(sz);
        LOCK();
        GC_bytes_freed += sz;
        if (IS_UNCOLLECTABLE(knd)) GC_non_gc_bytes -= sz;
        if (nblocks > 1) {
          GC_large_allocd_bytes -= nblocks * HBLKSIZE;
        }
        GC_freehblk(h);
        UNLOCK();
    }
}

/* Explicitly deallocate an object p when we already hold lock.         */
/* Only used for internally allocated objects, so we can take some      */
/* shortcuts.                                                           */
#ifdef THREADS
  GC_INNER void GC_free_inner(void * p)
  {
    struct hblk *h;
    hdr *hhdr;
    size_t sz; /* bytes */
    size_t ngranules;  /* sz in granules */
    void ** flh;
    int knd;
    struct obj_kind * ok;

    h = HBLKPTR(p);
    hhdr = HDR(h);
    knd = hhdr -> hb_obj_kind;
    sz = hhdr -> hb_sz;
    ngranules = BYTES_TO_GRANULES(sz);
    ok = &GC_obj_kinds[knd];
    if (ngranules <= MAXOBJGRANULES) {
        GC_bytes_freed += sz;
        if (IS_UNCOLLECTABLE(knd)) GC_non_gc_bytes -= sz;
        if (ok -> ok_init) {
            BZERO((word *)p + 1, sz-sizeof(word));
        }
        flh = &(ok -> ok_freelist[ngranules]);
        obj_link(p) = *flh;
        *flh = (ptr_t)p;
    } else {
        size_t nblocks = OBJ_SZ_TO_BLOCKS(sz);
        GC_bytes_freed += sz;
        if (IS_UNCOLLECTABLE(knd)) GC_non_gc_bytes -= sz;
        if (nblocks > 1) {
          GC_large_allocd_bytes -= nblocks * HBLKSIZE;
        }
        GC_freehblk(h);
    }
  }
#endif /* THREADS */

#if defined(REDIRECT_MALLOC) && !defined(REDIRECT_FREE)
# define REDIRECT_FREE GC_free
#endif

#if defined(REDIRECT_FREE) && !defined(REDIRECT_MALLOC_IN_HEADER)
  void free(void * p)
  {
#   ifndef IGNORE_FREE
#     if defined(GC_LINUX_THREADS) && !defined(USE_PROC_FOR_LIBRARIES)
        /* Don't bother with initialization checks.  If nothing         */
        /* has been initialized, the check fails, and that's safe,      */
        /* since we have not allocated uncollectible objects neither.   */
        ptr_t caller = (ptr_t)__builtin_return_address(0);
        /* This test does not need to ensure memory visibility, since   */
        /* the bounds will be set when/if we create another thread.     */
        if (((word)caller >= (word)GC_libpthread_start
             && (word)caller < (word)GC_libpthread_end)
            || ((word)caller >= (word)GC_libld_start
                && (word)caller < (word)GC_libld_end)) {
          GC_free(p);
          return;
        }
#     endif
      REDIRECT_FREE(p);
#   endif
  }
#endif /* REDIRECT_FREE */
