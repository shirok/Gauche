/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1995 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 2000 by Hewlett-Packard Company.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 */

#include "private/gc_pmark.h"

#include <stdio.h>

#if defined(MSWIN32) && defined(__GNUC__)
# include <excpt.h>
#endif

/* Make arguments appear live to compiler.  Put here to minimize the    */
/* risk of inlining.  Used to minimize junk left in registers.          */
void GC_noop6(word arg1 GC_ATTR_UNUSED, word arg2 GC_ATTR_UNUSED,
              word arg3 GC_ATTR_UNUSED, word arg4 GC_ATTR_UNUSED,
              word arg5 GC_ATTR_UNUSED, word arg6 GC_ATTR_UNUSED)
{
  /* Empty */
}

/* Single argument version, robust against whole program analysis. */
volatile word GC_noop_sink;
GC_API void GC_CALL GC_noop1(word x)
{
    GC_noop_sink = x;
}

/* mark_proc GC_mark_procs[MAX_MARK_PROCS] = {0} -- declared in gc_priv.h */

GC_INNER unsigned GC_n_mark_procs = GC_RESERVED_MARK_PROCS;

/* Initialize GC_obj_kinds properly and standard free lists properly.   */
/* This must be done statically since they may be accessed before       */
/* GC_init is called.                                                   */
/* It's done here, since we need to deal with mark descriptors.         */
GC_INNER struct obj_kind GC_obj_kinds[MAXOBJKINDS] = {
/* PTRFREE */ { &GC_aobjfreelist[0], 0 /* filled in dynamically */,
                /* 0 | */ GC_DS_LENGTH, FALSE, FALSE
                /*, */ OK_DISCLAIM_INITZ },
/* NORMAL */  { &GC_objfreelist[0], 0,
                /* 0 | */ GC_DS_LENGTH,
                                /* adjusted in GC_init for EXTRA_BYTES  */
                TRUE /* add length to descr */, TRUE
                /*, */ OK_DISCLAIM_INITZ },
/* UNCOLLECTABLE */
              { &GC_uobjfreelist[0], 0,
                /* 0 | */ GC_DS_LENGTH, TRUE /* add length to descr */, TRUE
                /*, */ OK_DISCLAIM_INITZ },
# ifdef GC_ATOMIC_UNCOLLECTABLE
              { &GC_auobjfreelist[0], 0,
                /* 0 | */ GC_DS_LENGTH, FALSE /* add length to descr */, FALSE
                /*, */ OK_DISCLAIM_INITZ },
# endif
# ifdef STUBBORN_ALLOC
              { (void **)&GC_sobjfreelist[0], 0,
                /* 0 | */ GC_DS_LENGTH, TRUE /* add length to descr */, TRUE
                /*, */ OK_DISCLAIM_INITZ },
# endif
};

# ifdef STUBBORN_ALLOC
#   define GC_N_KINDS_INITIAL_VALUE (STUBBORN+1)
# else
#   define GC_N_KINDS_INITIAL_VALUE STUBBORN
# endif

GC_INNER unsigned GC_n_kinds = GC_N_KINDS_INITIAL_VALUE;

# ifndef INITIAL_MARK_STACK_SIZE
#   define INITIAL_MARK_STACK_SIZE (1*HBLKSIZE)
                /* INITIAL_MARK_STACK_SIZE * sizeof(mse) should be a    */
                /* multiple of HBLKSIZE.                                */
                /* The incremental collector actually likes a larger    */
                /* size, since it wants to push all marked dirty        */
                /* objects before marking anything new.  Currently we   */
                /* let it grow dynamically.                             */
# endif

STATIC word GC_n_rescuing_pages = 0;
                                /* Number of dirty pages we marked from */
                                /* excludes ptrfree pages, etc.         */

GC_INNER size_t GC_mark_stack_size = 0;

#ifdef PARALLEL_MARK
  STATIC volatile AO_t GC_first_nonempty = 0;
        /* Lowest entry on mark stack   */
        /* that may be nonempty.        */
        /* Updated only by initiating   */
        /* thread.                      */
#endif

GC_INNER mark_state_t GC_mark_state = MS_NONE;

GC_INNER GC_bool GC_mark_stack_too_small = FALSE;

static struct hblk * scan_ptr;

STATIC GC_bool GC_objects_are_marked = FALSE;
                /* Are there collectible marked objects in the heap?    */

/* Is a collection in progress?  Note that this can return true in the  */
/* nonincremental case, if a collection has been abandoned and the      */
/* mark state is now MS_INVALID.                                        */
GC_INNER GC_bool GC_collection_in_progress(void)
{
    return(GC_mark_state != MS_NONE);
}

/* clear all mark bits in the header */
GC_INNER void GC_clear_hdr_marks(hdr *hhdr)
{
    size_t last_bit = FINAL_MARK_BIT(hhdr -> hb_sz);
    BZERO(hhdr -> hb_marks, sizeof(hhdr->hb_marks));
    set_mark_bit_from_hdr(hhdr, last_bit);
    hhdr -> hb_n_marks = 0;
}

/* Set all mark bits in the header.  Used for uncollectible blocks. */
GC_INNER void GC_set_hdr_marks(hdr *hhdr)
{
    unsigned i;
    size_t sz = hhdr -> hb_sz;
    unsigned n_marks = (unsigned)FINAL_MARK_BIT(sz);

#   ifdef USE_MARK_BYTES
      for (i = 0; i <= n_marks; i += (unsigned)MARK_BIT_OFFSET(sz)) {
        hhdr -> hb_marks[i] = 1;
      }
#   else
      for (i = 0; i < divWORDSZ(n_marks + WORDSZ); ++i) {
        hhdr -> hb_marks[i] = ONES;
      }
#   endif
#   ifdef MARK_BIT_PER_OBJ
      hhdr -> hb_n_marks = n_marks - 1;
#   else
      hhdr -> hb_n_marks = HBLK_OBJS(sz);
#   endif
}

/*
 * Clear all mark bits associated with block h.
 */
static void clear_marks_for_block(struct hblk *h, word dummy GC_ATTR_UNUSED)
{
    register hdr * hhdr = HDR(h);

    if (IS_UNCOLLECTABLE(hhdr -> hb_obj_kind)) return;
        /* Mark bit for these is cleared only once the object is        */
        /* explicitly deallocated.  This either frees the block, or     */
        /* the bit is cleared once the object is on the free list.      */
    GC_clear_hdr_marks(hhdr);
}

/* Slow but general routines for setting/clearing/asking about mark bits */
GC_API void GC_CALL GC_set_mark_bit(const void *p)
{
    struct hblk *h = HBLKPTR(p);
    hdr * hhdr = HDR(h);
    word bit_no = MARK_BIT_NO((ptr_t)p - (ptr_t)h, hhdr -> hb_sz);

    if (!mark_bit_from_hdr(hhdr, bit_no)) {
      set_mark_bit_from_hdr(hhdr, bit_no);
      ++hhdr -> hb_n_marks;
    }
}

GC_API void GC_CALL GC_clear_mark_bit(const void *p)
{
    struct hblk *h = HBLKPTR(p);
    hdr * hhdr = HDR(h);
    word bit_no = MARK_BIT_NO((ptr_t)p - (ptr_t)h, hhdr -> hb_sz);

    if (mark_bit_from_hdr(hhdr, bit_no)) {
      size_t n_marks;
      clear_mark_bit_from_hdr(hhdr, bit_no);
      n_marks = hhdr -> hb_n_marks - 1;
#     ifdef PARALLEL_MARK
        if (n_marks != 0 || !GC_parallel)
          hhdr -> hb_n_marks = n_marks;
        /* Don't decrement to zero.  The counts are approximate due to  */
        /* concurrency issues, but we need to ensure that a count of    */
        /* zero implies an empty block.                                 */
#     else
          hhdr -> hb_n_marks = n_marks;
#     endif
    }
}

GC_API int GC_CALL GC_is_marked(const void *p)
{
    struct hblk *h = HBLKPTR(p);
    hdr * hhdr = HDR(h);
    word bit_no = MARK_BIT_NO((ptr_t)p - (ptr_t)h, hhdr -> hb_sz);

    return (int)mark_bit_from_hdr(hhdr, bit_no); /* 0 or 1 */
}

/*
 * Clear mark bits in all allocated heap blocks.  This invalidates
 * the marker invariant, and sets GC_mark_state to reflect this.
 * (This implicitly starts marking to reestablish the invariant.)
 */
GC_INNER void GC_clear_marks(void)
{
    GC_apply_to_all_blocks(clear_marks_for_block, (word)0);
    GC_objects_are_marked = FALSE;
    GC_mark_state = MS_INVALID;
    scan_ptr = 0;
}

#ifdef CHECKSUMS
  void GC_check_dirty(void);
#endif

/* Initiate a garbage collection.  Initiates a full collection if the   */
/* mark state is invalid.                                               */
GC_INNER void GC_initiate_gc(void)
{
#   ifndef GC_DISABLE_INCREMENTAL
        if (GC_dirty_maintained) GC_read_dirty();
#   endif
#   ifdef STUBBORN_ALLOC
        GC_read_changed();
#   endif
#   ifdef CHECKSUMS
        if (GC_dirty_maintained) GC_check_dirty();
#   endif
    GC_n_rescuing_pages = 0;
    if (GC_mark_state == MS_NONE) {
        GC_mark_state = MS_PUSH_RESCUERS;
    } else if (GC_mark_state != MS_INVALID) {
        ABORT("Unexpected state");
    } /* else this is really a full collection, and mark        */
      /* bits are invalid.                                      */
    scan_ptr = 0;
}

#ifdef PARALLEL_MARK
    STATIC void GC_do_parallel_mark(void); /* initiate parallel marking. */
#endif /* PARALLEL_MARK */

#ifdef GC_DISABLE_INCREMENTAL
# define GC_push_next_marked_dirty(h) GC_push_next_marked(h)
#else
  STATIC struct hblk * GC_push_next_marked_dirty(struct hblk *h);
                /* Invoke GC_push_marked on next dirty block above h.   */
                /* Return a pointer just past the end of this block.    */
#endif /* !GC_DISABLE_INCREMENTAL */
STATIC struct hblk * GC_push_next_marked(struct hblk *h);
                /* Ditto, but also mark from clean pages.       */
STATIC struct hblk * GC_push_next_marked_uncollectable(struct hblk *h);
                /* Ditto, but mark only from uncollectible pages.       */

static void alloc_mark_stack(size_t);

# if (((defined(MSWIN32) || defined(MSWINCE)) && !defined(__GNUC__)) \
        || (defined(MSWIN32) && defined(I386)) /* for Win98 */ \
        || (defined(USE_PROC_FOR_LIBRARIES) && defined(THREADS))) \
     && !defined(NO_WRAP_MARK_SOME)
    /* Under rare conditions, we may end up marking from nonexistent memory. */
    /* Hence we need to be prepared to recover by running GC_mark_some       */
    /* with a suitable handler in place.                                     */
    /* FIXME: Should we really need it for WinCE?  If yes then          */
    /* WRAP_MARK_SOME should be also defined for CeGCC which requires   */
    /* CPU/OS-specific code in mark_ex_handler() and GC_mark_some()     */
    /* (for manual stack unwinding and exception handler installation). */
#   define WRAP_MARK_SOME
# endif

/* Perform a small amount of marking.                   */
/* We try to touch roughly a page of memory.            */
/* Return TRUE if we just finished a mark phase.        */
/* Cold_gc_frame is an address inside a GC frame that   */
/* remains valid until all marking is complete.         */
/* A zero value indicates that it's OK to miss some     */
/* register values.                                     */
/* We hold the allocation lock.  In the case of         */
/* incremental collection, the world may not be stopped.*/
#ifdef WRAP_MARK_SOME
  /* For win32, this is called after we establish a structured  */
  /* exception handler, in case Windows unmaps one of our root  */
  /* segments.  See below.  In either case, we acquire the      */
  /* allocator lock long before we get here.                    */
  STATIC GC_bool GC_mark_some_inner(ptr_t cold_gc_frame)
#else
  GC_INNER GC_bool GC_mark_some(ptr_t cold_gc_frame)
#endif
{
    switch(GC_mark_state) {
        case MS_NONE:
            break;

        case MS_PUSH_RESCUERS:
            if ((word)GC_mark_stack_top
                >= (word)(GC_mark_stack_limit - INITIAL_MARK_STACK_SIZE/2)) {
                /* Go ahead and mark, even though that might cause us to */
                /* see more marked dirty objects later on.  Avoid this   */
                /* in the future.                                        */
                GC_mark_stack_too_small = TRUE;
                MARK_FROM_MARK_STACK();
                break;
            } else {
                scan_ptr = GC_push_next_marked_dirty(scan_ptr);
                if (scan_ptr == 0) {
                    GC_COND_LOG_PRINTF("Marked from %lu dirty pages\n",
                                       (unsigned long)GC_n_rescuing_pages);
                    GC_push_roots(FALSE, cold_gc_frame);
                    GC_objects_are_marked = TRUE;
                    if (GC_mark_state != MS_INVALID) {
                        GC_mark_state = MS_ROOTS_PUSHED;
                    }
                }
            }
            break;

        case MS_PUSH_UNCOLLECTABLE:
            if ((word)GC_mark_stack_top
                >= (word)(GC_mark_stack + GC_mark_stack_size/4)) {
#               ifdef PARALLEL_MARK
                  /* Avoid this, since we don't parallelize the marker  */
                  /* here.                                              */
                  if (GC_parallel) GC_mark_stack_too_small = TRUE;
#               endif
                MARK_FROM_MARK_STACK();
                break;
            } else {
                scan_ptr = GC_push_next_marked_uncollectable(scan_ptr);
                if (scan_ptr == 0) {
                    GC_push_roots(TRUE, cold_gc_frame);
                    GC_objects_are_marked = TRUE;
                    if (GC_mark_state != MS_INVALID) {
                        GC_mark_state = MS_ROOTS_PUSHED;
                    }
                }
            }
            break;

        case MS_ROOTS_PUSHED:
#           ifdef PARALLEL_MARK
              /* In the incremental GC case, this currently doesn't     */
              /* quite do the right thing, since it runs to             */
              /* completion.  On the other hand, starting a             */
              /* parallel marker is expensive, so perhaps it is         */
              /* the right thing?                                       */
              /* Eventually, incremental marking should run             */
              /* asynchronously in multiple threads, without grabbing   */
              /* the allocation lock.                                   */
                if (GC_parallel) {
                  GC_do_parallel_mark();
                  GC_ASSERT((word)GC_mark_stack_top < (word)GC_first_nonempty);
                  GC_mark_stack_top = GC_mark_stack - 1;
                  if (GC_mark_stack_too_small) {
                    alloc_mark_stack(2*GC_mark_stack_size);
                  }
                  if (GC_mark_state == MS_ROOTS_PUSHED) {
                    GC_mark_state = MS_NONE;
                    return(TRUE);
                  }
                  break;
                }
#           endif
            if ((word)GC_mark_stack_top >= (word)GC_mark_stack) {
                MARK_FROM_MARK_STACK();
                break;
            } else {
                GC_mark_state = MS_NONE;
                if (GC_mark_stack_too_small) {
                    alloc_mark_stack(2*GC_mark_stack_size);
                }
                return(TRUE);
            }

        case MS_INVALID:
        case MS_PARTIALLY_INVALID:
            if (!GC_objects_are_marked) {
                GC_mark_state = MS_PUSH_UNCOLLECTABLE;
                break;
            }
            if ((word)GC_mark_stack_top >= (word)GC_mark_stack) {
                MARK_FROM_MARK_STACK();
                break;
            }
            if (scan_ptr == 0 && GC_mark_state == MS_INVALID) {
                /* About to start a heap scan for marked objects. */
                /* Mark stack is empty.  OK to reallocate.        */
                if (GC_mark_stack_too_small) {
                    alloc_mark_stack(2*GC_mark_stack_size);
                }
                GC_mark_state = MS_PARTIALLY_INVALID;
            }
            scan_ptr = GC_push_next_marked(scan_ptr);
            if (scan_ptr == 0 && GC_mark_state == MS_PARTIALLY_INVALID) {
                GC_push_roots(TRUE, cold_gc_frame);
                GC_objects_are_marked = TRUE;
                if (GC_mark_state != MS_INVALID) {
                    GC_mark_state = MS_ROOTS_PUSHED;
                }
            }
            break;

        default:
            ABORT("GC_mark_some: bad state");
    }
    return(FALSE);
}

#ifdef WRAP_MARK_SOME

# if (defined(MSWIN32) || defined(MSWINCE)) && defined(__GNUC__)

    typedef struct {
      EXCEPTION_REGISTRATION ex_reg;
      void *alt_path;
    } ext_ex_regn;

    static EXCEPTION_DISPOSITION mark_ex_handler(
        struct _EXCEPTION_RECORD *ex_rec,
        void *est_frame,
        struct _CONTEXT *context,
        void *disp_ctxt GC_ATTR_UNUSED)
    {
        if (ex_rec->ExceptionCode == STATUS_ACCESS_VIOLATION) {
          ext_ex_regn *xer = (ext_ex_regn *)est_frame;

          /* Unwind from the inner function assuming the standard */
          /* function prologue.                                   */
          /* Assumes code has not been compiled with              */
          /* -fomit-frame-pointer.                                */
          context->Esp = context->Ebp;
          context->Ebp = *((DWORD *)context->Esp);
          context->Esp = context->Esp - 8;

          /* Resume execution at the "real" handler within the    */
          /* wrapper function.                                    */
          context->Eip = (DWORD )(xer->alt_path);

          return ExceptionContinueExecution;

        } else {
            return ExceptionContinueSearch;
        }
    }
# endif /* __GNUC__  && MSWIN32 */

#if defined(GC_WIN32_THREADS) && !defined(__GNUC__)
  GC_INNER GC_bool GC_started_thread_while_stopped(void);
  /* In win32_threads.c.  Did we invalidate mark phase with an  */
  /* unexpected thread start?                                   */
#endif

  GC_INNER GC_bool GC_mark_some(ptr_t cold_gc_frame)
  {
      GC_bool ret_val;

#   if defined(MSWIN32) || defined(MSWINCE)
#    ifndef __GNUC__
      /* Windows 98 appears to asynchronously create and remove  */
      /* writable memory mappings, for reasons we haven't yet    */
      /* understood.  Since we look for writable regions to      */
      /* determine the root set, we may try to mark from an      */
      /* address range that disappeared since we started the     */
      /* collection.  Thus we have to recover from faults here.  */
      /* This code does not appear to be necessary for Windows   */
      /* 95/NT/2000+. Note that this code should never generate  */
      /* an incremental GC write fault.                          */
      /* This code seems to be necessary for WinCE (at least in  */
      /* the case we'd decide to add MEM_PRIVATE sections to     */
      /* data roots in GC_register_dynamic_libraries()).         */
      /* It's conceivable that this is the same issue with       */
      /* terminating threads that we see with Linux and          */
      /* USE_PROC_FOR_LIBRARIES.                                 */

      __try {
          ret_val = GC_mark_some_inner(cold_gc_frame);
      } __except (GetExceptionCode() == EXCEPTION_ACCESS_VIOLATION ?
                EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH) {
          goto handle_ex;
      }
#     ifdef GC_WIN32_THREADS
        /* With DllMain-based thread tracking, a thread may have        */
        /* started while we were marking.  This is logically equivalent */
        /* to the exception case; our results are invalid and we have   */
        /* to start over.  This cannot be prevented since we can't      */
        /* block in DllMain.                                            */
        if (GC_started_thread_while_stopped()) goto handle_ex;
#     endif
     rm_handler:
      return ret_val;

#    else /* __GNUC__ */

      /* Manually install an exception handler since GCC does    */
      /* not yet support Structured Exception Handling (SEH) on  */
      /* Win32.                                                  */

      ext_ex_regn er;

      er.alt_path = &&handle_ex;
      er.ex_reg.handler = mark_ex_handler;
      __asm__ __volatile__ ("movl %%fs:0, %0" : "=r" (er.ex_reg.prev));
      __asm__ __volatile__ ("movl %0, %%fs:0" : : "r" (&er));
      ret_val = GC_mark_some_inner(cold_gc_frame);
      /* Prevent GCC from considering the following code unreachable */
      /* and thus eliminating it.                                    */
        if (er.alt_path == 0)
          goto handle_ex;
    rm_handler:
      /* Uninstall the exception handler */
      __asm__ __volatile__ ("mov %0, %%fs:0" : : "r" (er.ex_reg.prev));
      return ret_val;

#    endif /* __GNUC__ */
#   else /* !MSWIN32 */
      /* Here we are handling the case in which /proc is used for root  */
      /* finding, and we have threads.  We may find a stack for a       */
      /* thread that is in the process of exiting, and disappears       */
      /* while we are marking it.  This seems extremely difficult to    */
      /* avoid otherwise.                                               */
      if (GC_incremental) {
        WARN("Incremental GC incompatible with /proc roots\n", 0);
        /* I'm not sure if this could still work ...    */
      }
      GC_setup_temporary_fault_handler();
      if(SETJMP(GC_jmp_buf) != 0) goto handle_ex;
      ret_val = GC_mark_some_inner(cold_gc_frame);
    rm_handler:
      GC_reset_fault_handler();
      return ret_val;

#   endif /* !MSWIN32 */

handle_ex:
    /* Exception handler starts here for all cases. */
      WARN("Caught ACCESS_VIOLATION in marker;"
           " memory mapping disappeared\n", 0);

      /* We have bad roots on the stack.  Discard mark stack.   */
      /* Rescan from marked objects.  Redetermine roots.        */
      GC_invalidate_mark_state();
      scan_ptr = 0;

      ret_val = FALSE;
      goto rm_handler;  /* Back to platform-specific code. */
  }
#endif /* WRAP_MARK_SOME */

GC_INNER void GC_invalidate_mark_state(void)
{
    GC_mark_state = MS_INVALID;
    GC_mark_stack_top = GC_mark_stack-1;
}

GC_INNER mse * GC_signal_mark_stack_overflow(mse *msp)
{
    GC_mark_state = MS_INVALID;
#   ifdef PARALLEL_MARK
      /* We are using a local_mark_stack in parallel mode, so   */
      /* do not signal the global mark stack to be resized.     */
      /* That will be done if required in GC_return_mark_stack. */
      if (!GC_parallel)
        GC_mark_stack_too_small = TRUE;
#   else
      GC_mark_stack_too_small = TRUE;
#   endif
    GC_COND_LOG_PRINTF("Mark stack overflow; current size = %lu entries\n",
                       (unsigned long)GC_mark_stack_size);
    return(msp - GC_MARK_STACK_DISCARDS);
}

/*
 * Mark objects pointed to by the regions described by
 * mark stack entries between mark_stack and mark_stack_top,
 * inclusive.  Assumes the upper limit of a mark stack entry
 * is never 0.  A mark stack entry never has size 0.
 * We try to traverse on the order of a hblk of memory before we return.
 * Caller is responsible for calling this until the mark stack is empty.
 * Note that this is the most performance critical routine in the
 * collector.  Hence it contains all sorts of ugly hacks to speed
 * things up.  In particular, we avoid procedure calls on the common
 * path, we take advantage of peculiarities of the mark descriptor
 * encoding, we optionally maintain a cache for the block address to
 * header mapping, we prefetch when an object is "grayed", etc.
 */
GC_INNER mse * GC_mark_from(mse *mark_stack_top, mse *mark_stack,
                            mse *mark_stack_limit)
{
  signed_word credit = HBLKSIZE;  /* Remaining credit for marking work  */
  ptr_t current_p;      /* Pointer to current candidate ptr.            */
  word current;         /* Candidate pointer.                           */
  ptr_t limit = 0;      /* (Incl) limit of current candidate range.     */
  word descr;
  ptr_t greatest_ha = GC_greatest_plausible_heap_addr;
  ptr_t least_ha = GC_least_plausible_heap_addr;
  DECLARE_HDR_CACHE;

# define SPLIT_RANGE_WORDS 128  /* Must be power of 2.          */

  GC_objects_are_marked = TRUE;
  INIT_HDR_CACHE;
# ifdef OS2 /* Use untweaked version to circumvent compiler problem */
    while ((word)mark_stack_top >= (word)mark_stack && credit >= 0)
# else
    while ((((ptr_t)mark_stack_top - (ptr_t)mark_stack) | credit) >= 0)
# endif
  {
    current_p = mark_stack_top -> mse_start;
    descr = mark_stack_top -> mse_descr.w;
  retry:
    /* current_p and descr describe the current object.         */
    /* *mark_stack_top is vacant.                               */
    /* The following is 0 only for small objects described by a simple  */
    /* length descriptor.  For many applications this is the common     */
    /* case, so we try to detect it quickly.                            */
    if (descr & ((~(WORDS_TO_BYTES(SPLIT_RANGE_WORDS) - 1)) | GC_DS_TAGS)) {
      word tag = descr & GC_DS_TAGS;

      GC_STATIC_ASSERT(GC_DS_TAGS == 0x3);
      switch(tag) {
        case GC_DS_LENGTH:
          /* Large length.                                              */
          /* Process part of the range to avoid pushing too much on the */
          /* stack.                                                     */
          GC_ASSERT(descr < (word)GC_greatest_plausible_heap_addr
                            - (word)GC_least_plausible_heap_addr
                || (word)(current_p + descr)
                            <= (word)GC_least_plausible_heap_addr
                || (word)current_p >= (word)GC_greatest_plausible_heap_addr);
#         ifdef ENABLE_TRACE
            if ((word)GC_trace_addr >= (word)current_p
                && (word)GC_trace_addr < (word)(current_p + descr)) {
              GC_log_printf("GC #%u: large section; start %p, len %lu\n",
                        (unsigned)GC_gc_no, current_p, (unsigned long)descr);
            }
#         endif /* ENABLE_TRACE */
#         ifdef PARALLEL_MARK
#           define SHARE_BYTES 2048
            if (descr > SHARE_BYTES && GC_parallel
                && (word)mark_stack_top < (word)(mark_stack_limit - 1)) {
              int new_size = (descr/2) & ~(sizeof(word)-1);
              mark_stack_top -> mse_start = current_p;
              mark_stack_top -> mse_descr.w = new_size + sizeof(word);
                                        /* makes sure we handle         */
                                        /* misaligned pointers.         */
              mark_stack_top++;
#             ifdef ENABLE_TRACE
                if ((word)GC_trace_addr >= (word)current_p
                    && (word)GC_trace_addr < (word)(current_p + descr)) {
                  GC_log_printf("GC #%u: splitting (parallel) %p at %p\n",
                        (unsigned)GC_gc_no, current_p, current_p + new_size);
                }
#             endif /* ENABLE_TRACE */
              current_p += new_size;
              descr -= new_size;
              goto retry;
            }
#         endif /* PARALLEL_MARK */
          mark_stack_top -> mse_start =
                limit = current_p + WORDS_TO_BYTES(SPLIT_RANGE_WORDS-1);
          mark_stack_top -> mse_descr.w =
                                descr - WORDS_TO_BYTES(SPLIT_RANGE_WORDS-1);
#         ifdef ENABLE_TRACE
            if ((word)GC_trace_addr >= (word)current_p
                && (word)GC_trace_addr < (word)(current_p + descr)) {
              GC_log_printf("GC #%u: splitting %p at %p\n",
                            (unsigned)GC_gc_no, current_p, limit);
            }
#         endif /* ENABLE_TRACE */
          /* Make sure that pointers overlapping the two ranges are     */
          /* considered.                                                */
          limit += sizeof(word) - ALIGNMENT;
          break;
        case GC_DS_BITMAP:
          mark_stack_top--;
#         ifdef ENABLE_TRACE
            if ((word)GC_trace_addr >= (word)current_p
                && (word)GC_trace_addr < (word)(current_p
                                                + WORDS_TO_BYTES(WORDSZ-2))) {
              GC_log_printf("GC #%u: tracing from %p bitmap descr %lu\n",
                            (unsigned)GC_gc_no, current_p,
                            (unsigned long)descr);
            }
#         endif /* ENABLE_TRACE */
          descr &= ~GC_DS_TAGS;
          credit -= WORDS_TO_BYTES(WORDSZ/2); /* guess */
          while (descr != 0) {
            if ((signed_word)descr < 0) {
              current = *(word *)current_p;
              FIXUP_POINTER(current);
              if (current >= (word)least_ha && current < (word)greatest_ha) {
                PREFETCH((ptr_t)current);
#               ifdef ENABLE_TRACE
                  if (GC_trace_addr == current_p) {
                    GC_log_printf("GC #%u: considering(3) %p -> %p\n",
                                  (unsigned)GC_gc_no, current_p,
                                  (ptr_t)current);
                  }
#               endif /* ENABLE_TRACE */
                PUSH_CONTENTS((ptr_t)current, mark_stack_top,
                              mark_stack_limit, current_p, exit1);
              }
            }
            descr <<= 1;
            current_p += sizeof(word);
          }
          continue;
        case GC_DS_PROC:
          mark_stack_top--;
#         ifdef ENABLE_TRACE
            if ((word)GC_trace_addr >= (word)current_p
                && GC_base(current_p) != 0
                && GC_base(current_p) == GC_base(GC_trace_addr)) {
              GC_log_printf("GC #%u: tracing from %p, proc descr %lu\n",
                            (unsigned)GC_gc_no, current_p,
                            (unsigned long)descr);
            }
#         endif /* ENABLE_TRACE */
          credit -= GC_PROC_BYTES;
          mark_stack_top = (*PROC(descr))((word *)current_p, mark_stack_top,
                                          mark_stack_limit, ENV(descr));
          continue;
        case GC_DS_PER_OBJECT:
          if ((signed_word)descr >= 0) {
            /* Descriptor is in the object.     */
            descr = *(word *)(current_p + descr - GC_DS_PER_OBJECT);
          } else {
            /* Descriptor is in type descriptor pointed to by first     */
            /* word in object.                                          */
            ptr_t type_descr = *(ptr_t *)current_p;
            /* type_descr is either a valid pointer to the descriptor   */
            /* structure, or this object was on a free list.            */
            /* If it was anything but the last object on the free list, */
            /* we will misinterpret the next object on the free list as */
            /* the type descriptor, and get a 0 GC descriptor, which    */
            /* is ideal.  Unfortunately, we need to check for the last  */
            /* object case explicitly.                                  */
            if (0 == type_descr) {
                /* Rarely executed.     */
                mark_stack_top--;
                continue;
            }
            if ((GC_word)(type_descr) >= (GC_word)GC_least_plausible_heap_addr
                    && (GC_word)(type_descr)
                        <= (GC_word)GC_greatest_plausible_heap_addr) {
                /* type_descr looks like a pointer into the heap.       */
                /* It could still be the link pointer in a free list    */
                /* though.  That's not a problem as long as the offset  */
                /* of the actual descriptor in the pointed to object is */
                /* within the same object.  In that case it will either */
                /* point at the next free object in the list (if offset */
                /* is 0) or be zeroed (which we check for below,        */
                /* descr == 0).  If the offset is larger than the       */
                /* objects in the block type_descr points to it cannot  */
                /* be a proper pointer.                                 */
                word offset = ~(descr + (GC_INDIR_PER_OBJ_BIAS
                                         - GC_DS_PER_OBJECT - 1));
                hdr *hhdr;
                GET_HDR(type_descr, hhdr);
                if (NULL == hhdr || hhdr->hb_sz - sizeof(word) < offset) {
                    mark_stack_top--;
                    continue;
                }
            }
            descr = *(word *)(type_descr
                              - (descr + (GC_INDIR_PER_OBJ_BIAS
                                          - GC_DS_PER_OBJECT)));
          }
          if (0 == descr) {
              /* Can happen either because we generated a 0 descriptor  */
              /* or we saw a pointer to a free object.          */
              mark_stack_top--;
              continue;
          }
          goto retry;
      }
    } else /* Small object with length descriptor */ {
      mark_stack_top--;
#     ifndef SMALL_CONFIG
        if (descr < sizeof(word))
          continue;
#     endif
      limit = current_p + (word)descr;
    }
#   ifdef ENABLE_TRACE
        if ((word)GC_trace_addr >= (word)current_p
            && (word)GC_trace_addr < (word)limit) {
          GC_log_printf("GC #%u: Tracing from %p, length is %lu\n",
                        (unsigned)GC_gc_no, current_p, (unsigned long)descr);
        }
#   endif /* ENABLE_TRACE */
    /* The simple case in which we're scanning a range. */
    GC_ASSERT(!((word)current_p & (ALIGNMENT-1)));
    credit -= limit - current_p;
    limit -= sizeof(word);
    {
#     define PREF_DIST 4

#     ifndef SMALL_CONFIG
        word deferred;

        /* Try to prefetch the next pointer to be examined ASAP.        */
        /* Empirically, this also seems to help slightly without        */
        /* prefetches, at least on linux/X86.  Presumably this loop     */
        /* ends up with less register pressure, and gcc thus ends up    */
        /* generating slightly better code.  Overall gcc code quality   */
        /* for this loop is still not great.                            */
        for(;;) {
          PREFETCH(limit - PREF_DIST*CACHE_LINE_SIZE);
          GC_ASSERT((word)limit >= (word)current_p);
          deferred = *(word *)limit;
          FIXUP_POINTER(deferred);
          limit -= ALIGNMENT;
          if (deferred >= (word)least_ha && deferred < (word)greatest_ha) {
            PREFETCH((ptr_t)deferred);
            break;
          }
          if ((word)current_p > (word)limit) goto next_object;
          /* Unroll once, so we don't do too many of the prefetches     */
          /* based on limit.                                            */
          deferred = *(word *)limit;
          FIXUP_POINTER(deferred);
          limit -= ALIGNMENT;
          if (deferred >= (word)least_ha && deferred < (word)greatest_ha) {
            PREFETCH((ptr_t)deferred);
            break;
          }
          if ((word)current_p > (word)limit) goto next_object;
        }
#     endif

      while ((word)current_p <= (word)limit) {
        /* Empirically, unrolling this loop doesn't help a lot. */
        /* Since PUSH_CONTENTS expands to a lot of code,        */
        /* we don't.                                            */
        current = *(word *)current_p;
        FIXUP_POINTER(current);
        PREFETCH(current_p + PREF_DIST*CACHE_LINE_SIZE);
        if (current >= (word)least_ha && current < (word)greatest_ha) {
          /* Prefetch the contents of the object we just pushed.  It's  */
          /* likely we will need them soon.                             */
          PREFETCH((ptr_t)current);
#         ifdef ENABLE_TRACE
            if (GC_trace_addr == current_p) {
              GC_log_printf("GC #%u: considering(1) %p -> %p\n",
                            (unsigned)GC_gc_no, current_p, (ptr_t)current);
            }
#         endif /* ENABLE_TRACE */
          PUSH_CONTENTS((ptr_t)current, mark_stack_top,
                        mark_stack_limit, current_p, exit2);
        }
        current_p += ALIGNMENT;
      }

#     ifndef SMALL_CONFIG
        /* We still need to mark the entry we previously prefetched.    */
        /* We already know that it passes the preliminary pointer       */
        /* validity test.                                               */
#       ifdef ENABLE_TRACE
            if (GC_trace_addr == current_p) {
              GC_log_printf("GC #%u: considering(2) %p -> %p\n",
                            (unsigned)GC_gc_no, current_p, (ptr_t)deferred);
            }
#       endif /* ENABLE_TRACE */
        PUSH_CONTENTS((ptr_t)deferred, mark_stack_top,
                      mark_stack_limit, current_p, exit4);
        next_object:;
#     endif
    }
  }
  return mark_stack_top;
}

#ifdef PARALLEL_MARK

STATIC GC_bool GC_help_wanted = FALSE;  /* Protected by mark lock       */
STATIC unsigned GC_helper_count = 0;    /* Number of running helpers.   */
                                        /* Protected by mark lock       */
STATIC unsigned GC_active_count = 0;    /* Number of active helpers.    */
                                        /* Protected by mark lock       */
                                        /* May increase and decrease    */
                                        /* within each mark cycle.  But */
                                        /* once it returns to 0, it     */
                                        /* stays zero for the cycle.    */

GC_INNER word GC_mark_no = 0;

#define LOCAL_MARK_STACK_SIZE HBLKSIZE
        /* Under normal circumstances, this is big enough to guarantee  */
        /* we don't overflow half of it in a single call to             */
        /* GC_mark_from.                                                */

/* Wait all markers to finish initialization (i.e. store        */
/* marker_[b]sp, marker_mach_threads, GC_marker_Id).            */
GC_INNER void GC_wait_for_markers_init(void)
{
  word count;

  if (GC_markers_m1 == 0)
    return;

  /* Reuse marker lock and builders count to synchronize        */
  /* marker threads startup.                                    */
  GC_acquire_mark_lock();
  GC_fl_builder_count += (word)GC_markers_m1;
  count = GC_fl_builder_count;
  GC_release_mark_lock();
  if (count != 0)
    GC_wait_for_reclaim();
}

/* Steal mark stack entries starting at mse low into mark stack local   */
/* until we either steal mse high, or we have max entries.              */
/* Return a pointer to the top of the local mark stack.                 */
/* *next is replaced by a pointer to the next unscanned mark stack      */
/* entry.                                                               */
STATIC mse * GC_steal_mark_stack(mse * low, mse * high, mse * local,
                                 unsigned max, mse **next)
{
    mse *p;
    mse *top = local - 1;
    unsigned i = 0;

    GC_ASSERT((word)high >= (word)(low - 1)
              && (word)(high - low + 1) <= GC_mark_stack_size);
    for (p = low; (word)p <= (word)high && i <= max; ++p) {
        word descr = (word)AO_load(&p->mse_descr.ao);
        if (descr != 0) {
            /* Must be ordered after read of descr: */
            AO_store_release_write(&p->mse_descr.ao, 0);
            /* More than one thread may get this entry, but that's only */
            /* a minor performance problem.                             */
            ++top;
            top -> mse_descr.w = descr;
            top -> mse_start = p -> mse_start;
            GC_ASSERT((top->mse_descr.w & GC_DS_TAGS) != GC_DS_LENGTH ||
                      top->mse_descr.w < (word)GC_greatest_plausible_heap_addr
                                         - (word)GC_least_plausible_heap_addr);
            /* If this is a big object, count it as                     */
            /* size/256 + 1 objects.                                    */
            ++i;
            if ((descr & GC_DS_TAGS) == GC_DS_LENGTH) i += (int)(descr >> 8);
        }
    }
    *next = p;
    return top;
}

/* Copy back a local mark stack.        */
/* low and high are inclusive bounds.   */
STATIC void GC_return_mark_stack(mse * low, mse * high)
{
    mse * my_top;
    mse * my_start;
    size_t stack_size;

    if ((word)high < (word)low) return;
    stack_size = high - low + 1;
    GC_acquire_mark_lock();
    my_top = GC_mark_stack_top; /* Concurrent modification impossible. */
    my_start = my_top + 1;
    if ((word)(my_start - GC_mark_stack + stack_size)
                > (word)GC_mark_stack_size) {
      GC_COND_LOG_PRINTF("No room to copy back mark stack\n");
      GC_mark_state = MS_INVALID;
      GC_mark_stack_too_small = TRUE;
      /* We drop the local mark stack.  We'll fix things later. */
    } else {
      BCOPY(low, my_start, stack_size * sizeof(mse));
      GC_ASSERT((mse *)AO_load((volatile AO_t *)(&GC_mark_stack_top))
                == my_top);
      AO_store_release_write((volatile AO_t *)(&GC_mark_stack_top),
                             (AO_t)(my_top + stack_size));
                /* Ensures visibility of previously written stack contents. */
    }
    GC_release_mark_lock();
    GC_notify_all_marker();
}

/* Mark from the local mark stack.              */
/* On return, the local mark stack is empty.    */
/* But this may be achieved by copying the      */
/* local mark stack back into the global one.   */
STATIC void GC_do_local_mark(mse *local_mark_stack, mse *local_top)
{
    unsigned n;
#   define N_LOCAL_ITERS 1

#   ifdef GC_ASSERTIONS
      /* Make sure we don't hold mark lock. */
        GC_acquire_mark_lock();
        GC_release_mark_lock();
#   endif
    for (;;) {
        for (n = 0; n < N_LOCAL_ITERS; ++n) {
            local_top = GC_mark_from(local_top, local_mark_stack,
                                     local_mark_stack + LOCAL_MARK_STACK_SIZE);
            if ((word)local_top < (word)local_mark_stack) return;
            if ((word)(local_top - local_mark_stack)
                        >= LOCAL_MARK_STACK_SIZE / 2) {
                GC_return_mark_stack(local_mark_stack, local_top);
                return;
            }
        }
        if ((word)AO_load((volatile AO_t *)&GC_mark_stack_top)
            < (word)AO_load(&GC_first_nonempty)
            && GC_active_count < GC_helper_count
            && (word)local_top > (word)(local_mark_stack + 1)) {
            /* Try to share the load, since the main stack is empty,    */
            /* and helper threads are waiting for a refill.             */
            /* The entries near the bottom of the stack are likely      */
            /* to require more work.  Thus we return those, even though */
            /* it's harder.                                             */
            mse * new_bottom = local_mark_stack
                                + (local_top - local_mark_stack)/2;
            GC_ASSERT((word)new_bottom > (word)local_mark_stack
                      && (word)new_bottom < (word)local_top);
            GC_return_mark_stack(local_mark_stack, new_bottom - 1);
            memmove(local_mark_stack, new_bottom,
                    (local_top - new_bottom + 1) * sizeof(mse));
            local_top -= (new_bottom - local_mark_stack);
        }
    }
}

#define ENTRIES_TO_GET 5

/* Mark using the local mark stack until the global mark stack is empty */
/* and there are no active workers. Update GC_first_nonempty to reflect */
/* progress.                                                            */
/* Caller does not hold mark lock.                                      */
/* Caller has already incremented GC_helper_count.  We decrement it,    */
/* and maintain GC_active_count.                                        */
STATIC void GC_mark_local(mse *local_mark_stack, int id)
{
    mse * my_first_nonempty;

    GC_acquire_mark_lock();
    GC_active_count++;
    my_first_nonempty = (mse *)AO_load(&GC_first_nonempty);
    GC_ASSERT((word)GC_mark_stack <= (word)my_first_nonempty);
    GC_ASSERT((word)my_first_nonempty
        <= (word)AO_load((volatile AO_t *)&GC_mark_stack_top) + sizeof(mse));
    GC_VERBOSE_LOG_PRINTF("Starting mark helper %lu\n", (unsigned long)id);
    GC_release_mark_lock();
    for (;;) {
        size_t n_on_stack;
        unsigned n_to_get;
        mse * my_top;
        mse * local_top;
        mse * global_first_nonempty = (mse *)AO_load(&GC_first_nonempty);

        GC_ASSERT((word)my_first_nonempty >= (word)GC_mark_stack &&
                  (word)my_first_nonempty <=
                        (word)AO_load((volatile AO_t *)&GC_mark_stack_top)
                        + sizeof(mse));
        GC_ASSERT((word)global_first_nonempty >= (word)GC_mark_stack &&
                  (word)global_first_nonempty <=
                        (word)AO_load((volatile AO_t *)&GC_mark_stack_top)
                        + sizeof(mse));
        if ((word)my_first_nonempty < (word)global_first_nonempty) {
            my_first_nonempty = global_first_nonempty;
        } else if ((word)global_first_nonempty < (word)my_first_nonempty) {
            AO_compare_and_swap(&GC_first_nonempty,
                                (AO_t) global_first_nonempty,
                                (AO_t) my_first_nonempty);
            /* If this fails, we just go ahead, without updating        */
            /* GC_first_nonempty.                                       */
        }
        /* Perhaps we should also update GC_first_nonempty, if it */
        /* is less.  But that would require using atomic updates. */
        my_top = (mse *)AO_load_acquire((volatile AO_t *)(&GC_mark_stack_top));
        n_on_stack = my_top - my_first_nonempty + 1;
        if (0 == n_on_stack) {
            GC_acquire_mark_lock();
            my_top = GC_mark_stack_top;
                /* Asynchronous modification impossible here,   */
                /* since we hold mark lock.                     */
            n_on_stack = my_top - my_first_nonempty + 1;
            if (0 == n_on_stack) {
                GC_active_count--;
                GC_ASSERT(GC_active_count <= GC_helper_count);
                /* Other markers may redeposit objects  */
                /* on the stack.                                */
                if (0 == GC_active_count) GC_notify_all_marker();
                while (GC_active_count > 0
                       && (word)AO_load(&GC_first_nonempty)
                                > (word)GC_mark_stack_top) {
                    /* We will be notified if either GC_active_count    */
                    /* reaches zero, or if more objects are pushed on   */
                    /* the global mark stack.                           */
                    GC_wait_marker();
                }
                if (GC_active_count == 0
                    && (word)AO_load(&GC_first_nonempty)
                        > (word)GC_mark_stack_top) {
                    GC_bool need_to_notify = FALSE;
                    /* The above conditions can't be falsified while we */
                    /* hold the mark lock, since neither                */
                    /* GC_active_count nor GC_mark_stack_top can        */
                    /* change.  GC_first_nonempty can only be           */
                    /* incremented asynchronously.  Thus we know that   */
                    /* both conditions actually held simultaneously.    */
                    GC_helper_count--;
                    if (0 == GC_helper_count) need_to_notify = TRUE;
                    GC_VERBOSE_LOG_PRINTF("Finished mark helper %lu\n",
                                          (unsigned long)id);
                    GC_release_mark_lock();
                    if (need_to_notify) GC_notify_all_marker();
                    return;
                }
                /* else there's something on the stack again, or        */
                /* another helper may push something.                   */
                GC_active_count++;
                GC_ASSERT(GC_active_count > 0);
                GC_release_mark_lock();
                continue;
            } else {
                GC_release_mark_lock();
            }
        }
        n_to_get = ENTRIES_TO_GET;
        if (n_on_stack < 2 * ENTRIES_TO_GET) n_to_get = 1;
        local_top = GC_steal_mark_stack(my_first_nonempty, my_top,
                                        local_mark_stack, n_to_get,
                                        &my_first_nonempty);
        GC_ASSERT((word)my_first_nonempty >= (word)GC_mark_stack &&
                  (word)my_first_nonempty <=
                        (word)AO_load((volatile AO_t *)&GC_mark_stack_top)
                        + sizeof(mse));
        GC_do_local_mark(local_mark_stack, local_top);
    }
}

/* Perform Parallel mark.                       */
/* We hold the GC lock, not the mark lock.      */
/* Currently runs until the mark stack is       */
/* empty.                                       */
STATIC void GC_do_parallel_mark(void)
{
    mse local_mark_stack[LOCAL_MARK_STACK_SIZE];
                /* Note: local_mark_stack is quite big (up to 128 KiB). */

    GC_acquire_mark_lock();
    GC_ASSERT(I_HOLD_LOCK());
    /* This could be a GC_ASSERT, but it seems safer to keep it on      */
    /* all the time, especially since it's cheap.                       */
    if (GC_help_wanted || GC_active_count != 0 || GC_helper_count != 0)
        ABORT("Tried to start parallel mark in bad state");
    GC_VERBOSE_LOG_PRINTF("Starting marking for mark phase number %lu\n",
                          (unsigned long)GC_mark_no);
    GC_first_nonempty = (AO_t)GC_mark_stack;
    GC_active_count = 0;
    GC_helper_count = 1;
    GC_help_wanted = TRUE;
    GC_release_mark_lock();
    GC_notify_all_marker();
        /* Wake up potential helpers.   */
    GC_mark_local(local_mark_stack, 0);
    GC_acquire_mark_lock();
    GC_help_wanted = FALSE;
    /* Done; clean up.  */
    while (GC_helper_count > 0) {
      GC_wait_marker();
    }
    /* GC_helper_count cannot be incremented while GC_help_wanted == FALSE */
    GC_VERBOSE_LOG_PRINTF("Finished marking for mark phase number %lu\n",
                          (unsigned long)GC_mark_no);
    GC_mark_no++;
    GC_release_mark_lock();
    GC_notify_all_marker();
}


/* Try to help out the marker, if it's running.         */
/* We do not hold the GC lock, but the requestor does.  */
GC_INNER void GC_help_marker(word my_mark_no)
{
    unsigned my_id;
    mse local_mark_stack[LOCAL_MARK_STACK_SIZE];
                /* Note: local_mark_stack is quite big (up to 128 KiB). */

    if (!GC_parallel) return;

    GC_acquire_mark_lock();
    while (GC_mark_no < my_mark_no
           || (!GC_help_wanted && GC_mark_no == my_mark_no)) {
      GC_wait_marker();
    }
    my_id = GC_helper_count;
    if (GC_mark_no != my_mark_no || my_id > (unsigned)GC_markers_m1) {
      /* Second test is useful only if original threads can also        */
      /* act as helpers.  Under Linux they can't.                       */
      GC_release_mark_lock();
      return;
    }
    GC_helper_count = my_id + 1;
    GC_release_mark_lock();
    GC_mark_local(local_mark_stack, my_id);
    /* GC_mark_local decrements GC_helper_count. */
}

#endif /* PARALLEL_MARK */

/* Allocate or reallocate space for mark stack of size n entries.  */
/* May silently fail.                                              */
static void alloc_mark_stack(size_t n)
{
    mse * new_stack = (mse *)GC_scratch_alloc(n * sizeof(struct GC_ms_entry));
#   ifdef GWW_VDB
      /* Don't recycle a stack segment obtained with the wrong flags.   */
      /* Win32 GetWriteWatch requires the right kind of memory.         */
      static GC_bool GC_incremental_at_stack_alloc = FALSE;
      GC_bool recycle_old = (!GC_incremental || GC_incremental_at_stack_alloc);

      GC_incremental_at_stack_alloc = GC_incremental;
#   else
#     define recycle_old TRUE
#   endif

    GC_mark_stack_too_small = FALSE;
    if (GC_mark_stack_size != 0) {
        if (new_stack != 0) {
          if (recycle_old) {
            /* Recycle old space */
              size_t page_offset = (word)GC_mark_stack & (GC_page_size - 1);
              size_t size = GC_mark_stack_size * sizeof(struct GC_ms_entry);
              size_t displ = 0;

              if (0 != page_offset) displ = GC_page_size - page_offset;
              size = (size - displ) & ~(GC_page_size - 1);
              if (size > 0) {
                GC_add_to_heap((struct hblk *)
                                ((word)GC_mark_stack + displ), (word)size);
              }
          }
          GC_mark_stack = new_stack;
          GC_mark_stack_size = n;
          /* FIXME: Do we need some way to reset GC_mark_stack_size?    */
          GC_mark_stack_limit = new_stack + n;
          GC_COND_LOG_PRINTF("Grew mark stack to %lu frames\n",
                             (unsigned long)GC_mark_stack_size);
        } else {
          WARN("Failed to grow mark stack to %" WARN_PRIdPTR " frames\n", n);
        }
    } else {
        if (new_stack == 0) {
            GC_err_printf("No space for mark stack\n");
            EXIT();
        }
        GC_mark_stack = new_stack;
        GC_mark_stack_size = n;
        GC_mark_stack_limit = new_stack + n;
    }
    GC_mark_stack_top = GC_mark_stack-1;
}

GC_INNER void GC_mark_init(void)
{
    alloc_mark_stack(INITIAL_MARK_STACK_SIZE);
}

/*
 * Push all locations between b and t onto the mark stack.
 * b is the first location to be checked. t is one past the last
 * location to be checked.
 * Should only be used if there is no possibility of mark stack
 * overflow.
 */
GC_API void GC_CALL GC_push_all(char *bottom, char *top)
{
    register word length;

    bottom = (char *)(((word) bottom + ALIGNMENT-1) & ~(ALIGNMENT-1));
    top = (char *)(((word) top) & ~(ALIGNMENT-1));
    if ((word)bottom >= (word)top) return;

    GC_mark_stack_top++;
    if ((word)GC_mark_stack_top >= (word)GC_mark_stack_limit) {
        ABORT("Unexpected mark stack overflow");
    }
    length = top - bottom;
#   if GC_DS_TAGS > ALIGNMENT - 1
        length += GC_DS_TAGS;
        length &= ~GC_DS_TAGS;
#   endif
    GC_mark_stack_top -> mse_start = bottom;
    GC_mark_stack_top -> mse_descr.w = length;
}

#ifndef GC_DISABLE_INCREMENTAL

  /* Analogous to the above, but push only those pages h with           */
  /* dirty_fn(h) != 0.  We use GC_push_all to actually push the block.  */
  /* Used both to selectively push dirty pages, or to push a block in   */
  /* piecemeal fashion, to allow for more marking concurrency.          */
  /* Will not overflow mark stack if GC_push_all pushes a small fixed   */
  /* number of entries.  (This is invoked only if GC_push_all pushes    */
  /* a single entry, or if it marks each object before pushing it, thus */
  /* ensuring progress in the event of a stack overflow.)               */
  STATIC void GC_push_selected(ptr_t bottom, ptr_t top,
                               GC_bool (*dirty_fn)(struct hblk *))
  {
    struct hblk * h;

    bottom = (ptr_t)(((word) bottom + ALIGNMENT-1) & ~(ALIGNMENT-1));
    top = (ptr_t)(((word) top) & ~(ALIGNMENT-1));
    if ((word)bottom >= (word)top) return;

    h = HBLKPTR(bottom + HBLKSIZE);
    if ((word)top <= (word)h) {
        if ((*dirty_fn)(h-1)) {
            GC_push_all(bottom, top);
        }
        return;
    }
    if ((*dirty_fn)(h-1)) {
        GC_push_all(bottom, (ptr_t)h);
    }

    while ((word)(h+1) <= (word)top) {
        if ((*dirty_fn)(h)) {
            if ((word)(GC_mark_stack_top - GC_mark_stack)
                > 3 * GC_mark_stack_size / 4) {
                /* Danger of mark stack overflow */
                GC_push_all((ptr_t)h, top);
                return;
            } else {
                GC_push_all((ptr_t)h, (ptr_t)(h+1));
            }
        }
        h++;
    }

    if ((ptr_t)h != top && (*dirty_fn)(h)) {
       GC_push_all((ptr_t)h, top);
    }
    if ((word)GC_mark_stack_top >= (word)GC_mark_stack_limit) {
        ABORT("Unexpected mark stack overflow");
    }
  }

  GC_API void GC_CALL GC_push_conditional(char *bottom, char *top, int all)
  {
    if (!all) {
      GC_push_selected((ptr_t)bottom, (ptr_t)top, GC_page_was_dirty);
    } else {
#     ifdef PROC_VDB
        if (GC_dirty_maintained) {
          /* Pages that were never dirtied cannot contain pointers.     */
          GC_push_selected((ptr_t)bottom, (ptr_t)top, GC_page_was_ever_dirty);
        } else
#     endif
      /* else */ {
        GC_push_all(bottom, top);
      }
    }
  }
#else
  GC_API void GC_CALL GC_push_conditional(char *bottom, char *top,
                                          int all GC_ATTR_UNUSED)
  {
    GC_push_all(bottom, top);
  }
#endif /* GC_DISABLE_INCREMENTAL */

#if defined(MSWIN32) || defined(MSWINCE)
  void __cdecl GC_push_one(word p)
#else
  void GC_push_one(word p)
#endif
{
    GC_PUSH_ONE_STACK(p, MARKED_FROM_REGISTER);
}

GC_API struct GC_ms_entry * GC_CALL GC_mark_and_push(void *obj,
                                                mse *mark_stack_ptr,
                                                mse *mark_stack_limit,
                                                void ** src GC_ATTR_UNUSED)
{
    hdr * hhdr;

    PREFETCH(obj);
    GET_HDR(obj, hhdr);
    if ((EXPECT(IS_FORWARDING_ADDR_OR_NIL(hhdr), FALSE)
         && (!GC_all_interior_pointers
             || NULL == (hhdr = GC_find_header(GC_base(obj)))))
        || EXPECT(HBLK_IS_FREE(hhdr), FALSE)) {
      GC_ADD_TO_BLACK_LIST_NORMAL(obj, (ptr_t)src);
      return mark_stack_ptr;
    }

    PUSH_CONTENTS_HDR(obj, mark_stack_ptr /* modified */, mark_stack_limit,
                      (ptr_t)src, was_marked, hhdr, TRUE);
 was_marked:
    return mark_stack_ptr;
}

#if defined(MANUAL_VDB) && defined(THREADS)
  void GC_dirty(ptr_t p);
#endif

/* Mark and push (i.e. gray) a single object p onto the main    */
/* mark stack.  Consider p to be valid if it is an interior     */
/* pointer.                                                     */
/* The object p has passed a preliminary pointer validity       */
/* test, but we do not definitely know whether it is valid.     */
/* Mark bits are NOT atomically updated.  Thus this must be the */
/* only thread setting them.                                    */
# if defined(PRINT_BLACK_LIST) || defined(KEEP_BACK_PTRS)
    GC_INNER void GC_mark_and_push_stack(ptr_t p, ptr_t source)
# else
    GC_INNER void GC_mark_and_push_stack(ptr_t p)
#   define source ((ptr_t)0)
# endif
{
    hdr * hhdr;
    ptr_t r = p;

    PREFETCH(p);
    GET_HDR(p, hhdr);
    if (EXPECT(IS_FORWARDING_ADDR_OR_NIL(hhdr), FALSE)
        && (NULL == hhdr
            || (r = GC_base(p)) == NULL
            || (hhdr = HDR(r)) == NULL)) {
        GC_ADD_TO_BLACK_LIST_STACK(p, source);
        return;
    }
    if (EXPECT(HBLK_IS_FREE(hhdr), FALSE)) {
        GC_ADD_TO_BLACK_LIST_NORMAL(p, source);
        return;
    }
#   if defined(MANUAL_VDB) && defined(THREADS)
      /* Pointer is on the stack.  We may have dirtied the object       */
      /* it points to, but not yet have called GC_dirty();              */
      GC_dirty(p);      /* Implicitly affects entire object.            */
#   endif
    PUSH_CONTENTS_HDR(r, GC_mark_stack_top, GC_mark_stack_limit,
                      source, mark_and_push_exit, hhdr, FALSE);
  mark_and_push_exit: ;
    /* We silently ignore pointers to near the end of a block,  */
    /* which is very mildly suboptimal.                         */
    /* FIXME: We should probably add a header word to address   */
    /* this.                                                    */
}
# undef source

# ifdef TRACE_BUF

# define TRACE_ENTRIES 1000

struct trace_entry {
    char * kind;
    word gc_no;
    word bytes_allocd;
    word arg1;
    word arg2;
} GC_trace_buf[TRACE_ENTRIES];

int GC_trace_buf_ptr = 0;

void GC_add_trace_entry(char *kind, word arg1, word arg2)
{
    GC_trace_buf[GC_trace_buf_ptr].kind = kind;
    GC_trace_buf[GC_trace_buf_ptr].gc_no = GC_gc_no;
    GC_trace_buf[GC_trace_buf_ptr].bytes_allocd = GC_bytes_allocd;
    GC_trace_buf[GC_trace_buf_ptr].arg1 = arg1 ^ 0x80000000;
    GC_trace_buf[GC_trace_buf_ptr].arg2 = arg2 ^ 0x80000000;
    GC_trace_buf_ptr++;
    if (GC_trace_buf_ptr >= TRACE_ENTRIES) GC_trace_buf_ptr = 0;
}

void GC_print_trace_inner(word gc_no)
{
    int i;
    struct trace_entry *p;

    for (i = GC_trace_buf_ptr-1; i != GC_trace_buf_ptr; i--) {
        if (i < 0) i = TRACE_ENTRIES-1;
        p = GC_trace_buf + i;
        if (p -> gc_no < gc_no || p -> kind == 0) {
            return;
        }
        GC_printf("Trace:%s (gc:%u, bytes:%lu) 0x%lX, 0x%lX\n",
                  p -> kind, (unsigned)p -> gc_no,
                  (unsigned long)p -> bytes_allocd,
                  (long)p->arg1 ^ 0x80000000L, (long)p->arg2 ^ 0x80000000L);
    }
    GC_printf("Trace incomplete\n");
}

void GC_print_trace(word gc_no)
{
    DCL_LOCK_STATE;

    LOCK();
    GC_print_trace_inner(gc_no);
    UNLOCK();
}

# endif /* TRACE_BUF */

/*
 * A version of GC_push_all that treats all interior pointers as valid
 * and scans the entire region immediately, in case the contents
 * change.
 */
GC_API void GC_CALL GC_push_all_eager(char *bottom, char *top)
{
    word * b = (word *)(((word) bottom + ALIGNMENT-1) & ~(ALIGNMENT-1));
    word * t = (word *)(((word) top) & ~(ALIGNMENT-1));
    register word *p;
    register word q;
    register word *lim;
    register ptr_t greatest_ha = GC_greatest_plausible_heap_addr;
    register ptr_t least_ha = GC_least_plausible_heap_addr;
#   define GC_greatest_plausible_heap_addr greatest_ha
#   define GC_least_plausible_heap_addr least_ha

    if (top == 0) return;
    /* check all pointers in range and push if they appear      */
    /* to be valid.                                             */
      lim = t - 1 /* longword */;
      for (p = b; (word)p <= (word)lim;
           p = (word *)(((ptr_t)p) + ALIGNMENT)) {
        q = *p;
        GC_PUSH_ONE_STACK(q, p);
      }
#   undef GC_greatest_plausible_heap_addr
#   undef GC_least_plausible_heap_addr
}

GC_INNER void GC_push_all_stack(ptr_t bottom, ptr_t top)
{
# if defined(THREADS) && defined(MPROTECT_VDB)
    GC_push_all_eager(bottom, top);
# else
    if (!NEED_FIXUP_POINTER && GC_all_interior_pointers) {
      GC_push_all(bottom, top);
    } else {
      GC_push_all_eager(bottom, top);
    }
# endif
}

#if !defined(SMALL_CONFIG) && !defined(USE_MARK_BYTES) && \
    defined(MARK_BIT_PER_GRANULE)
# if GC_GRANULE_WORDS == 1
#   define USE_PUSH_MARKED_ACCELERATORS
#   define PUSH_GRANULE(q) \
                do { \
                  word qcontents = (q)[0]; \
                  GC_PUSH_ONE_HEAP(qcontents, q, GC_mark_stack_top); \
                } while (0)
# elif GC_GRANULE_WORDS == 2
#   define USE_PUSH_MARKED_ACCELERATORS
#   define PUSH_GRANULE(q) \
                do { \
                  word qcontents = (q)[0]; \
                  GC_PUSH_ONE_HEAP(qcontents, q, GC_mark_stack_top); \
                  qcontents = (q)[1]; \
                  GC_PUSH_ONE_HEAP(qcontents, (q)+1, GC_mark_stack_top); \
                } while (0)
# elif GC_GRANULE_WORDS == 4
#   define USE_PUSH_MARKED_ACCELERATORS
#   define PUSH_GRANULE(q) \
                do { \
                  word qcontents = (q)[0]; \
                  GC_PUSH_ONE_HEAP(qcontents, q, GC_mark_stack_top); \
                  qcontents = (q)[1]; \
                  GC_PUSH_ONE_HEAP(qcontents, (q)+1, GC_mark_stack_top); \
                  qcontents = (q)[2]; \
                  GC_PUSH_ONE_HEAP(qcontents, (q)+2, GC_mark_stack_top); \
                  qcontents = (q)[3]; \
                  GC_PUSH_ONE_HEAP(qcontents, (q)+3, GC_mark_stack_top); \
                } while (0)
# endif
#endif /* !USE_MARK_BYTES && MARK_BIT_PER_GRANULE */

#ifdef USE_PUSH_MARKED_ACCELERATORS
/* Push all objects reachable from marked objects in the given block */
/* containing objects of size 1 granule.                             */
STATIC void GC_push_marked1(struct hblk *h, hdr *hhdr)
{
    word * mark_word_addr = &(hhdr->hb_marks[0]);
    word *p;
    word *plim;
    word *q;
    word mark_word;

    /* Allow registers to be used for some frequently accessed  */
    /* global variables.  Otherwise aliasing issues are likely  */
    /* to prevent that.                                         */
    ptr_t greatest_ha = GC_greatest_plausible_heap_addr;
    ptr_t least_ha = GC_least_plausible_heap_addr;
    mse * mark_stack_top = GC_mark_stack_top;
    mse * mark_stack_limit = GC_mark_stack_limit;

#   undef GC_mark_stack_top
#   undef GC_mark_stack_limit
#   define GC_mark_stack_top mark_stack_top
#   define GC_mark_stack_limit mark_stack_limit
#   define GC_greatest_plausible_heap_addr greatest_ha
#   define GC_least_plausible_heap_addr least_ha

    p = (word *)(h->hb_body);
    plim = (word *)(((word)h) + HBLKSIZE);

    /* go through all words in block */
        while ((word)p < (word)plim) {
            mark_word = *mark_word_addr++;
            q = p;
            while(mark_word != 0) {
              if (mark_word & 1) {
                  PUSH_GRANULE(q);
              }
              q += GC_GRANULE_WORDS;
              mark_word >>= 1;
            }
            p += WORDSZ*GC_GRANULE_WORDS;
        }

#   undef GC_greatest_plausible_heap_addr
#   undef GC_least_plausible_heap_addr
#   undef GC_mark_stack_top
#   undef GC_mark_stack_limit
#   define GC_mark_stack_limit GC_arrays._mark_stack_limit
#   define GC_mark_stack_top GC_arrays._mark_stack_top
    GC_mark_stack_top = mark_stack_top;
}


#ifndef UNALIGNED_PTRS

/* Push all objects reachable from marked objects in the given block */
/* of size 2 (granules) objects.                                     */
STATIC void GC_push_marked2(struct hblk *h, hdr *hhdr)
{
    word * mark_word_addr = &(hhdr->hb_marks[0]);
    word *p;
    word *plim;
    word *q;
    word mark_word;

    ptr_t greatest_ha = GC_greatest_plausible_heap_addr;
    ptr_t least_ha = GC_least_plausible_heap_addr;
    mse * mark_stack_top = GC_mark_stack_top;
    mse * mark_stack_limit = GC_mark_stack_limit;

#   undef GC_mark_stack_top
#   undef GC_mark_stack_limit
#   define GC_mark_stack_top mark_stack_top
#   define GC_mark_stack_limit mark_stack_limit
#   define GC_greatest_plausible_heap_addr greatest_ha
#   define GC_least_plausible_heap_addr least_ha

    p = (word *)(h->hb_body);
    plim = (word *)(((word)h) + HBLKSIZE);

    /* go through all words in block */
        while ((word)p < (word)plim) {
            mark_word = *mark_word_addr++;
            q = p;
            while(mark_word != 0) {
              if (mark_word & 1) {
                  PUSH_GRANULE(q);
                  PUSH_GRANULE(q + GC_GRANULE_WORDS);
              }
              q += 2 * GC_GRANULE_WORDS;
              mark_word >>= 2;
            }
            p += WORDSZ*GC_GRANULE_WORDS;
        }

#   undef GC_greatest_plausible_heap_addr
#   undef GC_least_plausible_heap_addr
#   undef GC_mark_stack_top
#   undef GC_mark_stack_limit
#   define GC_mark_stack_limit GC_arrays._mark_stack_limit
#   define GC_mark_stack_top GC_arrays._mark_stack_top
    GC_mark_stack_top = mark_stack_top;
}

# if GC_GRANULE_WORDS < 4
/* Push all objects reachable from marked objects in the given block */
/* of size 4 (granules) objects.                                     */
/* There is a risk of mark stack overflow here.  But we handle that. */
/* And only unmarked objects get pushed, so it's not very likely.    */
STATIC void GC_push_marked4(struct hblk *h, hdr *hhdr)
{
    word * mark_word_addr = &(hhdr->hb_marks[0]);
    word *p;
    word *plim;
    word *q;
    word mark_word;

    ptr_t greatest_ha = GC_greatest_plausible_heap_addr;
    ptr_t least_ha = GC_least_plausible_heap_addr;
    mse * mark_stack_top = GC_mark_stack_top;
    mse * mark_stack_limit = GC_mark_stack_limit;

#   undef GC_mark_stack_top
#   undef GC_mark_stack_limit
#   define GC_mark_stack_top mark_stack_top
#   define GC_mark_stack_limit mark_stack_limit
#   define GC_greatest_plausible_heap_addr greatest_ha
#   define GC_least_plausible_heap_addr least_ha

    p = (word *)(h->hb_body);
    plim = (word *)(((word)h) + HBLKSIZE);

    /* go through all words in block */
        while ((word)p < (word)plim) {
            mark_word = *mark_word_addr++;
            q = p;
            while(mark_word != 0) {
              if (mark_word & 1) {
                  PUSH_GRANULE(q);
                  PUSH_GRANULE(q + GC_GRANULE_WORDS);
                  PUSH_GRANULE(q + 2*GC_GRANULE_WORDS);
                  PUSH_GRANULE(q + 3*GC_GRANULE_WORDS);
              }
              q += 4 * GC_GRANULE_WORDS;
              mark_word >>= 4;
            }
            p += WORDSZ*GC_GRANULE_WORDS;
        }
#   undef GC_greatest_plausible_heap_addr
#   undef GC_least_plausible_heap_addr
#   undef GC_mark_stack_top
#   undef GC_mark_stack_limit
#   define GC_mark_stack_limit GC_arrays._mark_stack_limit
#   define GC_mark_stack_top GC_arrays._mark_stack_top
    GC_mark_stack_top = mark_stack_top;
}

#endif /* GC_GRANULE_WORDS < 4 */

#endif /* UNALIGNED_PTRS */

#endif /* USE_PUSH_MARKED_ACCELERATORS */

/* Push all objects reachable from marked objects in the given block */
STATIC void GC_push_marked(struct hblk *h, hdr *hhdr)
{
    size_t sz = hhdr -> hb_sz;
    word descr = hhdr -> hb_descr;
    ptr_t p;
    word bit_no;
    ptr_t lim;
    mse * GC_mark_stack_top_reg;
    mse * mark_stack_limit = GC_mark_stack_limit;

    /* Some quick shortcuts: */
        if ((/* 0 | */ GC_DS_LENGTH) == descr) return;
        if (GC_block_empty(hhdr)/* nothing marked */) return;
    GC_n_rescuing_pages++;
    GC_objects_are_marked = TRUE;
    if (sz > MAXOBJBYTES) {
        lim = h -> hb_body;
    } else {
        lim = (h + 1)->hb_body - sz;
    }

    switch(BYTES_TO_GRANULES(sz)) {
#   if defined(USE_PUSH_MARKED_ACCELERATORS)
     case 1:
       GC_push_marked1(h, hhdr);
       break;
#    if !defined(UNALIGNED_PTRS)
       case 2:
         GC_push_marked2(h, hhdr);
         break;
#     if GC_GRANULE_WORDS < 4
       case 4:
         GC_push_marked4(h, hhdr);
         break;
#     endif
#    endif
#   endif
     default:
      GC_mark_stack_top_reg = GC_mark_stack_top;
      for (p = h -> hb_body, bit_no = 0; (word)p <= (word)lim;
           p += sz, bit_no += MARK_BIT_OFFSET(sz)) {
         if (mark_bit_from_hdr(hhdr, bit_no)) {
           /* Mark from fields inside the object */
             PUSH_OBJ(p, hhdr, GC_mark_stack_top_reg, mark_stack_limit);
         }
      }
      GC_mark_stack_top = GC_mark_stack_top_reg;
    }
}

#ifdef ENABLE_DISCLAIM
/* Unconditionally mark from all objects which have not been reclaimed. */
/* This is useful in order to retain pointers which are reachable from  */
/* the disclaim notifiers.                                              */
/*                                                                      */
/* To determine whether an object has been reclaimed, we require that   */
/* any live object has a non-zero as one of the two lowest bits of the  */
/* first word.  On the other hand, a reclaimed object is a members of   */
/* free-lists, and thus contains a word-aligned next-pointer as the     */
/* first word.                                                          */
 STATIC void GC_push_unconditionally(struct hblk *h, hdr *hhdr)
 {
    size_t sz = hhdr -> hb_sz;
    word descr = hhdr -> hb_descr;
    ptr_t p;
    ptr_t lim;
    mse * GC_mark_stack_top_reg;
    mse * mark_stack_limit = GC_mark_stack_limit;

    if ((/* 0 | */ GC_DS_LENGTH) == descr)
        return;

    GC_n_rescuing_pages++;
    GC_objects_are_marked = TRUE;
    if (sz > MAXOBJBYTES)
        lim = h -> hb_body;
    else
        lim = (h + 1)->hb_body - sz;

    GC_mark_stack_top_reg = GC_mark_stack_top;
    for (p = h -> hb_body; (word)p <= (word)lim; p += sz)
        if ((*(word *)p & 0x3) != 0)
            PUSH_OBJ(p, hhdr, GC_mark_stack_top_reg, mark_stack_limit);
    GC_mark_stack_top = GC_mark_stack_top_reg;
  }
#endif /* ENABLE_DISCLAIM */

#ifndef GC_DISABLE_INCREMENTAL
  /* Test whether any page in the given block is dirty.   */
  STATIC GC_bool GC_block_was_dirty(struct hblk *h, hdr *hhdr)
  {
    size_t sz = hhdr -> hb_sz;

    if (sz <= MAXOBJBYTES) {
         return(GC_page_was_dirty(h));
    } else {
         ptr_t p = (ptr_t)h;
         while ((word)p < (word)h + sz) {
             if (GC_page_was_dirty((struct hblk *)p)) return(TRUE);
             p += HBLKSIZE;
         }
         return(FALSE);
    }
  }
#endif /* GC_DISABLE_INCREMENTAL */

/* Similar to GC_push_marked, but skip over unallocated blocks  */
/* and return address of next plausible block.                  */
STATIC struct hblk * GC_push_next_marked(struct hblk *h)
{
    hdr * hhdr = HDR(h);

    if (EXPECT(IS_FORWARDING_ADDR_OR_NIL(hhdr) || HBLK_IS_FREE(hhdr), FALSE)) {
      h = GC_next_used_block(h);
      if (h == 0) return(0);
      hhdr = GC_find_header((ptr_t)h);
    }
    GC_push_marked(h, hhdr);
    return(h + OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz));
}

#ifndef GC_DISABLE_INCREMENTAL
  /* Identical to above, but mark only from dirty pages   */
  STATIC struct hblk * GC_push_next_marked_dirty(struct hblk *h)
  {
    hdr * hhdr = HDR(h);

    if (!GC_dirty_maintained) ABORT("Dirty bits not set up");
    for (;;) {
        if (EXPECT(IS_FORWARDING_ADDR_OR_NIL(hhdr)
                   || HBLK_IS_FREE(hhdr), FALSE)) {
          h = GC_next_used_block(h);
          if (h == 0) return(0);
          hhdr = GC_find_header((ptr_t)h);
        }
#       ifdef STUBBORN_ALLOC
          if (hhdr -> hb_obj_kind == STUBBORN) {
            if (GC_page_was_changed(h) && GC_block_was_dirty(h, hhdr)) {
                break;
            }
          } else {
            if (GC_block_was_dirty(h, hhdr)) break;
          }
#       else
          if (GC_block_was_dirty(h, hhdr)) break;
#       endif
        h += OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz);
        hhdr = HDR(h);
    }
    GC_push_marked(h, hhdr);
    return(h + OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz));
  }
#endif /* !GC_DISABLE_INCREMENTAL */

/* Similar to above, but for uncollectible pages.  Needed since we      */
/* do not clear marks for such pages, even for full collections.        */
STATIC struct hblk * GC_push_next_marked_uncollectable(struct hblk *h)
{
    hdr * hhdr = HDR(h);

    for (;;) {
        if (EXPECT(IS_FORWARDING_ADDR_OR_NIL(hhdr)
                   || HBLK_IS_FREE(hhdr), FALSE)) {
          h = GC_next_used_block(h);
          if (h == 0) return(0);
          hhdr = GC_find_header((ptr_t)h);
        }
        if (hhdr -> hb_obj_kind == UNCOLLECTABLE) {
            GC_push_marked(h, hhdr);
            break;
        }
#       ifdef ENABLE_DISCLAIM
            if ((hhdr -> hb_flags & MARK_UNCONDITIONALLY) != 0) {
                GC_push_unconditionally(h, hhdr);
                break;
            }
#       endif
        h += OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz);
        hhdr = HDR(h);
    }
    return(h + OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz));
}
