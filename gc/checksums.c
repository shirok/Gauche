/*
 * Copyright (c) 1992-1994 by Xerox Corporation.  All rights reserved.
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

#ifdef CHECKSUMS

/* This is debugging code intended to verify the results of dirty bit   */
/* computations. Works only in a single threaded environment.           */
/* We assume that stubborn objects are changed only when they are       */
/* enabled for writing.  (Certain kinds of writing are actually         */
/* safe under other conditions.)                                        */
#define NSUMS 10000

#define OFFSET 0x10000

typedef struct {
        GC_bool new_valid;
        word old_sum;
        word new_sum;
        struct hblk * block;    /* Block to which this refers + OFFSET  */
                                /* to hide it from collector.           */
} page_entry;

page_entry GC_sums[NSUMS];

STATIC word GC_faulted[NSUMS] = { 0 };
                /* Record of pages on which we saw a write fault.       */

STATIC size_t GC_n_faulted = 0;

void GC_record_fault(struct hblk * h)
{
    word page = ROUNDUP_PAGESIZE((word)h);

    if (GC_n_faulted >= NSUMS) ABORT("write fault log overflowed");
    GC_faulted[GC_n_faulted++] = page;
}

STATIC GC_bool GC_was_faulted(struct hblk *h)
{
    size_t i;
    word page = ROUNDUP_PAGESIZE((word)h);

    for (i = 0; i < GC_n_faulted; ++i) {
        if (GC_faulted[i] == page) return TRUE;
    }
    return FALSE;
}

STATIC word GC_checksum(struct hblk *h)
{
    word *p = (word *)h;
    word *lim = (word *)(h+1);
    word result = 0;

    while ((word)p < (word)lim) {
        result += *p++;
    }
    return(result | 0x80000000 /* doesn't look like pointer */);
}

#ifdef STUBBORN_ALLOC
  /* Check whether a stubborn object from the given block appears on    */
  /* the appropriate free list.                                         */
  STATIC GC_bool GC_on_free_list(struct hblk *h)
  {
    hdr * hhdr = HDR(h);
    size_t sz = BYTES_TO_WORDS(hhdr -> hb_sz);
    ptr_t p;

    if (sz > MAXOBJWORDS) return(FALSE);
    for (p = GC_sobjfreelist[sz]; p != 0; p = obj_link(p)) {
        if (HBLKPTR(p) == h) return(TRUE);
    }
    return(FALSE);
  }
#endif

int GC_n_dirty_errors = 0;
int GC_n_faulted_dirty_errors = 0;
int GC_n_changed_errors = 0;
int GC_n_clean = 0;
int GC_n_dirty = 0;

STATIC void GC_update_check_page(struct hblk *h, int index)
{
    page_entry *pe = GC_sums + index;
    hdr * hhdr = HDR(h);
    struct hblk *b;

    if (pe -> block != 0 && pe -> block != h + OFFSET) ABORT("goofed");
    pe -> old_sum = pe -> new_sum;
    pe -> new_sum = GC_checksum(h);
#   if !defined(MSWIN32) && !defined(MSWINCE)
        if (pe -> new_sum != 0x80000000 && !GC_page_was_ever_dirty(h)) {
            GC_err_printf("GC_page_was_ever_dirty(%p) is wrong\n", (void *)h);
        }
#   endif
    if (GC_page_was_dirty(h)) {
        GC_n_dirty++;
    } else {
        GC_n_clean++;
    }
    b = h;
    while (IS_FORWARDING_ADDR_OR_NIL(hhdr) && hhdr != 0) {
        b -= (word)hhdr;
        hhdr = HDR(b);
    }
    if (pe -> new_valid
        && hhdr != 0 && hhdr -> hb_descr != 0 /* may contain pointers */
        && pe -> old_sum != pe -> new_sum) {
        if (!GC_page_was_dirty(h) || !GC_page_was_ever_dirty(h)) {
            GC_bool was_faulted = GC_was_faulted(h);
            /* Set breakpoint here */GC_n_dirty_errors++;
            if (was_faulted) GC_n_faulted_dirty_errors++;
        }
#       ifdef STUBBORN_ALLOC
          if (!HBLK_IS_FREE(hhdr)
            && hhdr -> hb_obj_kind == STUBBORN
            && !GC_page_was_changed(h)
            && !GC_on_free_list(h)) {
            /* if GC_on_free_list(h) then reclaim may have touched it   */
            /* without any allocations taking place.                    */
            /* Set breakpoint here */GC_n_changed_errors++;
          }
#       endif
    }
    pe -> new_valid = TRUE;
    pe -> block = h + OFFSET;
}

word GC_bytes_in_used_blocks = 0;

STATIC void GC_add_block(struct hblk *h, word dummy GC_ATTR_UNUSED)
{
   hdr * hhdr = HDR(h);
   size_t bytes = hhdr -> hb_sz;

   bytes += HBLKSIZE-1;
   bytes &= ~(HBLKSIZE-1);
   GC_bytes_in_used_blocks += bytes;
}

STATIC void GC_check_blocks(void)
{
    word bytes_in_free_blocks = GC_large_free_bytes;

    GC_bytes_in_used_blocks = 0;
    GC_apply_to_all_blocks(GC_add_block, (word)0);
    GC_COND_LOG_PRINTF("GC_bytes_in_used_blocks = %lu,"
                       " bytes_in_free_blocks = %lu, heapsize = %lu\n",
                       (unsigned long)GC_bytes_in_used_blocks,
                       (unsigned long)bytes_in_free_blocks,
                       (unsigned long)GC_heapsize);
    if (GC_bytes_in_used_blocks + bytes_in_free_blocks != GC_heapsize) {
        GC_err_printf("LOST SOME BLOCKS!!\n");
    }
}

/* Should be called immediately after GC_read_dirty and GC_read_changed. */
void GC_check_dirty(void)
{
    int index;
    unsigned i;
    struct hblk *h;
    ptr_t start;

    GC_check_blocks();

    GC_n_dirty_errors = 0;
    GC_n_faulted_dirty_errors = 0;
    GC_n_changed_errors = 0;
    GC_n_clean = 0;
    GC_n_dirty = 0;

    index = 0;
    for (i = 0; i < GC_n_heap_sects; i++) {
        start = GC_heap_sects[i].hs_start;
        for (h = (struct hblk *)start;
             (word)h < (word)(start + GC_heap_sects[i].hs_bytes); h++) {
             GC_update_check_page(h, index);
             index++;
             if (index >= NSUMS) goto out;
        }
    }
out:
    GC_COND_LOG_PRINTF("Checked %lu clean and %lu dirty pages\n",
                       (unsigned long)GC_n_clean, (unsigned long)GC_n_dirty);
    if (GC_n_dirty_errors > 0) {
        GC_err_printf("Found %d dirty bit errors (%d were faulted)\n",
                      GC_n_dirty_errors, GC_n_faulted_dirty_errors);
    }
    if (GC_n_changed_errors > 0) {
        GC_err_printf("Found %lu changed bit errors\n",
                      (unsigned long)GC_n_changed_errors);
        GC_err_printf(
                "These may be benign (provoked by nonpointer changes)\n");
#       ifdef THREADS
          GC_err_printf(
            "Also expect 1 per thread currently allocating a stubborn obj\n");
#       endif
    }
    for (i = 0; i < GC_n_faulted; ++i) {
        GC_faulted[i] = 0; /* Don't expose block pointers to GC */
    }
    GC_n_faulted = 0;
}

#endif /* CHECKSUMS */
