/*
 * bench-arith.c - microbenchmark for gauche/priv/arith.h macros
 *
 * Compares the portable C implementations of UADD/UMUL/... against the
 * arch-specific inline-asm versions.
 *
 * For indivudual building (from src/):
 *     cc -O2 -I. bench-arith.c -o bench-arith-native
 *     cc -O2 -I. -DBENCH_PORTABLE bench-arith.c -o bench-arith-portable
 *
 * Or run bench-arith.scm to build both & compare.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <limits.h>
#include <time.h>

/* arith.h needs SIZEOF_LONG at include time. */
#if defined(__SIZEOF_LONG__)
# define SIZEOF_LONG __SIZEOF_LONG__
#else
# error "cannot determine SIZEOF_LONG"
#endif

/* Select the arch header by defining SCM_TARGET_* before including arith.h.
   Define BENCH_PORTABLE at compile time to force the portable path. */
#if !defined(BENCH_PORTABLE)
# if defined(__x86_64__) && SIZEOF_LONG == 8
#  define SCM_TARGET_X86_64 1
# elif defined(__aarch64__) && SIZEOF_LONG == 8
#  define SCM_TARGET_AARCH64 1
# elif defined(__i386__)
#  define SCM_TARGET_I386 1
# endif
#endif

#include "gauche/priv/arith.h"

#if defined(SCM_TARGET_X86_64)
static const char *VARIANT = "native/x86_64";
#elif defined(SCM_TARGET_AARCH64)
static const char *VARIANT = "native/aarch64";
#elif defined(SCM_TARGET_I386)
static const char *VARIANT = "native/i386";
#else
static const char *VARIANT = "portable";
#endif

#ifndef ITER
# define ITER 100000000UL   /* 100M iterations per bench */
#endif
#define REPEAT 5            /* keep best of REPEAT runs */

static double now_sec(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + (double)ts.tv_nsec * 1e-9;
}

/*=============================================================
 * Benchmark each macro
 *
 * Each iteration depends on previous iteration, to prevent optimizer
 * from parallelization.
 *
 * *chk receives a fingerprint of the loop's final state (r and v).
 * It is used to detect any divergence between portable version and asm version.
 */

static double bench_uadd(u_long *chk) {
    u_long r = 1, c = 0, x = 0x9E3779B97F4A7C15UL, y = 0xBB67AE8584CAA73BUL;
    double t0 = now_sec();
    for (u_long i = 0; i < ITER; i++) {
        UADD(r, c, x, y);
        x = r; y ^= i;
    }
    double t1 = now_sec();
    *chk = r ^ c;
    return t1 - t0;
}

static double bench_uaddov(u_long *chk) {
    u_long r = 1, x = 0x9E3779B9UL, y = 0xBB67AE85UL;
    u_long v = 0;
    double t0 = now_sec();
    for (u_long i = 0; i < ITER; i++) {
        UADDOV(r, v, x, y);
        x = r; y ^= i + v;
    }
    double t1 = now_sec();
    *chk = r ^ v;
    return t1 - t0;
}

static double bench_saddov(u_long *chk) {
    long r = 1, x = 0x9E3779B9L, y = -0xBB67AE85L;
    long v = 0;
    double t0 = now_sec();
    for (u_long i = 0; i < ITER; i++) {
        SADDOV(r, v, x, y);
        x = r; y ^= (long)i + v;
    }
    double t1 = now_sec();
    *chk = (u_long)r ^ (u_long)v;
    return t1 - t0;
}

static double bench_usub(u_long *chk) {
    u_long r = 0, c = 0, x = 0xBB67AE8584CAA73BUL, y = 0x9E3779B97F4A7C15UL;
    double t0 = now_sec();
    for (u_long i = 0; i < ITER; i++) {
        USUB(r, c, x, y);
        x = r; y ^= i;
    }
    double t1 = now_sec();
    *chk = r ^ c;
    return t1 - t0;
}

static double bench_usubov(u_long *chk) {
    u_long r = 0, x = 0xBB67AE85UL, y = 0x9E3779B9UL;
    u_long v = 0;
    double t0 = now_sec();
    for (u_long i = 0; i < ITER; i++) {
        USUBOV(r, v, x, y);
        x = r; y ^= i + v;
    }
    double t1 = now_sec();
    *chk = r ^ v;
    return t1 - t0;
}

static double bench_ssubov(u_long *chk) {
    long r = 0, x = 0x7FFFFFFFL, y = -0x40000000L;
    long v = 0;
    double t0 = now_sec();
    for (u_long i = 0; i < ITER; i++) {
        SSUBOV(r, v, x, y);
        x = r; y ^= (long)i + v;
    }
    double t1 = now_sec();
    *chk = (u_long)r ^ (u_long)v;
    return t1 - t0;
}

static double bench_umul(u_long *chk) {
    u_long hi = 0, lo = 1, x = 0x9E3779B9UL, y = 0xBB67AE85UL;
    double t0 = now_sec();
    for (u_long i = 0; i < ITER; i++) {
        UMUL(hi, lo, x, y);
        x = lo ^ i;
        y ^= hi;
    }
    double t1 = now_sec();
    *chk = hi ^ lo;
    return t1 - t0;
}

static double bench_umulov(u_long *chk) {
    u_long r = 1, x = 0x9E3779B9UL, y = 0xBB67AE85UL;
    u_long v = 0;
    double t0 = now_sec();
    for (u_long i = 0; i < ITER; i++) {
        UMULOV(r, v, x, y);
        x = r ^ i;
        y ^= v;
    }
    double t1 = now_sec();
    *chk = r ^ v;
    return t1 - t0;
}

static double bench_smulov(u_long *chk) {
    /* NB: the portable SMULOV leaves r untouched on overflow, while the
       asm variants write the wrapped product.  To catch the bugs with the
       checksum , we bound x and y so x*y always fits in long. */
    long r = 1, v = 0;
    long x = 0x12345L, y = 0x6789L;
    double t0 = now_sec();
    for (u_long i = 0; i < ITER; i++) {
        SMULOV(r, v, x, y);
        x = (long)((u_long)r & 0xFFFF);
        y = (long)(i & 0xFFFF) ^ v;
    }
    double t1 = now_sec();
    *chk = (u_long)r ^ (u_long)v;
    return t1 - t0;
}

/*=============================================================
 * Driver
 */

typedef double (*bench_fn)(u_long *chk);

struct entry {
    const char *name;
    bench_fn    fn;
};

/* Run the benchmark REPEAT times, returning the smallest elapsed time.
   *chk gets the fingerprint from the first run. */
static double best_of(bench_fn fn, u_long *chk) {
    double best = 1e9;
    u_long first_chk = 0;
    for (int i = 0; i < REPEAT; i++) {
        u_long this_chk;
        double t = fn(&this_chk);
        if (i == 0) first_chk = this_chk;
        if (t < best) best = t;
    }
    *chk = first_chk;
    return best;
}

int main(int argc, char **argv) {
    (void)argc; (void)argv;

    struct entry benches[] = {
        { "UADD",   bench_uadd   },
        { "UADDOV", bench_uaddov },
        { "SADDOV", bench_saddov },
        { "USUB",   bench_usub   },
        { "USUBOV", bench_usubov },
        { "SSUBOV", bench_ssubov },
        { "UMUL",   bench_umul   },
        { "UMULOV", bench_umulov },
        { "SMULOV", bench_smulov },
    };
    size_t n = sizeof(benches) / sizeof(benches[0]);

    double  ns_per_op[sizeof benches / sizeof benches[0]];
    u_long  checksums[sizeof benches / sizeof benches[0]];
    for (size_t i = 0; i < n; i++) {
        double t = best_of(benches[i].fn, &checksums[i]);
        ns_per_op[i] = t * 1e9 / (double)ITER;
    }

    /* Output as an alist of metadata plus two nested alists:
         (ns-per-op (OP . NS) ...)   ns per op, best-of-REPEAT
         (checksum  (OP . #x...) ...) fingerprint of final r,v
       The checksum lets the driver flag any macro whose native asm
       diverges from the portable implementation. */
    printf("((variant . \"%s\")\n", VARIANT);
    printf(" (iter . %lu)\n", (unsigned long)ITER);
    printf(" (repeat . %d)\n", REPEAT);
    printf(" (ns-per-op\n");
    for (size_t i = 0; i < n; i++) {
        printf("  (%s . %.4f)%s\n",
               benches[i].name, ns_per_op[i],
               (i + 1 == n) ? ")" : "");
    }
    printf(" (checksum\n");
    for (size_t i = 0; i < n; i++) {
        printf("  (%s . #x%016lx)%s\n",
               benches[i].name, checksums[i],
               (i + 1 == n) ? "))" : "");
    }
    return 0;
}
