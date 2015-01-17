/*
 * Test the lowest-level numeric routines.
 */

#include "gauche.h"
#include "gauche/priv/arith.h"
#include "gauche/scmconst.h"

#define UMAX SCM_ULONG_MAX
#define SMAX LONG_MAX
#define SMIN LONG_MIN

int errcount = 0;

void message(FILE *out, const char *m, int filler)
{
    int i;
    fprintf(out, "%s", m);
    if (filler) {
        int len = 79 - (int)strlen(m);
        if (len < 0) len = 5;
        for (i=0; i<len; i++) putc(filler, out);
    }
    putc('\n', out);
}

/*=============================================================
 * Testing macros in gauche/priv/arith.h
 */

#define TEST_SECTION(name) message(stdout, "<" name ">", '-')

#define TEST5(x_, y_, c_, rexp, cexp, numfmt, opmsg, op)        \
    do {                                                        \
        printf("testing " numfmt opmsg numfmt " c=" numfmt      \
               " expects r=" numfmt ", c=" numfmt " =>",        \
               x_, y_, c_, rexp, cexp);                         \
        c = c_;                                                 \
        x = x_;                                                 \
        y = y_;                                                 \
        op(r, c, x, y);                                         \
        if (r == rexp && c == cexp) {                           \
            printf("ok\n");                                     \
        } else {                                                \
            errcount++;                                         \
            printf("ERROR: got r=" numfmt                       \
                   ", c=" numfmt "\n", r, c);                   \
        }                                                       \
    } while (0)

#define TESTOV(x_, y_, rexp, cexp, numfmt, opmsg, op)           \
    do {                                                        \
        x = x_;                                                 \
        y = y_;                                                 \
        if (cexp)                                               \
            printf("testing " numfmt opmsg numfmt               \
                   " expects overflow =>", x_, y_);             \
        else                                                    \
            printf("testing " numfmt opmsg numfmt               \
                   " expects " numfmt " =>", x_, y_, rexp);     \
        op(r, c, x, y);                                         \
        if (c) {                                                \
            if (cexp == c) printf("ok\n");                      \
            else {                                              \
                errcount++;                                     \
                printf("ERROR: got r=" numfmt                   \
                       ", c=" numfmt "\n", r, c);               \
            }                                                   \
        } else {                                                \
            if (r == rexp) printf("ok\n");                      \
            else {                                              \
                errcount++;                                     \
                printf("ERROR: got r=" numfmt "\n", r);         \
            }                                                   \
        }                                                       \
    } while (0)

/*
 * UADD
 */
#define TEST_UADD(x_, y_, c_, rexp, cexp)               \
  TEST5(x_, y_, c_, rexp, cexp, "%lu", "+", UADD)

void test_uadd(void)
{
    u_long r, c, x, y;
    TEST_SECTION("UADD");

    /* MAX + 0 + 0 => [MAX, 0] */
    TEST_UADD(UMAX, 0UL, 0UL, UMAX, 0UL);
    /* MAX + 1 + 0 => [0, 1]   */
    TEST_UADD(UMAX, 1UL, 0UL, 0UL, 1UL);
    /* MAX + 2 + 0 => [1, 1]   */
    TEST_UADD(UMAX, 2UL, 0UL, 1UL, 1UL);
    /* MAX + MAX + 0 => [MAX-1, 1] */
    TEST_UADD(UMAX, UMAX, 0UL, UMAX-1, 1UL);
    /* MAX/2 + MAX/2 + 0 => [MAX-1, 0] */
    TEST_UADD(UMAX/2, UMAX/2, 0UL, UMAX-1, 0UL);
    /* MAX/2 + MAX + 0 => [MAX/2-1, 1] */
    TEST_UADD(UMAX/2, UMAX, 0UL, UMAX/2-1, 1UL);
    /* MAX-1 + 0 + 1 => [MAX, 0] */
    TEST_UADD(UMAX-1, 0UL, 1UL, UMAX, 0UL);
    /* MAX + 0 + 1 => [0, 1] */
    TEST_UADD(UMAX, 0UL, 1UL, 0UL, 1UL);
    /* MAX + 1 + 1 => [1, 1] */
    TEST_UADD(UMAX, 1UL, 1UL, 1UL, 1UL);
    /* MAX + MAX + 1 => [MAX, 1] */
    TEST_UADD(UMAX, UMAX, 1UL, UMAX, 1UL);
    /* MAX/2 + MAX/2 + 1 => [MAX, 0] */
    TEST_UADD(UMAX/2, UMAX/2, 1UL, UMAX, 0UL);
    /* MAX/2 + MAX + 1 => [MAX/2, 1] */
    TEST_UADD(UMAX/2, UMAX, 1UL, UMAX/2, 1UL);
}

/*
 * UADDOV
 */
#define TEST_UADDOV(x_, y_, rexp, cexp)         \
    TESTOV(x_, y_, rexp, cexp, "%lu", "+", UADDOV)

void test_uaddov(void)
{
    u_long r, c, x, y;
    TEST_SECTION("UADDOV");

    /* MAX + 0 => MAX */
    TEST_UADDOV(UMAX, 0UL, UMAX, 0UL);
    /* MAX + 1 => overflow */
    TEST_UADDOV(UMAX, 1UL, 0UL, 1UL);
    /* MAX + 2 => overflow */
    TEST_UADDOV(UMAX, 2UL, 0UL, 1UL);
    /* MAX + MAX => overflow */
    TEST_UADDOV(UMAX, UMAX, 0UL, 1UL);
    /* MAX/2 + MAX/2 => MAX-1, 0 */
    TEST_UADDOV(UMAX/2, UMAX/2, UMAX-1, 0UL);
    /* MAX/2 + MAX => overflow */
    TEST_UADDOV(UMAX/2, UMAX, 0UL, 1UL);
    /* MAX-1 + 1 => MAX */
    TEST_UADDOV(UMAX-1, 1UL, UMAX, 0UL);
    /* MAX-1 + 2 => overflow */
    TEST_UADDOV(UMAX-1, 2UL, 0UL, 1UL);
}

/*
 * SADDOV
 */
#define TEST_SADDOV(x_, y_, rexp, cexp)         \
    TESTOV(x_, y_, rexp, cexp, "%ld", "+", SADDOV)

void test_saddov(void)
{
    long r, c, x, y;
    TEST_SECTION("SADDOV");

    /* 1 + 1 => 2 */
    TEST_SADDOV(1L, 1L, 2L, 0L);
    /* -1 + -1 => -2 */
    TEST_SADDOV(-1L, -1L, -2L, 0L);
    /* SMAX + 0 => SMAX */
    TEST_SADDOV(SMAX, 0L, SMAX, 0L);
    /* SMAX + 1 => overflow */
    TEST_SADDOV(SMAX, 1L, 0L, 1L);
    /* SMAX + 2 => overflow */
    TEST_SADDOV(SMAX, 2L, 0L, 1L);
    /* SMAX + -1 => SMAX-1 */
    TEST_SADDOV(SMAX, -1L, SMAX-1, 0L);
    /* SMAX + -SMAX => 0 */
    TEST_SADDOV(SMAX, -SMAX, 0L, 0L);
    /* SMAX + -SMAX-1 => -1 */
    TEST_SADDOV(SMAX, -SMAX-1, -1L, 0L);
    /* SMAX/2 + SMAX/2 => SMAX-1 */
    TEST_SADDOV(SMAX/2, SMAX/2, SMAX-1, 0L);
    /* SMAX/2 + -SMAX/2 => 0 */
    TEST_SADDOV(SMAX/2, -(SMAX/2), 0L, 0L);
    /* -SMAX/2 + -SMAX/2 => -(SMAX-1) */
    TEST_SADDOV(-(SMAX/2), -(SMAX/2), -(SMAX-1), 0L);
    /* 0 + -SMAX => -SMAX */
    TEST_SADDOV(0L, -SMAX, -SMAX, 0L);
    /* -1 + -SMAX => SMIN */
    TEST_SADDOV(-1L, -SMAX, SMIN, 0L);
    /* -2 + -SMAX => -overflow */
    TEST_SADDOV(-2L, -SMAX, 0L, -1L);
    /* -SMAX + -2 => -overflow */
    TEST_SADDOV(-SMAX, -2L, 0L, -1L);
    /* -SMAX + -SMAX => -overflow */
    TEST_SADDOV(-SMAX, -SMAX, 0L, -1L);
    /* -SMAX + SMIN => -overflow */
    TEST_SADDOV(-SMAX, SMIN, 0L, -1L);
}

/*
 * USUB
 */
#define TEST_USUB(x_, y_, c_, rexp, cexp)               \
    TEST5(x_, y_, c_, rexp, cexp, "%lu", "-", USUB)

void test_usub(void)
{
    u_long r, c, x, y;
    TEST_SECTION("USUB");

    /* MAX - 0 - 0 => [MAX, 0] */
    TEST_USUB(UMAX, 0UL, 0UL, UMAX, 0UL);
    /* MAX - 1 - 0 => [MAX-1, 0] */
    TEST_USUB(UMAX, 1UL, 0UL, UMAX-1, 0UL);
    /* 0 - MAX - 0 => [1, 1] */
    TEST_USUB(0UL, UMAX, 0UL, 1UL, 1UL);
    /* 1 - MAX - 0 => [2, 1] */
    TEST_USUB(1UL, UMAX, 0UL, 2UL, 1UL);
    /* MAX - MAX/2 - 0 => [MAX/2+1, 0] */
    TEST_USUB(UMAX, UMAX/2, 0UL, UMAX/2+1, 0UL);
    /* MAX/2 - MAX - 0 => [MAX/2+1, 1] */
    TEST_USUB(UMAX/2, UMAX, 0UL, UMAX/2+1, 1UL);
    /* MAX - 0 - 1 => [MAX-1, 0] */
    TEST_USUB(UMAX, 0UL, 1UL, UMAX-1, 0UL);
    /* MAX - 1 - 1 => [MAX-2, 0] */
    TEST_USUB(UMAX, 1UL, 1UL, UMAX-2, 0UL);
    /* 0 - MAX - 1 => [0, 1] */
    TEST_USUB(0UL, UMAX, 1UL, 0UL, 1UL);
    /* 1 - MAX - 1 => [1, 1] */
    TEST_USUB(1UL, UMAX, 1UL, 1UL, 1UL);
    /* MAX - MAX/2 - 1 => [MAX/2, 0] */
    TEST_USUB(UMAX, UMAX/2, 1UL, UMAX/2, 0UL);
    /* MAX/2 - MAX - 1 => [MAX/2, 1] */
    TEST_USUB(UMAX/2, UMAX, 1UL, UMAX/2, 1UL);
}

/*
 * USUBOV
 */
#define TEST_USUBOV(x_, y_, rexp, cexp)         \
    TESTOV(x_, y_, rexp, cexp, "%lu", "-", USUBOV)

void test_usubov(void)
{
    u_long r, c, x, y;
    TEST_SECTION("USUBOV");

    /* MAX - 0  => MAX */
    TEST_USUBOV(UMAX, 0UL, UMAX, 0UL);
    /* MAX - 1  => MAX-1 */
    TEST_USUBOV(UMAX, 1UL, UMAX-1, 0UL);
    /* MAX - MAX => 0 */
    TEST_USUBOV(UMAX, UMAX, 0UL, 0UL);
    /* MAX-1 - MAX => overflow */
    TEST_USUBOV(UMAX-1, UMAX, 0UL, 1UL);
    /* MAX - MAX-1 => 1 */
    TEST_USUBOV(UMAX, UMAX-1, 1UL, 0UL);
    /* 0 - 1 => overflow */
    TEST_USUBOV(0UL, 1UL, 0UL, 1UL);
    /* 0 - MAX => overflow */
    TEST_USUBOV(0UL, UMAX, 0UL, 1UL);
}

/*
 * SSUBOV
 */
#define TEST_SSUBOV(x_, y_, rexp, cexp)         \
    TESTOV(x_, y_, rexp, cexp, "%ld", "-", SSUBOV)

void test_ssubov(void)
{
    long r, c, x, y;
    TEST_SECTION("SSUBOV");

    /* 1 - 1 => 0 */
    TEST_SSUBOV(1L, 1L, 0L, 0L);
    /* -1 - -1 => 0 */
    TEST_SSUBOV(-1L, -1L, 0L, 0L);
    /* SMAX - 0 => SMAX */
    TEST_SSUBOV(SMAX, 0L, SMAX, 0L);
    /* SMAX - 1 => SMAX-1 */
    TEST_SSUBOV(SMAX, 1L, SMAX-1, 0L);
    /* SMAX - -1 => overflow */
    TEST_SSUBOV(SMAX, -1L, 0L, 1L);
    /* SMAX - -2 => overflow */
    TEST_SSUBOV(SMAX, -2L, 0L, 1L);
    /* SMAX - -SMAX => overflow */
    TEST_SSUBOV(SMAX, -SMAX, 0L, 1L);
    /* SMAX - SMAX => 0 */
    TEST_SSUBOV(SMAX, SMAX, 0L, 0L);
    /* SMAX/2 - -SMAX/2 => SMAX-1 */
    TEST_SSUBOV(SMAX/2, -(SMAX/2), SMAX-1, 0L);
    /* SMAX/2 - SMAX/2 => 0 */
    TEST_SSUBOV(SMAX/2, SMAX/2, 0L, 0L);
    /* -SMAX/2 - SMAX/2 => 0 */
    TEST_SSUBOV(-(SMAX/2), SMAX/2, -(SMAX-1), 0L);
    /* 0 - -SMAX => SMAX */
    TEST_SSUBOV(0L, -SMAX, SMAX, 0L);
    /* 1 - -SMAX => overflow */
    TEST_SSUBOV(1L, -SMAX, 0L, 1L);
    /* 0 - -SMAX => SMAX */
    TEST_SSUBOV(0L, -SMAX, SMAX, 0L);
    /* SMIN - 1 => -overflow */
    TEST_SSUBOV(SMIN, 1L, 0L, -1L);
    /* SMIN - -1 => SMIN+1 */
    TEST_SSUBOV(SMIN, -1L, SMIN+1, 0L);
    /* -SMAX - -SMAX => 0 */
    TEST_SSUBOV(-SMAX, -SMAX, 0L, 0L);
    /* -SMAX - SMIN => 1 */
    TEST_SSUBOV(-SMAX, SMIN, 1L, 0L);
}

/*
 * UMUL
 */
#define TEST_UMUL(x_, y_, hiexp, loexp)                                 \
    do {                                                                \
        printf("testing %lu*%lu expects hi=%lu, lo=%lu =>", x_, y_,     \
               hiexp, loexp);                                           \
        x = x_;                                                         \
        y = y_;                                                         \
        UMUL(hi, lo, x, y);                                             \
        if (hi == hiexp && lo == loexp) {                               \
            printf("ok\n");                                             \
        } else {                                                        \
            errcount++;                                                 \
            printf("ERROR: got hi=%lu, lo=%lu\n", hi, lo);              \
        }                                                               \
    } while (0)

void test_umul(void)
{
    u_long hi, lo, x, y;
    TEST_SECTION("UMUL");

    /* MAX * MAX => [MAX-1, 1] */
    TEST_UMUL(UMAX, UMAX, UMAX-1, 1UL);
    /* MAX-1 * MAX-1 => [MAX-3, 4] */
    TEST_UMUL(UMAX-1, UMAX-1, UMAX-3, 4UL);
    /* MAX/2 * 2 => [0, MAX-1] */
    TEST_UMUL(UMAX/2, 2UL, 0UL, UMAX-1);
    /* MAX/2+1 * 2 => [1, 0] */
    TEST_UMUL(UMAX/2+1, 2UL, 1UL, 0UL);
}

/*
 * UMULOV
 */
#define TEST_UMULOV(x_, y_, rexp, cexp)          \
    TESTOV(x_, y_, rexp, cexp, "%lu", "*", UMULOV)

void test_umulov(void)
{
    u_long r, c, x, y;
    TEST_SECTION("UMULOV");

    /* 0 * 1 => 0 */
    TEST_UMULOV(0UL, 1UL, 0UL, 0UL);
    /* 1 * 0 => 0 */
    TEST_UMULOV(1UL, 0UL, 0UL, 0UL);
    /* 1 * 1 => 1 */
    TEST_UMULOV(1UL, 1UL, 1UL, 0UL);
    /* 1 * UMAX => UMAX */
    TEST_UMULOV(1UL, UMAX, UMAX, 0UL);
    /* 2 * UMAX => overflow */
    TEST_UMULOV(2UL, UMAX, 0UL, 1UL);
    /* UMAX/2 * 2 => UMAX-1 */
    TEST_UMULOV(UMAX/2, 2UL, UMAX-1, 0UL);
    /* UMAX/2 * 3 => overflow */
    TEST_UMULOV(UMAX/2, 3UL, 0UL, 1UL);
    /* UMAX>>4 * UMAX>>4 => overflow */
    TEST_UMULOV((UMAX>>4), (UMAX>>4), 0UL, 1UL);
    /* UMAX>>8 * UMAX>>8 => overflow */
    TEST_UMULOV((UMAX>>8), (UMAX>>8), 0UL, 1UL);
    /* UMAX * UMAX => overflow */
    TEST_UMULOV(UMAX, UMAX, 0UL, 1UL);
    /* UMAX-1 * UMAX-1 => overflow */
    TEST_UMULOV(UMAX-1, UMAX-1, 0UL, 1UL);
}

/*
 * SMULOV
 */
#define TEST_SMULOV(x_, y_, rexp, cexp)          \
    TESTOV(x_, y_, rexp, cexp, "%ld", "*", SMULOV)

void test_smulov(void)
{
    long r, c, x, y;
    TEST_SECTION("SMULOV");

    /* 0 * 1 => 0 */
    TEST_SMULOV(0L, 1L, 0L, 0L);
    /* 1 * 0 => 0 */
    TEST_SMULOV(1L, 0L, 0L, 0L);
    /* 1 * 1 => 1 */
    TEST_SMULOV(1L, 1L, 1L, 0L);
    /* 1 * -1 => -1 */
    TEST_SMULOV(1L, -1L, -1L, 0L);
    /* -1 * 1 => -1 */
    TEST_SMULOV(-1L, 1L, -1L, 0L);
    /* -1 * -1 => 1 */
    TEST_SMULOV(-1L, -1L, 1L, 0L);
    /* 1 * SMAX => SMAX */
    TEST_SMULOV(1L, SMAX, SMAX, 0L);
    /* 1 * -SMAX => -SMAX */
    TEST_SMULOV(1L, -SMAX, -SMAX, 0L);
    /* -1 * SMAX => -SMAX */
    TEST_SMULOV(-1L, SMAX, -SMAX, 0L);
    /* -1 * -SMAX => SMAX */
    TEST_SMULOV(-1L, -SMAX, SMAX, 0L);
    /* 1 * SMIN => SMIN */
    TEST_SMULOV(1L, SMIN, SMIN, 0L);
    /* -1 * SMIN => overflow */
    TEST_SMULOV(-1L, SMIN, 0L, 1L);
    /* 2 * SMAX => overflow */
    TEST_SMULOV(2L, SMAX, 0L, 1L);
    /* 2 * -SMAX => -overflow */
    TEST_SMULOV(2L, -SMAX, 0L, -1L);
    /* -2 * SMAX => -overflow */
    TEST_SMULOV(-2L, SMAX, 0L, -1L);
    /* -2 * -SMAX => overflow */
    TEST_SMULOV(-2L, -SMAX, 0L, 1L);
    /* SMAX/2 * 2 => SMAX-1 */
    TEST_SMULOV(SMAX/2, 2L, SMAX-1, 0L);
    /* SMAX/2 * -2 => -(SMAX-1) */
    TEST_SMULOV(SMAX/2, -2L, -(SMAX-1), 0L);
    /* -SMAX/2 * 2 => -(SMAX-1) */
    TEST_SMULOV(-(SMAX/2), 2L, -(SMAX-1), 0L);
    /* -SMAX/2 * -2 => SMAX-1 */
    TEST_SMULOV(-(SMAX/2), -2L, SMAX-1, 0L);
    /* SMAX/2+1 * 2 => overflow */
    TEST_SMULOV(SMAX/2+1, 2L, 0L, 1L);
    /* SMAX/2+1 * -2 => SMIN */
    TEST_SMULOV(SMAX/2+1, -2L, SMIN, 0L);
    /* -(SMAX/2+1) * 2 => SMIN */
    TEST_SMULOV(-(SMAX/2+1), 2L, SMIN, 0L);
    /* -(SMAX/2+1) * -2 => overflow */
    TEST_SMULOV(-(SMAX/2+1), -2L, 0L, 1L);
    /* SMAX>>4 * SMAX>>4 => overflow */
    TEST_SMULOV((SMAX>>4), (SMAX>>4), 0L, 1L);
    /* SMAX>>4 * -SMAX>>4 => -overflow */
    TEST_SMULOV((SMAX>>4), -(SMAX>>4), 0L, -1L);
    /* -SMAX>>4 * SMAX>>4 => -overflow */
    TEST_SMULOV(-(SMAX>>4), (SMAX>>4), 0L, -1L);
    /* -SMAX>>4 * -SMAX>>4 => overflow */
    TEST_SMULOV(-(SMAX>>4), -(SMAX>>4), 0L, 1L);
    /* SMAX>>8 * SMAX>>8 => overflow */
    TEST_SMULOV((SMAX>>8), (SMAX>>8), 0L, 1L);
    /* SMAX>>8 * -SMAX>>8 => -overflow */
    TEST_SMULOV((SMAX>>8), -(SMAX>>8), 0L, -1L);
    /* -SMAX>>8 * SMAX>>8 => -overflow */
    TEST_SMULOV(-(SMAX>>8), (SMAX>>8), 0L, -1L);
    /* -SMAX>>8 * -SMAX>>8 => overflow */
    TEST_SMULOV(-(SMAX>>8), -(SMAX>>8), 0L, 1L);
    /* SMAX * SMAX => overflow */
    TEST_SMULOV(SMAX, SMAX, 0L, 1L);
    /* SMAX * -SMAX => -overflow */
    TEST_SMULOV(SMAX, -SMAX, 0L, -1L);
    /* -SMAX * SMAX => -overflow */
    TEST_SMULOV(-SMAX, SMAX, 0L, -1L);
    /* -SMAX * -SMAX => overflow */
    TEST_SMULOV(-SMAX, -SMAX, 0L, 1L);
}

/*=============================================================
 * Testing 32/64-bit conversion routines
 */

void test_scm_c_scm(const char *msg, ScmObj expect, ScmObj val)
{
    Scm_Printf(SCM_CUROUT, "testing %s, expects %S =>", msg, expect);
    if (Scm_EqualP(expect, val)) {
        Scm_Printf(SCM_CUROUT, "ok\n");
    } else {
        Scm_Printf(SCM_CUROUT, "ERROR: got %S\n", val);
        errcount++;
    }
}

void test_true(const char *msg, int val)
{
    Scm_Printf(SCM_CUROUT, "testing %s, expects TRUE =>", msg);
    if (val) {
        Scm_Printf(SCM_CUROUT, "ok\n");
    } else {
        Scm_Printf(SCM_CUROUT, "ERROR: got %d\n", val);
        errcount++;
    }
}



void test_32_64(void)
{
    ScmObj vv;
    int oor;

    TEST_SECTION("integer conversions, non clamping");

    vv = Scm_Add(SCM_2_31, SCM_MAKE_INT(-1));
    test_scm_c_scm("long roundtrip 2^31-1", vv,
                   Scm_MakeInteger(Scm_GetInteger(vv)));
    vv = Scm_Add(SCM_2_31, SCM_MAKE_INT(-3));
    test_scm_c_scm("long roundtrip 2^31-3", vv,
                   Scm_MakeInteger(Scm_GetInteger(vv)));
    vv = Scm_Negate(SCM_2_31);
    test_scm_c_scm("long roundtrip -2^31", vv,
                   Scm_MakeInteger(Scm_GetInteger(vv)));
    vv = Scm_Add(Scm_Negate(SCM_2_31), SCM_MAKE_INT(2));
    test_scm_c_scm("long roundtrip -2^31+2", vv,
                   Scm_MakeInteger(Scm_GetInteger(vv)));
#if SIZEOF_LONG >= 8
    vv = Scm_Add(SCM_2_63, SCM_MAKE_INT(-1));
    test_scm_c_scm("long roundtrip 2^63-1", vv,
                   Scm_MakeInteger(Scm_GetInteger(vv)));
    vv = Scm_Add(SCM_2_63, SCM_MAKE_INT(-3));
    test_scm_c_scm("long roundtrip 2^63-3", vv,
                   Scm_MakeInteger(Scm_GetInteger(vv)));
    vv = Scm_Negate(SCM_2_63);
    test_scm_c_scm("long roundtrip -2^63", vv,
                   Scm_MakeInteger(Scm_GetInteger(vv)));
    vv = Scm_Add(Scm_Negate(SCM_2_63), SCM_MAKE_INT(2));
    test_scm_c_scm("long roundtrip -2^63+2", vv,
                   Scm_MakeInteger(Scm_GetInteger(vv)));
#endif
    vv = Scm_Add(SCM_2_32, SCM_MAKE_INT(-1));
    test_scm_c_scm("u_long roundtrip 2^31-1", vv,
                   Scm_MakeIntegerU(Scm_GetIntegerU(vv)));
    vv = Scm_Add(SCM_2_32, SCM_MAKE_INT(-3));
    test_scm_c_scm("u_long roundtrip 2^31-3", vv,
                   Scm_MakeIntegerU(Scm_GetIntegerU(vv)));
#if SIZEOF_LONG >= 8
    vv = Scm_Add(SCM_2_64, SCM_MAKE_INT(-1));
    test_scm_c_scm("u_long roundtrip 2^64-1", vv,
                   Scm_MakeIntegerU(Scm_GetIntegerU(vv)));
    vv = Scm_Add(SCM_2_64, SCM_MAKE_INT(-3));
    test_scm_c_scm("u_long roundtrip 2^64-3", vv,
                   Scm_MakeIntegerU(Scm_GetIntegerU(vv)));
#endif

    vv = Scm_Add(SCM_2_31, SCM_MAKE_INT(-1));
    test_scm_c_scm("ScmInt32 roundtrip 2^31-1", vv,
                   Scm_MakeInteger(Scm_GetInteger32Clamp(vv, 0, NULL)));
    vv = Scm_Add(SCM_2_31, SCM_MAKE_INT(-3));
    test_scm_c_scm("ScmInt32 roundtrip 2^31-3", vv,
                   Scm_MakeInteger(Scm_GetInteger32Clamp(vv, 0, NULL)));
    vv = Scm_Negate(SCM_2_31);
    test_scm_c_scm("ScmInt32 roundtrip -2^31", vv,
                   Scm_MakeInteger(Scm_GetInteger32Clamp(vv, 0, NULL)));
    vv = Scm_Add(Scm_Negate(SCM_2_31), SCM_MAKE_INT(2));
    test_scm_c_scm("ScmInt32 roundtrip -2^31+2", vv,
                   Scm_MakeInteger(Scm_GetInteger32Clamp(vv, 0, NULL)));
    vv = Scm_Add(SCM_2_32, SCM_MAKE_INT(-1));
    test_scm_c_scm("ScmUInt32 roundtrip 2^32-1", vv,
                   Scm_MakeIntegerU(Scm_GetIntegerU32Clamp(vv, 0, NULL)));
    vv = Scm_Add(SCM_2_32, SCM_MAKE_INT(-3));
    test_scm_c_scm("ScmUInt32 roundtrip 2^32-3", vv,
                   Scm_MakeIntegerU(Scm_GetIntegerU32Clamp(vv, 0, NULL)));

    vv = Scm_Add(SCM_2_31, SCM_MAKE_INT(-1));
    test_scm_c_scm("ScmInt64 roundtrip 2^31-1", vv,
                   Scm_MakeInteger64(Scm_GetInteger64(vv)));
    vv = SCM_2_31;
    test_scm_c_scm("ScmInt64 roundtrip 2^31", vv,
                   Scm_MakeInteger64(Scm_GetInteger64(vv)));
    vv = Scm_Add(SCM_2_32, SCM_MAKE_INT(-1));
    test_scm_c_scm("ScmInt64 roundtrip 2^32-1", vv,
                   Scm_MakeInteger64(Scm_GetInteger64(vv)));
    vv = SCM_2_32;
    test_scm_c_scm("ScmInt64 roundtrip 2^32", vv,
                   Scm_MakeInteger64(Scm_GetInteger64(vv)));

    vv = Scm_Add(Scm_Negate(SCM_2_31), SCM_MAKE_INT(1));
    test_scm_c_scm("ScmInt64 roundtrip -2^31+1", vv,
                   Scm_MakeInteger64(Scm_GetInteger64(vv)));
    vv = Scm_Negate(SCM_2_31);
    test_scm_c_scm("ScmInt64 roundtrip -2^31", vv,
                   Scm_MakeInteger64(Scm_GetInteger64(vv)));
    vv = Scm_Add(Scm_Negate(SCM_2_32), SCM_MAKE_INT(1));
    test_scm_c_scm("ScmInt64 roundtrip -2^32+1", vv,
                   Scm_MakeInteger64(Scm_GetInteger64(vv)));
    vv = Scm_Negate(SCM_2_32);
    test_scm_c_scm("ScmInt64 roundtrip -2^32", vv,
                   Scm_MakeInteger64(Scm_GetInteger64(vv)));


    vv = Scm_Add(SCM_2_63, SCM_MAKE_INT(-1));
    test_scm_c_scm("ScmInt64 roundtrip 2^63-1", vv,
                   Scm_MakeInteger64(Scm_GetInteger64(vv)));
    vv = Scm_Add(SCM_2_63, SCM_MAKE_INT(-3));
    test_scm_c_scm("ScmInt64 roundtrip 2^63-3", vv,
                   Scm_MakeInteger64(Scm_GetInteger64(vv)));
    vv = Scm_Negate(SCM_2_63);
    test_scm_c_scm("ScmInt64 roundtrip -2^63", vv,
                   Scm_MakeInteger64(Scm_GetInteger64(vv)));
    vv = Scm_Add(Scm_Negate(SCM_2_63), SCM_MAKE_INT(2));
    test_scm_c_scm("ScmInt64 roundtrip -2^63+2", vv,
                   Scm_MakeInteger64(Scm_GetInteger64(vv)));

    vv = Scm_Add(SCM_2_31, SCM_MAKE_INT(-1));
    test_scm_c_scm("ScmUInt64 roundtrip 2^31", vv,
                   Scm_MakeIntegerU64(Scm_GetIntegerU64(vv)));
    vv = SCM_2_31;
    test_scm_c_scm("ScmUInt64 roundtrip 2^31", vv,
                   Scm_MakeIntegerU64(Scm_GetIntegerU64(vv)));
    vv = Scm_Add(SCM_2_32, SCM_MAKE_INT(-1));
    test_scm_c_scm("ScmUInt64 roundtrip 2^32-1", vv,
                   Scm_MakeIntegerU64(Scm_GetIntegerU64(vv)));
    vv = SCM_2_32;
    test_scm_c_scm("ScmUInt64 roundtrip 2^32", vv,
                   Scm_MakeIntegerU64(Scm_GetIntegerU64(vv)));

    vv = Scm_Add(SCM_2_64, SCM_MAKE_INT(-1));
    test_scm_c_scm("ScmUInt64 roundtrip 2^64-1", vv,
                   Scm_MakeIntegerU64(Scm_GetIntegerU64(vv)));

    TEST_SECTION("integer conversions, clamping");
    vv = SCM_2_32;
    test_scm_c_scm("ScmInt32 clamp 2^32",
                   Scm_Add(SCM_2_31, SCM_MAKE_INT(-1)),
                   Scm_MakeInteger(Scm_GetInteger32Clamp(vv, SCM_CLAMP_BOTH, NULL)));
    vv = SCM_2_63;
    test_scm_c_scm("ScmInt32 clamp 2^63",
                   Scm_Add(SCM_2_31, SCM_MAKE_INT(-1)),
                   Scm_MakeInteger(Scm_GetInteger32Clamp(vv, SCM_CLAMP_BOTH, NULL)));
    vv = SCM_2_64;
    test_scm_c_scm("ScmInt32 clamp 2^64",
                   Scm_Add(SCM_2_31, SCM_MAKE_INT(-1)),
                   Scm_MakeInteger(Scm_GetInteger32Clamp(vv, SCM_CLAMP_BOTH, NULL)));
    vv = Scm_Negate(SCM_2_32);
    test_scm_c_scm("ScmInt32 clamp -2^32",
                   Scm_Negate(SCM_2_31),
                   Scm_MakeInteger(Scm_GetInteger32Clamp(vv, SCM_CLAMP_BOTH, NULL)));
    vv = Scm_Negate(SCM_2_63);
    test_scm_c_scm("ScmInt32 clamp -2^63",
                   Scm_Negate(SCM_2_31),
                   Scm_MakeInteger(Scm_GetInteger32Clamp(vv, SCM_CLAMP_BOTH, NULL)));
    vv = Scm_Negate(SCM_2_64);
    test_scm_c_scm("ScmInt32 clamp -2^64",
                   Scm_Negate(SCM_2_31),
                   Scm_MakeInteger(Scm_GetInteger32Clamp(vv, SCM_CLAMP_BOTH, NULL)));


    TEST_SECTION("integer conversions, CLAMP_NONE");

    vv = SCM_MAKE_INT(-1);
    Scm_GetIntegerU64Clamp(vv, SCM_CLAMP_NONE, &oor);
    test_true("ScmUInt64 oor -1", oor);

    vv = Scm_Add(Scm_Negate(SCM_2_31), SCM_MAKE_INT(1));
    Scm_GetIntegerU64Clamp(vv, SCM_CLAMP_NONE, &oor);
    test_true("ScmUInt64 oor -2^31+1", oor);

    vv = Scm_Add(Scm_Negate(SCM_2_32), SCM_MAKE_INT(1));
    Scm_GetIntegerU64Clamp(vv, SCM_CLAMP_NONE, &oor);
    test_true("ScmUInt64 oor -2^32+1", oor);
}

/*=============================================================
 * Testing rounding to machine-word
 */

void test_round_ulong_1(u_long expect, ScmObj obj)
{
    Scm_Printf(SCM_CUROUT, "testing Scm_GetIntegerUMod(%S), expects %lu =>",
               obj, expect);
    u_long r = Scm_GetIntegerUMod(obj);
    if (expect == r) {
        Scm_Printf(SCM_CUROUT, "ok\n");
    } else {
        Scm_Printf(SCM_CUROUT, "ERROR: got %lu\n", r);
        errcount++;
    }
}

void test_round_ulong()
{
    TEST_SECTION("integers modulo ulong range");
    test_round_ulong_1(142857, SCM_MAKE_INT(142857));
    test_round_ulong_1(285714, Scm_Add(Scm_Expt(SCM_MAKE_INT(2),
                                                SCM_MAKE_INT(100)),
                                       SCM_MAKE_INT(285714)));
    test_round_ulong_1(UMAX, SCM_MAKE_INT(-1));
#if SIZEOF_LONG == 4
    test_round_ulong_1(571428, Scm_Sub(SCM_MAKE_INT(571428), SCM_2_32));
#else  /*SIZEOF_LONG > 4 */
    test_round_ulong_1(571428, Scm_Sub(SCM_MAKE_INT(571428), SCM_2_64));
#endif /*SIZEOF_LONG > 4 */
}

/*=============================================================
 * Testing 16bit floats
 */

void test_half(const char *msg, int expect, int val)
{
    Scm_Printf(SCM_CUROUT, "testing %s, expects %04x =>", msg, expect);
    if (expect == val) {
        Scm_Printf(SCM_CUROUT, "ok\n");
    } else {
        Scm_Printf(SCM_CUROUT, "ERROR: got %04x\n", val);
        errcount++;
    }
}

void test_double(const char *msg, double expect, double val)
{
    Scm_Printf(SCM_CUROUT, "testing %s, expects %lg =>", msg, expect);
    if (expect == val) {
        Scm_Printf(SCM_CUROUT, "ok\n");
    } else {
        Scm_Printf(SCM_CUROUT, "ERROR: got %lg\n", val);
        errcount++;
    }
}

void test_f16(void)
{
    double z;

    TEST_SECTION("half floats");
    test_double("half->double 0",    0.0,    Scm_HalfToDouble(0));
    test_double("half->double 1",    1.0,    Scm_HalfToDouble(0x3c00));
    test_double("half->double -1",   -1.0,   Scm_HalfToDouble(0xbc00));
    test_double("half->double 1.5",  1.5,    Scm_HalfToDouble(0x3e00));
    test_double("half->double -1.5", -1.5,   Scm_HalfToDouble(0xbe00));
    test_double("half->double 0.75", 0.75,   Scm_HalfToDouble(0x3a00));
    test_double("half->double 0.875",0.875,  Scm_HalfToDouble(0x3b00));

    test_double("half->double all 1", 1.9990234375, Scm_HalfToDouble(0x3fff));
    test_double("half->double maximum", 65504.0, Scm_HalfToDouble(0x7bff));
    test_double("half->double normalized min", 6.103515625e-5,
                Scm_HalfToDouble(0x0400));
    test_double("half->double denormalized max", 6.097555160522461e-5,
                Scm_HalfToDouble(0x03ff));
    test_double("half->double denormalized max-1", 6.091594696044922e-5,
                Scm_HalfToDouble(0x03fe));
    test_double("half->double denormalized min", 5.960464477539063e-8,
                Scm_HalfToDouble(0x0001));

    test_double("half->double inf", SCM_DBL_POSITIVE_INFINITY,
                Scm_HalfToDouble(0x7c00));
    test_double("half->double -inf", SCM_DBL_NEGATIVE_INFINITY,
                Scm_HalfToDouble(0xfc00));
    z = Scm_HalfToDouble(0xffff);
    test_true("half->double nan", !(z==z));


    test_half("double->half 0",    0,      Scm_DoubleToHalf(0.0));
    test_half("double->half 1",    0x3c00, Scm_DoubleToHalf(1.0));
    test_half("double->half -1",   0xbc00, Scm_DoubleToHalf(-1.0));
    test_half("double->half 1.5",  0x3e00, Scm_DoubleToHalf(1.5));
    test_half("double->half -1.5", 0xbe00, Scm_DoubleToHalf(-1.5));
    test_half("double->half 0.75", 0x3a00, Scm_DoubleToHalf(0.75));
    test_half("double->half 0.875",0x3b00, Scm_DoubleToHalf(0.875));

    test_half("double->half all 1", 0x3fff, Scm_DoubleToHalf(1.9990234375));
    test_half("double->half max", 0x7bff, Scm_DoubleToHalf(65504.0));
    test_half("double->half normalized min", 0x0400,
              Scm_DoubleToHalf(6.103515625e-5));
    test_half("double->half denormalized max", 0x03ff,
              Scm_DoubleToHalf(6.097555160522461e-5));
    test_half("double->half denormalized max-1", 0x03fe,
              Scm_DoubleToHalf(6.091594696044922e-5));
    test_half("double->half denormalized min", 0x0001,
              Scm_DoubleToHalf(5.960464477539063e-8));

    test_half("double->half inf",  0x7c00, Scm_DoubleToHalf(SCM_DBL_POSITIVE_INFINITY));
    test_half("double->half -inf", 0xfc00, Scm_DoubleToHalf(SCM_DBL_NEGATIVE_INFINITY));
    test_half("double->half nan",  0x7fff, Scm_DoubleToHalf(SCM_DBL_NAN));

    test_half("double->half, rounding", 0x4000,
              Scm_DoubleToHalf(1.999755859375)); /* m=#b1111111111.11 */
    test_half("double->half, rounding", 0x4000,
              Scm_DoubleToHalf(1.99951171875));  /* m=#b1111111111.10 */
    test_half("double->half, rounding", 0x3fff,
              Scm_DoubleToHalf(1.999267578125)); /* m=#b1111111111.01 */
    test_half("double->half, rounding", 0x3fff,
              Scm_DoubleToHalf(1.998779296875)); /* m=#b1111111110.11 */
    test_half("double->half, rounding", 0x3ffe,
              Scm_DoubleToHalf(1.99853515625));  /* m=#b1111111110.10 */
    test_half("double->half, rounding", 0x3ffe,
              Scm_DoubleToHalf(1.998291015625)); /* m=#b1111111110.01 */
    test_half("double->half, rounding to overflow", 0x7c00,
              Scm_DoubleToHalf(65520.0));
    test_half("double->half, just below overflow", 0x7bff,
              Scm_DoubleToHalf(65519.99999999999));
    test_half("double->half, rounding to overflow", 0xfc00,
              Scm_DoubleToHalf(-65520.0));
    test_half("double->half, just below overflow", 0xfbff,
              Scm_DoubleToHalf(-65519.99999999999));
    test_half("double->half, rounding to normalized", 0x0400,
              Scm_DoubleToHalf(6.1005353927612305e-5));
    test_half("double->half, rounding to denormalized min", 0x0001,
              Scm_DoubleToHalf(2.980232238770209e-8));
    test_half("double->half, just below above", 0x0000,
              Scm_DoubleToHalf(2.9802322387695312e-8));
}

/*=============================================================
 * main
 */
int main(int argc, char **argv)
{
    const char *testmsg = "Testing integer arithmetic macros ... ";

    Scm_Init(GAUCHE_SIGNATURE);

    fprintf(stderr, "%-65s", testmsg);
    message(stdout, testmsg, '=');

    test_uadd();
    test_uaddov();
    test_saddov();
    test_usub();
    test_usubov();
    test_ssubov();
    test_umul();
    test_umulov();
    test_smulov();

    test_32_64();

    test_round_ulong();

    test_f16();

    if (errcount) {
        fprintf(stderr, "failed.\n");
        fprintf(stdout, "failed.\n");
    } else {
        fprintf(stderr, "passed.\n");
        fprintf(stdout, "passed.\n");
    }
    return 0;
}

