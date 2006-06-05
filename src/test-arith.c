/*
 * Test the lowest-level numeric routines.
 * $Id: test-arith.c,v 1.7 2006-06-05 05:11:25 shirok Exp $
 */

#include <stdio.h>
#include "gauche.h"
#include "gauche/arith.h"
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
        int len = 79 - strlen(m);
        if (len < 0) len = 5;
        for (i=0; i<len; i++) putc(filler, out);
    }
    putc('\n', out);
}

/*=============================================================
 * Testing macros in gauche/arith.h
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
  TEST5(x_, y_, c_, rexp, cexp, "%u", "+", UADD)

void test_uadd(void)
{
    u_long r, c, x, y;
    TEST_SECTION("UADD");

    /* MAX + 0 + 0 => [MAX, 0] */
    TEST_UADD(UMAX, 0, 0, UMAX, 0);
    /* MAX + 1 + 0 => [0, 1]   */
    TEST_UADD(UMAX, 1, 0, 0, 1);
    /* MAX + 2 + 0 => [1, 1]   */
    TEST_UADD(UMAX, 2, 0, 1, 1);
    /* MAX + MAX + 0 => [MAX-1, 1] */
    TEST_UADD(UMAX, UMAX, 0, UMAX-1, 1);
    /* MAX/2 + MAX/2 + 0 => [MAX-1, 0] */
    TEST_UADD(UMAX/2, UMAX/2, 0, UMAX-1, 0);
    /* MAX/2 + MAX + 0 => [MAX/2-1, 1] */
    TEST_UADD(UMAX/2, UMAX, 0, UMAX/2-1, 1);
    /* MAX-1 + 0 + 1 => [MAX, 0] */
    TEST_UADD(UMAX-1, 0, 1, UMAX, 0);
    /* MAX + 0 + 1 => [0, 1] */
    TEST_UADD(UMAX, 0, 1, 0, 1);
    /* MAX + 1 + 1 => [1, 1] */
    TEST_UADD(UMAX, 1, 1, 1, 1);
    /* MAX + MAX + 1 => [MAX, 1] */
    TEST_UADD(UMAX, UMAX, 1, UMAX, 1);
    /* MAX/2 + MAX/2 + 1 => [MAX, 0] */
    TEST_UADD(UMAX/2, UMAX/2, 1, UMAX, 0);
    /* MAX/2 + MAX + 1 => [MAX/2, 1] */
    TEST_UADD(UMAX/2, UMAX, 1, UMAX/2, 1);
}

/*
 * UADDOV
 */
#define TEST_UADDOV(x_, y_, rexp, cexp)         \
    TESTOV(x_, y_, rexp, cexp, "%u", "+", UADDOV)

void test_uaddov(void)
{
    u_long r, c, x, y;
    TEST_SECTION("UADDOV");

    /* MAX + 0 => MAX */
    TEST_UADDOV(UMAX, 0, UMAX, 0);
    /* MAX + 1 => overflow */
    TEST_UADDOV(UMAX, 1, 0, 1);
    /* MAX + 2 => overflow */
    TEST_UADDOV(UMAX, 2, 0, 1);
    /* MAX + MAX => overflow */
    TEST_UADDOV(UMAX, UMAX, 0, 1);
    /* MAX/2 + MAX/2 => MAX-1, 0 */
    TEST_UADDOV(UMAX/2, UMAX/2, UMAX-1, 0);
    /* MAX/2 + MAX => overflow */
    TEST_UADDOV(UMAX/2, UMAX, 0, 1);
    /* MAX-1 + 1 => MAX */
    TEST_UADDOV(UMAX-1, 1, UMAX, 0);
    /* MAX-1 + 2 => overflow */
    TEST_UADDOV(UMAX-1, 2, 0, 1);
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
    TEST_SADDOV(1, 1, 2, 0);
    /* -1 + -1 => -2 */
    TEST_SADDOV(-1, -1, -2, 0);
    /* SMAX + 0 => SMAX */
    TEST_SADDOV(SMAX, 0, SMAX, 0);
    /* SMAX + 1 => overflow */
    TEST_SADDOV(SMAX, 1, 0, 1);
    /* SMAX + 2 => overflow */
    TEST_SADDOV(SMAX, 2, 0, 1);
    /* SMAX + -1 => SMAX-1 */
    TEST_SADDOV(SMAX, -1, SMAX-1, 0);
    /* SMAX + -SMAX => 0 */
    TEST_SADDOV(SMAX, -SMAX, 0, 0);
    /* SMAX + -SMAX-1 => -1 */
    TEST_SADDOV(SMAX, -SMAX-1, -1, 0);
    /* SMAX/2 + SMAX/2 => SMAX-1 */
    TEST_SADDOV(SMAX/2, SMAX/2, SMAX-1, 0);
    /* SMAX/2 + -SMAX/2 => 0 */
    TEST_SADDOV(SMAX/2, -(SMAX/2), 0, 0);
    /* -SMAX/2 + -SMAX/2 => -(SMAX-1) */
    TEST_SADDOV(-(SMAX/2), -(SMAX/2), -(SMAX-1), 0);
    /* 0 + -SMAX => -SMAX */
    TEST_SADDOV(0, -SMAX, -SMAX, 0);
    /* -1 + -SMAX => SMIN */
    TEST_SADDOV(-1, -SMAX, SMIN, 0);
    /* -2 + -SMAX => -overflow */
    TEST_SADDOV(-2, -SMAX, 0, -1);
    /* -SMAX + -2 => -overflow */
    TEST_SADDOV(-SMAX, -2, 0, -1);
    /* -SMAX + -SMAX => -overflow */
    TEST_SADDOV(-SMAX, -SMAX, 0, -1);
    /* -SMAX + SMIN => -overflow */
    TEST_SADDOV(-SMAX, SMIN, 0, -1);
}

/*
 * USUB
 */
#define TEST_USUB(x_, y_, c_, rexp, cexp)               \
    TEST5(x_, y_, c_, rexp, cexp, "%u", "-", USUB)

void test_usub(void)
{
    u_long r, c, x, y;
    TEST_SECTION("USUB");

    /* MAX - 0 - 0 => [MAX, 0] */
    TEST_USUB(UMAX, 0, 0, UMAX, 0);
    /* MAX - 1 - 0 => [MAX-1, 0] */
    TEST_USUB(UMAX, 1, 0, UMAX-1, 0);
    /* 0 - MAX - 0 => [1, 1] */
    TEST_USUB(0, UMAX, 0, 1, 1);
    /* 1 - MAX - 0 => [2, 1] */
    TEST_USUB(1, UMAX, 0, 2, 1);
    /* MAX - MAX/2 - 0 => [MAX/2+1, 0] */
    TEST_USUB(UMAX, UMAX/2, 0, UMAX/2+1, 0);
    /* MAX/2 - MAX - 0 => [MAX/2+1, 1] */
    TEST_USUB(UMAX/2, UMAX, 0, UMAX/2+1, 1);
    /* MAX - 0 - 1 => [MAX-1, 0] */
    TEST_USUB(UMAX, 0, 1, UMAX-1, 0);
    /* MAX - 1 - 1 => [MAX-2, 0] */
    TEST_USUB(UMAX, 1, 1, UMAX-2, 0);
    /* 0 - MAX - 1 => [0, 1] */
    TEST_USUB(0, UMAX, 1, 0, 1);
    /* 1 - MAX - 1 => [1, 1] */
    TEST_USUB(1, UMAX, 1, 1, 1);
    /* MAX - MAX/2 - 1 => [MAX/2, 0] */
    TEST_USUB(UMAX, UMAX/2, 1, UMAX/2, 0);
    /* MAX/2 - MAX - 1 => [MAX/2, 1] */
    TEST_USUB(UMAX/2, UMAX, 1, UMAX/2, 1);
}

/*
 * USUBOV
 */
#define TEST_USUBOV(x_, y_, rexp, cexp)         \
    TESTOV(x_, y_, rexp, cexp, "%u", "-", USUBOV)

void test_usubov(void)
{
    u_long r, c, x, y;
    TEST_SECTION("USUBOV");

    /* MAX - 0  => MAX */
    TEST_USUBOV(UMAX, 0, UMAX, 0);
    /* MAX - 1  => MAX-1 */
    TEST_USUBOV(UMAX, 1, UMAX-1, 0);
    /* MAX - MAX => 0 */
    TEST_USUBOV(UMAX, UMAX, 0, 0);
    /* MAX-1 - MAX => overflow */
    TEST_USUBOV(UMAX-1, UMAX, 0, 1);
    /* MAX - MAX-1 => 1 */
    TEST_USUBOV(UMAX, UMAX-1, 1, 0);
    /* 0 - 1 => overflow */
    TEST_USUBOV(0, 1, 0, 1);
    /* 0 - MAX => overflow */
    TEST_USUBOV(0, UMAX, 0, 1);
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
    TEST_SSUBOV(1, 1, 0, 0);
    /* -1 - -1 => 0 */
    TEST_SSUBOV(-1, -1, 0, 0);
    /* SMAX - 0 => SMAX */
    TEST_SSUBOV(SMAX, 0, SMAX, 0);
    /* SMAX - 1 => SMAX-1 */
    TEST_SSUBOV(SMAX, 1, SMAX-1, 0);
    /* SMAX - -1 => overflow */
    TEST_SSUBOV(SMAX, -1, 0, 1);
    /* SMAX - -2 => overflow */
    TEST_SSUBOV(SMAX, -2, 0, 1);
    /* SMAX - -SMAX => overflow */
    TEST_SSUBOV(SMAX, -SMAX, 0, 1);
    /* SMAX - SMAX => 0 */
    TEST_SSUBOV(SMAX, SMAX, 0, 0);
    /* SMAX/2 - -SMAX/2 => SMAX-1 */
    TEST_SSUBOV(SMAX/2, -(SMAX/2), SMAX-1, 0);
    /* SMAX/2 - SMAX/2 => 0 */
    TEST_SSUBOV(SMAX/2, SMAX/2, 0, 0);
    /* -SMAX/2 - SMAX/2 => 0 */
    TEST_SSUBOV(-(SMAX/2), SMAX/2, -(SMAX-1), 0);
    /* 0 - -SMAX => SMAX */
    TEST_SSUBOV(0, -SMAX, SMAX, 0);
    /* 1 - -SMAX => overflow */
    TEST_SSUBOV(1, -SMAX, 0, 1);
    /* 0 - -SMAX => SMAX */
    TEST_SSUBOV(0, -SMAX, SMAX, 0);
    /* SMIN - 1 => -overflow */
    TEST_SSUBOV(SMIN, 1, 0, -1);
    /* SMIN - -1 => SMIN+1 */
    TEST_SSUBOV(SMIN, -1, SMIN+1, 0);
    /* -SMAX - -SMAX => 0 */
    TEST_SSUBOV(-SMAX, -SMAX, 0, 0);
    /* -SMAX - SMIN => 1 */
    TEST_SSUBOV(-SMAX, SMIN, 1, 0);
}

/*
 * UMUL
 */
#define TEST_UMUL(x_, y_, hiexp, loexp)                                 \
    do {                                                                \
        printf("testing %u*%u expects hi=%u, lo=%u =>", x_, y_,        \
               hiexp, loexp);                                           \
        x = x_;                                                         \
        y = y_;                                                         \
        UMUL(hi, lo, x, y);                                             \
        if (hi == hiexp && lo == loexp) {                               \
            printf("ok\n");                                             \
        } else {                                                        \
            errcount++;                                                 \
            printf("ERROR: got hi=%u, lo=%u\n", hi, lo);                \
        }                                                               \
    } while (0)

void test_umul(void)
{
    u_long hi, lo, x, y;
    TEST_SECTION("UMUL");

    /* MAX * MAX => [MAX-1, 1] */
    TEST_UMUL(UMAX, UMAX, UMAX-1, 1);
    /* MAX-1 * MAX-1 => [MAX-3, 4] */
    TEST_UMUL(UMAX-1, UMAX-1, UMAX-3, 4);
    /* MAX/2 * 2 => [0, MAX-1] */
    TEST_UMUL(UMAX/2, 2, 0, UMAX-1);
    /* MAX/2+1 * 2 => [1, 0] */
    TEST_UMUL(UMAX/2+1, 2, 1, 0);
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
    TEST_UMULOV(0, 1, 0, 0);
    /* 1 * 0 => 0 */
    TEST_UMULOV(1, 0, 0, 0);
    /* 1 * 1 => 1 */
    TEST_UMULOV(1, 1, 1, 0);
    /* 1 * UMAX => UMAX */
    TEST_UMULOV(1, UMAX, UMAX, 0);
    /* 2 * UMAX => overflow */
    TEST_UMULOV(2, UMAX, 0, 1);
    /* UMAX/2 * 2 => UMAX-1 */
    TEST_UMULOV(UMAX/2, 2, UMAX-1, 0);
    /* UMAX/2 * 3 => overflow */
    TEST_UMULOV(UMAX/2, 3, 0, 1);
    /* UMAX>>4 * UMAX>>4 => overflow */
    TEST_UMULOV((UMAX>>4), (UMAX>>4), 0, 1);
    /* UMAX>>8 * UMAX>>8 => overflow */
    TEST_UMULOV((UMAX>>8), (UMAX>>8), 0, 1);
    /* UMAX * UMAX => overflow */
    TEST_UMULOV(UMAX, UMAX, 0, 1);
    /* UMAX-1 * UMAX-1 => overflow */
    TEST_UMULOV(UMAX-1, UMAX-1, 0, 1);
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
    TEST_SMULOV(0, 1, 0, 0);
    /* 1 * 0 => 0 */
    TEST_SMULOV(1, 0, 0, 0);
    /* 1 * 1 => 1 */
    TEST_SMULOV(1, 1, 1, 0);
    /* 1 * -1 => -1 */
    TEST_SMULOV(1, -1, -1, 0);
    /* -1 * 1 => -1 */
    TEST_SMULOV(-1, 1, -1, 0);
    /* -1 * -1 => 1 */
    TEST_SMULOV(-1, -1, 1, 0);
    /* 1 * SMAX => SMAX */
    TEST_SMULOV(1, SMAX, SMAX, 0);
    /* 1 * -SMAX => -SMAX */
    TEST_SMULOV(1, -SMAX, -SMAX, 0);
    /* -1 * SMAX => -SMAX */
    TEST_SMULOV(-1, SMAX, -SMAX, 0);
    /* -1 * -SMAX => SMAX */
    TEST_SMULOV(-1, -SMAX, SMAX, 0);
    /* 1 * SMIN => SMIN */
    TEST_SMULOV(1, SMIN, SMIN, 0);
    /* -1 * SMIN => overflow */
    TEST_SMULOV(-1, SMIN, 0, 1);
    /* 2 * SMAX => overflow */
    TEST_SMULOV(2, SMAX, 0, 1);
    /* 2 * -SMAX => -overflow */
    TEST_SMULOV(2, -SMAX, 0, -1);
    /* -2 * SMAX => -overflow */
    TEST_SMULOV(-2, SMAX, 0, -1);
    /* -2 * -SMAX => overflow */
    TEST_SMULOV(-2, -SMAX, 0, 1);
    /* SMAX/2 * 2 => SMAX-1 */
    TEST_SMULOV(SMAX/2, 2, SMAX-1, 0);
    /* SMAX/2 * -2 => -(SMAX-1) */
    TEST_SMULOV(SMAX/2, -2, -(SMAX-1), 0);
    /* -SMAX/2 * 2 => -(SMAX-1) */
    TEST_SMULOV(-(SMAX/2), 2, -(SMAX-1), 0);
    /* -SMAX/2 * -2 => SMAX-1 */
    TEST_SMULOV(-(SMAX/2), -2, SMAX-1, 0);
    /* SMAX/2+1 * 2 => overflow */
    TEST_SMULOV(SMAX/2+1, 2, 0, 1);
    /* SMAX/2+1 * -2 => SMIN */
    TEST_SMULOV(SMAX/2+1, -2, SMIN, 0);
    /* -(SMAX/2+1) * 2 => SMIN */
    TEST_SMULOV(-(SMAX/2+1), 2, SMIN, 0);
    /* -(SMAX/2+1) * -2 => overflow */
    TEST_SMULOV(-(SMAX/2+1), -2, 0, 1);
    /* SMAX>>4 * SMAX>>4 => overflow */
    TEST_SMULOV((SMAX>>4), (SMAX>>4), 0, 1);
    /* SMAX>>4 * -SMAX>>4 => -overflow */
    TEST_SMULOV((SMAX>>4), -(SMAX>>4), 0, -1);
    /* -SMAX>>4 * SMAX>>4 => -overflow */
    TEST_SMULOV(-(SMAX>>4), (SMAX>>4), 0, -1);
    /* -SMAX>>4 * -SMAX>>4 => overflow */
    TEST_SMULOV(-(SMAX>>4), -(SMAX>>4), 0, 1);
    /* SMAX>>8 * SMAX>>8 => overflow */
    TEST_SMULOV((SMAX>>8), (SMAX>>8), 0, 1);
    /* SMAX>>8 * -SMAX>>8 => -overflow */
    TEST_SMULOV((SMAX>>8), -(SMAX>>8), 0, -1);
    /* -SMAX>>8 * SMAX>>8 => -overflow */
    TEST_SMULOV(-(SMAX>>8), (SMAX>>8), 0, -1);
    /* -SMAX>>8 * -SMAX>>8 => overflow */
    TEST_SMULOV(-(SMAX>>8), -(SMAX>>8), 0, 1);
    /* SMAX * SMAX => overflow */
    TEST_SMULOV(SMAX, SMAX, 0, 1);
    /* SMAX * -SMAX => -overflow */
    TEST_SMULOV(SMAX, -SMAX, 0, -1);
    /* -SMAX * SMAX => -overflow */
    TEST_SMULOV(-SMAX, SMAX, 0, -1);
    /* -SMAX * -SMAX => overflow */
    TEST_SMULOV(-SMAX, -SMAX, 0, 1);
}

/*=============================================================
 * Testing 32/64-bit conversion routines
 */

int test_scm_c_scm(const char *msg, ScmObj expect, ScmObj val)
{
    Scm_Printf(SCM_CUROUT, "testing %s, expects %S =>", msg, expect);
    if (Scm_EqualP(expect, val)) {
        Scm_Printf(SCM_CUROUT, "ok\n");
    } else {
        Scm_Printf(SCM_CUROUT, "ERROR: got %S\n", val);
        errcount++;
    }
}

int test_true(const char *msg, int val)
{
    Scm_Printf(SCM_CUROUT, "testing %s, expects TRUE =>", msg);
    if (val) {
        Scm_Printf(SCM_CUROUT, "ok\n");
    } else {
        Scm_Printf(SCM_CUROUT, "ERROR: got %d\n", val);
        errcount++;
    }
}



int test_32_64(void)
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

    if (errcount) {
        fprintf(stderr, "failed.\n");
        fprintf(stdout, "failed.\n");
    } else {
        fprintf(stderr, "passed.\n");
        fprintf(stdout, "passed.\n");
    }
    return 0;
}

