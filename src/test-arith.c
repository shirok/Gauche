/*
 * Test gauche/arith.h macros
 * $Id: test-arith.c,v 1.2 2002-06-18 06:16:30 shirok Exp $
 */

#include <stdio.h>
#include "gauche.h"
#include "gauche/arith.h"

#define UMAX SCM_ULONG_MAX

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

#define TEST_SECTION(name) message(stdout, "<" name ">", '-')

#define TEST_UADD(x_, y_, c_, rexp, cexp)                               \
    do {                                                                \
        printf("testing %u+%u c=%u expectes r=%u, c=%u =>", x_, y_, c_, \
               rexp, cexp);                                             \
        c = c_;                                                         \
        x = x_;                                                         \
        y = y_;                                                         \
        UADD(r, c, x, y);                                               \
        if (r == rexp && c == cexp) {                                   \
            printf("ok\n");                                             \
        } else {                                                        \
            errcount++;                                                 \
            printf("ERROR: got r=%u, c=%u\n", r, c);                    \
        }                                                               \
    } while (0)

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

#define TEST_USUB(x_, y_, c_, rexp, cexp)                               \
    do {                                                                \
        printf("testing %u-%u c=%u expectes r=%u, c=%u =>", x_, y_, c_, \
               rexp, cexp);                                             \
        c = c_;                                                         \
        x = x_;                                                         \
        y = y_;                                                         \
        USUB(r, c, x, y);                                               \
        if (r == rexp && c == cexp) {                                   \
            printf("ok\n");                                             \
        } else {                                                        \
            errcount++;                                                 \
            printf("ERROR: got r=%u, c=%u\n", r, c);                    \
        }                                                               \
    } while (0)

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

#define TEST_UMUL(x_, y_, hiexp, loexp)                                 \
    do {                                                                \
        printf("testing %u*%u expectes hi=%u, lo=%u =>", x_, y_,        \
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

int main(int argc, char **argv)
{
    const char *testmsg = "Testing integer arithmetic macros ... ";
    
    fprintf(stderr, "%-65s", testmsg);
    message(stdout, testmsg, '=');
    
    test_uadd();
    test_usub();
    test_umul();

    if (errcount) {
        fprintf(stderr, "failed.\n");
        fprintf(stdout, "failed.\n");
    } else {
        fprintf(stderr, "passed.\n");
        fprintf(stdout, "passed.\n");
    }
    return 0;
}

