#include <inttypes.h>
#include <limits.h>
#include <stdarg.h>
#include <string.h>
#include "f.h"


char F_c(void)
{
    return 9;
}

int F_i(void)
{
    return 42;
}

unsigned long F_ul(void)
{
    return ULONG_MAX;
}

float F_f(void)
{
    return 1.25f;
}

double F_d(void)
{
    return 3.14;
}

void F_v(void)
{
}

int Fi_i(int n)
{
    return n+1;
}

float Ff_f(float x)
{
    return x/2;
}

float Ffff_f(float x, float y, float z)
{
    return x+y+z;
}

double Fd_d(double d)
{
    return d*2;
}

double Gd_d(double d)
{
    return d*4;
}

double Fddd_d(double x, double y, double z)
{
    return x+y+z;
}

double Fifd_d(int x, float y, double z)
{
    return x*y + x*z;
}

float Fifd_f(int x, float y, double z)
{
    return -(x*y + x*z);
}

double Fidf_d(int x, double y, float z)
{
    return x*y - x*z;
}

float Fidf_f(int x, double y, float z)
{
    return -(x*y - x*z);
}

struct foo *F_pstruct_c_pstruct(struct foo *st, char c)
{
    st->c = c;
    return st;
}

struct foo *F_pstruct_s_pstruct(struct foo *st, short s)
{
    st->s = s;
    return st;
}

struct foo *F_pstruct_i_pstruct(struct foo *st, int i)
{
    st->i = i;
    return st;
}

struct foo *F_pstruct_l_pstruct(struct foo *st, long l)
{
    st->l = l;
    return st;
}

struct foo *F_pstruct_f_pstruct(struct foo *st, float f)
{
    st->f = f;
    return st;
}

struct foo *F_pstruct_d_pstruct(struct foo *st, double d)
{
    st->d = d;
    return st;
}

int Fivar(int cnt, ...)
{
    int r = 0;
    va_list ap;
    va_start(ap, cnt);

    while (cnt-- > 0) {
        int v = va_arg(ap, int);
        r += v;
    }
    va_end(ap);
    return r;
}

double Fdvar(int cnt, ...)
{
    double r = 0;
    va_list ap;
    va_start(ap, cnt);

    while (cnt-- > 0) {
        double v = va_arg(ap, double);
        r += v;
    }
    va_end(ap);
    return r;
}

/* Spill tests: more args than fit in registers.
   SysV x86_64 ABU uses 6 integer arg regs and 8 float arg regs (xmm0-7).
   Win64 ABI uses less regs, so we test for SysV spill case to cover both.
*/

/* 7 int args: 7th spills past the 6 integer registers */
int Fiiiiiii_i(int a, int b, int c, int d, int e, int f, int g)
{
    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g;
}

/* 9 double args: 9th spills past the 8 xmm registers */
double Fddddddddd_d(double a, double b, double c, double d,
                    double e, double f, double g, double h, double i)
{
    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i;
}

/* 7 ints + 4 doubles: integer registers spill, floats stay in xmm */
double Fiiiiiiidddd_d(int a, int b, int c, int d, int e, int f, int g,
                      double h, double i, double j, double k)
{
    return a + b + c + d + e + f + g + h + i + j + k;
}

/* c-string: take a string arg, return its length */
int Fs_i(const char *s)
{
    return (int)strlen(s);
}

/* c-string: take an int, return a static string */
const char *Fi_s(int n)
{
    static const char *labels[] = {"zero", "one", "two", "three", "four"};
    if (n >= 0 && n < 5) return labels[n];
    return "";
}

/* array */
int Fia_i(int n, int32_t *a)
{
    int r = 0;
    for (int i = 0; i < n; i++) {
        r += a[i];
    }
    return r;
}


/* Returns 1 if pointer is NULL, 0 otherwise */
int Fpnull_i(void *p)
{
    return p == NULL ? 1 : 0;
}

int Fcb2_i(int (*cb)(int, int), int x, int y)
{
    return cb(x, y);
}

double Fcb2_d(double (*cb)(double, double), double x, double y)
{
    return cb(x, y);
}

int Fcb_v_count(void (*cb)(void), int times)
{
    int i;
    for (i = 0; i < times; i++) cb();
    return i;
}

int Fcb_pi_i(int (*cb)(int *, int), int *p, int n)
{
    return cb(p, n);
}

/* Mixed varargs: cnt pairs of (int n, double x); returns sum of n*x */
double Fidfvar(int cnt, ...)
{
    double r = 0;
    va_list ap;
    va_start(ap, cnt);
    while (cnt-- > 0) {
        int n = va_arg(ap, int);
        double x = va_arg(ap, double);
        r += n * x;
    }
    va_end(ap);
    return r;
}

/* Integer return value variations  */
int8_t Fi_i8(int v)
{
    return v;
}

int16_t Fi_i16(int v)
{
    return v;
}

int32_t Fi_i32(int v)
{
    return v;
}

int64_t Fi_i64(int v)
{
    return v;
}

uint8_t Fu_u8(unsigned int v)
{
    return v;
}

uint16_t Fu_u16(unsigned int v)
{
    return v;
}

uint32_t Fu_u32(unsigned int v)
{
    return v;
}

uint64_t Fu_u64(unsigned int v)
{
    return v;
}
