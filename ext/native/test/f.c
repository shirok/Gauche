#include <inttypes.h>
#include "f.h"

char f_c(void)
{
    return 9;
}

int f_i(void)
{
    return 42;
}

float f_f(void)
{
    return 1.25f;
}

double f_d(void)
{
    return 3.14;
}

void f_v(void)
{
}

int f_i_i(int n)
{
    return n+1;
}

float f_f_f(float x)
{
    return x/2;
}

double f_d_d(double d)
{
    return d*2;
}

void f_no_proto()
{
}
