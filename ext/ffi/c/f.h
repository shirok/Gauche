#include <stdio.h>

struct foo {
    char c;
    int i;
    short s;
    long l;
    float f;
    double d;
};

extern char F_c(void);
extern int F_i(void);
extern unsigned long F_ul(void);
extern float F_f(void);
extern double F_d(void);
extern void F_v(void);

extern int Fi_i(int);
extern float Ff_f(float);
extern float Ffff_f(float, float, float);
extern double Fd_d(double);
extern double Gd_d(double);
extern double Fddd_d(double, double, double);
extern double Fifd_d(int, float, double);
extern float Fifd_f(int, float, double);
extern double Fidf_d(int, double, float);
extern float Fidf_f(int, double, float);

extern struct foo *F_pstruct_c_pstruct(struct foo*, char);
extern struct foo *F_pstruct_s_pstruct(struct foo*, short);
extern struct foo *F_pstruct_i_pstruct(struct foo*, int);
extern struct foo *F_pstruct_l_pstruct(struct foo*, long);
extern struct foo *F_pstruct_f_pstruct(struct foo*, float);
extern struct foo *F_pstruct_d_pstruct(struct foo*, double);

extern int Fivar(int cnt, ...);
extern double Fdvar(int cnt, ...);

extern int Fiiiiiii_i(int, int, int, int, int, int, int);
extern double Fddddddddd_d(double, double, double, double, double, double, double, double, double);
extern double Fiiiiiiidddd_d(int, int, int, int, int, int, int, double, double, double, double);
extern int Fs_i(const char *);
extern const char *Fi_s(int);
extern double Fidfvar(int cnt, ...);
extern int Fpnull_i(void *);

/* Callback test: invoke cb(x, y) and return its result. */
extern int Fcb2_i(int (*cb)(int, int), int x, int y);
/* Callback test: invoke cb(x, y) with doubles. */
extern double Fcb2_d(double (*cb)(double, double), double x, double y);
/* Callback test: invoke cb() with no args returning void; also returns
   how many times cb was called via a side-channel counter. */
extern int Fcb_v_count(void (*cb)(void), int times);
/* Callback test with pointer arg: cb receives a pointer and an int,
   returns int. */
extern int Fcb_pi_i(int (*cb)(int *, int), int *p, int n);
