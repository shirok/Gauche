/*
 * mt-random.c - implements MT19937 random number generation algorithm
 *
 * Mersenne Twister algorithm invented by Makoto Matsumoto & Takuji Nishimura.
 *   http://www.math.keio.ac.jp/~matumoto/emt.html
 * This code is based on mt18837ar.c (2002/1/16).   It is released under
 * BSD license.
 * I modified the code in the following parts.
 *   - make it modular, esp., all the state information is kept in
 *     the allocated memory for random number generator object.
 *   - added stuff to make it as a Gauche extension module.
 * $Id: mt-random.c,v 1.1 2002-05-10 10:57:48 shirok Exp $
 *
 * The original copyright notice follows.
 */
/*
   A C-program for MT19937, with initialization improved 2002/1/26.
   Coded by Takuji Nishimura and Makoto Matsumoto.

   Before using, initialize the state by using init_genrand(seed)  
   or init_by_array(init_key, key_length).

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   All rights reserved.                          

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote 
        products derived from this software without specific prior written 
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


   Any feedback is very welcome.
   http://www.math.keio.ac.jp/matumoto/emt.html
   email: matumoto@math.keio.ac.jp
*/

#include "gauche.h"
#include "mt-random.h"
#include <math.h>

/* Period parameters */  
#define M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */

/* initializes mt[N] with a seed */
static void
init_genrand(ScmMersenneTwister *mt, unsigned long s)
{
    int mti;
    mt->mt[0]= s & 0xffffffffUL;
    for (mti=1; mti<N; mti++) {
        mt->mt[mti] = 
	    (1812433253UL * (mt->mt[mti-1] ^ (mt->mt[mti-1] >> 30)) + mti); 
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array mt[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
        mt->mt[mti] &= 0xffffffffUL;
        /* for >32 bit machines */
    }
    mt->mti = mti;
}

/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
static void init_by_array(ScmMersenneTwister *mt,
                          unsigned long init_key[],
                          unsigned long key_length)
{
    int i, j, k;
    init_genrand(mt, 19650218UL);
    i=1; j=0;
    k = (N>key_length ? N : key_length);
    for (; k; k--) {
        mt->mt[i] = (mt->mt[i] ^ ((mt->mt[i-1] ^ (mt->mt[i-1] >> 30)) * 1664525UL))
            + init_key[j] + j; /* non linear */
        mt->mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++; j++;
        if (i>=N) { mt->mt[0] = mt->mt[N-1]; i=1; }
        if (j>=key_length) j=0;
    }
    for (k=N-1; k; k--) {
        mt->mt[i] = (mt->mt[i] ^ ((mt->mt[i-1] ^ (mt->mt[i-1] >> 30)) * 1566083941UL))
            - i; /* non linear */
        mt->mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++;
        if (i>=N) { mt->mt[0] = mt->mt[N-1]; i=1; }
    }

    mt->mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */ 
}

/* generates a random number on [0,0xffffffff]-interval */
inline unsigned long Scm_MTGenrandU32(ScmMersenneTwister *mt)
{
    unsigned long y;
    int mti = mt->mti;
    static unsigned long mag01[2]={0x0UL, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (mti >= N) { /* generate N words at one time */
        int kk;

        if (mti == N+1)   /* if init_genrand() has not been called, */
            init_genrand(mt, 5489UL); /* a default initial seed is used */

        for (kk=0;kk<N-M;kk++) {
            y = (mt->mt[kk]&UPPER_MASK)|(mt->mt[kk+1]&LOWER_MASK);
            mt->mt[kk] = mt->mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        for (;kk<N-1;kk++) {
            y = (mt->mt[kk]&UPPER_MASK)|(mt->mt[kk+1]&LOWER_MASK);
            mt->mt[kk] = mt->mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        y = (mt->mt[N-1]&UPPER_MASK)|(mt->mt[0]&LOWER_MASK);
        mt->mt[N-1] = mt->mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1UL];

        mti = 0;
    }
  
    y = mt->mt[mti++];

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    mt->mti = mti;
    return y;
}

/* generates a random number on (0,1)-real-interval */
/* this is modified from the original version to exclude 0,
   according to the proposed SRFI-27 */
float Scm_MTGenrandF32(ScmMersenneTwister *mt)
{
    float r;
    do {
        r = (float)(Scm_MTGenrandU32(mt)*(1.0/4294967296.0));
        /* divided by 2^32 */
    } while (r == 0.0); /*if we get 0.0, try another one. */;
    return r;
}

/* generates a random number on (0,1) with 53-bit resolution*/
double Scm_MTGenrandF64(ScmMersenneTwister *mt)
{
    double r;
    unsigned long a, b;
    do {
        a = Scm_MTGenrandU32(mt)>>5;
        b = Scm_MTGenrandU32(mt)>>6;
        r = (a*67108864.0+b)*(1.0/9007199254740992.0);
    } while (r == 0.0); /*if we get 0.0, try another one. */;
    return r;
} 

/*
 * Generic integer routine for [0, n-1]
 */

/* if integer N is 2^e, returns e; otherwise, returns -1. */
static inline int xlog2(unsigned long n)
{
#if SIZEOF_LONG == 4
# define START_BIT 16
# define MAX_BIT 31
#else /* assume sizeof(long) == 8 */
# define START_BIT 32
# define MAX_BIT 63
#endif
    int e = START_BIT;
    unsigned long m = (1UL<<16);

    if (n < m) {
        do {
            m >> 1;
            e--;
            if (n == m) return e;
        } while (e >= 0 && n < m);
    } else if (n > m) {
        do {
            m << 1;
            e++;
            if (n == m) return e;
        } while (e < MAX_BIT && n > m);
    } else { /* n == m */
        return e;
    }
    return -1;
#undef START_BIT
#undef MAX_BIT
}


/* generates a random number on [0,n-1], n < 2^32. */
static ScmObj genrand_int_small(ScmMersenneTwister *mt, unsigned long n)
{
    int e;
    unsigned long r;
    if ((e = xlog2(n)) >= 0) {
        /* optimize for 2^e case */
        r = Scm_MTGenrandU32(mt);
        return Scm_MakeInteger(r >> e);
    } else {
        double q = (double)0xffffffff / (double)(n-1);
        r = Scm_MTGenrandU32(mt);
        return Scm_MakeInteger((unsigned long)(r/q));
    }
}

#if 0
ScmObj Scm_MTGenrandInt(ScmMersenneTwister *mt, ScmObj n)
{
    /* special optimization if n == 2^e */
    if (SCM_BIGNUMP(n)) {
        
    }
    
    if (SCM_INTP(n)) {
        long m = SCM_INT_VALUE(n), q, r;
        int i;
        /* 
        

    } else if (SCM_BIGNUMP(n)) {
    }
  err:
    Scm_Error("bad type of argument for n: positive integer required, but got %S", n);
    return SCM_UNDEFINED; /*dummy*/
}
#endif

/*
 * Gauche specific stuff
 */
static ScmObj key_seed;
static ScmObj mt_allocate(ScmClass *klass, ScmObj initargs);
SCM_DEFINE_BUILTIN_CLASS(Scm_MersenneTwisterClass,
                         NULL, NULL, NULL, mt_allocate,
                         SCM_CLASS_DEFAULT_CPL);

static ScmObj mt_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmObj seed = Scm_GetKeyword(key_seed, initargs, SCM_FALSE);
    ScmMersenneTwister *mt;
    
    if (!SCM_FALSEP(seed) && !SCM_EXACTP(seed)) {
        Scm_Error("seed needs to be an exact integer, but got: %S", seed);
    }
    
    mt = SCM_NEW(ScmMersenneTwister);
    SCM_SET_CLASS(mt, &Scm_MersenneTwisterClass);
    mt->mti = N+1;
    if (SCM_EXACTP(seed)) {
        init_genrand(mt, Scm_GetUInteger(seed));
    }
    return SCM_OBJ(mt);
}

extern Scm_Init_mt_lib(ScmModule*);

void Scm_Init_mt_random(void)
{
    ScmModule *mod = SCM_MODULE(SCM_FIND_MODULE("math.mt-random", TRUE));
    Scm_InitBuiltinClass(&Scm_MersenneTwisterClass, "<mersenne-twister>",
                         NULL, sizeof(ScmMersenneTwister), mod);
    key_seed = SCM_MAKE_KEYWORD("seed");
    Scm_Init_mt_lib(mod);
}

