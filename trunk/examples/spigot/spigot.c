/*
 * spigot.c - calculate pi and e by spigot algorithm
 *
 *  Written by Shiro Kawai (shiro@acm.org)
 *  I put this program in public domain.  Use it as you like.
 */

#include <gauche.h>
#include <gauche/extend.h>
#include <math.h>
#include "spigot.h"

/* The spigot algorithm for pi:

  pi = 2 + 1/3*(2 + 2/5*(2 + 3/7*(2 + ... (2 + k/(2k+1)*(2 + ...)) ...)))  :[1]

  The last term can be approximated by:

       2 + k/(2k+1)*(4)

  where

       k = n * log2 (10)

  for a desired precision of n decimal digits.

  Now, pi is 3.14159 ... in decimal.  That means:

  pi = 3 + 1/10*(1 + 1/10*(4 + 1/10*(1 + 1/10*(5 + 1/10*(9 + ....)))))    :[2]

  Note the similality of [1] and [2].  If we think a special variable
  base system that the k-th digit after point has the weight of k/(2k+1)
  of the previous digit, [1] means pi can be writen as [2.222222...]
  in such a base system.
  
    Base:   [1/3, 2/5, 3/7, ... , k/(2k+1), ...]
    Number: [2 . 222222222 ...]

    Base:   [1/10, 1/10, 1/10, ....]
    Number: [3 . 141592653 ...]

  Now, calculating pi is effectively a base conversion problem.
  
  The actual calculation process is explained in detail at:
   http://membres.lycos.fr/bgourevitch/mathematiciens/goutte/goutte.html

  It uses a table, but the rows "A" and "B" are mechanically calculated,
  and the rows "retenue", "somme", and "reste" can share the same array
  for those columns are calculated left to right for each iteration.
  Thus we need to keep only one array to hold intermediate calculation.

  E can be calculated in the same way, starting from the following number:

    Base:   [1, 1/2, 1/3, 1/4, ... , 1/(k-1), ...]
    Number: [1 . 11111111 ...]

  The following code is a very naive implementation of the algorithm.
  You would get less memory footprint and faster calculation by using
  big digit, like 10000 instead of 10.
*/

ScmObj Spigot_calculate_pi(int digits)
{
    int k, i, j, l, b, q, r, *array;
    ScmObj rvec, *relts;

    if (digits <= 0) Scm_Error("digits must be a positive integer");

    /* Scheme vector to keep the result */
    rvec = Scm_MakeVector(digits, SCM_MAKE_INT(0));
    relts = SCM_VECTOR_ELEMENTS(rvec);

    /* Prepare the array for variable base system */
    k = (int)floor(digits * 3.3219280948873626);
    array = SCM_NEW_ATOMIC2(int *, (k+1)*sizeof(int));
    for (i=0; i<k; i++) array[i] = 2;
    array[k] = 4;

    for (i=0, b=2; i<digits; i++) {
        q = 0;
        for (j=k; j>0; j--) {
            q += array[j] * 10;
            array[j] = q % (2*j+1);
            q /= 2*j+1;
            q *= j;
        }
        r = b + q/10;
        b = q % 10;
        /* Here, we have the i-th digit in r.
           In rare occasions, r becomes more than 10, and we need to back-up
           to increment the previous digit(s).  (It's rarely the case that
           this back-up cascades for more than one digit). */
        if (r < 10) {
            relts[i] = SCM_MAKE_INT(r);
        } else {
            relts[i] = SCM_MAKE_INT(r%10);
            for (l=i-1, r/=10; r && l>=0; l--, r/=10) {
                r += SCM_INT_VALUE(relts[l]);
                relts[l] = SCM_MAKE_INT(r%10);
            }
        }
    }
    return rvec;
}

ScmObj Spigot_calculate_e(int digits)
{
    int k, i, j, l, b, q, r, *array;
    ScmObj rvec, *relts;

    if (digits <= 0) Scm_Error("digits must be a positive integer");

    /* Scheme vector to keep the result */
    rvec = Scm_MakeVector(digits, SCM_MAKE_INT(0));
    relts = SCM_VECTOR_ELEMENTS(rvec);

    /* Prepare the array for variable base system */
    k = (int)floor(digits * 3.3219280948873626);
    array = SCM_NEW_ATOMIC2(int *, (k+1)*sizeof(int));
    for (i=0; i<k; i++) array[i] = 1;
    array[k] = 2;

    for (i=0, b=1; i<digits; i++) {
        q = 0;
        for (j=k; j>0; j--) {
            q += array[j] * 10;
            array[j] = q % j;
            q /= j;
        }
        r = b + q/10;
        b = q % 10;
        /* Here, we have the i-th digit in r.
           In rare occasions, r becomes more than 10, and we need to back-up
           to increment the previous digit(s).  (It's rarely the case that
           this back-up cascades for more than one digit). */
        if (r < 10) {
            relts[i] = SCM_MAKE_INT(r);
        } else {
            relts[i] = SCM_MAKE_INT(r%10);
            for (l=i-1, r/=10; r && l>=0; l--, r/=10) {
                r += SCM_INT_VALUE(relts[l]);
                relts[l] = SCM_MAKE_INT(r%10);
            }
        }
    }
    return rvec;
}

/*
 * Module initialization function.
 * This is called when spigot.so is dynamically loaded into gosh.
 */
ScmObj Scm_Init_spigot(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(spigot);

    /* Create "spigot" module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("spigot", TRUE));

    /* Register stub-generated procedures inside "spigot" module */
    Scm_Init_spigotlib(mod);
}

