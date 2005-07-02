/*
 * @@extname@@.c
 */

#include "@@extname@@.h"

/*
 * Put your C function definitions here
 */

/*
 * Module initialization function
 */
extern void Scm_Init_@@extname@@lib(ScmModule*);

ScmObj Scm_Init_@@extname@@(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(@@extname@@);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("@@extname@@", TRUE));

    /* Register stub-generated procedures */
    Scm_Init_@@extname@@lib(mod);
}
