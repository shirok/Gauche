/*
 * @@extname@@.c
 */

#include "@@extname@@.h"

/*
 * The following function is a dummy one; replace it for
 * your C function definitions.
 */

ScmObj test_@@extname@@(void)
{
    return SCM_MAKE_STR("@@extname@@ is working");
}

/*
 * Module initialization function.
 */
extern void Scm_Init_@@extname@@lib(ScmModule*);

void Scm_Init_@@extname@@(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(@@extname@@);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("@@modname@@", TRUE));

    /* Register stub-generated procedures */
    Scm_Init_@@extname@@lib(mod);
}
