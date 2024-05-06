/*
 * spigot_subdir.c
 */

#include "spigot_subdir.h"

/*
 * The following function is a dummy one; replace it for
 * your C function definitions.
 */

ScmObj test_spigot_subdir(void)
{
    return SCM_MAKE_STR("spigot_subdir is working");
}

/*
 * Module initialization function.
 */
extern void Scm_Init_spigot_subdirlib(ScmModule*);

void Scm_Init_spigot_subdir(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(spigot_subdir);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("math.spigot", TRUE));

    /* Register stub-generated procedures */
    Scm_Init_spigot_subdirlib(mod);
}
