/*
 * Example dynamic loading library
 */

#include <gauche.h>

static ScmObj example(ScmObj *args, int nargs, void *data)
{
    return Scm_Cons(SCM_INTERN("example"), args[0]);
}

void Scm_Init_example(void)
{
    ScmModule *module = SCM_MODULE(SCM_FIND_MODULE("example", TRUE));
    ScmObj subr = Scm_MakeSubr(example, NULL, 0, 1, SCM_MAKE_STR("example"));
    SCM_DEFINE(module, "example", subr);
}

