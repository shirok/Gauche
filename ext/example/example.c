/*
 * Example dynamic loading library
 */

#include <gauche.h>
#include <gauche/extend.h>

static ScmObj example(ScmObj *args, int nargs, void *data)
{
    return Scm_Cons(SCM_INTERN("example"), args[0]);
}

void Scm_Init_example(void)
{
    ScmModule *module;
    ScmObj subr;
    SCM_INIT_EXTENSION(example);
    module = SCM_MODULE(SCM_FIND_MODULE("example", TRUE));
    subr = Scm_MakeSubr(example, NULL, 0, 1, SCM_MAKE_STR("example"));
    SCM_DEFINE(module, "example", subr);
}

