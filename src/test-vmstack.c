/*
 * Test VM stack sanity
 */

#include <stdio.h>
#include "gauche.h"
#include "gauche/vm.h"
#include "gauche/priv/vmP.h"

int errcount = 0;

void message(FILE *out, const char *m, int filler)
{
    int i;
    fprintf(out, "%s", m);
    if (filler) {
        int len = 79 - (int)strlen(m);
        if (len < 0) len = 5;
        for (i=0; i<len; i++) putc(filler, out);
    }
    putc('\n', out);
}

int is_valid_stack_outcome(ScmObj *pre_sp, ScmVM *post_vm)
{
    ScmObj *post_sp = post_vm->sp;

    if (pre_sp == post_sp) return TRUE;

    /* Boundary cont frame pushes ScmPromptData before it.  After boundary
       cont frame is popped, ScmPromptData still remains.  (It will eventually
       be GC-ed when active frames get moved to the heap.)
    */
    if (pre_sp + (sizeof(ScmPromptData)/sizeof(ScmObj)) == post_sp)
        return TRUE;

    /* Stack has been emptied. */
    if (post_sp == post_vm->stack) return TRUE;

    return FALSE;
}

void test_eval(const char *msg, const char *sexp)
{
    ScmVM *vm = Scm_VM();
    ScmObj *pre_sp = vm->sp;
    ScmObj x = Scm_ReadFromCString(sexp);
    printf("%s ... ", msg);
    Scm_Eval(x, SCM_UNBOUND, NULL); /* ignore errors */

    if (!is_valid_stack_outcome(pre_sp, vm)) {
        printf("ERROR\n");
        errcount++;
    } else {
        printf("ok\n");
    }
}

ScmObj dummy_eproc(ScmObj *args SCM_UNUSED,
                   int nargs SCM_UNUSED,
                   void *data SCM_UNUSED)
{
    return SCM_UNDEFINED;
}

int main(int argc SCM_UNUSED, char **argv SCM_UNUSED)
{
    ScmObj eproc;
    const char *testmsg = "Testing VM stack sanity... ";

    fprintf(stderr, "%-65s", testmsg);
    message(stdout, testmsg, '=');
    Scm_Init(GAUCHE_SIGNATURE);

    eproc = Scm_MakeSubr(dummy_eproc, NULL, 0, 1, SCM_FALSE);
    Scm_VM()->customErrorReporter = eproc;

    test_eval("simple expression", "(+ 1 2 3)");
    test_eval("with-error-handler (1)",
              "(with-error-handler (lambda (e) #f) (lambda () 1)))");
    test_eval("with-error-handler (2)",
              "(with-error-handler (lambda (e) #f) (lambda () (car 1))))");
    test_eval("with-error-handler (3)",
              "(car 3)");

    if (errcount) {
        fprintf(stderr, "failed.\n");
        fprintf(stdout, "failed.\n");
        return 1;
    } else {
        fprintf(stderr, "passed.\n");
        fprintf(stdout, "passed.\n");
        return 0;
    }
}
