/*
 * Test VM stack sanity
 */

#include <stdio.h>
#include "gauche.h"
#include "gauche/vm.h"

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

void test_eval(const char *msg, const char *sexp)
{
    ScmObj *pre_stack = Scm_VM()->sp, *post_stack;
    ScmObj x = Scm_ReadFromCString(sexp);
    printf("%s ... ", msg);
    Scm_Eval(x, SCM_UNBOUND, NULL); /* ignore errors */

    post_stack = Scm_VM()->sp;
    if (pre_stack != post_stack) {
        printf("ERROR.\n");
        errcount++;
    } else {
        printf("ok\n");
    }
}

ScmObj dummy_eproc(ScmObj *args, int nargs, void *data)
{
    return SCM_UNDEFINED;
}

int main(int argc, char **argv)
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
    } else {
        fprintf(stderr, "passed.\n");
        fprintf(stdout, "passed.\n");
    }
    return 0;
}
