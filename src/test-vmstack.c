/* 
 * Test VM stack sanity
 */

#include <stdio.h>
#include "gauche.h"
#include "gauche/vm.h"

int errcount = 0;

void test_eval(const char *msg, const char *sexp)
{
    ScmObj *pre_stack = Scm_VM()->sp, *post_stack;
    ScmObj x = Scm_ReadFromCString(sexp);
    printf("%s ... ", msg);
    SCM_UNWIND_PROTECT {
        Scm_Eval(x, SCM_UNBOUND);
    }
    SCM_WHEN_ERROR {
    }
    SCM_END_PROTECT;
        
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

    fprintf(stderr, "Testing VM stack sanity... ");
    Scm_Init();
    
    eproc = Scm_MakeSubr(dummy_eproc, NULL, 0, 1, SCM_FALSE);
    Scm_VM()->defaultEscapeHandler = eproc;
    
    test_eval("simple expression", "(+ 1 2 3)");
    test_eval("with-error-handler (1)",
              "(with-error-handler (lambda (e) #f) (lambda () 1)))");
    test_eval("with-error-handler (2)",
              "(with-error-handler (lambda (e) #f) (lambda () (car 1))))");
    test_eval("with-error-handler (2)",
              "(car 3)");

    if (errcount == 0) {
        fprintf(stderr, "passed.\n");
    } else {
        fprintf(stderr, "dispcrepancy found.\n");
    }
    return 0;
}
