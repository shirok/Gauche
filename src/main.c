/* test program */

#include <unistd.h>
#include "gauche.h"

int show_compile = 0;
int allow_inline = 1;




void toplevel(void)
{
    volatile ScmObj in, out, err, v;
    ScmVM *vm = Scm_VM();
    in = Scm_Stdin();
    out = Scm_Stdout();
    err = Scm_Stderr();

    vm->enableInline = allow_inline;

    for (;;) {
        SCM_PUSH_ERROR_HANDLER {
            ScmObj c;

            for (;;) {
                SCM_PUTCSTR("gosh> ", out);
                SCM_FLUSH(out);
                v = Scm_Read(in);
                if (SCM_EOFP(v)) break;
                v = Scm_Compile(v, SCM_NIL, -1);
                if (show_compile) Scm_Printf(SCM_PORT(out), "== %S\n", v);

                Scm_Run(v);

                SCM_FOR_EACH(c, Scm_VMGetResult(vm)) {
                    Scm_Printf(SCM_PORT(out), "%S\n", SCM_CAR(c));
                }
            }
        }
        SCM_WHEN_ERROR {
        }
        SCM_POP_ERROR_HANDLER;
    }
}

int main(int argc, char **argv)
{
    int c;
    
    while ((c = getopt(argc, argv, "gI")) >= 0) {
        switch (c) {
        case 'g': show_compile = 1; break;
        case 'I': allow_inline = 0; break;
        }
    }

    Scm_Init();

    if (optind < argc) {
        /* file name passed. */
        Scm_Load(argv[optind]);
        Scm_Cont();
        exit(0);
    }

    toplevel();

    return 0;
}
