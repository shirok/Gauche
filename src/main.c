/* test program */

#include <unistd.h>
#include "gauche.h"

int show_compile = 0;

void toplevel(void)
{
    volatile ScmObj in, out, err, v;
    ScmVM *vm = Scm_VM();
    in = Scm_Stdin();
    out = Scm_Stdout();
    err = Scm_Stderr();

    for (;;) {
        SCM_PUSH_ERROR_HANDLER {
            ScmObj c;

            for (;;) {
                SCM_PUTCSTR("gosh> ", out);
                SCM_FLUSH(out);
                v = Scm_Read(in);
                if (SCM_EOFP(v)) break;
                v = Scm_Compile(v);
                if (show_compile) Scm_Printf(SCM_PORT(out), "== %S\n", v);

                Scm_Run(v);

                SCM_FOR_EACH(c, Scm_VMGetResult(vm)) {
                    Scm_Printf(SCM_PORT(out), "%S\n", SCM_CAR(c));
                }
            }
        }
        SCM_WHEN_ERROR {
            ScmObj stack = Scm_VMGetStack(vm), cp;
            int depth = 0;

            SCM_PUTCSTR("*** ERROR: ", err);
            if (SCM_STRINGP(vm->errstr)) SCM_PUTS(vm->errstr, err);
            SCM_PUTNL(err);

            SCM_PUTCSTR("Stack Trace:\n", err);
            SCM_PUTCSTR("_______________________________________\n", err);
            SCM_FOR_EACH(cp, stack) {
                Scm_Printf(SCM_PORT(err), "%3d   %66.1S\n",
                           depth++, SCM_CAR(cp));
            }
        }
        SCM_POP_ERROR_HANDLER;
    }
}

int main(int argc, char **argv)
{
    int c;
    
    while ((c = getopt(argc, argv, "g")) >= 0) {
        switch (c) {
        case 'g': show_compile = 1;
        }
    }
    
    Scm_Init();

    toplevel();

    return 0;
}
