/* test program */

#include <unistd.h>
#include "gauche.h"

int main(int argc, char **argv)
{
    int c;
    
    Scm_Init();

    while ((c = getopt(argc, argv, "gI")) >= 0) {
        switch (c) {
        case 'g': Scm_VM()->debugCompile = TRUE; break;
        case 'I': Scm_VM()->enableInline = FALSE; break;
        }
    }

    if (optind < argc) {
        /* file name passed. */
        Scm_Load(argv[optind]);
        exit(0);
    }

    Scm_Repl(SCM_PORT(Scm_Stdin()), SCM_PORT(Scm_Stdout()));

    return 0;
}
