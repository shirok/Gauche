/* test program */

#include <unistd.h>
#include "gauche.h"

int show_compile = 0;
int allow_inline = 1;

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
        exit(0);
    }

    Scm_Repl(SCM_PORT(Scm_Stdin()), SCM_PORT(Scm_Stdout()));

    return 0;
}
