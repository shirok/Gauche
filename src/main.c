/*
 * main.c - interpreter main program
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: main.c,v 1.20 2001-04-14 23:52:20 shiro Exp $
 */

#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include "gauche.h"

int load_initfile = TRUE;
ScmObj extra_load_paths = SCM_NIL;

void usage(void)
{
    fprintf(stderr,
            "Usage: gosh [-qV][-I<path>] [--] [file]\n"
            "options:\n"
            "  -V       print version and exit.\n"
            "  -q       don't read the default initiailzation file.\n"
            "  -I<path> add <path> to the head of load path (multiple -I's are allowed).\n"
        );
    exit(1);
}

void version(void)
{
    printf("Gauche scheme interpreter, version %s\n", GAUCHE_VERSION);
    exit(0);
}

void further_options(const char *optarg)
{
    if (strcmp(optarg, "no-inline") == 0) {
        Scm_VM()->compilerFlags |= SCM_COMPILE_NOINLINE;
    }
    else if (strcmp(optarg, "debug-compiler") == 0) {
        Scm_VM()->compilerFlags |= SCM_COMPILE_SHOWRESULT;
    }
    else if (strcmp(optarg, "no-source-info") == 0) {
        Scm_VM()->compilerFlags |= SCM_COMPILE_NOSOURCE;
    }
    else {
        fprintf(stderr, "unknown -f option: %s\n", optarg);
        fprintf(stderr, "supported options are: -fno-inine, -fdebug-compiler, -fno-source-info\n");
        exit(1);
    }
}

/*-----------------------------------------------------------------
 * MAIN
 */
int main(int argc, char **argv)
{
    int c;
    ScmObj cp;

    Scm_Init();
    while ((c = getopt(argc, argv, "qVf:I:-")) >= 0) {
        switch (c) {
        case 'q': load_initfile = FALSE; break;
        case 'V': version(); break;
        case 'f': further_options(optarg); break;
        case 'I':
            extra_load_paths = Scm_Cons(Scm_MakeString(optarg, -1, -1),
                                        extra_load_paths);
            break;
        case '-': break;
        case '?': usage(); break;
        }
    }
    SCM_DEFINE(Scm_UserModule(), "*program-name*",
               Scm_MakeString(argv[0], -1, -1));
    SCM_FOR_EACH(cp, extra_load_paths) {
        Scm_AddLoadPath(Scm_GetStringConst(SCM_STRING(SCM_CAR(cp))), FALSE);
    }

    if (geteuid() != 0) {     /* don't add extra paths when run by root */
        struct stat statbuf;
        if (stat("../lib/gauche", &statbuf) >= 0
            && S_ISDIR(statbuf.st_mode)) {
            /* This is the case that the interpreter is invoked in place
               of the compilation. */
            Scm_AddLoadPath("../lib", FALSE);
        }
        Scm_AddLoadPath(".", FALSE);
        
    }
    if (load_initfile) {
        SCM_PUSH_ERROR_HANDLER {
            Scm_Load("gauche-init.scm", TRUE);
        }
        SCM_WHEN_ERROR {
            fprintf(stderr, "Error in initialization file.\n");
        }
        SCM_POP_ERROR_HANDLER;
    }

    if (optind < argc) {
        ScmObj av = SCM_NIL, at;
        int ac;
        for (ac = optind+1; ac < argc; ac++) {
            SCM_APPEND1(av, at, Scm_MakeString(argv[ac], -1, -1));
        }
        SCM_DEFINE(Scm_UserModule(), "*argv*", av);
        Scm_Load(argv[optind], TRUE);
        exit(0);
    } else {
        SCM_DEFINE(Scm_UserModule(), "*argv*", SCM_NIL);
    }

    Scm_Repl(SCM_MAKE_STR("gosh> "),
             SCM_PORT(Scm_Stdin()),
             SCM_PORT(Scm_Stdout()));

    return 0;
}
