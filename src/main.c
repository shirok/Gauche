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
 *  $Id: main.c,v 1.27 2001-08-06 07:38:22 shirok Exp $
 */

#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include "gauche.h"

int load_initfile = TRUE;
int batch_mode = FALSE;
ScmObj extra_load_paths = SCM_NIL;
ScmObj use_modules = SCM_NIL;

void usage(void)
{
    fprintf(stderr,
            "Usage: gosh [-qV][-I<path>][-u<module>][--] [file]\n"
            "options:\n"
            "  -V       print version and exit.\n"
            "  -q       don't read the default initiailzation file.\n"
            "  -I<path> add <path> to the head of load path.\n"
            "  -u<module> (use) load and import <module>\n"
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
    while ((c = getopt(argc, argv, "bqu:Vf:I:-")) >= 0) {
        switch (c) {
        case 'b': batch_mode = TRUE; break;
        case 'q': load_initfile = FALSE; break;
        case 'V': version(); break;
        case 'f': further_options(optarg); break;
        case 'u':
            use_modules = Scm_Cons(SCM_INTERN(optarg), use_modules);
            break;
        case 'I':
            extra_load_paths = Scm_Cons(SCM_MAKE_STR_COPYING(optarg),
                                        extra_load_paths);
            break;
        case '-': break;
        case '?': usage(); break;
        }
    }
    SCM_FOR_EACH(cp, extra_load_paths) {
        Scm_AddLoadPath(Scm_GetStringConst(SCM_STRING(SCM_CAR(cp))), FALSE);
    }

    /* set up load paths */
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

    /* load init file */
    if (load_initfile) {
        SCM_PUSH_ERROR_HANDLER {
            Scm_Load("gauche-init.scm", TRUE);
        }
        SCM_WHEN_ERROR {
            fprintf(stderr, "Error in initialization file.\n");
        }
        SCM_POP_ERROR_HANDLER;
    }

    /* pre-load specified modules */
    if (!SCM_NULLP(use_modules)) {
        ScmObj m;
        SCM_FOR_EACH(m, Scm_Reverse(use_modules)) {
            ScmObj mod = SCM_CAR(m);
            ScmObj p = Scm_StringSplitByChar(SCM_SYMBOL_NAME(mod), '.');
            ScmObj path = Scm_StringJoin(p, SCM_STRING(SCM_MAKE_STR("/")),
                                         SCM_STRING_JOIN_INFIX);
            Scm_Require(path);
            Scm_ImportModules(SCM_CURRENT_MODULE(), SCM_LIST1(mod));
        }
    }

    /* if script file is specified, load it. */
    if (optind < argc) {
        ScmObj av = SCM_NIL, at, mainproc;
        int ac;
        for (ac = optind+1; ac < argc; ac++) {
            SCM_APPEND1(av, at, SCM_MAKE_STR_IMMUTABLE(argv[ac]));
        }
        SCM_DEFINE(Scm_UserModule(), "*argv*", av);
        SCM_DEFINE(Scm_UserModule(), "*program-name*",
                   SCM_MAKE_STR_IMMUTABLE(argv[optind]));
        Scm_Load(argv[optind], TRUE);

        /* if symbol 'main is bound to a procedure in the user module,
           call it.  (SRFI-22) */
        mainproc = Scm_SymbolValue(Scm_UserModule(),
                                   SCM_SYMBOL(SCM_INTERN("main")));
        if (SCM_PROCEDUREP(mainproc)) {
            ScmObj result = Scm_Apply(mainproc, SCM_LIST1(av));
            if (SCM_INTP(result)) exit(SCM_INT_VALUE(result));
        }
        exit(0);
    }

    /* now, we're in the interactive mode. */
    SCM_DEFINE(Scm_UserModule(), "*argv*", SCM_NIL);
    SCM_DEFINE(Scm_UserModule(), "*program-name*",
               SCM_MAKE_STR_IMMUTABLE(argv[0]));

    /* (use gauche.interactive) only for interactive session */
    SCM_PUSH_ERROR_HANDLER {
        Scm_Require(SCM_MAKE_STR("gauche/interactive"));
        Scm_ImportModules(SCM_CURRENT_MODULE(),
                          SCM_LIST1(SCM_INTERN("gauche.interactive")));
    }
    SCM_WHEN_ERROR {
        fprintf(stderr, "warning: couldn't load gauche.interactive\n");
    }
    SCM_POP_ERROR_HANDLER;

    if (batch_mode || !isatty(0)) {
        Scm_LoadFromPort(SCM_PORT(Scm_Stdin()));
    } else {
        Scm_Repl(SCM_MAKE_STR("gosh> "),
                 SCM_PORT(Scm_Stdin()),
                 SCM_PORT(Scm_Stdout()));
    }
    return 0;
}
