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
 *  $Id: main.c,v 1.40 2002-01-08 05:58:55 shirok Exp $
 */

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include "gauche.h"

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

/* options */
int load_initfile = TRUE;       /* if false, not to load init files */
int batch_mode = FALSE;         /* force batch mode */
int interactive_mode = FALSE;   /* force interactive mode */
ScmObj extra_load_paths = SCM_NIL; /* -I path */
ScmObj extra_loads = SCM_NIL;   /* -u modules (symbol) and -l files (string) */
ScmObj eval_expr = SCM_NIL;     /* -e expr */

void usage(void)
{
    fprintf(stderr,
            "Usage: gosh [-biqV][-I<path>][-u<module>][--] [file]\n"
            "options:\n"
            "  -V       print version and exit.\n"
            "  -b       batch mode.  don't print prompts.  supersedes -i.\n"
            "  -i       interactive mode.  force to print prompts.\n"
            "  -q       don't read the default initialization file.\n"
            "  -I<path> add <path> to the head of load path.\n"
            "  -u<module> (use) load and import <module>\n"
            "  -f<flag> sets various flags\n"
            "      no-inline       don't inline primitive procedures\n"
            "      no-source-info  don't preserve source information for debug\n"
            "      load-verbose    report while loading files\n"
            );
    exit(1);
}

void version(void)
{
    printf("Gauche scheme interpreter, version %s [%s]\n",
           GAUCHE_VERSION, SCM_CHAR_ENCODING_NAME);
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
    else if (strcmp(optarg, "load-verbose") == 0) {
        Scm_VM()->runtimeFlags |= SCM_LOAD_VERBOSE;
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
    while ((c = getopt(argc, argv, "+be:iql:u:Vf:I:-")) >= 0) {
        switch (c) {
        case 'b': batch_mode = TRUE; break;
        case 'i': interactive_mode = TRUE; break;
        case 'q': load_initfile = FALSE; break;
        case 'V': version(); break;
        case 'f': further_options(optarg); break;
        case 'u':
            extra_loads = Scm_Cons(SCM_INTERN(optarg), extra_loads);
            break;
        case 'l':
            extra_loads = Scm_Cons(SCM_MAKE_STR_COPYING(optarg), extra_loads);
            break;
        case 'I':
            extra_load_paths = Scm_Cons(SCM_MAKE_STR_COPYING(optarg),
                                        extra_load_paths);
            break;
        case 'e':
            eval_expr = Scm_Cons(Scm_ReadFromCString(optarg), eval_expr);
        case '-': break;
        case '?': usage(); break;
        }
    }
    SCM_FOR_EACH(cp, extra_load_paths) {
        Scm_AddLoadPath(Scm_GetStringConst(SCM_STRING(SCM_CAR(cp))), FALSE);
    }

    /* load init file */
    if (load_initfile) {
        SCM_UNWIND_PROTECT {
            Scm_Load("gauche-init.scm", TRUE);
        }
        SCM_WHEN_ERROR {
            fprintf(stderr, "Error in initialization file.\n");
        }
        SCM_END_PROTECT;
    }

    /* pre-load specified modules */
    if (!SCM_NULLP(extra_loads)) {
        ScmObj m;
        SCM_FOR_EACH(m, Scm_Reverse(extra_loads)) {
            ScmObj mod = SCM_CAR(m), p, path;

            if (SCM_SYMBOLP(mod)) {
                p = Scm_StringSplitByChar(SCM_SYMBOL_NAME(mod), '.');
                path = Scm_StringJoin(p, SCM_STRING(SCM_MAKE_STR("/")),
                                      SCM_STRING_JOIN_INFIX);
                Scm_Require(path);
                Scm_ImportModules(SCM_CURRENT_MODULE(), SCM_LIST1(mod));
            } else if (SCM_STRINGP(mod)) {
                Scm_Load(Scm_GetStringConst(SCM_STRING(mod)), TRUE);
            }
        }
    }

    /* pre-evaluate -e experssions */
    if (!SCM_NULLP(eval_expr)) {
        ScmObj e;
        SCM_FOR_EACH(e, Scm_Reverse(eval_expr)) {
            ScmObj expr = SCM_CAR(e);
            Scm_Eval(expr, SCM_OBJ(Scm_UserModule()));
        }
    }

    /* If script file is specified, load it. */
    if (optind < argc) {
        ScmObj av = SCM_NIL, at = SCM_NIL, mainproc;
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
    if (load_initfile) {
        SCM_UNWIND_PROTECT {
            Scm_Require(SCM_MAKE_STR("gauche/interactive"));
            Scm_ImportModules(SCM_CURRENT_MODULE(),
                              SCM_LIST1(SCM_INTERN("gauche.interactive")));
        }
        SCM_WHEN_ERROR {
            fprintf(stderr, "warning: couldn't load gauche.interactive\n");
        }
        SCM_END_PROTECT;
    }

    if (batch_mode || (!isatty(0) && !interactive_mode)) {
        Scm_LoadFromPort(SCM_PORT(Scm_Stdin()));
    } else {
        Scm_Repl(SCM_MAKE_STR("gosh> "),
                 SCM_PORT(Scm_Stdin()),
                 SCM_PORT(Scm_Stdout()));
    }
    return 0;
}
