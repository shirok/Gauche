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
 *  $Id: main.c,v 1.54 2002-07-09 04:56:45 shirok Exp $
 */

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <signal.h>
#include "gauche.h"

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

/* options */
int load_initfile = TRUE;       /* if false, not to load init files */
int batch_mode = FALSE;         /* force batch mode */
int interactive_mode = FALSE;   /* force interactive mode */
int srfi22_mode = TRUE;         /* 'main' behaves strictly as in SRFI-22,
                                   or backward-compatible way.   This is a
                                   temporary flag and will be removed later. */
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
            "      case-fold       uses case-insensitive reader (as in R5RS)\n"
            "      compat-0.5      'main' behaves backward-compatible for 0.5 and before\n"
            "      load-verbose    report while loading files\n"
            "      no-inline       don't inline primitive procedures\n"
            "      no-source-info  don't preserve source information for debug\n"
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
    else if (strcmp(optarg, "case-fold") == 0) {
        Scm_VM()->runtimeFlags |= SCM_CASE_FOLD;
    }
    else if (strcmp(optarg, "compat-0.5") == 0) {
        srfi22_mode = FALSE;
    }
    else {
        fprintf(stderr, "unknown -f option: %s\n", optarg);
        fprintf(stderr, "supported options are: -fcase-fold or -fcompat-0.5, -fload-verbose, -fno-inline, -fno-source-info\n");
        exit(1);
    }
}

int parse_options(int argc, char *argv[])
{
    int c;
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
    return optind;
}

/* signal handler setup.  let's catch as many signals as possible. */
static void sig_setup(void)
{
    sigset_t set;
    sigfillset(&set);
    sigdelset(&set, SIGABRT);
    sigdelset(&set, SIGILL);
    sigdelset(&set, SIGKILL);
    sigdelset(&set, SIGCONT);
    sigdelset(&set, SIGSTOP);
    sigdelset(&set, SIGSEGV);
    sigdelset(&set, SIGPWR);  /* for now; this signal is used in gc */
    sigdelset(&set, SIGXCPU); /* for now; this signal is used in gc */
    Scm_SetMasterSigmask(&set);
}

/*-----------------------------------------------------------------
 * MAIN
 */
int main(int argc, char **argv)
{
    int argind;
    ScmObj cp;

#ifdef __CYGWIN__
    /* Cygwin needs explicit initialization for GC module.
       This code is taken from gc.h and gcconfig.h (I don't want to
       include private/gcconfig.h)
       May not work except cygwin 1.3.x */
    extern int _data_start__;
    extern int _bss_end__;
    GC_add_roots((void*)&_data_start__, (void*)&_bss_end__);
#endif
    Scm_Init();
    sig_setup();

    /* For backward compatibility -- see the notes about SRFI-22 below. */
    if (getenv("GAUCHE_COMPAT_0_5") != NULL) {
        srfi22_mode = FALSE;
    }

    /* Special case; if the binary is invoked as "./gosh", we may be in
       the source tree.  Adds . and ../lib to the library path.
       This feature is turned off if we're run by root or suid-ed. */
    if (strcmp(argv[0], "./gosh") == 0
        && access("./gauche.h", R_OK) == 0
        && access("./gauche-init.scm", R_OK) == 0
        && access("../lib/gauche/object.scm", R_OK) == 0
        && geteuid() != 0 && getuid() == geteuid()) {
        Scm_AddLoadPath("../lib", FALSE);
        Scm_AddLoadPath(".", FALSE);
    }

    argind = parse_options(argc, argv);

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
        ScmObj av = SCM_NIL, at = SCM_NIL, mainproc, result;
        int ac;
        struct stat statbuf;
        const char *scriptfile;

        /* if the script name is given in relative pathname, see if
           it exists from the current directory.  if not, leave it
           to load() to search in the load paths */
        if (argv[optind][0] == '\0') Scm_Error("bad script name");
        if (argv[optind][0] == '/') {
            scriptfile = argv[optind];
        } else {
            if (stat(argv[optind], &statbuf) == 0) {
                ScmDString ds;
                Scm_DStringInit(&ds);
                Scm_DStringPutz(&ds, "./", -1);
                Scm_DStringPutz(&ds, argv[optind], -1);
                scriptfile = Scm_DStringGetz(&ds);
            } else {
                scriptfile = argv[optind];
            }
        }

        /* sets up arguments. */
        for (ac = optind; ac < argc; ac++) {
            SCM_APPEND1(av, at, SCM_MAKE_STR_IMMUTABLE(argv[ac]));
        }
        SCM_DEFINE(Scm_UserModule(), "*argv*", SCM_CDR(av));
        SCM_DEFINE(Scm_UserModule(), "*program-name*", SCM_CAR(av));

        /* load the file */
        Scm_Load(scriptfile, TRUE);

        /* if symbol 'main is bound to a procedure in the user module,
           call it.  (SRFI-22)
           NB: prior to 0.5.1, 'main' got the cmdline arguments without
           the script name itself.  SRFI-22 specifies the first element
           of the arg list is the script name.   The user can set
           -fcompat-0.5 flag or environment variable GAUCHE_COMPAT_0_5
           to keep the previous behavior.  (This backward compatibility
           will be removed after a while.) */
        mainproc = Scm_SymbolValue(Scm_UserModule(),
                                   SCM_SYMBOL(SCM_INTERN("main")));
        if (SCM_PROCEDUREP(mainproc)) {
            if (srfi22_mode) {
                result = Scm_Apply(mainproc, SCM_LIST1(av));
                if (SCM_INTP(result)) exit(SCM_INT_VALUE(result));
                else exit(70);  /* EX_SOFTWARE, see SRFI-22. */
            } else {
                result = Scm_Apply(mainproc, SCM_LIST1(SCM_CDR(av)));
                if (SCM_INTP(result)) exit(SCM_INT_VALUE(result));
            }
        }
        Scm_Exit(0);
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
        Scm_Repl(SCM_FALSE, SCM_FALSE, SCM_FALSE, SCM_FALSE);
    }
    Scm_Exit(0);
    return 0;                   /* dummy */
}
