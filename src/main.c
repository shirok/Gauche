/*
 * main.c - interpreter main program
 *
 *  Copyright(C) 2000-2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: main.c,v 1.64 2002-11-21 05:21:00 shirok Exp $
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
int test_mode = FALSE;          /* add . and ../lib implicitly  */

ScmObj pre_cmds = SCM_NIL;      /* assoc list of commands that needs to be
                                   processed before entering repl.
                                   Each car has either #\I, #\A, #\u, #\l
                                   or #\e, according to the given cmdargs. */

void usage(void)
{
    fprintf(stderr,
            "Usage: gosh [-biqV][-I<path>][-A<path>][-u<module>][-l<file>][-e<expr>][--] [file]\n"
            "options:\n"
            "  -V       Prints version and exits.\n"
            "  -b       Batch mode.  Doesn't print prompts.  Supersedes -i.\n"
            "  -i       Interactive mode.  Forces to print prompts.\n"
            "  -q       Doesn't read the default initialization file.\n"
            "  -I<path> Adds <path> to the head of the load path list.\n"
            "  -A<path> Adds <path> to the tail of the load path list.\n"
            "  -u<module> (use) load and import <module>\n"
            "  -l<file> Loads <file> before executing the script file or\n"
            "           entering repl.\n"
            "  -e<expr> Evaluate Scheme expression <expr> before executing\n"
            "           the script file or entering repl.\n"
            "  -E<expr> Similar to -e, but reads <expr> as if it is surrounded\n"
            "           by parenthesis.\n"
            "  -f<flag> Sets various flags\n"
            "      case-fold       uses case-insensitive reader (as in R5RS)\n"
            "      load-verbose    report while loading files\n"
            "      no-inline       don't inline primitive procedures\n"
            "      no-source-info  don't preserve source information for debug\n"
            "      test            test mode, to run gosh inside the build tree\n"
            );
    exit(1);
}

#ifdef GAUCHE_USE_PTHREADS
#define PTHREAD_OPT ",pthreads"
#else
#define PTHREAD_OPT ""
#endif

void version(void)
{
    printf("Gauche scheme interpreter, version %s [%s%s]\n",
           GAUCHE_VERSION, SCM_CHAR_ENCODING_NAME, PTHREAD_OPT);
    exit(0);
}

void further_options(const char *optarg)
{
    ScmVM *vm = Scm_VM();
    if (strcmp(optarg, "no-inline") == 0) {
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_NOINLINE);
    }
    else if (strcmp(optarg, "debug-compiler") == 0) {
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_SHOWRESULT);
    }
    else if (strcmp(optarg, "no-source-info") == 0) {
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_NOSOURCE);
    }
    else if (strcmp(optarg, "load-verbose") == 0) {
        SCM_VM_RUNTIME_FLAG_SET(vm, SCM_LOAD_VERBOSE);
    }
    else if (strcmp(optarg, "case-fold") == 0) {
        SCM_VM_RUNTIME_FLAG_SET(vm, SCM_CASE_FOLD);
    }
    else if (strcmp(optarg, "test") == 0) {
        test_mode = TRUE;
    }
    else {
        fprintf(stderr, "unknown -f option: %s\n", optarg);
        fprintf(stderr, "supported options are: -fcase-fold or -fload-verbose, -fno-inline, -fno-source-info, -ftest\n");
        exit(1);
    }
}

int parse_options(int argc, char *argv[])
{
    int c;
    while ((c = getopt(argc, argv, "+be:E:iql:u:Vf:I:A:-")) >= 0) {
        switch (c) {
        case 'b': batch_mode = TRUE; break;
        case 'i': interactive_mode = TRUE; break;
        case 'q': load_initfile = FALSE; break;
        case 'V': version(); break;
        case 'f': further_options(optarg); break;
        case 'u': /*FALLTHROUGH*/;
        case 'l': /*FALLTHROUGH*/;
        case 'I': /*FALLTHROUGH*/;
        case 'A': /*FALLTHROUGH*/;
        case 'e': /*FALLTHROUGH*/;
        case 'E': /*FALLTHROUGH*/;
            pre_cmds = Scm_Acons(SCM_MAKE_CHAR(c),
                                 SCM_MAKE_STR_COPYING(optarg), pre_cmds);
            break;
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
#ifdef SIGBUS
    sigdelset(&set, SIGBUS);
#endif /*SIGBUS*/
#if defined(GC_LINUX_THREADS)
    /* some signals are used in the system */
    sigdelset(&set, SIGPWR);  /* used in gc */
    sigdelset(&set, SIGXCPU); /* used in gc */
    sigdelset(&set, SIGUSR1); /* used in linux threads */
    sigdelset(&set, SIGUSR2); /* used in linux threads */
#endif /*SCM_LINUX_SIGNALS&&GAUCHE_USE_PTHREADS*/
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

    argind = parse_options(argc, argv);

    /* If -ftest option is given, and we seems to be in the source
       tree, adds ../src and ../lib to the library path _before_
       loading init file.  */
    if (test_mode) {
        if (access("../lib", R_OK) == 0) Scm_AddLoadPath("../lib", FALSE);
        if (access("../src", R_OK) == 0) Scm_AddLoadPath("../src", FALSE);
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

    /* process pre-commands */
    SCM_FOR_EACH(cp, Scm_Reverse(pre_cmds)) {
        ScmObj p = SCM_CAR(cp);
        ScmObj v = SCM_CDR(p);
        switch (SCM_CHAR_VALUE(SCM_CAR(p))) {
        case 'I':
            Scm_AddLoadPath(Scm_GetStringConst(SCM_STRING(v)), FALSE);
            break;
        case 'A':
            Scm_AddLoadPath(Scm_GetStringConst(SCM_STRING(v)), TRUE);
            break;
        case 'l':
            Scm_Load(Scm_GetStringConst(SCM_STRING(v)), TRUE);
            break;
        case 'u':
            Scm_Require(Scm_StringJoin(Scm_StringSplitByChar(SCM_STRING(v),
                                                             '.'),
                                       SCM_STRING(SCM_MAKE_STR("/")),
                                       SCM_STRING_JOIN_INFIX));
            Scm_ImportModules(SCM_CURRENT_MODULE(),
                              SCM_LIST1(Scm_Intern(SCM_STRING(v))));
            break;
        case 'e':
            Scm_Eval(Scm_ReadFromString(SCM_STRING(v)),
                     SCM_OBJ(Scm_UserModule()));
            break;
        case 'E':
            v = Scm_StringAppend(SCM_LIST3(SCM_MAKE_STR("("),
                                           v,
                                           SCM_MAKE_STR(")")));
            Scm_Eval(Scm_ReadFromString(SCM_STRING(v)),
                     SCM_OBJ(Scm_UserModule()));
            break;
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
           call it.  (SRFI-22) */
        mainproc = Scm_SymbolValue(Scm_UserModule(),
                                   SCM_SYMBOL(SCM_INTERN("main")));
        if (SCM_PROCEDUREP(mainproc)) {
            result = Scm_Apply(mainproc, SCM_LIST1(av));
            if (SCM_INTP(result)) Scm_Exit(SCM_INT_VALUE(result));
            else Scm_Exit(70);  /* EX_SOFTWARE, see SRFI-22. */
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
            Scm_Warn("couldn't load gauche.interactive\n");
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
