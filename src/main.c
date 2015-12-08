/*
 * main.c - interpreter main program
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "gauche.h"

#include <signal.h>
#include <ctype.h>
#include <fcntl.h>              /* for _O_BINMODE on windows. */

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

/* options */
int load_initfile = TRUE;       /* if false, not to load init files */
int batch_mode = FALSE;         /* force batch mode */
int interactive_mode = FALSE;   /* force interactive mode */
int test_mode = FALSE;          /* add . and ../lib implicitly  */
int profiling_mode = FALSE;     /* profile the script? */
int stats_mode = FALSE;         /* collect stats (EXPERIMENTAL) */

ScmObj pre_cmds = SCM_NIL;      /* assoc list of commands that needs to be
                                   processed before entering repl.
                                   Each car has either #\I, #\A, #\u, #\l
                                   or #\e, according to the given cmdargs. */

ScmObj main_module = SCM_FALSE; /* The name of the module where we
                                   look for 'main'.  If #f, 'user' module
                                   is used. */
ScmModule *default_toplevel_module = NULL;
                                /* The initial module for the script execution
                                   and interactive REPL.  If NULL, the user
                                   module is assumed.  In R7RS mode, it's
                                   'r7rs.user' module.
                                   Note that the .gaucherc script,
                                   if there's one, is always evaluated in 'user'
                                   module; see lib/gauche/interactive.scm */

void usage(void)
{
    fprintf(stderr,
            "Usage: gosh [-biqV][-I<path>][-A<path>][-u<module>][-m<module>][-l<file>][-L<file>][-e<expr>][-E<expr>][-p<type>][-F<feature>][-r<standard>][-f<flag>][--] [file]\n"
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
            "  -L<file> Like -l, but doesn't complain if <file> doesn't exist.\n"
            "  -e<expr> Evaluate Scheme expression <expr> before executing\n"
            "           the script file or entering repl.\n"
            "  -E<expr> Similar to -e, but reads <expr> as if it is surrounded\n"
            "           by parenthesis.\n"
            "  -m<module> When the script file is given, this option specifies the\n"
            "           name of the module in which the 'main' procedure is defined.\n"
            "           By default, the 'main' procedure in the user module is called\n"
            "           after loading the script (srfi-22).  This option allows to call\n"
            "           a main procedure in the different module.\n"
            "  -p<type> Turns on the profiler.  <Type> can be 'time' or 'load'.\n"
            "  -F<feature> Makes <feature> available in cond-expand forms\n"
            "  -r<standard>  Starts gosh with the default environment defined\n"
            "           in RnRS, where n is determined by <standard>.  The following\n"
            "           values are supported as <standard>.\n"
            "      7               R7RS (R7RS-small)\n"
            "  -f<flag> Sets various flags\n"
            "      case-fold       uses case-insensitive reader (as in R5RS)\n"
            "      load-verbose    report while loading files\n"
            "      include-verbose report while including files\n"
            "      warn-legacy-syntax\n"
            "                      print warning when legacy Gauche syntax is encountered\n"
            "      no-inline       don't inline procedures & constants (combined\n"
            "                      no-inline-globals, no-inline-locals, and\n"
            "                      no-inline-constants.)\n"
            "      no-inline-globals\n"
            "                      don't inline global procedures.\n"
            "      no-inline-locals\n"
            "                      don't inline local procedures.\n"
            "      no-inline-constants\n"
            "                      don't inline constants.\n"
            "      no-post-inline-pass\n"
            "                      don't run post-inline optimization pass.\n"
            "      no-source-info  don't preserve source information for debugging\n"
            "      test            test mode, to run gosh inside the build tree\n"
            );
    exit(1);
}

#ifdef GAUCHE_USE_PTHREADS
#define THREAD_OPT ",pthreads"
#elif  GAUCHE_USE_WTHREADS
#define THREAD_OPT ",wthreads"
#else
#define THREAD_OPT ""
#endif

void version(void)
{
    printf("Gauche scheme shell, version %s [%s%s], %s\n",
           GAUCHE_VERSION, SCM_CHAR_ENCODING_NAME, THREAD_OPT,
           Scm_HostArchitecture());
    exit(0);
}

void further_options(const char *optarg)
{
    ScmVM *vm = Scm_VM();
    if (strcmp(optarg, "no-inline-globals") == 0) {
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_NOINLINE_GLOBALS);
    }
    else if (strcmp(optarg, "no-inline-locals") == 0) {
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_NOINLINE_LOCALS);
    }
    else if (strcmp(optarg, "no-inline-constants") == 0) {
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_NOINLINE_CONSTS);
    }
    else if (strcmp(optarg, "no-inline") == 0) {
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_NOINLINE_GLOBALS);
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_NOINLINE_LOCALS);
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_NOINLINE_CONSTS);
    }
    else if (strcmp(optarg, "no-post-inline-pass") == 0) {
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_NO_POST_INLINE_OPT);
    }
    else if (strcmp(optarg, "no-lambda-lifting-pass") == 0) {
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_NO_LIFTING);
    }
    else if (strcmp(optarg, "no-source-info") == 0) {
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_NOSOURCE);
    }
    else if (strcmp(optarg, "load-verbose") == 0) {
        SCM_VM_RUNTIME_FLAG_SET(vm, SCM_LOAD_VERBOSE);
    }
    else if (strcmp(optarg, "include-verbose") == 0) {
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_INCLUDE_VERBOSE);
    }
    else if (strcmp(optarg, "case-fold") == 0) {
        SCM_VM_RUNTIME_FLAG_SET(vm, SCM_CASE_FOLD);
    }
    else if (strcmp(optarg, "warn-legacy-syntax") == 0) {
        Scm_SetReaderLexicalMode(SCM_INTERN("warn-legacy"));
    }
    else if (strcmp(optarg, "test") == 0) {
        test_mode = TRUE;
    }
    /* For development; not for public use */
    else if (strcmp(optarg, "collect-stats") == 0) {
        stats_mode = TRUE;
        SCM_VM_RUNTIME_FLAG_SET(vm, SCM_COLLECT_VM_STATS);
    }
    /* For development; not for public use */
    else if (strcmp(optarg, "no-combine-instructions") == 0) {
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_NOCOMBINE);
    }
    /* For development; not for public use */
    else if (strcmp(optarg, "debug-compiler") == 0) {
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_SHOWRESULT);
    }
    /* Experimental */
    else if (strcmp(optarg, "limit-module-mutation") == 0) {
        SCM_VM_RUNTIME_FLAG_SET(vm, SCM_LIMIT_MODULE_MUTATION);
    }
    /* Experimental */
    else if (strcmp(optarg, "c-expr") == 0) {
        SCM_VM_COMPILER_FLAG_SET(vm, SCM_COMPILE_ENABLE_CEXPR);
    }
    else {
        fprintf(stderr, "unknown -f option: %s\n", optarg);
        fprintf(stderr, "supported options are: -fcase-fold, -fload-verbose, -finclude-verbose, -fno-inline, -fno-inline-globals, -fno-inline-locals, -fno-inline-constants, -fno-source-info, -fno-post-inline-pass, -fno-lambda-lifting-pass, -fwarn-legacy-syntax, or -ftest\n");
        exit(1);
    }
}

void profiler_options(const char *optarg)
{
    ScmVM *vm = Scm_VM();

    if (strcmp(optarg, "time") == 0) {
        profiling_mode = TRUE;
    }
    else if (strcmp(optarg, "load") == 0) {
        SCM_VM_RUNTIME_FLAG_SET(vm, SCM_COLLECT_LOAD_STATS);
    }
    else {
        fprintf(stderr, "unknown -p option: %s\n", optarg);
        fprintf(stderr, "supported profiling options are: -ptime or -pload\n");
    }
}

void feature_options(const char *optarg)
{
    Scm_AddFeature(optarg, NULL);
}

int parse_options(int argc, char *argv[])
{
    int c;
    while ((c = getopt(argc, argv, "+be:E:ip:ql:L:m:u:Vr:F:f:I:A:-")) >= 0) {
        switch (c) {
        case 'b': batch_mode = TRUE; break;
        case 'i': interactive_mode = TRUE; break;
        case 'q': load_initfile = FALSE; break;
        case 'V': version(); break;
        case 'f': further_options(optarg); break;
        case 'p': profiler_options(optarg); break;
        case 'F': feature_options(optarg); break;
        case 'm':
            main_module = Scm_Intern(SCM_STRING(SCM_MAKE_STR_COPYING(optarg)));
            break;
        case 'r': /*FALLTHROUGH*/;
        case 'u': /*FALLTHROUGH*/;
        case 'l': /*FALLTHROUGH*/;
        case 'L': /*FALLTHROUGH*/;
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
#ifdef SIGKILL
    sigdelset(&set, SIGKILL);
#endif
#ifdef SIGCONT
    sigdelset(&set, SIGCONT);
#endif
#ifdef SIGSTOP
    sigdelset(&set, SIGSTOP);
#endif
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
#endif /*GC_LINUX_THREADS*/
#if defined(GC_FREEBSD_THREADS)
    sigdelset(&set, SIGUSR1); /* used by GC to stop the world */
    sigdelset(&set, SIGUSR2); /* used by GC to restart the world */
#endif /*GC_FREEBSD_THREADS*/
    Scm_SetMasterSigmask(&set);
}

/* Path setup.  When we're ran with -ftest flag, add paths to refer
   to the files in the source tree.  This is called after option processing
   and before loading init file. */
void test_paths_setup(void)
{
    /* The order of directories is important.  'lib' should
       be searched first (hence it should come latter), since some
       extension modules are built from the file in src then linked
       from lib, and we want to test the one in lib. */
    if (access("../src/gauche/config.h", R_OK) == 0
        && access("../libsrc/srfi-1.scm", R_OK) == 0
        && access("../lib/srfi-0.scm", R_OK) == 0) {
        Scm_AddLoadPath("../src", FALSE);
        Scm_AddLoadPath("../libsrc", FALSE);
        Scm_AddLoadPath("../lib", FALSE);
    } else if (access("../../src/gauche/config.h", R_OK) == 0
               && access("../../libsrc/srfi-1.scm", R_OK) == 0
               && access("../../lib/srfi-0.scm", R_OK) == 0) {
        Scm_AddLoadPath("../../src", FALSE);
        Scm_AddLoadPath("../../libsrc", FALSE);
        Scm_AddLoadPath("../../lib", FALSE);
    }
}

/* Cleanup */
void cleanup_main(void *data)
{
    ScmVM *vm = Scm_VM();

    if (profiling_mode) {
        Scm_ProfilerStop();
        Scm_EvalCString("(profiler-show)",
                        SCM_OBJ(Scm_GaucheModule()),
                        NULL); /* ignore errors */
    }

    /* EXPERIMENTAL */
    if (stats_mode) {
        fprintf(stderr, "\n;; Statistics (*: main thread only):\n");
        fprintf(stderr,
                ";;  GC: %zubytes heap, %zubytes allocated\n",
                GC_get_heap_size(), GC_get_total_bytes());
        fprintf(stderr,
                ";;  stack overflow*: %ldtimes, %.2fms total/%.2fms avg\n",
                vm->stat.sovCount,
                vm->stat.sovTime/1000.0,
                (vm->stat.sovCount > 0?
                 (double)(vm->stat.sovTime/vm->stat.sovCount)/1000.0 :
                 0.0));
    }

    /* EXPERIMENTAL */
    if (SCM_VM_RUNTIME_FLAG_IS_SET(vm, SCM_COLLECT_LOAD_STATS)) {
        Scm_Eval(SCM_LIST2(SCM_INTERN("profiler-show-load-stats"),
                           SCM_LIST2(SCM_INTERN("quote"),
                                     vm->stat.loadStat)),
                 SCM_OBJ(Scm_GaucheModule()),
                 NULL);    /* ignore errors */
    }
}

/* Error handling */
void error_exit(ScmObj c)
{
    ScmObj m = Scm_ConditionMessage(c);
    if (SCM_FALSEP(m)) {
        Scm_Printf(SCM_CURERR, "gosh: Thrown unknown condition: %S\n", c);
    } else {
        Scm_Printf(SCM_CURERR, "gosh: %S: %A\n", Scm_ConditionTypeName(c), m);
    }
    Scm_Exit(1);
}

/* Returns FALSE if the process doesn't have a console. */
#if defined(GAUCHE_WINDOWS)
static int init_console(void)
{
#  if defined(GAUCHE_WINDOWS_NOCONSOLE)
    char buf[100];
    int in_fd, out_fd;
#define ERR(msg) do {sprintf(buf, msg, strerror(errno));goto fail;} while(0)
    /* If we don't have console, we'll start off with stdio to be redirected
       to NUL; but whenever Scheme program tries to do I/O from/to
       standard Scheme ports, we open the console and reconnect the
       ports. */
    if ((in_fd  = open("NUL", O_RDONLY)) < 0) ERR("couldn't open NUL: %s");
    if ((out_fd = open("NUL", O_WRONLY)) < 0) ERR("couldn't open NUL: %s");
    if (_dup2(in_fd,  0) < 0) ERR("dup2(0) failed (%s)");
    if (_dup2(out_fd, 1) < 0) ERR("dup2(1) failed (%s)");
    if (_dup2(out_fd, 2) < 0) ERR("dup2(2) failed (%s)");
    close(in_fd);
    close(out_fd);
    return FALSE;
#undef ERR
 fail:
    MessageBoxA(NULL, buf, "gosh-noconsole", MB_OK|MB_ICONERROR);
    Scm_Exit(1);
#  else /*!defined(GAUCHE_WINDOWS_NOCONSOLE)*/
    /* This saves so much trouble */
    _setmode(_fileno(stdin),  _O_BINARY);
    _setmode(_fileno(stdout), _O_BINARY);
    _setmode(_fileno(stderr), _O_BINARY);
    return TRUE;
#  endif /*!defined(GAUCHE_WINDOWS_NOCONSOLE)*/
}

/* called in main to set up trapper ports; defined in port.c */
extern void Scm__SetupPortsForWindows(int);
#endif /*defined(GAUCHE_WINDOWS)*/

/* Process command-line options that needs to run after Scheme runtime
   is initialized.  CMD_ARGS is an list of (OPTION-CHAR . OPTION-ARG) */
static void process_command_args(ScmObj cmd_args)
{
    ScmEvalPacket epak;
    ScmLoadPacket lpak;
    int standard_given = FALSE;
    ScmObj cp;

    SCM_FOR_EACH(cp, cmd_args) {
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
            if (Scm_Load(Scm_GetStringConst(SCM_STRING(v)), 0, &lpak) < 0)
                error_exit(lpak.exception);
            break;
        case 'L':
            if (Scm_Load(Scm_GetStringConst(SCM_STRING(v)), SCM_LOAD_QUIET_NOFILE, &lpak) < 0)
                error_exit(lpak.exception);
            break;
        case 'u':
            if (Scm_Require(Scm_StringJoin(Scm_StringSplitByChar(SCM_STRING(v),
                                                                 '.'),
                                           SCM_STRING(SCM_MAKE_STR("/")),
                                           SCM_STRING_JOIN_INFIX),
                            0, &lpak) < 0) {
                error_exit(lpak.exception);
            }
            Scm_ImportModule(SCM_CURRENT_MODULE(), Scm_Intern(SCM_STRING(v)),
                             SCM_FALSE, 0);
            break;
        case 'e':
            if (Scm_EvalCString(Scm_GetStringConst(SCM_STRING(v)),
                                SCM_OBJ(Scm_UserModule()),
                                &epak) < 0) {
                error_exit(epak.exception);
            }
            break;
        case 'E':
            v = Scm_StringAppend(SCM_LIST3(SCM_MAKE_STR("("),
                                           v,
                                           SCM_MAKE_STR(")")));

            if (Scm_EvalCString(Scm_GetStringConst(SCM_STRING(v)),
                                SCM_OBJ(Scm_UserModule()),
                                &epak) < 0) {
                error_exit(epak.exception);
            }
            break;
        case 'r':
            if (standard_given) {
                Scm_Error("Multiple -r option is specified.");
            } else {
                /* R7RS mode.  Preload r7rs module, set the default toplevel
                   to r7rs.user, and define *r7rs-mode* in user module
                   so that gauche.interactive can do proper setup. */
                const char *std = Scm_GetStringConst(SCM_STRING(v));
                if (strcmp(std, "7") == 0) {
                    if (Scm_Require(SCM_MAKE_STR("r7rs"), 0, &lpak) < 0) {
                        error_exit(lpak.exception);
                    }
                    SCM_DEFINE(Scm_UserModule(), "*r7rs-mode*", SCM_TRUE);
                    default_toplevel_module = SCM_FIND_MODULE("r7rs.user", 0);
                    standard_given = TRUE;
                } else {
                    Scm_Error("Unsupported standard for -r option: %s", std);
                }
            }
        }
    }
}

/* When scriptfile is provided, execute it.  Returns exit code. */
int execute_script(const char *scriptfile, ScmObj args)
{
    /* If script file is specified, load it. */
    ScmObj mainproc = SCM_FALSE;
    ScmLoadPacket lpak;

    Scm_Load(scriptfile, SCM_LOAD_PROPAGATE_ERROR, &lpak);
    if (!lpak.loaded) return 1;

    /* If symbol 'main is bound, call it (SRFI-22).   */
    ScmModule *mainmod = (SCM_SYMBOLP(main_module)
                          ? Scm_FindModule(SCM_SYMBOL(main_module), 0)
                          : Scm_UserModule());
    if (mainmod) {
        mainproc = Scm_GlobalVariableRef(mainmod,
                                         SCM_SYMBOL(SCM_INTERN("main")),
                                         SCM_BINDING_STAY_IN_MODULE);
    }
    if (SCM_PROCEDUREP(mainproc)) {
#if 0 /* Temporarily turned off due to the bug that loses stack traces. */
        ScmEvalPacket epak;
        int r = Scm_Apply(mainproc, SCM_LIST1(args), &epak);
        if (r > 0) {
            ScmObj res = epak.results[0];
            if (SCM_INTP(res)) return SCM_INT_VALUE(res);
            else return 70;  /* EX_SOFTWARE, see SRFI-22. */
        } else {
            Scm_ReportError(epak.exception);
            return 70;  /* EX_SOFTWARE, see SRFI-22. */
        }
#else
        ScmObj r = Scm_ApplyRec1(mainproc, args);
        if (SCM_INTP(r)) return SCM_INT_VALUE(r);
        else             return 70;
#endif
    }
    return 0;
}

/* When no script is given, enter REPL. */
void enter_repl()
{
    /* We're in interactive mode. (use gauche.interactive) */
    if (load_initfile) {
        ScmLoadPacket lpak;
        if (Scm_Require(SCM_MAKE_STR("gauche/interactive"), 0, &lpak) < 0) {
            Scm_Warn("couldn't load gauche.interactive");
        } else {
            Scm_ImportModule(SCM_CURRENT_MODULE(),
                             SCM_INTERN("gauche.interactive"), SCM_FALSE, 0);
        }
    }

    /* If -fcase-fold flag is given, switch the stdin port to the
       case folding mode. */
    if (SCM_VM_RUNTIME_FLAG_IS_SET(Scm_VM(), SCM_CASE_FOLD)) {
        Scm_SetPortCaseFolding(SCM_PORT(Scm_Stdin()), TRUE);
    }
    
    if (batch_mode || (!isatty(0) && !interactive_mode)) {
        Scm_LoadFromPort(SCM_PORT(Scm_Stdin()), SCM_LOAD_PROPAGATE_ERROR, NULL);
    } else {
        /* Call read-eval-print-loop.  If gauche.interactive is loaded,
           this will invoke 'user-friendly' version of repl; otherwise,
           this calls the 'bare' version in libeval.scm. */
        Scm_EvalCString("(read-eval-print-loop)",
                        SCM_OBJ(Scm_CurrentModule()), NULL);
    }
}

/*-----------------------------------------------------------------
 * MAIN
 */
int main(int ac, char **av)
{
    const char *scriptfile = NULL;
    ScmObj args = SCM_NIL;
    int exit_code = 0;
    /* NB: For Windows, we can't use passed argv array if the command-line
       argument contains multibyte characters.  We'll overwrite those later. */
    int argc = ac;
    char **argv = av;

#if defined(GAUCHE_WINDOWS)
    /* Need this before core initialization */
    int has_console = init_console();
#endif /*defined(GAUCHE_WINDOWS)*/

    GC_INIT();
    Scm_Init(GAUCHE_SIGNATURE);
    sig_setup();

#if defined(GAUCHE_WINDOWS)
    Scm__SetupPortsForWindows(has_console);
#  if defined(UNICODE)
    /* Set up argument array correctly */
    LPWSTR *argvW = CommandLineToArgvW(GetCommandLineW(), &argc);
    argv = SCM_NEW_ATOMIC_ARRAY(char*, argc);
    /* Kludge! Need to discard 'const' qualifier, for getopt() expects
       char * const*, not const char**.  It's safe since Scm_WCS2MBS
       always returns freshly allocated strings and we won't share them. */
    for (int i=0; i<argc; i++) argv[i] = (char*)Scm_WCS2MBS(argvW[i]);
    LocalFree(argvW);
#  endif  /* UNICODE */
#endif /*defined(GAUCHE_WINDOWS)*/

    /* Check command-line options */
    int argind = parse_options(argc, argv);

    /* If -ftest option is given and we seem to be in the source
       tree, adds build directories to the library path _before_
       loading init file.   This is to help development of Gauche
       itself; normal user should never need this. */
    if (test_mode) test_paths_setup();

    /* prepare *program-name* and *argv* */
    if (argind < argc) {
        /* We have a script file specified. */
        struct stat statbuf;

        /* if the script name is given in relative pathname, see if
           it exists from the current directory.  if not, leave it
           to load() to search in the load paths */
        if (argv[argind][0] == '\0') Scm_Error("bad script name");
        if (argv[argind][0] == '/') {
            scriptfile = argv[argind];
#if defined(__CYGWIN__) || defined(GAUCHE_WINDOWS)
        } else if (isalpha(argv[argind][0]) && argv[argind][1] == ':') {
            /* support of wicked legacy DOS drive letter */
            scriptfile = argv[argind];
#endif /* __CYGWIN__ || GAUCHE_WINDOWS */
        } else {
            if (stat(argv[argind], &statbuf) == 0) {
                ScmDString ds;
                Scm_DStringInit(&ds);
                Scm_DStringPutz(&ds, "./", -1);
                Scm_DStringPutz(&ds, argv[argind], -1);
                scriptfile = Scm_DStringGetz(&ds);
            } else {
                scriptfile = argv[argind];
            }
        }

        /* sets up arguments. */
        args = Scm_InitCommandLine(argc - argind, (const char**)argv + argind);
    } else {
        args = Scm_InitCommandLine(1, (const char**)argv);
    }

    process_command_args(Scm_Reverse(pre_cmds));

    /* Set up instruments. */
    ScmLoadPacket lpak;
    if (profiling_mode) {
        if (Scm_Require(SCM_MAKE_STR("gauche/vm/profiler"), 0, &lpak) < 0) {
            error_exit(lpak.exception);
        }
        Scm_ProfilerStart();
    }
    Scm_AddCleanupHandler(cleanup_main, NULL);

    if (default_toplevel_module != NULL) {
        Scm_SelectModule(default_toplevel_module);
    }

    /* Following is the main dish. */
    if (scriptfile != NULL) exit_code = execute_script(scriptfile, args);
#if !defined(GAUCHE_WINDOWS_NOCONSOLE)
    else                    enter_repl();
#endif /*!defined(GAUCHE_WINDOWS_NOCONSOLE)*/

    /* All is done.  */
    Scm_Exit(exit_code);
    return 0;                   /*NOTREACHED*/
}
