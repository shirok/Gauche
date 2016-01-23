/*
 * signal.c - signal handling
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

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/vm.h"
#include "gauche/class.h"

/* Signals
 *
 *  C-application that embeds Gauche can specify a set of signals
 *  that Gauche can handle.
 *
 *  The Scheme program can specify which signal it wants to handle
 *  by setting a Scheme signal handler.  Gauche registers the internal
 *  signal handler for the specified signal.  What the internal signal
 *  handler does is just queue the signal in the VM's signal queue.
 *  VM calls Scm_SigCheck() at the "safe" point, which flushes
 *  the signal queue and make a list of handlers to be called.
 *
 *  Scheme signal handler vector is shared by all threads.  Each
 *  thread can set a signal mask.  By default, only the primordial
 *  thread handles signals.
 *
 *  For most signals, Gauche installs the default signal handler that
 *  raises 'unhandled signal exception'.   For other signals, Gauche lets
 *  the system to handle the signal unless the Scheme program installs
 *  the handler.   Such signals are the ones that can't be caught, or
 *  are ignored by default.  SIGPWR and SIGXCPU are also left to the system
 *  since GC uses it in the Linux/pthread environment.
 *
 *  About the signal behavior on windows, see "Note on windows port" below.
 */
#if !defined(GAUCHE_WINDOWS)
# ifdef GAUCHE_USE_PTHREADS
#  define SIGPROCMASK pthread_sigmask
# else
#  define SIGPROCMASK sigprocmask
# endif
#else  /* GAUCHE_WINDOWS */
/* emulation routine is defined below */
# define SIGPROCMASK sigprocmask_win
static int sigprocmask_win(int how, const sigset_t *set, sigset_t *oldset);
#endif /* GAUCHE_WINDOWS */

/* Master signal handler vector. */
static struct sigHandlersRec {
    ScmObj handlers[SCM_NSIG];  /* Scheme signal handlers.  This is #<undef> on
                                   signals to which Gauche does not install
                                   C-level signal handler (sig_handle). */
    ScmSysSigset *masks[SCM_NSIG];/* Signal masks during executing Scheme
                                   handlers.  Can be NULL, which means
                                   the handling signal(s) are blocked. */
    sigset_t masterSigset;      /* The signals Gauche is _allowed_ to handle.
                                   set by Scm_SetMasterSigmask.
                                   For some signals in this set Gauche sets
                                   the default signal handlers; for other
                                   signals in this set Gauche leaves them
                                   for the system to handle.  These can be
                                   overridden by Scm_SetSignalHandler. */
    ScmInternalMutex mutex;
} sigHandlers = {{NULL}};

/* Maximum # of the same signals before it is processed by the VM loop.
   If any one of signals exceeds this count, Gauche exits with Scm_Abort.
   It is useful to terminate unresponsive program that are executing
   long-running C-routine and do not returns to VM.
   The actual limit can be changed at runtime by Scm_SetSignalPendingLimit().
   If signalPendingLimit is 0, the number of pending signals is unlimited. */
#define SIGNAL_PENDING_LIMIT_DEFAULT 3
#define SIGNAL_PENDING_LIMIT_MAX 255

static unsigned int signalPendingLimit = SIGNAL_PENDING_LIMIT_DEFAULT;


/* Table of signals and its initial behavior.   If Application asks
   Gauche to handle a specific signal (via Scm_SetMasterSigmask),
   Gauche installs a signal handler specified in this table.

   Note on SIGPIPE behavior: The Unix convention is that the default
   behavior or writing to closed pipe terminates the process.  It is
   suitable for small commands designed to be piped together, for
   a failure of any one of the commands in the chunk causes
   entire chain to be finished promptly.  However it is rather an
   annoyance if we use an fd for transient communication (e.g.
   sockets, most notably).  Since signal handlers are process-global,
   it is quite tricky for general libraries/middlewares to control
   SIGPIPE behavior transparently.

   As a library, Gauche employs simpler model: Application will specify
   which signals they want handle and which signals they want Gauche
   to handle.  Once app delegates handling of a specific signal,
   Gauche has a right to install its own handler.

   So, here's the deal: If the app wants to handle SIGPIPE by itself,
   it's fine.  By default SIGPIPE terminates the process.  If the app
   installs its own SIGPIPE handler that returns, then Gauche sees
   EPIPE and raises <system-error>.

   If the app let Gauche to handle SIGPIPE, we install our own handler,
   which does nothing.  Scheme code can handle the situation via EPIPE
   <system-error>.  To emulate Unix default behavior, however, EPIPE
   caused by standard output and standard error will terminate the
   process by default.  It can be configured per-port basis.
*/

#define SIGDEF_NOHANDLE 0       /* Gauche doesn't install a signal handler,
                                   leaving it to the application. */
#define SIGDEF_DFL      1       /* Gauche resets the singal handler to
                                   SIG_DFL. */
#define SIGDEF_ERROR    2       /* Gauche installs a default signal handler
                                   that raises an error. */
#define SIGDEF_EXIT     3       /* Gauche installs a handler that calls
                                   Scm_Exit(). */
#define SIGDEF_INDIFFERENT 4    /* Gauche installs a handler that does
                                   nothing. */

#define SIGDEF(x, flag)  { #x, x, flag }

static struct sigdesc {
    const char *name;
    int num;
    int defaultHandle;
} sigDesc[] = {
#ifdef SIGHUP
    SIGDEF(SIGHUP,  SIGDEF_EXIT),     /* Hangup (POSIX) */
#endif
    SIGDEF(SIGINT,  SIGDEF_ERROR),    /* Interrupt (ANSI) */
#ifdef SIGQUIT
    SIGDEF(SIGQUIT, SIGDEF_EXIT),     /* Quit (POSIX) */
#endif
    SIGDEF(SIGILL,  SIGDEF_NOHANDLE), /* Illegal instruction (ANSI) */
#ifdef SIGTRAP
    SIGDEF(SIGTRAP, SIGDEF_ERROR),    /* Trace trap */
#endif
    SIGDEF(SIGABRT, SIGDEF_NOHANDLE), /* Abort (ANSI) */
#ifdef SIGIOT
    SIGDEF(SIGIOT,  SIGDEF_ERROR),    /* IOT trap (4.2 BSD) */
#endif
#ifdef SIGBUS
    SIGDEF(SIGBUS,  SIGDEF_NOHANDLE), /* BUS error (4.2 BSD) */
#endif
    SIGDEF(SIGFPE,  SIGDEF_ERROR),    /* Floating-point exception (ANSI) */
#ifdef SIGKILL
    SIGDEF(SIGKILL, SIGDEF_NOHANDLE), /* Kill, unblockable (POSIX) */
#endif
#ifdef SIGUSR1
    SIGDEF(SIGUSR1, SIGDEF_ERROR),    /* User-defined signal 1 (POSIX) */
#endif
    SIGDEF(SIGSEGV, SIGDEF_NOHANDLE), /* Segmentation violation (ANSI) */
#ifdef SIGUSR2
    SIGDEF(SIGUSR2, SIGDEF_ERROR),    /* User-defined signal 2 (POSIX) */
#endif
#ifdef SIGPIPE
    SIGDEF(SIGPIPE, SIGDEF_INDIFFERENT), /* Broken pipe (POSIX).
                                            See above note on SIGPIPE. */
#endif
#ifdef SIGALRM
    SIGDEF(SIGALRM, SIGDEF_ERROR),    /* Alarm clock (POSIX) */
#endif
    SIGDEF(SIGTERM, SIGDEF_EXIT),     /* Termination (ANSI) */
#ifdef SIGSTKFLT
    SIGDEF(SIGSTKFLT, SIGDEF_ERROR),  /* Stack fault */
#endif
#ifdef SIGCHLD
    SIGDEF(SIGCHLD, SIGDEF_DFL),      /* Child status has changed (POSIX) */
#endif
#ifdef SIGCONT
    SIGDEF(SIGCONT, SIGDEF_NOHANDLE), /* Continue (POSIX) */
#endif
#ifdef SIGSTOP
    SIGDEF(SIGSTOP, SIGDEF_NOHANDLE), /* Stop, unblockable (POSIX) */
#endif
#ifdef SIGTSTP
    SIGDEF(SIGTSTP, SIGDEF_NOHANDLE), /* Keyboard stop (POSIX) */
#endif
#ifdef SIGTTIN
    SIGDEF(SIGTTIN, SIGDEF_NOHANDLE), /* Background read from tty (POSIX) */
#endif
#ifdef SIGTTOU
    SIGDEF(SIGTTOU, SIGDEF_NOHANDLE), /* Background write to tty (POSIX) */
#endif
#ifdef SIGURG
    SIGDEF(SIGURG,  SIGDEF_NOHANDLE), /* Urgent condition on socket (4.2 BSD) */
#endif
#ifdef SIGXCPU
    SIGDEF(SIGXCPU, SIGDEF_NOHANDLE), /* CPU limit exceeded (4.2 BSD) */
#endif
#ifdef SIGXFSZ
    SIGDEF(SIGXFSZ, SIGDEF_ERROR),    /* File size limit exceeded (4.2 BSD) */
#endif
#ifdef SIGVTALRM
    SIGDEF(SIGVTALRM, SIGDEF_ERROR),  /* Virtual alarm clock (4.2 BSD) */
#endif
#ifdef SIGPROF
    SIGDEF(SIGPROF, SIGDEF_ERROR),    /* Profiling alarm clock (4.2 BSD) */
#endif
#ifdef SIGWINCH
    SIGDEF(SIGWINCH, SIGDEF_NOHANDLE),/* Window size change (4.3 BSD, Sun) */
#endif
#ifdef SIGPOLL
    SIGDEF(SIGPOLL, SIGDEF_ERROR),    /* Pollable event occurred (System V) */
#endif
#ifdef SIGIO
    SIGDEF(SIGIO,   SIGDEF_ERROR),    /* I/O now possible (4.2 BSD) */
#endif
#ifdef SIGPWR
    SIGDEF(SIGPWR,  SIGDEF_NOHANDLE), /* Power failure restart (System V) */
#endif
    { NULL, -1 }
};

/*===============================================================
 * Signal set operations
 */

/*
 * utilities for sigset
 */
static void display_sigset(sigset_t *set, ScmPort *port)
{
    struct sigdesc *desc = sigDesc;
    int cnt = 0;
    for (; desc->name; desc++) {
        if (sigismember(set, desc->num)) {
            if (cnt++) Scm_Putc('|', port);
            Scm_Putz(desc->name+3, -1, port);
        }
    }
}

static int validsigp(int signum)
{
    if (signum > 0) {
        struct sigdesc *desc = sigDesc;
        for (; desc->name; desc++) {
            if (desc->num == signum) return TRUE;
        }
    }
    return FALSE;
}

static void sigset_op(sigset_t *s1, sigset_t *s2, int delp)
{
    struct sigdesc *desc = sigDesc;
    for (; desc->name; desc++) {
        if (sigismember(s2, desc->num)) {
            if (!delp) sigaddset(s1, desc->num);
            else       sigdelset(s1, desc->num);
        }
    }
}

ScmObj Scm_SignalName(int signum)
{
    struct sigdesc *desc = sigDesc;
    for (; desc->name; desc++) {
        if (desc->num == signum) {
            return SCM_MAKE_STR_IMMUTABLE(desc->name);
        }
    }
    return SCM_FALSE;
}

/*
 * sigset class
 */

static void sigset_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx);
static ScmObj sigset_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_SysSigsetClass, sigset_print,
                         NULL, NULL, sigset_allocate, SCM_CLASS_DEFAULT_CPL);

void sigset_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    Scm_Printf(out, "#<sys-sigset [");
    display_sigset(&SCM_SYS_SIGSET(obj)->set, out);
    Scm_Printf(out, "]>");
}

ScmObj sigset_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmSysSigset *s = SCM_NEW_INSTANCE(ScmSysSigset, klass);
    sigemptyset(&s->set);
    return SCM_OBJ(s);
}

ScmSysSigset *make_sigset(void)
{
    return SCM_SYS_SIGSET(sigset_allocate(SCM_CLASS_SYS_SIGSET, SCM_NIL));
}

/* multifunction on sigset
    if delp == FALSE, signals are added to set.
    else, signals are removed from set.
    signals is a list of either integer or #t (all signals), or other sigset.
*/
ScmObj Scm_SysSigsetOp(ScmSysSigset *set, ScmObj signals, int delp)
{
    if (!SCM_PAIRP(signals)) {
        Scm_Error("list of signals required, but got %S", signals);
    }

    ScmObj cp;
    SCM_FOR_EACH(cp, signals) {
        ScmObj s = SCM_CAR(cp);
        if (SCM_TRUEP(s)) {
            if (!delp) sigfillset(&set->set);
            else       sigemptyset(&set->set);
            break;
        }
        if (SCM_SYS_SIGSET_P(s)) {
            sigset_op(&set->set, &SCM_SYS_SIGSET(s)->set, delp);
            continue;
        }
        if (!SCM_INTP(s) || !validsigp(SCM_INT_VALUE(s))) {
            Scm_Error("bad signal number %S", s);
        }
        if (!delp) sigaddset(&set->set, SCM_INT_VALUE(s));
        else       sigdelset(&set->set, SCM_INT_VALUE(s));
    }
    return SCM_OBJ(set);
}

/* fill or empty sigset. */
ScmObj Scm_SysSigsetFill(ScmSysSigset *set, int emptyp)
{
    if (emptyp) sigemptyset(&(set->set));
    else        sigfillset(&(set->set));
    return SCM_OBJ(set);
}

/*=============================================================
 * C-level signal handling
 */


/*-------------------------------------------------------------------
 * C-level signal handler - just records the signal delivery.
 */

static void sig_handle(int signum)
{
    ScmVM *vm = Scm_VM();
    /* It is possible that vm == NULL at this point, if the thread is
       terminating and in the cleanup phase. */
    if (vm == NULL) return;

    if (signalPendingLimit == 0) {
        vm->sigq.sigcounts[signum] = 1;
    } else if (++vm->sigq.sigcounts[signum] >= signalPendingLimit) {
        Scm_Abort("Received too many signals before processing them.  Exitting for the emergency...\n");
    }
    vm->signalPending = TRUE;
    vm->attentionRequest = TRUE;
}

/*-------------------------------------------------------------------
 * Signal queue operations
 */

/*
 * Clear the signal queue
 */
void Scm_SignalQueueClear(ScmSignalQueue* q)
{
    for (int i=0; i<SCM_NSIG; i++) q->sigcounts[i] = 0;
}

/*
 * Initializes signal queue
 */
void Scm_SignalQueueInit(ScmSignalQueue* q)
{
#if defined(GAUCHE_API_0_9)
    q->sigcounts = SCM_NEW_ARRAY(unsigned char, SCM_NSIG);
#endif /* GAUCHE_API_0_9 */
    Scm_SignalQueueClear(q);
    q->pending = SCM_NIL;
}

/*
 * Get/Set signal pending limit
 */
int Scm_GetSignalPendingLimit(void)
{
    return signalPendingLimit;
}

void Scm_SetSignalPendingLimit(int num)
{
    if (num < 0 || num >= SIGNAL_PENDING_LIMIT_MAX) {
        Scm_Error("signal-pending-limit argument out of range: %d", num);
    }
    signalPendingLimit = num;
}

/*
 * Called from VM's safe point to flush the queued signals.
 * VM already checks there's a pending signal in the queue.
 */
void Scm_SigCheck(ScmVM *vm)
{
    ScmSignalQueue *q = &vm->sigq;
    unsigned char sigcounts[SCM_NSIG]; /* copy of signal counter */

    /* Copy VM's signal counter to local storage, for we can't call
       storage allocation during blocking signals. */
    sigset_t omask;
    SIGPROCMASK(SIG_BLOCK, &sigHandlers.masterSigset, &omask);
    memcpy(sigcounts, vm->sigq.sigcounts, SCM_NSIG * sizeof(unsigned char));
    Scm_SignalQueueClear(&vm->sigq);
    vm->signalPending = FALSE;
    SIGPROCMASK(SIG_SETMASK, &omask, NULL);

#if defined(GAUCHE_USE_PTHREADS) && defined(GAUCHE_PTHREAD_SIGNAL)
    /* We may use GAUCHE_PTHREAD_SIGNAL signal to terminate a thread
       gracefully.  See Scm_ThreadTerminate in ext/threads/threads.c */
    if (sigcounts[GAUCHE_PTHREAD_SIGNAL] > 0) {
        /* The thread state will be set to TERMINATED by the cleanup
           handler so we don't need to change it. */
        SCM_INTERNAL_THREAD_EXIT();
        /* NOTREACHED */
    }
#endif  /* defined(GAUCHE_USE_PTHREADS) && defined(GAUCHE_PTHREAD_SIGNAL) */

    /* Now, prepare queued signal handlers
       If an error is thrown in this loop, the queued signals will be
       lost---it doesn't look like so, but I may overlook something. */
    ScmObj tail = q->pending;
    if (!SCM_NULLP(tail)) tail = Scm_LastPair(tail);
    for (int i=0; i<SCM_NSIG; i++) {
        if (sigcounts[i] == 0) continue;
        if (SCM_PROCEDUREP(sigHandlers.handlers[i])) {
            ScmObj cell = Scm_Cons(SCM_LIST3(sigHandlers.handlers[i],
                                             SCM_MAKE_INT(i),
                                             SCM_OBJ_SAFE(sigHandlers.masks[i])),
                            SCM_NIL);
            if (SCM_NULLP(tail)) {
                q->pending = tail = cell;
            } else {
                SCM_SET_CDR(tail, cell);
                tail = SCM_CDR(tail);
            }
        }
    }

    /* Call the queued signal handlers.  If an error is thrown in one
       of those handlers, the rest of handlers remain in the queue. */
    /* TODO: if VM is active, it'd be better to make the active VM to handle
       those handler procs, instead of calling Scm_Eval. */
    ScmObj sp;
    SCM_FOR_EACH(sp, q->pending) {
        ScmObj e = SCM_CAR(sp);
        q->pending = SCM_CDR(sp);
        ScmObj handler = SCM_CAR(e);
        ScmObj num = SCM_CADR(e);
        ScmObj mask = SCM_CAR(SCM_CDDR(e));
        if (SCM_SYS_SIGSET_P(mask)) {
            sigset_t omask;
            SCM_UNWIND_PROTECT {
                SIGPROCMASK(SIG_BLOCK, &SCM_SYS_SIGSET(mask)->set, &omask);
                Scm_ApplyRec(handler, SCM_LIST1(num));
            }
            SCM_WHEN_ERROR {
                SIGPROCMASK(SIG_SETMASK, &omask, NULL);
                SCM_NEXT_HANDLER;
            }
            SCM_END_PROTECT;
            SIGPROCMASK(SIG_SETMASK, &omask, NULL);
        } else {
            Scm_ApplyRec(handler, SCM_LIST1(num));
        }
    }
}

/*=============================================================
 * Scheme-level signal handling
 */

/*-------------------------------------------------------------
 * Default Scheme-level handlers
 */
/* For most signals, default handler raises an error. */
static ScmObj default_sighandler(ScmObj *args, int nargs, void *data)
{
    SCM_ASSERT(nargs == 1 && SCM_INTP(args[0]));
    int signum = SCM_INT_VALUE(args[0]);

    struct sigdesc *desc;
    const char *name = NULL;
    for (desc = sigDesc; desc->name; desc++) {
        if (desc->num == signum) {
            name = desc->name;
            break;
        }
    }
    if (name) {
        Scm_RaiseCondition(SCM_OBJ(SCM_CLASS_UNHANDLED_SIGNAL_ERROR),
                           "signal", SCM_MAKE_INT(signum),
                           SCM_RAISE_CONDITION_MESSAGE,
                           "unhandled signal %d (%s)", signum, name);
    } else {
        Scm_RaiseCondition(SCM_OBJ(SCM_CLASS_UNHANDLED_SIGNAL_ERROR),
                           "signal", SCM_MAKE_INT(signum),
                           SCM_RAISE_CONDITION_MESSAGE,
                           "unhandled signal %d (unknown signal)", signum);
    }
    return SCM_UNDEFINED;       /* dummy */
}

static SCM_DEFINE_STRING_CONST(default_sighandler_name,
                               "%default-signal-handler", 23, 23);
static SCM_DEFINE_SUBR(default_sighandler_stub, 1, 0,
                       SCM_OBJ(&default_sighandler_name),
                       default_sighandler,
                       NULL, NULL);

#define DEFAULT_SIGHANDLER    SCM_OBJ(&default_sighandler_stub)

/* For some signals, exits. */
static ScmObj exit_sighandler(ScmObj *args, int nargs, void *data)
{
    Scm_Exit(0);
    return SCM_UNDEFINED;       /* dummy */
}

static SCM_DEFINE_STRING_CONST(exit_sighandler_name,
                               "%exit-signal-handler", 20, 20);
static SCM_DEFINE_SUBR(exit_sighandler_stub, 1, 0,
                       SCM_OBJ(&exit_sighandler_name),
                       exit_sighandler,
                       NULL, NULL);

#define EXIT_SIGHANDLER    SCM_OBJ(&exit_sighandler_stub)

/* For some signals, gauche does nothing */
static ScmObj indifferent_sighandler(ScmObj *args, int nargs, void *data)
{
    return SCM_UNDEFINED;
}

static SCM_DEFINE_STRING_CONST(indifferent_sighandler_name,
                               "%indifferent-signal-handler", 27, 27);
static SCM_DEFINE_SUBR(indifferent_sighandler_stub, 1, 0,
                       SCM_OBJ(&indifferent_sighandler_name),
                       indifferent_sighandler,
                       NULL, NULL);

#define INDIFFERENT_SIGHANDLER    SCM_OBJ(&indifferent_sighandler_stub)

/*
 * An emulation stub for Windows
 *
 * Note on windows port:
 *  Windows does provide signal() function to conform C standard, but
 *  its use is so limited that it's effectively useless.  You can only
 *  trap SIGABRT, SIGFPE, SIGILL, SIGSEGV and SIGTERM, and you cannot
 *  send signal to other processes.  Emulating POSIX signal behavior is
 *  not an easy task (see http://cygwin.com/cgi-bin/cvsweb.cgi/src/winsup/cygwin/how-signals-work.txt?cvsroot=src ).
 *  So, although we provide a signal interface, DO NOT USE IT.  It's better
 *  to expose Windows native IPC and build an abstraction on top if it.
 */
#if defined(GAUCHE_WINDOWS)
int sigaction(int signum, const struct sigaction *act,
              struct sigaction *oact)
{
    if (oact != NULL) {
        Scm_Panic("sigaction() with oldact != NULL isn't supported on MinGW port");
    }
    if (signal(signum, act->sa_handler) == SIG_ERR) {
        return -1;
    } else {
        return 0;
    }
}

int sigprocmask_win(int how, const sigset_t *set, sigset_t *oldset)
{
    return 0;
}
#endif /* GAUCHE_WINDOWS */

/*
 * set-signal-handler!
 */
ScmObj Scm_SetSignalHandler(ScmObj sigs, ScmObj handler, ScmSysSigset *mask)
{
    sigset_t sigset;
    int badproc = FALSE, sigactionfailed = FALSE;

    if (SCM_INTP(sigs)) {
        int signum = SCM_INT_VALUE(sigs);
        if (signum < 0 || signum >= SCM_NSIG) {
            Scm_Error("bad signal number: %d", signum);
        }
        sigemptyset(&sigset);
        sigaddset(&sigset, signum);
    } else if (SCM_SYS_SIGSET_P(sigs)) {
        sigset = SCM_SYS_SIGSET(sigs)->set;
    } else {
        Scm_Error("bad signal number: must be an integer signal number or a <sys-sigset> object, but got %S", sigs);
    }

    if (SCM_UNDEFINEDP(handler)) return SCM_UNDEFINED;

    struct sigaction act;
    if (SCM_TRUEP(handler)) {
        act.sa_handler = SIG_DFL;
    } else if (SCM_FALSEP(handler)) {
        act.sa_handler = SIG_IGN;
    } else if (SCM_PROCEDUREP(handler)
               && SCM_PROCEDURE_TAKE_NARG_P(handler, 1)) {
        act.sa_handler = sig_handle;
    } else {
        badproc = TRUE;
    }

    if (mask == NULL) {
        /* If no mask is specified, block singals in SIGS. */
        mask = make_sigset();
        mask->set = sigset;
    }

    (void)SCM_INTERNAL_MUTEX_LOCK(sigHandlers.mutex);
    if (!badproc) {
        sigfillset(&act.sa_mask); /* we should block all the signals */
        act.sa_flags = 0;
        for (struct sigdesc *desc=sigDesc; desc->name; desc++) {
            if (!sigismember(&sigset, desc->num)) continue;
            if (!sigismember(&sigHandlers.masterSigset, desc->num)) continue;
            else if (sigaction(desc->num, &act, NULL) != 0) {
                sigactionfailed = desc->num;
            } else {
                sigHandlers.handlers[desc->num] = handler;
                sigHandlers.masks[desc->num] = mask;
            }
        }
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(sigHandlers.mutex);
    if (badproc) Scm_Error("bad signal handling procedure: must be either a procedure that takes at least one argument, #t, or #f, but got %S", handler);
    if (sigactionfailed) Scm_Error("sigaction failed when setting a sighandler for signal %d", sigactionfailed);
    return SCM_UNDEFINED;
}

ScmObj Scm_GetSignalHandler(int signum)
{
    if (signum < 0 || signum >= SCM_NSIG) {
        Scm_Error("bad signal number: %d", signum);
    }
    /* No lock; atomic pointer access */
    return sigHandlers.handlers[signum];
}

ScmObj Scm_GetSignalHandlerMask(int signum)
{
    ScmSysSigset *r;
    if (signum < 0 || signum >= SCM_NSIG) {
        Scm_Error("bad signal number: %d", signum);
    }
    /* No lock; atomic pointer access */
    r = sigHandlers.masks[signum];
    return r? SCM_OBJ(r) : SCM_FALSE;
}

ScmObj Scm_GetSignalHandlers(void)
{
    ScmObj h = SCM_NIL;
    ScmObj handlers[SCM_NSIG];

    /* copy handler vector and master sig set locally, so that we won't
       grab the lock for extensive time */
    (void)SCM_INTERNAL_MUTEX_LOCK(sigHandlers.mutex);
    for (int i=0; i<SCM_NSIG; i++) handlers[i] = sigHandlers.handlers[i];
    sigset_t masterSet = sigHandlers.masterSigset;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(sigHandlers.mutex);

    for (struct sigdesc *desc=sigDesc; desc->name; desc++) {
        if (!sigismember(&masterSet, desc->num)) continue;
        ScmObj hp;
        SCM_FOR_EACH(hp, h) {
            if (SCM_EQ(SCM_CDAR(hp), handlers[desc->num])) {
                sigaddset(&(SCM_SYS_SIGSET(SCM_CAAR(hp))->set), desc->num);
                break;
            }
        }
        if (SCM_NULLP(hp)) {
            ScmSysSigset *set = make_sigset();
            sigaddset(&(set->set), desc->num);
            h = Scm_Acons(SCM_OBJ(set), handlers[desc->num], h);
        }
    }
    return h;
}

/*
 * set/get master signal
 */
sigset_t Scm_GetMasterSigmask(void)
{
    return sigHandlers.masterSigset;
}

/* this should be called before any thread is created. */
 void Scm_SetMasterSigmask(sigset_t *set)
{
    struct sigdesc *desc = sigDesc;
    struct sigaction acton, actoff;

    acton.sa_handler = (void(*)(int))sig_handle;
    acton.sa_mask = *set;
    acton.sa_flags = 0;
    actoff.sa_handler = SIG_DFL;
    sigemptyset(&actoff.sa_mask);
    actoff.sa_flags = 0;

    for (; desc->name; desc++) {
        if (sigismember(&sigHandlers.masterSigset, desc->num)
            && !sigismember(set, desc->num)) {
            /* remove sighandler */
            if (sigaction(desc->num, &actoff, NULL) != 0) {
                Scm_SysError("sigaction on %d failed", desc->num);
            }
            sigHandlers.handlers[desc->num] = SCM_TRUE;
        } else if (!sigismember(&sigHandlers.masterSigset, desc->num)
                   && sigismember(set, desc->num)) {
            /* add sighandler if necessary */
            if (desc->defaultHandle == SIGDEF_DFL) {
                if (sigaction(desc->num, &actoff, NULL) != 0) {
                    Scm_SysError("sigaction on %d failed", desc->num);
                }
                sigHandlers.handlers[desc->num] = SCM_TRUE;
            } else if (desc->defaultHandle != SIGDEF_NOHANDLE) {
                if (sigaction(desc->num, &acton, NULL) != 0) {
                    Scm_SysError("sigaction on %d failed", desc->num);
                }
                switch (desc->defaultHandle) {
                case SIGDEF_ERROR:
                    sigHandlers.handlers[desc->num] = DEFAULT_SIGHANDLER;
                    break;
                case SIGDEF_EXIT:
                    sigHandlers.handlers[desc->num] = EXIT_SIGHANDLER;
                    break;
                case SIGDEF_INDIFFERENT:
                    sigHandlers.handlers[desc->num] = INDIFFERENT_SIGHANDLER;
                    break;
                default:
                    Scm_Panic("Scm_SetMasterSigmask: can't be here");
                }
            }
        }
    }
#ifdef GAUCHE_PTHREAD_SIGNAL
    /* On pthread and when available, we reserve one signal for inter-thread
       communication.  See gauche/pthread.h for the definition of
       GAUCHE_PTHREAD_SIGNAL.  In sigHandlers we set DEFAULT_SIGHANDLER,
       but this signal is intercepted in Scm_SigCheck() so a you can't
       set Scheme handler for this signal. */
    if (sigaction(GAUCHE_PTHREAD_SIGNAL, &acton, NULL) != 0) {
        Scm_SysError("sigaction on %d failed", GAUCHE_PTHREAD_SIGNAL);
    }
    sigHandlers.handlers[GAUCHE_PTHREAD_SIGNAL] = DEFAULT_SIGHANDLER;
#endif  /* GAUCHE_PTHREAD_SIGNAL */
    sigHandlers.masterSigset = *set;
    Scm_VM()->sigMask = sigHandlers.masterSigset;
}

/*============================================================
 * Other signal-related operations
 */

/*
 * Convenience routines hiding platform-dependent stuff
 *
 * TRANSIENT: These used to be used in vm.c, but no longer.  As of 0.9.3 these
 * aren't used anywhere.  Scm_SysSigmask covers those functionalities,
 * so we'll drop them by 1.0.
 */
void Scm_GetSigmask(sigset_t *mask)
{
    if (SIGPROCMASK(SIG_SETMASK, NULL, mask) != 0) {
        Scm_SysError("sigprocmask failed");
    }
}

void Scm_SetSigmask(sigset_t *mask)
{
    if (SIGPROCMASK(SIG_SETMASK, mask, NULL) != 0) {
        Scm_SysError("sigprocmask failed");
    }
}

/*
 * set signal mask
 */

ScmObj Scm_SysSigmask(int how, ScmSysSigset *newmask)
{
    ScmSysSigset *oldmask = make_sigset();
    sigset_t *newset = NULL;

    if (newmask) {
        newset = &(newmask->set);
        if (how != SIG_SETMASK && how != SIG_BLOCK && how != SIG_UNBLOCK) {
            Scm_Error("bad 'how' argument for signal mask action: %d", how);
        }
    }
    if (SIGPROCMASK(how, newset, &(oldmask->set)) != 0) {
        Scm_SysError("sigprocmask failed");
    }
    return SCM_OBJ(oldmask);
}

/*
 * Reset signal handlers except the masked ones.
 * This is called just before we change the signal mask and call exec(2),
 * so that we can avoid the hazard that the signal handler is called
 * between sigsetmask and exec.
 */
void Scm_ResetSignalHandlers(sigset_t *mask)
{
    struct sigdesc *desc = sigDesc;
    struct sigaction act;

    for (; desc->name; desc++) {
        if (!sigismember(&sigHandlers.masterSigset, desc->num)
            && (!mask || !sigismember(mask, desc->num))) {
            act.sa_flags = 0;
            act.sa_handler = SIG_IGN;
            // NB: we tolerate failure of this
            sigaction(desc->num, &act, NULL);
        }
    }
}

/*
 * sigsuspend
 */
static void scm_sigsuspend(sigset_t *mask)
{
#if !defined(GAUCHE_WINDOWS)
    sigset_t omask;
    ScmVM *vm = Scm_VM();
    for (;;) {
        SIGPROCMASK(SIG_BLOCK, &sigHandlers.masterSigset, &omask);
        if (vm->signalPending) {
            SIGPROCMASK(SIG_SETMASK, &omask, NULL);
            Scm_SigCheck(vm);
            continue;
        }
        break;
    }
    sigsuspend(mask);
    SIGPROCMASK(SIG_SETMASK, &omask, NULL);
    SCM_SIGCHECK(vm);
#else  /* GAUCHE_WINDOWS */
    Scm_Error("sigsuspend not supported on Windows port");
#endif /* GAUCHE_WINDOWS */
}

ScmObj Scm_SigSuspend(ScmSysSigset *mask)
{
    scm_sigsuspend(&(mask->set));
    return SCM_UNDEFINED;
}

/*
 * Alternative of 'pause()'
 * we can't use pause() reliably, since the process may miss a signal
 * if it is delivered after the last call of Scm_SigCheck before pause();
 * the signal is queued, but will never be processed until pause() returns
 * by another signal.
 */
ScmObj Scm_Pause(void)
{
    sigset_t omask;
    SIGPROCMASK(SIG_SETMASK, NULL, &omask);
    scm_sigsuspend(&omask);
    return SCM_UNDEFINED;
}

/*
 * Sigwait wrapper
 *
 * The behavior of sigwait is undefined if a signal handler is set to
 * the waiting signal.  On Cygwin, for example, using both signal handler
 * and sigwait makes havoc.  Since Gauche installs sig_handle()
 * implicitly to some signals, a casual user may be confused by the
 * unpredictable behavior when he doesn't reset signal handlers explicitly.
 * So we take care of them here.
 *
 * We remove the signal handlers for the signals to be waited before calling
 * sigwait(), and restore them after its return.  We assume those signals
 * are blocked at this moment (if not, the behavior of sigwait() is
 * undefined), so we don't need to care about race condition.  If another
 * thread replaces signal handlers during this thread's waiting for a
 * signal, it would be reverted upon returning from this function, but
 * such operation is inherently unsafe anyway, so we don't care.
 */
int Scm_SigWait(ScmSysSigset *mask)
{
#if defined(HAVE_SIGWAIT)
    int r = 0, sig = 0;
    int failed_sig = -1;
    int sigwait_called = FALSE;
    int errno_save = 0;
    sigset_t to_wait;        /* real set of signals to wait */
    sigset_t saved;
    struct sigaction act, oacts[SCM_NSIG];

    (void)SCM_INTERNAL_MUTEX_LOCK(sigHandlers.mutex);
    /* we can't wait for the signals Gauche doesn't handle. */
    to_wait = mask->set;
    for (int i=0; i<SCM_NSIG; i++) {
        if (!sigismember(&sigHandlers.masterSigset, i)) {
            sigdelset(&to_wait, i);
        }
    }

    /* Remove C-level handlers */
    sigemptyset(&saved);
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    for (int i=1; i<SCM_NSIG; i++) {
        if (!sigismember(&to_wait, i)) continue;
        if (sigaction(i, &act, &oacts[i]) < 0) {
            failed_sig = i;
            errno_save = errno;
            break;
        }
        sigaddset(&saved, i);
    }

    if (failed_sig < 0) {
        (void)SCM_INTERNAL_MUTEX_UNLOCK(sigHandlers.mutex);
        sigwait_called = TRUE;
        r = sigwait(&to_wait, &sig);
        (void)SCM_INTERNAL_MUTEX_LOCK(sigHandlers.mutex);
    }

    /* Restore C-level handlers */
    for (int i=1; i<SCM_NSIG; i++) {
        if (!sigismember(&saved, i)) continue;
        if (sigaction(i, &oacts[i], NULL) < 0) {
            failed_sig = i;
            errno_save = errno;
        }
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(sigHandlers.mutex);

    /* error handling */
    if (failed_sig >= 0) {
        errno = errno_save;
        Scm_SysError("sigaction(2) call failed on signal %d"
                     " %s sigwait call",
                     failed_sig,
                     sigwait_called? "after" : "before");
    }
    if (r != 0) {
        errno = r;
        Scm_SysError("sigwait failed");
    }
    return sig;
#else  /* !HAVE_SIGWAIT */
    Scm_Error("sigwait not supported on this platform");
    return 0;
#endif
}


/*================================================================
 * Initialize
 */

void Scm__InitSignal(void)
{
    ScmModule *mod = Scm_GaucheModule();
    ScmObj defsigh_sym = Scm_Intern(&default_sighandler_name);

    (void)SCM_INTERNAL_MUTEX_INIT(sigHandlers.mutex);
    sigemptyset(&sigHandlers.masterSigset);
    for (int i=0; i<SCM_NSIG; i++) sigHandlers.handlers[i] = SCM_UNDEFINED;

    Scm_InitStaticClass(&Scm_SysSigsetClass, "<sys-sigset>",
                        mod, NULL, 0);

    for (struct sigdesc *desc = sigDesc; desc->name; desc++) {
        SCM_DEFINE(mod, desc->name, SCM_MAKE_INT(desc->num));
    }
    Scm_Define(mod, SCM_SYMBOL(defsigh_sym), DEFAULT_SIGHANDLER);
}
