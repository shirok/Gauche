/*
 * signal.c - signal handling
 *
 *  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, disribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: signal.c,v 1.15 2002-07-08 12:33:47 shirok Exp $
 */

#include <stdlib.h>
#include <signal.h>
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
 */

/* Master signal handler vector. */
static struct sigHandlersRec {
    sigset_t masterSigset;      /* the signals Gauche is allowed to handle */
    ScmObj handlers[NSIG];      /* Scheme signal handlers */
    ScmInternalMutex mutex;
} sigHandlers;

/* Table of signals and its name, to display sigset content. */
#define SIGDEF(x, flag)  { #x, x, flag }

static struct sigdesc {
    const char *name;
    int num;
    int defaultHandle;
} sigDesc[] = {
    SIGDEF(SIGHUP, TRUE),       /* Hangup (POSIX).  */
    SIGDEF(SIGINT, TRUE),       /* Interrupt (ANSI).  */
    SIGDEF(SIGQUIT, TRUE),      /* Quit (POSIX).  */
    SIGDEF(SIGILL, FALSE),      /* Illegal instruction (ANSI).  */
#ifdef SIGTRAP
    SIGDEF(SIGTRAP, TRUE),      /* Trace trap.  */
#endif
    SIGDEF(SIGABRT, FALSE),     /* Abort (ANSI).  */
#ifdef SIGIOT
    SIGDEF(SIGIOT, TRUE),       /* IOT trap (4.2 BSD).  */
#endif
#ifdef SIGBUS
    SIGDEF(SIGBUS, FALSE),      /* BUS error (4.2 BSD).  */
#endif
    SIGDEF(SIGFPE, TRUE),       /* Floating-point exception (ANSI).  */
    SIGDEF(SIGKILL, FALSE),     /* Kill, unblockable (POSIX).  */
    SIGDEF(SIGUSR1, TRUE),      /* User-defined signal 1 (POSIX).  */
    SIGDEF(SIGSEGV, FALSE),     /* Segmentation violation (ANSI).  */
    SIGDEF(SIGUSR2, TRUE),      /* User-defined signal 2 (POSIX).  */
    SIGDEF(SIGPIPE, TRUE),      /* Broken pipe (POSIX).  */
    SIGDEF(SIGALRM, TRUE),      /* Alarm clock (POSIX).  */
    SIGDEF(SIGTERM, TRUE),      /* Termination (ANSI).  */
#ifdef SIGSTKFLT
    SIGDEF(SIGSTKFLT, TRUE),    /* Stack fault.  */
#endif
    SIGDEF(SIGCHLD, FALSE),     /* Child status has changed (POSIX).  */
    SIGDEF(SIGCONT, FALSE),     /* Continue (POSIX).  */
    SIGDEF(SIGSTOP, FALSE),     /* Stop, unblockable (POSIX).  */
    SIGDEF(SIGTSTP, FALSE),     /* Keyboard stop (POSIX).  */
    SIGDEF(SIGTTIN, FALSE),     /* Background read from tty (POSIX).  */
    SIGDEF(SIGTTOU, FALSE),     /* Background write to tty (POSIX).  */
#ifdef SIGURG
    SIGDEF(SIGURG, FALSE),      /* Urgent condition on socket (4.2 BSD).  */
#endif
#ifdef SIGXCPU
    SIGDEF(SIGXCPU, FALSE),     /* CPU limit exceeded (4.2 BSD).  */
#endif
#ifdef SIGXFSZ
    SIGDEF(SIGXFSZ, TRUE),      /* File size limit exceeded (4.2 BSD).  */
#endif
#ifdef SIGVTALRM
    SIGDEF(SIGVTALRM, TRUE),    /* Virtual alarm clock (4.2 BSD).  */
#endif
#ifdef SIGPROF
    SIGDEF(SIGPROF, TRUE),      /* Profiling alarm clock (4.2 BSD).  */
#endif
#ifdef SIGWINCH
    SIGDEF(SIGWINCH, FALSE),    /* Window size change (4.3 BSD, Sun).  */
#endif
#ifdef SIGPOLL
    SIGDEF(SIGPOLL, TRUE),      /* Pollable event occurred (System V).  */
#endif
#ifdef SIGIO
    SIGDEF(SIGIO, TRUE),        /* I/O now possible (4.2 BSD).  */
#endif
#ifdef SIGPWR
    SIGDEF(SIGPWR, FALSE),      /* Power failure restart (System V).  */
#endif
    { NULL, -1 }
};

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
 * default handler
 */
static ScmObj default_sighandler(ScmObj *args, int nargs, void *data)
{
    int signum;
    struct sigdesc *desc;
    const char *name = NULL;
    
    SCM_ASSERT(nargs == 1 && SCM_INTP(args[0]));
    signum = SCM_INT_VALUE(args[0]);

    for (desc = sigDesc; desc->name; desc++) {
        if (desc->num == signum) {
            name = desc->name;
            break;
        }
    }
    if (name) {
        Scm_Error("unhandled signal %d (%s)", signum, name);
    } else {
        Scm_Error("unhandled signal %d (unknown signal)", signum);
    }
    return SCM_UNDEFINED;       /* dummy */
}

static SCM_DEFINE_STRING_CONST(default_sighandler_name,
                               "%default-signal-handler", 22, 22);
static SCM_DEFINE_SUBR(default_sighandler_stub, 1, 0,
                       SCM_OBJ(&default_sighandler_name),
                       default_sighandler,
                       NULL, NULL);

#define DEFAULT_SIGHANDLER    SCM_OBJ(&default_sighandler_stub)

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
    ScmSysSigset *s = SCM_ALLOCATE(ScmSysSigset, klass);
    SCM_SET_CLASS(s, klass);
    sigemptyset(&s->set);
    return SCM_OBJ(s);
}

/* multifunction on sigset
    if delp == FALSE, signals are added to set.
    else, signals are removed from set.
    signals is a list of either integer or #t (all signals), or other sigset.
*/
ScmObj Scm_SysSigsetOp(ScmSysSigset *set, ScmObj signals, int delp)
{
    ScmObj cp;
    
    if (!SCM_PAIRP(signals)) {
        Scm_Error("list of signals required, but got %S", signals);
    }
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

/*
 * System's signal handler - just enqueue the signal
 */

static void sig_handle(int signum)
{
    ScmVM *vm = Scm_VM();
    /* It is possible that vm == NULL at this point, if the thread is
       terminating and in the cleanup phase. */
    if (vm == NULL) return;
    
    if (vm->sigOverflow) return;
    
    if (vm->sigQueueHead <= vm->sigQueueTail) {
        vm->sigQueue[vm->sigQueueTail++] = signum;
        if (vm->sigQueueTail >= SCM_VM_SIGQ_SIZE) {
            vm->sigQueueTail = 0;
        }
    } else {
        vm->sigQueue[vm->sigQueueTail++] = signum;
    }
    if (vm->sigQueueTail == vm->sigQueueHead) {
        Scm_Error("signal queue overflow\n");
    }
}

/*
 * Called from VM's safe point to flush the queued signals.
 * VM already checks there's a pending signal in the queue.
 */
void Scm_SigCheck(ScmVM *vm)
{
    ScmObj tail, cell, sp;
    int sigQsize, i;
    int sigQcopy[SCM_VM_SIGQ_SIZE]; /* copy of signal queue */

    /* Copy VM's signal queue to local storage, for we can't call
       storage allocation during blocking signals. */
    sigprocmask(SIG_BLOCK, &vm->sigMask, NULL);
    for (sigQsize = 0; vm->sigQueueHead != vm->sigQueueTail; sigQsize++) {
        sigQcopy[sigQsize] = vm->sigQueue[vm->sigQueueHead++];
        if (vm->sigQueueHead >= SCM_VM_SIGQ_SIZE) vm->sigQueueHead = 0;
    }
    vm->sigOverflow = 0; /*TODO: we should do something*/
    sigprocmask(SIG_UNBLOCK, &vm->sigMask, NULL);

    /* Now, prepare queued signal handlers
       If an error is thrown in this loop, the queued signals will be
       lost---it doesn't look like so, but I may overlook something. */
    tail = vm->sigPending;
    if (!SCM_NULLP(tail)) tail = Scm_LastPair(tail);
    for (i=0; i<sigQsize; i++) {
        if (SCM_PROCEDUREP(sigHandlers.handlers[sigQcopy[i]])) {
            cell = Scm_Acons(sigHandlers.handlers[sigQcopy[i]],
                             SCM_MAKE_INT(sigQcopy[i]),
                             SCM_NIL);
            if (SCM_NULLP(tail)) {
                vm->sigPending = tail = cell;
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
    SCM_FOR_EACH(sp, vm->sigPending) {
        ScmObj h = SCM_CAR(sp);
        vm->sigPending = SCM_CDR(sp);
        Scm_Apply(SCM_CAR(h), SCM_LIST1(SCM_CDR(h)));
    }
}

/*
 * set-signal-handler!
 */
ScmObj Scm_SetSignalHandler(ScmObj sigs, ScmObj handler)
{
    struct sigaction act;
    sigset_t sigset;
    int badproc = FALSE, sigactionfailed = FALSE, i;
    if (SCM_INTP(sigs)) {
        int signum = SCM_INT_VALUE(sigs);
        if (signum < 0 || signum >= NSIG) {
            Scm_Error("bad signal number: %d", signum);
        }
        sigemptyset(&sigset);
        sigaddset(&sigset, signum);
    } else if (SCM_SYS_SIGSET_P(sigs)) {
        sigset = SCM_SYS_SIGSET(sigs)->set;
    } else {
        Scm_Error("bad signal number: must be an integer signal number or a <sys-sigset> object, but got %S", sigs);
    }
    
    (void)SCM_INTERNAL_MUTEX_LOCK(sigHandlers.mutex);
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
    if (!badproc) {
        sigemptyset(&act.sa_mask);
        act.sa_flags = 0;
        for (i=0; i<NSIG; i++) {
            if (sigismember(&sigset, i)
                && sigismember(&sigHandlers.masterSigset, i)) {
                if (sigaction(i, &act, NULL) != 0) {
                    sigactionfailed = TRUE;
                    break;
                }
                sigHandlers.handlers[i] = handler;
            }
        }
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(sigHandlers.mutex);
    if (badproc) Scm_Error("bad signal handling procedure: must be either a procedure that takes at least one argument, #t, or #f, but got %S", handler);
    if (sigactionfailed) Scm_Error("sigaction failed when setting a sighandler");
    return SCM_UNDEFINED;
}

ScmObj Scm_GetSignalHandler(int signum)
{
    ScmObj r;
    if (signum < 0 || signum >= NSIG) {
        Scm_Error("bad signal number: %d", signum);
    }
    (void)SCM_INTERNAL_MUTEX_LOCK(sigHandlers.mutex);
    r = sigHandlers.handlers[signum];
    (void)SCM_INTERNAL_MUTEX_UNLOCK(sigHandlers.mutex);
    return r;
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

    acton.sa_handler = (void(*)())sig_handle;
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
            /* add sighandler, only if defaultHandle is true. */
            if (desc->defaultHandle) {
                if (sigaction(desc->num, &acton, NULL) != 0) {
                    Scm_SysError("sigaction on %d failed", desc->num);
                }
                sigHandlers.handlers[desc->num] = DEFAULT_SIGHANDLER;
            }
        }
    }
    sigHandlers.masterSigset = *set;
    Scm_VM()->sigMask = sigHandlers.masterSigset;
}

/*
 * initialize
 */

void Scm__InitSignal(void)
{
    ScmModule *mod = Scm_GaucheModule();
    ScmObj defsigh_sym = Scm_Intern(&default_sighandler_name);
    struct sigdesc *desc;
    int i;

    (void)SCM_INTERNAL_MUTEX_INIT(sigHandlers.mutex);
    sigemptyset(&sigHandlers.masterSigset);
    for (i=0; i<NSIG; i++) sigHandlers.handlers[i] = SCM_FALSE;
    
    Scm_InitBuiltinClass(&Scm_SysSigsetClass, "<sys-sigset>", NULL,
                         sizeof(ScmSysSigset), mod);

    for (desc = sigDesc; desc->name; desc++) {
        SCM_DEFINE(mod, desc->name, SCM_MAKE_INT(desc->num));
    }
    Scm_Define(mod, SCM_SYMBOL(defsigh_sym), DEFAULT_SIGHANDLER);
}
