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
 *  $Id: signal.c,v 1.7 2002-01-22 11:39:46 shirok Exp $
 */

#include <signal.h>
#include "gauche.h"
#include "gauche/vm.h"
#include "gauche/class.h"

/* Signals
 *
 *  C-application that embeds Gauche can specify a set of signals
 *  that Gauche should handle.
 *
 *  Gauche sets its internal signal handlers for them.  The signal
 *  is queued to the signal buffer of the thread that caught the
 *  signal.
 *
 *  VM calls Scm_SigCheck() at the "safe" point, which flushes
 *  the signal queue and make a list of handlers to be called.
 *
 *  This signal handling mechanism only deals with the
 *  thread-private data.
 */

/* Master set of signals that the Gauche traps.  Need to be MT safe. */
static sigset_t masterSigset;

/* Table of signals and its name, to display sigset content. */
#define SIGDEF(x)  { #x, x }

static struct sigdesc {
    const char *name;
    int num;
} sigDesc[] = {
    SIGDEF(SIGHUP),	/* Hangup (POSIX).  */
    SIGDEF(SIGINT),	/* Interrupt (ANSI).  */
    SIGDEF(SIGQUIT),	/* Quit (POSIX).  */
    SIGDEF(SIGILL),	/* Illegal instruction (ANSI).  */
    SIGDEF(SIGTRAP),	/* Trace trap (POSIX).  */
    SIGDEF(SIGABRT),	/* Abort (ANSI).  */
#ifdef SIGIOT
    SIGDEF(SIGIOT),	/* IOT trap (4.2 BSD).  */
#endif
#ifdef SIGBUS
    SIGDEF(SIGBUS),	/* BUS error (4.2 BSD).  */
#endif
    SIGDEF(SIGFPE),	/* Floating-point exception (ANSI).  */
    SIGDEF(SIGKILL),	/* Kill, unblockable (POSIX).  */
    SIGDEF(SIGUSR1),	/* User-defined signal 1 (POSIX).  */
    SIGDEF(SIGSEGV),	/* Segmentation violation (ANSI).  */
    SIGDEF(SIGUSR2),	/* User-defined signal 2 (POSIX).  */
    SIGDEF(SIGPIPE),	/* Broken pipe (POSIX).  */
    SIGDEF(SIGALRM),	/* Alarm clock (POSIX).  */
    SIGDEF(SIGTERM),	/* Termination (ANSI).  */
#ifdef SIGSTKFLT
    SIGDEF(SIGSTKFLT),	/* Stack fault.  */
#endif
    SIGDEF(SIGCHLD),	/* Child status has changed (POSIX).  */
    SIGDEF(SIGCONT),	/* Continue (POSIX).  */
    SIGDEF(SIGSTOP),	/* Stop, unblockable (POSIX).  */
    SIGDEF(SIGTSTP),	/* Keyboard stop (POSIX).  */
    SIGDEF(SIGTTIN),	/* Background read from tty (POSIX).  */
    SIGDEF(SIGTTOU),	/* Background write to tty (POSIX).  */
#ifdef SIGURG
    SIGDEF(SIGURG),	/* Urgent condition on socket (4.2 BSD).  */
#endif
#ifdef SIGXCPU
    SIGDEF(SIGXCPU),	/* CPU limit exceeded (4.2 BSD).  */
#endif
#ifdef SIGXFSZ
    SIGDEF(SIGXFSZ),	/* File size limit exceeded (4.2 BSD).  */
#endif
#ifdef SIGVTALRM
    SIGDEF(SIGVTALRM),	/* Virtual alarm clock (4.2 BSD).  */
#endif
#ifdef SIGPROF
    SIGDEF(SIGPROF),	/* Profiling alarm clock (4.2 BSD).  */
#endif
#ifdef SIGWINCH
    SIGDEF(SIGWINCH),	/* Window size change (4.3 BSD, Sun).  */
#endif
#ifdef SIGPOLL
    SIGDEF(SIGPOLL),	/* Pollable event occurred (System V).  */
#endif
#ifdef SIGIO
    SIGDEF(SIGIO),	/* I/O now possible (4.2 BSD).  */
#endif
#ifdef SIGPWR
    SIGDEF(SIGPWR),	/* Power failure restart (System V).  */
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
    int i;
    ScmObj sp, tail;

    /*sigprocmask(SIG_BLOCK, &vm->sigMask, NULL);*/
    /* NB: if an error occurs during the critical section,
       some signals may be lost. */
    SCM_UNWIND_PROTECT {
        tail = vm->sigPending;
        if (!SCM_NULLP(tail)) tail = Scm_LastPair(tail);
        while (vm->sigQueueHead != vm->sigQueueTail) {
            int signum = vm->sigQueue[vm->sigQueueHead++];
            if (vm->sigQueueHead >= SCM_VM_SIGQ_SIZE) vm->sigQueueHead = 0;

            SCM_FOR_EACH(sp, vm->sigHandlers) {
                ScmObj sigh = SCM_CAR(sp);
                sigset_t *set;
                SCM_ASSERT(SCM_PAIRP(sigh)&&SCM_SYS_SIGSET_P(SCM_CAR(sigh)));
                set = &(SCM_SYS_SIGSET(SCM_CAR(sigh))->set);
                if (sigismember(set, signum)) {
                    if (SCM_NULLP(tail)) {
                        tail = Scm_Cons(SCM_CDR(sigh), SCM_NIL);
                    } else {
                        SCM_SET_CDR(tail, Scm_Cons(SCM_CDR(sigh), SCM_NIL));
                        tail = SCM_CDR(tail);
                    }
                    break;
                }
            }
        }
    }
    SCM_WHEN_ERROR {
        /*sigprocmask(SIG_UNBLOCK, &vm->sigMask, NULL);*/
        SCM_NEXT_HANDLER;
    }
    SCM_END_PROTECT;
    /* TODO: signal overflow handling */
    vm->sigOverflow = 0;
    /*sigprocmask(SIG_UNBLOCK, &vm->sigMask, NULL);*/
}

/*
 * %with-signal-handlers
 */
/* this is a low-level routine that will be called from
   with-signal-handlers macro.  handlers are list of (<sigset> . <handler>)
   where <handler> is either a handler procedure or #f (ignore).
*/

static ScmObj set_sighandlers(ScmObj *args, int nargs, void *data)
{
    /* NB: change sigmask of this thread */
    ScmObj handlers = SCM_OBJ(data);
    Scm_VM()->sigHandlers = handlers;
}

ScmObj Scm_VMWithSignalHandlers(ScmObj handlers, ScmProcedure *thunk)
{
    ScmObj cp, before, after, newhandlers;
    ScmVM *vm = Scm_VM();
    /* check validity of args */
    SCM_FOR_EACH(cp, handlers) {
        ScmObj p = SCM_CAR(cp);
        if (!SCM_PAIRP(p) || !SCM_SYS_SIGSET_P(SCM_CAR(p))
            || !SCM_PROCEDUREP(SCM_CDR(p))) {
            Scm_Error("bad sighandler entry: %S", p);
        }
    }
    if (!SCM_NULLP(cp)) Scm_Error("bad sighandler list: %S", handlers);
    newhandlers = Scm_Append2(handlers, vm->sigHandlers);
    before = Scm_MakeSubr(set_sighandlers, newhandlers, 0, 0, SCM_FALSE);
    after = Scm_MakeSubr(set_sighandlers, vm->sigHandlers, 0, 0, SCM_FALSE);
    return Scm_VMDynamicWind(before, SCM_OBJ(thunk), after);
}

/*
 * set/get master signal
 */
/* TODO: MT protected */
sigset_t Scm_GetMasterSigmask(void)
{
    return masterSigset;
}

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
        if (sigismember(&masterSigset, desc->num)
            && !sigismember(set, desc->num)) {
            /* remove sighandler */
            if (sigaction(desc->num, &actoff, NULL) != 0) {
                Scm_SysError("sigaction on %d failed\n", desc->num);
            }
        } else if (!sigismember(&masterSigset, desc->num)
                   && sigismember(set, desc->num)) {
            /* add sighandler */
            if (sigaction(desc->num, &acton, NULL) != 0) {
                Scm_SysError("sigaction on %d failed\n", desc->num);
            }
        }
    }
    masterSigset = *set;
}

/*
 * initialize
 */

void Scm__InitSignal(void)
{
    ScmModule *mod = Scm_GaucheModule();
    struct sigdesc *desc;

    sigemptyset(&masterSigset);
    
    Scm_InitBuiltinClass(&Scm_SysSigsetClass, "<sys-sigset>", NULL,
                         sizeof(ScmSysSigset), mod);

    for (desc = sigDesc; desc->name; desc++) {
        SCM_DEFINE(mod, desc->name, SCM_MAKE_INT(desc->num));
    }
}
    
