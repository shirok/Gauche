/*
 * termios.c - termios interface
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

#include <string.h>
#include <gauche.h>
#include <gauche/class.h>
#include <gauche/extend.h>

#include "gauche-termios.h"

#if !defined(GAUCHE_WINDOWS)

/*
 * Termios interface
 */
static ScmObj termios_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_SysTermiosClass,
                         NULL, NULL, NULL,
                         termios_allocate,
                         NULL);

static ScmObj termios_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmSysTermios *t = SCM_NEW(ScmSysTermios);
    SCM_SET_CLASS(t, SCM_CLASS_SYS_TERMIOS);
    memset(&t->term, 0, sizeof(t->term));
    return SCM_OBJ(t);
}

/* slot accessors */
#define TERMIOS_GET_N_SET(name) \
  static ScmObj SCM_CPP_CAT3(termios_, name, _get)(ScmSysTermios* t)         \
  { return Scm_MakeIntegerFromUI((u_long)t->term.name); }                    \
  static void SCM_CPP_CAT3(termios_, name, _set)(ScmSysTermios* t, ScmObj v) \
  {                                                                          \
      if (!SCM_INTEGERP(v)) Scm_Error("integer required, but got %S", v);    \
      t->term.name = (tcflag_t)Scm_GetUInteger(v);                           \
  }

TERMIOS_GET_N_SET(c_iflag)
TERMIOS_GET_N_SET(c_oflag)
TERMIOS_GET_N_SET(c_cflag)
TERMIOS_GET_N_SET(c_lflag)

static ScmObj termios_c_cc_get(ScmSysTermios* t)
{
    return Scm_MakeU8VectorFromArray(NCCS, (const unsigned char*)t->term.c_cc);
}

static void termios_c_cc_set(ScmSysTermios* t, ScmObj val)
{
    if (!SCM_U8VECTORP(val)) {
        Scm_Error("cc type must be a u8vector, but got %S", val);
    }
    if (SCM_U8VECTOR_SIZE(val) != NCCS) {
        Scm_Error("size of cc must be %u, but got %u",
                  NCCS, SCM_U8VECTOR_SIZE(val));
    }
    memcpy(t->term.c_cc, SCM_U8VECTOR_ELEMENTS(val), NCCS);
}

static ScmClassStaticSlotSpec termios_slots[] = {
    SCM_CLASS_SLOT_SPEC("iflag", termios_c_iflag_get, termios_c_iflag_set),
    SCM_CLASS_SLOT_SPEC("oflag", termios_c_oflag_get, termios_c_oflag_set),
    SCM_CLASS_SLOT_SPEC("cflag", termios_c_cflag_get, termios_c_cflag_set),
    SCM_CLASS_SLOT_SPEC("lflag", termios_c_lflag_get, termios_c_lflag_set),
    SCM_CLASS_SLOT_SPEC("cc", termios_c_cc_get, termios_c_cc_set),
    SCM_CLASS_SLOT_SPEC_END()
};

ScmObj Scm_MakeSysTermios(void)
{
    return termios_allocate(NULL, SCM_NIL);
}

/*
 * pty
 */

#ifdef HAVE_OPENPTY
ScmObj Scm_Openpty(ScmObj slaveterm)
{
    int master, slave;
    struct termios *term = NULL;

    if (SCM_SYS_TERMIOS_P(slaveterm)) {
        term = &SCM_SYS_TERMIOS(slaveterm)->term;
    }
    if (openpty(&master, &slave, NULL, term, NULL) < 0) {
        Scm_SysError("openpty failed");
    }
    return Scm_Values2(SCM_MAKE_INT(master), SCM_MAKE_INT(slave));
}
#endif /*HAVE_OPENPTY*/

#ifdef HAVE_FORKPTY
ScmObj Scm_Forkpty(ScmObj slaveterm)
{
    int master;
    pid_t pid;
    struct termios *term = NULL;
    if (SCM_SYS_TERMIOS_P(slaveterm)) {
        term = &SCM_SYS_TERMIOS(slaveterm)->term;
    }
    if ((pid = forkpty(&master, NULL, term, NULL)) < 0) {
        Scm_SysError("forkpty failed");
    }
    return Scm_Values2(Scm_MakeInteger(pid), SCM_MAKE_INT(master));
}

ScmObj Scm_ForkptyAndExec(ScmString *file, ScmObj args, ScmObj iomap,
                          ScmObj slaveterm, ScmSysSigset *mask)
{
    int argc = Scm_Length(args);
    struct termios *term = NULL;

    if (argc < 1) {
        Scm_Error("argument list must have at least one element: %S", args);
    }
    char **argv = Scm_ListToCStringArray(args, TRUE, NULL);
    const char *program = Scm_GetStringConst(file);

    if (SCM_SYS_TERMIOS_P(slaveterm)) {
        term = &SCM_SYS_TERMIOS(slaveterm)->term;
    }

    int *fds = Scm_SysPrepareFdMap(iomap);

    int master;
    pid_t pid;
    if ((pid = forkpty(&master, NULL, term, NULL)) < 0) {
        Scm_SysError("forkpty failed");
    }
    if (pid == 0) {
        Scm_SysSwapFds(fds);
        if (mask) {
            Scm_ResetSignalHandlers(&mask->set);
            Scm_SysSigmask(SIG_SETMASK, mask);
        }
        execvp(program, (char *const*)argv);
        /* here, we failed */
        Scm_Panic("exec failed: %s: %s", program, strerror(errno));
    }
    return Scm_Values2(Scm_MakeInteger(pid), SCM_MAKE_INT(master));
}
#endif /*HAVE_FORKPTY*/

#endif /*!defined(GAUCHE_WINDOWS) */

/*
 * Initializaion
 */

void Scm_Init_termios(void)
{
    SCM_INIT_EXTENSION(gauche__termios);
    ScmModule *mod = SCM_FIND_MODULE("gauche.termios", SCM_FIND_MODULE_CREATE);

#if !defined(GAUCHE_WINDOWS)
    Scm_InitStaticClass(&Scm_SysTermiosClass, "<sys-termios>", mod,
                        termios_slots, 0);

    /* Constants for termios.  Non-POSIX symbols are guarded by #ifdef's */
#define DEFSYM(sym) \
    SCM_DEFINE(mod, #sym, Scm_MakeIntegerFromUI(sym))

    /* c_iflag masks */
    DEFSYM(IGNBRK);
    DEFSYM(BRKINT);
    DEFSYM(IGNPAR);
    DEFSYM(PARMRK);
    DEFSYM(INPCK);
    DEFSYM(ISTRIP);
    DEFSYM(INLCR);
    DEFSYM(IGNCR);
    DEFSYM(ICRNL);
    DEFSYM(IXON);
    DEFSYM(IXOFF);
#ifdef IXANY
    DEFSYM(IXANY);
#endif
#ifdef IUCLC
    DEFSYM(IUCLC);
#endif
#ifdef IMAXBEL
    DEFSYM(IMAXBEL);
#endif

    /* c_oflag masks */
    DEFSYM(OPOST);
#ifdef OLCUC
    DEFSYM(OLCUC);
#endif
#ifdef ONLCR
    DEFSYM(ONLCR);
#endif
#ifdef OCRNL
    DEFSYM(OCRNL);
#endif
#ifdef ONOCR
    DEFSYM(ONOCR);
#endif
#ifdef ONLRET
    DEFSYM(ONLRET);
#endif
#ifdef OFILL
    DEFSYM(OFILL);
#endif
#ifdef OFDEL
    DEFSYM(OFDEL);
#endif
#ifdef NLDLY
    DEFSYM(NLDLY);
#endif
#ifdef NL0
    DEFSYM(NL0);
#endif
#ifdef NL1
    DEFSYM(NL1);
#endif
#ifdef CRDLY
    DEFSYM(CRDLY);
#endif
#ifdef CR0
    DEFSYM(CR0);
#endif
#ifdef CR1
    DEFSYM(CR1);
#endif
#ifdef CR2
    DEFSYM(CR2);
#endif
#ifdef CR3
    DEFSYM(CR3);
#endif
#ifdef BSDLY
    DEFSYM(BSDLY);
#endif
#ifdef BS0
    DEFSYM(BS0);
#endif
#ifdef BS1
    DEFSYM(BS1);
#endif
#ifdef VTDLY
    DEFSYM(VTDLY);
#endif
#ifdef VT0
    DEFSYM(VT0);
#endif
#ifdef VT1
    DEFSYM(VT1);
#endif
#ifdef FFDLY
    DEFSYM(FFDLY);
#endif
#ifdef FF0
    DEFSYM(FF0);
#endif
#ifdef FF1
    DEFSYM(FF1);
#endif

    /* c_cflag masks */
    DEFSYM(CLOCAL);
    DEFSYM(CREAD);
    DEFSYM(CSIZE);
    DEFSYM(CS5);
    DEFSYM(CS6);
    DEFSYM(CS7);
    DEFSYM(CS8);
    DEFSYM(CSTOPB);
    DEFSYM(HUPCL);
    DEFSYM(PARENB);
    DEFSYM(PARODD);
#ifdef CIBAUD
    DEFSYM(CIBAUD);
#endif
#ifdef CRTSCTS
    DEFSYM(CRTSCTS);
#endif

    /* c_lflag masks */
    DEFSYM(ECHO);
    DEFSYM(ECHOE);
    DEFSYM(ECHOK);
    DEFSYM(ECHONL);
    DEFSYM(ICANON);
    DEFSYM(ISIG);
    DEFSYM(NOFLSH);
    DEFSYM(TOSTOP);
    DEFSYM(IEXTEN);
#ifdef XCASE
    DEFSYM(XCASE);
#endif
#ifdef ECHOCTL
    DEFSYM(ECHOCTL);
#endif
#ifdef ECHOPRT
    DEFSYM(ECHOPRT);
#endif
#ifdef ECHOKE
    DEFSYM(ECHOKE);
#endif
#ifdef FLUSH0
    DEFSYM(FLUSH0);
#endif
#ifdef PENDIN
    DEFSYM(PENDIN);
#endif

    /* c_cc size */
    DEFSYM(NCCS);

    /* disable character */
    DEFSYM(_POSIX_VDISABLE);

    /* c_cc subscripts */
    DEFSYM(VEOF);
    DEFSYM(VEOL);
    DEFSYM(VERASE);
    DEFSYM(VINTR);
    DEFSYM(VKILL);
    DEFSYM(VMIN);
    DEFSYM(VQUIT);
    DEFSYM(VSTART);
    DEFSYM(VSTOP);
    DEFSYM(VSUSP);
    DEFSYM(VTIME);
#ifdef VDISCARD
    DEFSYM(VDISCARD);
#endif
#ifdef VDSUSP
    DEFSYM(VDSUSP);
#endif
#ifdef EOL2
    DEFSYM(VEOL2);
#endif
#ifdef LNEXT
    DEFSYM(VLNEXT);
#endif
#ifdef VREPRINT
    DEFSYM(VREPRINT);
#endif
#ifdef VSTATUS
    DEFSYM(VSTATUS);
#endif
#ifdef WERASE
    DEFSYM(VWERASE);
#endif
#ifdef VSWTCH
    DEFSYM(VSWTCH);
#endif
#ifdef VSWTC
    DEFSYM(VSWTC);
#endif

    /* extra baudrates.   <= B38400 is defined in termiolib.stub */
#ifdef B57600
    DEFSYM(B57600);
#endif
#ifdef B115200
    DEFSYM(B115200);
#endif
#ifdef B230400
    DEFSYM(B230400);
#endif

#endif /*!GAUCHE_WINDOWS*/
}
