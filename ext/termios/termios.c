/*
 * termios.c - termios interface
 *
 *  Copyright(C) 2001-2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: termios.c,v 1.6 2002-04-15 22:04:59 shirok Exp $
 */

#include <string.h>
#include "termios.h"
#include <gauche/class.h>
#include <gauche/extend.h>

static ScmObj termios_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_SysTermiosClass,
                         NULL, NULL, NULL,
                         termios_allocate,
                         NULL);

static ScmObj termios_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmSysTermios *t = SCM_NEW(ScmSysTermios);
    SCM_SET_CLASS(t, SCM_CLASS_SYS_TERMIOS);
    memset(&t->term, sizeof(t->term), 0);
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

static ScmClassStaticSlotSpec termios_slots[] = {
    SCM_CLASS_SLOT_SPEC("iflag", termios_c_iflag_get, termios_c_iflag_set),
    SCM_CLASS_SLOT_SPEC("oflag", termios_c_oflag_get, termios_c_oflag_set),
    SCM_CLASS_SLOT_SPEC("cflag", termios_c_cflag_get, termios_c_cflag_set),
    SCM_CLASS_SLOT_SPEC("lflag", termios_c_lflag_get, termios_c_lflag_set),
    { NULL }
};

ScmObj Scm_MakeSysTermios(void)
{
    return termios_allocate(NULL, SCM_NIL);
}

/*
 * Initializaion
 */

extern void Scm_Init_termiolib(ScmModule *mod);

void Scm_Init_termios(void)
{
    ScmModule *mod;
    SCM_INIT_EXTENSION(termios);
    mod = SCM_MODULE(SCM_FIND_MODULE("gauche.termios", TRUE));
    Scm_InitBuiltinClass(&Scm_SysTermiosClass, "<sys-termios>",
                         termios_slots, sizeof(ScmSysTermios), mod);
    Scm_Init_termiolib(mod);

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
}
