/*
 * termios.c - termios interface
 *
 *  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: termios.c,v 1.1 2001-09-07 11:35:30 shirok Exp $
 */

#include "termios.h"
#include <gauche/class.h>

static ScmObj termios_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_SysTermiosClass,
                         NULL, NULL, NULL,
                         termios_allocate,
                         SCM_CLASS_DEFAULT_CPL);

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
    SCM_CLASS_SLOT_SPEC("iflag",
                        termios_c_iflag_get,
                        termios_c_iflag_set, SCM_FALSE),
    SCM_CLASS_SLOT_SPEC("oflag",
                        termios_c_oflag_get,
                        termios_c_oflag_set, SCM_FALSE),
    SCM_CLASS_SLOT_SPEC("cflag",
                        termios_c_cflag_get,
                        termios_c_cflag_set, SCM_FALSE),
    SCM_CLASS_SLOT_SPEC("lflag",
                        termios_c_lflag_get,
                        termios_c_lflag_set, SCM_FALSE),
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
    ScmModule *mod = SCM_MODULE(SCM_FIND_MODULE("gauche.termios", TRUE));

    Scm_InitBuiltinClass(&Scm_SysTermiosClass, "<sys-termios>",
                         termios_slots, mod);
    Scm_Init_termiolib(mod);
}



