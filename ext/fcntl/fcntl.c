/*
 * fcntl.c - fcntl interface
 *
 *   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
 *
 *  $Id: fcntl.c,v 1.10 2003-07-05 03:29:10 shirok Exp $
 */

#include <string.h>
#include <errno.h>
#include <gauche.h>
#include <gauche/class.h>
#include <gauche/extend.h>

#undef SCM_EXTERN
#define SCM_EXTERN  extern
#include "gauche/fcntl.h"

/* struct flock */

static ScmObj flock_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_SysFlockClass,
                         NULL, NULL, NULL,
                         flock_allocate,
                         NULL);

static ScmObj flock_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmSysFlock *f = SCM_NEW(ScmSysFlock);
    SCM_SET_CLASS(f, SCM_CLASS_SYS_FLOCK);
    memset(&f->lock, sizeof(f->lock), 0);
    return SCM_OBJ(f);
}

#define FLOCK_GET_N_SET(name, type)                                       \
  static ScmObj SCM_CPP_CAT3(flock_, name, _get)(ScmSysFlock* t)          \
  { return Scm_MakeInteger(t->lock.name); }                               \
  static void SCM_CPP_CAT3(flock_, name, _set)(ScmSysFlock* t, ScmObj v)  \
  {                                                                       \
      if (!SCM_INTEGERP(v)) Scm_Error("integer required, but got %S", v); \
      t->lock.name = (type)Scm_GetInteger(v);                             \
  }

FLOCK_GET_N_SET(l_type, short)
FLOCK_GET_N_SET(l_whence, short)
FLOCK_GET_N_SET(l_start, off_t)
FLOCK_GET_N_SET(l_len, off_t)
FLOCK_GET_N_SET(l_pid, pid_t)

static ScmClassStaticSlotSpec flock_slots[] = {
    SCM_CLASS_SLOT_SPEC("type",   flock_l_type_get, flock_l_type_set),
    SCM_CLASS_SLOT_SPEC("whence", flock_l_whence_get, flock_l_whence_set),
    SCM_CLASS_SLOT_SPEC("start",  flock_l_start_get, flock_l_start_set),
    SCM_CLASS_SLOT_SPEC("len",    flock_l_len_get, flock_l_len_set),
    SCM_CLASS_SLOT_SPEC("pid",    flock_l_pid_get, flock_l_pid_set),
    { NULL }
};

ScmObj Scm_MakeSysFlock(void)
{
    return flock_allocate(SCM_CLASS_SYS_FLOCK, SCM_NIL);
}

/*
 * Fcntl bridge
 */
static const char *flag_name(int flag)
{
    switch (flag) {
    case F_GETFD: return "F_GETFD";
    case F_SETFD: return "F_SETFD";
    case F_GETFL: return "F_GETFL";
    case F_SETFL: return "F_SETFL";
    case F_DUPFD: return "F_DUPFD";
    case F_GETLK: return "F_GETLK";
    case F_SETLK: return "F_SETLK";
    case F_SETLKW: return "F_SETLKW";
    }
    return "(unknown flag)";
}

ScmObj Scm_SysFcntl(ScmObj port_or_fd, int op, ScmObj arg)
{
    int fd = Scm_GetPortFd(port_or_fd, TRUE), iarg, r;
    ScmSysFlock *fl;
    
    switch (op) {
    case F_GETFD:; case F_GETFL:;
        r = Scm_SysCall(fcntl(fd, op));
        if (r < 0) {
            Scm_SysError("fcntl(%s) failed", flag_name(op));
        }
        return Scm_MakeInteger(r);
    case F_SETFD:; case F_SETFL:; case F_DUPFD:;
        if (!SCM_EXACTP(arg)) {
            Scm_Error("exact integer required for fcntl(%s), but got %S",
                      flag_name(op), arg);
        }
        r = Scm_SysCall(fcntl(fd, op, Scm_GetInteger(arg)));
        if (r < 0) {
            Scm_SysError("fcntl(%s) failed", flag_name(op));
        }
        return Scm_MakeInteger(r);
    case F_GETLK:; case F_SETLK:; case F_SETLKW:;
        if (!SCM_SYS_FLOCK_P(arg)) {
            Scm_Error("flock object required for fcntl(%s), but got %S",
                      flag_name(op), arg);
        }
        fl = SCM_SYS_FLOCK(arg);
        r = Scm_SysCall(fcntl(fd, op, &fl->lock));
        if (op == F_SETLK) {
            if (r >= 0) return SCM_TRUE;
            if (errno == EAGAIN) return SCM_FALSE;
        }
        if (r < 0) Scm_SysError("fcntl(%s) failed", flag_name(op));
        return SCM_TRUE;
    default:
        Scm_Error("unknown operation code (%d) for fcntl", op);
        return SCM_UNDEFINED;   /* dummy */
    }
}

/*
 * Initialization
 */

extern void Scm_Init_fcntlib(ScmModule *mod);

void Scm_Init_fcntl(void)
{
    ScmModule *mod;

    SCM_INIT_EXTENSION(fcntl);
    mod = SCM_MODULE(SCM_FIND_MODULE("gauche.fcntl", TRUE));
    Scm_InitBuiltinClass(&Scm_SysFlockClass, "<sys-flock>",
                         flock_slots, sizeof(ScmSysFlock), mod);
    Scm_Init_fcntlib(mod);
}

