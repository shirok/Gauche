/*
 * fcntl.c - fcntl interface
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

#define _GNU_SOURCE  /* for Linux, this enables additional features */

#include <gauche.h>
#include <string.h>
#include <errno.h>
#include <gauche/class.h>
#include <gauche/extend.h>

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
    memset(&f->lock, 0, sizeof(f->lock));
    return SCM_OBJ(f);
}

#define FLOCK_GET_N_SET(name, type, make, get)                            \
  static ScmObj SCM_CPP_CAT3(flock_, name, _get)(ScmSysFlock* t)          \
  { return make(t->lock.name); }                                          \
  static void SCM_CPP_CAT3(flock_, name, _set)(ScmSysFlock* t, ScmObj v)  \
  {                                                                       \
      if (!SCM_INTEGERP(v)) Scm_Error("integer required, but got %S", v); \
      t->lock.name = (type)get(v);                                        \
  }

FLOCK_GET_N_SET(l_type, short, Scm_MakeInteger, Scm_GetInteger)
FLOCK_GET_N_SET(l_whence, short, Scm_MakeInteger, Scm_GetInteger)
FLOCK_GET_N_SET(l_start, off_t, Scm_OffsetToInteger, Scm_IntegerToOffset)
FLOCK_GET_N_SET(l_len, off_t, Scm_OffsetToInteger, Scm_IntegerToOffset)
FLOCK_GET_N_SET(l_pid, pid_t, Scm_MakeInteger, Scm_GetInteger)

static ScmClassStaticSlotSpec flock_slots[] = {
    SCM_CLASS_SLOT_SPEC("type",   flock_l_type_get, flock_l_type_set),
    SCM_CLASS_SLOT_SPEC("whence", flock_l_whence_get, flock_l_whence_set),
    SCM_CLASS_SLOT_SPEC("start",  flock_l_start_get, flock_l_start_set),
    SCM_CLASS_SLOT_SPEC("len",    flock_l_len_get, flock_l_len_set),
    SCM_CLASS_SLOT_SPEC("pid",    flock_l_pid_get, flock_l_pid_set),
    SCM_CLASS_SLOT_SPEC_END()
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
#define FLAG_NAME(n) case n: return #n
    switch (flag) {
        FLAG_NAME(F_GETFD);
        FLAG_NAME(F_SETFD);
        FLAG_NAME(F_GETFL);
        FLAG_NAME(F_SETFL);
        FLAG_NAME(F_DUPFD);
        FLAG_NAME(F_GETLK);
        FLAG_NAME(F_SETLK);
        FLAG_NAME(F_SETLKW);
#if defined(F_GETOWN)
        FLAG_NAME(F_GETOWN);
#endif
#if defined(F_SETOWN)
        FLAG_NAME(F_SETOWN);
#endif
#if defined(F_GETSIG)
        FLAG_NAME(F_GETSIG);
#endif
#if defined(F_SETSIG)
        FLAG_NAME(F_SETSIG);
#endif
#if defined(F_GETLEASE)
        FLAG_NAME(F_GETLEASE);
#endif
#if defined(F_SETLEASE)
        FLAG_NAME(F_SETLEASE);
#endif
#if defined(F_NOTIFY)
        FLAG_NAME(F_NOTIFY);
#endif
    }
    return "(unknown flag)";
#undef FLAG_NAME
}

ScmObj Scm_SysFcntl(ScmObj port_or_fd, int op, ScmObj arg)
{
#if !defined(GAUCHE_WINDOWS)
    int fd = Scm_GetPortFd(port_or_fd, TRUE), r;

    switch (op) {
    case F_GETFD:; case F_GETFL:;
#if defined(F_GETOWN)           /* BSD and Linux specific */
    case F_GETOWN:;
#endif /*F_GETOWN*/
#if defined(F_GETSIG)           /* Linux specific */
    case F_GETSIG:;
#endif /*F_GETSIG */
#if defined(F_GETLEASE)         /* Linux specific */
    case F_GETLEASE:;
#endif /*F_GETLEASE */
        SCM_SYSCALL(r, fcntl(fd, op));
        if (r == -1) { /*NB: F_GETOWN may return a negative value on success*/
            Scm_SysError("fcntl(%s) failed", flag_name(op));
        }
        return Scm_MakeInteger(r);
    case F_SETFD:; case F_SETFL:; case F_DUPFD:;
#if defined(F_SETOWN)           /* BSD and Linux specific */
    case F_SETOWN:;
#endif /*F_SETOWN*/
#if defined(F_SETSIG)           /* Linux specific */
    case F_SETSIG:;
#endif /*F_SETSIG */
#if defined(F_SETLEASE)         /* Linux specific */
    case F_SETLEASE:;
#endif /*F_SETLEASE */
#if defined(F_NOTIFY)           /* Linux specific */
    case F_NOTIFY:;
#endif /*F_NOTIFY */
        if (!SCM_EXACTP(arg)) {
            Scm_Error("exact integer required for fcntl(%s), but got %S",
                      flag_name(op), arg);
        }
        SCM_SYSCALL(r, fcntl(fd, op, Scm_GetInteger(arg)));
        if (r < 0) {
            Scm_SysError("fcntl(%s) failed", flag_name(op));
        }
        return Scm_MakeInteger(r);
    case F_GETLK:; case F_SETLK:; case F_SETLKW:;
        if (!SCM_SYS_FLOCK_P(arg)) {
            Scm_Error("flock object required for fcntl(%s), but got %S",
                      flag_name(op), arg);
        }
        ScmSysFlock *fl = SCM_SYS_FLOCK(arg);
        SCM_SYSCALL(r, fcntl(fd, op, &fl->lock));
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
#else  /*GAUCHE_WINDOWS*/
    Scm_Error("fcntl not supported on MinGW port");
    return SCM_UNDEFINED; /*dummy*/
#endif /*GAUCHE_WINDOWS*/
}

/*
 * Initialization
 */

//extern void Scm_Init_fcntlib(ScmModule *mod);

void Scm_Init_fcntl(void)
{
    //SCM_INIT_EXTENSION(gauche__fcntl);
    ScmModule *mod = SCM_FIND_MODULE("gauche.fcntl", SCM_FIND_MODULE_CREATE);
    Scm_InitStaticClass(&Scm_SysFlockClass, "<sys-flock>",
                        mod, flock_slots, 0);
    //    Scm_Init_fcntlib(mod);

#ifndef GAUCHE_WINDOWS
    Scm_AddFeature("gauche.sys.fcntl", NULL);
#endif
}
