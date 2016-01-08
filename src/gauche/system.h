/*
 * system.h - Gauche system interface
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

#ifndef GAUCHE_SYSTEM_H
#define GAUCHE_SYSTEM_H

/* This file provides some OS abstraction layer for Gauche. */

/*==============================================================
 * System call wrapper
 */

/* Our rule of thumb is that we always restart system calls
 * when it is interrupted prematurely.  The following macro
 * hides restarting.
 */

#define SCM_SYSCALL3(result, expr, check)                       \
  do {                                                          \
    (result) = (expr);                                          \
    if ((check) && (errno == EINTR || errno == EPIPE)) {        \
      ScmVM *vm__ = Scm_VM();                                   \
      int epipe__ = (errno == EPIPE);                           \
      errno = 0;                                                \
      SCM_SIGCHECK(vm__);                                       \
      if (epipe__) {                                            \
        errno = EPIPE;                                          \
        break;                                                  \
      }                                                         \
    } else {                                                    \
      break;                                                    \
    }                                                           \
  } while (1)

#define SCM_SYSCALL(result, expr) \
  SCM_SYSCALL3(result, expr, (result < 0))

/*==============================================================
 * Utility functions
 */

/* Some commonly-used utilities */

SCM_EXTERN int Scm_GetPortFd(ScmObj port_or_fd, int needfd);

SCM_EXTERN off_t  Scm_IntegerToOffset(ScmObj i);
SCM_EXTERN ScmObj Scm_OffsetToInteger(off_t o);

/*==============================================================
 * Filesystem
 */

SCM_EXTERN ScmObj Scm_ReadDirectory(ScmString *pathname);
SCM_EXTERN ScmObj Scm_GetCwd(void);

#define SCM_PATH_ABSOLUTE       (1L<<0)
#define SCM_PATH_EXPAND         (1L<<1)
#define SCM_PATH_CANONICALIZE   (1L<<2)
#define SCM_PATH_FOLLOWLINK     (1L<<3) /* not supported yet */
SCM_EXTERN ScmObj Scm_NormalizePathname(ScmString *pathname, int flags);
SCM_EXTERN ScmObj Scm_TmpDir(void);
SCM_EXTERN ScmObj Scm_DirName(ScmString *filename);
SCM_EXTERN ScmObj Scm_BaseName(ScmString *filename);

/* struct stat */
typedef struct ScmSysStatRec {
    SCM_HEADER;
    struct stat statrec;
} ScmSysStat;

SCM_CLASS_DECL(Scm_SysStatClass);
#define SCM_CLASS_SYS_STAT    (&Scm_SysStatClass)
#define SCM_SYS_STAT(obj)     ((ScmSysStat*)(obj))
#define SCM_SYS_STAT_P(obj)   (SCM_XTYPEP(obj, SCM_CLASS_SYS_STAT))
#define SCM_SYS_STAT_STAT(obj) (&SCM_SYS_STAT(obj)->statrec)

SCM_EXTERN ScmObj Scm_MakeSysStat(void); /* returns empty SysStat */

/*==============================================================
 * Time
 */

/* time_t
 * NB: POSIX defines time_t to be a type to represent number of seconds
 * since Epoch.  It may be a structure.  In Gauche we just convert it
 * to a number.
 */
SCM_EXTERN ScmObj Scm_MakeSysTime(time_t time);
SCM_EXTERN time_t Scm_GetSysTime(ScmObj val);

SCM_EXTERN void Scm_GetTimeOfDay(u_long *sec, u_long *usec);
SCM_EXTERN long Scm_CurrentMicroseconds();
SCM_EXTERN int  Scm_ClockGetTimeMonotonic(u_long *sec, u_long *nsec);
SCM_EXTERN int  Scm_ClockGetResMonotonic(u_long *sec, u_long *nsec);

/* Gauche also has a <time> object, as specified in SRFI-18, SRFI-19
 * and SRFI-21.  It can be constructed from the basic system interface
 * such as sys-time or sys-gettimeofday.
 */
typedef struct ScmTimeRec {
    SCM_HEADER;
    ScmObj type;       /* 'time-utc by default.  see SRFI-19 */
    ScmInt64 sec;      /* seconds */
    long nsec;         /* nanoseconds */
} ScmTime;

SCM_CLASS_DECL(Scm_TimeClass);
#define SCM_CLASS_TIME        (&Scm_TimeClass)
#define SCM_TIME(obj)         ((ScmTime*)obj)
#define SCM_TIMEP(obj)        SCM_XTYPEP(obj, SCM_CLASS_TIME)

SCM_EXTERN ScmObj Scm_CurrentTime(void);
SCM_EXTERN ScmObj Scm_MakeTime(ScmObj type, long sec, long nsec);
SCM_EXTERN ScmObj Scm_MakeTime64(ScmObj type, ScmInt64 sec, long nsec);
SCM_EXTERN ScmObj Scm_IntSecondsToTime(long sec);
SCM_EXTERN ScmObj Scm_Int64SecondsToTime(ScmInt64 sec);
SCM_EXTERN ScmObj Scm_RealSecondsToTime(double sec);
SCM_EXTERN ScmObj Scm_TimeToSeconds(ScmTime *t);

/* struct timespec compatibility handling.  MinGW32 runtime v3.21, at least, has
   incompatible struct timespec. */
#if defined(HAVE_STRUCT_TIMESPEC) && (!defined(GAUCHE_WINDOWS) || defined(__MINGW64_VERSION_MAJOR))
typedef struct timespec ScmTimeSpec;
#else  /*!(defined(HAVE_STRUCT_TIMESPEC) && (!defined(GAUCHE_WINDOWS) || defined(__MINGW64_VERSION_MAJOR)))*/
typedef struct ScmTimeSpecRec {
    time_t tv_sec;
    long   tv_nsec;
} ScmTimeSpec;
#endif /*!(defined(HAVE_STRUCT_TIMESPEC) && (!defined(GAUCHE_WINDOWS) || defined(__MINGW64_VERSION_MAJOR)))*/

SCM_EXTERN ScmTimeSpec *Scm_GetTimeSpec(ScmObj t, ScmTimeSpec *spec);

/* sched_yield */
SCM_EXTERN void   Scm_YieldCPU(void);

/* struct tm */
typedef struct ScmSysTmRec {
    SCM_HEADER;
    struct tm tm;
} ScmSysTm;

SCM_CLASS_DECL(Scm_SysTmClass);
#define SCM_CLASS_SYS_TM      (&Scm_SysTmClass)
#define SCM_SYS_TM(obj)       ((ScmSysTm*)(obj))
#define SCM_SYS_TM_P(obj)     (SCM_XTYPEP(obj, SCM_CLASS_SYS_TM))
#define SCM_SYS_TM_TM(obj)    SCM_SYS_TM(obj)->tm

SCM_EXTERN ScmObj Scm_MakeSysTm(struct tm *);

SCM_EXTERN int    Scm_NanoSleep(const ScmTimeSpec *req,
                                ScmTimeSpec *rem);

/*==============================================================
 * Groups and users
 */

/* struct group */
typedef struct ScmSysGroupRec {
    SCM_HEADER;
    ScmObj name;
    ScmObj gid;
    ScmObj passwd;
    ScmObj mem;
} ScmSysGroup;

SCM_CLASS_DECL(Scm_SysGroupClass);
#define SCM_CLASS_SYS_GROUP    (&Scm_SysGroupClass)
#define SCM_SYS_GROUP(obj)     ((ScmSysGroup*)(obj))
#define SCM_SYS_GROUP_P(obj)   (SCM_XTYPEP(obj, SCM_CLASS_SYS_GROUP))

SCM_EXTERN ScmObj Scm_GetGroupById(gid_t gid);
SCM_EXTERN ScmObj Scm_GetGroupByName(ScmString *name);

/* struct passwd */
typedef struct ScmSysPasswdRec {
    SCM_HEADER;
    ScmObj name;
    ScmObj passwd;
    ScmObj uid;
    ScmObj gid;
    ScmObj gecos;
    ScmObj dir;
    ScmObj shell;
    ScmObj pwclass;
} ScmSysPasswd;

SCM_CLASS_DECL(Scm_SysPasswdClass);
#define SCM_CLASS_SYS_PASSWD    (&Scm_SysPasswdClass)
#define SCM_SYS_PASSWD(obj)     ((ScmSysPasswd*)(obj))
#define SCM_SYS_PASSWD_P(obj)   (SCM_XTYPEP(obj, SCM_CLASS_SYS_PASSWD))

SCM_EXTERN ScmObj Scm_GetPasswdById(uid_t uid);
SCM_EXTERN ScmObj Scm_GetPasswdByName(ScmString *name);

/*==============================================================
 * Program execution
 */

SCM_EXTERN int    Scm_IsSugid(void);

/* flags for Scm_SysExec */
enum {
    SCM_EXEC_WITH_FORK = (1L<<0), /* fork() before exec(), i.e. spawn(). */
    SCM_EXEC_DETACHED = (1L<<1)   /* try to detach from process group.
                                     good for daemoninzing. */
};

SCM_EXTERN ScmObj Scm_SysExec(ScmString *file, ScmObj args,
                              ScmObj iomap, ScmSysSigset *mask,
                              ScmString *dir, int flags);
SCM_EXTERN int   *Scm_SysPrepareFdMap(ScmObj iomap);
SCM_EXTERN void   Scm_SysSwapFds(int *fds);

SCM_EXTERN void   Scm_SysKill(ScmObj process, int signal);
SCM_EXTERN ScmObj Scm_SysWait(ScmObj process, int options);

/*==============================================================
 * Select
 */

/* select */
#ifdef HAVE_SELECT
typedef struct ScmSysFdsetRec {
    SCM_HEADER;
    int maxfd;
    fd_set fdset;
} ScmSysFdset;

SCM_CLASS_DECL(Scm_SysFdsetClass);
#define SCM_CLASS_SYS_FDSET     (&Scm_SysFdsetClass)
#define SCM_SYS_FDSET(obj)      ((ScmSysFdset*)(obj))
#define SCM_SYS_FDSET_P(obj)    (SCM_XTYPEP(obj, SCM_CLASS_SYS_FDSET))

SCM_EXTERN ScmObj Scm_SysSelect(ScmObj rfds, ScmObj wfds, ScmObj efds,
                                ScmObj timeout);
SCM_EXTERN ScmObj Scm_SysSelectX(ScmObj rfds, ScmObj wfds, ScmObj efds,
                                 ScmObj timeout);
#else  /*!HAVE_SELECT*/
/* dummy definitions */
typedef struct ScmHeaderRec ScmSysFdset;
#define SCM_SYS_FDSET(obj)      (obj)
#define SCM_SYS_FDSET_P(obj)    (FALSE)
#endif /*!HAVE_SELECT*/

/*==============================================================
 * Miscellaneous
 */

SCM_EXTERN int    Scm_Mkstemp(char *tmpl);
SCM_EXTERN ScmObj Scm_SysMkstemp(ScmString *tmpl);
SCM_EXTERN ScmObj Scm_SysMkdtemp(ScmString *tmpl);
SCM_EXTERN ScmObj Scm_Environ(void);
SCM_EXTERN const char *Scm_GetEnv(const char *name);
SCM_EXTERN void Scm_SetEnv(const char *name, const char *value, int overwrite);
SCM_EXTERN void Scm_UnsetEnv(const char *name);
SCM_EXTERN void Scm_ClearEnv(void);
SCM_EXTERN int  Scm_AvailableProcessors(void);

/*==============================================================
 * Windows-specific utility functions
 */

#if defined(GAUCHE_WINDOWS)
/* special object to wrap windows handle */
SCM_EXTERN ScmObj Scm_MakeWinHandle(HANDLE h, ScmObj type);
SCM_EXTERN int    Scm_WinHandleP(ScmObj obj, ScmObj type);
SCM_EXTERN HANDLE Scm_WinHandle(ScmObj h, ScmObj type);

SCM_EXTERN ScmObj Scm_MakeWinProcess(HANDLE h);
SCM_EXTERN int    Scm_WinProcessP(ScmObj p);
SCM_EXTERN pid_t  Scm_WinProcessPID(ScmObj p);
SCM_EXTERN HANDLE Scm_WinProcess(ScmObj p);
#endif /* GAUCHE_WINDOWS */

#endif /* GAUCHE_SYSTEM_H */

