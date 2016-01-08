/*
 * win-compat.h - Compatibility header for Win32 (MinGW)
 *
 *  Collection of #defines and typedefs to fool Gauche source enough
 *  to compile it on Windows.
 *  I tried to put mingw-specific stuff here as much as possible,
 *  instead of scattering #ifdefs around the sources.
 */

#ifndef GAUCHE_WIN_COMPAT_H
#define GAUCHE_WIN_COMPAT_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* A common symbol, used throughout the Gauche source */
#define GAUCHE_WINDOWS 1

/* Preparation. */
#ifndef WINVER
#define WINVER 0x0500           /* we support Windows 2000 or later */
#endif /*WINVER*/

#if defined(UNICODE) && !defined(_UNICODE)
#define _UNICODE                /* Windows needs both UNICODE and _UNICODE */
#endif /* UNICODE && !_UNICODE */

#include <winsock2.h>           /* MinGW needs this before windows.h */
#include <windows.h>
#include <shlwapi.h>
#include <utime.h>
#include <mswsock.h>
#include <direct.h>
#include <tchar.h>
#undef small  /* windows.h defines 'small' as 'char'; what's the hell? */

#ifndef _BSDTYPES_DEFINED
typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;
#define _BSDTYPES_DEFINED
#endif /* _BSDTYPES_DEFINED */

/* Mingw-w64 only defines sigset_t when _POSIX is defined. */
#if defined(__MINGW64_VERSION_MAJOR) && !defined(_POSIX)
typedef _sigset_t sigset_t;
#endif  /*defined(__MINGW64_VERSION_MAJOR) && !defined(_POSIX)*/


/*======================================================================
 * Time calculation
 * Win32API's FILETIME is 64bit time count since 1601/1/1 UTC, measured
 * in 100nanosecs.  The macro conveniently converts the value to
 * (seconds, microseconds) pair since Unix Epoch.
 */
#define SCM_FILETIME_TO_UNIXTIME(ft, secs, usecs)                       \
  do {                                                                  \
    const int64_t off_ = ((int64_t)27111902UL << 32) + 3577643008UL;    \
    int64_t val_ = ((int64_t)(ft).dwHighDateTime << 32) + (ft).dwLowDateTime; \
    val_ = (val_ - off_)/10;                                            \
    secs  = (u_long)(val_ / 1000000);                                   \
    usecs = (u_long)(val_ % 1000000);                                   \
  } while (0)

/*==================================================================
 * Users and groups
 * Windows doesn't really have users and groups in the sense of POSIX,
 * so we fake them.
 */
#ifndef gid_t
typedef int gid_t;
#endif
#ifndef uid_t
typedef int uid_t;
#endif

struct passwd {
    const char *pw_name;
    const char *pw_passwd;
    int         pw_uid;
    int         pw_gid;
    const char *pw_comment;
    const char *pw_gecos;
    const char *pw_dir;
    const char *pw_shell;
};

struct passwd *getpwuid(uid_t);
struct passwd *getpwnam(const char *t);

struct group {
    const char  *gr_name;
    const char  *gr_passwd;
    gid_t        gr_gid;
    const char  **gr_mem;
};

struct group *getgrgid(gid_t);
struct group *getgrnam(const char *t);

/*=======================================================================
 * No sigsetjmps.  Fake it with ordinary setjmps.
 */
typedef jmp_buf  sigjmp_buf;

#define sigsetjmp(jbuf, flag)  setjmp(jbuf)
#define siglongjmp(jbuf, val)  longjmp(jbuf, val)

/*=======================================================================
 * Signal stuff
 * NB: this may need to be changed if MinGW starts supporting
 * sigemptyset etc.
 */

/* Windows doesn't support SIGKILL explicitly, but we want to emulate
   (sys-kill SIGKILL) by TerminateProcess.
   The signal number 9 is unused in Windows at this moment.  Chekc signal.h.*/
#define SIGKILL 9

#ifndef _SIGSET_T_
#define _SIGSET_T_
typedef unsigned long sigset_t;
#endif /* _SIGSET_T_ */

#define sigemptyset(pset)    (*(pset) = 0)
#define sigfillset(pset)     (*(pset) = (unsigned int)-1)
#define sigaddset(pset, num) (*(pset) |= (1L<<(num)))
#define sigdelset(pset, num) (*(pset) &= ~(1L<<(num)))
#define sigismember(pset, num) (*(pset) & (1L<<(num)))

#ifndef SIG_SETMASK
#define SIG_SETMASK (0)
#define SIG_BLOCK   (1)
#define SIG_UNBLOCK (2)
#endif /*SIG_SETMASK*/

struct sigaction {
    int          sa_flags;
    sigset_t     sa_mask;
#if defined(__MINGW32__)
    __p_sig_fn_t sa_handler;   /* see mingw/include/signal.h about the type */
#else  /* MSVC */
    void         (*sa_handler)(int);
#endif
};

/* there's a dummy sigaction defined in src/signal.c */
int sigaction(int signum, const struct sigaction *act, struct sigaction *oact);

/*=======================================================================
 * wchar <-> mbchar stuff.
 */

/* Implementation of Scm_MBS2WCS/WCS2MBS is in win-compat.c */
#if defined(LIBGAUCHE_BODY)
extern __declspec(dllexport) WCHAR *Scm_MBS2WCS(const char *s);
extern __declspec(dllexport) const char *Scm_WCS2MBS(const WCHAR *s);
#else  /*!LIBGAUCHE_BODY*/
extern __declspec(dllimport) WCHAR *Scm_MBS2WCS(const char *s);
extern __declspec(dllimport) const char *Scm_WCS2MBS(const WCHAR *s);
#endif /*!LIBGAUCHE_BODY*/

#if defined(UNICODE)
#define SCM_MBS2WCS(s)  Scm_MBS2WCS(s)
#define SCM_WCS2MBS(s)  Scm_WCS2MBS(s)
#else  /* !UNICODE */
#define SCM_MBS2WCS(s)  (s)
#define SCM_WCS2MBS(s)  (s)
#endif /* !UNICODE */

/* Replace some system calls with wide-char counterparts
   NB: Windows' mkdir() and _wmkdir() takes one argument.
   NB: stat() needs special treatment; MinGW defines its own macro.
 */
#if defined(UNICODE)
#define open(path, ...)    _wopen(Scm_MBS2WCS(path), __VA_ARGS__)
#define access(path, mode) _waccess(Scm_MBS2WCS(path), mode)
#define chdir(dir)         _wchdir(Scm_MBS2WCS(dir))
#define chmod(path, mode)  _wchmod(Scm_MBS2WCS(path), mode)
#define mkdir(dir)         _wmkdir(Scm_MBS2WCS(dir))
#define remove(path)       _wremove(Scm_MBS2WCS(path))
#define rename(o, n)       _wrename(Scm_MBS2WCS(o), Scm_MBS2WCS(n))
#define rmdir(dir)         _wrmdir(Scm_MBS2WCS(dir))
#define unlink(path)       _wunlink(Scm_MBS2WCS(path))
#define system(path)       _wsystem(Scm_MBS2WCS(path))
#endif /*UNICODE*/

/*===================================================================
 * Miscellaneous POSIX stuff
 */
uid_t getuid(void);
uid_t geteuid(void);
gid_t getgid(void);
gid_t getegid(void);
pid_t getppid(void);

int link(const char *existing, const char *newpath);
int fork(void);
int kill(pid_t pid, int signal);
int pipe(int fd[]);
char *ttyname(int desc);
unsigned int alarm(unsigned int seconds);

#ifndef __MINGW64_VERSION_MAJOR
int truncate(const char *path, off_t len);
int ftruncate(int fd, off_t len);
#endif

#define WNOHANG   (1L<<0)
#define WUNTRACED (1L<<1)

/* Windows doesn't really distinguish how the process is ended.  We
   kind of emulate it with the following way:
   For normal exit via Scm_Exit() - use the lower 8 bits of status
     code as they are.
   For signalled temination by Scm_SysKill() - the sending process
     sends 0x100 + signal_number as the exit code.
 */
#define WIFEXITED(status)   (((status)>>8)!=0xff)
#define WEXITSTATUS(stauts) (status)
#define WIFSIGNALED(status) (((status)>>8)==0xff)
#define WTERMSIG(stauts)    ((status)&0xff)
#define WIFSTOPPED(status)  FALSE
#define WSTOPSIG(status)    (status)

/* followings are in auxsys.c */
const char *getlogin(void);

struct tms {
    u_int tms_utime;
    u_int tms_stime;
    u_int tms_cutime;
    u_int tms_cstime;
};

clock_t times(struct tms *buf);

/*
 * Fakes for ext/fcntl
 */
struct flock {
    short l_type;
    short l_whence;
    off_t l_start;
    off_t l_len;
    pid_t l_pid;
};

#define F_DUPFD  0
#define F_GETFD  1
#define F_SETFD  2
#define F_GETFL  3
#define F_SETFL  4
#define F_GETLK  5
#define F_SETLK  6
#define F_SETLKW 7

#define F_RDLCK  0
#define F_WRLCK  1
#define F_UNLCK  2

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* GAUCHE_WIN_COMPAT_H */
