/*
 * win-compat.h - Compatibility header for Win32 (MSVC and MinGW)
 *
 *  Collection of #defines and typedefs to fool Gauche source enough
 *  to compile it on Windows.
 *  I tried to put mingw-specific stuff here as much as possible,
 *  instead of scattering #ifdefs around the sources.
 */

/* Since we MinGW and MSVC share large part of compatibility hacks,
   we put both support here.  If either one requires a special support,
   we delimit it by defined(__MINGW32__) or defined(MSVC).
*/

#ifndef GAUCHE_WIN_COMPAT_H
#define GAUCHE_WIN_COMPAT_H

/* A common symbol, used throughout the Gauche source */
#define GAUCHE_WINDOWS 1

/* Preparation.
   Note: for MSVC, we already have these in config.h.
*/
#if defined(__MINGW32__)
#ifndef WINVER
#define WINVER 0x0500           /* we support Windows 2000 or later */
#endif /*WINVER*/
#include <winsock2.h>           /* MinGW needs this before windows.h */
#include <windows.h>
#include <shlwapi.h>
#include <utime.h>
#undef small  /* windows.h defines 'small' as 'char'; what's the hell? */
#ifndef _BSDTYPES_DEFINED
typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;
#define _BSDTYPES_DEFINED
#endif /* _BSDTYPES_DEFINED */
#ifndef _T
#define _T(x) TEXT(x)   /* MSVC unicode macro */
#endif /* _T */
#endif /* __MINGW32__ */

/* MSVC linker is broken; it cannot handle address of variables in
   external dlls in constant expression. */
#if defined(_MSC_VER)
#define GAUCHE_BROKEN_LINKER_WORKAROUND 1
#endif

#include <mswsock.h>

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

/*====================================================================
 * string stuff
 */
#if defined(_MSC_VER)
#define snprintf _snprintf
#define strcasecmp _stricmp
#endif /* _MSC_VER */

/*====================================================================
 * POSIX I/O and process functions
 */
#if defined(_MSC_VER)
#include <io.h>
#include <process.h>
#include <direct.h>
#include <sys/utime.h>

#define read   _read
#define write  _write
#define open   _open
#define close  _close
#define lseek  _lseek
#define access _access
#define chmod  _chmod
#define unlink _unlink
#define umask  _umask
#define isatty _isatty
#define chdir  _chdir
#define getcwd _getcwd
#define getpid _getpid
#define mkdir  _mkdir
#define rmdir  _rmdir
#define execvp _execvp
#define utime  _utime
#define utimbuf _utimbuf
#define putenv _putenv
#define dup    _dup
#define dup2   _dup2

#define O_APPEND    _O_APPEND
#define O_BINARY    _O_BINARY
#define O_TEXT      _O_TEXT
#define O_CREAT     _O_CREAT
#define O_RDONLY    _O_RDONLY
#define O_RDWR      _O_RDWR
#define O_WRONLY    _O_WRONLY
#define O_TRUNC     _O_TRUNC

#define O_ACCMODE   (O_RDONLY|O_WRONLY|O_RDWR)

#define R_OK        0x02
#define W_OK        0x04
#define X_OK        0x06
#define F_OK        0x00

#define S_ISREG(modebits)  ((modebits)&_S_IFREG)
#define S_ISDIR(modebits)  ((modebits)&_S_IFDIR)

/* windows doesn't really have device files and named fifo */
#define S_ISCHR(modebits)  FALSE
#define S_ISBLK(modebits)  FALSE
#define S_ISFIFO(modebits) FALSE

#endif /* _MSC_VER */

/* wchar <-> mbchar stuff.  implementation in win-compat.c */
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

/*===================================================================
 * Miscellaneous POSIX stuff
 */
#if defined(_MSC_VER)
typedef int  pid_t;
#endif

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
int truncate(const char *path, off_t len);
int ftruncate(int fd, off_t len);
unsigned int alarm(unsigned int seconds);

struct timespec {
    time_t tv_sec;
    long   tv_nsec;
};
#define HAVE_STRUCT_TIMESPEC 1

int nanosleep(const struct timespec *req, struct timespec *rem);

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

#endif /* GAUCHE_WIN_COMPAT_H */
