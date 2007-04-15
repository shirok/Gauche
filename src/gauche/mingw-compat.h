/*
 * mingw-compat.h - MinGW compatibility
 *
 *  Collection of #defines and typedefs to fool Gauche source enough
 *  to compile it on Windows/MinGW.
 *  I tried to put mingw-specific stuff here as much as possible,
 *  instead of scattering #ifdefs around the sources.
 *
 *  $Id: mingw-compat.h,v 1.5 2007-04-15 10:36:03 shirok Exp $
 */

#ifndef GAUCHE_MINGW_COMPAT_H
#define GAUCHE_MINGW_COMPAT_H

#include <windows.h>
#include <shlwapi.h>

/* windows.h defines 'small' as 'char'; what's the hell? */
#undef small

/* MinGW's sys/types.h doesn't define these.
   It appearse that _BSDTYPES_DEFINED is the symbol commonly used
   to guard them from being typedef'ed multiple times.  
   (see gmon.h or winsock.h in MinGW distribution) */

#ifndef _BSDTYPES_DEFINED
typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;
#define _BSDTYPES_DEFINED
#endif /* _BSDTYPES_DEFINED */

/*
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

/* times(2) emulation is in ext/auxsys/auxsys.c */
struct tms {
    u_int tms_utime;
    u_int tms_stime;
    u_int tms_cutime;
    u_int tms_cstime;
};

clock_t times(struct tms *buf);

/*
 * Users and groups
 * MinGW doesn't really have users and groups in the sense of POSIX,
 * so we fake them.
 */
#ifndef gid_t
typedef int gid_t;
#endif
#ifndef uid_t
typedef int uid_t;
#endif

struct passwd {
    char *pw_name;
    char *pw_passwd;
    int   pw_uid;
    int   pw_gid;
    char *pw_comment;
    char *pw_gecos;
    char *pw_dir;
    char *pw_shell;
};

struct passwd *getpwuid(uid_t);
struct passwd *getpwnam(const char *t);

struct group {
    char  *gr_name;
    char  *gr_passwd;
    gid_t gr_gid;
    char  **gr_mem;
};

struct group *getgrgid(gid_t);
struct group *getgrnam(const char *t);

/* No sigsetjmps.  Fake it with ordinary setjmps. */
typedef jmp_buf  sigjmp_buf;

#define sigsetjmp(jbuf, flag)  setjmp(jbuf)
#define siglongjmp(jbuf, val)  longjmp(jbuf, val)

/*
 * sinaction emulation layer
 * NB: this may need to be changed if MinGW starts supporting
 * sigemptyset etc.
 */
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
    __p_sig_fn_t sa_handler;   /* see mingw/include/signal.h about the type */
};

/* there's a dummy sigaction defined in src/signal.c */
int sigaction(int signum, const struct sigaction *act, struct sigaction *oact);

/*
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
int truncate(const char *path, off_t len);
int ftruncate(int fd, off_t len);
unsigned int alarm(unsigned int seconds);

/* followings are in auxsys.c */
const char *getlogin(void);

/*
 * Fakes for networking (ext/net)
 */
struct sockaddr_un {
    unsigned short sun_family;
    char sun_path[108];
};

/* winsock2 appears to have only inet_addr(), the obsolete interaface */
int inet_aton(const char *cp, struct in_addr *inp);

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

#endif /* GAUCHE_MINGW_COMPAT_H */
