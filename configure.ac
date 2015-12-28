dnl Process this file with autoconf to produce a configure script.
AC_PREREQ([2.68])
AC_INIT([Gauche],[0.9.5_pre1],[shiro@acm.org],,[http://practical-scheme.net/gauche/])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AC_CONFIG_HEADER([src/gauche/config.h])
AC_CONFIG_MACRO_DIRS([m4])

# ABI_VERSION is set to major.minor of Gauche version by default.
# It can be overridden by uncommenting the following line.
# (This may be useful for "prerelease", such as 0.99.x as pre-1.0)
# GAUCHE_ABI_VERSION=X.Y

dnl ==========================================================
dnl Process args given to configure
dnl

dnl ----------------------------------------------------------
dnl   enable-multibyte
dnl
GAUCHE_CHAR_ENCODING=utf8
AC_ARG_ENABLE(multibyte,
  AS_HELP_STRING([--enable-multibyte=ENCODING],
                 [Use ENCODING as the internal multibyte string encoding.
Following encodings are currently recognized:
'euc-jp' (EUC-JP code), 'utf-8' (UTF-8), 'sjis' (Shift-JIS),
or 'none' (No multibyte support)]),
  [
   AS_CASE([$enable_multibyte],
     [euc-jp|eucjp],   [GAUCHE_CHAR_ENCODING=eucjp],
     [utf-8|utf8|yes], [GAUCHE_CHAR_ENCODING=utf8],
     [sjis|shift-jis], [GAUCHE_CHAR_ENCODING=sjis],
     [no|none],        [GAUCHE_CHAR_ENCODING=none],
                       [AC_MSG_ERROR([unrecognized encoding option: '$enable_multibyte'; type ./configure --help for available options], 1)])
  ],
  [
    GAUCHE_CHAR_ENCODING=utf8
    ac_configure_args="$ac_configure_args '--enable-multibyte=utf-8'"
  ])

AS_CASE([$GAUCHE_CHAR_ENCODING],
  [eucjp], [AC_DEFINE(GAUCHE_CHAR_ENCODING_EUC_JP, [], [Define if Gauche handles multi-byte character as EUC-JP])],
  [utf8],  [AC_DEFINE(GAUCHE_CHAR_ENCODING_UTF_8,  [], [Define if Gauche handles multi-byte character as UTF-8])],
  [sjis],  [AC_DEFINE(GAUCHE_CHAR_ENCODING_SJIS,   [], [Define if Gauche handles multi-byte character as Shift JIS])])

dnl ----------------------------------------------------------
dnl  with-slib
dnl
SLIB_DIR=/usr/local/slib
AC_ARG_WITH(slib,
  AS_HELP_STRING([--with-slib=PATH],
                 [Configure Gauche's slib module to use Aubrey Jaffer's SLIB
installed under PATH.  If PATH is not specified, or this option is omitted,
Gauche still tries to find your slib installation from some typical places.]),
  [
    AS_CASE([$with_slib],
      [no],  [search_slib=no],
      [yes], [search_slib=yes],
	     [search_slib=no; SLIB_DIR=$with_slib])
  ], [ search_slib=yes ])

AS_IF([test $search_slib = "yes"], [
  AC_MSG_CHECKING(slib)
  slib_found=no
  for dir in /usr/share/slib /usr/local/slib /usr/local/lib/slib /usr/local/share/slib /usr/src/slib /opt/share/slib; do
    AS_IF([test -f $dir/require.scm], [
       SLIB_DIR=$dir
       AC_MSG_RESULT($SLIB_DIR)
       slib_found=yes
       break
    ])
  done
  AS_IF([test $slib_found = "no"], [
    AC_MSG_RESULT([not found, using fallback $SLIB_DIR])
  ])
])
AC_SUBST(SLIB_DIR)

dnl ----------------------------------------------------------
dnl  with-local=PATH:...
dnl
AC_ARG_WITH(local,
  AS_HELP_STRING([--with-local=PATH:PATH...],
                 [For each PATH, add PATH/include to the include search
paths and PATH/lib to the library search paths.  Useful if you have some
libraries installed in non-standard places.  On MinGW/MSYS, use ';' as a
separator instead of ':'.]),
  [
    case $with_local in
      yes|no|"") ;;  #no effect
      *) IFS_save=$IFS
         case $host in *mingw*) IFS=";" ;; *) IFS=":" ;; esac
         sep0=""
         sep1=""
         for p in $with_local; do
           LOCAL_INC="${LOCAL_INC}${sep0}-I$p/include"
           LOCAL_LIB="${LOCAL_LIB}${sep0}-L$p/lib"
           LOCAL_INCDIRS="${LOCAL_INCDIRS}${sep1}$p/include"
           LOCAL_LIBDIRS="${LOCAL_LIBDIRS}${sep1}$p/lib"
           sep0=" "
           sep1="$IFS"
         done
         IFS=$IFS_save ;;
    esac
  ])
AC_SUBST(LOCAL_INC)
AC_SUBST(LOCAL_LIB)
AC_SUBST(LOCAL_INCDIRS)
AC_SUBST(LOCAL_LIBDIRS)
dnl need to add here, for it may be used by the tests below.
INCLUDES="$INCLUDES $LOCAL_INC"
CPPFLAGS="$CPPFLAGS $LOCAL_INC"
LDFLAGS="$LDFLAGS $LOCAL_LIB"

dnl ----------------------------------------------------------
dnl   with-rpath=PATH:...
dnl
try_rpath=yes
rpath=
AC_ARG_WITH(rpath,
  AS_HELP_STRING([--with-rpath=PATH:PATH...],
                 [Use -rpath option while building dynamically loadable objects (experimental).]),
  [
    AS_CASE([$with_rpath],
      [yes], [],
      [no],  [try_rpath=no],
             [rpath=`echo $with_rpath | sed -e 's/^/-Wl,-rpath=/ ; s/:/,-rpath=/g'`])
  ])

dnl ----------------------------------------------------------
dnl   with-libatomic-ops[=yes|no|check]
dnl
dnl This code comes from "gc/configure.ac", and must be sync with that.
dnl NB: Don't forget to update path name from "libatomic_ops" to
dnl "gc/libatomic_ops".
dnl
# Do we want to use an external libatomic_ops?  By default use it if it's
# found.
AC_ARG_WITH([libatomic-ops],
    [AS_HELP_STRING([--with-libatomic-ops[=yes|no|check]],
                    [Use a external libatomic_ops? (default: check)])],
    [], [with_libatomic_ops=check])

# Check for an external libatomic_ops if the answer was yes or check.  If not
# found, fail on yes, and convert check to no.
# Note: "syntax error near unexpected token ATOMIC_OPS" reported by configure
# means Autotools pkg.m4 file was not found during aclocal.m4 generation.
missing_libatomic_ops=false
AS_IF([test x"$with_libatomic_ops" != xno],
  [ PKG_CHECK_MODULES([ATOMIC_OPS], [atomic_ops], [],
                      [ missing_libatomic_ops=true ]) ])
AS_IF([test x$missing_libatomic_ops = xtrue ],
      [ AS_IF([test x"$with_libatomic_ops" != xcheck],
              [ AC_MSG_ERROR([An external libatomic_ops was not found]) ])
        with_libatomic_ops=no ])

# If we have neither an external or an internal version, offer a useful hint
# and exit.
AS_IF([test x"$with_libatomic_ops" = xno -a ! -e "$srcdir/gc/libatomic_ops"],
  [ AC_MSG_ERROR([libatomic_ops is required.  You can either install it on
                  your system, or fetch and unpack a recent version into the
                  source directory and link or rename it to libatomic_ops.]) ])

# Finally, emit the definitions for bundled or external AO.
AC_MSG_CHECKING([which libatomic_ops to use])
AS_IF([test x"$with_libatomic_ops" != xno],
  [ AC_MSG_RESULT([external]) ],
  [ AC_MSG_RESULT([internal])
    ATOMIC_OPS_CFLAGS='-I$(top_builddir)/gc/libatomic_ops/src -I$(top_srcdir)/gc/libatomic_ops/src'
    ATOMIC_OPS_LIBS=""
    AC_SUBST([ATOMIC_OPS_CFLAGS])
    AC_CONFIG_SUBDIRS([libatomic_ops])
  ])
AM_CONDITIONAL([USE_INTERNAL_LIBATOMIC_OPS],
    [test x$with_libatomic_ops = xno -a x"$THREADS" != xnone])
AM_CONDITIONAL([NEED_ATOMIC_OPS_ASM],
    [test x$with_libatomic_ops = xno -a x$need_atomic_ops_asm = xtrue])


dnl ----------------------------------------------------------
dnl   enable-thread=TYPE
dnl
GAUCHE_THREAD_TYPE=default
AC_ARG_ENABLE(threads,
  AS_HELP_STRING([--enable-threads=TYPE],
                 [Choose thread type.  Possible values are: 'none' for not using threads, 'pthreads' to use pthreads, and 'default' to choose system's suitable one (if any). ]),
  [
    case $enableval in
      pthreads)
       GAUCHE_THREAD_TYPE=pthreads;;
      win32)
       GAUCHE_THREAD_TYPE=win32;;
      no|none)
       GAUCHE_THREAD_TYPE=none;;
      default)
       GAUCHE_THREAD_TYPE=default;;
      *)
       AC_MSG_ERROR([invalid value $enableval for --enable-threads option (must be either none, pthreads, win32, or default])
      ;;
    esac
  ], [])

dnl Platform-dependent thread configuration.  This must be in sync
dnl with gc's configure.in.
case $GAUCHE_THREAD_TYPE in
 pthreads|default)
  case "$host" in
    *-*-linux*)
      AC_DEFINE(GC_LINUX_THREADS,1,[Define to use Linux threads])
      AC_DEFINE(_REENTRANT,1,[Define to use reentrant libc])
      THREADLIBS="-lpthread"
      GAUCHE_THREAD_TYPE=pthreads
      ;;
    *-*-hpux*)
      AC_MSG_WARN("Only HP/UX 11 threads are supported.")
      AC_DEFINE(GC_HPUX_THREADS,1,[Define to use HP-UX threads])
      AC_DEFINE(_POSIX_C_SOURCE,199506L,[Define POSIX C version])
      THREADLIBS="-lpthread -lrt"
      GAUCHE_THREAD_TYPE=pthreads
      ;;
    *-*-*freebsd*|*-*-dragonfly*)
      AC_MSG_WARN("FreeBSD does not yet fully support threads with Boehm GC.")
      AC_DEFINE(GC_FREEBSD_THREADS,1,[Define to use FreeBSD threads])
      INCLUDES="$INCLUDES -pthread"
      THREADLIBS="-pthread"
      GAUCHE_THREAD_TYPE=pthreads
      ;;
    *-*-kfreebsd*-gnu)
      AC_DEFINE(GC_FREEBSD_THREADS,1,[Define to use FreeBSD threads])
      INCLUDES="$INCLUDES -pthread"
      THREADDLLIBS=-pthread
      AC_DEFINE(_REENTRANT,1,[Define to use reentrant libc])
      ;;
    *-*-gnu*)
      AC_DEFINE(GC_GNU_THREADS,1,[Define to use GNU threads])
      AC_DEFINE(_REENTRANT,1,[Define to use reentrant libc])
      THREADLIBS="-lpthread"
      GAUCHE_THREAD_TYPE=pthreads
      ;;
    *-*-netbsd*)
      AC_MSG_WARN("Only on NetBSD 2.0 or later.")
      AC_DEFINE(GC_NETBSD_THREADS,1,[Define to use NetBSD threads])
      AC_DEFINE(_REENTRANT, [], [Define to use reentrant libc])
      AC_DEFINE(_PTHREADS,  [], [Define if thread support requires this symbol])
      THREADLIBS="-lpthread"
      THREADDLLIBS="-lpthread -lrt"
      GAUCHE_THREAD_TYPE=pthreads
      ;;
    *-*-solaris*)
      AC_DEFINE(GC_SOLARIS_THREADS,1,[Define to use Solaris threads])
      AC_DEFINE(GC_SOLARIS_PTHREADS,1,[Define to use Solaris pthreads])
      AC_DEFINE(_POSIX_PTHREAD_SEMANTICS,1,[Define to use Solaris pthreads])
      THREADLIBS="-lpthread"
      GAUCHE_THREAD_TYPE=pthreads
      ;;
    *-*-irix*)
      AC_DEFINE(GC_IRIX_THREADS,1,[Define to use IRIX threads])
      THREADLIBS="-lpthread"
      GAUCHE_THREAD_TYPE=pthreads
      ;;
    *-*-cygwin*)
      AC_DEFINE(GC_WIN32_THREADS,1,[Define to use Win32 threads])
      THREADLIBS="-lpthread"
      GAUCHE_THREAD_TYPE=pthreads
      ;;
    *-*-darwin*)
      AC_DEFINE(GC_DARWIN_THREADS,1,[Define to use Darwin threads])
      AC_DEFINE(GC_PTHREADS,1,[Define to use pthreads])
      INCLUDES="$INCLUDES -I/sw/include"
      THREADLIBS="-lpthread"
      GAUCHE_THREAD_TYPE=pthreads
      ;;
    *-*-mingw*)
      if test $GAUCHE_THREAD_TYPE = "pthreads"; then
        AC_MSG_ERROR([pthread can't be used on Windows.  Use --enable-threads=win32 to enable threads])
      else
        GAUCHE_THREAD_TYPE=win32
      fi
      ;;
    *)
      if test $GAUCHE_THREAD_TYPE = "pthreads"; then
        AC_MSG_ERROR([pthread is not supported on $host])
      else
        GAUCHE_THREAD_TYPE=none
      fi
      ;;
  esac
 ;;
esac
case $GAUCHE_THREAD_TYPE in
  pthreads)
   AC_DEFINE(GAUCHE_USE_PTHREADS,1,[Define if we use pthreads])
   ;;
  win32)
   AC_DEFINE(GAUCHE_USE_WTHREADS,1,[Define if we use windows threads])
   ;;
  *)
   ;;
esac

LIBS="$LIBS $THREADLIBS"
dnl Save thread model to be inherited by gc/ subdir.
AC_SUBST(GAUCHE_THREAD_TYPE)

dnl ----------------------------------------------------------
dnl   enable-framework
dnl
ac_gauche_framework=no
AC_ARG_ENABLE(framework,
  AS_HELP_STRING([--enable-framework],
                 [On MacOSX, build Gauche as a framework.  This flag has no effect on other platforms.]),
  [
    case $enableval in
      no)
      ;;
      *)
      ac_gauche_framework=yes
      ;;
    esac
  ], [])

dnl ===========================================================
dnl Set up version-related macros
GAUCHE_VERSION=$PACKAGE_VERSION
AC_SUBST(GAUCHE_VERSION)
AC_DEFINE_UNQUOTED(GAUCHE_VERSION, "$GAUCHE_VERSION", [Gauche version string])

GAUCHE_VERSION_TRUNC=`echo $GAUCHE_VERSION | sed 's/@<:@-_@:>@.*//'`
OLD_IFS=$IFS; IFS="."; set $GAUCHE_VERSION_TRUNC; IFS=$OLD_IFS
GAUCHE_MAJOR_VERSION=$1
GAUCHE_MINOR_VERSION=$2
GAUCHE_MICRO_VERSION=$3
if test -z "$GAUCHE_MICRO_VERSION"; then GAUCHE_MICRO_VERSION=0; fi
AC_SUBST(GAUCHE_MAJOR_VERSION)
AC_SUBST(GAUCHE_MINOR_VERSION)
AC_SUBST(GAUCHE_MICRO_VERSION)
AC_DEFINE_UNQUOTED(GAUCHE_MAJOR_VERSION, $GAUCHE_MAJOR_VERSION,[Gauche major version number])
AC_DEFINE_UNQUOTED(GAUCHE_MINOR_VERSION, $GAUCHE_MINOR_VERSION,[Gauche minor version number])
AC_DEFINE_UNQUOTED(GAUCHE_MICRO_VERSION, $GAUCHE_MICRO_VERSION,[Gauche patch level number])

if test -z "$GAUCHE_ABI_VERSION"; then
  GAUCHE_ABI_VERSION="$GAUCHE_MAJOR_VERSION.$GAUCHE_MINOR_VERSION"
fi
AC_SUBST(GAUCHE_ABI_VERSION)
AC_DEFINE_UNQUOTED(GAUCHE_ABI_VERSION, "$GAUCHE_ABI_VERSION", [Gauche ABI version string])

GAUCHE_SIGNATURE="$GAUCHE_ABI_VERSION,$GAUCHE_CHAR_ENCODING,$GAUCHE_THREAD_TYPE"
AC_DEFINE_UNQUOTED(GAUCHE_SIGNATURE, "$GAUCHE_SIGNATURE", [Gauche signature string])

LIBGAUCHE=libgauche-$GAUCHE_ABI_VERSION
AC_SUBST(LIBGAUCHE)
LIBGAUCHE_STATIC=libgauche-static-$GAUCHE_ABI_VERSION
AC_SUBST(LIBGAUCHE_STATIC)
LINKGAUCHE=gauche-$GAUCHE_ABI_VERSION
AC_SUBST(LINKGAUCHE)

dnl put the args to the configure in gauche-config script for later use.
dnl the use of ac_configure_args depends on autoconf 2.52.
GAUCHE_CONFIGURE_ARGS=`echo "$ac_configure_args" | sed 's/@<:@\\"\`\$@:>@/\\\\&/g'`
AC_SUBST(GAUCHE_CONFIGURE_ARGS)

dnl ==========================================================
dnl Checks for programs.

dnl Kludge for Solaris.  We need this setup before start checking with
dnl the compiler.   NB: These flags do not coexist with -std=gnu99 flag.
dnl Be careful when we start using C99 features.
case $host in
  *-pc-solaris2.*)
    CFLAGS="$CFLAGS -D_XOPEN_SOURCE -D_XOPEN_SOURCE_EXTENDED=1 -D__EXTENSIONS__" ;;
esac

AC_PROG_CC
AC_PROG_CC_STDC
AC_C_CONST
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S
AC_CHECK_PROGS(AR, ar gar)
AC_CHECK_PROGS(AS, as gas)
AC_CHECK_PROGS(MAKEINFO, makeinfo)
AC_CHECK_PROGS(GZIP_PROGRAM, gzip)

dnl for cross build
AC_CHECK_PROGS(BUILD_CC, ${build}-gcc ${build_alias}-gcc gcc cc)

dnl for build from repo, we need preinstalled gosh
dnl NB: The third arg is the value when we don't detect preinstalled gosh.
dnl In such case, building from repo fails anyway; shall we use alternative
dnl name or something to tell the builder that she needs preinstalled gosh?
AC_PATH_PROG(BUILD_GOSH, gosh, gosh)

dnl Safe default CFLAGS (usually -g -O2 if you're using gcc, empty otherwise).
dnl If the user overrides CFLAGS during configure, which is recorded.
dnl This information is used when configuring extensions.
DEFAULT_CFLAGS="$CFLAGS"
AC_SUBST(DEFAULT_CFLAGS)

dnl ===========================================================
dnl Checks for header files.
dnl   We check sys/types.h merely to include it in the following tests,
dnl   for some tests needs it.
AC_HEADER_STDC
AC_HEADER_TIME
AC_CHECK_HEADERS(time.h sys/time.h sys/types.h glob.h dlfcn.h getopt.h sched.h)
AC_CHECK_HEADERS(unistd.h stdint.h inttypes.h rpc/types.h malloc.h)
AC_CHECK_HEADERS(syslog.h crypt.h)
AC_CHECK_HEADERS(pty.h util.h bsd/libutil.h libutil.h sys/loadavg.h sys/resource.h)

dnl glibc specific
AC_CHECK_HEADERS(fpu_control.h)

dnl solaris specific
AC_CHECK_HEADERS(sunmath.h)

dnl OSX specific.  If this header exists, we assume that extern char **environ
dnl isn't available and we should use _NSGetEnviron().
AC_CHECK_HEADERS(crt_externs.h)

dnl ===========================================================
dnl Checks processor type, for processor-specific stuff
AS_CASE([$host],
  [i?86-apple-darwin*], [
    AC_CHECK_DEFINED(i386,
      AC_DEFINE(SCM_TARGET_I386,1,[Define to use i386 optimizations]))],
  [i?86-*], [
    AC_DEFINE(SCM_TARGET_I386,1,[Define to use i386 optimizations])],
  [x86_64-*], [
    AC_DEFINE(SCM_TARGET_X86_64,1,[Define to use x86_64 optimizations])],
  [alpha*], [
    CFLAGS="$CFLAGS -mieee"],
  [arm*], [
    dnl ARM processor may use a special mixed endian for doubles.  It can
    dnl be switched by processor configuration.  So we check it at runtime.
    AC_DEFINE(DOUBLE_ARMENDIAN,1,[Define to use mixed endian ARM processor])])

dnl ===========================================================
dnl Checks for typedefs, structures, and compiler characteristics.
AC_SYS_LARGEFILE
AC_C_INLINE
AC_C_BIGENDIAN
AC_STRUCT_TM
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(off_t)
AC_CHECK_TYPES([uint16_t, int32_t, uint32_t, int64_t, uint64_t, long long, long double])
AC_CHECK_TYPES([struct timespec])

if test "$GAUCHE_THREAD_TYPE" = pthreads; then
  AC_CHECK_TYPES(pthread_spinlock_t,,,[#include <pthread.h>])
fi

AC_CHECK_SIZEOF(rlim_t,,[
#if HAVE_SYS_TIME_H
# include <sys/time.h>
#endif
#if HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif
])

dnl Checks non-POSIX members of system structure
AC_CHECK_MEMBERS([struct group.gr_passwd],,,[#include <grp.h>])
AC_CHECK_MEMBERS([struct passwd.pw_passwd,
                  struct passwd.pw_gecos,
                  struct passwd.pw_class],,,[#include <pwd.h>])

dnl checks if time_t is integer or flonum
AC_CACHE_CHECK(time_t is integral, ac_cv_type_time_t_integral, [
AC_TRY_RUN([
#include <time.h>
int main()
{
   time_t t = 3.14;
   return (t == 3.14);
}], ac_cv_type_time_t_integral=yes, ac_cv_type_time_t_integral=no,
ac_cv_type_time_t_integral=yes)])

if test "$ac_cv_type_time_t_integral" = yes; then
AC_DEFINE(INTEGRAL_TIME_T,1,[Define if time_t is typedef'ed to an integral type])
fi

dnl ===========================================================
dnl Checks for library functions.
AC_PROG_GCC_TRADITIONAL

dnl This is required to test trunc and rint below
AC_CHECK_LIB(m, sin)

dnl x86-solaris 9 requires this to use isinf
AC_CHECK_LIB(sunmath, isinf)

dnl clock_gettime may be in librt
AC_CHECK_LIB(rt, clock_gettime)

dnl For Windows/MinGW, manually adds several libraries
dnl Also adds -DUNICODE to CFLAGS enable Windows wchar API,
dnl  if GAUCHE_CHAR_ENCODING is UTF_8.
dnl ALTERNATIVE_GOSH is no-console version of gosh; only built on Windows.
case "$host" in
  *mingw*) LIBS="$LIBS -lnetapi32 -lshlwapi -lws2_32"
           ALTERNATIVE_GOSH="gosh-noconsole.exe"
           case "$GAUCHE_CHAR_ENCODING" in
             utf8) CFLAGS="$CFLAGS -DUNICODE" ;;
           esac ;;
  *)       ALTERNATIVE_GOSH="" ;;
esac
AC_SUBST(ALTERNATIVE_GOSH)

AC_FUNC_ALLOCA
AC_CHECK_FUNCS(isnan isinf trunc rint tgamma lgamma)
AC_CHECK_FUNCS(symlink readlink lchown realpath nanosleep usleep)
AC_CHECK_FUNCS(random srandom lrand48 srand48)
AC_CHECK_FUNCS(putenv setenv unsetenv clearenv getpgid setgroups)
AC_CHECK_FUNCS(gethostname sethostname getdomainname setdomainname)
AC_CHECK_FUNCS(gettimeofday getloadavg clock_gettime clock_getres)
AC_CHECK_FUNCS(syslog setlogmask)
AC_CHECK_FUNCS(sigwait)
AC_CHECK_FUNCS(fpsetprec)

dnl KLUDGE: As of Dec 2015, Mingw-w64  provides mkstemp() but it opens
dnl the file with _O_TEMPORARY flag, so the file gets automatically deleted
dnl when its opened file descriptors are all closed.  It is incompatible
dnl for typical behavior and we avoid using it.  This kludge should be
dnl removed once mingw-w64 fixes mkstemp.  (We include mkdtemp here as
dnl well, just because it's nice to see them togeter.  Mingw-w64 doesn't
dnl have mkdtemp yet, so it doesn't matter wheter we include it or not.)
case "$host" in
  *mingw*) ;;
  *)
   AC_CHECK_FUNCS(mkdtemp mkstemp);;
esac

dnl Check for select().  HP-UX and MinGW doesn't like the way configure tests
dnl select() existence and we know they have one, so we skip the test on them.
case "$host" in
  *-hpux*|*mingw*)
    AC_DEFINE(HAVE_SELECT, 1, [Define if you have select]) ;;
  *)
    AC_CHECK_FUNCS(select);;
esac

dnl Checks for pty-related fns.  It appears that recent Cygwin has them,
dnl but only in a static library.  That prevents us from creating DLL
dnl version of gauche.  Thus we explictly exclude them on cygwin.
case "$host" in
  *cygwin*)
    : ;;
  *)
    AC_SEARCH_LIBS(openpty, util, AC_DEFINE(HAVE_OPENPTY, 1, [Define if you have openpty]))
    AC_SEARCH_LIBS(forkpty, util, AC_DEFINE(HAVE_FORKPTY, 1, [Define if you have forkpty]))
    ;;
esac

dnl Checks if crypt() exists and whether it's in libc or libcrypt.
dnl Note: on cygwin, libcrypt is only available as a static library,
dnl and prevents libgauche.dll from building.   We explicitly excludes it.
dnl
case "$host" in
  *cygwin*)
     : ;;
  *)
     AC_SEARCH_LIBS(crypt, crypt, AC_DEFINE(HAVE_CRYPT,1,[Define if uses libcrypt]))
     ;;
esac

dnl Checks if dlopen exists, and if it's in libc or libdl.
AC_SEARCH_LIBS(dlopen, dl, AC_DEFINE(HAVE_DLOPEN,1,[Define if the system has dlopen()]))

dnl Checks for sched_yield.
AC_SEARCH_LIBS(sched_yield, rt, AC_DEFINE(HAVE_SCHED_YIELD,1,[Define if uses librt]))

dnl
dnl Checks compiler options for dynamic link and thread support.
dnl
dnl  SHLIB_SO_SUFFIX     Suffix of dlopen-able module.  most systems doesn't
dnl                      have restriction for this.  default "so".
dnl  SHLIB_DYLIB_SUFFIX  Suffix of dynamic linkable module.  On Unices
dnl                      it is usuallly "so".  On MacOSX it is "dylib".
dnl                      On Win32 it is "dll".   If not explicitly specified,
dnl                      SHLIB_SO_SUFFIX is used.
dnl  SHLIB_SO_CFLAGS     flags to compile files which is dlopen-able
dnl                      e.g. "-fpic"
dnl  SHLIB_SO_LDFLAGS    flags to link files to create dlopen-able
dnl                      module, e.g. "-shared -o"
dnl  SHLIB_DYLIB_LDFLAGS flags to link files to create dynamically linkable
dnl                      module.   On most unices this is the same as
dnl                      SHLIB_SO_LDFLAGS.
dnl  SHLIB_MAIN_LDFLAGS  flags required to compile a main program which
dnl                      exposes symbols to be referred by the dynamically
dnl                      loaded module, e.g. "-rdynamic"
dnl  LINK_HELPER         A script that helps linking libgauche.  Sort of
dnl                      simplified libtool.  So far we only need this on
dnl                      MacOSX.  We have absolutely no intention to
dnl                      re-invent libtool, though.
dnl  LIBGAUCHE_SO        defined as "libgauche.$(SHLIB_SO_SUFFIX)" if
dnl                      system supports dso.  empty otherwise.
dnl  GOSH_USE_SHLIB      If set to "yes", gosh uses dynamic linkable version
dnl                      of libgauche, instead of statically links libgauche.a.
dnl                      On Win32 it is required to allow extensions to work.

AC_MSG_CHECKING(how to make dynamic loadable module)

# A flag to indicate libgauche can be linked when building extension module.
# It is required on cygwin; it has to be omitted on MacOS X; Other unices
# generally don't care.
ext_use_libgauche=no

# A flag to indicate we need to create a link with version numbers to
# libgauche.so after building it.
libgauche_version_link=yes

# A flag to indicate uvector.so should be added when linking extension module
# that refers to uvector.  It is required if we want to resolve all symbols,
# and although it's the usual case, we can't add it on MacOS X, on which
# we only build the static version of libgauche.
ext_use_uvector=no

# We put additional information, such as whether we use Framework for
# darwin, in this variable.
xhost=$host

# On cygwin, we likely to need rebaseall to make dlls work with sys-fork.
CYGWIN_FIXDLL=:

case $host in
  *freebsd2*)
    SHLIB_SO_CFLAGS="-fpic -fPIC"
    SHLIB_SO_LDFLAGS="-v;ld -Bshareable -o"
    SHLIB_SO_SUFFIX="so"
    SHLIB_MAIN_LDFLAGS=""
    SHLIB_OK=ok
    ;;
  *-linux-gnu*|*-*-gnu*|*freebsd*|*dragonfly*)
    SHLIB_SO_CFLAGS="-fPIC"
    SHLIB_SO_LDFLAGS="$rpath -shared -o"
    SHLIB_SO_SUFFIX="so"
    SHLIB_MAIN_LDFLAGS="-rdynamic"
    SHLIB_OK=ok
    ;;
  *netbsd*|*openbsd*)
    SHLIB_SO_CFLAGS="-fPIC -DPIC"
    SHLIB_SO_LDFLAGS="$rpath -shared -o"
    SHLIB_SO_SUFFIX="so"
    SHLIB_MAIN_LDFLAGS="-rdynamic"
    SHLIB_OK=ok
    ;;
  *darwin*)
    # -no-cpp-precomp is not related to shared library, but needed to
    # get src/{vm.c,char.c} compiled -skimu
    # [Shiro] Darwin 1.3 and later needs different flags
    CPPFLAGS="$CPPFLAGS -no-cpp-precomp"
    case $host_os in
      darwin1.@<:@012@:>@) FLAT_NAMESPACE="" ;;
      *)               FLAT_NAMESPACE="-flat_namespace" ;;
    esac
    case $host_os in
      darwin9.8.*) ;;           dnl Leopard doesn't like -no_pie
      *)           SHLIB_MAIN_LDFLAGS="-Wl,-no_pie";;
    esac
    SHLIB_SO_CFLAGS="-no-cpp-precomp -fPIC -fno-common"
    SHLIB_SO_LDFLAGS="-bundle $FLAT_NAMESPACE -undefined suppress -o"
    SHLIB_SO_SUFFIX="so"
    SHLIB_DYLIB_LDFLAGS='-framework CoreFoundation -dynamiclib -o'
    if test "$ac_gauche_framework" = yes; then
      AC_DEFINE(GAUCHE_MACOSX_FRAMEWORK, 1, [Define 1 if building framework on MacOSX])
      xhost="${host}-framework"
    fi
    SHLIB_DYLIB_SUFFIX="dylib"
    SHLIB_LIBS_FOR_EXT=""
    SHLIB_OK=ok
    LINK_HELPER='$(srcdir)/link-dylib'
    ext_use_libgauche=no
    libgauche_version_link=no
    ;;
  mips-sgi-irix*)
    if test "$GCC" = yes; then
      SHLIB_SO_CFLAGS="-fPIC"
    else
      SHLIB_SO_CFLAGS="-KPIC"
    fi
    SHLIB_SO_LDFLAGS="$rpath -shared -o"
    SHLIB_SO_SUFFIX="so"
    SHLIB_MAIN_LDFLAGS=""
    SHLIB_OK=ok
    ;;
  *solaris*)
    if test "$GCC" = yes; then
      SHLIB_SO_CFLAGS="-fPIC"
      SHLIB_SO_LDFLAGS="-shared -o"
    else
      SHLIB_SO_CFLAGS="-Kpic"
      SHLIB_SO_LDFLAGS="-G -o"
    fi
    SHLIB_SO_SUFFIX="so"
    SHLIB_MAIN_LDFLAGS=""
    SHLIB_OK=ok
    ;;
  *hp*)
    if test "$GCC" = yes; then
      SHLIB_SO_CFLAGS="-fPIC"
      SHLIB_SO_LDFLAGS="-shared -o"
    else
      SHLIB_SO_CFLAGS="+z +Z"
      SHLIB_SO_LDFLAGS=";ld -b -o"
    fi
    SHLIB_SO_SUFFIX="sl"
    SHLIB_MAIN_LDFLAGS=""
    SHLIB_OK=ok
    LINK_HELPER='$(srcdir)/link-hpux'
    ;;
  *cygwin*)
    SHLIB_SO_CFLAGS=""
    SHLIB_SO_LDFLAGS='-Wl,--export-all-symbols -Wl,--enable-auto-import -shared -o'
    SHLIB_SO_SUFFIX="dll"
    SHLIB_DYLIB_SUFFIX="dll"
    SHLIB_MAIN_LDFLAGS="-Wl,--enable-auto-import"
    SHLIB_OK=ok
    GOSH_USE_SHLIB=yes
    ext_use_uvector=yes
    ext_use_libgauche=yes
    libgauche_version_link=no
    CYGWIN_FIXDLL='$(srcdir)/src/cygwin-fixdll.sh'
    ;;
  *mingw*)
    # Kludge: We add -static-libgcc flag when creating dll, in order to avoid
    # introducing dependency to libgcc_s_dw2-1.dll.
    SHLIB_SO_CFLAGS=""
    SHLIB_SO_LDFLAGS='-Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-runtime-pseudo-reloc -static-libgcc -shared -o'
    SHLIB_SO_SUFFIX="dll"
    SHLIB_DYLIB_SUFFIX="dll"
    SHLIB_MAIN_LDFLAGS="-Wl,--enable-auto-import -static-libgcc"
    SHLIB_OK=ok
    GOSH_USE_SHLIB=yes
    ext_use_uvector=yes
    ext_use_libgauche=yes
    libgauche_version_link=no
    ;;
  *)
    SHLIB_SO_CFLAGS=""
    SHLIB_SO_LDFLAGS=""
    SHLIB_SO_SUFFIX=""
    SHLIB_MAIN_LDFLAGS=""
    SHLIB_OK=unknown
    ;;
esac
if test -z "$SHLIB_DYLIB_LDFLAGS"; then
  SHLIB_DYLIB_LDFLAGS=$SHLIB_SO_LDFLAGS
fi
if test -z "$SHLIB_DYLIB_SUFFIX"; then
  SHLIB_DYLIB_SUFFIX=$SHLIB_SO_SUFFIX
fi
if test "$SHLIB_OK" = ok; then
  LIBGAUCHE_SO="$LIBGAUCHE.$SHLIB_DYLIB_SUFFIX"
else
  LIBGAUCHE_SO=
fi
if test "$ext_use_libgauche" = yes; then
  EXT_LIBGAUCHE="-l$LINKGAUCHE"
else
  EXT_LIBGAUCHE=
fi
if test "$ext_use_uvector" = yes; then
  EXT_UVECTOR='$(top_builddir)/ext/uvector/libgauche-uvector.$(SOEXT)'
else
  EXT_UVECTOR=
fi
if test "$libgauche_version_link" = yes -a "$SHLIB_OK" = ok; then
  MAKEVERSLINK='$(SHELL) ./makeverslink'
else
  MAKEVERSLINK=:
fi

AC_SUBST(xhost)
AC_SUBST(SHLIB_SO_CFLAGS)
AC_SUBST(SHLIB_SO_LDFLAGS)
AC_SUBST(SHLIB_SO_SUFFIX)
AC_SUBST(SHLIB_LIBS_FOR_EXT)
AC_SUBST(SHLIB_DYLIB_LDFLAGS)
AC_SUBST(SHLIB_DYLIB_SUFFIX)
AC_SUBST(SHLIB_MAIN_LDFLAGS)
AC_SUBST(EXT_LIBGAUCHE)
AC_SUBST(EXT_UVECTOR)
AC_SUBST(LIBGAUCHE_SO)
AC_SUBST(GOSH_USE_SHLIB)
AC_DEFINE_UNQUOTED(SHLIB_SO_SUFFIX, "$SHLIB_SO_SUFFIX", [Shared library file suffix])
AC_SUBST(LINK_HELPER)
AC_SUBST(MAKEVERSLINK)
AC_SUBST(CYGWIN_FIXDLL)
AC_MSG_RESULT($SHLIB_OK)

dnl Check to see if the linker takes --rpath flag via -Wl.
RPATH_FLAG=
if test "$try_rpath" = "yes"; then
  cflags_save="$CFLAGS"
  CFLAGS="$CFLAGS -Wl,--rpath=/"
  AC_CACHE_CHECK([linker takes --rpath flag], ac_cv_linker_rpath_flag, [
    AC_LINK_IFELSE([AC_LANG_SOURCE([int main(void) {return 0;}])],
                   ac_cv_linker_rpath_flag=yes,
                   ac_cv_linker_rpath_flag=no)
    ])
  CFLAGS="$cflags_save"
  if test "$ac_cv_linker_rpath_flag" = "yes"; then
    RPATH_FLAG='-Wl,--rpath='
  else
    # solaris' ld uses -R instead of --rpath
    CFLAGS="$CFLAGS -Wl,-R,/"
    AC_LINK_IFELSE([AC_LANG_SOURCE([int main(void) {return 0;}])],
                   ac_cv_linker_rpath_flag=yes,
                   ac_cv_linker_rpath_flag=no)
    CFLAGS="$cflags_save"
    if test "$ac_cv_linker_rpath_flag" = "yes"; then
      RPATH_FLAG='-Wl,-R,'
    fi
  fi
  if test "$RPATH_FLAG" != ""; then
    RPATH_TMP=$RPATH_FLAG'`pwd`'
    RPATH_REAL=$RPATH_FLAG'$(LIB_INSTALL_DIR)'
  fi
fi
AC_SUBST(RPATH_FLAG)
AC_SUBST(RPATH_TMP)
AC_SUBST(RPATH_REAL)

dnl Check to see if the linker takes --soname flag
cflags_save="$CFLAGS"
CFLAGS="$CFLAGS -Wl,--soname,libgauche.so.0"
AC_CACHE_CHECK([linker takes --soname flag], ac_cv_linker_soname_flag, [
  AC_LINK_IFELSE([AC_LANG_SOURCE([int main(void) {return 0;}])],
                 ac_cv_linker_soname_flag=yes,
                 ac_cv_linker_soname_flag=no)
  ])
CFLAGS="$cflags_save"
if test "$ac_cv_linker_soname_flag" = "yes"; then
  SONAME_FLAG="-Wl,--soname,$LIBGAUCHE.so.0.$GAUCHE_MICRO_VERSION"
fi
AC_SUBST(SONAME_FLAG)

dnl Sets some optimization flags by heuristics.
AC_GAUCHE_OPTFLAGS

dnl ===========================================================
dnl Tests for extensions

EXT_LIBS=
# Those *.ac's may add libraries to EXT_LIBS.
m4_include([ext/charconv/charconv.ac])
m4_include([ext/dbm/dbm.ac])
m4_include([ext/net/net.ac])
m4_include([ext/zlib/zlib.ac])
m4_include([ext/tls/tls.ac])

dnl Setup STATIC_LIBS
STATIC_LIBS=
for lib in $EXT_LIBS $LIBS; do
  if [ echo "$STATIC_LIBS" | grep -e $lib > /dev/null 2>&1 ]
  then
    : # lib is alreay in STATIC_LIBS.  do nothing
  else
    STATIC_LIBS="$STATIC_LIBS $lib"
  fi
done
STATIC_LIBS="`echo $LIBGAUCHE_STATIC | sed s/^lib/-l/` $STATIC_LIBS"
AC_SUBST(STATIC_LIBS)

dnl ===========================================================
dnl Cross compiling
if test ${cross_compiling} = no ; then
   # self compile
   CROSS_COMPILING_yes="@%:@"
   CROSS_COMPILING_no=
else
   # cross compile
   CROSS_COMPILING_yes=
   CROSS_COMPILING_no="@%:@"
fi
AC_SUBST(CROSS_COMPILING_yes)
AC_SUBST(CROSS_COMPILING_no)

dnl ===========================================================
dnl Configure gc
AC_CONFIG_SUBDIRS(gc)

dnl ===========================================================
dnl Create output files
AC_CONFIG_COMMANDS([VERSION],
	[echo "${GAUCHE_VERSION}" > VERSION],
	[GAUCHE_VERSION="${GAUCHE_VERSION}"])
AC_CONFIG_FILES([
          Makefile
          src/Makefile src/genconfig src/makeverslink
          lib/Makefile lib/slib.scm
          doc/gosh.1 doc/gauche-config.1 doc/gauche-cesconv.1
          doc/gauche-install.1 doc/gauche-package.1 doc/Makefile
          ext/Makefile ext/Makefile.ext
          ext/bcrypt/Makefile
          ext/binary/Makefile
          ext/charconv/Makefile ext/charconv/charconv.h
          ext/data/Makefile
          ext/dbm/Makefile
          ext/digest/Makefile
          ext/fcntl/Makefile
          ext/file/Makefile
          ext/gauche/Makefile
          ext/mt-random/Makefile
          ext/net/Makefile
          ext/peg/Makefile
          ext/sparse/Makefile
          ext/srfi/Makefile
          ext/sxml/Makefile
          ext/syslog/Makefile
          ext/tls/Makefile
          ext/termios/Makefile
          ext/text/Makefile
          ext/threads/Makefile
          ext/util/Makefile
          ext/uvector/Makefile
          ext/vport/Makefile
          ext/rfc/Makefile
          ext/zlib/Makefile
          ext/windows/Makefile
          examples/Makefile
          examples/standalone/Makefile
          gc/configure.gnu-gauche:tools/gc-configure.gnu-gauche.in
         ])
AC_OUTPUT

dnl ===========================================================
dnl Show some of the configuration results, for developer's
dnl convenience.

[OPTDBMS=`echo "$DBM_SCMFILES" | sed 's/\.sci//g'`]
if test "$ac_cv_use_zlib" = yes; then OPTZLIB=" zlib"; else OPTZLIB=" "; fi

AC_MSG_RESULT(
[
This Gauche has been configured with the following parameters:
           version: $GAUCHE_VERSION
         multibyte: $GAUCHE_CHAR_ENCODING
              slib: $SLIB_DIR
            thread: $GAUCHE_THREAD_TYPE
           tls/ssl: $GAUCHE_TLS_TYPE
  optional modules: $OPTDBMS$OPTZLIB
])
