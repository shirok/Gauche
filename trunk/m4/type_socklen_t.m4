dnl [SK] this is taken from the Autoconf Macro Archive, http://ac-archive.sourceforge.net/
dnl It is licenced under GPL2 with the exception same as other autoconf-related
dnl scripts.  I stripped '$' around Id signature to prevent my checking in
dnl alters it.

dnl @synopsis TYPE_SOCKLEN_T
dnl
dnl Check whether sys/socket.h defines type socklen_t. Please note
dnl that some systems require sys/types.h to be included before
dnl sys/socket.h can be compiled.
dnl
dnl @version Id: type_socklen_t.m4,v 1.2 2005/01/11 04:18:04 guidod Exp 
dnl @author Lars Brinkhoff <lars@nocrew.org>
dnl
AC_DEFUN([TYPE_SOCKLEN_T],
[AC_CACHE_CHECK([for socklen_t], ac_cv_type_socklen_t,
[
  AC_TRY_COMPILE(
  [#include <sys/types.h>
   #include <sys/socket.h>],
  [socklen_t len = 42; return 0;],
  ac_cv_type_socklen_t=yes,
  ac_cv_type_socklen_t=no)
])
  if test $ac_cv_type_socklen_t != yes; then
    AC_DEFINE(socklen_t, int, [Substitute for socklen_t])
  fi
])
