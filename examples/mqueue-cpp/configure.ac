dnl
dnl Configuring mqueue-cpp
dnl  process this file with autoconf to generate 'configure'.
dnl

AC_PREREQ(2.54)
AC_INIT(mqueue-cpp, 1.0, shiro@acm.org)
dnl If you want to use the system name (OS, architecture, etc) in the
dnl configure, uncomment the following line.  In such a case, you need
dnl to copy config.guess and config.sub from automake distribution.
dnl AC_CANONICAL_SYSTEM

dnl Set up gauche related commands.  The commands are set by scanning
dnl PATH.  You can override them by "GOSH=/my/gosh ./configure" etc.
AC_PATH_PROG([GOSH], gosh)
AC_PATH_PROG([GAUCHE_CONFIG], gauche-config)
AC_PATH_PROG([GAUCHE_PACKAGE], gauche-package)
AC_PATH_PROG([GAUCHE_INSTALL], gauche-install)
AC_PATH_PROG([GAUCHE_CESCONV], gauche-cesconv)

dnl We need the name of C++ compiler.  Actually the C++ compile must
dnl be compatible with the C compiler used to build Gauche, but I
dnl don't know how to guarantee that; here we just assume system's
dnl C++ compiler is compatible with the cc used for Gauche.
AC_PROG_CXX

dnl Usually these parameters are set by AC_PROG_CC, but we'd rather use
dnl the same one as Gauche has been compiled with.
SOEXT=`"$GAUCHE_CONFIG" --so-suffix`
OBJEXT=`"$GAUCHE_CONFIG" --object-suffix`
EXEEXT=`"$GAUCHE_CONFIG" --executable-suffix`
AC_SUBST(SOEXT)
AC_SUBST(OBJEXT)
AC_SUBST(EXEEXT)

dnl Check for headers.
dnl Add your macro calls to check required headers, if you have any.

dnl Check for other programs.
dnl Add your macro calls to check existence of programs, if you have any.

dnl Check for libraries
dnl Add your macro calls to check required libraries, if you have any.

dnl Creating gpd (gauche package description) file
GAUCHE_PACKAGE_CONFIGURE_ARGS=`echo "$ac_configure_args" | sed 's/@<:@\\"\`\$@:>@/\\\\&/g'`
AC_MSG_NOTICE([creating ${PACKAGE_NAME}.gpd])
"$GAUCHE_PACKAGE" make-gpd "$PACKAGE_NAME" \
  -version "$PACKAGE_VERSION" \
  -configure "./configure $GAUCHE_PACKAGE_CONFIGURE_ARGS"

dnl Output
echo $PACKAGE_VERSION > VERSION
AC_OUTPUT(Makefile)

