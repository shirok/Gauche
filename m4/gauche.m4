dnl -*- mode: autoconf -*-
dnl Gauche-specific autoconf macros.
dnl
dnl  NB: As of 0.8.6, use of Gauche-specific macros is deprecated.
dnl      The 'gauche-package' program can generate template configure.ac
dnl      that doesn't have any Gauche-specific macros.  We still keep
dnl      this for backward compatibility, but will remove it after some time.

#serial 1

dnl AC_GAUCHE_INIT_EXT
dnl   Sets some parameters about installed Gauche package. 
dnl
dnl    GOSH            - Path of gosh executable
dnl    GAUCHE_CONFIG   - Path of gauche-config program
dnl    GAUCHE_PACKAGE  - Path of gauche-package program
dnl    GAUCHE_INSTALL  - Path of gauche-install program
dnl    GAUCHE_CESCONV  - Path of gauche-cesconv program
dnl    GAUCHE_INC      - '-I' macros required to compile extensions.
dnl    GAUCHE_LIB      - '-L' macros required to link extensions.
dnl    GAUCHE_VERSION  - The version of Gauche.
AC_DEFUN([AC_GAUCHE_INIT_EXT],
         [
AC_PATH_PROG([GOSH], gosh)
AC_PATH_PROG([GAUCHE_CONFIG], gauche-config)
AC_PATH_PROG([GAUCHE_PACKAGE], gauche-package)
AC_PATH_PROG([GAUCHE_INSTALL], gauche-install)
AC_PATH_PROG([GAUCHE_CESCONV], gauche-cesconv)
GAUCHE_TOP=
GAUCHE_INC="`gauche-config -I`"
GAUCHE_LIB="`gauche-config -L`"
AC_SUBST(GAUCHE_INC)
AC_SUBST(GAUCHE_LIB)
GAUCHE_VERSION=`$GAUCHE_CONFIG -V`
AC_SUBST(GAUCHE_VERSION)
AC_DEFINE_UNQUOTED(GAUCHE_VERSION, "$GAUCHE_VERSION", [Gauche version string])
])

dnl AC_GAUCHE_INSTALL_TYPE(TYPE)
dnl   Sets the default value of INSTALL_TYPE macro.  TYPE must be either
dnl   sys or site.
AC_DEFUN([AC_GAUCHE_INSTALL_TYPE],
         [
: ${INSTALL_TYPE=$1}
AS_IF([test "X$INSTALL_TYPE" != "Xsys" -a "X$INSTALL_TYPE" != "Xsite"],
	[AC_MSG_ERROR([INSTALL_TYPE must be either 'sys' or 'site'])])
AC_SUBST(INSTALL_TYPE)
])

dnl AC_GAUCHE_CC
dnl   Gets compiler parameters which Gauche has been compiled with.
AC_DEFUN([AC_GAUCHE_CC],
         [
CC="`$GAUCHE_CONFIG --cc`"
AC_SUBST(CC)
# adds default CFLAGS if it has not been set.
ac_gauche_CFLAGS=${CFLAGS+set}
AS_IF([test -z "$ac_gauche_CFLAGS"],
	[CFLAGS="`$GAUCHE_CONFIG --default-cflags`"])
# adds default OBJEXT if it has not been set.
ac_gauche_OBJEXT=${OBJEXT+set}
AS_IF([test -z "$ac_gauche_OBJEXT"],
	[OBJEXT="`$GAUCHE_CONFIG --object-suffix`"])
ac_gauche_EXEEXT=${EXEEXT+set}
AS_IF([test -z "$ac_gauche_EXEEXT"],
	[EXEEXT="`$GAUCHE_CONFIG --executable-suffix`"])
ac_gauche_SOEXT=${SOEXT+set}
AS_IF([test -z "$ac_gauche_SOEXT"],
	[SOEXT="`$GAUCHE_CONFIG --so-suffix`"])
ac_gauche_DYLIBEXT=${DYLIBEXT+set}
AS_IF([test -z "$ac_gauche_DYLIBEXT"],
	[DYLIBEXT="`$GAUCHE_CONFIG --dylib-suffix`"])
AC_SUBST(OBJEXT)
AC_SUBST(EXEEXT)
AC_SUBST(SOEXT)
AC_SUBST(DYLIBEXT)
])

dnl AC_GAUCHE_FLAGS
dnl   Sets CFLAGS, CPPFLAGS and LDFLAGS appropriate for furthre testing.
dnl   This should come before any testings that require those flags to be set.
AC_DEFUN([AC_GAUCHE_FLAGS],
         [
CFLAGS="$CFLAGS $GAUCHE_INC"
AC_SUBST(CFLAGS)
CPPFLAGS="$CPPFLAGS $GAUCHE_INC"       # some test requires this
LDFLAGS="$LDFLAGS `$GAUCHE_CONFIG --local-libdir`"
AC_GAUCHE_OPTFLAGS
])

dnl AC_GAUCHE_OPTFLAGS
dnl   Sets OPTFLAGS with some optimization flags using heuristics.
dnl   If you use AC_GAUCHE_FLAGS, this test is included.
dnl   The main configure and gc's configure also use this.
AC_DEFUN([AC_GAUCHE_OPTFLAGS],
         [
AC_ARG_ENABLE([heuristic-optimization],
	AS_HELP_STRING([--enable-heuristic-optimization],
		[Sets some architecture specific optimization flags using heuristics (default: enable)]),
	[USE_HEURISTIC_OPTIMIZATION=$enableval],
	[USE_HEURISTIC_OPTIMIZATION=yes])
AS_IF([test "x$USE_HEURISTIC_OPTIMIZATION" = xyes],[
  AS_CASE(["$target"],
	  [i686-*], [I686OPT="-DUSE_I686_PREFETCH"])
  AS_CASE(["$CC"],
	  [gcc*], [ dnl some systems may have gcc-2.95, gcc-3, etc.
		  AS_CASE(["$target"],
			  [*mingw*], [],
				     [GCCOPT="-fomit-frame-pointer"])
		  AS_CASE(["$target"],
			  [i586-*], [GCCOPT="$GCCOPT -march=i586"],
			  [i686-*], [GCCOPT="$GCCOPT -march=i686"])])])
OPTFLAGS="$GCCOPT $I686OPT"
AC_SUBST(OPTFLAGS)
])

dnl AC_GAUCHE_FIX_LIBS
dnl   Sets LDFLAGS and LIBS to generate shared library.
dnl   This has to come after all the tests that require linking, or those test
dnl   will fail because they can't generate stand-alone executable.
AC_DEFUN([AC_GAUCHE_FIX_LIBS],
	[
LDFLAGS="$LDFLAGS `$GAUCHE_CONFIG --so-ldflags`"
LIBS="$GAUCHE_LIB `$GAUCHE_CONFIG -l` $LIBS"
AC_SUBST(LDFLAGS)
])

dnl AC_GAUCHE_EXT_FIXUP(FILE [, MODULE])
dnl   [OBSOLETED: Use gauche-config --fixup-extension instead]
dnl   Sets the shell command to generate 'FILE_head.c' and 'FILE_tail.c',
dnl   needed by some platforms for GC.  MODULE must be the extension
dnl   module's name, and has to match the name given to the SCM_INIT_EXTENSION
dnl   macro in the extension initialization code.   If MODULE is omitted
dnl   FILE is used as the module's name.
AC_DEFUN([AC_GAUCHE_EXT_FIXUP],
	 [AC_CONFIG_COMMANDS("$1_head_n_tail",
			     [
AS_IF([test "X$2" = X],
	[ac_gauche_ext_fixup_name=`echo $1 | tr -c "\012A-Za-z0-9" "_"`],
	[ac_gauche_ext_fixup_name="$2"])
AC_MSG_NOTICE(generating $1_head.c and $1_tail.c);
echo "void *Scm__datastart_$ac_gauche_ext_fixup_name = (void*)&Scm__datastart_$ac_gauche_ext_fixup_name;" > $1_head.c
echo "void *Scm__bssstart_$ac_gauche_ext_fixup_name;" >> $1_head.c
echo "void *Scm__dataend_$ac_gauche_ext_fixup_name = (void*)&Scm__dataend_$ac_gauche_ext_fixup_name;" > $1_tail.c
echo "void *Scm__bssend_$ac_gauche_ext_fixup_name;" >> $1_tail.c
])])

dnl AC_GAUCHE_MAKE_GPD
dnl   Creates a Gauche package description file.
dnl
AC_DEFUN([AC_GAUCHE_MAKE_GPD],
	 [
GAUCHE_PACKAGE_CONFIGURE_ARGS=`echo "$ac_configure_args" | sed 's/@<:@\\"\`\$@:>@/\\\\&/g'`
AC_MSG_NOTICE([creating ${PACKAGE_NAME}.gpd])
$GAUCHE_PACKAGE make-gpd "$PACKAGE_NAME" \
  -version "$PACKAGE_VERSION" \
  -configure "./configure $GAUCHE_PACKAGE_CONFIGURE_ARGS"
])
