#
# Gauche-specific aucotonf macros.
# $Id: aclocal.m4,v 1.13 2002-10-15 10:27:53 shirok Exp $

# AC_GAUCHE_INIT_EXT
#   Sets some parameters about installed Gauche package.  This macro checks
#   if you're configuring within Gauche source tree or as an individual
#   extension package, and sets up the following macros appropriately.
#
#    GAUCHE_CONFIG   - Name of gauche-config script
#    GAUCHE_TOP      - Directory prefix where Gauche is installed, or
#                      empty if this is an individual extension.
#    GAUCHE_INC      - '-I' macros required to compile extensions.
#    GAUCHE_LIB      - '-L' macros required to link extensions.
#    GOSH            - Name of gosh executable, possibly including
#                      options to run it from the extension directory.
#    GAUCHE_VERSION  - The version of Gauche.
AC_DEFUN([AC_GAUCHE_INIT_EXT],
         [
if test -f ../../src/gauche.h; then
  GAUCHE_CONFIG="sh ../../src/gauche-config"
  GAUCHE_TOP='../../'
  GAUCHE_INC="-I../../src -I../../gc/include `$GAUCHE_CONFIG --local-incdir`"
  GAUCHE_LIB="-L../../src"
  GOSH="../../src/gosh -q -I../../src -I../../lib -lgauche-init"
else
  GAUCHE_CONFIG=gauche-config
  GAUCHE_TOP=
  GAUCHE_INC="`gauche-config -I`"
  GAUCHE_LIB="`gauche-config -L`"
  GOSH=gosh
fi
AC_SUBST(GAUCHE_CONFIG)
AC_SUBST(GAUCHE_TOP)
AC_SUBST(GAUCHE_INC)
AC_SUBST(GOSH)

GAUCHE_VERSION=`$GAUCHE_CONFIG -V`
AC_SUBST(GAUCHE_VERSION)
AC_DEFINE_UNQUOTED(GAUCHE_VERSION, "$GAUCHE_VERSION")
])

# AC_GAUCHE_INSTALL_TYPE(TYPE)
#   Sets the default value of INSTALL_TYPE macro.  TYPE must be either
#   sys or site.
AC_DEFUN([AC_GAUCHE_INSTALL_TYPE],
         [
: ${INSTALL_TYPE=$1}
if test "X$INSTALL_TYPE" != "Xsys" -a "X$INSTALL_TYPE" != "Xsite"; then
  AC_MSG_ERROR([INSTALL_TYPE must be either 'sys' or 'site'])
fi
AC_SUBST(INSTALL_TYPE)
])

# AC_GAUCHE_CC
#   Gets compiler parameters which Gauche has been compiled with.
AC_DEFUN([AC_GAUCHE_CC],
         [
CC="`$GAUCHE_CONFIG --cc`"
AC_SUBST(CC)
# adds default CFLAGS if it has not been set.
ac_gauche_CFLAGS=${CFLAGS+set}
if test -z "$ac_gauche_CFLAGS"; then
  CFLAGS="`$GAUCHE_CONFIG --default-cflags`"
fi
])

# AC_GAUCHE_FLAGS
#   Sets CFLAGS, CPPFLAGS and LDFLAGS appropriate for furthre testing.
#   This should come before any testings that requires those flags to be set.
AC_DEFUN([AC_GAUCHE_FLAGS],
         [
CFLAGS="$CFLAGS $GAUCHE_INC `$GAUCHE_CONFIG --so-cflags`"
AC_SUBST(CFLAGS)
CPPFLAGS="$CPPFLAGS $GAUCHE_INC"       # some test requires this
LDFLAGS="$LDFLAGS `$GAUCHE_CONFIG --local-libdir`"
AC_GAUCHE_OPTFLAGS
])

# AC_GAUCHE_OPTFLAGS
#   Sets OPTFLAGS with some optimization flags using heuristics.
#   If you use AC_GAUCHE_FLAGS, this test is included.
#   The main configure and gc's configure also use this.
AC_DEFUN([AC_GAUCHE_OPTFLAGS],
         [
case "$host" in
  i686-*) I686OPT="-DUSE_I686_PREFETCH";;
esac
if test $CC = "gcc"; then
  GCCOPT="-fomit-frame-pointer"
  case "$host" in
   i586-*) GCCOPT="$GCCOPT -march=i586";;
   i686-*) GCCOPT="$GCCOPT -march=i686";;
  esac
fi
OPTFLAGS="$GCCOPT $I686OPT"
AC_SUBST(OPTFLAGS)
])

# AC_GAUCHE_FIX_LIBS
#   Sets LDFLAGS and LIBS to generate shared library.
#   This has to come all the tests that requre linking, or those test
#   will fail because they can't generate stand-alone executable.
AC_DEFUN([AC_GAUCHE_FIX_LIBS],
         [
LDFLAGS="$LDFLAGS `$GAUCHE_CONFIG --so-ldflags`"
LIBS="$GAUCHE_LIB `$GAUCHE_CONFIG -l` $LIBS"
AC_SUBST(LDFLAGS)
])

# AC_GAUCHE_EXT_FIXUP(FILE [, MODULE])
#   Sets the shell command to generate 'FILE_head.c' and 'FILE_tail.c',
#   needed by some platforms for GC.  MODULE must be the extension
#   module's name, and has to match the name given to the SCM_INIT_EXTENSION
#   macro in the extension initialization code.   If MODULE is omitted
#   FILE is used as the module's name.
AC_DEFUN([AC_GAUCHE_EXT_FIXUP],
         [AC_CONFIG_COMMANDS("$1_head_n_tail",
                             [
if test "X$2" = X; then 
  ac_gauche_ext_fixup_name=`echo $1 | tr -c "\012A-Za-z0-9" "_"`
else
  ac_gauche_ext_fixup_name="$2"
fi
AC_MSG_NOTICE(generating $1_head.c and $1_tail.c);
echo "void *Scm__datastart_$ac_gauche_ext_fixup_name = (void*)&Scm__datastart_$ac_gauche_ext_fixup_name;" > $1_head.c
echo "void *Scm__bssstart_$ac_gauche_ext_fixup_name;" >> $1_head.c
echo "void *Scm__dataend_$ac_gauche_ext_fixup_name = (void*)&Scm__dataend_$ac_gauche_ext_fixup_name;" > $1_tail.c
echo "void *Scm__bssend_$ac_gauche_ext_fixup_name;" >> $1_tail.c
])])


