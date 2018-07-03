# Create MinGW binary distribution.
# Run this script on topsrcdir
# You need MinGW and MSYS.  (You no longer need Cygwin.)
# The compiled binary is installed in ../Gauche-mingw-dist.  You can just
# zip it and distribute.
# See doc/HOWTO-mingw.txt for the details.

set -e

# Set MINGWDIR if MinGW is installed in different place.
case "$MSYSTEM" in
  MINGW64)
    mingwdir=${MINGWDIR:-/mingw64}
    mingwarch="x86_64"
    ;;
  MINGW32)
    mingwdir=${MINGWDIR:-/mingw32}
    mingwarch="i686"
    ;;
  *)
    echo 'WARNING: $MSYSTEM is neither "MINGW32" or "MINGW64".'
    mingwdir=${MINGWDIR:-/mingw}
    mingwarch="unknown"
    ;;
esac

# Setting PATH to make sure the build process find right tools.
# NB: We don't use CC=/path/to/gcc trick for ./configure, since
# the gcc path embedded in gauche-config should be Windows path.
export PATH=$mingwdir/bin:$PATH

## Build architecture
#  Mingw-w64 automatically identifies this by default if you're in an
#  appropriate shell, but we sometimes need to cross-build (meaning,
#  building i686 binary on x86_64 shell, for example), so this setting
#  makes things easier.
case "$MSYSTEM" in
  MINGW64)
    buildopt=--build=x86_64-w64-mingw32;;
  MINGW32)
    buildopt=--build=i686-w64-mingw32;;
  *)
    buildopt=--build=i686-pc-mingw32;;
esac

# Process Options:
while [ "$#" -gt 0 ]; do
  case $1 in
    --with-gl)   WITH_GL=yes; shift;;
    --with-installer) INSTALLER=yes; shift;;
    --skip-config) SKIP_CONFIG=yes; shift;;
    -*)
     echo "Options:"
     echo "  --with-gl: Include Gauche-gl.  Gauche-gl source must be in ../Gauche-gl."
     echo "  --with-installer:  Creates binary installer using Wix.  'candle.exe' and"
     echo "      'light.exe' must be visible in PATH."
     echo "  --skip-config:  Skip cleanup and configuration."
     exit 1;;
  esac
done

if [ "$WITH_GL" = yes ]; then
  if [ -d ../Gauche-gl ]; then echo "Found Gauche-gl source."
  else echo "--with-gl: Cannot find ../Gauche-gl.  Aborting."; exit 1
  fi
fi

if [ "$INSTALLER" = yes ]; then
  wix_path=`which candle.exe 2> /dev/null`
  if test -e "$wix_path"; then echo "Wix SDK found: $wix_path"
  else echo "--installer: Cannot find Wix SDK.  Aborting."; exit 1
  fi
fi

# check gosh
gosh_path=`which gosh.exe 2> /dev/null`
if test -e "$gosh_path"; then
  echo "Using $gosh_path: " `gosh -V`
else
  echo "Cannot find gosh.exe.  Aborting."
  exit 1
fi

# build
if [ "$SKIP_CONFIG" != yes ]; then
  if [ -f Makefile ]; then make distclean; fi
  if [ -f examples/spigot/Makefile ]; then
    (cd examples/spigot; make maintainer-clean);
  fi
  if [ -f examples/mqueue-cpp/Makefile ]; then
    (cd examples/mqueue-cpp; make maintainer-clean);
  fi
fi

if [ "$INSTALLER" = yes ]; then
  distdir=`pwd`/winnt/wix/Gauche
else
  distdir=`pwd`/../Gauche-mingw-dist/Gauche-${mingwarch}
fi

if [ "$SKIP_CONFIG" != yes ]; then
  rm -rf $distdir
  ./configure --prefix=$distdir --enable-threads=win32 \
              --enable-multibyte=utf8 --enable-ipv6=no \
	      --with-tls=axtls \
              --with-dbm=ndbm,odbm $buildopt
fi
make

if [ $? -ne 0 ]; then
  echo "Build failed.  Aborting packaging."
  exit 1
fi

# prepare precompiled directory tree.
make install
(cd src; make install-mingw)
make install-examples
rm -rf $distdir/lib/libgauche.dll*
case "$MSYSTEM" in
  MINGW64|MINGW32)
    for dll in libwinpthread-1.dll; do
      if [ -f $mingwdir/bin/$dll ]; then
        cp $mingwdir/bin/$dll $distdir/bin
      fi
    done
    ;;
  *)
    cp $mingwdir/bin/mingwm10.dll $distdir/bin
    ;;
esac

# Enable using freshly-built gosh for the subsequent operations.
PATH=$distdir/bin:$PATH

# Build GL
if [ "$WITH_GL" = "yes" ]; then
  if [ "$SKIP_CONFIG" != yes ]; then
    (cd ../Gauche-gl; ./DIST gen; ./configure --prefix=$distdir --with-glut=mingw-static $buildopt)
  fi
  (cd ../Gauche-gl; make clean; make; make install install-examples)
fi

# Build installer
if [ "$INSTALLER" = "yes" ]; then
  (cd winnt/wix; make)
  cp winnt/wix/Gauche-mingw-*.msi ..
fi

# 'zip' isn't included in MinGW.
#VERSION=`cat VERSION`
#(cd $distdir; zip -r Gauche-mingw-$VERSION.zip Gauche)
