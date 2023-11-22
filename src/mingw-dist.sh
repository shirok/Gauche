#!/bin/sh
# Create MinGW binary distribution.
# Run this script on topsrcdir
# You need MinGW and MSYS.  (You no longer need Cygwin.)
# The compiled binary is installed in ../Gauche-mingw-dist.  You can just
# zip it and distribute.
# See doc/HOWTO-mingw.adoc for the details.

set -e

# TRANSIENT: Disabled version specification of Gauche for build.
# Consider to enable it after 0.9.14 release.
DISABLE_BUILD_GOSH_FLAGS="BUILD_GOSH_FLAGS="

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
    --with-mbedtls) MBEDTLS=yes; shift;;
    --with-mbedtls=dll) MBEDTLS=dll; shift;;
    --with-zip) ZIP_ARCHIVE=yes; shift;;
    --without-axtls) shift;;    # Ignore this not to reject legacy scripts
    --skip-config) SKIP_CONFIG=yes; shift;;
    -*)
     echo "Options:"
     echo "  --with-gl: Include Gauche-gl.  Gauche-gl source must be in ../Gauche-gl."
     echo "  --with-installer:  Creates binary installer using Wix.  'candle.exe' and"
     echo "      'light.exe' must be visible in PATH."
     echo "  --with-mbedtls: Include MbedTLS."
     echo "      If you create an installer with this option, the binary"
     echo "      will also be covered by Apache License 2.0 for MbedTLS."
     echo "      This option comes with two flavors:"
     echo "      * If you simply say --with-mbedtls, we build MbedTLS from source"
     echo "        and bundle it with extension modules, so you don't need MbedTLS"
     echo "        DLL on the system.  You need CMake, though."
     echo "      * If you say --with-mbedtls=dll, we build to link with MbedTLS"
     echo "        DLL installed on the system.  You need mingw-w64-{i686|x86_64}-mbedtls."
     echo "        If you choose this option, those DLLs in turn depends on libgcc DLL."
     echo "  --with-zip:  Creates Zip archive using p7zip. '7z.exe' must be visible"
     echo "      in PATH."
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

if [ "$MBEDTLS" = yes ]; then
  tlslibs=${tlslibs}mbedtls-internal
elif [ "$MBEDTLS" = dll ]; then
  tlslibs=${tlslibs}mbedtls
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
  ./configure --prefix=$distdir \
              --with-tls=$tlslibs \
              --with-dbm=ndbm,odbm $buildopt
fi
make $DISABLE_BUILD_GOSH_FLAGS

if [ $? -ne 0 ]; then
  echo "Build failed.  Aborting packaging."
  exit 1
fi

# prepare precompiled directory tree.
make install $DISABLE_BUILD_GOSH_FLAGS
(cd src; make install-mingw $DISABLE_BUILD_GOSH_FLAGS)
make install-examples $DISABLE_BUILD_GOSH_FLAGS
rm -rf $distdir/lib/libgauche.dll*
case "$MSYSTEM" in
  MINGW64|MINGW32)
    mingw_dll="libwinpthread-1.dll";;
  *)
    mingw_dll="mingwm10.dll";;
esac

# If we use external mbedtls, we need these DLLs.
# NB: those dlls also depend on libgcc.
if [ "$MBEDTLS" = dll ]; then
  mingw_dll="$mingw_dll libmbedcrypto.dll libmbedtls.dll libmbedx509.dll"
fi

if [ -n "$mingw_dll" ]; then
  for dll in $mingw_dll; do
    if [ -f $mingwdir/bin/$dll ]; then
      cp $mingwdir/bin/$dll $distdir/bin
    fi
  done
fi

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

# Build zip archive
if [ "$ZIP_ARCHIVE" = "yes" ]; then
    VERSION=$(cat VERSION)
    ZIP_FILE="Gauche-mingw-${VERSION}.zip"

    (cd "${distdir}/../" && rm -f "./${ZIP_FILE}" && 7z a "./${ZIP_FILE}" $(basename "${distdir}"))
    cp "${distdir}/../${ZIP_FILE}" ..
fi
