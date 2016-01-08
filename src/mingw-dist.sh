# Create MinGW binary distribution.
# Run this script on topsrcdir
# You need MinGW and MSYS.  (You no longer need Cygwin.)
# The compiled binary is installed in ../Gauche-mingw-dist.  You can just
# zip it and distribute.
# See doc/HOWTO-mingw.txt for the details.

# Set MINGWDIR if MinGW is installed in different place.
case "$MSYSTEM" in
  MINGW64)
    mingwdir=${MINGWDIR:-/mingw64}
    ;;
  MINGW32)
    mingwdir=${MINGWDIR:-/mingw32}
    ;;
  *)
    mingwdir=${MINGWDIR:-/mingw}
    ;;
esac

# Process Options:
while [ "$#" -gt 0 ]; do
  case $1 in
    --with-gl)   WITH_GL=yes; shift;;
    --with-installer) INSTALLER=yes; shift;;
    -*)
     echo "Options:"
     echo "  --with-gl: Include Gauche-gl.  Gauche-gl source must be in ../Gauche-gl."
     echo "  --with-installer:  Creates binary installer using Wix.  'candle.exe' and"
     echo "      'light.exe' must be visible in PATH."
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

# build
if [ -f Makefile ]; then make distclean; fi
if [ -f examples/spigot/Makefile ]; then
  (cd examples/spigot; make maintainer-clean);
fi
if [ -f examples/mqueue-cpp/Makefile ]; then
  (cd examples/mqueue-cpp; make maintainer-clean);
fi

if [ "$INSTALLER" = yes ]; then
  distdir=`pwd`/winnt/wix/Gauche
else
  distdir=`pwd`/../Gauche-mingw-dist/Gauche
fi
rm -rf $distdir
./configure --prefix=$distdir --enable-threads=win32 \
            --enable-multibyte=utf8 --enable-ipv6=no \
            --with-dbm=ndbm,odbm
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
    for dll in libwinpthread-1.dll libcharset-1.dll libiconv-2.dll zlib1.dll libz-1.dll; do
      if [ -f $mingwdir/bin/$dll ]; then
        cp $mingwdir/bin/$dll $distdir/bin
      fi
    done
    ;;
  *)
    cp $mingwdir/bin/mingwm10.dll $distdir/bin
    for dll in libcharset-1.dll libiconv-2.dll zlib1.dll libz-1.dll; do
      if [ -f $mingwdir/bin/$dll ]; then
        cp $mingwdir/bin/$dll $distdir/bin
      fi
    done
    ;;
esac

# Build GL
if [ "$WITH_GL" = "yes" ]; then
  PATH=$distdir/bin:$PATH
  (cd ../Gauche-gl; ./DIST gen; \
   if test -f Makefile; then make clean; fi; \
   ./configure --prefix=$distdir --with-glut=mingw-static; \
   make; make install; make install-examples)
fi

# Build installer
if [ "$INSTALLER" = "yes" ]; then
  (cd winnt/wix; make)
fi

# 'zip' isn't included in MinGW.
#VERSION=`cat VERSION`
#(cd $distdir; zip -r Gauche-mingw-$VERSION.zip Gauche)
