# Create MinGW binary distribution.
# Run this script on topsrcdir
# You need MinGW and MSYS.  (You no longer need Cygwin.)
# The compiled binary is installed in ../Gauche-mingw-dist.  You can just
# zip it and distribute.
# See mingw-memo.txt for the details.

# Set MINGWDIR if MinGW is installed in different place.
mingwdir=${MINGWDIR:-/mingw}

# Process Options:
while [ "$#" -gt 0 ]; do
  case $1 in
    --with-gl)   WITH_GL=yes; shift;;
    --with-installer) INSTALLER=yes; shift;;
    -*)
     echo "Options:"
     echo "  --with-gl: Include Gauche-gl.  Gauche-gl source must be in ../Gauche-gl."
     echo "  --with-installer:  Creates binary installer using NSIS.  'makensis.exe'"
     echo "      must be visible in PATH."
     exit 1;;
  esac
done

if [ "$WITH_GL" = yes ]; then
  if [ -d ../Gauche-gl ]; then echo "Found Gauche-gl source."
  else echo "--with-gl: Cannot find ../Gauche-gl.  Aborting."; exit 1
  fi
fi

if [ "$INSTALLER" = yes ]; then
  makensis_path=`which makensis.exe 2> /dev/null`
  if test -e "$makensis_path"; then echo "NSIS compiler found: $makensis_path"
  else echo "--installer: Cannot find makensis.exe.  Aborting."; exit 1
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
distdir=`pwd`/../Gauche-mingw-dist
rm -rf $distdir
./configure --enable-multibyte=utf8 --prefix=$distdir/Gauche
make

# prepare precompiled directory tree.
make install
(cd src; make install-mingw)
make install-examples
rm -rf $distdir/Gauche/lib/libgauche.dll*
cp COPYING $distdir/Gauche
cp $mingwdir/bin/mingwm10.dll $distdir/Gauche/bin
for dll in libcharset-1.dll libiconv-2.dll libz-1.dll; do
  if [ -f $mingwdir/bin/$dll ]; then
    cp $mingwdir/bin/$dll $distdir/Gauche/bin
  fi
done

# Build GL
if [ "$WITH_GL" = "yes" ]; then
  PATH=$distdir/Gauche/bin:$PATH
  (cd ../Gauche-gl; ./DIST gen; \
   if test -f Makefile; then make clean; fi; \
   ./configure --prefix=$distdir/Gauche --with-glut=mingw-static; \
   make; make install; make install-examples)
fi

# Build installer
if [ "$INSTALLER" = "yes" ]; then
  (cd winnt/nsis; make)
fi

# 'zip' isn't included in MinGW.
#VERSION=`cat VERSION`
#(cd $distdir; zip -r Gauche-mingw-$VERSION.zip Gauche)
