# Create MinGW binary distribution.
# You need MinGW and MSYS.  (You no longer need Cygwin.)
# The compiled binary is installed in ../Gauche-mingw-dist.  You can just
# zip it and distribute.
# See mingw-memo.txt for the details.

# Set MINGWDIR if MinGW is installed in different place.
mingwdir=${MINGWDIR:-/mingw}

# make sure we're going to use Mingw gcc
case `gcc --version` in
  *mingw*) ;;
  *) echo "Set PATH to have MinGW bin directory first"
     exit 1;;
esac

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
rm -rf $distdir/Gauche/lib/libgauche.dll*
cp COPYING $distdir/Gauche
cp $mingwdir/bin/mingwm10.dll $distdir/Gauche/bin
for dll in libcharset-1.dll libiconv-2.dll libz-1.dll; do
  if [ -f $mingwdir/bin/$dll ]; then
    cp $mingwdir/bin/$dll $distdir/Gauche/bin
  fi
done

# 'zip' isn't included in MinGW.
#VERSION=`cat VERSION`
#(cd $distdir; zip -r Gauche-mingw-$VERSION.zip Gauche)
