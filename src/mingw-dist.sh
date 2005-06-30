# Create Mingw binary distribution.
# See mingw-memo.txt for the details.

# You need Cygwin, and Cygwin-version of Gauche installed in order to
# make Mingw binary package from the release tarball.

# Set those two variables for your env.
mingwdir=${MINGWDIR:-/cygdrive/c/mingw}
cyggosh=${CYGGOSH:-/usr/local/bin/gosh}

# make sure we're going to use Mingw gcc
PATH=$mingwdir/bin:$PATH
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
distdir=`pwd`/../Gauche-mingw-dist
rm -rf $distdir
./configure --build=i686-pc-mingw32 --enable-multibyte=utf8 --prefix=$distdir/Gauche
make

# use Cygwin's gosh to install stuff
VERSION=`cat VERSION`
make GOSH=$cyggosh install
(cd src; make GOSH=$cyggosh install-mingw)
rm -rf $distdir/Gauche/lib/libgauche.dll*
cp COPYING $distdir/Gauche
cp $mingwdir/bin/mingwm10.dll $distdir/Gauche/bin
(cd $distdir; zip -r Gauche-mingw-$VERSION.zip Gauche)
