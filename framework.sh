#!/bin/sh
# Install Gauche as a framework on MacOSX

set -e

# This script installs the Gauche stuff under Gauche.framework.
# It can be just copied into the application bundle as a private
# framework.  The advantage is that the application bundle can
# just simply copied to the machine that doesn't have Gauche installed.
#
# We don't, and probably won't, support a shared framework, though.
# A shared framework needs installation process anyway, and it embeds
# its abosolute path in it so that it can't be casually copied
# afterwards.  So it doesn't have the advantage of private frameworks,
# and we'll get all the disadvantages of the frameworks (e.g. nonstandard
# header paths, difficulty of using gauche auxiliary scripts, etc).

version=$1
framework_dir=`pwd`/Gauche.framework
prefix=$framework_dir/Versions/$version

# Install stuff under $prefix.
rm -rf $framework_dir
(cd lib; make prefix=$prefix install)
(cd ext; make prefix=$prefix install)
(cd src; make prefix=$prefix TARGETLIB=@executable_path/../Frameworks/Gauche.framework install)
mkdir $prefix/Resources
cp src/Info.plist $prefix/Resources

# Creates apropriate links
rm -f $framework_dir/Versions/Current
ln -s ./$version $framework_dir/Versions/Current
rm -f $framework_dir/Headers
ln -s ./Versions/Current/lib/gauche/$version/include $framework_dir/Headers
rm -f $framework_dir/libgauche.dylib
ln -s ./Versions/Current/lib/libgauche.dylib $framework_dir/libgauche.dylib
rm -f $framework_dir/Gauche
ln -s ./libgauche.dylib $framework_dir/Gauche
rm -f $framework_dir/Resources
ln -s ./Versions/Current/Resources $framework_dir/Resources

# Modifies header to meet MacOSX convention (duh!)
for h in $framework_dir/Headers/*.h $framework_dir/Headers/gauche/*.h
do
  sed -e 's@^#include <gc\.h>@#include <Gauche/gc.h>@' \
      -e 's@^# *include <\(gauche.*\)\.h>@#include <Gauche/\1.h>@' \
      -e 's@^# *include "\(gc.*\)\.h"@#include "Gauche/\1.h"@' $h > tmp.h && \
   mv -f $h $h.bak && \
   mv tmp.h $h && \
   chmod 444 $h && \
   rm -f $h.bak
done
