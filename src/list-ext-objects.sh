#!/bin/sh
#
# list-ext-objects $(top_builddir)
#
#   Gather list of object files in ext/ to be included in libgauche-static.a
#

top_builddir=$1

cd $top_builddir/ext; ${MAKE:=make} list-objects | grep $top_builddir | grep -v 'make\[.*\]: \(Entering\|Leaving\) directory'

