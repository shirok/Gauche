#!/bin/sh
#
# list-ext-objects $(top_builddir) <extra-dir> ...
#
#   Gather list of object files in ext/ to be included in libgauche-static.a
#
#   Directories under ext/ are scanned (via `make list-objects`) unconditionally.
#   Directories given in <extra-dir> are also scanned; they must be relative
#   to $top_builddir.  The makefile in the directory must recognize
#   list-objects target.  Currently it may be used to scan internal mbedtls
#   library.

top_builddir=$1

(cd $top_builddir/ext && ${MAKE:=make} list-objects | grep '^/// ' | sed "sx/// xx")

shift
while [ $# -gt 0 ]; do
    (cd $top_builddir/$1 && ${MAKE:=make} list-objects | grep '^/// ' | sed "sx/// xx")
    shift
done
