#!/bin/sh
#
# list-ext-objects $(top_builddir)
#
#   Gather list of object files in ext/ to be included in libgauche-static.a
#

top_builddir=$1

${MAKE:=make} -C "${top_builddir}/ext" -s list-objects
