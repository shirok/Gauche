#!/bin/sh

top_builddir=$1
option=$2

gc_makefile=${top_builddir}/gc/Makefile
gc_config=${top_builddir}/gc/include/config.h

if [ ! -e "$gc_makefile" ]; then
    echo "$gc_makefile doesn't exist---you have to run configure script."
    exit 1
fi

case $option in
    --cflags)
        cflags=`sed -n -e 's@ATOMIC_OPS_CFLAGS =\(.*\)@\1@p' $gc_makefile`
        if grep 'define GC_BUILTIN_ATOMIC' $gc_config > /dev/null; then
            cflags="-DGC_BUILTIN_ATOMIC $cflags"
        fi
        echo $cflags
        ;;
    --libs)
        sed -n -e 's@ATOMIC_OPS_LIBS =\(.*\)@\1@p' $gc_makefile
        ;;
    *)
        echo "Usage: get-atomic-ops-flags.sh TOP_BUILDDIR --cflags|--libs"
        exit 1
        ;;
esac
