#!/bin/bash
#
# Generate feature
#

top_srcdir=$1
top_builddir=$2

config=${top_builddir}/src/gauche/config.h

features_c=${top_builddir}/src/features.c
features_flags=${top_builddir}/src/features.flags

rm -f ${features_c} ${features_flags}

# check feature_name definition ...
check () {
    feature_name=$1
    have_feature=no
    shift
    while [ $# -gt 0 ]; do
        if grep "#define $definition" $config > /dev/null; then
            have_feature=yes
            break
        fi
    done

    if [ $have_feature = yes ]; then
        echo "  { \"$feature_name\", NULL }," >> $features_c
        echo " -F$feature_name" >> $features_flags
    else
        echo " -F-$feature_name" >> $features_flags
    fi
}


check gauche.sys.sigwait HAVE_SIGWAIT
check gauche.sys.setenv HAVE_PUTENV HAVE_SETENV
check gauche.sys.unsetenv HAVE_UNSETENV
check gauche.sys.clearenv HAVE_CLEARENV
check gauche.sys.getloadavg HAVE_GETLOADAVG
check gauche.sys.getrlimit HAVE_SYS_RESOURCE_H
check gauche.sys.lchown HAVE_LCHOWN
check gauche.sys.getpgid HAVE_GETPGID
check gauche.sys.setgroups HAVE_SETGROUPS
check gauche.sys.nanosleep HAVE_NANOSLEEP GAUCHE_WINDOWS
check gauche.sys.crypt HAVE_CRYPT
check gauche.sys.symlink HAVE_SYMLINK
check gauche.sys.readlink HAVE_READLINK
check gauche.sys.select HAVE_SELECT
