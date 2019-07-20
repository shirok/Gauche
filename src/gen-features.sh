#!/bin/sh
#
# Generate feature.{c|flags}
#

top_srcdir=$1
top_builddir=$2

if [ -z "$1" -o -z "$2" ]; then
    echo "Usage: gen-features.sh TOP_SRCDIR TOP_BUILDDIR"
    exit 1
fi

config=${top_builddir}/src/gauche/config.h

features_c=${top_builddir}/src/features.c
features_c_tmp=${features_c}.$$
features_flags=${top_builddir}/src/features.flags
features_flags_tmp=${features_flags}.$$

clean () {
    rm -f ${features_c_tmp}
    rm -f ${features_flags_tmp}
}

trap clean EXIT

# check feature_name definition ...
check () {
    feature_name=$1
    module_name=$2
    have_feature=no
    shift 2
    while [ $# -gt 0 ]; do
        definition=$1
        shift
        if grep "#define $definition" $config > /dev/null; then
            have_feature=yes
            break
        fi
    done

    if [ $have_feature = yes ]; then
        if [ $module_name = NULL ]; then
            echo "  { \"$feature_name\", NULL }," >> $features_c_tmp
        else
            echo "  { \"$feature_name\", \"$module_name\" }," >> $features_c_tmp
        fi
        echo " -F$feature_name" >> $features_flags_tmp
    else
        echo " -F-$feature_name" >> $features_flags_tmp
    fi
}

clean
check gauche.sys.sigwait NULL HAVE_SIGWAIT
check gauche.sys.setenv NULL HAVE_PUTENV HAVE_SETENV
check gauche.sys.unsetenv NULL HAVE_UNSETENV
check gauche.sys.clearenv NULL HAVE_CLEARENV
check gauche.sys.getloadavg NULL HAVE_GETLOADAVG
check gauche.sys.getrlimit NULL HAVE_SYS_RESOURCE_H
check gauche.sys.lchown NULL HAVE_LCHOWN
check gauche.sys.getpgid NULL HAVE_GETPGID
check gauche.sys.setgroups NULL HAVE_SETGROUPS
check gauche.sys.nanosleep NULL HAVE_NANOSLEEP GAUCHE_WINDOWS
check gauche.sys.crypt NULL HAVE_CRYPT
check gauche.sys.symlink NULL HAVE_SYMLINK
check gauche.sys.readlink NULL HAVE_READLINK
check gauche.sys.select NULL HAVE_SELECT

check gauche.net.ipv6 gauche.net HAVE_IPV6
check gauche.sys.openpty gauche.termios HAVE_OPENPTY
check gauche.sys.forkpty gauche.termios HAVE_FORKPTY
check gauche.sys.syslog gauche.syslog HAVE_SYSLOG
check gauche.sys.setlogmask gauche.syslog HAVE_SETLOGMASK
check gauche.sys.fcntl gauche.fcntl HAVE_FCNTL

mv $features_c_tmp $features_c
mv $features_flags_tmp $features_flags
