#!/bin/sh
#
# Generate feature.{c|flags}
#

top_srcdir=$1
top_builddir=$2
host=$3

if [ -z "$1" -o -z "$2" -o -z "$3" ]; then
    echo "Usage: gen-features.sh TOP_SRCDIR TOP_BUILDDIR HOST"
    exit 1
fi

config=${top_builddir}/src/gauche/config.h

features_c=${top_builddir}/src/features.c
features_c_tmp=${features_c}.$$
features_flags=${top_builddir}/src/features.flags
features_flags_tmp=${features_flags}.$$
host_tmp=gen-features.tmp.$$

clean () {
    rm -f ${features_c_tmp}
    rm -f ${features_flags_tmp}
    rm -f ${host_tmp}
}

trap clean EXIT

# add_feature feature_name [module_name]
add_feature() {
    feature_name=$1
    module_name=$2
    if [ -z "$module_name" -o "$module_name" = NULL ]; then
        echo "  { \"$feature_name\", NULL }," >> $features_c_tmp
    else
        echo "  { \"$feature_name\", \"$module_name\" }," >> $features_c_tmp
    fi
    echo " -F$feature_name" >> $features_flags_tmp
}

# remove_feature feature_name
remove_feature() {
    feature_name=$1
    echo " -F-$feature_name" >> $features_flags_tmp
}

set_cpu_os_features() {
    # set up cpu and os from autoconf triplet
    echo $host | tr -- '-' ' ' > $host_tmp
    read cputype manufacturer kernel os < $host_tmp

    if [ -n "$cputype" ]; then
        add_feature "$cputype"
    fi

    if [ -n "$kernel" ]; then
        # kernel part may contain release number.  We set both the full name
        # and the name without release number.
        kernel_sans_rel=`echo $kernel | sed -e 's/[0-9.]*$//'`

        add_feature "$kernel"
        if [ "$kernel" != "$kernel_sans_rel" ]; then
            add_feature "$kernel_sans_rel"
        fi
        case $kernel_sans_rel in
            freebsd|openbsd|netbsd|netbsdelf|darwin)
                add_feature "bsd";;
        esac

        if [ -n "$os" ]; then
            # e.g. 'linux-gnu'
            add_feature "$kernel-$os"
            if [ "$kernel" != "$kernel_sans_rel" ]; then
                add_feature "$kernel_sans_rel-$os"
            fi
        fi
    fi
}

# check feature_name definition ...
#   Search definition ... in config.h, and if found, add feature_name.
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
        add_feature "$feature_name" "$module_name"
    else
        remove_feature "$feature_name"
    fi
}


clean
set_cpu_os_features
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
check gauche.sys.statvfs gauche.fcntl HAVE_SYS_STATVFS_H

mv $features_c_tmp $features_c
mv $features_flags_tmp $features_flags
