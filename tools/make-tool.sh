#!/bin/sh
#
# Create a runnable file of auxiliary tools, such as gauche-install.
#
# Auxiliary scripts are written in Gauche, and on Unix systems it is
# suffice to use shebang magic to execute them.  However, on Windows,
# shebang doesn't work, and we don't want to rely on .bat files
# (its command-line processing is totaly broken).  So we create
# executables.  Windows also requires resource info if the command
# takes certain actions.

usage() {
    echo "Usage: make-tool.sh <bindir> <target> <source> <aux-source> ..."
    exit 1
}

do_posix() {
    bindir=$1
    target=$2
    source=$3
    rm -f $target
    echo "#!${bindir}/gosh" > $target && cat $source >> $target
    chmod -w+x $target
}

scm2c() {
    source=$1
    target=$2

    rm -f $target
    cat > $target <<EOF
#include "gauche.h"

static const char* script = "\\n"
EOF

    sed -e 's@\\@\\\\@g' -e 's@"@\\"@g' -e 's@^.*$@"&\\n"@g' $source > $target

    cat >> $target <<EOF
;

int main(int argc, const char **argv)
{
    Scm_Init(GAUCHE_SIGNATURE);
    Scm_SimpleMain(argc, argv, script, 0);
    return 0;
}
EOF
}

do_mingw() {
    # NB: Currently we still use ming-exify.  Implement this using scm2c above
    # and replace mingw-exify with this script.
    echo "WRITEME!"
}

#
# Main body
#

bindir=$1
target=$2
source=$3
rsrc=$4

if [ -z "$source" -o -z "$target" -o -z "$bindir" ]; then
    usage
fi

if uname -a | grep -i 'mingw'; then
    do_mingw "${bindir}" "${target}" "${source}" "${rsrc}"
else
    do_posix "${bindir}" "${target}" "${source}"
fi
