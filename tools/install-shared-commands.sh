#!/bin/sh

# Create a symlink (if possible) or copy of the commadns with common-name.
# Used to create common name commands when --enable-shared-commands is given.

usage() {
    echo "install-shaerd-commands.h <BINDIR> <DESTDIR>"
    exit 1
}

bindir=$1
destdir=$2                      # aux prefix when install for packaging

if [ -z "$bindir" ]; then usage; fi

target="$destdir$bindir"

makelink() {
    gauche_name=$1
    common_name=$2

    rm -f $target/$common_name
    if uname -a | grep -i 'mingw'; then
        cp "$target/$gauche_name.exe" "$target/$common_name.exe"
    else
        ln -s "$gauche_name" "$target/$common_name"
    fi
}

# SRFI-22
makelink gosh scheme-r7rs
makelink gosh scheme-srfi-0
makelink gosh scheme-srfi-7

# SRFI-138
makelink gauche-compile-r7rs compile-r7rs
