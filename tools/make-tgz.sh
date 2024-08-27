#!/bin/sh
#
# Build distribution archive
#
# Usage:
#  $ sh tools/make-tgz.sh [destdir [snapshot_id]]
#
#  Output file will save to ../Gauche-${VERSION}[-${SNAPSHOT_ID}.tgz
#
# DESTDIR specifies where the tarball is placed.  The default is '$pwd/..'.
#
# If SNAPSHOT_ID is given, it is set to GAUCHE_SNAPSHOT_ID and shown in
# the output of 'gosh -V'.  It is not included in '(gauche-version)', so
# it won't affect the installation directory.

set -e

destdir=$1
if [ -z "$destdir" ]; then
    destdir=`pwd`/..
fi

snapshot_id=$2

VERSION=$(grep AC_INIT configure.ac | cut --delimiter=, --fields=2 | tr --delete "[]")
if [ -z "$snapshot_id" ]; then
    name="Gauche-${VERSION}"
else
    name="Gauche-${VERSION}+${snapshot_id}"
fi

t=$(mktemp --directory)
g_dir="${t}/${name}"

#
# Build distribution source tree
#
git archive --prefix="${name}/" HEAD | tar --extract --directory="${t}"

if [ -n "$snapshot_id" ]; then
    sed "s/GAUCHE_SNAPSHOT_ID=/GAUCHE_SNAPSHOT_ID=${snapshot_id}/" \
        ${g_dir}/configure.ac > configure.ac.tmp && \
        mv configure.ac.tmp ${g_dir}/configure.ac
fi

#
# Build generated files
#
cd "${g_dir}"
./DIST gen
./configure --enable-threads=pthreads --enable-multibyte=utf8 --disable-dependency-tracking
make GOSH=gosh BUILD_GOSH_FLAGS=${BUILD_GOSH_FLAGS} pre-package
make distclean

#
# Make final archive file
#
cd "${t}"
tar --create --exclude-from="${g_dir}/DIST_EXCLUDE" --owner=root --group=root --sort=name --file=- "${name}" | gzip --best > "${destdir}/${name}.tgz"

echo "${destdir}/${name}.tgz is ready."

#
# Cleanup
#
cd "${destdir}"
rm --recursive --force "${t}"
