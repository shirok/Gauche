#!/bin/sh
#
# Build distribution archive
#
# Usage:
#  $ sh tools/make-tgz.sh
#
#  Output file will save to ../Gauche-${VERSION}.tgz
#

set -e

topdir=$(pwd)

VERSION=$(cat VERSION)
name="Gauche-${VERSION}"

t=$(mktemp --directory)
g_dir="${t}/${name}"

#
# Build distribution source tree
#
git archive --prefix="${name}/" HEAD | tar --extract --directory="${t}"

#
# Build generated files
#
cd "${g_dir}"
./DIST gen
./configure
make GOSH=gosh pre-package
make distclean

#
# Make final archive file
#
cd "${t}"
tar --create --exclude-from="${g_dir}/DIST_EXCLUDE" --owner=root --group=root --sort=name --file="${name}.tar" "${name}"
gzip -9 "${name}.tar"
cp "${name}.tar.gz" "${topdir}/../${name}.tgz"

#
# Cleanup
#
cd "${topdir}"
rm --recursive --force "${t}"
