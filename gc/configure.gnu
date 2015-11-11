#!/bin/sh
#
# Trampoline script to pass everything to "${top_builddir}/gc/configure.gauche".
# This file is required to work out-of-tree build properly.

${CONFIG_SHELL} ./configure.gnu-gauche "${@}"
