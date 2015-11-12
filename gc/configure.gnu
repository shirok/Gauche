#!/bin/sh
#
# Trampoline script to pass everything to "${top_builddir}/gc/configure.gauche".
# This file is required to work out-of-tree build properly.

# NB: The parent configure sets SHELL with their CONFIG_SHELL

${SHELL} ./configure.gnu-gauche "${@}"
