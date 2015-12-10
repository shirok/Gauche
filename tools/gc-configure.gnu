#!/bin/sh
#
# Trampoline script to pass everything to "${top_builddir}/gc/configure.gnu-gauche".
# This file is required to work out-of-tree build properly.

# NB: This file will be copied from "tools/gc-configure.gnu" to
#     "gc/configure.gnu" by "./DIST gen" command.

# NB: The parent configure sets SHELL with their CONFIG_SHELL

${SHELL} ./configure.gnu-gauche "${@}"
