#!/bin/sh

# It seems quite complicated to run extra commands in middle of
# building in VC++, so we pre-generate intermediate .c/.h files.
#
# In Cygwin, run this script in the $(top_srcdir), as "sh winnt/winvc-prep.sh"

./configure --build=i686-pc-winnt
GAUCHE_PRE_GENERATE_FOR_WINVC=1 make GOSH=gosh GAUCHE_CONFIG=gauche-config pre-package-ext
(cd src; make GOSH=gosh gauche-config.c)

