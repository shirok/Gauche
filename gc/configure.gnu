#!/bin/sh

# "--enable-threads=..."
#   [SK] Original Boehm GC checks gcc for the default thread support.
#   In our case we need the thread config in sync with the main Gauche
#   source tree.  The Gauche's configure leaves its thread settings in
#   'config.threads' so we just read it.
#   config.threads is created in the build directory instead of $srcdir,
#   so we directly refer it.

${CONFIG_SHELL} ./configure ${@} \
		--enable-threads=$(cat ../config.threads)
