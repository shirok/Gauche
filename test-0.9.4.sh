#!/bin/sh
make maintainer-clean
./DIST gen
./configure --prefix=$HOME/tmp/0.9.4
make clean && make -j && make install
make maintainer-clean
./DIST gen
PATH=$HOME/tmp/0.9.4/bin:$PATH ./configure
PATH=$HOME/tmp/0.9.4/bin:$PATH make -j CPPFLAGS="-DAUTO_UNBOXING=0"



