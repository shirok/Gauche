#!/bin/sh

set -e

# Run on cygwin under $(topdir) to create a list of dlls to be rebased.
# The user is expected to run:
#  $ /bin/rebaseall -v -T dlls.txt
# After finishing 'make'.   (Even make check won't run without rebase).

needrebase=no
if [ ! -f dlls.txt ]; then
  needrebase=yes
else
  for dll in `cat dlls.txt`; do
    if [ -e $dll -a $dll -nt dlls.txt ]; then
      needrebase=yes
    fi
  done
fi

if [ $needrebase = no ]; then exit 0; fi

find . -name '*.dll' -print > dlls.txt

echo '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
echo '!!                        I M P O R T A N T                             !!'
echo '!!                                                                      !!'
echo '!! You need to run "rebaseall" with the following command line in this  !!'
echo '!! directory, before running "make install" or even "make check".       !!'
echo '!!                                                                      !!'
echo '!!                 $ /bin/rebaseall -v -T dlls.txt                      !!'
echo '!!                                                                      !!'
echo '!! It must be run without running other Cygwin processes but ash.       !!'
echo '!! (You must not even run bash.  Close all cygwin windows, open Windows !!'
echo '!! command prompt, run \cygwin\bin\ash (the actual path may differ      !!'
echo '!! depending on how you install cygwin), cd to your build directory,    !!'
echo '!! then run the above command.)                                         !!'
echo '!! See http://www.tishler.net/jason/software/rebase/rebase-2.4.2.README !!'
echo '!! for more details.                                                    !!'
echo '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
