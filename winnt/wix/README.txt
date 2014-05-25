To create Gauche-mingw Windows installer:

Requirements:
  MinGW
  MSYS
  WiX toolset 3.8 or later ( http://wixtoolset.org/ )

Make sure WiX toolset is in $PATH.

How To:

In the top source directory, run mingw-dist.sh script as follow:

  $ src/mingw-dist.sh --with-installer

It builds MinGW Gauche and installs binaries and examples under
'Gauche' subdirectory in this directory, then run 'make' in 
this directory.

On successful completion, you get Gauche-mingw-$VERSION.msi here.




