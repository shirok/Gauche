include ../Makefile.ext

SCM_CATEGORY = gauche

pkgarch_LTLIBRARIES = fcntl.la
pkgdata_DATA        = fcntl.scm

fcntl_la_SOURCES = fcntl_head.c fcntl.c fcntlib.c fcntl_tail.c
fcntl_la_LDFLAGS = -no-undefined -module -avoid-version
fcntl_la_LIBADD  = $(top_builddir)/src/libgauche.la

GENERATED = Makefile fcntllib.c fcntl_head.c fcntl_tail.c

fcntl_head.c fcntl_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension fcntl
