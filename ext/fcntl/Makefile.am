include ../Makefile.ext

pkghdrdir  = $(GAUCHE_HDRDIR)/gauche
pkgdatadir = $(GAUCHE_LIBDIR)/gauche
pkglibdir  = $(GAUCHE_ARCHDIR)

pkglib_LTLIBRARIES = fcntl.la

fcntl_la_SOURCES = fcntl_head.c fcntl.c fcntlib.c fcntl_tail.c
fcntl_la_LDFLAGS = -no-undefined -module -avoid-version
fcntl_la_LIBADD  = $(top_builddir)/src/libgauche.la

GENERATED = Makefile fcntllib.c fcntl_head.c fcntl_tail.c

pkgdata_SCRIPTS = fcntl.scm

pkgdata_HEADERS = gauche/fcntl.h

fcntl_head.c fcntl_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension fcntl
