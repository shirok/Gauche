include ../Makefile.ext

INSTALL_TYPE = sys

pkglibdir  = $(GAUCHE_ARCHDIR)
pkgdatadir = $(GAUCHE_LIBDIR)/gauche

pkglib_LTLIBRARIES = auxsys.la

auxsys_la_SOURCES = auxsys_head.c auxsys.c auxsyslib.c auxsys_tail.c
auxsys_la_LDFLAGS = -no-undefined -module -avoid-version
auxsys_la_LIBADD  = $(top_builddir)/src/libgauche.la

GENERATED = Makefile auxsyslib.c auxsys_head.c auxsys_tail.c

pkgdata_SCRIPTS = auxsys.scm

CLEANFILES = core *~ test.log

DISTCLEANFILES = $(GENERATED)

MAINTAINERCLEANFILES = Makefile.in $(GENERATED)

auxsys_head.c auxsys_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension auxsys

check :
	@rm -f test.log
	$(GOSH) -I. test.scm > test.log

