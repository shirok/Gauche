include ../Makefile.ext

pkghdrdir  = $(GAUCHE_HDRDIR)/gauche
pkgdatadir = $(GAUCHE_LIBDIR)/gauche
pkglibdir  = $(GAUCHE_ARCHDIR)

pkglib_LTLIBRARIES = threads.la

threads_la_SOURCES = threads_head.c threads.c mutex.c thrlib.c threads_tail.c
threads_la_LDFLAGS = -no-undefined -module -avoid-version
threads_la_LIBADD  = $(top_builddir)/src/libgauche.la

GENERATED = Makefile thrlib.c threads_head.c threads_tail.c

pkgdata_SCRIPTS = threads.scm

threads_head.c threads_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension threads
