include ../Makefile.ext

pkglibdir  = $(GAUCHE_ARCHDIR)
pkgdatadir = $(GAUCHE_LIBDIR)/math

pkglib_LTLIBRARIES = mt-random.la

mt_random_la_SOURCES = mt-random_head.c mt-random.c mt-lib.c mt-random_tail.c
mt_random_la_LDFLAGS = -no-undefined -module -avoid-version
mt_random_la_LIBADD  = $(top_builddir)/src/libgauche.la ../uvector/libgauche-uvector.la

GENERATED = Makefile mt-lib.c mt-random_head.c mt-random_tail.c

INCLUDES += -I../uvector

pkgdata_SCRIPTS = mt-random.scm

mt-random_head.c mt-random_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension mt-random mtrandom
