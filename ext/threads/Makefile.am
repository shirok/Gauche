include ../Makefile.ext

SCM_CATEGORY = gauche

pkgarch_LTLIBRARIES = threads.la
pkgdata_DATA        = threads.scm

threads_la_SOURCES = threads_head.c threads.c mutex.c thrlib.c threads_tail.c
threads_la_LDFLAGS = -no-undefined -module -avoid-version
threads_la_LIBADD  = $(top_builddir)/src/libgauche.la

GENERATED = Makefile thrlib.c threads_head.c threads_tail.c

threads_head.c threads_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension threads
