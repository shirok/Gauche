include ../Makefile.ext

pkghdrdir  = $(GAUCHE_HDRDIR)/gauche
pkgdatadir = $(GAUCHE_LIBDIR)/gauche
pkglibdir  = $(GAUCHE_ARCHDIR)

pkglib_LTLIBRARIES = termios.la

termios_la_SOURCES = termios_head.c termios.c termiolib.c termios_tail.c
termios_la_LDFLAGS = -no-undefined -module -avoid-version
termios_la_LIBADD  = $(top_builddir)/src/libgauche.la

GENERATED = Makefile termiolib.c termios_head.c termios_tail.c

pkgdata_SCRIPTS = termios.scm

pkgdata_HEADERS = gauche/termios.h

termios_head.c termios_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension termios
