include ../Makefile.ext

SCM_CATEGORY = gauche

pkgarch_LTLIBRARIES = termios.la
pkgdata_DATA        = termios.scm

termios_la_SOURCES = termios_head.c termios.c termiolib.c termios_tail.c
termios_la_LDFLAGS = -no-undefined -module -avoid-version
termios_la_LIBADD  = $(top_builddir)/src/libgauche.la

GENERATED = Makefile termiolib.c termios_head.c termios_tail.c

termios_head.c termios_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension termios
