include ../Makefile.ext

pkghdrdir  = $(GAUCHE_HDRDIR)/gauche
pkgdatadir = $(GAUCHE_LIBDIR)/gauche
pkglibdir  = $(GAUCHE_ARCHDIR)

pkglib_LTLIBRARIES = syslog.la

syslog_la_SOURCES = syslog_head.c syslog.c syslog_tail.c
syslog_la_LDFLAGS = -no-undefined -module -avoid-version
syslog_la_LIBADD  = $(top_builddir)/src/libgauche.la

GENERATED = Makefile syslog.c syslog_head.c syslog_tail.c

pkgdata_SCRIPTS = syslog.scm

syslog_head.c syslog_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension syslog
