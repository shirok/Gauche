include ../Makefile.ext

SCM_CATEGORY = gauche

pkgarch_LTLIBRARIES = syslog.la
pkgdata_DATA        = syslog.scm

syslog_la_SOURCES = syslog_head.c syslog.c syslog_tail.c
syslog_la_LDFLAGS = -no-undefined -module -avoid-version
syslog_la_LIBADD  = $(top_builddir)/src/libgauche.la

GENERATED = Makefile syslog.c syslog_head.c syslog_tail.c

syslog_head.c syslog_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension syslog
