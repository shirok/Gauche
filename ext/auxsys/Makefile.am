include ../Makefile.ext

SCM_CATEGORY = gauche

pkgarch_LTLIBRARIES = auxsys.la
pkgdata_DATA        = auxsys.scm

auxsys_la_SOURCES = auxsys_head.c auxsys.c auxsyslib.c auxsys_tail.c
auxsys_la_LDFLAGS = -no-undefined -module -avoid-version
auxsys_la_LIBADD  = $(top_builddir)/src/libgauche.la

GENERATED = Makefile auxsyslib.c auxsys_head.c auxsys_tail.c

auxsys_head.c auxsys_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension auxsys
