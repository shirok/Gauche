include ../Makefile.ext

SCM_CATEGORY = gauche

SCM_INSTHDRS  = gauche/uvector.h

pkgarch_LTLIBRARIES = libgauche-uvector.la uvector.la
pkgdata_DATA        = array.scm uvector.scm

libgauche_uvector_la_SOURCES = uvector.c uvutil.c
libgauche_uvector_la_LIBADD  = $(top_builddir)/src/libgauche.la

uvector_la_SOURCES = uvector_head.c uvinit.c uvlib.c uvector_tail.c
uvector_la_LDFLAGS = -no-undefined -module -avoid-version
uvector_la_LIBADD  = $(top_builddir)/src/libgauche.la $(srcdir)/libgauche-uvector.la

GENERATED = Makefile uvlib.c uvlib.stub uvector.c gauche/uvector.h \
            uvutil.c uvector_head.c uvector_tail.c

uvector_head.c uvector_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension uvector

gauche/uvector.h : uvector.h.sh
	if test ! -d gauche; then mkdir gauche; fi
	rm -rf gauche/uvector.h
	sh ./uvector.h.sh > gauche/uvector.h

uvector.lo uvutil.lo uvlib.lo uvinit.lo : gauche/uvector.h

uvector.c : uvector.c.sh
	rm -rf uvector.c
	sh ./uvector.c.sh > uvector.c

uvutil.c : uvutil.c.sh
	rm -rf uvutil.c
	sh ./uvutil.c.sh > uvutil.c

uvlib.c : uvlib.stub
	$(GOSH) genstub uvlib.stub

uvlib.stub : uvlib.stub.sh
	rm -rf uvlib.stub
	sh ./uvlib.stub.sh > uvlib.stub

