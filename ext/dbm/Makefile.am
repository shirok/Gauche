include ../Makefile.ext

INSTALL_TYPE = sys

pkglibdir  = $(GAUCHE_ARCHDIR)
pkgdatadir = $(GAUCHE_LIBDIR)/dbm

## NB: automake doesn't like substituted variable in pkglib_LTLIBRARIES;
## it requires EXTRA_LTLIBRARIES defined in such a case; however, if I do so,
## I need to add -rpath manually in _LDFLAGS to convince libtool that
## I'm creating dlopen-able library.

pkglib_LTLIBRARIES = @DBM_ARCHFILES@

EXTRA_LTLIBRARIES = gdbm.la ndbm.la odbm.la

gdbm_la_SOURCES   = gdbm_head.c gdbm.c gdbm_tail.c
gdbm_la_LDFLAGS   = -no-undefined -module -avoid-version -rpath $(pkglibdir)
gdbm_la_LIBADD    = $(top_builddir)/src/libgauche.la @GDBMLIB@

ndbm_la_SOURCES   = ndbm_head.c ndbm.c ndbm_tail.c
ndbm_la_LDFLAGS   = -no-undefined -module -avoid-version -rpath $(pkglibdir)
ndbm_la_LIBADD    = $(top_builddir)/src/libgauche.la @NDBMLIB@

odbm_la_SOURCES   = odbm_head.c odbm.c odbm_tail.c
odbm_la_LDFLAGS   = -no-undefined -module -avoid-version -rpath $(pkglibdir)
odbm_la_LIBADD    = $(top_builddir)/src/libgauche.la @ODBMLIB@

GENERATED = Makefile gdbm.c ndbm.c odbm.c *_head.c *_tail.c

pkgdata_SCRIPTS = @DBM_SCMFILES@

EXTRA_SCRIPTS = gdbm.scm ndbm.scm odbm.scm

gdbm_head.c gdbm_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension gdbm

ndbm_head.c ndbm_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension ndbm

odbm_head.c odbm_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension odbm

