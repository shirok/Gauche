include ../Makefile.ext

SCM_CATEGORY = rfc

pkgarch_LTLIBRARIES = md5.la sha1.la
pkgdata_DATA        = md5.scm sha1.scm

md5_la_SOURCES = md5_head.c md5.c md5c.c md5_tail.c
md5_la_LDFLAGS = -no-undefined -module -avoid-version
md5_la_LIBADD  = $(top_builddir)/src/libgauche.la

sha1_la_SOURCES = sha1_head.c sha1.c sha.c sha1_tail.c
sha1_la_LDFLAGS = -no-undefined -module -avoid-version
sha1_la_LIBADD  = $(top_builddir)/src/libgauche.la

GENERATED = Makefile md5.c sha1.c *_head.c *_tail.c

md5_head.c md5_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension md5

sha1_head.c sha1_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension sha1
