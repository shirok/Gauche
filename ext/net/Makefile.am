include ../Makefile.ext

SCM_CATEGORY = gauche

pkgarch_LTLIBRARIES = libnet.la
pkgdata_DATA        = net.scm

libnet_la_SOURCES = net_head.c net.c addr.c netdb.c netlib.c net_tail.c
libnet_la_LDFLAGS = -no-undefined -module -avoid-version
libnet_la_LIBADD  = $(top_builddir)/src/libgauche.la

GENERATED = Makefile netlib.c net_head.c net_tail.c

net_head.c net_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension net
