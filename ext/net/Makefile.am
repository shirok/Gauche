include ../Makefile.ext

pkghdrdir  = $(GAUCHE_HDRDIR)/gauche
pkgdatadir = $(GAUCHE_LIBDIR)/gauche
pkglibdir  = $(GAUCHE_ARCHDIR)

pkglib_LTLIBRARIES = libnet.la

libnet_la_SOURCES = net_head.c net.c addr.c netdb.c netlib.c net_tail.c
libnet_la_LDFLAGS = -no-undefined -module -avoid-version
libnet_la_LIBADD  = $(top_builddir)/src/libgauche.la

GENERATED = Makefile netlib.c net_head.c net_tail.c

pkgdata_SCRIPTS = net.scm

pkghdr_HEADERS = net.h

net_head.c net_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension net
