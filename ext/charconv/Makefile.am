include ../Makefile.ext

SCM_CATEGORY = gauche

pkgarch_LTLIBRARIES = libcharconv.la
pkgdata_DATA        = charconv.scm

libcharconv_la_SOURCES = charconv_head.c charconv.c convlib.c jconv.c \
                         guess.c charconv_tail.c
libcharconv_la_LDFLAGS = -no-undefined -module -avoid-version
libcharconv_la_LIBADD  = $(top_builddir)/src/libgauche.la

GENERATED = Makefile charconv.h convlib.c charconv_head.c charconv_tail.c


jconv.lo : eucj2ucs.c ucs2eucj.c jconv.c

guess.lo : guess_tab.c guess.c

eucj2ucs.c ucs2eucj.c : cvt.scm euc-jp-2000-std.txt
	$(GOSH) cvt.scm

guess_tab.c : guess.scm
	$(GOSH) guess.scm guess_tab.c

charconv_head.c charconv_tail.c :
	$(GAUCHE_CONFIG) --fixup-extension charconv

