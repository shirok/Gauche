TEXISRCS  = gauche-ref.texi libgauche-ref.texi
EXTRACTED = gauche-refe.texi gauche-refj.texi
GENERATED = Makefile $(MANPAGES)
GOSH = ../src/gosh -q -I../src -I../lib -lgauche-init

## we can't use info_TEXINFOS, since the texi's are generated.
info_TEXINFOS =

INFO_DEPS += gauche-refe.info gauche-refj.info

## NB: AM_MAKEINFOFLAGS doesn't work on automake 1.4
MAKEINFO = @MAKEINFO@ --no-warn

man1_MANS = gosh.1 gauche-config.1

EXTRA_DIST = $(man1_MANS) $(TEXISRCS)

CLEANFILES += $(EXTRACTED) *~

DISTCLEANFILES += $(GENERATED)

MAINTAINERCLEANFILES += Makefile.in texinfo.tex

gauche-refe.texi : gauche-ref.texi extract
	$(GOSH) ./extract -en -o gauche-refe.texi gauche-ref.texi

gauche-refj.texi : gauche-ref.texi extract
	$(GOSH) ./extract -jp -o gauche-refj.texi gauche-ref.texi
