SUBDIRS = . gc lib libltdl src ext doc

install-data-local: acinclude.m4
	$(INSTALL_DATA) acinclude.m4 $(DESTDIR)/$(datadir)/aclocal/gauche.m4

CLEANFILES += core *~

DISTCLEANFILES += autom4*

MAINTAINERCLEANFILES += INSTALL INSTALL.eucjp aclocal.m4 VERSION \
	configure config.guess config.sub missing mkinstalldirs ltmain.sh \
	libltdl Makefile.in DIST_EXCLUDE_X

## override these rules to clean up the subdirectories.
distclean-generic:
	-rm -f Makefile $(CONFIG_CLEAN_FILES)
	-rm -f config.cache config.log stamp-h stamp-h[0-9]*
	-test -z "$(DISTCLEANFILES)" || rm -rf $(DISTCLEANFILES)

maintainer-clean-generic:
	-test -z "$(MAINTAINERCLEANFILES)" || rm -rf $(MAINTAINERCLEANFILES)

## for backward compatibility from pre-automake age of Gauche
test : check
