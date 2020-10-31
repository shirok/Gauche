SUBDIRS = scoop tls

all:

check:

clean:
	@for d in $(SUBDIRS); do (cd $$d; $(MAKE) clean); done

distclean: clean
	@for d in $(SUBDIRS); do (cd $$d; $(MAKE) distclean); done

maintainer-clean: clean
	@for d in $(SUBDIRS); do (cd $$d; $(MAKE) maintainer-clean); done
	rm -f Makefile
