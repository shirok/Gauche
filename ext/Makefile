
# the following line is examined by configure.  keep "^SUBDIRS=" pattern.
SUBDIRS= example uvector charconv net termios fcntl dbm

all :
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE) all); done

test :
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE) test); done

install :
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE) install); done

clean :
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE) clean); done

distclean :
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE) distclean); done

realclean :
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE) realclean); done


