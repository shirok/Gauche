
# the following line is examined by configure.  keep "^SUBDIRS=" pattern.
SUBDIRS= example auxsys threads uvector charconv net termios fcntl dbm mt-random

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

link :
	@for dir in $(SUBDIRS); do (cd $$dir; $(MAKE) link); done

unlink :
	@if [ -x ../src/gosh ]; then for dir in $(SUBDIRS); do (cd $$dir; $(MAKE) unlink); done; fi



