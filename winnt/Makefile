PROJDIRS = gosh/ libgauche/ gauche-config/ test-arith/ test-vmstack/ ext-*/

all :

clean :
	rm -rf debug/ release/ *~ *.ncb *.suo *.vcproj.* test.log $(PROJDIRS)

check : debug/gosh.exe
	for t in `cat ../test/TESTS`; do \
	  debug/gosh -ftest ../test/$$t; \
	done > test.log

