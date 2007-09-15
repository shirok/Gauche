PROJDIRS = gosh/ libgauche/ gauche-config/ test-arith/ test-vmstack/ ext-*/

all :

clean :
	rm -rf gauche/ debug/ release/ *~ *.ncb *.suo *.vcproj.* test.log $(PROJDIRS)

check : debug/gosh.exe
	for t in `cat ../test/TESTS`; do \
	  (cd debug; ./gosh -ftest -I. -I../../test $$t); \
	done > test.log

