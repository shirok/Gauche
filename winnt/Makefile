PROJDIRS = gosh/ libgauche/ gauche-config/ test-arith/ test-vmstack/ ext-*/

all :

clean :
	rm -rf gauche/ debug/ release/ *~ *.ncb *.suo *.vcproj.* test.log $(PROJDIRS)

check : debug/gosh.exe
	rm -f test.log
	-@for t in `cat ../test/TESTS`; do \
	  debug/gosh -ftest -Idebug -I../test $$t; \
	done > test.log
	-@for t in ../ext/*/test.scm; do \
	  (cd `dirname $$t`; ../../winnt/debug/gosh -ftest -I../../winnt/debug ./test.scm); \
	done >> test.log
