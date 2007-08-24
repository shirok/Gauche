PROJECTS = gosh libgauche test-arith test-vmstack

all :

clean :
	rm -rf gauche *~ *.ncb *.suo *.vcproj.* $(PROJECTS)

check : debug/gosh.exe
	for t in `cat ../test/TESTS`; do \
	  debug/gosh -ftest ../test/$$t; \
	done > test.log

