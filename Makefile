
all : gc/libgc.a src/gosh

gc/libgc.a :
	cd gc; make

src/gosh : gc/libgc.a
	cd src; make

clean :
	cd gc; make clean
	cd src; make clean
