SCM = snow
CC = cc
#CFLAGS = -g -I. -I../gc
CFLAGS = -O2 -I. -I../gc
LDFLAGS = -lm
AR = ar

GENERATED = stdlib.c extlib.c

OBJS = core.o vm.o compile.o inline.o class.o boolean.o string.o list.o \
       port.o hash.o write.o read.o vector.o symbol.o module.o proc.o \
       number.o io.o load.o promise.o stdlib.o extlib.o

.SUFFIXES: .stub

.stub.c:
	$(SCM) genstub $<

HDRS = gauche.h

all : gosh

gosh : libgauche.a main.o
	$(CC) $(CFLAGS) -o gosh main.o -L. -lgauche ../gc/gc.a $(LDFLAGS)

$(OBJS) main.o : $(HDRS)

stdlib.c : stdlib.stub
extlib.c : extlib.stub

libgauche.a : $(OBJS)
	$(AR) r libgauche.a $(OBJS)

clean :
	rm -f core gosh *.o *~ *.a $(GENERATED) gauche/*~

