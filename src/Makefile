SCM = snow
CC = cc
CFLAGS = -g -I. -I../gc
LDFLAGS = -lm
AR = ar

OBJS = core.o vm.o compile.o class.o boolean.o string.o list.o port.o \
       hash.o write.o read.o vector.o symbol.o module.o proc.o number.o \
       load.o stdlib.o 


HDRS = gauche.h

all : gosh

gosh : libgauche.a main.o
	$(CC) $(CFLAGS) -o gosh main.o -L. -lgauche ../gc/gc.a $(LDFLAGS)

stdlib.c : stdlib.stub genstub
	$(SCM) genstub stdlib.stub

$(OBJS) main.o : $(HDRS)

libgauche.a : $(OBJS)
	$(AR) r libgauche.a $(OBJS)

clean :
	rm -f core gosh *.o *~ *.a stdlib.c gauche/*~

