LIBOBJS = spew.o mem.o

test: test.o yeah.o $(LIBOBJS)
	gcc -g -o test test.o yeah.o $(LIBOBJS)

test.o: test.c yeah.h
	gcc -g -c test.c

test.i: test.h test.c yeah.h
	gcc -g -E test.c > test.i

yeah.o: yeah.h yeah.c
	gcc -g -c yeah.c -I../..

yeah.h yeah.c: yeah.ctors ../../ctor-gen
	../../ctor-gen yeah

spew.o: ../../spew.c ../../spew.h
	gcc -g -c ../../spew.c

mem.o: ../../mem.c ../../mem.h
	gcc -g -c ../../mem.c

clean:
	rm -f test *.o yeah.h yeah.c
