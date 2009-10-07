LIBOBJS = spew.o mem.o

vm: vm.o yeah.o $(LIBOBJS)
	gcc -g -o vm vm.o yeah.o $(LIBOBJS)

vm.o: vm.h vm.c yeah.h
	gcc -g -c vm.c

vm.i: vm.h vm.c yeah.h
	gcc -g -E vm.c > vm.i

yeah.o: yeah.h yeah.c ctor-support.c
	gcc -g -c yeah.c

yeah.h yeah.c: yeah.ctors ctor-gen
	ctor-gen yeah

spew.o: spew.c spew.h
	gcc -g -c spew.c

mem.o: mem.c mem.h
	gcc -g -c mem.c

clean:
	rm -f vm *.o yeah.h yeah.c
