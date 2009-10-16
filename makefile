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

hoop.o: hoop.c yeah.h
	gcc -g -c hoop.c

blip.o: blip.c blip.h yeah.h
	gcc -g -c blip.c

yeahlib.o: yeahlib.c yeahlib.h yeah.h
	gcc -g -c yeahlib.c

hoop.c: h.ss src.ss
	@rm -f hoop.c
	mzscheme -r main.ss
#	@indent -i2 < hoop.c > __indent_tmp
#	@mv __indent_tmp hoop.c

hoop: hoop.o yeah.o spew.o mem.o blip.o yeahlib.o
	gcc -o hoop hoop.o yeah.o spew.o mem.o blip.o yeahlib.o

clean:
	rm -f vm *.o yeah.h yeah.c hoop.o hoop.c hoop
