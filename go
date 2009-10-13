rm -f hoop.c hoop hoop.o
mzscheme -r main.ss > hoop.c
indent -i2 < hoop.c > _$$
mv _$$ hoop.c
#cat hoop.c
gcc -c hoop.c
gcc -c blip.c yeah.c spew.c mem.c
gcc -o hoop hoop.o yeah.o spew.o mem.o blip.o
./hoop

#rm -f vm vm.i
#make vm.i
#cat vm.i | tail -50
#make vm
#./vm
