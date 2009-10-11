mzscheme -r main.ss > hoop.c
indent -i2 < hoop.c > _$$
mv _$$ hoop.c
cat hoop.c
#gcc -c hoop.c

#rm -f vm vm.i
#make vm.i
#cat vm.i | tail -50
#make vm
#./vm
