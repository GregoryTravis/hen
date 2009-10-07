#mzscheme -g -r main.ss
rm -f vm vm.i
make vm.i
cat vm.i | tail -50
make vm
./vm
