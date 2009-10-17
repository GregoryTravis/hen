mzscheme -r main.ss
./src
exit

rm -f hoop
make hoop
./hoop
