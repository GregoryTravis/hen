rm -f src src.c
mzscheme -r main.ss
./src
exit

rm -f hoop
make hoop
./hoop
