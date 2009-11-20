clean
rm -f src src.c
hen src.ss
./src
exit

rm -f speed-test
hen speed-test.ss
time ./speed-test
exit

rm -f hoop
make hoop
./hoop
