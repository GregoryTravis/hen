# rigg shew
# exit


# clean
# rigg ref
# hen src.ss
# exit

rm -f src src.c
hen src.ss
./src < input
exit

rm -f speed-test
hen speed-test.ss
time ./speed-test
exit

rm -f hoop
make hoop
./hoop
