#chez --script mtch.ss
#exit

# rm -f fbo fbo.c
# hen fbo.ss
# ./fbo
# exit

#rm joe.blick *stub* *impl*
#rigg joe "-I/Developer/SDKs/MacOSX10.5.sdk/usr/X11/include/"
#exit

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
