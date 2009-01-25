rigg vertex_and_fragment_program.c.t00.tu #| head -2000
gcc -c garf.impl.c
exit

./hum 2> errs
cat errs | grep -v "Unparsed data" | grep -v binfo | grep -v "at /Library/Perl"
exit

hen src.ss
