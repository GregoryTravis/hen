hen src.ss
#rigg hoot
#gcc -c hoot.c.defs.c
exit

hen src.ss
exit

rm -f vertex_and_fragment_program.impl.*
#g++ -c -fdump-translation-unit vertex_and_fragment_program.c
rigg vertex_and_fragment_program
g++ -c vertex_and_fragment_program.impl.c -I/Developer/SDKs/MacOSX10.5.sdk/usr/X11/include -I/Library/Frameworks/Cg.framework/Versions/1.0/Headers/ -I/Developer/SDKs/MacOSX10.5.sdk/System/Library/Frameworks/GLUT.framework/Versions/A/Headers/ #2>&1 | head -20
exit

#g++ -c vertex_and_fragment_program.impl.c -framework OpenGL
#gcc -c vertex_and_fragment_program.impl.c -framework GLUT -framework OpenGL -framework Cg -I/Developer/SDKs/MacOSX10.5.sdk/usr/X11/include -I/Library/Frameworks/Cg.framework/Versions/1.0/Headers/ 2>&1 | head -15

g++ -c -fdump-translation-unit butt.c

hen src.ss
exit

rm -f butt.c.impl* butt.c.stub*
rm -f butt.c.t00.tu
g++ -c -fdump-translation-unit butt.c
rigg butt.c
gcc -c butt_c.impl.c
ls -l butt_c.impl.*
exit

./hum 2> errs
cat errs | grep -v "Unparsed data" | grep -v binfo | grep -v "at /Library/Perl"
exit

hen src.ss
