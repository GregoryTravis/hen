libs=-I/Developer/SDKs/MacOSX10.4u.sdk/System/Library/Frameworks/GLUT.framework/Versions/A/Headers

all: vor fbo

clean:
	rm -f *.o fbo vor

vor.o: vor.c a.h spew.h mem.h
	gcc -std=c99 -c -g vor.c $(libs)

primcalls.o: primcalls.c vor.h a.h spew.h mem.h
	gcc -std=c99 -c -g primcalls.c $(libs)

spew.o: spew.c spew.h a.h
	gcc -std=c99 -c -g spew.c

ref.impl.o: ref.impl.c ref.impl.h vor.h
	gcc -std=c99 -c -g ref.impl.c

shew.impl.o: shew.impl.c shew.impl.h vor.h
	gcc -std=c99 -c -g shew.impl.c

fbo.impl.o: fbo.impl.c fbo.impl.h vor.h
	gcc -std=c99 -c -g fbo.impl.c $(libs)

fakey.impl.o: fakey.impl.c fakey.impl.h vor.h
	gcc -std=c99 -c -g fakey.impl.c $(libs)

mem.o: mem.c mem.h a.h
	gcc -std=c99 -c -g mem.c

fbo.o: fbo.c
	gcc -g -c fbo.c $(libs)

# GLee.o: GLee.c GLee.h
# 	gcc -g -c GLee.c $(libs)

# fbo: fbo.o GLee.o
# 	gcc -o fbo -g fbo.o GLee.o -framework GLUT -framework OpenGL -framework CoreFoundation

vertex_and_fragment_program.o: vertex_and_fragment_program.c
	gcc -Wall   -c -o vertex_and_fragment_program.o vertex_and_fragment_program.c
