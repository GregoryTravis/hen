libs=-I/Developer/SDKs/MacOSX10.4u.sdk/System/Library/Frameworks/GLUT.framework/Versions/A/Headers

all: vor fbo

clean:
	rm -f *.o fbo vor

vor.o: vor.c a.h spew.h mem.h
	gcc -std=c99 -c -g vor.c $(libs)

spew.o: spew.c spew.h a.h
	gcc -std=c99 -c -g spew.c

mem.o: mem.c mem.h a.h
	gcc -std=c99 -c -g mem.c

fbo.o: fbo.c
	gcc -g -c fbo.c $(libs)

GLee.o: GLee.c GLee.h
	gcc -g -c GLee.c $(libs)

fbo: fbo.o GLee.o
	gcc -o fbo -g fbo.o GLee.o -framework GLUT -framework OpenGL -framework CoreFoundation
