all: vor fbo

clean:
	rm -f *.o fbo vor

vor: vor.o spew.o mem.o obj.i fbo.o GLee.o
	g++ -std=c99 -g -o vor vor.o spew.o mem.o fbo.o GLee.o -framework GLUT -framework OpenGL -framework CoreFoundation

vor.o: vor.c a.h spew.h mem.h obj.i
	gcc -std=c99 -c -g vor.c

spew.o: spew.c spew.h a.h
	gcc -std=c99 -c -g spew.c

mem.o: mem.c mem.h a.h
	gcc -std=c99 -c -g mem.c

fbo.o: fbo.c
	gcc -g -c fbo.c -I/Developer/SDKs/MacOSX10.4u.sdk/System/Library/Frameworks/GLUT.framework/Versions/A/Headers

GLee.o: GLee.c GLee.h
	gcc -g -c GLee.c  -I/Developer/SDKs/MacOSX10.4u.sdk/System/Library/Frameworks/GLUT.framework/Versions/A/Headers

fbo: fbo.o GLee.o
	gcc -o fbo -g fbo.o GLee.o -framework GLUT -framework OpenGL -framework CoreFoundation
