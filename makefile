all: vor fbo

clean:
	rm -f *.o fbo vor

vor: vor.c a.c spew.c mem.c obj.i
	gcc -std=c99 -g -o vor vor.c a.c spew.c mem.c

fbo.o: fbo.cc
	g++ -g -c fbo.cc -I/Developer/SDKs/MacOSX10.4u.sdk/System/Library/Frameworks/GLUT.framework/Versions/A/Headers

GLee.o: GLee.c GLee.h
	gcc -g -c GLee.c  -I/Developer/SDKs/MacOSX10.4u.sdk/System/Library/Frameworks/GLUT.framework/Versions/A/Headers

fbo: fbo.o GLee.o
	g++ -o fbo -g fbo.o GLee.o -framework GLUT -framework OpenGL -framework CoreFoundation
