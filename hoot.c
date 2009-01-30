
/* 02_vertex_and_fragment_program.c - OpenGL-based example using a Cg
   vertex and a Cg fragment programs from Chapter 2 of "The Cg Tutorial"
   (Addison-Wesley, ISBN 0321194969). */

/* Requires the OpenGL Utility Toolkit (GLUT) and Cg runtime (version
   1.0 or higher). */

#include <stdio.h>    /* for printf and NULL */
#include <stdlib.h>   /* for exit */
#include <math.h>     /* for sin and cos */
#if __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

#define JERK 20
