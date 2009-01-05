// FBO Example
//
// A demonstration of using an FBO in your own programs.
// Uses GLee for extension setup
// Note: This isn't meant to indicate good programming style, just act as an example
//

#include <stdio.h>
#include <stdlib.h>

#include "glee.h"
#include <glut.h>

void ShutDown(GLvoid)
{
// TODO
fprintf(stderr, "Can't really shut down!\n");
exit(1);
/*
        glDeleteFramebuffersEXT(1, &fbo);
        glDeleteRenderbuffersEXT(1, &depthBuffer);
        glDeleteTextures(1,&img);
*/
}

void fbo_main0() {
  int dummy_argc = 0;
  char* dummy_argv[1];
  
  glutInit(&dummy_argc, (char**)&dummy_argv);
}

void fbo_main1() {
  // HEN
  //glutInitDisplayMode ( GLUT_RGB | GLUT_DOUBLE );         // Display Mode
  //glutInitWindowSize(800,600);
  //glutCreateWindow( "FrameBuffer Object Example - Press ESC to exit" );
  // GLee is used to setup all the extensions available
/*   if(!GLeeInit()) */
/*     exit(2); */
//        init();
        
        // Setup the various call back functions GLUT requires
//        glutDisplayFunc     ( display );  
//        glutReshapeFunc     ( reshape );
//        glutKeyboardFunc    ( keyboard );
//        glutIdleFunc            ( idle );

        // HEY
        //glutMainLoop        ( );                        // Run the main GLUT loop for rendering
}
