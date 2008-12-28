// FBO Example
//
// A demonstration of using an FBO in your own programs.
// Uses GLee for extension setup
// Note: This isn't meant to indicate good programming style, just act as an example
//

#include <stdlib.h>

#include "glee.h"
#include <glut.h>

GLuint fbo;                                     // Our handle to the FBO
GLuint depthBuffer;                     // Our handle to the depth render buffer
GLuint img;                                     // Our handle to a texture

const int width = 512;          // The hight of the texture we'll be rendering to
const int height = 512;         // The width of the texture we'll be rendering to

// Used for drawing the 3D cube with our rendered texture on it
GLfloat xrot = 0;                       // X Rotation
GLfloat yrot = 0;                       // Y Rotation
GLfloat xspeed = 0.2f;          // X Rotation Speed
GLfloat yspeed = 0.1f;          // Y Rotation Speed

void init(GLvoid)     
{

        glShadeModel(GL_SMOOTH);
        glClearColor(0.0f, 0.0f, 0.2f, 0.5f);
        glClearDepth(1.0f);                                     
        glEnable(GL_DEPTH_TEST);                        
        glDepthFunc(GL_LEQUAL);                         
        glViewport(0,0,800,600);

        // Setup our FBO
        glGenFramebuffersEXT(1, &fbo);
        glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fbo);

        // Create the render buffer for depth   
        glGenRenderbuffersEXT(1, &depthBuffer);
        glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, depthBuffer);
        glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT, width, height);

        // Now setup a texture to render to
        glGenTextures(1, &img);
        glBindTexture(GL_TEXTURE_2D, img);
                glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8,  width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
                glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
                glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
                glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
                glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

//  The following 3 lines enable mipmap filtering and generate the mipmap data so rendering works
//      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
//      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
//      glGenerateMipmapEXT(GL_TEXTURE_2D);

        // And attach it to the FBO so we can render to it
        glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, img, 0);

        // Attach the depth render buffer to the FBO as it's depth attachment
        glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, depthBuffer);
        

        GLenum status = glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
        if(status != GL_FRAMEBUFFER_COMPLETE_EXT)
                exit(1);
        
        glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);    // Unbind the FBO for now
}

void ShutDown(GLvoid)
{
        glDeleteFramebuffersEXT(1, &fbo);
        glDeleteRenderbuffersEXT(1, &depthBuffer);
        glDeleteTextures(1,&img);
}

void reshape(int w,int h)                       
{
        glViewport( 0, 0, w, h );
        glMatrixMode(GL_PROJECTION);    
        glLoadIdentity();                                       
        if ( h==0 )                                                     
                gluPerspective(80,(float)w,1.0,5000.0);
        else
                gluPerspective(80,(float)w/(float)h,1.0,5000.0);
        glMatrixMode(GL_MODELVIEW);     
        glLoadIdentity();                                       
}

void keyboard(unsigned char key,int x,int y)  
{
        switch(key)
        {
        case 27:                                // When Escape Is Pressed...
                ShutDown();
                exit(0);                        // Exit The Program
                break;                          
        default:                                
                break;
        }
}

void idle(void)
{
        glutPostRedisplay();
}

void display(void)   
{
        // First we bind the FBO so we can render to it
        glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fbo);
        
        // Save the view port and set it to the size of the texture
        glPushAttrib(GL_VIEWPORT_BIT);
        glViewport(0,0,width,height);

        // Then render as normal
        // Today's scene is a wonderful multi-coloured spinning cube ;)
        glClearColor(0.0f, 0.0f, 0.0f, 0.5f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);     // Clear Screen And Depth Buffer
        glLoadIdentity();

        glTranslatef(0.0f,0.0f,-2.0f);
        glRotatef(xrot,1.0f,0.0f,0.0f);
        glRotatef(yrot,0.0f,1.0f,0.0f);

        glBegin(GL_QUADS);
                // Front Face
                glColor4f(0.0f,1.0f,0.0f,1.0f);
                glVertex3f(-0.5f, -0.5,  0.5);
                glVertex3f( 0.5, -0.5,  0.5);
                glVertex3f( 0.5,  0.5,  0.5);
                glVertex3f(-0.5,  0.5,  0.5);
                // Back Face
                glColor4f(1.0f,0.0f,0.0f,1.0f);
                glVertex3f(-0.5, -0.5, -0.5);
                glVertex3f(-0.5,  0.5, -0.5);
                glVertex3f( 0.5,  0.5, -0.5);
                glVertex3f( 0.5, -0.5, -0.5);
                // Top Face
                glColor4f(0.0f,0.0f,1.0f,1.0f);
                glVertex3f(-0.5,  0.5, -0.5);
                glVertex3f(-0.5,  0.5,  0.5);
                glVertex3f( 0.5,  0.5,  0.5);
                glVertex3f( 0.5,  0.5, -0.5);
                // Bottom Face
                glColor4f(0.0f,1.0f,1.0f,1.0f);
                glVertex3f(-0.5, -0.5, -0.5);
                glVertex3f( 0.5, -0.5, -0.5);
                glVertex3f( 0.5, -0.5,  0.5);
                glVertex3f(-0.5, -0.5,  0.5);
                // Right face
                glColor4f(1.0f,1.0f,0.0f,1.0f);
                glVertex3f( 0.5, -0.5, -0.5);
                glVertex3f( 0.5,  0.5, -0.5);
                glVertex3f( 0.5,  0.5,  0.5);
                glVertex3f( 0.5, -0.5,  0.5);
                // Left Face
                glColor4f(1.0f,1.0f,1.0f,1.0f);
                glVertex3f(-0.5, -0.5, -0.5);
                glVertex3f(-0.5, -0.5,  0.5);
                glVertex3f(-0.5,  0.5,  0.5);
                glVertex3f(-0.5,  0.5, -0.5);
        glEnd();

        // Restore old view port and set rendering back to default frame buffer
        glPopAttrib();
        glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);

        glClearColor(0.0f, 0.0f, 0.2f, 0.5f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);     // Clear Screen And Depth Buffer
        glLoadIdentity();
        // Now bind the texture to use it
        glBindTexture(GL_TEXTURE_2D, img);
//      If you enabled the mipmap filtering on setup earlier then you'll need to uncomment the line
//      below so OpenGL can generate all the mipmap data for the new main image each frame
//      glGenerateMipmapEXT(GL_TEXTURE_2D);
        glEnable(GL_TEXTURE_2D);

        glTranslatef(0.0f,0.0f,-2.0f);
        glRotatef(-xrot,1.0f,0.0f,0.0f);
        glRotatef(-yrot,0.0f,1.0f,0.0f);

        glColor4f(1.0f,1.0f,1.0f,1.0f);

        // This time it's a textured spinning cube!
        // The texture being the scene we just rendered!
        glBegin(GL_QUADS);
                // Front Face
                glNormal3f( 0.0f, 0.0f, 1.0);
                glTexCoord2f(0.0f, 1.0f); glVertex3f(-0.5f, -0.5,  0.5);
                glTexCoord2f(1.0f, 1.0f); glVertex3f( 0.5, -0.5,  0.5);
                glTexCoord2f(1.0f, 0.0f); glVertex3f( 0.5,  0.5,  0.5);
                glTexCoord2f(0.0f, 0.0f); glVertex3f(-0.5,  0.5,  0.5);
                // Back Face
                glNormal3f( 0.0f, 0.0f,-1.0);
                glTexCoord2f(1.0f, 0.0f); glVertex3f(-0.5, -0.5, -0.5);
                glTexCoord2f(1.0f, 1.0f); glVertex3f(-0.5,  0.5, -0.5);
                glTexCoord2f(0.0f, 1.0f); glVertex3f( 0.5,  0.5, -0.5);
                glTexCoord2f(0.0f, 0.0f); glVertex3f( 0.5, -0.5, -0.5);
                // Top Face
                glNormal3f( 0.0f, 1.0, 0.0f);
                glTexCoord2f(0.0f, 1.0f); glVertex3f(-0.5,  0.5, -0.5);
                glTexCoord2f(0.0f, 0.0f); glVertex3f(-0.5,  0.5,  0.5);
                glTexCoord2f(1.0f, 0.0f); glVertex3f( 0.5,  0.5,  0.5);
                glTexCoord2f(1.0f, 1.0f); glVertex3f( 0.5,  0.5, -0.5);
                // Bottom Face
                glNormal3f( 0.0f,-1.0, 0.0f);
                glTexCoord2f(1.0f, 1.0f); glVertex3f(-0.5, -0.5, -0.5);
                glTexCoord2f(0.0f, 1.0f); glVertex3f( 0.5, -0.5, -0.5);
                glTexCoord2f(0.0f, 0.0f); glVertex3f( 0.5, -0.5,  0.5);
                glTexCoord2f(1.0f, 0.0f); glVertex3f(-0.5, -0.5,  0.5);
                // Right face
                glNormal3f( 1.0, 0.0f, 0.0f);
                glTexCoord2f(1.0f, 0.0f); glVertex3f( 0.5, -0.5, -0.5);
                glTexCoord2f(1.0f, 1.0f); glVertex3f( 0.5,  0.5, -0.5);
                glTexCoord2f(0.0f, 1.0f); glVertex3f( 0.5,  0.5,  0.5);
                glTexCoord2f(0.0f, 0.0f); glVertex3f( 0.5, -0.5,  0.5);
                // Left Face
                glNormal3f(-1.0, 0.0f, 0.0f);
                glTexCoord2f(0.0f, 0.0f); glVertex3f(-0.5, -0.5, -0.5);
                glTexCoord2f(1.0f, 0.0f); glVertex3f(-0.5, -0.5,  0.5);
                glTexCoord2f(1.0f, 1.0f); glVertex3f(-0.5,  0.5,  0.5);
                glTexCoord2f(0.0f, 1.0f); glVertex3f(-0.5,  0.5, -0.5);
        glEnd();

        glDisable(GL_TEXTURE_2D);
        
        xrot+=xspeed;
        yrot+=yspeed;

        glutSwapBuffers ( );
        // Swap The Buffers To Not Be Left With A Clear Screen
}

void fbo_main0() {
  int dummy_argc = 0;
  char* dummy_argv[1];
  
  glutInit(&dummy_argc, (char**)&dummy_argv);
}

void fbo_main1() {
        glutInitDisplayMode ( GLUT_RGB | GLUT_DOUBLE );         // Display Mode
        glutInitWindowSize(800,600);
        glutCreateWindow( "FrameBuffer Object Example - Press ESC to exit" );

        // GLee is used to setup all the extensions available
        if(!GLeeInit())
                exit(2);

        init();
        
        // Setup the various call back functions GLUT requires
        glutDisplayFunc     ( display );  
        glutReshapeFunc     ( reshape );
        glutKeyboardFunc    ( keyboard );
        glutIdleFunc            ( idle );
        glutMainLoop        ( );                        // Run the main GLUT loop for rendering
}

void fbo_main() {
  fbo_main0();
  fbo_main1();
}
