#define GLUT_RGB			0
#define GLUT_DOUBLE			2
#define GL_CLAMP_TO_EDGE 33071
#define GL_COLOR_BUFFER_BIT 16384
#define GL_DEPTH_COMPONENT 6402
#define GL_DEPTH_COMPONENT16 33189
#define GL_DEPTH_COMPONENT24 33190
#define GL_DEPTH_COMPONENT32 33191
#define GL_DEPTH_TEST 2929
#define GL_LEQUAL 515
#define GL_LINEAR_ATTENUATION 4616
#define GL_LINEAR 9729
#define GL_LINEAR_MIPMAP_NEAREST 9985
#define GL_LINEAR_MIPMAP_LINEAR 9987
#define GL_LINEAR_MIPMAP_LINEAR 9987
#define GL_MODELVIEW_STACK_DEPTH 2979
#define GL_MODELVIEW_MATRIX 2982
#define GL_MODELVIEW 5888
#define GL_PROJECTION_STACK_DEPTH 2980
#define GL_PROJECTION_MATRIX 2983
#define GL_PROJECTION 5889
#define GL_QUADS 7
#define GL_RGBA_MODE 3121
#define GL_RGBA 6408
#define GL_RGBA2 32853
#define GL_RGBA4 32854
#define GL_RGBA8 32856
#define GL_RGBA12 32858
#define GL_RGBA16 32859
#define GL_RGBA8 32856
#define GL_SMOOTH 7425
#define GL_SMOOTH_POINT_SIZE_RANGE 2834
#define GL_SMOOTH_POINT_SIZE_GRANULARITY 2835
#define GL_SMOOTH_LINE_WIDTH_RANGE 2850
#define GL_SMOOTH_LINE_WIDTH_GRANULARITY 2851
#define GL_TEXTURE_2D 3553
#define GL_TEXTURE_MAG_FILTER 10240
#define GL_TEXTURE_MIN_FILTER 10241
#define GL_TEXTURE_WRAP_S 10242
#define GL_TEXTURE_WRAP_T 10243
#define GL_UNSIGNED_BYTE 5121
#define GL_UNSIGNED_BYTE_3_3_2 32818
#define GL_UNSIGNED_BYTE_2_3_3_REV 33634
#define GL_VIEWPORT_BIT 2048

#define GL_FRAMEBUFFER_EXT 36160
#define GL_RENDERBUFFER_EXT 36161

#if 0
#define GL_CLAMP_TO_EDGE                  #x812F
#define GL_COLOR_ATTACHMENT0_EXT           #x8CE0
#define GL_COLOR_BUFFER_BIT               #x00004000
#define GL_DEPTH_ATTACHMENT_EXT            #x8D00
#define GL_DEPTH_BUFFER_BIT               #x00000100
#define GL_DEPTH_COMPONENT                #x1902
#define GL_DEPTH_TEST                     #x0B71
#define GL_FRAMEBUFFER_COMPLETE_EXT                        #x8CD5
#define GL_FRAMEBUFFER_EXT                 #x8D40
#define GL_LEQUAL                         #x0203
#define GL_LINEAR                         #x2601
#define GL_LINEAR_MIPMAP_LINEAR           #x2703
#define GL_MODELVIEW                      #x1700
#define GL_PROJECTION                     #x1701
#define GL_QUADS                          #x0007
#define GL_RENDERBUFFER_EXT                #x8D41
#define GL_RGBA                           #x1908
#define GL_RGBA8                          #x8058
#define GL_SMOOTH                         #x1D01
#define GL_TEXTURE_2D                     #x0DE1
#define GL_TEXTURE_MAG_FILTER             #x2800
#define GL_TEXTURE_MIN_FILTER             #x2801
#define GL_TEXTURE_WRAP_S                 #x2802
#define GL_TEXTURE_WRAP_T                 #x2803
#define GL_UNSIGNED_BYTE                  #x1401
#endif

typedef unsigned char GLboolean;
typedef unsigned int GLenum;
typedef long GLint;
typedef float GLclampf;
typedef double GLclampd;
typedef long GLsizei;
typedef unsigned long GLuint;

extern void glutInitDisplayMode(unsigned int mode);
extern void glutInitWindowSize(int width, int height);
extern int glutCreateWindow(const char *title);
extern GLboolean GLeeInit();
extern void glutMainLoop(void);
extern void glutDisplayFunc(void (*func)(void));
extern void glutIdleFunc(void (*func)(void));
extern void glShadeModel(GLenum mode);
extern void glClearColor (GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
extern void glClearDepth (GLclampd depth);
extern void glEnable (GLenum cap);
extern void glDepthFunc (GLenum func);
extern void glViewport (GLint x, GLint y, GLsizei width, GLsizei height);
extern void glGenFramebuffersEXT(GLsizei n, GLuint *framebuffers);
extern void glBindFramebufferEXT(GLenum target, GLuint framebuffer);
extern void glGenRenderbuffersEXT(GLsizei n, GLuint *renderbuffers);

extern void glBindRenderbufferEXT(GLenum target, GLuint renderbuffer);
extern void glRenderbufferStorageEXT(GLenum target, GLenum internalformat, GLsizei width, GLsizei height);
extern void glGenTextures (GLsizei n, GLuint *textures);

extern void glBindTexture (GLenum target, GLuint texture);

//extern void glBindFramebufferEXT(GLenum target, GLuint framebuffer);
// extern void glDeleteFramebuffersEXT(GLsizei n, const GLuint *framebuffers);
// extern void glDeleteRenderbuffersEXT(GLsizei n, const GLuint *renderbuffers);
// extern void glFramebufferRenderbufferEXT(GLenum target, GLenum attachment, GLenum renderbuffertarget, GLuint renderbuffer);
// extern void glFramebufferTexture2DEXT(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level);
// extern void glutIdleFunc(void (*func)(void));
// extern void glutInit(int *argcp, char **argv);
// extern void glutKeyboardFunc(void (*func)(unsigned char key, int x, int y));
// extern void glutPostRedisplay(void);
// extern void glutReshapeFunc(void (*func)(int width, int height));
// extern void glutSwapBuffers(void);
