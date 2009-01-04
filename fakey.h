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
