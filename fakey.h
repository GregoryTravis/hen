#define GLUT_RGB			0
#define GLUT_DOUBLE			2
#define GL_SMOOTH                         7425

typedef unsigned char GLboolean;
typedef unsigned int GLenum;

extern void glutInitDisplayMode(unsigned int mode);
extern void glutInitWindowSize(int width, int height);
extern int glutCreateWindow(const char *title);
extern GLboolean GLeeInit();
extern void glutMainLoop(void);
extern void glutDisplayFunc(void (*func)(void));
extern void glutIdleFunc(void (*func)(void));
extern void glShadeModel(GLenum mode);
