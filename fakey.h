#define GLUT_RGB			0
#define GLUT_DOUBLE			2
extern void glutInitDisplayMode(unsigned int mode);
extern void glutInitWindowSize(int width, int height);
extern int glutCreateWindow(const char *title);
typedef unsigned char GLboolean;
extern GLboolean GLeeInit();
extern void glutMainLoop(void);
extern void glutDisplayFunc(void (*func)(void));
