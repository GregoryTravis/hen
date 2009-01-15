#define GLUT_RGB__GLUT_DOUBLE__GLUT_DEPTH 18
#define CG_FALSE 0
typedef struct _CGcontext *CGcontext;
typedef int CGbool;

void mainby();
int mainby2(CGcontext _myCgContext);

CGcontext cgCreateContext();
void checkForCgError(char *situation);
void cgGLSetDebugMode(CGbool debug);
