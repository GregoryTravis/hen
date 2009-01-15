#define GLUT_RGB__GLUT_DOUBLE__GLUT_DEPTH 18
#define CG_FALSE 0
#define CG_NO_ERROR 0
#define CG_COMPILER_ERROR 1
#define CG_DEFERRED_PARAMETER_SETTING 4133
#define CG_GL_VERTEX 8
#define CG_SOURCE 4112
#define CG_GL_FRAGMENT 9

typedef struct _CGcontext *CGcontext;
typedef int CGbool;
typedef int CGenum;

void mainby();
int mainby2(CGcontext _myCgContext);

CGcontext cgCreateContext();
void checkForCgError(char *situation);
void cgGLSetDebugMode(CGbool debug);
void cgSetParameterSettingMode(CGcontext ctx, CGenum parameterSettingMode);
