#define GLUT_RGB__GLUT_DOUBLE__GLUT_DEPTH 18
#define CG_FALSE 0
#define CG_NO_ERROR 0
#define CG_COMPILER_ERROR 1
#define CG_DEFERRED_PARAMETER_SETTING 4133
#define CG_GL_VERTEX 8
#define CG_SOURCE 4112
#define CG_GL_FRAGMENT 9

typedef struct _CGcontext *CGcontext;
typedef struct _CGprofile *CGprofile;
typedef struct _CGprogram *CGprogram;
typedef int CGbool;
typedef int CGenum;
typedef int CGerror;

void mainby();
int mainby2(CGcontext _myCgContext);

CGcontext cgCreateContext();
void checkForCgError(char *situation);

CGprogram cgCreateProgramFromFile(CGcontext ctx, CGenum program_type, char *program_file, CGprofile profile, char *entry, char **args);
void cgDestroyContext(CGcontext ctx); 
void cgDestroyProgram(CGprogram program); 
void cgGLBindProgram(CGprogram program);
void cgGLDisableProfile(CGprofile profile);
void cgGLEnableProfile(CGprofile profile);
void cgGLLoadProgram(CGprogram program);
void cgGLSetDebugMode(CGbool debug );
void cgGLSetOptimalOptions(CGprofile profile);
char * cgGetLastErrorString(CGerror *error);
char * cgGetLastListing(CGcontext ctx);
void cgSetParameterSettingMode(CGcontext ctx, CGenum parameterSettingMode);
