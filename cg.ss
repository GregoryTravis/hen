(foreign "vertex_and_fragment_program" "vertex_and_fragment_program.impl.o vertex_and_fragment_program.o" "-framework OpenGL -framework GLUT -framework Cg")
(foreign "fakey" "fakey.impl.o GLee.o" "-framework GLUT -framework OpenGL -framework CoreFoundation")

(fun (displaype)
     (doo
      _ (display)))

(doo
 _ (glutInitWindowSize 800 800)
 _ (glutInitDisplayMode _GLUT_RGB__GLUT_DOUBLE__GLUT_DEPTH)

 char-ref (create-charp-ref "vertex_and_fragment_program")
 int-ref (create-int-ref 1)
 _ (glutInit int-ref char-ref)

 ret (glutCreateWindow "dag!")
 _ (shew ret)

 _ (mainby)
 _ (glutDisplayFunc displaype)
 _ (mainby3)

 _ (glClearColor .1 .3 .6 .0)
 myCgContext (cgCreateContext)
 _ (shew ($ 'myCgContext myCgContext))

 _ (checkForCgError "creating context")
 _ (cgGLSetDebugMode _CG_FALSE)
 _ (cgSetParameterSettingMode myCgContext _CG_DEFERRED_PARAMETER_SETTING)

 _ (mainby2 myCgContext)
)
