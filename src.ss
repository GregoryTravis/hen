(foreign "fbo" "fbo.o fbo.impl.o" "")
(foreign "fakey" "fakey.impl.o GLee.o" "-framework GLUT -framework OpenGL -framework CoreFoundation")

(fun (hendisplay fbo ) (doo
                   ;_ (shew 'display-callback)
                   _ (display fbo)))

(fun (henidle) (doo
                ;_ (shew 'idle-callback)
                _ (idle)))
(fun (myinit)
     (doo
      _ (glShadeModel _GL_SMOOTH)
      _ (glClearColor 0.0 0.0 0.2 0.5)
      _ (glClearDepth 1.0)
      _ (glEnable _GL_DEPTH_TEST)
      _ (glDepthFunc _GL_LEQUAL)
      _ (glViewport 0 0 800 600)

      ref (create-int-ref 0)
      _ (glGenFramebuffersEXT 1 ref)
      fbo (read-int-ref ref)
      _ (destroy-int-ref ref)
      _ (shew fbo)

      _ (init fbo)
      _ (Return fbo)))

(doo
 _ (fbo_main0)
 _ (glutInitDisplayMode _GLUT_DOUBLE)
 _ (glutInitWindowSize 800 600)
 ret (glutCreateWindow "holy cow")
 _ (shew ret)
 ret (_GLeeInit)
 _ (shew ret)

 fbo (myinit)
 _ (shew fbo)

 _ (fbo_main1)
 _ (glutDisplayFunc (/. () (hendisplay fbo)))
 _ (glutIdleFunc henidle)
 _ (glutMainLoop))
