(foreign "fbo" "fbo.o fbo.impl.o" "")
(foreign "fakey" "fakey.impl.o GLee.o" "-framework GLUT -framework OpenGL -framework CoreFoundation")

(fun (hendisplay) (doo
                   ;_ (shew 'display-callback)
                   _ (display)))

(fun (henidle) (doo
                ;_ (shew 'idle-callback)
                _ (idle)))

(fun (myinit)
     (doo
      _ (glShadeModel _GL_SMOOTH)
      _ (init)))

(doo
 _ (fbo_main0)
 _ (glutInitDisplayMode _GLUT_DOUBLE)
 _ (glutInitWindowSize 800 600)
 ret (glutCreateWindow "holy cow")
 _ (shew ret)
 ret (_GLeeInit)
 _ (shew ret)

 _ (myinit)

 _ (fbo_main1)
 _ (glutDisplayFunc hendisplay)
 _ (glutIdleFunc henidle)
 _ (glutMainLoop))
