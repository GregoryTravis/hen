(foreign "fbo" "fbo.o fbo.impl.o" "")
(foreign "fakey" "fakey.impl.o GLee.o" "-framework GLUT -framework OpenGL -framework CoreFoundation")

(doo
 _ (fbo_main0)
 _ (glutInitDisplayMode _GLUT_DOUBLE)
 _ (glutInitWindowSize 800 600)
 ret (glutCreateWindow "holy cow")
 _ (shew ret)
 _ (fbo_main1))
