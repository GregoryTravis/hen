(foreign "fbo" "fbo.o fbo.impl.o" "")
(foreign "fakey" "" "-framework GLUT -framework OpenGL -framework CoreFoundation")

(doo
 dummy (fbo_main0)
 dummy (glutInitDisplayMode _GLUT_DOUBLE)
 dummy (fbo_main1))
