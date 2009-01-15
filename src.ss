(foreign "vertex_and_fragment_program" "vertex_and_fragment_program.impl.o vertex_and_fragment_program.o" "-framework OpenGL -framework GLUT -framework Cg")
(foreign "fakey" "fakey.impl.o GLee.o" "-framework GLUT -framework OpenGL -framework CoreFoundation")

(doo
 _ (glutInitWindowSize 800 800)
 _ (glutInitDisplayMode _GLUT_RGB__GLUT_DOUBLE__GLUT_DEPTH)

 char-ref (create-charp-ref "vertex_and_fragment_program")
 int-ref (create-int-ref 1)
 _ (glutInit int-ref char-ref)

 ret (glutCreateWindow "dag!")
 _ (shew ret)

 _ (mainby)
 _ (mainby2)
)
