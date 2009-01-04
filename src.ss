;; (fun (vok) (Hoo 10 20))

;; (doo
;;  (Hoo a b) (Return (Hoo 10 20))
;;  _ (shew b))

(foreign "fbo" "fbo.o fbo.impl.o" "")
(foreign "fakey" "fakey.impl.o GLee.o" "-framework GLUT -framework OpenGL -framework CoreFoundation")

(fun (hendisplay fbo img) (doo
                   ;_ (shew 'display-callback)
                   _ (display fbo img)))

(fun (henidle) (doo
                ;_ (shew 'idle-callback)
                _ (idle)))

(fun (make-fbo)
     (doo
      ref (create-int-ref 0)
      _ (glGenFramebuffersEXT 1 ref)
      fbo (read-int-ref ref)
      _ (destroy-int-ref ref)
      _ (shew fbo)
      _ (Return fbo)))

(fun (make-depth-buffer)
     (doo
      ref (create-int-ref 0)
      _ (glGenRenderbuffersEXT 1 ref)
      depthBuffer (read-int-ref ref)
      _ (destroy-int-ref ref)
      _ (shew depthBuffer)
      _ (Return depthBuffer)))

(fun (make-texture)
     (doo
      ref (create-int-ref 0)
      _ (glGenTextures 1 ref)
      img (read-int-ref ref)
      _ (destroy-int-ref ref)
      _ (shew img)
      _ (Return img)))

(fun (myinit)
     (doo
      _ (glShadeModel _GL_SMOOTH)
      _ (glClearColor 0.0 0.0 0.2 0.5)
      _ (glClearDepth 1.0)
      _ (glEnable _GL_DEPTH_TEST)
      _ (glDepthFunc _GL_LEQUAL)
      _ (glViewport 0 0 800 600)

      fbo (make-fbo)

      depthBuffer (make-depth-buffer)

      img (make-texture)

      _ (glBindFramebufferEXT _GL_FRAMEBUFFER_EXT fbo)
      _ (glBindRenderbufferEXT _GL_RENDERBUFFER_EXT depthBuffer)
      _ (glRenderbufferStorageEXT _GL_RENDERBUFFER_EXT _GL_DEPTH_COMPONENT 512 512)
      _ (glBindTexture _GL_TEXTURE_2D img)

      _ (init fbo depthBuffer img)

;      _ (Return (Haha fbo depthBuffer))))
      _ (Return (Foo fbo img))))

(doo
 _ (fbo_main0)
 _ (glutInitDisplayMode _GLUT_DOUBLE)
 _ (glutInitWindowSize 800 600)
 ret (glutCreateWindow "holy cow")
 _ (shew ret)
 ret (_GLeeInit)
 _ (shew ret)

; (Haha fbo depthBuffer) (myinit)
 (Foo fbo img) (myinit)
 _ (shew fbo)

 _ (fbo_main1)
; _ (glutDisplayFunc (/. () (hendisplay fbo depthBuffer)))
 _ (glutDisplayFunc (/. () (hendisplay fbo img)))
 _ (glutIdleFunc henidle)
 _ (glutMainLoop))
