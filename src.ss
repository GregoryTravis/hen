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
;      vork ((== 0) 0)
;      _ (shew ($ 'hoont vork))

      depthBuffer (make-depth-buffer)

      img (make-texture)

      _ (glBindFramebufferEXT _GL_FRAMEBUFFER_EXT fbo)
      _ (glBindRenderbufferEXT _GL_RENDERBUFFER_EXT depthBuffer)
      _ (glRenderbufferStorageEXT _GL_RENDERBUFFER_EXT _GL_DEPTH_COMPONENT 512 512)
      _ (glBindTexture _GL_TEXTURE_2D img)
      null (create-null-ref)
      _ (glTexImage2D _GL_TEXTURE_2D 0 _GL_RGBA8  512 512 0 _GL_RGBA _GL_UNSIGNED_BYTE null)

      _ (glTexParameterf _GL_TEXTURE_2D _GL_TEXTURE_WRAP_S _GL_CLAMP_TO_EDGE)
      _ (glTexParameterf _GL_TEXTURE_2D _GL_TEXTURE_WRAP_T _GL_CLAMP_TO_EDGE)
      _ (glTexParameterf _GL_TEXTURE_2D _GL_TEXTURE_MAG_FILTER _GL_LINEAR)
      _ (glTexParameterf _GL_TEXTURE_2D _GL_TEXTURE_MIN_FILTER _GL_LINEAR)
      _ (glFramebufferTexture2DEXT _GL_FRAMEBUFFER_EXT _GL_COLOR_ATTACHMENT0_EXT _GL_TEXTURE_2D img 0)
      _ (glFramebufferRenderbufferEXT _GL_FRAMEBUFFER_EXT _GL_DEPTH_ATTACHMENT_EXT _GL_RENDERBUFFER_EXT depthBuffer)

      status (glCheckFramebufferStatusEXT _GL_FRAMEBUFFER_EXT)
      _ (shew ($ 'must-be-== status _GL_FRAMEBUFFER_COMPLETE_EXT))

      _ (glBindFramebufferEXT _GL_FRAMEBUFFER_EXT 0)

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
