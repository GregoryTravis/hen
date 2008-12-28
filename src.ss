(def _GLUT_RGB 0)
(def _GLUT_DOUBLE 2)
(def or-em 2)
(X 'fbo_main0 Nil
   (/. (x) (X 'glutInitDisplayMode or-em
              (/. (x) (X 'fbo_main1 Nil id)))))
