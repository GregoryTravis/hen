(def voo
     (/./.
      (/. ((P f) ((P a) b)) ((cons 20) (voo ((P f) b))))
      (/. ((P f) x) x)))
(def double (/. x ((+ x) x)))

(voo ((P double) ((P 1) Nil)))
;(voo ((P double) Nil))

((/. ((P f) ((P a) b)) b) ((P 1000) ((P 1) 2)))
((/. ((P a) b) a) ((P 1) 2))
