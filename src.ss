(def double (/. (x) ((+ x) x)))
(def map2
     (/./.
      ;(/. (f a . b) ((cons (f a)) (map2 f . b)))
      (/. (f (P a b)) ((cons (f a)) (map2 f b)))
      ;(/. (f . x) x)
      (/. (f Nil) Nil)))

(map2 double (P 1 (P 2 (P 3 Nil))))
