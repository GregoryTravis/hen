;; (def map2
;;      (/./.
;;       (/. ((P f) ((P a) b)) ((cons (f a)) (map2 ((P f) b))))
;;       (/. ((P f) x) x)))

;; (def double (/. x (+ x x)))

;; ((map2 double) ((P 1) ((P 2) ((P 3) Nil))))

(def double (/. x ((+ x) x)))
(double 10)
