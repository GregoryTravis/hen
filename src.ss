((/. (a b c) ((+ a) c)) 1 2 3)

;; (def map2
;;      (/./.
;;       (/. (f ((P a) b)) ((cons (f a)) (map2 f b)))
;;       (/. (f x) x)))

;; (def double (/. (x) ((+ x) x)))

;; ;; (def map
;; ;;      (/. (f)
;; ;;          (/./.
;; ;;           (/. (a . d) ((cons (f a)) (map f d)))
;; ;;           (/. (x) x))))

;; ;; (def double (/. (x) ((+ x) x)))
;; ;(double 10)

;; ;(map double ((cons 1) ((cons 2) ((cons 3) Nil))))
;; ;((map double) 'Nil)

;; ;(map2 double ((cons 1) ((cons 2) Nil)))
;; (map2 double Nil)
