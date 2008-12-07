(load "h.ss")

(shew (doobie-exp '(/. (x . joe) x)))
(shew (doobie-exp '(/. (x y z . q) x)))
(shew (doobie-exp '(f . 1)))
(shew (doobie-exp '(f 1 . 2)))
(shew (doobie-exp '(f 1 2 . 3)))

(shew (doobie-exp '(/. (a b . c) (+ a c))))
(shew (doobie-exp '(ulp 1 2 . 3)))
(shew (doobie-exp '((/. (a b . c) ((+ a) c)) 1 2 . 3)))

(shew (doobie-exp '((/. (a b c) ((+ a) c)) 1 2 3)))

;; (map evl-check
;;      '(;a
;;        ((/. r r) 1)
;;        ((/. p (p 2)) (/. y y))
;;        ((+ 1) 2)
;; ;;        ((= 3) 3)
;; ;;        ((= 3) 4)
;; ;       (((if True) 1) 2)
;; ;       (((if False) 1) 2)
;;        ))

(run-file "test-input.ss")
