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

(shew (build-receiver 'x 'x))
(shew (build-receiver '((P a) b) 'a))
(shew (build-receiver '((P a) ((P b) c)) 'a))
(shew (build-receiver '((P ((P a) aa)) ((P b) c)) 'a))

(let ((pat '((P j) k)))
  (shew (fuck pat (build-receiver pat 'j))))

(shew (blunk '(/. x x)))
(shew (blunk '(/. ((P a) b) a)))
(shew (blunk '(/. ((P a) ((P b) c)) a)))
(shew (blunk '(/. ((P ((P a) aa)) ((P b) c)) a)))
(shew (blunk '(/. ((P j) k) j)))

(evl-top (blunk '((/. ((P j) k) j) ((P 10) 20))))
(evl-top (blunk '((/. ((P j) k) k) ((P 10) 20))))
(evl-top (blunk '((/. ((P ((P j) jj)) ((P k) r)) j) ((P ((P 10) 100)) ((P 20) 30)))))
(evl-top (blunk '((/. ((P ((P j) jj)) ((P k) r)) jj) ((P ((P 10) 100)) ((P 20) 30)))))
(evl-top (blunk '((/. ((P ((P j) jj)) ((P k) r)) k) ((P ((P 10) 100)) ((P 20) 30)))))
(evl-top (blunk '((/. ((P ((P j) jj)) ((P k) r)) r) ((P ((P 10) 100)) ((P 20) 30)))))

(evl-top (blunk '((/. ((P j) ((P k) r)) j) ((P 10) ((P 20) 30)))))
(evl-top (blunk '((/. ((P j) ((P k) r)) k) ((P 10) ((P 20) 30)))))
(evl-top (blunk '((/. ((P j) ((P k) r)) r) ((P 10) ((P 20) 30)))))

(evl-top (blunk '((/. ((P ((P j) jj)) q) j) ((P ((P 10) 100)) 20))))
(evl-top (blunk '((/. ((P ((P j) jj)) q) jj) ((P ((P 10) 100)) 20))))
(evl-top (blunk '((/. ((P ((P j) jj)) q) q) ((P ((P 10) 100)) 20))))

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
