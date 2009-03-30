(define (tflift a)
  (mtch a #t 'True #f 'False))

;; (define (lift-pred p)
;;   (lambda args (tflift (apply p args))))
   
;; (define prim-= (lift-pred =))

(define (prim-== a b) (tflift (smart== a b)))

(define prim-+ +)
(define prim-- -)
(define prim-* *)
(define prim-/ /)

(define prim-shew shew-no-voids)
