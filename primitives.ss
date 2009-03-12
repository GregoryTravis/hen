(define (tflift a)
  (mtch a #t 'True #f 'False))

;; (define (lift-pred p)
;;   (lambda args (tflift (apply p args))))
   
;; (define prim-= (lift-pred =))

(define (prim-= a b)
  (tflift
   (cond
    ((number? a) (= a b))
    ((string? a) (string= a b))
    ((symbol? a) (eq? a b))
    (#t (err 'prim= a b)))))

(define prim-+ +)
(define prim-- -)
(define prim-* *)
(define prim-/ /)
