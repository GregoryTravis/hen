(define (primitive-call? e)
  (is-this-labeled-doublet? 'primitive-call e))

(define (primitivize e)
  (atom-traverse
   (lambda (e)
     (cond
      ((integer? e) `(Integer (Primitive ,e)))
      ((string? e) `(String (Primitive ,e)))
      (#t e)))
   e))

(define (unprimitivize e)
  (cond
   ((is-quote? e) `(quote ,(unprimitivize (cadr e))))
   ((is-some-primitive? e) (cadadr e))
   ((list? e) (map unprimitivize e))
   (#t e)))

(define (do-primitive-call e)
  (let* ((f (car e))
         (f (if (is-quote? f)
                (quote-quoted f)
                f)))
    (let ((real-f (eval (->symbol (++ "pea-primitive-" f)))))
      (apply real-f (cdr e)))))
;;   (let ((e
;;          (map eval (map (lambda (e)
;;                           (if (primitive2? e)
;;                               (cadr e)
;;                               e))
;;                         e))))
;;     (apply (eval (car e)) (cdr e))))

;; (tracefun do-primitive-call)

(define (unprim e)
  (if (primitive? e)
      (cadr e)
      e))

(define (prim e)
  (list 'Primitive e))

(define (pea-primitive-integer-+ a b)
  (prim (+ (unprim a) (unprim b))))
(define (pea-primitive-integer-- a b)
  (prim (- (unprim a) (unprim b))))
(define (pea-primitive-integer-* a b)
  (prim (* (unprim a) (unprim b))))
(define (pea-primitive-integer-/ a b)
  (prim (/ (unprim a) (unprim b))))

(define (tfy x)
  (cond
   ((eq? #f x) 'False)
   ((eq? #t x) 'True)
   (#t (err 'tfy x))))

(define (untfy x)
  (cond
   ((eq? x 'True) #t)
   ((eq? x 'False) #f)
   (#t (err 'untfy x))))

(define (tfall? lyst)
  (all? (map untfy lyst)))

(define (pea-primitive-== a b)
  (let ((ta (typeof a))
        (tb (typeof b)))
    (tfy (and (eq? ta tb)
              (cond
               ((eq? 'cton ta)
                (if (= (length a) (length b))
                    (tfall? (map pea-primitive-== a b))
                    #f))
               ((eq? 'string ta) (string= a b))
               ((eq? 'number ta) (= a b))
               ((eq? 'ctor ta) (eq? a b))
               (#t (err '== a b)))))))
;(tracefun pea-primitive-== all?)

(define (pea-primitive-> a b)
  (let ((a (unprim a))
        (b (unprim b)))
    (tfy (and (number? a) (number? b) (> a b)))))

(define (pea-primitive-read-file filename)
  (sb-consyize (sb-read-file (unprim filename))))

(define (pea-primitive-shew e)
  (sb (unpreprocess e))
  '())
