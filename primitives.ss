(define (primitive-call? e)
  (is-this-labeled-doublet? ''primitive-call e))

(define (primitivize e)
  (atom-traverse
   (lambda (e)
     (cond
      ((integer? e) `(integer (primitive ,e)))
      ((string? e) `(string (primitive ,e)))
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
  (if (primitive2? e)
      (cadr e)
      e))

(define (prim e)
  (list ''primitive e))

(define (pea-primitive-integer-+ a b)
  (prim (+ (unprim a) (unprim b))))
(define (pea-primitive-integer-- a b)
  (prim (- (unprim a) (unprim b))))
(define (pea-primitive-integer-* a b)
  (prim (* (unprim a) (unprim b))))
(define (pea-primitive-integer-/ a b)
  (prim (/ (unprim a) (unprim b))))

(define (true-false-ify x)
  (cond
   ((eq? #f x) ''false)
   ((eq? #t x) ''true)
   (#t (err 'true-false-ify x))))

(define (pea-primitive-== a b)
  (let ((a (unprim a))
        (b (unprim b)))
    (cond
     ((string? a) (true-false-ify (string= a b)))
     ((symbol? a) (true-false-ify (eq? a b)))
     ((number? a) (true-false-ify (= a b)))
     (#t (err 'pea-primitive-= a b)))))

(define (pea-primitive-> a b)
  (let ((a (unprim a))
        (b (unprim b)))
    (true-false-ify (and (number? a) (number? b) (> a b)))))

(define (pea-primitive-read-file filename)
  (sb-consyize (sb-read-file (unprim filename))))

(define (pea-primitive-shew e)
  (sb (unpreprocess e))
  '())
