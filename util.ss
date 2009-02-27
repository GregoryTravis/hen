(define count-reductions #f)
(define show-tsgs #f)
(define pretty-output #t)

(define n-reductions 0)
(define (count-reductions-start)
  (set! n-reductions 0))
(define (count-reductions-end)
  (if count-reductions
      (begin
        (display n-reductions)
        (display " reductions.\n"))
      '()))

(define sg (symbol-generator-generator))
(define tsg
  (let ((sg (tagged-symbol-generator-generator)))
    (lambda (tag . stuff)
      (let ((v (sg tag)))
        (if show-tsgs (shew `(,v ,tag . ,stuff)) '())
        v))))

(define (prettify-shewer shewer)
  (lambda args (apply shewer ((if pretty-output syntax-sugar id) args))))

(define plshew (prettify-shewer lshew))

(define (p-ify-ctons e)
  (cond
   ((cton? e) (p-ify (map p-ify-ctons e)))
   ((pair? e) (map-improper p-ify-ctons e))
   (#t e)))

(define (p-ify e)
  (mtch e
        '() 'Nil
        (a . b) `(P ,a ,(p-ify b))
        x x))

(define (un-p-ify e)
  (mtch e
        ('P a 'Nil) (list (un-p-ify a))
        ('P a b) (cons (un-p-ify a) (un-p-ify b))
        (a . b) (map un-p-ify e)
        x x))

(define (cons-ify e)
  (mtch e
        ('$) 'Nil
        ('$ a . d) `(Cons ,(cons-ify a) ,(cons-ify `($ . ,d)))
        (a . d) (map-improper cons-ify e)
        x x))

(define (un-cons-ify e)
  (mtch e
        ('Cons a b) `($ . ,(map-improper un-cons-ify (un-cons-ify-1 e)))
        (a . b) (map un-cons-ify e)
        x x))

(define (un-cons-ify-1 e)
  (mtch e
        ('Cons a b) (cons a (un-cons-ify-1 b))
        'Nil '()
        x x))
