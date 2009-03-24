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
  (let ((sg (tagging-symbol-generator-generator)))
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

(define (cons-ify e)
  (mtch e
        '() 'Nil
        (a . b) `(Cons ,a ,(cons-ify b))
        x x))

(define (un-cons-ify e)
  (mtch e
        ('Cons a 'Nil) (list (un-cons-ify a))
        ('Cons a b) (cons (un-cons-ify a) (un-cons-ify b))
        (a . b) (map un-cons-ify e)
        x x))
