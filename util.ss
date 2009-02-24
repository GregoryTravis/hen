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

