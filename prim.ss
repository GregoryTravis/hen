(define (ctorize-predicate p)
  (lambda args
    (if (apply p args) 'True 'False)))

(define primitives
  `(
    ('= . ,(lambda (a b) (if (equal? a b) 'True 'False)))
    ('ctor? . ,(ctorize-predicate ctor?))
    ))

(define (try-primitive-rewrite e)
  (mtch e
        (fun . args) (if (lookup-exists? fun primitives) (just (apply (lookup fun primitives) args)) 'fail)
        _ fail))

;(tracefun try-primitive-rewrite lookup lookup-exists?)
