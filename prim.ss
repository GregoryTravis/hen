(define (ctorize-predicate p)
  (lambda args
    (if (apply p args) 'True 'False)))

(define primitives
  `(
    (= . ,(ctorize-predicate equal?))
    (app? . ,(ctorize-predicate (lambda (x) (and (pair? x) (not (quote? x))))))
    (data? . ,(ctorize-predicate data?))
    (var? . ,(ctorize-predicate var?))
    ))

(define (try-primitive-rewrite e)
  (mtch e
        (fun . args) (if (lookup-exists? fun primitives) (just (apply (lookup fun primitives) args)) 'fail)
        _ fail))

;(tracefun try-primitive-rewrite lookup lookup-exists?)
