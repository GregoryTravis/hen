(define (ctorize-predicate p)
  (lambda args
    (if (apply p args) 'True 'False)))

(define primitives
  `(
    (== . ,(ctorize-predicate equal?))
    (app? . ,(ctorize-predicate (lambda (x) (and (pair? x) (not (quote? x))))))
    (data? . ,(ctorize-predicate data?))
    (var? . ,(ctorize-predicate var?))
    ))

(define (is-primitive-call? e) (and (pair? e) (lookup-exists? (car e) primitives)))
(define (run-primitive e) (mtch e (fun . args) (apply (lookup fun primitives) args)))

;(tracefun try-primitive-rewrite lookup lookup-exists?)
