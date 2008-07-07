(load "lib.ss")

(define (forms->program forms)
  (let* ((funs (grep fun? forms))
         (exps (grep (fnot fun?) forms))
         (main-fun
          `(fun (main) (begin ,@exps)))
         (rules (map cdr (snoc funs main-fun))))
    `(/./. ,rules)))

(define (go)
  (let ((forms (read-objects "src.ss")))
    (shew (compile-program (forms->program forms)))))

(define (compile-program program)
  (assert (multi-lambda? program))
  (map compile-rule (cadr program)))

(define (compile-rule rule)
  (compile-pattern-lambda (cons '/. rule)))

(define (compile-pattern-lambda lam)
  (assert (lambda? lam))
  lam)

;(tracefun lambda?)
;(tracefun compile-program compile-pattern-lambda compile-rule)

(go)
