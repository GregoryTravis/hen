(load "lib.ss")

(define program '())

(define (process-forms forms)
  (let* ((funs (grep fun? forms))
         (exps (grep (fnot fun?) forms))
         (main-fun
          `(fun (main) (begin ,@exps)))
         (rules (map cdr (snoc funs main-fun))))
    (set! program `(/./. ,rules))))

(define (go)
  (let ((forms (read-objects "src.ss")))
    (process-forms forms)
    (shew program)))

(go)
