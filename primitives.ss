(define (do-primitive-call e)
  (let ((e (cons (eval (quote-quoted (car e)))
                 (map extract-primitive-maybe (cdr e)))))
    (apply (car e) (cdr e))))
