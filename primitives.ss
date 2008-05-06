(define pass-thrus '(+ - * /))
(define pass-thrus-tf '(< >))
(define special-forms '("if"))
(define custom '(==))

(define all-primitives
  (append pass-thrus pass-thrus-tf special-forms custom))

(define (t/f->true/false f)
  (lambda args
    (let ((r (apply f args)))
      (cond
       ((eq? r #t) 'true)
       ((eq? r #f) 'false)
       (#t (err ->true/false f r))))))

(define (prim-name x)
  (string->symbol (++ 'primitive- x)))

(define (eval-primitive e)
;  (shew e)
  (eval e))

(map (lambda (p)
       (eval-primitive `(define ,(prim-name p) ,(eval p))))
     (append pass-thrus special-forms))

(map (lambda (p) (eval-primitive `(define ,(prim-name p) ,(t/f->true/false (eval p)))))
     pass-thrus-tf)

(define (primitive-== a b)
  (cond
   ((string? a)
    (if (string? b) (if (string= a b) 'true 'false) 'false))
   ((number? a)
    (if (number? b) (if (= a b) 'true 'false) 'false))
   (#t
    (if (equal? a b) 'true 'false))))

(define (special-form? x)
  (member? x special-forms))
