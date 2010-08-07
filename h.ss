(load "lib.ss")

(define pat '(Cons a (Barf c d 12)))

(define make-var (symbol-generator-generator))

(define (pat-literal? x) (or (number? x) (is-quote? x) (string? x)))
(define (literal-value x) (if (is-quote? x) (quote-quoted x) x))
(define (pat-variable? x) (and (symbol? x) (not (ctor? x))))
(define (pat-ctor? x) (ctor? x))

(define (pat->explicit-terms pat)
  (cond
   ((pat-literal? pat) `(literal ,pat))
   ((pat-ctor? pat) `(literal ,pat))
   ((pat-variable? pat) `(variable ,pat))
   ((pair? pat) `(list ,(map pat->explicit-terms pat)))))

(define (pat->/. pat exp body)
  (mtch pat
        ('literal lit) `(if (equals? ',lit ,exp) ,body (fail))
        ('variable v) `(let ((,v ,exp)) ,body)
        ('list (pat . pats)) (let ((new-v (make-var)))
                               `(let ((,new-v ,exp))
                                  ,(pat->/. pat `(car ,new-v) (pat->/. `(list ,pats) `(cdr ,new-v) body))))
        ('list ()) body))

;(pat->explicit-terms pat)
;(tracefun pat->explicit-terms pat->/.)
(shew pat)
(pat->/. (pat->explicit-terms pat) 'v 'yeah)

;; (cond
;;    ((pat-literal? pat)
;;     `(if (equals? ,pat ',exp) ,body (fail)))
;;    ((pat-variable? pat)
;;     `(let ((,pat ,exp)) ,body))
;;    ((pat-ctor?

;(pat->explicit-terms pat)
