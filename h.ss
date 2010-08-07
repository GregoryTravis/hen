(load "lib.ss")

(define pat '(Cons a (Barf c d 12)))
(define body '(Joe d c a))
(define clause `(clause ,pat ,body))

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
(define (clause->explicit-terms clause)
  (mtch clause
        ('clause pat body) `(clause ,(pat->explicit-terms pat)
                            ,(pat->explicit-terms body))))

(define (compile-pat pat exp body)
  (mtch pat
        ('literal lit) `(if (equal? ',lit ,exp) ,body (fail))
        ('variable v) `(let ((,v ,exp)) ,body)
        ('list (pat . pats)) (let ((new-v (make-var)))
                               `(let ((,new-v ,exp))
                                  ,(compile-pat pat `(car ,new-v) (compile-pat `(list ,pats) `(cdr ,new-v) body))))
        ('list ()) body))

(define (compile-body body)
  (mtch body
        ('literal lit) `',lit
        ('variable v) v
        ('list exps) `(list . ,(map compile-body exps))))

(define (compile-clause clause)
  (mtch clause
        ('clause pat body) (let ((new-v (make-var)))
                             `(/. (,new-v) ,(compile-pat pat new-v (compile-body body))))))

(define (->scheme e)
  (mtch e
        ('quote lit) e
        ('/. vars body) `(lambda ,vars ,(->scheme body))
        ('let bindings body) `(let ,(lensmap cadr-lens ->scheme bindings) ,(->scheme body))
        (a . d) (map ->scheme e)
        x x))

;(pat->explicit-terms pat)
;(tracefun pat->explicit-terms compile-pat)

;(shew pat)
;(compile-pat (pat->explicit-terms pat) 'v 'yeah)

;(shew body)
;(compile-body (pat->explicit-terms body))

(shew clause)
(compile-clause (clause->explicit-terms clause))
(->scheme (compile-clause (clause->explicit-terms clause)))
(define term (list (->scheme (compile-clause (clause->explicit-terms clause))) ''(Cons 10 (Barf 20 30 12))))
(shew term)
(eval term)

;; (cond
;;    ((pat-literal? pat)
;;     `(if (equals? ,pat ',exp) ,body (fail)))
;;    ((pat-variable? pat)
;;     `(let ((,pat ,exp)) ,body))
;;    ((pat-ctor?

;(pat->explicit-terms pat)
