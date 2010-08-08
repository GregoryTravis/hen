(load "lib.ss")

(define pat '(Cons a (Barf c d 12)))
(define body '(Joe d c a))
(define clause `(clause ,pat ,body))
(define clauses `((,pat ,body))) ;`(clauses (,clause)))

(define make-var (symbol-generator-generator))

(define (pat-literal? x) (or (number? x) (is-quote? x) (string? x)))
(define (literal-value x) (if (is-quote? x) (quote-quoted x) x))
(define (pat-variable? x) (and (symbol? x) (not (ctor? x))))
(define (pat-ctor? x) (ctor? x))

(define (fun-name fun)
  (mtch fun ('fun (name . pat) body) name))
(define (fun->clause fun)
  (mtch fun ('fun (name . pat) body) `(,pat ,body)))

(define (pat->explicit-terms pat)
  (cond
   ((pat-literal? pat) `(literal ,pat))
   ((pat-ctor? pat) `(literal ,pat))
   ((pat-variable? pat) `(variable ,pat))
   ((pair? pat) `(list ,(map pat->explicit-terms pat)))))
(define (clause->explicit-terms clause)
  (mtch clause
        (pat body) `(clause ,(pat->explicit-terms pat)
                            ,(pat->explicit-terms body))))
(define (clauses->explicit-terms clauses)
  `(clauses ,(map clause->explicit-terms clauses)))

(define (compile-pat pat exp body)
  (mtch pat
        ('literal lit) `(if (equal? ',lit ,exp) ,body (fail))
        ('variable v) `(let ((,v ,exp)) ,body)
        ('list (pat . pats)) (let ((new-var (make-var)))
                               `(let ((,new-var ,exp))
                                  ,(compile-pat pat `(car ,new-var) (compile-pat `(list ,pats) `(cdr ,new-var) body))))
        ('list ()) body))

(define (compile-body body)
  (mtch body
        ('literal lit) `',lit
        ('variable v) v
        ('list exps) `(list . ,(map compile-body exps))))

(define (compile-clause clause)
  (mtch clause
        ('clause pat body) (let ((new-var (make-var)))
                             `(/. (,new-var) ,(compile-pat pat new-var (compile-body body))))))

(define (compile-clauses var clauses)
  (mtch clauses
        ('clauses (clause . clauses)) `(let ((fail (/. () (,(compile-clauses var `(clauses ,clauses)) ,var))))
                                         (,(compile-clause clause) ,var))
        ('clauses ()) `(begin (display 'fail) (exit))))

(define (function->scheme name clauses)
  (let ((new-var (make-var)))
    `(define (,name . ,new-var) ,(->scheme (compile-clauses new-var clauses)))))

(define (->scheme e)
  (mtch e
        ('quote lit) e
        ('/. vars body) `(lambda ,vars ,(->scheme body))
        ('let bindings body) `(let ,(lensmap cadr-lens ->scheme bindings) ,(->scheme body))
        (a . d) (map ->scheme e)
        x x))

;; ((fun ...) (fun ...) ...) => ((name clauses) (name clauses) ...)
(define (funs->named-clause-lists src)
  (lensmap cadr-lens ($ map fun->clause _) (group-by fun-name src)))

(define (src->defines src)
  (map (lambda (blah) (mtch blah (name clauses) (function->scheme name (clauses->explicit-terms clauses))))
       (funs->named-clause-lists src)))

(tracefun function->scheme)

;(pat->explicit-terms pat)
;(tracefun pat->explicit-terms compile-pat)

;(shew pat)
;(compile-pat (pat->explicit-terms pat) 'v 'yeah)

;(shew body)
;(compile-body (pat->explicit-terms body))

;(shew clause)
;(compile-clause (clause->explicit-terms clause))
;(->scheme (compile-clause (clause->explicit-terms clause)))
;(define term (list (->scheme (compile-clause (clause->explicit-terms clause))) ''(Cons 10 (Barf 20 30 12))))
;(shew term)
;(eval term)

;(shew clauses)
;(clauses->explicit-terms clauses)
;(->scheme (compile-clauses 'joe (clauses->explicit-terms clauses)))

;; (define func (function->scheme 'joe (clauses->explicit-terms clauses)))
;; (shew func)
;; (eval func)
;; (shew joe)
;; (define term '(joe '(Cons 10 (Barf 20 30 12))))
;; (shew term)
;; (eval term)

(define src
  '(
    (fun (foo (Cons a (Barf c d 12))) (Joe d c a))
    (fun (foo (Cons a (Tween t))) (Ack t t t))
    (fun (bar a b) (Blech b a))
    ))

(shew src)
;(shew (funs->named-clause-lists src))
;(shew (src->defines src))
(define term '(foo '(Cons 10 (Barf 20 30 12))))
(if (file-exists? "obj.ss") (delete-file "obj.ss") '())
(write-objects-to-file "obj.ss" (append (src->defines src) (list term)))
(shew (load "obj.ss"))
(delete-file "obj.ss")

;(shew fun)
;(fun->explicit-terms fun)

;; (cond
;;    ((pat-literal? pat)
;;     `(if (equals? ,pat ',exp) ,body (fail)))
;;    ((pat-variable? pat)
;;     `(let ((,pat ,exp)) ,body))
;;    ((pat-ctor?

;(pat->explicit-terms pat)
