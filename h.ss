(load "lib.ss")

(define (rewrite e src)
  (let ((new-e (rewrite-this e src)))
    (if (equal? new-e e)
        (if (pair? new-e) (map ($ rewrite _ src) new-e) new-e)
        (rewrite new-e src))))

(define (rewrite-this e src)
  (if (null? src)
      e
      (mtch src
            (('fun pat body) . the-rest)
            (mtch (match-maybe e pat)
                  'fail e
                  ('just bindings) (apply-bindings body bindings)))))

(define (match-maybe e pat)
  (cond
   ((quote? pat) (if (equal? e pat) (just '()) 'fail))
   ((and (pair? pat) (pair? e)) (maybe-append (match-maybe (car e) (car pat))
                                              (match-maybe (cdr e) (cdr pat))))
   ((ctor? pat) (if (eq? e pat) (just '()) fail))
   ((and (null? pat) (null? e)) (just '()))
   ((symbol? pat) (just `((,pat . ,e))))
   (#t fail)))

(define (apply-bindings e bindings)
  (cond
   ((pair? e) (cons (apply-bindings (car e) bindings) (apply-bindings (cdr e) bindings)))
   ((ctor? e) e)
   ((null? e) e)
   ((symbol? e) (lookup e bindings))
   (#t (err 'unbound e bindings))))

(define (preprocess src)
  (map (lambda (rule) (mtch rule ('fun pat body) `(fun ,pat ,(preprocess-exp body)))) src))

(define (preprocess-exp e)
  (cond
   ((pair? e) (cons (if (and (symbol? (car e)) (not (ctor? (car e)))) `(quote ,(car e)) (preprocess-exp (car e)))
                    (map preprocess-exp (cdr e))))
   (#t e)))

(define (run e src) (rewrite (preprocess-exp e) (preprocess src)))

; tests
(define (test)
  (map (lambda (test) (mtch test (a b) (if (equal? a b) 'ok `(fail ,a ,b))))
       `(
         (,(match-maybe '(a B R c e) '(d B R f g)) (just ((d . a) (f . c) (g . e))))
         (,(match-maybe '(a B R c e) '(d B Rr f g)) fail)
         (,(map ($ apply-bindings _ '((d . a) (f . c) (g . e))) '(d f g Joe (d f g Joe)))
          (a c e Joe (a c e Joe)))
         (,(run '(boot (Cons Dop Nil)) '((fun (boot (Cons a Nil)) (Cons a (Cons a Nil)))))
          (Cons Dop (Cons Dop Nil)))
         )))

(test)
