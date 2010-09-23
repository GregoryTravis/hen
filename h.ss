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
                  'fail (rewrite-this e the-rest)
                  ('just bindings) (apply-bindings body bindings)))))

(define (match-maybe e pat)
  (cond
   ((or (quote? pat) (ctor? pat) (null? pat)) (if (equal? e pat) (just '()) fail))
   ((and (pair? pat) (pair? e)) (maybe-append (match-maybe (car e) (car pat))
                                              (match-maybe (cdr e) (cdr pat))))
   ((symbol? pat) (just `((,pat . ,e))))
   (#t fail)))

(define (apply-bindings e bindings)
  (cond
   ((or (quote? e) (ctor? e) (null? e)) e)
   ((pair? e) (cons (apply-bindings (car e) bindings) (apply-bindings (cdr e) bindings)))
   ((symbol? e) (lookup e bindings))
   (#t (err 'unbound e bindings))))

(define (preprocess src)
  (map (lambda (rule) (mtch rule ('fun pat body) `(fun ,(preprocess-exp pat) ,(preprocess-exp body)))) src))

(define (preprocess-exp e)
  (cond
   ((pair? e) (cons (if (non-ctor-symbol? (car e)) `(quote ,(car e)) (preprocess-exp (car e)))
                    (map preprocess-exp (cdr e))))
   (#t e)))

(define (run e src) (rewrite (preprocess-exp e) (preprocess src)))

; tests
(define (test)
  (map (lambda (test) (mtch test (a b) (if (equal? a b) 'ok `(fail ,a ,b))))
       `(
         (,(match-maybe '(a B R c e) '(d B R f g)) (just ((d . a) (f . c) (g . e))))
         (,(match-maybe '(a B R c e) '(d B Rr f g)) fail)
         (,(map ($ apply-bindings _ '((d . a) (f . c) (g . e))) '(d f g Joe (d f g Joe))) (a c e Joe (a c e Joe)))
         (,(run '(boot (Cons Dop Nil)) '((fun (boot (Cons a Nil)) (Cons a (Cons a Nil))))) (Cons Dop (Cons Dop Nil)))
         (,(run '(boot (Cons Dop Nil)) '((fun (boot (Cons a Nil)) (Cons a (Cons a Nil)))
                                         (fun (boote (Cons a Nil)) (Cons a (Cons a Nil))))) (Cons Dop (Cons Dop Nil)))
         (,(run '(boot (Cons Dop Nil)) '((fun (boote (Cons a Nil)) (Cons a (Cons a Nil)))
                                         (fun (boot (Cons a Nil)) (Cons a (Cons a Nil))))) (Cons Dop (Cons Dop Nil)))
         (,(run '(boote (Cons Dop Nil)) '((fun (boot (Cons a Nil)) (Cons a (Cons a Nil)))
                                         (fun (boote (Cons a Nil)) (Cons a (Cons a Jerk))))) (Cons Dop (Cons Dop Jerk)))
         (,(run '(boote (Cons Dop Nil)) '((fun (boote (Cons a Nil)) (Cons a (Cons a Jerk)))
                                         (fun (boot (Cons a Nil)) (Cons a (Cons a Nil))))) (Cons Dop (Cons Dop Jerk)))
         (,(run '(dumbify (Cons A (Cons B (Cons C Nil))))
                '(
                  (fun (dumbify Nil) Nil)
                  (fun (dumbify (Cons a d)) (Cons (Cons Dumb a) (dumbify d)))
                  ))
          (Cons (Cons Dumb A) (Cons (Cons Dumb B) (Cons (Cons Dumb C) Nil))))
         )))

(test)
