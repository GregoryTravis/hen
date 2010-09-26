(load "lib.ss")

(load "prim.ss")

(define (constant? e) (or (quote? e) (ctor? e) (null? e)))

(define (rewrite e src)
  (cond
   ((quote? e) e)
   ((constant? e) e)
   ((pair? e)
    (let ((new-e (rewrite-this-rule-list (map ($ rewrite _ src) e) src)))
      (if (equal? new-e e)
          new-e
          (rewrite new-e src))))
   (#t (err 'rewrite e src))))

(define (rewrite-this-rule-list e src)
  (mtch (try-primitive-rewrite e)
        (just result)
        result

        _
        (mtch src
              '() e
              (rule . rules) (mtch (rewrite-this e rule)
                                   'fail (rewrite-this-rule-list e rules)
                                   ('just result) result))))

(define (rewrite-this e rule)
  (mtch rule
        ('fun pat body)
        (mtch (match-maybe e pat)
              'fail 'fail
              ('just bindings) (just (apply-bindings body bindings)))))

(define (match-maybe e pat)
  (cond
   ((constant? pat) (if (equal? e pat) (just '()) fail))
   ((and (pair? pat) (pair? e)) (maybe-append (match-maybe (car e) (car pat)) (match-maybe (cdr e) (cdr pat))))
   ((symbol? pat) (just `((,pat . ,e))))
   (#t fail)))

(define (apply-bindings e bindings)
  (cond
   ((constant? e) e)
   ((pair? e) (cons (apply-bindings (car e) bindings) (apply-bindings (cdr e) bindings)))
   ((symbol? e) (lookup e bindings))
   (#t (err 'unbound e bindings))))

(define (preprocess src)
  (map (lambda (rule) (mtch rule ('fun pat body) `(fun ,(preprocess-exp pat) ,(preprocess-exp body)))) src))

(define (quote-funtion-name e)
  (cons (if (non-ctor-symbol? (car e)) `(quote ,(car e)) (car e))
        (cdr e)))

(define (preprocess-exp e)
  (cond
   ((constant? e) e)
   ((pair? e) (map preprocess-exp (quote-funtion-name e)))
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
         (,(run '(= Joe Joe) '()) True)
         )))

;(tracefun rewrite rewrite-this rewrite-this-rule-list)
;(tracefun match-maybe apply-bindings)

(test)
