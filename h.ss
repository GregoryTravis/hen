(load "lib.ss")

(define data? quoted-symbol?)
(define var? symbol?)

(load "prim.ss")

(define (rewrite e src)
  (cond
   ((or (var? e) (data? e)) e)
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
   ((and (null? e) (null? pat)) (just '()))
   ((var? pat) (just `((,pat . ,e))))
   ((data? pat) (if (equal? e pat) (just '()) fail))
   ((and (pair? pat) (pair? e)) (maybe-append (match-maybe (car e) (car pat)) (match-maybe (cdr e) (cdr pat))))
   (#t fail)))

(define (apply-bindings e bindings)
  (cond
   ((null? e) e)
   ((data? e) e)
   ((var? e) (lookup e bindings))
   ((pair? e) (cons (apply-bindings (car e) bindings) (apply-bindings (cdr e) bindings)))
   (#t (err 'apply-bindings e bindings))))

(define (check-exp e)
  (or
   (data? e)
   (var? e)
   (all? (map check-exp e))))

(define (check-src src)
  (map (lambda (rule) (mtch rule
                            ('fun pat body)
                            (and (check-exp pat) (check-exp body))))
       src))

(define (run e src)
  (assert (check-exp e))
  (assert (check-src src))
  (rewrite e src))

; tests
(define (test)
  (map (lambda (test) (mtch test (a b) (if (equal? a b) 'ok `(fail ,a ,b))))
       `(
         (,(match-maybe '('a 'B 'R 'c 'e) '(d 'B 'R f g)) (just ((d quote a) (f quote c) (g quote e))))
         (,(match-maybe '('a 'B 'R 'c 'e) '(d 'B 'Rr f g)) fail)
         (,(map ($ apply-bindings _ '((d . a) (f . c) (g . e))) '(d f g 'Joe (d f g 'Joe))) (a c e 'Joe (a c e 'Joe)))
         (,(run '('boot ('Cons Dop 'Nil)) '((fun ('boot ('Cons a 'Nil)) ('Cons a ('Cons a 'Nil))))) ('Cons Dop ('Cons Dop 'Nil)))
         (,(run '('boot ('Cons Dop 'Nil)) '((fun ('boot ('Cons a 'Nil)) ('Cons a ('Cons a 'Nil)))
                                         (fun ('boote ('Cons a 'Nil)) ('Cons a ('Cons a 'Nil))))) ('Cons Dop ('Cons Dop 'Nil)))
         (,(run '('boot ('Cons Dop 'Nil)) '((fun ('boote ('Cons a 'Nil)) ('Cons a ('Cons a 'Nil)))
                                         (fun ('boot ('Cons a 'Nil)) ('Cons a ('Cons a 'Nil))))) ('Cons Dop ('Cons Dop 'Nil)))
         (,(run '('boote ('Cons Dop 'Nil)) '((fun ('boot ('Cons a 'Nil)) ('Cons a ('Cons a 'Nil)))
                                         (fun ('boote ('Cons a 'Nil)) ('Cons a ('Cons a 'Jerk))))) ('Cons Dop ('Cons Dop 'Jerk)))
         (,(run '('boote ('Cons Dop 'Nil)) '((fun ('boote ('Cons a 'Nil)) ('Cons a ('Cons a 'Jerk)))
                                          (fun ('boot ('Cons a 'Nil)) ('Cons a ('Cons a 'Nil))))) ('Cons Dop ('Cons Dop 'Jerk)))
         (,(run '(dumbify ('Cons A ('Cons B ('Cons C 'Nil))))
                '(
                  (fun (dumbify 'Nil) 'Nil)
                  (fun (dumbify ('Cons a d)) ('Cons ('Cons 'Dumb a) (dumbify d)))
                  ))
          ('Cons ('Cons 'Dumb A) ('Cons ('Cons 'Dumb B) ('Cons ('Cons 'Dumb C) 'Nil))))
         (,(run '('= 'Joe 'Joe) '()) 'True)
         (,(run '('car ('Cons 'Haha 'Nil)) '((fun ('car ('Cons a d)) a) (fun ('cdr ('Cons a d)) d) (fun (cons a d) ('Cons a d)))) 'Haha)
         (,(run '('car ('Cons 'Haha ('Cons 'HoHo 'Nil))) '((fun ('car ('Cons a d)) a) (fun ('cdr ('Cons a d)) d) (fun (cons a d) ('Cons a d)))) 'Haha)
         (,(run '('cdr ('Cons 'Haha 'Nil)) '((fun ('car ('Cons a d)) a) (fun ('cdr ('Cons a d)) d) (fun (cons a d) ('Cons a d)))) 'Nil)
         (,(run '('cdr ('Cons 'Haha ('Cons 'HoHo 'Nil))) '((fun ('car ('Cons a d)) a) (fun ('cdr ('Cons a d)) d) (fun (cons a d) ('Cons a d)))) ('Cons 'HoHo 'Nil))
         (,(run '('var? e) '()) 'True)
         (,(run '('var? 'e) '()) 'False)
         (,(run '('data? e) '()) 'False)
         (,(run '('data? 'e) '()) 'True)
         (,(run 'Foo '()) Foo)
;;          (,(run '(current-program) '((fun ('boote ('Cons a 'Nil)) ('Cons a ('Cons a 'Jerk)))
;;                                      (fun ('boot ('Cons a 'Nil)) ('Cons a ('Cons a 'Nil)))))
;;           ('Cons ('Cons fun ('Cons ('Cons 'boote ('Cons ('Cons 'Cons ('Cons a ('Cons 'Nil ()))) ()))
;;                                 ('Cons ('Cons 'Cons ('Cons a ('Cons ('Cons 'Cons ('Cons a ('Cons 'Jerk ()))) ()))) ())))
;;                 ('Cons ('Cons fun ('Cons ('Cons 'boot ('Cons ('Cons 'Cons ('Cons a ('Cons 'Nil ()))) ()))
;;                                       ('Cons ('Cons 'Cons ('Cons a ('Cons ('Cons 'Cons ('Cons a ('Cons 'Nil ()))) ()))) ()))) ())))
         )))

;(tracefun rewrite rewrite-this rewrite-this-rule-list)
;(tracefun match-maybe apply-bindings)
;(tracefun preprocess preprocess-exp)

(test)
