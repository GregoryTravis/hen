(load "lib.ss")

(define data? symbol?)
(define var? unquoted-symbol?)

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
  (if (equal? e '(current-program))
      (reify-src src)
      (mtch (try-primitive-rewrite e)
            (just result)
            result

            _
            (mtch src
                  '() e
                  (rule . rules) (mtch (rewrite-this e rule)
                                       'fail (rewrite-this-rule-list e rules)
                                       ('just result) result)))))

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
   (null? e)
   (and (pair? e) (check-exp (car e)) (check-exp (cdr e)))
   (data? e)
   (var? e)))

(define (run e src)
  (assert (check-exp e))
  (assert (check-exp src))
  (rewrite e src))

(define (consify e)
  (cond
   ((or (var? e) (data? e)) e)
   ((null? e) 'Nil)
   ((pair? e) `(Cons ,(consify (car e)) ,(consify (cdr e))))
   (#t (err 'consify e))))

(define reify-src consify) ; (reify-src e) (foldr (lambda (a d) `(Cons ,a ,d)) 'Nil e))

(define (run-test test) (mtch test (a b) (if (equal? a b) 'ok `(fail ,a ,b))))

; tests
(define (test)
  (let ((some-funs '((fun (map ,f Nil) (,f Nil))
                   (fun (map ,f (Cons ,a ,d)) (Cons (,f ,a) (map ,f ,d)))
                   (fun (swap (P ,a ,d)) (P ,d ,a))
                   (fun (apply ,f . ,a) (,f . ,a)))))
    (map run-test
         `(
           ,(list (match-maybe '(a B R c e) '(,d B R ,f ,g)) '(just ((,d . a) (,f . c) (,g . e))))
           (,(match-maybe '('a 'B 'R 'c 'e) '(d 'B 'Rr f g)) fail)
           (,(map ($ apply-bindings _ '((,d . a) (,f . c) (,g . e))) '(,d ,f ,g 'Joe (,d ,f ,g 'Joe))) (a c e 'Joe (a c e 'Joe)))
           (,(run '(boot (Cons Dop Nil)) '((fun (boot (Cons ,a Nil)) (Cons ,a (Cons ,a Nil))))) (Cons Dop (Cons Dop Nil)))
           (,(run '(boot (Cons Dop Nil)) '((fun (boot (Cons ,a Nil)) (Cons ,a (Cons ,a Nil)))
                                           (fun (boote (Cons ,a Nil)) (Cons ,a (Cons ,a Nil))))) (Cons Dop (Cons Dop Nil)))
           (,(run '(boot (Cons Dop Nil)) '((fun (boote (Cons ,a Nil)) (Cons ,a (Cons ,a Nil)))
                                           (fun (boot (Cons ,a Nil)) (Cons ,a (Cons ,a Nil))))) (Cons Dop (Cons Dop Nil)))
           (,(run '(boote (Cons Dop Nil)) '((fun (boot (Cons ,a Nil)) (Cons ,a (Cons ,a Nil)))
                                            (fun (boote (Cons ,a Nil)) (Cons ,a (Cons ,a Jerk))))) (Cons Dop (Cons Dop Jerk)))
           (,(run '(boote (Cons Dop Nil)) '((fun (boote (Cons ,a Nil)) (Cons ,a (Cons ,a Jerk)))
                                            (fun (boot (Cons ,a Nil)) (Cons ,a (Cons ,a Nil))))) (Cons Dop (Cons Dop Jerk)))
           (,(run '(dumbify (Cons A (Cons B (Cons C Nil))))
                  '((fun (dumbify Nil) Nil) (fun (dumbify (Cons ,a ,d)) (Cons (Cons Dumb ,a) (dumbify ,d)))))
            (Cons (Cons Dumb A) (Cons (Cons Dumb B) (Cons (Cons Dumb C) Nil))))
           (,(run '(= Joe Joe) '()) True)
           (,(run '(car (Cons Haha Nil)) '((fun (car (Cons ,a ,d)) ,a) (fun (cdr (Cons ,a ,d)) ,d) (fun (cons ,a ,d) (Cons ,a ,d)))) Haha)
           (,(run '(car (Cons Haha (Cons HoHo Nil))) '((fun (car (Cons ,a ,d)) ,a) (fun (cdr (Cons ,a ,d)) ,d) (fun (cons ,a ,d) (Cons ,a ,d)))) Haha)
           (,(run '(cdr (Cons Haha Nil)) '((fun (car (Cons ,a ,d)) ,a) (fun (cdr (Cons ,a ,d)) ,d) (fun (cons ,a ,d) (Cons ,a ,d)))) Nil)
           (,(run '(cdr (Cons Haha (Cons HoHo Nil))) '((fun (car (Cons ,a ,d)) ,a) (fun (cdr (Cons ,a ,d)) ,d) (fun (cons ,a ,d) (Cons ,a ,d)))) (Cons HoHo Nil))
           (,(run '(var? ,e) '()) True)
           (,(run '(var? e) '()) False)
           (,(run '(data? ,e) '()) False)
           (,(run '(data? e) '()) True)
           (,(run 'Foo '()) Foo)
           ,(list (run '(current-program) '((fun (boote (Cons ,a Nil)) (Cons ,a (Cons ,a Jerk)))
                                            (fun (boot (Cons ,a Nil)) (Cons ,a (Cons ,a Nil)))))
                  '(Cons (Cons fun (Cons (Cons boote (Cons (Cons Cons (Cons ,a (Cons Nil Nil))) Nil))
                                         (Cons (Cons Cons (Cons ,a (Cons (Cons Cons (Cons ,a (Cons Jerk Nil))) Nil))) Nil)))
                         (Cons (Cons fun (Cons (Cons boot (Cons (Cons Cons (Cons ,a (Cons Nil Nil))) Nil))
                                               (Cons (Cons Cons (Cons ,a (Cons (Cons Cons (Cons ,a (Cons Nil Nil))) Nil))) Nil))) Nil)))
           (,(run '(map swap (Cons (P A B) (Cons (P C D) Nil))) some-funs) (Cons (P B A) (Cons (P D C) (swap Nil))))
           (,(run '(apply swap (P A B)) some-funs) (P B A))
           ))))

;(tracefun rewrite rewrite-this rewrite-this-rule-list)
;(tracefun match-maybe apply-bindings)
;(tracefun reify-src)

;(test)

(define (gather-vars e)
  (cond
   ((var? e) (list e))
   ((pair? e) (let* ((a-vars (gather-vars (car e)))
                     (d-vars (gather-vars (cdr e))))
                (unique (append a-vars d-vars))))
   (#t '())))

(define (alpha-rename-rule rule symbol-generator)
  (mtch rule
        ('fun pattern body)
        (let ((bindings (build-mapping-for-list (gather-vars (list pattern body)) symbol-generator)))
          `(,bindings (fun ,(apply-bindings pattern bindings) ,(apply-bindings body bindings))))))

;; (define (unify-rules left right)
;;   (mtch (list left (alpha-rename-rule right (symbol-generator 'a)))
;;         (('fun left-pattern left-body) ('fun right-pattern right-body))
;;         (let ((left-bindings (match-maybe right-pattern left-body))
;;               (right-bindings (match-maybe left-body right-pattern)))
;;           (if (just? left-bindings)

(define (test)
  (map run-test
       `(
         (,(build-mapping-for-list '(1 2 3) (symbol-generator-generator 'a)) ((1 . a0) (2 . a1) (3 . a2)))
         ,(list (gather-vars '(A (B ,c ,c (R ,u ,v ,d) ,d) ,j)) '(,c ,u ,v ,d ,j))
         ,(list (alpha-rename-rule '(fun (foo (A (B ,c ,c (R ,u ,v ,d) ,d) ,j)) (Bart ,u ,v ,j ,c ,c))
                                   (symbol-generator-generator 'a))
                '(((,d . a0) (,u . a1) (,v . a2) (,j . a3) (,c . a4))
                  (fun (foo (A (B a4 a4 (R a1 a2 a0) a0) a3)) (Bart a1 a2 a3 a4 a4))))
         ,(list (build-mapping-for-list '(1 2 3) (symbol-generator-generator 'a))
                '((1 . a0) (2 . a1) (3 . a2)))
         ,(list (gather-vars '(A (B ,c ,c (R ,u ,v ,d) ,d) ,j))
                '(,c ,u ,v ,d ,j))
         ,(list (match-maybe '(bar (B (C ,d ,e))) '(bar (B ,a)))
                '(just ((,a C ,d ,e))))
         ,(list (apply-bindings '(bar (B ,a)) (just-value (match-maybe '(bar (B (C ,d ,e))) '(bar (B ,a)))))
                '(bar (B (C ,d ,e))))
         )))
(test)
;; left: (fun (foo (A a)) (bar (B a)))
;; right: (fun (bar (B (C d e))) 
;; (foo (A (C X Y)))

;(apply-bindings '(bar (B ,a)) (just-value (match-maybe '(bar (B (C ,d ,e))) '(bar (B ,a)))))
