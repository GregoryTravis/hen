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

(define (var-generator-generator prefix)
  (let ((symbol-generator (symbol-generator-generator prefix)))
    (lambda () (list 'unquote (symbol-generator)))))

(define (alpha-rename-rule rule symbol-generator)
  (mtch rule
        ('fun pattern body)
        (let ((bindings (build-mapping-for-list (gather-vars (list pattern body)) symbol-generator)))
          `(fun ,(apply-bindings pattern bindings) ,(apply-bindings body bindings)))))
;          `(,bindings (fun ,(apply-bindings pattern bindings) ,(apply-bindings body bindings))))))

(define (test)
  (map run-test
       `(
         (,(build-mapping-for-list '(1 2 3) (var-generator-generator 'a)) ((1 . a0) (2 . a1) (3 . a2)))
         ,(list (gather-vars '(A (B ,c ,c (R ,u ,v ,d) ,d) ,j)) '(,c ,u ,v ,d ,j))
         ,(list (alpha-rename-rule '(fun (foo (A (B ,c ,c (R ,u ,v ,d) ,d) ,j)) (Bart ,u ,v ,j ,c ,c))
                                   (var-generator-generator 'a))
                '(((,d . a0) (,u . a1) (,v . a2) (,j . a3) (,c . a4))
                  (fun (foo (A (B a4 a4 (R a1 a2 a0) a0) a3)) (Bart a1 a2 a3 a4 a4))))
         ,(list (build-mapping-for-list '(1 2 3) (var-generator-generator 'a))
                '((1 . a0) (2 . a1) (3 . a2)))
         ,(list (gather-vars '(A (B ,c ,c (R ,u ,v ,d) ,d) ,j))
                '(,c ,u ,v ,d ,j))
         ,(list (match-maybe '(bar (B (C ,d ,e))) '(bar (B ,a)))
                '(just ((,a C ,d ,e))))
         ,(list (match-maybe '(bar (B ,a)) '(bar (B (C ,d ,e))))
                'fail)
         ,(list (apply-bindings '(bar (B ,a)) (just-value (match-maybe '(bar (B (C ,d ,e))) '(bar (B ,a)))))
                '(bar (B (C ,d ,e))))
         )))

(define (unify-rules left right)
  (mtch (list left (alpha-rename-rule right (var-generator-generator 'a)))
        (('fun left-pattern left-body) ('fun right-pattern right-body))
        (mtch (sr (unify left-body right-pattern))
              (just bindings)
              `((fun ,(apply-bindings left-pattern bindings) ,(apply-bindings left-body bindings))
                (fun ,(apply-bindings right-pattern bindings) ,(apply-bindings right-body bindings))))))

(define (unify e0 e1)
  (cond
   ((and (null? e0) (null? e1)) (just '()))
   ((and (var? e0) (var? e1)) (just e0))
   ((var? e0) (just e1))
   ((var? e1) (just e0))
   ((and (data? e0) (data? e1)) (if (equal? e0 e1) (just e0) fail))
   ((and (pair? e0) (pair? e1)) (maybe-cons (unify (car e0) (car e1)) (unify (cdr e0) (cdr e1))))
   (#t (err 'unify e0 e1))))

;; Bindings (substitution, really) to make two terms identical
(define (blurg e0 e1)
  (cond
   ((and (null? e0) (null? e1)) (just '()))
   ((and (var? e0) (var? e1)) (just `((,e0 . ,e0))))
   ((var? e0) (just `((,e0 . ,e1))))
   ((var? e1) (just `((,e1 . ,e0))))
   ((and (data? e0) (data? e1)) (just '()))
   ((and (pair? e0) (pair? e1)) (maybe-append (blurg (car e0) (car e1)) (blurg (cdr e0) (cdr e1))))
   (#t (err 'blurg e0 e1))))

(define (test)
  (map run-test
       `(
;;              ,(list (unify-rules '(fun (foo (A ,a       ) (G (H ,i ,j)) ) (bar (B ,a        (P ,j ,i) )))
;;                                  '(fun                                    (bar (B (C ,d ,e)   ,q        ) )  (T ,q        ,e ,d) ))
;;                     '(fun (bar (B (C a2 a1) a0)) (T a0 a1 a2)))

             ,(list (unify 'a 'a) (just 'a))
             ,(list (unify 'a 'b) 'fail)
             ,(list (unify '(a . b) '(a . b)) (just '(a . b)))
             ,(list (unify '(a . b) '(a . x)) 'fail)
             ,(list (unify '(a . x) '(a . b)) 'fail)
             ,(list (unify '(a . (b . c)) '(a . (b . c))) (just '(a . (b . c))))
             ,(list (unify '(a . (b . c)) '(x . (b . c))) fail)
             ,(list (unify '(a . (b . c)) '(a . (x . c))) fail)
             ,(list (unify '(a . (b . c)) '(a . (b . x))) fail)
         ,(list (unify ',v 'a) (just 'a))
         ,(list (unify 'a '(unquote v)) (just 'a))
         ,(list (unify ',v '(a . b)) (just '(a . b)))
         ,(list (unify '(a . b) ',v) (just '(a . b)))
         ,(list (unify ',u ',v) (just ',u))
         ,(list (unify '(,a . (,c . ,d)) '((,e . ,f) . ,g))
                (just '((,e . ,f) . (,c . ,d))))
         ,(list (unify '(bar (B ,a          (P ,j ,i) ) )
                       '(bar (B (C ,d ,e)   ,q        ) ))
                '(just (bar (B (C ,d ,e) (P ,j ,i)))))

         ,(list (blurg '(bar (B ,a          (P ,j ,i) ) )
                       '(bar (B (C ,d ,e)   ,q        ) ))
                '(just ((,a C ,d ,e) (,q P ,j ,i))))

         )))

;(tracefun unify var? data?)

(test)
;(build-mapping-for-list '(1 2 3) (var-generator-generator 'a))

;;  left: (fun (foo (A ,a       ) (G (H ,i ,j)) ) (bar (B ,a          (P ,j ,i) ) )                      )
;; right: (fun                                    (bar (B (C ,d ,e)   ,q        ) )  (T ,q        ,e ,d) )
;;  red0:      (foo (A (C  X  Y)) (G (H  W  Z)) ) (bar (B (C  X  Y)   (P Z  W)  ) )
;;  red1:                                         (bar (B (C  X  Y)   (P Z  W)  ) )  (T (P  Z  W) Y  X)
;;  both: (fun (foo (A (C ,x ,y)) (G (H ,i ,j)) )                                    (T (P ,j ,i) ,y ,x) )
