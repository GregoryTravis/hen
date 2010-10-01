(load "lib.ss")

(define data? symbol?)
(define var? quoted-symbol?)

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
              ('just bindings) (just (apply-bindings-friendly body bindings)))))

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

(define (apply-bindings-friendly e bindings)
  (cond
   ((null? e) e)
   ((data? e) e)
   ((var? e) (lookup-or-key e bindings))
   ((pair? e) (cons (apply-bindings-friendly (car e) bindings) (apply-bindings-friendly (cdr e) bindings)))
   (#t (err 'apply-bindings-friendly e bindings))))

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

(define reify-src consify)

(define (run-test test) (mtch test (a b) (if (equal? a b) 'ok `(fail ,a ,b))))

(define (gather-vars e)
  (cond
   ((var? e) (list e))
   ((pair? e) (let* ((a-vars (gather-vars (car e)))
                     (d-vars (gather-vars (cdr e))))
                (unique (append a-vars d-vars))))
   (#t '())))

(define (var-generator-generator prefix)
  (let ((symbol-generator (symbol-generator-generator prefix)))
    (lambda () (list 'quote (symbol-generator)))))

(define (alpha-rename-rule rule symbol-generator)
  (mtch rule
        ('fun pattern body)
        (let ((bindings (build-mapping-for-list (gather-vars (list pattern body)) symbol-generator)))
          `(fun ,(apply-bindings pattern bindings) ,(apply-bindings body bindings)))))

(define (unify-rules left right)
  (mtch (list left (alpha-rename-rule right (var-generator-generator 'a)))
        (('fun left-pattern left-body) ('fun right-pattern right-body))
        (mtch (blurg left-body right-pattern)
              (just bindings)
              `((fun ,(apply-bindings-friendly left-pattern bindings) ,(apply-bindings-friendly left-body bindings))
                (fun ,(apply-bindings-friendly right-pattern bindings) ,(apply-bindings-friendly right-body bindings))))))

(define (chain-rules left right)
  (mtch (unify-rules left right)
        (('fun left-pattern left-body) ('fun right-pattern right-body))
        (begin
          (assert (equal? left-body right-pattern))
          `(fun ,left-pattern ,right-body))))

;; (define (unify e0 e1)
;;   (cond
;;    ((and (null? e0) (null? e1)) (just '()))
;;    ((and (var? e0) (var? e1)) (just e0))
;;    ((var? e0) (just e1))
;;    ((var? e1) (just e0))
;;    ((and (data? e0) (data? e1)) (if (equal? e0 e1) (just e0) fail))
;;    ((and (pair? e0) (pair? e1)) (maybe-cons (unify (car e0) (car e1)) (unify (cdr e0) (cdr e1))))
;;    (#t (err 'unify e0 e1))))

;; Unify can thus be written by calling blurg to get the bindings and
;; then applying them to either term.  This produces the same results,
;; except in the case of two vars, where you will get either one or
;; the other.
(define (unify e0 e1)
  (mtch (blurg e0 e1)
        ('just bindings) (just (apply-bindings-friendly e0 bindings))
        'fail fail))

;; Bindings (substitution, really) to make two terms identical.  But
;; see comment on (unify).
(define (blurg e0 e1)
  (cond
   ((and (null? e0) (null? e1)) (just '()))
   ((and (var? e0) (var? e1) (equal? e0 e1)) (just '()))
   ((and (var? e0) (var? e1)) (just '()))
   ((var? e0) (just `((,e0 . ,e1))))
   ((var? e1) (just `((,e1 . ,e0))))
   ((and (data? e0) (data? e1)) (if (equal? e0 e1) (just '()) fail))
   ((and (pair? e0) (pair? e1)) (maybe-append (blurg (car e0) (car e1)) (blurg (cdr e0) (cdr e1))))
   (#t (err 'blurg e0 e1))))

(define (test)
  (let ((results
         (let ((some-funs '((fun (map 'f Nil) ('f Nil))
                            (fun (map 'f (Cons 'a 'd)) (Cons ('f 'a) (map 'f 'd)))
                            (fun (swap (P 'a 'd)) (P 'd 'a))
                            (fun (apply 'f . 'a) ('f . 'a)))))

           (map run-test
                `(
                  ,(list (match-maybe '(a B R c e) '('d B R 'f 'g)) '(just (('d . a) ('f . c) ('g . e))))
                  (,(match-maybe '('a 'B 'R 'c 'e) '(d 'B 'Rr f g)) fail)
                  (,(map ($ apply-bindings _ '(('d . a) ('f . c) ('g . e))) '('d 'f 'g Joe ('d 'f 'g Joe))) (a c e Joe (a c e Joe)))
                  (,(run '(boot (Cons Dop Nil)) '((fun (boot (Cons 'a Nil)) (Cons 'a (Cons 'a Nil))))) (Cons Dop (Cons Dop Nil)))
                  (,(run '(boot (Cons Dop Nil)) '((fun (boot (Cons 'a Nil)) (Cons 'a (Cons 'a Nil)))
                                                  (fun (boote (Cons 'a Nil)) (Cons 'a (Cons 'a Nil))))) (Cons Dop (Cons Dop Nil)))
                  (,(run '(boot (Cons Dop Nil)) '((fun (boote (Cons 'a Nil)) (Cons 'a (Cons 'a Nil)))
                                                  (fun (boot (Cons 'a Nil)) (Cons 'a (Cons 'a Nil))))) (Cons Dop (Cons Dop Nil)))
                  (,(run '(boote (Cons Dop Nil)) '((fun (boot (Cons 'a Nil)) (Cons 'a (Cons 'a Nil)))
                                                   (fun (boote (Cons 'a Nil)) (Cons 'a (Cons 'a Jerk))))) (Cons Dop (Cons Dop Jerk)))
                  (,(run '(boote (Cons Dop Nil)) '((fun (boote (Cons 'a Nil)) (Cons 'a (Cons 'a Jerk)))
                                                   (fun (boot (Cons 'a Nil)) (Cons 'a (Cons 'a Nil))))) (Cons Dop (Cons Dop Jerk)))
                  (,(run '(dumbify (Cons A (Cons B (Cons C Nil))))
                         '((fun (dumbify Nil) Nil) (fun (dumbify (Cons 'a 'd)) (Cons (Cons Dumb 'a) (dumbify 'd)))))
                   (Cons (Cons Dumb A) (Cons (Cons Dumb B) (Cons (Cons Dumb C) Nil))))
                  (,(run '(= Joe Joe) '()) True)
                  (,(run '(car (Cons Haha Nil)) '((fun (car (Cons 'a 'd)) 'a) (fun (cdr (Cons 'a 'd)) 'd) (fun (cons 'a 'd) (Cons 'a 'd)))) Haha)
                  (,(run '(car (Cons Haha (Cons HoHo Nil))) '((fun (car (Cons 'a 'd)) 'a) (fun (cdr (Cons 'a 'd)) 'd) (fun (cons 'a 'd) (Cons 'a 'd)))) Haha)
                  (,(run '(cdr (Cons Haha Nil)) '((fun (car (Cons 'a 'd)) 'a) (fun (cdr (Cons 'a 'd)) 'd) (fun (cons 'a 'd) (Cons 'a 'd)))) Nil)
                  (,(run '(cdr (Cons Haha (Cons HoHo Nil))) '((fun (car (Cons 'a 'd)) 'a) (fun (cdr (Cons 'a 'd)) 'd) (fun (cons 'a 'd) (Cons 'a 'd)))) (Cons HoHo Nil))
                  (,(run '(var? 'e) '()) True)
                  (,(run '(var? e) '()) False)
                  (,(run '(data? 'e) '()) False)
                  (,(run '(data? e) '()) True)
                  (,(run 'Foo '()) Foo)
                  ,(list (run '(current-program) '((fun (boote (Cons 'a Nil)) (Cons 'a (Cons 'a Jerk)))
                                                   (fun (boot (Cons 'a Nil)) (Cons 'a (Cons 'a Nil)))))
                         '(Cons (Cons fun (Cons (Cons boote (Cons (Cons Cons (Cons 'a (Cons Nil Nil))) Nil))
                                                (Cons (Cons Cons (Cons 'a (Cons (Cons Cons (Cons 'a (Cons Jerk Nil))) Nil))) Nil)))
                                (Cons (Cons fun (Cons (Cons boot (Cons (Cons Cons (Cons 'a (Cons Nil Nil))) Nil))
                                                      (Cons (Cons Cons (Cons 'a (Cons (Cons Cons (Cons 'a (Cons Nil Nil))) Nil))) Nil))) Nil)))
                  (,(run '(map swap (Cons (P A B) (Cons (P C D) Nil))) some-funs) (Cons (P B A) (Cons (P D C) (swap Nil))))
                  (,(run '(apply swap (P A B)) some-funs) (P B A))

                  ,(list (build-mapping-for-list '(1 2 3) (var-generator-generator 'a)) '((1 . 'a0) (2 . 'a1) (3 . 'a2)))
                  ,(list (gather-vars '(A (B 'c 'c (R 'u 'v 'd) 'd) 'j)) '('c 'u 'v 'd 'j))
                  ,(list (alpha-rename-rule '(fun (foo (A (B 'c 'c (R 'u 'v 'd) 'd) 'j)) (Bart 'u 'v 'j 'c 'c))
                                            (var-generator-generator 'a))
                         '(fun (foo (A (B 'a4 'a4 (R 'a1 'a2 'a0) 'a0) 'a3)) (Bart 'a1 'a2 'a3 'a4 'a4)))
                  ,(list (build-mapping-for-list '(1 2 3) (var-generator-generator 'a))
                         '((1 . 'a0) (2 . 'a1) (3 . 'a2)))
                  ,(list (gather-vars '(A (B 'c 'c (R 'u 'v 'd) 'd) 'j))
                         '('c 'u 'v 'd 'j))
                  ,(list (match-maybe '(bar (B (C 'd 'e))) '(bar (B 'a)))
                         '(just (('a C 'd 'e))))
                  ,(list (match-maybe '(bar (B 'a)) '(bar (B (C 'd 'e))))
                         'fail)
                  ,(list (apply-bindings '(bar (B 'a)) (just-value (match-maybe '(bar (B (C 'd 'e))) '(bar (B 'a)))))
                         '(bar (B (C 'd 'e))))

                  ,(list (unify 'a 'a) (just 'a))
                  ,(list (unify 'a 'b) 'fail)
                  ,(list (unify '(a . b) '(a . b)) (just '(a . b)))
                  ,(list (unify '(a . b) '(a . x)) 'fail)
                  ,(list (unify '(a . x) '(a . b)) 'fail)
                  ,(list (unify '(a . (b . c)) '(a . (b . c))) (just '(a . (b . c))))
                  ,(list (unify '(a . (b . c)) '(x . (b . c))) fail)
                  ,(list (unify '(a . (b . c)) '(a . (x . c))) fail)
                  ,(list (unify '(a . (b . c)) '(a . (b . x))) fail)
                  ,(list (unify ''v 'a) (just 'a))
                  ,(list (unify 'a '(quote v)) (just 'a))
                  ,(list (unify ''v '(a . b)) (just '(a . b)))
                  ,(list (unify '(a . b) ''v) (just '(a . b)))
                  ,(list (unify ''u ''v) (just ''u))
                  ,(list (unify '(A 'u) '(A 'v)) (just '(A 'u)))
                  ,(list (unify '('a . ('c . 'd)) '(('e . 'f) . 'g))
                         (just '(('e . 'f) . ('c . 'd))))
                  ,(list (unify '(bar (B 'a          (P 'j 'i) ) )
                                '(bar (B (C 'd 'e)   'q        ) ))
                         '(just (bar (B (C 'd 'e) (P 'j 'i)))))

                  ,(list (blurg '(bar (B 'a          (P 'j 'i) ) )
                                '(bar (B (C 'd 'e)   'q        ) ))
                         '(just (('a C 'd 'e) ('q P 'j 'i))))
                  ,(list (apply-bindings-friendly '(bar (B 'a          (P 'j 'i) ) )
                                                  (just-value (blurg '(bar (B 'a          (P 'j 'i) ) ) '(bar (B (C 'd 'e)   'q        ) ))))
                         '(bar (B (C 'd 'e) (P 'j 'i))))
                  ,(list (apply-bindings-friendly '(bar (B (C 'd 'e)   'q        ) )
                                                  (just-value (blurg '(bar (B 'a          (P 'j 'i) ) ) '(bar (B (C 'd 'e)   'q        ) ))))
                         '(bar (B (C 'd 'e) (P 'j 'i))))

                  ,(list (unify-rules '(fun (foo (A 'a       ) (G (H 'i 'j)) ) (bar (B 'a        (P 'j 'i) )))
                                      '(fun                                    (bar (B (C 'd 'e)   'q        ) )  (T 'q        'e 'd) ))
                         '((fun (foo (A (C 'a2 'a1)) (G (H 'i 'j))) (bar (B (C 'a2 'a1) (P 'j 'i))))
                           (fun (bar (B (C 'a2 'a1) (P 'j 'i))) (T (P 'j 'i) 'a1 'a2))))

                  ,(list (chain-rules '(fun (foo (A 'a       ) (G (H 'i 'j)) ) (bar (B 'a        (P 'j 'i) )))
                                      '(fun                                    (bar (B (C 'd 'e)   'q        ) )  (T 'q        'e 'd) ))
                         '(fun (foo (A (C 'a2 'a1)) (G (H 'i 'j))) (T (P 'j 'i) 'a1 'a2)))

                  ,(list (blurg '(A 'a) '(B 'a)) fail)
                  ,(list (unify-rules '(fun (A 'a) 'a) '(fun (B 'a) 'a)) '((fun (A (B 'a0)) (B 'a0)) (fun (B 'a0) 'a0)))
                  ,(list (chain-rules '(fun (A 'a) 'a) '(fun (B 'a) 'a)) '(fun (A (B 'a0)) 'a0))
                  ,(list (chain-rules '(fun (A 'a) 'a) '(fun (B 'a) (C 'a 'a))) '(fun (A (B 'a0)) (C 'a0 'a0)))
                  )))))
    (if (all? (map ($ eq? _ 'ok) results))
        '(ok)
        results)))

;(tracefun rewrite rewrite-this rewrite-this-rule-list)
;(tracefun match-maybe apply-bindings)
;(tracefun reify-src)

;(test)
(run '(main) (read-objects "src.k"))
;(run '(main) (read-objects "test.k"))
