(load "lib.ss")

(define data? symbol?)
(define var? quoted-symbol?)
(define atom? (for data? var?))
(define (lambda? e) (and (pair? e) (eq? '/. (car e))))

(load "prim.ss")

(define (rewrite e src)
  (let ((new-e (rewrite-step e src)))
    (if (equal? new-e e)
        new-e
        (rewrite new-e src))))

(define (rewrite-step e src)
  (cond
   ((equal? e '(current-program)) (reify-src src))
   ((and (pair? e) (eq? 'if (car e))) (rewrite-if e src))
   ((atom? e) e)
   ((pair? e)
    (let ((e (map ($ rewrite _ src) e)))
      (mtch (try-primitive-rewrite e)
            ('just result) result
            _ (rewrite-this-rule-list e src))))
   (#t (err 'rewrite-step e src))))

(define (rewrite-if e src)
  (mtch e ('if b t e)
        (mtch (rewrite b src)
              'True (rewrite t src)
              'False (rewrite e src))))

(define (rewrite-this-rule-list e src)
  (mtch src
        '() e
        (rule . rules) (mtch (rewrite-this e rule)
                             'fail (rewrite-this-rule-list e rules)
                             ('just result) result)))

(define (rewrite-this e rule)
  (if (cton? e)
      (just e)
      (mtch rule
            ('fun pat body)
            (mtch (match-maybe e pat)
                  'fail 'fail
                  ('just bindings) (just (apply-bindings-friendly body bindings))))))

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

(define (gather-vars e)
  (cond
   ((var? e) (list e))
   ((pair? e) (let* ((a-vars (gather-vars (car e)))
                     (d-vars (gather-vars (cdr e))))
                (unique (append a-vars d-vars))))
   (#t '())))

(define (lift-lambda e generator)
  (mtch e
        ('/. pattern body)
        (let ((lifted-name (generator))
              (closed-over (set-difference (gather-vars body) (gather-vars pattern))))
          (list `(,lifted-name . ,closed-over)
                `(fun ((,lifted-name . ,closed-over) . ,pattern) ,body)))))

(define (lift-lambdas-exp e generator)
  (descend-and-substitute
   e
   (lambda (e)
     (mtch e
           ('/. pattern body) (just (lift-lambda e generator))
           _ fail))))

;; Ooky.  I should only be descending through the bodies of the rules,
;; not the patterns as well, but that's more complicated so I'm not
;; doing that.  But it's going to bite me when I write the
;; metacircular interpreter and have /. in a pattern.
(define (lift-lambdas src)
  (mtch (lift-lambdas-exp src (symbol-generator-generator 'lambda-))
        (new-src lifted) (append new-src lifted)))

(define (preprocess src)
  (lift-lambdas src))

(define (run src)
  (assert (check-exp src))
  (rewrite '(main) (preprocess src)))

(define (consify e)
  (cond
   ((or (var? e) (data? e)) e)
   ((null? e) 'Nil)
   ((pair? e) `(Cons ,(consify (car e)) ,(consify (cdr e))))
   (#t (err 'consify e))))

(define reify-src consify)

(define (var-generator-generator prefix)
  (let ((symbol-generator (symbol-generator-generator prefix)))
    (lambda () (list 'quote (symbol-generator)))))

;(tracefun rewrite rewrite-this rewrite-this-rule-list)
;(tracefun match-maybe apply-bindings)
;(tracefun reify-src)
;(tracefun lift-lambdas lift-lambdas-exp lift-lambda)

(define (run-file filename)
  (run (read-objects filename)))
