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

(define (var-generator-generator prefix)
  (let ((symbol-generator (symbol-generator-generator prefix)))
    (lambda () (list 'quote (symbol-generator)))))

;(tracefun rewrite rewrite-this rewrite-this-rule-list)
;(tracefun match-maybe apply-bindings)
;(tracefun reify-src)

(define (run-file filename)
  (run '(main) (read-objects filename)))
