(load "lib.ss")

;; Three types of terms: constants, variables, and pairs.
;;
;; Preprocessing:

(define (preprocess-pattern e)
  (cond
   ((or (ctor? e) (number? e)) `(const ,e))
   ((symbol? e) `(var ,e))
   ((pair? e) `(pair ,(preprocess-pattern (car e)) ,(preprocess-pattern (cdr e))))
   ((null? e) '(const Nil))
   (#t (err "what is" e))))

(define (unpreprocess e)
  (mtch e
        ('const k) k
        ('pair a d) (cons a d)
        ('var v) v))

(define (match-pattern pattern e)
  (mtch (list pattern e)
        (('const k) ('const e)) (if (equal? k e) `(just ()) 'fail)
        (('var v) e) `(just (,(list v e)))
        (('pair pa pd) ('pair ea ed)) (maybe-append (match-pattern pa ea) (match-pattern pd ed))))

(define (try-rule pattern body e)
  (mtch (env (match-pattern pattern e))
        'fail 'fail
        (just env) (mtch (substitute env body)
                         ('unbound var) `(unbound ,var)
                         _ body)))

(define (main)
  (map
   (lambda (x)
     (mtch x
           (pattern e) (match-pattern (preprocess-pattern pattern) (preprocess-pattern e))))
   (scoop-by 2
             '(
               Nil Nil
               a 1
               a Nil
               (Foo a b) (Foo 1 2)
               (Foo a b) (Foo (Bar 2 1) (Baz 4 3))
               ))))
