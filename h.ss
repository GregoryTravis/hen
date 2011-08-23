(load "lib.ss")

;; Three types of terms: constants, variables, and pairs.
;;
;; Preprocessing:

(define (preprocess-pattern e)
  (cond
   ((ctor? e) `(ctor ,e))
   ((symbol? e) `(var ,e))
   ((pair? e) `(pair ,(preprocess-pattern (car e)) ,(preprocess-pattern (cdr e))))
   ((null? e) '(nil))
   (#t (err "what is" e))))

(define (main)
  (map preprocess-pattern
       '(a
         Foo
         (Foo a b)
         (Foo a (Bar b)))))
