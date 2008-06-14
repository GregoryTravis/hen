(load "lib.ss")

(define forms (read-objects "src.ss"))

(define (preprocess e)
  (cond
   ((or (symbol? e) (literal? e))
    e)
   ((proper-list? e)
    (cons (list 'quote (car e)) (map preprocess (cdr e))))
   (#t (err 'preprocess e))))

(define rules
  (map (lambda (rule) (list (preprocess (cadr rule)) (preprocess (caddr rule)))) forms))

(define (reduce rule)
  (let ((pat (car rule))
        (body (cadr rule)))
    

(shew rules)

(define reduced (map reduce rules))
(shew reduced)
