(load "lib.ss")

(define primitives-names (list '+ '- '* '/))
(define primitives
  (map (lambda (primitive) (cons primitive (eval primitive))) primitives-names))

(define (primitive? e)
  (not (eq? '() (assoc e primitives))))

(define (evl-primitive e)
  (apply (cdr (assoc (car e) primitives)) (cdr e)))

(define (evl-app e)
  (err '?))

(define (evl e)
  (cond
   ((and (proper-list? e) (primitive? (car e)))
    (evl-primitive e))
   ((proper-list? e)
    (evl-app e))
   (#t (err 'what-is e))))

(define (evl-and-show e)
  (shew e)
  (display "  =>\n")
  (let ((n (evl e)))
    (shew n)))
       
(define forms (read-objects "src.ss"))
(map evl-and-show forms)
