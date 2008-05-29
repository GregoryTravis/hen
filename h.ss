(load "lib.ss")

(define (preprocess pat)
  (if (pair? pat)
      (cons `(quote ,(car pat)) (map preprocess (cdr pat)))
      pat))

(define (linearize pat)
  (assert (proper-list? pat))
  (cond
   ((null? pat) '())
   ((null? (car pat)) (cons (car pat) (linearize (cdr pat))))
   ((and (is-quote? (car pat)) (pair? (cadar pat))) (err 'linearize pat))
   ((is-quote? (car pat)) (cons (car pat) (linearize (cdr pat))))
   ((pair? (car pat)) (linearize (cons ''p (cons (caar pat) (cons (cdar pat) (cdr pat))))))
   ((atom? (car pat)) (cons (car pat) (linearize (cdr pat))))
   (#t (err 'linearize pat))))

;(tracefun linearize)

(define pat '(larp a (finch b) (garp c d)))
(set! pat (preprocess pat))
(shew pat)
(shew (linearize pat))

;(define forms (read-objects "src.ss"))
