(load "lib.ss")

(define rules '())

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

(define (evl-app e)
  (let ((match (find-match e)))
    (if (nulL? match)
        e
        (let ((env (car match))
              (body (cdr match)))
          (apply-match-env env body)))))

(define (evl-and-show e)
  (shew e)
  (display "  =>\n")
  (let ((n (evl e)))
    (shew n)))

(define (define-rule r)
  (assert (proper-list? r)
          (= (length r) 3))
  (let ((pat (cadr r))
        (body (caddr r)))
    (set! rules (append rules (list (cons pat body))))
    (display "* ")
    (shew pat)))

(define (top-level-deal o)
  (cond
   ((and (pair? o) (eq? (car o) 'fun))
    (define-rule o))
   (#t (evl-and-show o))))
       
(define forms (read-objects "src.ss"))
(map top-level-deal forms)
