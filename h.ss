(load "lib.ss")

(define rules '())

(define trace-normal-form #t)

(define primitives-names (list '+ '- '* '/))
(define primitives
  (map (lambda (primitive) (cons primitive (eval primitive))) primitives-names))

(define (constant? k)
  (or (null? k) (numeric? l)))

(define (primitive? e)
  (not (eq? #f (assoc e primitives))))

(define (is-fun-def? o)
  (and (pair? o) (eq? (car o) 'fun) (assert (and (not (null? (cdr o))) (not (null? (cddr o)))))))

(define (is-quote? o)
  (and (pair? o)
       (eq? (car o) 'quote)
       (pair? (cdr o))
       (null? (cddr o))))

(define (quote-quoted o)
  (cadr o))

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

;(define (evl-app e)
;  (normal-form e))
;;   (let ((match (normal-form e)))
;;     (if (null? match)
;;         e
;;         (let ((env (car match))
;;               (body (cdr match)))
;;           (apply-match-env env body)))))

(define (define-rule r)
  (assert (proper-list? r)
          (= (length r) 3))
  (let ((pat (cadr r))
        (body (caddr r)))
    (let ((pat (cons `(quote ,(car pat)) (cdr pat))))
      (set! rules (append rules (list (cons pat body))))
      (display "* ")
      (shew pat))))

(define (find-matching-rule e)
  (first-success
   (lambda (rule)
     (let ((env (match-pat (car rule) e)))
       (if (fail? env)
           fail
           (just (list 'match env rule)))))
   rules))

;; (define (normal-form e)
;;   (if (and (pair? e) (primitive? (car e)))
;;       (evl-primitive e)
;;       (let ((match (find-matching-rule e)))
;;         (if (fail? match)
;;             e
;;             (normal-form (apply-matching-rule (just-value match) e))))))

(define (normal-form-step e)
  (if (and (pair? e) (primitive? (car e)))
      (cons 'not-normal (evl-primitive e))
      (let ((match (find-matching-rule e)))
        (if (fail? match)
            (cons 'normal e)
            (cons 'not-normal (apply-matching-rule (just-value match) e))))))

(define (normal-form e)
  (if trace-normal-form
      (shew e))
  (let* ((r (normal-form-step e))
         (normal-p (car r))
         (re (cdr r)))
    (if (eq? normal-p 'normal)
        re
        (begin
          (display "  ==>\n")
          (normal-form re)))))

(define (apply-matching-rule match e)
  (let* ((env (cadr match))
         (rule (caddr match))
         (body (cdr rule)))
    (rewrite-body env body)))

(define (rewrite-body env body)
  (cond
   ((is-quote? body) body)
   ((pair? body)
    (cons (rewrite-body env (car body))
          (rewrite-body env (cdr body))))
   ((symbol? body)
    (let ((v (assoc body env)))
      (if (eq? #f v)
          body
          (cdr v))))
   ((constant? body) body)
   (#t (err 'rewrite-body env body))))

(define (top-level-deal o)
  (cond
   ((is-fun-def? o) (define-rule o))
   (#t (normal-form o))))

;; Match the pats and es pairwise, and return the concatenation of the result.
;; However, if any of them are #f, it's a failure.
;; And if the pattern is nonlinear, we aren't handling that yet.
(define (match-pat-list pat e)
  (if (not (= (length pat) (length e)))
      fail
      (apply maybe-append (zip match-pat pat e))))

;; Literal match; if the pat matches the value, return the empty
;; environment.
(define (match-pat-quote pat e)
  (if (equal? pat e)
      (just '())
      'fail))

(define (match-pat pat e)
  (cond
   ((is-quote? pat) (match-pat-quote (quote-quoted pat) e))
   ((and (pair? pat) (pair? e)) (match-pat-list pat e))
   ((and (symbol? pat)) (just (list (cons pat e))))
   ((not (eq? (pair? pat) (pair? e))) fail)
   (#t (err 'match-pat pat e))))

;(tracefun evl evl-primitive evl-app primitive? match-pat match-pat-list match-pat-quote normal-form apply-matching-rule rewrite-body find-matching-rule is-fun-def? normal-form-step fail? first-success)

(define (run-file filename)
  (map top-level-deal (read-objects filename)))
