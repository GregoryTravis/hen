(load "lib.ss")
(load "primitives.ss")

(define rules '())

(define trace-normal-form #f)
(define trace-rule-definitions #f)

(define (constant? k)
  (or (null? k) (number? k)))

(define (is-fun-def? o)
  (and (pair? o) (eq? (car o) 'fun) (assert (and (not (null? (cdr o))) (not (null? (cddr o)))))))

(define (is-quote? o)
  (and (pair? o)
       (eq? (car o) 'quote)
       (pair? (cdr o))
       (null? (cddr o))))

(define (quote-quoted o)
  (cadr o))

(define (apply-primitive e)
  (assert (primitive? (car e)))
  (let ((real-name (string->symbol (++ "primitive-" (car e)))))
    (if (not (special-form? (car e)))
        (set! e (normal-form-children e)))
    (apply (eval real-name) (cdr e))))

(define (define-rule r)
  (assert (proper-list? r)
          (= (length r) 3))
  (let ((pat (cadr r))
        (body (caddr r)))
    (let ((pat (cons `(quote ,(car pat)) (cdr pat))))
      (set! rules (append rules (list (cons pat body))))
      (if trace-rule-definitions
          (begin
            (display "* ")
            (shew pat))))))

(define (find-matching-rule e)
  (first-success
   (lambda (rule)
     (let ((env (match-pat (car rule) e)))
       (if (fail? env)
           fail
           (just (list 'match env rule)))))
   rules))

(define trace-normal-form-level 0)

(define (normal-form-step e)
  ;(shew 'oof e (primitive? e))
  (if (not (pair? e))
      (cons 'normal e)
      (if (primitive? (car e))
          (cons 'not-normal (apply-primitive e))
          (let ((e (normal-form-children e)))
            (let ((match (find-matching-rule e)))
              (if (fail? match)
                  (cons 'normal e)
                  (cons 'not-normal (apply-matching-rule (just-value match) e))))))))

(define (normal-form-iterate e)
  (if trace-normal-form
      (begin
        (display (make-string-string trace-normal-form-level "| "))
        (display "+  ")
        (lshew e)
        (display "\n")))
  (let* ((r (normal-form-step e))
         (normal-p (car r))
         (re (cdr r)))
    (if (eq? normal-p 'normal)
        re
        (normal-form-iterate re))))

(define (normal-form-children e)
  (set! trace-normal-form-level (+ trace-normal-form-level 1))
  (let ((r (cons (car e) (map normal-form (cdr e)))))
    (set! trace-normal-form-level (- trace-normal-form-level 1))
    r))

(define (normal-form e)
  (let ((r (normal-form-iterate e)))
    (if trace-normal-form
        (begin
          (display (make-string-string trace-normal-form-level "| "))
          (display "-> ")
          (lshew r)
          (display "\n")))
    r))

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

(define (normal-form-top e)
  (if (not trace-normal-form)
      (begin
        (display "+  ")
        (shew e)))
  (let ((r (normal-form e)))
    (if (not trace-normal-form)
        (begin
          (display "=> ")
          (shew r)))
    r))

(define (top-level-deal o)
  (cond
   ((is-fun-def? o) (define-rule o))
   (#t (normal-form-top o))))

;; Match the pats and es pairwise, and return the concatenation of the result.
;; However, if any of them are #f, it's a failure.
;; And if the pattern is nonlinear, we aren't handling that yet.
(define (match-pat-list pat e)
  (if (not (= (length pat) (length e)))
      fail
      (apply maybe-append (zip match-pat pat e))))

;; Literal match; if the pat matches the value, return the empty
;; environment.
(define (match-constants pat e)
  (if (equal? pat e)
      (just '())
      'fail))

(define (match-pat pat e)
  (cond
   ((is-quote? pat) (match-constants (quote-quoted pat) e))
   ((and (pair? pat) (pair? e)) (match-pat-list pat e))
   ((and (symbol? pat)) (just (list (cons pat e))))
   ((not (eq? (pair? pat) (pair? e))) fail)
   ((constant? pat) (match-constants pat e))
   (#t (err 'match-pat pat e))))

;(tracefun primitive? match-pat match-pat-list match-constants normal-form normal-form-step normal-form-iterate normal-form-children apply-matching-rule apply-primitive rewrite-body find-matching-rule is-fun-def? first-success primitive-==)
;(tracefun primitive? match-pat match-pat-list match-constants apply-matching-rule find-matching-rule)

(define (run-file filename)
  (map top-level-deal (read-objects filename)))

(run-file "prelude.ss")
