(load "lib.ss")

(define rules '())

(define primitives-names (list '+ '- '* '/))
(define primitives
  (map (lambda (primitive) (cons primitive (eval primitive))) primitives-names))

(define (primitive? e)
  (not (eq? #f (assoc e primitives))))

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

(define (evl-app e)
  (let ((match (find-match e)))
    (if (null? match)
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
    (let ((pat (cons `(quote ,(car pat)) (cdr pat))))
      (set! rules (append rules (list (cons pat body))))
      (display "* ")
      (shew pat))))

(define (top-level-deal o)
  (cond
   ((and (pair? o) (eq? (car o) 'fun))
    (define-rule o))
   (#t (evl-and-show o))))

;; Match the pats and es pairwise, and return the concatenation of the result.
;; However, if any of them are #f, it's a failure.
;; And if the pattern is nonlinear, we aren't handling that yet.
(define (match-this-pat-list pat e)
  (if (not (= (length pat) (length e)))
      #f
      (let ((env (apply append (zip match-this-pat pat e))))
        (cond
         ((any (lambda (x) (eq? #f x)) env) #f)
         ((has-duplicates? env) (err 'nonlinear-pattern pat))
         (#t env)))))

;; Literal match; if the pat matches the value, return the empty
;; environment.
(define (match-this-pat-quote pat e)
  (if (equal? pat e)
      '()
      #f))

(define (match-this-pat pat e)
  (cond
   ((is-quote? pat) (match-this-pat-quote (quote-quoted pat) e))
   ((and (pair? pat) (pair? e)) (match-this-pat-list pat e))
   ((and (symbol? pat)) (list (cons pat e)))
   (#t #f)))

(define (match-this-pat-top rule e)
  (let ((r (match-this-pat (car rule) e)))
    (if (eq? #f r)
        #f
        (cons r (cdr rule)))))

;; Returns (pat . body)
(define (find-match e)
  (let ((r (first-success (lambda (rule) (match-this-pat-top rule e)) rules)))
    (if (eq? #f r)
        (err 'no-match e)
        (car r))))

(define (apply-match-env env e)
  (cond
   ((pair? e)
    (cons (apply-match-env env (car e))
          (apply-match-env env (cdr e))))
   ((symbol? e)
    (let ((v (assoc e env)))
      (if (eq? #f v)
          e
          (cdr v))))
   (#t e)))

;(tracefun evl evl-primitive evl-app primitive? apply-match-env find-match match-this-pat match-this-pat-list match-this-pat-quote match-this-pat-top)

(define forms (read-objects "src.ss"))
(map top-level-deal forms)
