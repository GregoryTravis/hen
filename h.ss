(load "lib.ss")

(define global-env '())

(define (closure? c)
  (and (proper-list? c)
       (= 3 (length c))
       (eq? 'closure (car c))
       (classic-lambda? (cadr c))))

(define (define? e)
  (and (proper-list? e)
       (= 3 (length e))
       (eq? 'define (car e))
       (symbol? (cadr e))))

(define (lookup-local-or-global s env)
  (let ((a (assoc s env)))
    (if (eq? #f a)
        (let ((a (assoc s global-env)))
          (if (eq? #f a)
              (err 'lookup-local-or-global s env)
              (cdr a)))
        (cdr a))))

(define (evl-app e env)
  (assert (app? e))
  (let* ((closure (evl1 (car e) env))
         (arg (evl1 (cadr e) env))
         (lamb (cadr closure))
         (closure-env (caddr closure))
         (param-name (cadr lamb))
         (body (caddr lamb))
         (new-env (cons (cons param-name arg) closure-env)))
    (evl1 body new-env)))

(define (evl1 e env)
  (cond
   ((or (literal? e) (is-quote? e)) e)
   ((closure? e) (evl-closure e))
   ((classic-lambda? e) `(closure ,e ,env))
   ((app? e) (evl-app e env))
   ((symbol? e) (lookup-local-or-global e env))
   (#t (err 'evl1 e env))))

(define (evl e)
  (evl1 e '()))

(define (top-evl e)
  (display "+ ")
  (shew e)
  (shew (evl e)))

(define (process-define e)
  (assert (define? e))
  (display "+ ")
  (shew e)
  (let ((name (cadr e))
        (value (evl (caddr e))))
    (set! global-env (cons (cons name value) global-env))
    (shew name)))

(define (process-top-level-form e)
  (cond
   ((define? e) (process-define e))
   (#t (top-evl e))))

;(tracefun evl evl1 evl-app lookup-local-or-global process-define process-top-level-form)
;(tracefun classic-lambda? lambda? app? symbol? is-quote?)

(define (exec-file filename)
  (let ((forms (read-objects filename)))
    (map process-top-level-form forms)))
