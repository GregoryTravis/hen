(load "lib.ss")

(define forms (read-objects "src.ss"))

(define (closure? c)
  (and (proper-list? c)
       (= 3 (length c))
       (eq? 'closure (car c))
       (classic-lambda? (cadr c))))

(define (env-lookup s env)
  (let ((a (assoc s env)))
    (if (eq? #f a)
        (err 'env-lookup s env)
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
   ((symbol? e) (env-lookup e env))
   (#t (err 'evl1 e env))))

(define (evl e)
  (evl1 e '()))

(define (top-evl e)
  (display "+ ")
  (shew e)
  (shew (evl e)))

;(tracefun evl evl1 evl-app env-lookup)
;(tracefun classic-lambda? lambda? app? symbol? is-quote?)

(map top-evl forms)
