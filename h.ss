(load "lib.ss")
(load "primitives.ss")

(define global-env '())

(define sgen (symbol-generator-generator))

(set! global-env (append (find-primitives) global-env))

(define (closure? c)
  (and (proper-list? c)
       (= 3 (length c))
       (eq? 'closure (car c))
       (lambda? (cadr c))))

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

(define (evl-app-closure e env)
  (assert (closure? (car e)))
  (let* ((closure (evl1 (car e) env))
         (arg (evl1 (cadr e) env))
         (lamb (cadr closure))
         (closure-env (caddr closure))
         (param-name (cadr lamb))
         (body (caddr lamb))
         (new-env (cons (cons param-name arg) closure-env)))
    (evl1 body new-env)))

(define (evl-app-primitive e env)
  (assert (primitive? (car e)))
  (let* ((prim (car e))
         ;(args (map (lambda (x) (evl1 x env)) (cdr e)))
         (args (cdr e))
         (fun (cadr prim)))
    (apply fun args)))

(define (evl-app e env)
;(shew e (app? e) (closure? (car e)) (primitive? (car e)))
  (assert (app? e))
  (let ((e (map (lambda (x) (evl1 x env)) e)))
    (cond
     ((closure? (car e)) (evl-app-closure e env))
     ((primitive? (car e)) (evl-app-primitive e env))
     (#t (err 'evl-app e env)))))

;; (define (evl-conditional e env)
;;   (assert (conditional? e))
;;   (let ((if-part (cadr e))
;;         (then-part (caddr e))
;;         (else-part (if (null? (cdddr e))
;;                        '()
;;                        (cadddr e))))
;;     (let ((v (evl1 if-part env)))
;;       (if (not (boolean? v))
;;           (err if-part-not-a-boolean e env)
;;           (if (eq? v 'true)
;;               (evl1 then-part env)
;;               (evl1 else-part env))))))

(define (evl1 e env)
  (cond
   ((or (literal? e) (is-quote? e) (closure? e)) e)
;   ((conditional? e) (evl-conditional e env))
;   ((pair?-exp? e) (if (pair? (cadr e)) 'true 'false))
   ;((closure? e) (evl-app-closure e))
   ((primitive? e) (evl-primitive e env))
   ((lambda? e) `(closure ,e ,env))
   ((app? e) (evl-app e env))
   ((symbol? e) (lookup-local-or-global e env))
   (#t (err 'evl1 e env))))

(define (evl e)
  (evl1 e '()))

(define (process-define e)
  (assert (define? e))
;  (display "+ ")
;  (shew e)
  (let ((name (cadr e))
        (value (evl (caddr e))))
    (set! global-env (cons (cons name value) global-env))
    (shew name)))

(define (quote-literal-maybe x)
  (if (eq? x '())
      ''()
      x))

(define (simplify-pattern-lambdas e)
  (cond
   ((literal? e) e)
   ((symbol? e) e)
   ((lambda? e)
    (let ((pat (cadr e))
          (body (caddr e)))
      (cond
       ((literal? pat)
        (let ((arg (sgen)))
          `(/. ,arg
             (if (equal? ,arg ,(quote-literal-maybe pat))
                 ,(simplify-pattern-lambdas body)
                 fail))))
       ((symbol? pat)
        `(/. ,pat ,(simplify-pattern-lambdas body)))
       ((pair? pat)
        (let* ((pair-arg (sgen))
               (receiver
                (simplify-pattern-lambdas
                 `(/. ,(car pat) (/. ,(cdr pat) ,body)))))
          `(/. ,pair-arg
               (if (pair? ,pair-arg)
                   (,receiver (car ,pair-arg) (cdr ,pair-arg))
                   fail))))
       (#t (err 'simplify-pattern-lambdas 'bad-lambda-pat e)))))
   ((and (pair? e) (not (lambda? e))) (map simplify-pattern-lambdas e))
   (#t (err 'simplify-pattern-lambdas e))))

(define (preprocess-exp e)
  (compile-lambda-rewrites e))

(define (preprocess e)
  (cond
   ((define? e) e)
   (#t (preprocess-exp e))))

(define (process-top-level-form e)
  (display "+ ")
  (shew e)
  (let ((e (preprocess e)))
    ;(display "* ")
    ;(shew e)
    (cond
     ((define? e) (process-define e))
     (#t (shew (evl e))))))

(define (exec-file filename)
  (let ((forms (read-objects filename)))
    (map process-top-level-form forms)))

(define (preprocess-file filename)
  (let ((forms (read-objects filename)))
    (map (lambda (x)
           (shew x)
           (let ((pp (preprocess x)))
             ;(display "* ")
             (shew pp)))
         forms)))

(define (build-binding-receiver pat body)
  (cond
   ((literal? pat) body)
   ((symbol? pat) `(/. ,pat ,body))
   ((pair? pat)
    (build-binding-receiver
     (car pat)
     (build-binding-receiver (cdr pat) body)))
   (#t (err 'build-binding-receiver pat body))))

(define (build-pattern-descender pat binding-receiver)
  (cond
   ((literal? pat)
    (let ((v (sgen)))
      `(/. ,v (if (equal? ,v ,pat)
                  ,binding-receiver
                  fail))))
   ((symbol? pat)
    binding-receiver)
   ((pair? pat)
    (let ((v (sgen)))
      `(/. ,v (if (pair? ,v)
                  ((,(build-pattern-descender
                      (cdr pat)
                      (build-pattern-descender
                       (car pat)
                       binding-receiver))
                    (car ,v))
                   (cdr ,v))
                 fail))))
   (#t (err 'build-pattern-descender pat binding-receiver))))

(define (cps-app e) (cps-app1 e '()))

(define (cps-app1 e rev-acc)
  (if (null? e)
      (reverse rev-acc)
      (let ((first (car e))
            (rest (cdr e))
            (v (sgen)))
        `((/. success ,(cps first))
          (/. ,v ,(cps-app1 rest (cons v rev-acc)))))))

(define (cps e)
  (cond
   ((or (literal? e) (symbol? e))
    `(success ,e))
   ((lambda? e)
    (let ((pat (cadr e))
          (body (caddr e)))
      `(/. success (/. fail (/. ,pat ,(cps body))))))
   ((app? e) (cps-app e))
   (#t (err 'cps e))))

(define (compile-lambda-rewrites e)
  (let ((pat (cadr e))
        (body (caddr e)))
    (cps (build-pattern-descender pat (build-binding-receiver pat body)))))

(define (go)
  (preprocess-file "src.ss"))
;  (exec-file "src.ss"))

;(tracefun evl evl1 evl-app lookup-local-or-global process-define process-top-level-form)
;(tracefun preprocess-exp simplify-pattern-lambdas)
;(tracefun classic-lambda? lambda? app? symbol? is-quote?)
;(tracefun cps cps-app cps-app1 build-binding-receiver build-pattern-descender)
