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
  (let* (;(closure (evl1 (car e) env))
         (closure (car e))
         ;(arg (evl1 (cadr e) env))
         (arg (cadr e))
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
  (assert (app? e))
  (let ((e (map (lambda (x) (evl1 x env)) e)))
    (cond
     ((closure? (car e)) (evl-app-closure e env))
     ((primitive? (car e)) (evl-app-primitive e env))
     (#t (err 'evl-app e env)))))

(define (evl-conditional e env)
  (assert (conditional? e))
  (let ((if-part (cadr e))
        (then-part (caddr e))
        (else-part (if (null? (cdddr e))
                       '()
                       (cadddr e))))
    (let ((v (evl1 if-part env)))
      (if (not (boolean? v))
          (err 'if-part-not-a-boolean e v env)
          (if (eq? v 'true)
              (evl1 then-part env)
              (evl1 else-part env))))))

(define (evl1 e env)
  (cond
   ((is-quote? e) (quote-quoted e))
   ((or (literal? e) (closure? e) (primitive? e)) e)
   ((conditional? e) (evl-conditional e env))
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

(define (build-binding-receiver pat body)
  (cond
   ((literal? pat) body)
   ((symbol? pat) `(/. ,pat ,body))
   ((pair? pat)
    (build-binding-receiver
     (car pat)
     (build-binding-receiver (cdr pat) body)))
   (#t (err 'build-binding-receiver pat body))))

(define (build-pattern-descender pat)
  (cond
   ((symbol? pat)
    (let ((v (sgen))
          (r (sgen)))
      `(/. ,v (/. ,r (,r ,v)))))
   ((literal? pat)
    (let ((v (sgen))
          (r (sgen)))
      `(/. ,v (/. ,r (if (equal? ,v ,pat)
                         ,r
                         'fail)))))
   ((pair? pat)
    (let ((v (sgen))
          (r (sgen))
          (car-descender (build-pattern-descender (car pat)))
          (cdr-descender (build-pattern-descender (cdr pat))))
      `(/. ,v (/. ,r
                  (if (not (pair? ,v))
                      'fail
                      ((,cdr-descender (cdr ,v))
                       ((,car-descender (car ,v)) ,r)))))))
   (#t (err 'build-pattern-descender pat binding-receiver))))

(define (build-rewriter pat body)
  (let ((descender (build-pattern-descender pat))
        (receiver (build-binding-receiver pat body))
        (v (sgen)))
    `(/. ,v ((,descender ,v) ,receiver))))

(define (compile-lambda-rewrites e)
  (cond
   ((or (atom? e) (literal? e)) e)
   ((lambda? e)
    (let ((pat (cadr e))
          (body (caddr e)))
      ;(cps-top (build-pattern-descender pat (build-binding-receiver pat body)))))
      ;(cps-top (build-rewriter pat body))))
      (build-rewriter pat body)))
   ((app? e) (map compile-lambda-rewrites e))
   (#t (err 'compile-lambda-rewrites e))))

(define (rerun-if-not-changed f v)
  (let ((vv (f v)))
    (if (equal? v vv)
        v
        (rerun-if-not-changed f vv))))

(define (normalizerify f)
  (lambda (v)
    (let ((fv (f v)))
      (if (equal? v fv)
          fv
          ((normalizerify f) fv)))))

(define (exp-substitute var value e)
  (cond
   ((literal? e) e)
   ((symbol? e)
    (if (eq? var e)
        value
        e))
   ((app? e)
    (map (lambda (e) (exp-substitute var value e))
         e))
   ((lambda? e)
    (let ((lam-arg (cadr e))
          (lam-body (caddr e)))
      (if (eq? var lam-arg)
          e
          `(/. ,lam-arg ,(exp-substitute var value lam-body)))))
   (#t (err 'exp-substitute var value e))))

(define (lambda-substitute lam arg)
  (exp-substitute (cadr lam) arg (caddr lam)))

(define (simplify-lambda-app e)
  (assert (and (= 2 (length e)) (app? e)))
  (lambda-substitute (simplify-lambda (car e))
                     (simplify-lambda (cadr e))))

(define (free-in v e)
  (assert (symbol? v))
  (cond
   ((equal? v e) #t)
   ((symbol? e) #f)
   ((literal? e) #f)
   ((lambda? e)
    (let ((arg (cadr e))
          (body (caddr e)))
      (if (equal? arg v)
          #f
          (free-in v body))))
   ((1-arg-app? e) (or (free-in (car e))
                       (free-in (cadr e))))
   (#t (err 'free-in v e))))

(define (simplify-lambda e)
  (cond
   ;; (/. x (y x)) -> y
   ((and (lambda? e)
         (let ((arg (cadr e))
               (body (caddr e)))
           (and (1-arg-app? body)
                (= 2 (length body))
                (equal? arg (cadr body))
                (not (free-in arg (car body))))))
    (simplify-lambda (car (caddr e))))
   ((closure? e)
    (let ((lam (cadr e))
          (env (caddr e)))
      `(closure ,(simplify-lambda lam) ,env)))
   ((lambda? e)
    (let ((arg (cadr e))
          (body (caddr e)))
      `(/. ,arg ,(simplify-lambda body))))
   ((and (app? e) (= 2 (length e)) (classic-lambda? (car e)))
    (simplify-lambda-app e))
   ((proper-list? e) (map simplify-lambda e))
   (#t e)))

;(define simplify-lambda (normalizerify simplify-lambda))

(define (cps e) (cps1 e '(/. x x)))

(define (cps1 e k)
  (cond
   ((or (symbol? e) (literal? e) (classic-lambda? e))
    `(,k ,e))
   ((1-arg-app? e)
    (let ((f (car e))
          (a (cadr e))
          (fv (sgen))
          (av (sgen)))
      (cps1 f
           `(/. ,fv ,(cps1 a
                          `(/. ,av (,k (,fv ,av))))))))
   (#t (err 'cps1 e k))))

(define (process-top-level-form e)
  (assert (not (define? e)))
  (display "+ ")
  (shew e)

  (set! e (compile-lambda-rewrites e))
;  (shew 'compile-lambda-rewrites e)

  (set! e (simplify-lambda e))
;  (shew 'simplify-lambda e)

  (set! e (cps e))
;  (shew 'cps e)

  (set! e (simplify-lambda e))
;  (shew 'simplify-lambda e)

  (set! e (evl e))
  (shew e)
  e)

(define (exec-file filename)
  (let ((forms (read-objects filename)))
    (map process-top-level-form forms)))

(define (go)
  (exec-file "src.ss"))

;(tracefun evl evl1 evl-app evl-app-closure evl-app-primitive evl-conditional lookup-local-or-global process-define process-top-level-form)
;(tracefun classic-lambda? lambda? app? is-quote?)
;(tracefun compile-lambda-rewrites build-binding-receiver build-pattern-descender)
;(tracefun cps cps1)
;(tracefun simplify-lambda simplify-lambda-app lambda-substitute exp-substitute)
