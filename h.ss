(load "lib.ss")

(define forms (read-objects "src.ss"))
(define (run-file filename)
  (process-defines (read-objects filename))
  (evl-top '(main)))

(define (process-defines forms)
  (map store-define forms))

(define (store-define e)
  (mtch e
        ('def var exp) (global-env-define var (evl exp '()))))

(define (global-env-define var exp)
  (set! global-env
        (cons (list var exp) global-env)))

(define global-env '())
(load "primitives.ss")

(define (global-lookup-env e)
  (let ((a (assoc e global-env)))
    (if (eq? a #f)
        (err 'global-lookup-env e)
        (2nd a))))

(define (lookup-env e env)
  (if (null? env)
      (global-lookup-env e)
      (let ((p (assoc e (car env))))
        (if (eq? p #f)
            (lookup-env e (cdr env))
            (2nd p)))))
                          
(define (apply-fun f args)
  (cond
   ((and (pair? f)
         (eq? '$ (car f)))
    (let* ((lam (2nd f))
           (env (3rd f))
           (formals (2nd lam))
           (body (3rd lam)))
      (evl body
           (cons (zip list formals args)
                 env))))
   ((and (pair? f)
         (eq? '@ (car f)))
    (blimpp (2nd f) args))
   (#t (err 'apply-fun f))))

(define (evl e env)
  (cond
   ((and (pair? e)
         (eq? (1st e) 'quote))
    (2nd e))
   ((and (pair? e)
         (eq? (1st e) 'if))
    (let ((pred (2nd e))
          (then (3rd e))
          (else (4th e)))
      (let ((b (evl pred env)))
        (cond
         ((eq? b 'True) (evl then env))
         ((eq? b 'False) (evl else env))
         (#t (err 'if pred b))))))
   ((symbol? e) (lookup-env e env))
   ((and (pair? e)
         (eq? '/. (car e)))
    `($ ,e ,env))
   ((pair? e)
    (let ((ee (map (lambda (e) (evl e env))
                   e)))
      (apply-fun (car ee) (cdr ee))))
   (#t e)))

(define (evl-top e)
  (display "+ ")
  (lshew e)
  (display "\n")
  (let ((r (evl e '())))
    (lshew r)
    (display "\n")
    r))

;(tracefun apply-fun evl lookup-env blimpp)
