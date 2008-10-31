(load "lib.ss")

(define forms (read-objects "src.ss"))
(define (go)
  (process-defines forms)
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

(shew global-env)

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
          (cons (zip list
                     formals
                     (map (lambda (e) (evl e env))
                          args))
                env))))
   ((and (pair? f)
         (eq? '@ (car f)))
    (blimpp (2nd f) args))
   (#t (err 'apply-fun f))))

(define (evl e env)
  (cond
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

(tracefun apply-fun evl lookup-env)
(tracefun blimpp)
