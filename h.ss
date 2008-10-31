(load "lib.ss")

(define forms (read-objects "src.ss"))
(define (go)
  (map evl-top forms))

(define (lookup-env e env)
  (if (null? env)
      (err 'no-such-variable e)
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

;(tracefun apply-fun evl lookup-env)
