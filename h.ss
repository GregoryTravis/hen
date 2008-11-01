(load "lib.ss")

(define lazy #f)

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

(define (unthunk f)
  (let ((body (3rd (2nd (2nd f))))
        (env (3rd (2nd f))))
    (evl body env)))

(define (mab-append a b)
  (if (or (eq? a #f) (eq? b #f))
      #f
      (append a b)))

(define (mych p t)
  (cond
   ((symbol? p) (list (list p t)))
   ((and (is-quote? p)
         (symbol? (quote-quoted p))
         (eq? (quote-quoted p) t)) '())
   ((pair? p) (mab-append (mych (car p) (car t))
                          (mych (cdr p) (cdr t))))
   ((and (null? p) (null? t)) '())
   ((and (number? p) (= p t)) '())
   (#t #f)))
   ;(#t (err 'mych p t))))

(define (must o . stuff)
  (if (eq? o #f)
      (err 'must o stuff)
      (car o)))

(define (try-apply-lam lam env args)
  (let* ((formals (2nd lam))
         (body (3rd lam)))
    (let ((bindings (mych formals args)))
      (if (eq? bindings #f)
          #f
          (list (evl body (cons bindings env)))))))

(define (must-apply-lam lam env args)
  (must (try-apply-lam lam env args) 'pattern-lambda-failure lam env args))

(define (apply-fun f args)
  (cond
   ((and (pair? f)
         (eq? '& (car f)))
    (apply-fun (unthunk f) args))
   ((and (pair? f)
         (eq? '$ (car f)))
    (must-apply-lam (2nd f) (3rd f) args))
   ((and (pair? f)
         (eq? '@ (car f)))
    (blimpp (2nd f) args))
   (#t (err 'apply-fun f))))

(define (evl e env)
  (cond
   ((and (pair? e)
         (ctor? (car e)))
    (cons (car e)
          (map (lambda (e) (evl e env))
               (cdr e))))
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
    (let ((ee (map (lambda (e) (if lazy
                                   `(& ($ (/. () ,e) ,env))
                                   (evl e env)))
                   e)))
      (apply-fun (car ee) (cdr ee))))
   (#t e)))

(define (evl-nf e)
  (let ((ee (evl e '())))
    (if (and (pair? ee)
             (eq? '& (car ee)))
        (unthunk ee)
        ee)))

(define (evl-top e)
  (display "+ ")
  (lshew e)
  (display "\n")
  (let ((r (evl-nf e)))
    (lshew r)
    (display "\n")
    r))

;(tracefun apply-fun evl blimpp unthunk evl-nf)
