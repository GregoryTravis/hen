(load "lib.ss")

(define lazy #f)
(define trace-evl #f)

(define forms (read-objects "src.ss"))
(define (run-files . filenames)
  (map process-top-level-forms (read-objects "overture.ss"))
  (map (lambda (filename)
         (map process-top-level-forms (read-objects filename)))
       filenames)
  (add-toplevel-exps)
  (add-default-main-maybe)
  ;(shew global-env)
  (evl-top '(main))
  (evl-top '(__toplevel-forms)))

(define (process-top-level-forms form)
  (cond
   ((and (pair? form) (eq? (car form) 'def)) (process-def form))
   ((and (pair? form) (eq? (car form) 'fun)) (process-fun form))
   (#t (process-toplevel-exp form))))

(define toplevel-forms '())
(define (process-toplevel-exp form)
  (set! toplevel-forms (snoc toplevel-forms form)))

(define (process-def e)
  (mtch e
        ('def var exp) (global-env-define var (evl exp '()))))

(define (process-fun e)
  (mtch e
        ('fun args body)
        (let* ((fun-name (car args))
               (lam `(/. ,(cdr args) ,body))
               (clo (evl lam '()))
               (existing-value (assoc fun-name global-env)))
          (if (eq? existing-value #f)
              (global-env-define fun-name `(/./. ,clo))
              (let ((clos (cadr existing-value)))
                (global-env-define fun-name `(/./. ,@(snoc (cdr clos) clo))))))))

(define (add-toplevel-exps)
  (process-fun `(fun (__toplevel-forms) (begin ,@(map (lambda (e) `(interactive-shew ,e)) toplevel-forms)))))

(define (add-default-main-maybe)
  (if (eq? (assoc 'main global-env) #f)
      (process-fun `(fun (main) 'Mu))
      '()))

(define (global-env-remove-maybe var)
  (let ((a (assoc var global-env)))
    (if (eq? a #f)
        '()
        (set! global-env
              (remove a global-env)))))

(define (global-env-define var exp)
  (global-env-remove-maybe var)
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
   ((and (symbol? p) (ctor? p) (eq? p t)) '())
   ((and (symbol? p) (not (ctor? p))) (list (list p t)))
   ((and (is-quote? p)
         (symbol? (quote-quoted p))
         (eq? (quote-quoted p) t)) '())
   ((and (pair? p) (pair? t)) (mab-append (mych (car p) (car t))
                                          (mych (cdr p) (cdr t))))
   ((and (null? p) (null? t)) '())
   ((and (number? p) (= p t)) '())
   (#t #f)))

(define (must o . stuff)
  (if (eq? o #f)
      (err 'must o stuff)
      (car o)))

(define (try-apply-multilam mlam args)
  (if (null? (cdr mlam))
      #f
      (let ((r (try-apply-lam (cadr mlam) args)))
        (if (eq? r #f)
            (try-apply-multilam
             (cons '/./. (cddr mlam))
             args)
            r))))

(define (try-apply-lam clo args)
  (let* ((lam (2nd clo))
         (env (3rd clo))
         (formals (2nd lam))
         (body (3rd lam)))
    (let ((bindings (mych formals args)))
      (if (eq? bindings #f)
          #f
          (list (evl body (cons bindings env)))))))

(define (must-apply-lam clo args)
  (must (try-apply-lam clo args) 'must-apply-lam clo args))

(define (must-apply-multilam f-name mlam args)
  (must (try-apply-multilam mlam args) 'must-apply-multilam (cons f-name args)))

(define (apply-fun f-name f args)
  (cond
   ((and (pair? f)
         (eq? '& (car f)))
    (apply-fun f-name (unthunk f) args))
   ((and (pair? f)
         (eq? '$ (car f)))
    (must-apply-lam f args))
   ((and (pair? f)
         (eq? '/./. (car f)))
    (must-apply-multilam f-name f args))
   ((and (pair? f)
         (eq? '@ (car f)))
    (blimpp (2nd f) args))
   (#t (err 'apply-fun f-name args))))

(define (evl-begin e env)
  (cond
   ((null? (cdr e)) 'Mu)
   ((null? (cddr e)) (evl (cadr e) env))
   (#t (evl `((/. (dummy) (begin ,@(cddr e))) ,(cadr e)) env))))

(define (let->lambda letexp)
  (let ((bindings (cadr letexp))
        (body (caddr letexp)))
    (if (null? bindings)
        body
        (let ((var (caar bindings))
              (value (cadar bindings))
              (rest-bindings (cdr bindings)))
          `((/. (,var) ,(let->lambda `(let ,rest-bindings ,body))) ,value)))))

(define (evl-match t cs env)
  (evl
   `((/./. ,@(map (lambda (ha) (list '/.
                                     (list (car ha))
                                        ;(car ha)
                                     (cadr ha)))
                  (scoop-by 2 cs)))
     ,t)
   env))

(define (evl e env)
  (cond

   ;; Special forms

   ((and (pair? e)
         (eq? (1st e) 'quote))
    (2nd e))
   ((and (pair? e)
         (eq? (car e) '@))
    (evl-match (cadr e) (cddr e) env))
   ((and (pair? e)
         (eq? (car e) 'let))
    (begin
      (assert (eq? (length e) 3))
      (evl (let->lambda e) env)))
   ((and (pair? e)
         (eq? (car e) 'begin))
    (evl-begin e env))
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

   ;; Atoms

   ((and (symbol? e) (not (ctor? e))) (lookup-env e env))
   ((and (symbol? e) (ctor? e)) e)

   ;; /. and /./.

   ((and (pair? e)
         (eq? '/. (car e)))
    `($ ,e ,env))
   ((and (pair? e)
         (eq? '/./. (car e)))
    `(/./. ,@(map (lambda (e) (evl e env)) (cdr e))))

   ;; Ctons

   ((and (pair? e)
         (ctor? (car e)))
    (cons (car e)
          (map-improper (lambda (e) (evl e env))
               (cdr e))))

   ;; Function application
   ((pair? e)
    (let ((ee (map-improper (lambda (e) (if lazy
                                            `(& ($ (/. () ,e) ,env))
                                            (evl e env)))
                            e)))
;      (if trace-evl (tracing-push-print (list (cons (car e) (cdr ee)) env)) '())
      (if trace-evl (tracing-push-print (cons (car e) (cdr ee))) '())
      (let ((r (apply-fun (car e) (car ee) (cdr ee))))
        (if trace-evl (tracing-pop-print r) '())
        r)))
   (#t e))) 

(define (evl-nf e)
  (let ((ee (evl e '())))
    (if (and (pair? ee)
             (eq? '& (car ee)))
        (unthunk ee)
        ee)))

(define (evl-top e)
  (let ((r (evl-nf e)))
    (if (not (eq? r 'Mu))
        (begin
          (lshew r)
          (display "\n"))
        '())
    r))

;(tracefun apply-fun evl blimpp unthunk evl-nf try-apply-lam try-apply-multilam let->lambda evl-match)
