(load "lib.ss")

(define sgen (tagged-symbol-generator-generator))

(define (forms->program forms)
  (let* ((funs (grep fun? forms))
         (exps (grep (fnot fun?) forms))
         (main-fun
          `(fun (main) (begin ,@exps)))
         (rules (map cdr (snoc funs main-fun))))
    `(/./. ,rules)))

(define (quote-ctors e)
  (cond
   ((and (pair? e) (symbol? (car e)) (not (eq? 'fun (car e))))
    `((quote ,(car e)) . ,(map quote-ctors (cdr e))))
   ((pair? e) (map quote-ctors e))
   (#t e)))

(define (all-over-preprocess e)
  (quote-ctors e))

(define (go)
  (let* ((forms (read-objects "src.ss"))
         (forms (all-over-preprocess forms)))
    (run-program (program->scheme (compile-program (forms->program forms))))))

(define (compile-program program)
  (assert (multi-lambda? program))
  (map compile-rule (cadr program)))

(define (compile-rule rule)
  (compile-pattern-lambda (cons '/. rule)))

(define (compile-pattern-lambda lam)
  (assert (lambda? lam))
  (let ((pat (cadr lam))
        (body (caddr lam))
        (tt (sgen 'tt)))
     `(/. ,tt
          (,(build-pattern-descender pat) ,(build-binding-receiver pat body)))))

(define (build-binding-receiver pat body)
  (cond
   ((literal? pat) body)
   ((pair? pat)
    (build-binding-receiver (car pat)
                            (build-binding-receiver (cdr pat) body)))
   ((symbol? pat)
    `(/. ,pat ,body))
   (#t (err build-binding-receiver pat body))))

(define (build-pattern-descender pat)
;(shew pat (symbol? pat) (literal? pat) (pair? pat))
  (cond
   ((symbol? pat)
    (let ((dt (sgen 'dt))
          (db (sgen 'db)))
      `(/. ,dt (/. ,db (,db ,dt)))))
   ((literal? pat)
    (let ((dt (sgen 'dt))
          (db (sgen 'db)))
      `(/. ,dt (/. ,db (ifequal? ,dt
                                 ,pat
                                 ,db
                                 'fail)))))
   ((pair? pat)
    (let ((dpt (sgen 'dpt))
          (dpb (sgen 'dpb)))
      `(/. ,dpt (/. ,dpb (ifpair? ,dpt
                                  ((,(build-pattern-descender (cdr pat))
                                    (cdr ,dpt))
                                   ((,(build-pattern-descender (car pat))
                                     (car ,dpt))
                                    ,dpb))
                                  'fail)))))
   (#t (err 'build-pattern-descender pat))))

(define (exp->scheme e)
  (cond
   ((null? e) ''())
   ((atom? e) e)
   ((lambda? e)
    (let ((arg (cadr e))
          (body (exp->scheme (caddr e))))
      `(lambda (,arg) ,body)))
   ((and (pair? e) (eq? 'ifpair? (car e)))
    (let ((target (cadr e))
          (then-part (exp->scheme (caddr e)))
          (else-part (exp->scheme (cadddr e))))
      `(if (pair? ,target)
           ,then-part
           ,else-part)))
   ((and (pair? e) (eq? 'ifequal? (car e)))
    (let ((a (cadr e))
          (b (caddr e))
          (then-part (exp->scheme (cadddr e)))
          (else-part (exp->scheme (car (cddddr e)))))
      `(if (equals? ,a ,(exp->scheme b))
           ,then-part
           ,else-part)))
   ((literal? e) e)
   ((app? e) (map exp->scheme e))
   (#t (err 'exp->scheme e))))

(define (program->scheme p)
  (map exp->scheme p))

(define (run-program program)
  (apply-program program '(main)))

(define (apply-program program t)
  (map (lambda (r) (apply-rule r t)) program))

(define (apply-rule rule t)
  ((eval rule) t))

;(tracefun lambda?)
;(tracefun compile-program compile-pattern-lambda compile-rule)
;(tracefun build-binding-receiver build-pattern-descender)
(tracefun program->scheme exp->scheme)
(tracefun apply-program run-program apply-rule)

(go)
