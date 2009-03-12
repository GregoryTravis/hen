(load "lib.ss")
(load "util.ss")
(load "primitives.ss")

(define (constant? a) (or (number? a) (null? a) (ctor? a)))
(define (quote-constant a) (mtch a '() ''() x x))

(define (simplify-patterns e)
  (let ((v (sg)))
    (mtch e
          ('/. args body)
          (simplify-patterns `(/. ,args ,body (boom 'boom)))

          ('/. ((a . b) . c) body fail)
          `(/. (,v . ,c) ((/. (,a . ,b) ,(simplify-patterns body) ,(simplify-patterns fail)) ,v) ,fail)

          ('/. (a . (b . c)) body fail)
          `(/. (,a . ,v) ((/. (,b . ,c) ,(simplify-patterns body) ,(simplify-patterns fail)) ,v) ,fail)

          ('/. args body fail)
          `(/. ,args ,(simplify-patterns body) ,(simplify-patterns fail))

          (a . b)
          (map simplify-patterns e)

          x x)))

(define (patterns->conditionals e)
  (let ((v (sg)))
    (mtch e
          ('/. (a . b) body fail)
          `(/. ,v (if (pair? ,v) ,(patterns->conditionals `(((/. ,a (/. ,b ,body ,fail) ,fail) (car ,v)) (cdr ,v))) (,(patterns->conditionals fail) ,v)))
;          `(/. ,v (if (pair? ,v) (let* ((,a (car ,v)) (,b (cdr ,v))) ,(patterns->conditionals body)) (,(patterns->conditionals fail) ,v)))

          ('/. a body fail) (cond ((symbol? a) `(/. ,a ,(patterns->conditionals body)))
                                  ((constant? a) `(/. ,v (if (prim-= ,v ,(quote-constant a)) ,(patterns->conditionals body) (,(patterns->conditionals fail) ,v))))
                                  (#t (err 'patterns->conditionals)))

          (a . b)
          (map patterns->conditionals e)

          x x)))

(define (->scheme e)
  (mtch e
        ('/. args body)
        `(lambda (,args) ,(->scheme body))

        ('if b t e) `(if ,(->scheme b) ,(->scheme t) ,(->scheme e))
        ('let* bindings body) `(let* ,(lensmap cadr-lens ->scheme bindings) ,(->scheme body))

        (a . b)
        (map ->scheme e)
;        (list (->scheme a) (map ->scheme b))

        x x))

;(define (boom a) (err 'boom a))
(define (boom name) (lambda (x) (err 'boom name x)))

;; (map (** shew eval ->scheme patterns->conditionals simplify-patterns)
;;      '(
;; ;;        (/. x x boom)
;; ;;        (/. (x . y) x boom)
;; ;;        (/. (x . (y . z)) x boom)
;; ;;        (/. ((x . y) . z) x boom)
;; ;;        ((/. x x boom) 10)
;; ;;        ((/. (x . y) x boom) (cons 1 2))
;; ;;        ((/. (x . y) y boom) (cons 1 2))
;; ;; ;;       ((/. (x . y) y boom) 3)
;; ;;        ((/. ((x . y) . z) x boom) (cons (cons 1 2) 3))
;; ;;        ((/. ((x . y) . z) y boom) (cons (cons 1 2) 3))
;; ;;        ((/. ((x . y) . z) z boom) (cons (cons 1 2) 3))
;; ;; ;;       ((/. ((x . y) . z) x boom) (cons 1 (cons 2 3)))
;; ;;        ((/. (x . (y . z)) x boom) (cons 1 (cons 2 3)))
;;        ((/. (x . (y . z)) y boom) (cons 1 (cons 2 3)))
;; ;;        ((/. (x . (y . z)) z boom) (cons 1 (cons 2 3)))
;; ;; ;;       ((/. (x . (y . z)) z boom) (cons (cons 1 2) 3))
;;        ))

(define (fun? e) (mtch e ('fun a b) #t _ #f))
(define (def? e) (mtch e ('def a b) #t _ #f))
(define (fun-name e) (mtch e ('fun (name . args) b) name))

(define (tlfs->defs e)
  (mtch (group-by-preds (list def? fun?) e)
        (defs funs)
        (append defs
                (funs->defs funs))))

(define (fun->/. e)
  (mtch e ('fun (name . args) body) `(/. ,args ,body)))

(define (funs->defs funs)
  (map (lambda (x) (mtch x (name . funs) `(def ,name ,(cascade-lambdas name (map fun->/. funs)))))
       (group-by fun-name funs)))

(define (cascade-lambdas name /.s)
  (mtch /.s
        (('/. args body) . rest)
        `(/. ,args ,body ,(cascade-lambdas name rest))

        () `(boom ',name)))

(define (evl e env)
  (eval e))

(define gort (** ->scheme patterns->conditionals simplify-patterns))

(define (def->scheme e)
;  (mtch e ('def name body) `(define ,name ,((** ->scheme patterns->conditionals simplify-patterns) body))))
;;   (mtch e
;;         ('def name (/. args body)) `(define ,name ,(cascade-lambdas name (list `(/. ,args ,body))))
;;         ('def name value) `(define ,name ,value)))
;        ('def name body) `(define ,name (lambda args (,(gort body) args)))))
  (mtch e ('def name body) `(define (,name . args) (,(gort body) args))))

;`(define ,name (lambda x (,((** ->scheme patterns->conditionals simplify-patterns) body) x)))))

(define (trace-em defines)
  (map (lambda (x) (mtch x ('define (name . args) body) `(tracefun ,name))) defines))

(define (hen-run file)
  (let ((scheme (sr (map def->scheme (sr (tlfs->defs (read-objects file)))))))
    (map eval scheme)
    (map eval (trace-em scheme))
    )
  (shew 'ok)
  (eval '(main)))

;(tracefun simplify-patterns patterns->conditionals ->scheme)
;(tracefun def->scheme gort)

(hen-run "src.ss")
