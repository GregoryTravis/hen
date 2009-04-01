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

          ('/. a body fail) (cond ((ctor? a) `(/. ,v (if (== ,v ',a) ,(patterns->conditionals body) ,(patterns->conditionals fail))))
                                  ((symbol? a) `(/. ,a ,(patterns->conditionals body)))
                                  ((constant? a) `(/. ,v (if (prim-== ,v ,(quote-constant a)) ,(patterns->conditionals body) (,(patterns->conditionals fail) ,v))))
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

        (a . b) (cond ((ctor? a) `(list ',a ,@b))
                      (#t (map ->scheme e)))

        x x))

(define (boom name) (lambda (x) (err 'boom name x)))

(define (fun? e) (mtch e ('fun a b) #t _ #f))
(define (def? e) (mtch e ('def a b) #t _ #f))
(define (fun-name e) (mtch e ('fun (name . args) b) name))

(define (tlfs->defs e)
  (mtch (group-by-preds (list def? fun? true-pred) e)
        (defs funs top-level-expressions)
        (append defs
                (list (tle-def top-level-expressions))
                (funs->defs funs))))

(define (tle-def tles)
  `(def _tle (/. () (shew-no-voids ,@tles))))

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
  (mtch e ('def name body) `(define (,name . args) (,(gort body) args))))

(define (trace-em defines)
  (map (lambda (x) (mtch x ('define (name . args) body) `(tracefun ,name))) defines))

;; Add main if there isn't one, and add _main which calls _tles and
;; main.  This is done before funs->defs.
(define (has-main tlfs) (any? (map (lambda (x) (and (fun? x) (eq? (caadr x) 'main))) tlfs)))
(define (add-mains tlfs)
  (cons
   (if (has-main tlfs)
       `(fun (_main) (begin (_tle) (main)))
       `(fun (_main) (_tle)))
   tlfs))

(define (hen-run file)
  (let ((scheme (map def->scheme (tlfs->defs (add-mains (read-objects file))))))
    ;(shew scheme)
    (map eval scheme)
    ;(map eval (trace-em scheme))
    )
  (eval '(_main)))

;(tracefun simplify-patterns patterns->conditionals ->scheme)
;(tracefun def->scheme gort)
