(load "lib.ss")

(define keep-generated #f)

(define make-var (symbol-generator-generator))

(define (pat-ctor? x) (ctor? x))
(define (pat-literal? x) (or (pat-ctor? x)(number? x) (is-quote? x) (string? x)))
(define (pat-variable? x) (and (symbol? x) (not (ctor? x))))
(define (explicit-ctor? x) (mtch x ('literal s) (ctor? s) _ #f))
(define (lambda? x) (mtch x ('/. v body) #t _ #f))

(define (fun-name fun) (mtch fun ('fun (name . pat) body) name))
(define (fun->clause fun) (mtch fun ('fun (name . pat) body) `(,pat ,body)))
(define (is-fun? fun) (mtch fun ('fun (name . pat) body) #t _ #f))

(define (compile-pat pat exp body)
  (cond
   ((pat-literal? pat) `(if (equal? ',pat ,exp) ,body (fail)))
   ((pat-variable? pat) `(let ((,pat ,exp)) ,body))
   ((pair? pat) (let ((new-var (make-var)))
                               `(let ((,new-var ,exp))
                                  (if (pair? ,new-var)
                                      ,(compile-pat (car pat) `(car ,new-var) (compile-pat (cdr pat) `(cdr ,new-var) body))
                                      (fail)))))
   ((null? pat) (let ((new-var (make-var)))
                               `(let ((,new-var ,exp))
                                  (if (null? ,new-var)
                                      ,body
                                      (fail)))))))

(define (compile-exp body)
  (cond
   ((lambda? body) (mtch body ('/. v body) `(/. ,v ,(compile-exp body))))
   ((pat-literal? body) `',body)
   ((pat-variable? body) body)
   ((pair? body) (if (ctor? (car body)) `(list . ,(map compile-exp body)) (map compile-exp body)))))

(define (compile-clause clause)
  (mtch clause
        (pat body) (let ((new-var (make-var))) `(/. (,new-var) ,(compile-pat pat new-var (compile-exp body))))))

(define (compile-clauses var clauses)
  (mtch clauses
        (clause . clauses) `(let ((fail (/. () ,(compile-clauses var clauses))))
                              (,(compile-clause clause) ,var))
        () `(begin (display 'fail) (exit))))

(define (->scheme e)
  (mtch e
        ('quote lit) e
        ('/. vars body) `(lambda ,vars ,(->scheme body))
        ('let bindings body) `(let ,(lensmap cadr-lens ->scheme bindings) ,(->scheme body))
        (a . d) (map ->scheme e)
        x x))

(define (named-clauses->define named-clauses)
  (mtch named-clauses
        (name clauses)
        (let ((new-var (make-var)))
          `(define (,name . ,new-var) ,(->scheme (compile-clauses new-var clauses))))))

;; ((fun ...) (fun ...) ...) => ((name clauses) (name clauses) ...)
(define (funs->named-clauseses src)
  (lensmap cadr-lens ($ map fun->clause _) (group-by fun-name src)))

(define (src->defines src)
  (map named-clauses->define (funs->named-clauseses (grep is-fun? src))))
(define (src->main src)
  `(begin . ,(map ->scheme (map compile-exp (grep (fnot is-fun?) src)))))

;; hen src -> defines and tle
(define (src->scheme src) (snoc (src->defines src) (src->main src)))

(define (run-src src)
  (let ((obj-file "obj.ss"))
    (if (file-exists? obj-file) (delete-file obj-file) '())
    (write-objects-to-file obj-file (src->scheme src))
    (shew (load obj-file))
    (if (not keep-generated) (delete-file obj-file) void)))

(define (run-file filename) (run-src (read-objects filename)))

(run-file "src.ss")
