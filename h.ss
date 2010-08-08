(load "lib.ss")

(define keep-generated #f)

(define make-var (symbol-generator-generator))

(define (pat-ctor? x) (ctor? x))
(define (pat-literal? x) (or (pat-ctor? x)(number? x) (is-quote? x) (string? x)))
(define (pat-variable? x) (and (symbol? x) (not (ctor? x))))
(define (explicit-ctor? x) (mtch x ('literal s) (ctor? s) _ #f))

(define (fun-name fun) (mtch fun ('fun (name . pat) body) name))
(define (fun->clause fun) (mtch fun ('fun (name . pat) body) `(,pat ,body)))
(define (is-fun? fun) (mtch fun ('fun (name . pat) body) #t _ #f))

(define (pat->explicit-terms pat) pat)
(define (clause->explicit-terms clause)
  (mtch clause
        (pat body) `(,(pat->explicit-terms pat)
                     ,(pat->explicit-terms body))))
(define (clauses->explicit-terms clauses)
  (map clause->explicit-terms clauses))

(define (compile-pat pat exp body)
  (cond
   ((pat-literal? pat) `(if (equal? ',pat ,exp) ,body (fail)))
   ((pat-variable? pat) `(let ((,pat ,exp)) ,body))
   ((pair? pat) (let ((new-var (make-var)))
                               `(let ((,new-var ,exp))
                                  (if (pair? ,new-var)
                                      ,(compile-pat (car pat) `(car ,new-var) (compile-pat (cdr pat) `(cdr ,new-var) body))
                                      (fail)))))
   ((null? pat) body)))

(define (compile-body body)
  (cond
   ((pat-literal? body) `',body)
   ((pat-variable? body) body)
   ((pair? body) (if (ctor? (car body))
                     `(list . ,(map compile-body body))
                     (map compile-body body)))))

(define (compile-clause clause)
  (mtch clause
        (pat body) (let ((new-var (make-var)))
                     `(/. (,new-var) ,(compile-pat pat new-var (compile-body body))))))

(define (compile-clauses var clauses)
  (mtch clauses
        (clause . clauses) `(let ((fail (/. () ,(compile-clauses var clauses))))
                              (,(compile-clause clause) ,var))
        () `(begin (display 'fail) (exit))))

(define (function->scheme name clauses)
  (let ((new-var (make-var)))
    `(define (,name . ,new-var) ,(->scheme (compile-clauses new-var clauses)))))

(define (->scheme e)
  (mtch e
        ('quote lit) e
        ('/. vars body) `(lambda ,vars ,(->scheme body))
        ('let bindings body) `(let ,(lensmap cadr-lens ->scheme bindings) ,(->scheme body))
        (a . d) (map ->scheme e)
        x x))

;; ((fun ...) (fun ...) ...) => ((name clauses) (name clauses) ...)
(define (funs->named-clause-lists src)
  (lensmap cadr-lens ($ map fun->clause _) (group-by fun-name src)))

(define (src->defines src)
  (map (lambda (blah) (mtch blah (name clauses) (function->scheme name (clauses->explicit-terms clauses))))
       (funs->named-clause-lists (grep is-fun? src))))
(define (src->tles src)
  (grep (fnot is-fun?) src))

(define (src->scheme src)
  (let ((defines (src->defines src))
        (main `(begin . ,(map compile-body (map pat->explicit-terms (src->tles src))))))
    (append defines (list main))))

(define (run-src src)
  (let ((obj-file "obj.ss"))
    (if (file-exists? obj-file) (delete-file obj-file) '())
    (write-objects-to-file obj-file (src->scheme src))
    (shew (load obj-file))
    (if (not keep-generated) (delete-file obj-file) void)))

(define (run-file filename) (run-src (read-objects filename)))

(run-file "src.ss")
