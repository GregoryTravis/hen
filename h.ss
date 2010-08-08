(load "lib.ss")

(define keep-generated #t)

(define make-var (symbol-generator-generator))

(define (pat-literal? x) (or (number? x) (is-quote? x) (string? x)))
(define (literal-value x) (if (is-quote? x) (quote-quoted x) x))
(define (pat-variable? x) (and (symbol? x) (not (ctor? x))))
(define (pat-ctor? x) (ctor? x))
(define (explicit-ctor? x) (mtch x ('literal s) (ctor? s) _ #f))

(define (fun-name fun)
  (mtch fun ('fun (name . pat) body) name))
(define (fun->clause fun)
  (mtch fun ('fun (name . pat) body) `(,pat ,body)))
(define (is-fun? fun)
  (mtch fun ('fun (name . pat) body) #t _ #f))

(define (pat->explicit-terms pat)
  (cond
   ((pat-literal? pat) `(literal ,pat))
   ((pat-ctor? pat) `(literal ,pat))
   ((pat-variable? pat) `(variable ,pat))
   ((pair? pat) `(list ,(map pat->explicit-terms pat)))))
(define (clause->explicit-terms clause)
  (mtch clause
        (pat body) `(clause ,(pat->explicit-terms pat)
                            ,(pat->explicit-terms body))))
(define (clauses->explicit-terms clauses)
  `(clauses ,(map clause->explicit-terms clauses)))

(define (compile-pat pat exp body)
  (mtch pat
        ('literal lit) `(if (equal? ',lit ,exp) ,body (fail))
        ('variable v) `(let ((,v ,exp)) ,body)
        ('list (pat . pats)) (let ((new-var (make-var)))
                               `(let ((,new-var ,exp))
                                  (if (pair? ,new-var)
                                      ,(compile-pat pat `(car ,new-var) (compile-pat `(list ,pats) `(cdr ,new-var) body))
                                      (fail))))
        ('list ()) body))

(define (compile-body body)
  (mtch body
        ('literal lit) `',lit
        ('variable v) v
        ('list exps) (if (explicit-ctor? (car exps))
                         `(list . ,(map compile-body exps))
                         (map compile-body exps))))

(define (compile-clause clause)
  (mtch clause
        ('clause pat body) (let ((new-var (make-var)))
                             `(/. (,new-var) ,(compile-pat pat new-var (compile-body body))))))

(define (compile-clauses var clauses)
  (mtch clauses
        ('clauses (clause . clauses)) `(let ((fail (/. () ,(compile-clauses var `(clauses ,clauses)))))
                                         (,(compile-clause clause) ,var))
        ('clauses ()) `(begin (display 'fail) (exit))))

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
