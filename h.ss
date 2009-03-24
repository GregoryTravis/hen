(load "lib.ss")
(load "util.ss")
(load "primitives.ss")

(define (constant? a) (or (number? a) (null? a) (ctor? a)))
(define (quote-constant a) (mtch a '() ''() x x))

(define (fun? e) (mtch e ('fun a b) #t _ #f))
(define (fun-name fun) (mtch fun ('fun (name . args) body) name))
(define (fun->clause fun) (mtch fun ('fun (name . args) body) `(clause ,args ,body)))

;(tracefun simplify-patterns patterns->conditionals ->scheme)
;(tracefun def->scheme gort)

(define (hen-run file)
  (shew (tlfs->rules (read-objects file))))

(define (tlfs->rules tlfs)
  (let ((clause-groups (group-by fun-name tlfs)))
    (map (lambda (group)
           (mtch group
                 (name . funs) `(,name ,(map fun->clause funs))))
         clause-groups)))
