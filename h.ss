(load "lib.ss")
(load "util.ss")
(load "primitives.ss")

(define (hen filename)
  (run-src (read-src filename)))

;(let ((e (/. (a . b) a)))

; Should 
(define (simplify-patterns e)
  (let ((v (sg)))
    (mtch e
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
          `(/. ,v (if (pair? ,v) (let* ((,a (car ,v)) (,b (cdr ,v))) ,(patterns->conditionals body)) (,(patterns->conditionals fail))))

          ('/. a body fail)
          `(/. ,a ,body)

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

        x x))

(define (boom) (err 'boom))

;(tracefun simplify-patterns patterns->conditionals ->scheme)

(map (** shew eval ->scheme patterns->conditionals simplify-patterns)
     '(
       (/. x x boom)
       (/. (x . y) x boom)
       (/. (x . (y . z)) x boom)
       (/. ((x . y) . z) x boom)
       ((/. x x boom) 10)
       ((/. (x . y) x boom) (cons 1 2))
       ((/. (x . y) y boom) (cons 1 2))
;;       ((/. (x . y) y boom) 3)
       ((/. ((x . y) . z) x boom) (cons (cons 1 2) 3))
       ((/. ((x . y) . z) y boom) (cons (cons 1 2) 3))
       ((/. ((x . y) . z) z boom) (cons (cons 1 2) 3))
;;       ((/. ((x . y) . z) x boom) (cons 1 (cons 2 3)))
       ((/. (x . (y . z)) x boom) (cons 1 (cons 2 3)))
       ((/. (x . (y . z)) y boom) (cons 1 (cons 2 3)))
       ((/. (x . (y . z)) z boom) (cons 1 (cons 2 3)))
;;       ((/. (x . (y . z)) z boom) (cons (cons 1 2) 3))
       ))
