(load "lib.ss")

(define verbose-list-primitives #f)

(define primitive-prefix "h-primitive-")

(define (primitive? p)
  (and (proper-list? p)
       (= 2 (length p))
       (eq? 'primitive (car p))))

(define (find-primitives)
  (maybe-successes
   (map (lambda (identifier)
          (if (starts-with (->string identifier) primitive-prefix)
              (let ((real-name (->symbol (substring (->string identifier) (string-length primitive-prefix)))))
                (just (cons real-name (list 'primitive (eval identifier)))))
              fail))
        (namespace-mapped-symbols))))

(define (h-primitive-+ a b) (+ a b))
(define (h-primitive-= a b)
  (if (equal? a b)
      'true
      'false))
(define (h-primitive-pair? a)
  (if (pair? a) 'true 'false))

(define (h-car a) (car a))
(define (h-cdr a) (cdr a))
