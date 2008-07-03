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

(define (h-primitive-car a) (car a))
(define (h-primitive-cdr a) (cdr a))
(define (h-primitive-cons a b) (cons a b))
;(define (h-primitive-list . args) (apply list args))
(define (h-primitive-list . args) args)

(define (h-primitive-not a)
  (if (eq? a 'true)
      'false
      (if (eq? a 'false)
          'true
          (err 'not a))))

(define (h-primitive-equal? a b)
  (if (equal? a b)
      'true
      'false))
