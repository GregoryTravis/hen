(load "lib.ss")

(define verbose-list-primitives #f)

(define primitive-prefix "h-primitive-")

(define (find-primitives)
  (maybe-successes
   (map (lambda (identifier)
          (if (starts-with (->string identifier) primitive-prefix)
              (let ((real-name (->symbol (substring (->string identifier) (string-length primitive-prefix)))))
                (just (cons real-name (eval identifier))))
              fail))
        (namespace-mapped-symbols))))

(define (h-primitive-+ a b) (+ a b))
(define (h-primitive-= a b)
  (if (equal? a b)
      'true
      'false))

(define primitives (find-primitives))
(if verbose-list-primitives
    (begin
      (display "primitives: ")
      (shew (map car primitives))))

(define (get-primitive name)
  (let ((blah (assoc name primitives)))
    (if (eq? #f blah)
        fail
        (just (cdr blah)))))
