(define joe
  (maybe-compose
   (lambda (a) (just (list 'a a)))
   (lambda (b) (just (list 'b b)))
;   (lambda (x) fail)
   (lambda (c) (just (list 'c c)))
))

(shew (joe 20))

(define (is-one a) (if (= a 1) (just 'one) fail))
(define (is-two a) (if (= a 2) (just 'two) fail))
(define (is-three a) (if (= a 3) (just 'three) fail))

(define joe (maybe-try is-one is-two is-three))
(shew (joe 3))
