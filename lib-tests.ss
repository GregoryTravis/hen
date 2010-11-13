(load "h.ss")

(define (run-test test) (mtch test (a b) (if (equal? a b) 'ok `(fail ,a ,b))))
(define (test)
  (let ((results
         (map run-test
                `(
                  (1 1)
                  (,(descend-and-substitute '(1 2 3 4 2 5) (lambda (n) (if (and (number? n) (== n 2)) (just (list 20 'ayup)) fail)))
                   ((1 20 3 4 20 5) (ayup ayup)))
                  (,(predmap even? (lambda (x) (* x 10)) '(0 1 2 3 4)) (0 1 20 3 40))
                  ))))
    (if (all? (map ($ eq? _ 'ok) results))
        '(ok)
        results)))
