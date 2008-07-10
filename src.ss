;; (fun ('foo a b) ('bar b b))
;; ('foo 10 20)
;; ('bar 10 20)
;; ('foo 20 10)
;; ('bar 20 10)

;(fun (cons a b) ('cons a b))

;(fun ('boo) ('fark))
;(fun ('boo a . d) ('boo . d))
;('boo 1 2 3)

(fun ('boo ()) ())
(fun ('boo (a . d)) (('gorp a a) . ('boo d)))
;('boo ('cons 10 ('cons 20 ())))
('boo (1 2 3 4))
