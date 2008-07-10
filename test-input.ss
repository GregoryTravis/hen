(fun (foo a b) (bar b b))
(foo 10 20)
(bar 10 20)
(foo 20 10)
(bar 20 10)

(fun (tak ()) ())
(fun (tak (cons a d)) (cons (harf a a) (tak d)))
(tak (cons 1 (cons 2 ())))

(fun (yep a) (double a a))
(fun (map f ()) ())
(fun (map f (cons a d)) (cons (f a) (map f d)))
(map yep (cons 1 (cons 2 ())))

(fun (append () a) a)
(fun (append (cons a d) b) (cons a (append d b)))
(append (cons 1 (cons 2 ())) (cons 3 (cons 4 ())))
